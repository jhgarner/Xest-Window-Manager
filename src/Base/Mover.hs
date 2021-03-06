{-# LANGUAGE TemplateHaskell #-}

module Base.Mover where

import Base.Helpers
import Base.Property
import Base.Minimizer
import Base.Executor
import Control.Monad.Cont
import Graphics.X11.Types
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Types
import Graphics.X11.Xlib.Window
import qualified SDL
import Standard
import Tiler.Tiler

-- | Anything that changes where a window sits goes in here.
data Mover m where
  ChangeLocation :: ParentChild -> XRect -> Mover ()

  -- | Like the above but asks on SDL windows
  ChangeLocationS :: SDL.Window -> XRect -> Mover ()

  Restack :: [Window] -> Mover ()

  -- | A super light wrapper around actually configuring stuff
  ConfigureWin :: Event -> Mover ()

  SetFocus :: ParentChild -> Mover ()

makeEffect ''Mover

-- A whole bunch of types used for caching.
type LocCache = Map Window XRect

type SDLLocCache = Map SDL.Window XRect

type WindowStack = [Window]

newtype FocusedCache = FocusedCache Window
  deriving (Eq, Show)

-- Move windows using IO
runMover :: Members (States [LocCache, SDLLocCache, Tiler, FocusedCache, OldTime] ++ '[Minimizer, Executor, Property, Input RootWindow, IO, Input Display]) m => Eff (Mover ': m) a -> Eff m a
runMover = interpret \case
  ChangeLocation (ParentChild p c pWin) r@(Rect x y h w) -> do
    -- Every window will have change location called every "frame".
    -- The cache makes sure we don't send more wark to X than we need.
    locCache <- get @LocCache
    unless (locCache ^. at c == Just r) $ do
      d <- input @Display
      void $
        if h == 0 || w == 0
          then minimize p
          else do
            liftIO $ do
              -- Both the parent and the child should be the same size.
              resizeWindow d c h w
              resizeWindow d p h w
              resizeWindow d pWin h w

              -- The child's location is relative to the parent.
              moveWindow d c 0 0
              moveWindow d pWin 0 0

              -- The parent's location is "global" so it needs to move.
              moveWindow d p x y

            -- If the parent or child had been minimized, fix that.
            restore c
            restore pWin
            restore p
    modify @LocCache $ set (at c) $ Just r

  ChangeLocationS win r@(Rect x y h w) -> do
    -- Avoid moving the SDL windows more than we need to
    sdlLocCache <- get @SDLLocCache
    unless (sdlLocCache ^. at win == Just r) $ do
      liftIO $ SDL.setWindowPosition win $ SDL.Absolute $ SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))
      SDL.windowSize win SDL.$= SDL.V2 (fromIntegral h) (fromIntegral w)
    modify @SDLLocCache $ set (at win) $ Just r

  Restack wins -> do
    d <- input
    -- The sync here fixes a race condition between SDL and Xest.
    -- We need the SDL bufferSwap function to run after the restack.
    -- Otherwise, SDL won't draw the parts of the border which are
    -- temporarily under another window.
    void . liftIO $ restackWindows d wins >> sync d False

  ConfigureWin ConfigureRequestEvent {..} -> do
    d <- input
    tiler <- get @Tiler
    void . liftIO $ try @SomeException do
      WindowAttributes {..} <- getWindowAttributes d ev_window
      let wc =
            WindowChanges
              ev_x
              ev_y
              ev_width
              ev_height
              ev_border_width
              ev_above
              ev_detail
      if findWindow ev_window tiler
        then allocaXEvent $ \pe -> do
          setEventType pe configureNotify
          setConfigureEvent pe ev_window ev_window wa_x wa_y wa_width wa_height wa_border_width none False
          sendEvent d ev_window False noEventMask pe
        else configureWindow d ev_window ev_value_mask wc
  ConfigureWin _ -> error "Don't call Configure Window with other events"

  SetFocus (ParentChild p c _) ->
    input @Display >>= \d -> do
      root <- get @Tiler
      rootWin <- input @RootWindow
      OldTime time <- get @OldTime
      wm_take_focus <- getAtom False "WM_TAKE_FOCUS"
      wm_protocols <- getAtom False "WM_PROTOCOLS"
      protocols <- liftIO $ getWMProtocols d c
      Rect _ _ width height <- gets @LocCache $ fromMaybe (Rect 0 0 0 0) . view (at c)
      focusCache <- get @FocusedCache
      unless (focusCache == FocusedCache c) $ do
        when ((width /= 0 && height /= 0) || rootWin == c) $ do
          hints <- liftIO $ getWMHints d c
          if wm_take_focus `elem` protocols && not (wmh_input hints)
            then do
              liftIO $
                allocaXEvent $ \pe -> do
                  setEventType pe clientMessage
                  setClientMessageEvent pe c wm_protocols 32 wm_take_focus time
                  sendEvent d c False noEventMask pe
            else do
              restore p
              restore c
              liftIO $ setInputFocus d c revertToPointerRoot currentTime
          liftIO $ cata (grabOthers d c) root
          put @FocusedCache $ FocusedCache c
      liftIO $ allowEvents d replayPointer currentTime >> sync d False
    where
      grabOthers d target (Wrap (ParentChild parent child _))
        | child == target = ungrabButton d anyButton anyModifier parent
        | otherwise =
          grabButton
            d
            anyButton
            anyModifier
            parent
            False
            (buttonPressMask .|. buttonReleaseMask)
            grabModeAsync
            grabModeAsync
            none
            none
      grabOthers _ _ t = sequence_ t


-- | Run a mover when you don't actually want to do anything
runFakeMover :: Eff (Mover ': m) a -> Eff m a
runFakeMover = interpret \case
  ChangeLocation _ _ -> return ()
  ChangeLocationS _ _ -> return ()
  Restack _ -> return ()
  ConfigureWin _ -> return ()
  SetFocus _ -> return ()
