{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Base.Mover where

import           Standard
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Window
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Misc
import qualified SDL
import Base.Helpers
import Base.Property
import Base.Minimizer
import           Base.Executor
import Tiler.Tiler
import Control.Monad.Cont
import Control.Monad.Trans.Control

-- |Anything that changes where a window sits goes in here.
class Mover m where
  changeLocation :: ParentChild -> XRect -> m ()
  -- |Like the above but asks on SDL windows
  changeLocationS :: SDL.Window -> XRect -> m ()
  restack :: [Window] -> m ()
  -- |A super light wrapper around actually configuring stuff
  configureWin :: Event -> m ()
  setFocus :: ParentChild -> m ()

-- A whole bunch of types used for caching.
type LocCache = Map Window XRect
type SDLLocCache = Map SDL.Window XRect
type WindowStack = [Window]
newtype FocusedCache = FocusedCache Window
  deriving (Eq, Show)

-- Move windows using IO
instance Members (States [LocCache, SDLLocCache, WindowStack, Tiler, FocusedCache, OldTime] ++ '[Minimizer, Executor, Property, Input RootWindow, MonadIO, Input Display]) m => Mover m where
  changeLocation (ParentChild p c) r@(Rect x y h w) = do
    -- Every window will have change location called every "frame".
    -- The cache makes sure we don't send more wark to X than we need.
    locCache <- get @LocCache
    unless (locCache ^. at c == Just r) $ do
      d <- input @Display
      void $ if h == 0 || w == 0 then minimize p else do
        liftIO $ do
          -- Both the parent and the child should be the same size.
          resizeWindow d c h w
          resizeWindow d p h w

          -- The child's location is relative to the parent.
          moveWindow d c 0 0 

          -- The parent's location is "global" so it needs to move.
          moveWindow d p x y 

        -- If the parent or child had been minimized, fix that.
        restore c
        restore p
    modify @LocCache $ set (at c) $ Just r

  changeLocationS win r@(Rect x y h w) = do
    -- Avoid moving the SDL windows more than we need to
    sdlLocCache <- get @SDLLocCache
    unless (sdlLocCache ^. at win == Just r) $ do
      liftIO $ SDL.setWindowPosition win $ SDL.Absolute $ SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))
      SDL.windowSize win SDL.$= SDL.V2 (fromIntegral h) (fromIntegral w)
    modify @SDLLocCache $ set (at win) $ Just r

  restack wins = do
    stackCache <- get @WindowStack
    unless (stackCache == wins) $ do
      d <- input
      -- The sync here fixes a race condition between SDL and Xest.
      -- We need the SDL bufferSwap function to run after the restack.
      -- Otherwise, SDL won't draw the parts of the border which are
      -- temporarily under another window.
      void . liftIO $ restackWindows d wins >> sync d False
    put @[Window] wins


  configureWin ConfigureRequestEvent {..} = do
    d <- input
    tiler <- get @Tiler
    void . liftIO $ try @SomeException do
      WindowAttributes {..} <- getWindowAttributes d ev_window
      let wc = WindowChanges ev_x
                            ev_y
                            ev_width
                            ev_height
                            ev_border_width
                            ev_above
                            ev_detail
      if findWindow ev_window tiler
        then
          allocaXEvent $ \pe -> do
            setEventType pe configureNotify
            setConfigureEvent pe ev_window ev_window wa_x wa_y wa_width wa_height wa_border_width none False
            sendEvent d ev_window False noEventMask pe
        else
          configureWindow d ev_window ev_value_mask wc

  configureWin _ = error "Don't call Configure Window with other events"

  setFocus (ParentChild p c) = input @Display >>= \d -> do
    root <- get @Tiler
    rootWin <- input @RootWindow
    OldTime time <- get @OldTime
    wm_take_focus <- getAtom False "WM_TAKE_FOCUS"
    wm_protocols <- getAtom False "WM_PROTOCOLS"
    protocols <- liftIO $ getWMProtocols d c
    Rect _ _ width height <- gets @LocCache (fromMaybe (Rect 0 0 0 0) . (view $ at c))
    focusCache <- get @FocusedCache
    unless (focusCache == FocusedCache c) $ do
      when ((width /= 0 && height /= 0) || rootWin == c) $ do
        hints <- liftIO $ getWMHints d c
        if wm_take_focus `elem` protocols && not (wmh_input hints)
          then do
            liftIO $ allocaXEvent $ \pe -> do
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
    grabOthers d target (Wrap (ParentChild parent child))
      | child == target = ungrabButton d anyButton anyModifier parent
      | otherwise = grabButton d
                               anyButton
                               anyModifier
                               parent
                               False
                               (buttonPressMask .|. buttonReleaseMask)
                               grabModeSync
                               grabModeAsync
                               none
                               none
    grabOthers _ _ t = sequence_ t


newtype FakeMover m a = FakeMover { runFakeMover :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)
instance MonadTrans FakeMover where
  lift = FakeMover
instance MonadTransControl FakeMover where
  type instance StT FakeMover a = a
  liftWith f = FakeMover $ f runFakeMover
  restoreT = lift

-- |Run a mover when you don't actually want to do anything
instance Monad m => Mover (FakeMover m) where
  changeLocation _ _ = return ()
  changeLocationS _ _ = return ()
  restack _ = return ()
  configureWin _ = return ()
  setFocus _ = return ()
