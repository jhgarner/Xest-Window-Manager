{-# LANGUAGE TemplateHaskell #-}

module Base.Global where

import Base.Helpers
import Base.Property
import Config
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Graphics.X11.Types
import Graphics.X11.Xlib.Atom
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types
import Graphics.X11.Xlib.Window
import Standard
import Tiler.TilerTypes
import Graphics.X11 (set_override_redirect, allocaSetWindowAttributes)

-- | Anything that doesn't fit somewhere else. This should probably be
--  split up at some point.
data GlobalX a where
  GetTree :: GlobalX [Window]
  -- Although this function's name sounds simple, it actually does a bit more
  -- and should probably be split somehow. Basically, it takes the input window,
  -- reparents it, then returns the reparented window as well as the cover
  -- window. The cover window is used to take input away from the real window.
  NewWindow :: Window -> GlobalX (Window, Window)
  MoveToRoot :: Window -> GlobalX ()

  -- | Bool True == kill softly. False == kill hard
  Kill :: Bool -> Window -> GlobalX (Maybe Window)

  Exit :: GlobalX Void
makeEffect ''GlobalX

-- | Filter out events we don't care about
eventFilter :: RootWindow -> Event -> Bool
eventFilter root ConfigureEvent {ev_window = win} = win == root
eventFilter _ ButtonEvent {ev_button = button} = button `notElem` [button5, button4]
eventFilter _ CrossingEvent {ev_detail = detail} = detail /= 2
eventFilter _ MapNotifyEvent {} = False
eventFilter _ _ = True

runGlobalX :: Members [IO, Input RootWindow, Input Conf, Input Display, State Bool, State Tiler, Property] m => Eff (GlobalX ': m) a -> Eff m a
runGlobalX = interpret \case
  GetTree -> do
    display <- input @Display
    root <- input @RootWindow
    liftIO $ alloca
      \numChildrenPtr -> alloca
        \childrenListPtr -> alloca
          \uselessPtr -> do
            _ <- xQueryTree display root uselessPtr uselessPtr childrenListPtr numChildrenPtr
            numChildren <- peek numChildrenPtr
            peek childrenListPtr >>= peekArray (fromIntegral numChildren)

  NewWindow w -> do
    d <- input @Display
    rootWin <- input @RootWindow
    let defScr = defaultScreen d
    wm_state <- getAtom False "WM_STATE"
    putProperty 32 wm_state w wm_state [1, fromIntegral none]
    -- Create parent
    xwin <-
      liftIO $
        createSimpleWindow
          d
          rootWin
          0
          0
          400
          200
          0
          (blackPixel d defScr)
          (blackPixel d defScr)
    liftIO $ reparentWindow d w xwin 0 0

    -- Create the cover window
    pointerWin <- liftIO $
      allocaSetWindowAttributes $ \wa -> do
        set_override_redirect wa True
        createWindow
          d
          rootWin
          0
          0
          100000
          100000
          0
          copyFromParent
          inputOnly
          -- Visual should take copyFromParent as a parameter I think...
          (unsafeCoerce copyFromParent)
          cWOverrideRedirect
          wa
    liftIO $ selectInput d pointerWin $
      substructureNotifyMask
        .|. pointerMotionMask
        .|. buttonPressMask
        .|. buttonReleaseMask
    liftIO $ reparentWindow d pointerWin xwin 0 0
    return (xwin, pointerWin)

  MoveToRoot w -> do
    d <- input @Display
    rootWin <- input @RootWindow
    liftIO $ reparentWindow d w rootWin 0 0
    liftIO $ unmapWindow d w


  Kill isSoft w -> do
    d <- input @Display
    liftIO $ do
      deleteName <- internAtom d "WM_DELETE_WINDOW" False
      protocols <- internAtom d "WM_PROTOCOLS" True
      supportedProtocols <- getWMProtocols d w
      -- Thanks Xmonad for the kill window code
      if deleteName `elem` supportedProtocols
        then allocaXEvent $ \ev -> do
          setEventType ev clientMessage
          setClientMessageEvent ev w protocols 32 deleteName currentTime
          sendEvent d w False noEventMask ev
          flush d
          return Nothing
        else
          if isSoft
            then destroyWindow d w >> return (Just w)
            else killClient d w >> return (Just w)

  Exit -> liftIO exitSuccess

-- I don't think I can represent this as a Freer effect?
getXEvents :: Members [IO, Input RootWindow, Input Conf, Input Display, State Bool, State Tiler, Property] m => Eff m (Stream (Eff m) Event)
getXEvents = do
  d <- input @Display
  root <- input @RootWindow
  filterStream (eventFilter root) =<< repeatStream do
    liftIO $
      allocaXEvent $ \p -> do
        nextEvent d p
        getEvent p
