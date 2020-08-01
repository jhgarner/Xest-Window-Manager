{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Base.Global where

import           Standard
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Window
import           Graphics.X11.Xlib.Atom
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import Base.Helpers
import Base.Property
import Config
import Tiler.TilerTypes

-- |Anything that doesn't fit somewhere else. This should probably be
-- split up at some point.
class GlobalX m where
   getTree :: m [Window]
   newWindow :: Window -> m Window
   moveToRoot :: Window -> m ()
   getXEvents :: m (Stream m Event)
   -- |Bool True == kill softly. False == kill hard
   kill :: Bool -> Window -> m (Maybe Window)
   -- If you look into the void, you can find anything
   exit :: m Void

-- |Filter out events we don't care about
eventFilter :: RootWindow -> Event -> Bool
eventFilter root ConfigureEvent {ev_window=win} = win == root
eventFilter _ ButtonEvent {ev_button=button} = button `notElem` [button5, button4]
eventFilter _ CrossingEvent {ev_detail=detail} = detail /= 2
eventFilter _ MapNotifyEvent {} = False
eventFilter _ _ = True

instance Members [MonadIO, Input RootWindow, Input Conf, Input Display, State Bool, State Tiler, Property] m => GlobalX m where
  getTree = do
    display <- input @Display
    root    <- input @RootWindow
    liftIO $ alloca
      \numChildrenPtr -> alloca
        \childrenListPtr -> alloca
          \uselessPtr -> do
            _ <- xQueryTree display root uselessPtr uselessPtr childrenListPtr numChildrenPtr
            numChildren <- peek numChildrenPtr
            peek childrenListPtr >>= peekArray (fromIntegral numChildren)

  newWindow w = do
    d         <- input @Display
    rootWin       <- input @RootWindow
    let defScr = defaultScreen d
    wm_state <- getAtom False "WM_STATE"
    putProperty 32 wm_state w wm_state [1, fromIntegral none]
    xwin <- liftIO $ createSimpleWindow d rootWin
      0 0 400 200 0
      (blackPixel d defScr)
      (blackPixel d defScr)
    liftIO $ reparentWindow d w xwin 0 0
    return xwin

  moveToRoot w = do
    d         <- input @Display
    rootWin       <- input @RootWindow
    liftIO $ reparentWindow d w rootWin 0 0

  getXEvents = do
    d <- input @Display
    root <- input @RootWindow
    filterStream (eventFilter root) =<< repeatStream do
      liftIO $ allocaXEvent $ \p -> do
        nextEvent d p
        getEvent p 

  kill isSoft w = do
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
        else if isSoft then destroyWindow d w >> return (Just w)
        else killClient d w >> return (Just w)

  exit = liftIO exitSuccess

