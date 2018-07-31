{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startWM
  ) where

import ClassyPrelude
import Config
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict
import Core
import Data.Bits
import Foreign.C.Types
import Graphics.X11.Types
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Screen
import Graphics.X11.Xlib.Types
import Graphics.X11.Xlib.Window

startWM :: IO ()
startWM = do
  display <- openDisplay ":99"
  let rootWindow = defaultRootWindow display
  selectInput
    display
    rootWindow
    (substructureNotifyMask .|. substructureRedirectMask .|. keyPressMask)
  grabKey display 133 anyModifier rootWindow False grabModeAsync grabModeAsync
  c <- getConfig ""
  initKeyBindings display rootWindow c
  mainLoop display c . ES $ Horizontal []

-- The recursive main loop of the function
mainLoop :: Display -> [KeyBinding] -> EventState -> IO ()
mainLoop d c es =
  allocaXEvent
    (\xPtr -> do
       nextEvent d xPtr
       e <- getEvent xPtr
       eType <- get_EventType xPtr
       let screen = defaultScreenOfDisplay d
       runReaderT
         (runXest (handler e >>= render))
         (IS d es (widthOfScreen screen, heightOfScreen screen) c)) >>=
  mainLoop d c

-- Acts on events
handler :: Event -> Xest EventState
-- Called on window creation
handler MapRequestEvent {..} = do
  IS display ES {..} (w, h) _ <- ask
  tWin <- manage ev_window
  liftIO $ mapWindow display ev_window
  return . ES $ addWindow tWin wins
-- Called on window destruction
-- TODO handle minimizing as unmapping
handler UnmapEvent {..} = do
  IS display ES {..} (w, h) _ <- ask
  tWin <- manage ev_window
  return . ES $ deleteWindow tWin wins
-- Tell the window it can configure itself however it wants
handler e@ConfigureRequestEvent {..} = do
  IS {..} <- ask
  liftIO $ do
    configureWindow display ev_window ev_value_mask wc
    return eventState
  where
    wc =
      WindowChanges
        ev_x
        ev_y
        ev_width
        ev_height
        ev_border_width
        ev_above
        ev_detail
-- Run any key configurations
handler e@KeyEvent {..} = do
  IS {..} <- ask
  ks <- liftIO $ keycodeToKeysym display ev_keycode 0
  case find (\(KeyBinding k _) -> ks == k) config of
    Nothing -> return eventState
    Just (KeyBinding _ ls) -> parseActions ls
-- Basically just id
handler _ = asks eventState

-- Utilities
manage :: Window -> Xest Tiler
manage w = do
  IS {..} <- ask
  liftIO $ selectInput display w keyPressMask
  return $ Wrap w

render :: EventState -> Xest EventState
render (ES t) = do
  IS display _ (w, h) _ <- ask
  placeWindows (Rect 0 0 w h) t
  return $ ES t
