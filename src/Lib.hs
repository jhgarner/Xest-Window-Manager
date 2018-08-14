{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Lib
  ( startWM
  ) where

import ClassyPrelude
import Config
import Core
import Data.Bits
import Graphics.X11.Types
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Screen
import Graphics.X11.Xlib.Types
import Graphics.X11.Xlib.Window

data Void

startWM :: IO ()
startWM = do
  display <- openDisplay ":99"
  let root = defaultRootWindow display
  selectInput
    display
    root
    (substructureNotifyMask .|. substructureRedirectMask .|. keyPressMask)
  grabKey display 133 anyModifier root False grabModeAsync grabModeAsync
  c <- getConfig "config.conf"
  -- initKeyBindings display root c
  mainLoop display root c $ ES (Horizontal []) InsertMode

-- The recursive main loop of the function
mainLoop :: Display -> Window -> Conf -> EventState -> IO a
mainLoop d w c es = runReaderT (runXest loop) state >>= mainLoop d w c
  where screen = defaultScreenOfDisplay d
        state = IS d w es (widthOfScreen screen, heightOfScreen screen) c
        loop = do
          render
          rebindKeys
          ptr <- liftIO . allocaXEvent $ \p -> nextEvent d p >> getEvent p
          handler ptr

-- Acts on events
-- Called on window creation
handler :: Event -> Xest EventState
handler MapRequestEvent {..} = do
  display <- asks display
  ES {..} <- asks eventState
  tWin <- manage ev_window
  liftIO $ mapWindow display ev_window
  return $ ES (addWindow tWin desktop) currentMode

-- Called on window destruction
-- TODO handle minimizing as unmapping
handler UnmapEvent {..} = do
  ES {..} <- asks eventState
  return $ ES (deleteWindow (Wrap ev_window) desktop) currentMode

-- Tell the window it can configure itself however it wants
handler ConfigureRequestEvent {..} = do
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
handler KeyEvent {..} = do
  eventState@ES {..} <- asks eventState
  config <- asks config
  case find (\(KeyBinding k _ _) -> ev_keycode == k && ev_event_type == keyPress) config of
    Nothing -> return eventState
    Just (KeyBinding _ ls _) -> parseActions ls

-- Basically just id
handler _ = asks eventState

-- Utilities
-- Would be used for reparenting (title bar)
manage :: Window -> Xest Tiler
manage w = do
  IS {..} <- ask
  return $ Wrap w

-- Renders/moves windows
render :: Xest ()
render = do
  (w, h) <- asks dimensions
  ES t m <- asks eventState
  placeWindows (Rect 0 0 w h) t

-- Chang the keybindings depending on the mode
rebindKeys :: Xest ()
rebindKeys = do
  ES {..} <- asks eventState
  kb <- asks config
  d <- asks display
  w <- asks rootWin
  liftIO . forM_ kb $ toggleModel currentMode d w
  where toggleModel m d w (KeyBinding k _ km) =
          if m == km then grabKey d k anyModifier w False grabModeAsync grabModeAsync
          else ungrabKey d k anyModifier w
