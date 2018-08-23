{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib
  ( startWM
  ) where

import ClassyPrelude
import Config
import Core
import Data.Bits
import Control.Lens
import Graphics.X11.Types
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Screen
import Graphics.X11.Xlib.Window

startWM :: IO ()
startWM = do
  display <- openDisplay ":99"
  let root = defaultRootWindow display
  selectInput
    display
    root
    (substructureNotifyMask .|. substructureRedirectMask)
  c <- getConfig "config.conf"
  let screen = defaultScreenOfDisplay display
      state = IS display root initialES (widthOfScreen screen, heightOfScreen screen) c
  initialize state
  mainLoop state

  where initialES = ES (Horizontal []) InsertMode parseActions
        initialize = runReaderT . runXest $ rebindKeys InsertMode

-- The recursive main loop of the function
mainLoop :: IterationState -> IO a
mainLoop state@IS{..} = runReaderT (runXest loop) state >>= \es -> mainLoop state {eventState = es}
  where loop = do
          render
          ptr <- liftIO . allocaXEvent $ \p -> nextEvent display p >> getEvent p
          handler ptr

-- Acts on events
-- Called on window creation
handler :: Event -> Xest EventState
handler MapRequestEvent {..} = do
  display <- asks display
  es <- asks eventState
  tWin <- manage ev_window
  liftIO $ mapWindow display ev_window
  return $ desktop .~ addWindow tWin (_desktop es) $ es

-- Called on window destruction
-- TODO handle minimizing as unmapping
handler UnmapEvent {..} = do
  es <- asks eventState
  return $ desktop .~ deleteWindow (Wrap ev_window) (_desktop es) $ es

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
  eventState <- asks eventState
  config <- asks config
  case find (\(KeyBinding k _ _) -> ev_keycode == k) config of
    Nothing -> return eventState
    Just kb -> view keyParser eventState kb (ev_event_type == keyPress)

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
  ES t _ _ <- asks eventState
  placeWindows (Rect 0 0 w h) t

