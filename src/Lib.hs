{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startWM
    ) where

import Core
import ClassyPrelude
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Window
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Screen
import Graphics.X11.Xlib.Types
import Graphics.X11.Types
import Data.Bits
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict
import Foreign.C.Types

startWM :: IO ()
startWM = do
  display <- openDisplay ":99"
  let rootWindow = defaultRootWindow display
  selectInput display rootWindow (substructureNotifyMask .|. substructureRedirectMask)
  mainLoop display . ES $ Vertical []

-- The recursive main loop of the function
mainLoop :: Display -> EventState -> IO ()
mainLoop d es = (allocaXEvent $ \xPtr -> do
  nextEvent d xPtr
  e <- getEvent xPtr
  eType <- get_EventType xPtr
  let screen = defaultScreenOfDisplay d
  case find ((==) eType . fst) handlers of
    Nothing -> say "Unknown" >> return es
    Just (_, f) -> runReaderT (runXest f) (IS d es e (widthOfScreen screen, heightOfScreen screen))
  ) >>= mainLoop d

-- Called on window creation
mapResponse :: Xest EventState
mapResponse = do
  (IS display ES{..} event resolution) <- ask
  let remainingList = addWindow (ev_window event) wins
  liftIO $ mapWindow display (ev_window event)
  placeWindows remainingList
  return $ ES remainingList

-- Called on window destruction
unmapAction :: Xest EventState
unmapAction = do
  IS display ES{..} event resolution <- ask
  let remainingList = deleteWindow (ev_window event) wins
  placeWindows remainingList
  return $ ES remainingList


-- Essentially just an identity function
configureResponse :: Xest EventState
configureResponse = do
  IS {..} <- ask
  let wc = WindowChanges (ev_x event) (ev_y event) (ev_width event) (ev_height event) (ev_border_width event) (ev_above event) (ev_detail event)
  liftIO $ do
    configureWindow display (ev_window event) (ev_value_mask event) wc
    return eventState

-- A list of event handlers
handlers :: [(EventType, Xest EventState)]
handlers = [(createNotify, doNothing "Creating")
           , (destroyNotify, doNothing "Destroying")
           , (configureRequest, configureResponse)
           , (unmapNotify, unmapAction)
           , (mapRequest, mapResponse)]

-- A simple handler for printing and returning
doNothing :: Text -> Xest EventState
doNothing t = do
  IS {..} <- ask
  liftIO $ say t
  return eventState
