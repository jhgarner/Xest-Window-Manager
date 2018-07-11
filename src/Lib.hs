{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( someFunc
    ) where

import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Window
import Graphics.X11.Xlib.Types
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Types
import Data.Bits
import Data.List
import Data.Text
import Data.Text.IO as T
import Control.Monad.IO.Class
import Control.Monad.Reader

data IterationState = IS {display :: Display
                         , numWindows :: Int
                         , event :: Event
                         }

newtype Xest a = Xest {runXest :: ReaderT IterationState IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader IterationState)

someFunc :: IO ()
someFunc = do
  display <- openDisplay ":99"
  let rootWindow = defaultRootWindow display
  selectInput display rootWindow (substructureNotifyMask .|. substructureRedirectMask)
  mainLoop display 0

mapResponse :: Xest Int
mapResponse = do
  IS {..} <- ask
  liftIO $ do
    (mapWindow display (ev_window event))
    moveWindow display (ev_window event) (fromIntegral numWindows * 100) 0
    print "Mapping"
    return $ numWindows + 1

configureResponse :: Xest Int
configureResponse = do
  IS {..} <- ask
  let wc = WindowChanges (ev_x event) (ev_y event) (ev_width event) (ev_height event) (ev_border_width event) (ev_above event) (ev_detail event)
  liftIO $ do
    configureWindow display (ev_window event) (ev_value_mask event) wc
    print numWindows
    return numWindows

destroyAction :: Xest Int
destroyAction = doNothing "Destroying" >>= return . (-) 1

mainLoop :: Display -> Int -> IO ()
mainLoop d numW = (allocaXEvent $ \xPtr -> do
  nextEvent d xPtr
  e <- getEvent xPtr
  eType <- get_EventType xPtr
  case Data.List.find ((==) eType . fst) handlers of
    Nothing -> print "Unknown" >> return numW
    Just (_, f) -> runReaderT (runXest f) (IS d numW e)
  ) >>= mainLoop d

handlers :: [(EventType, Xest Int)]
handlers = [(createNotify, doNothing "Destroying")
           , (destroyNotify, destroyAction)
           , (configureRequest, configureResponse)
           , (mapRequest, mapResponse)]

doNothing :: Text -> Xest Int
doNothing t = do
  IS {..} <- ask
  liftIO $ T.putStrLn t
  return numWindows
