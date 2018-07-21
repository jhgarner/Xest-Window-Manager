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
import Graphics.X11.Xlib.Screen
import Graphics.X11.Types
import Data.Bits
import Data.List
import Data.Text
import Data.Text.IO as T
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict
import Foreign.C.Types


-- Elements of the global state modifiable by an event
data EventState = ES { wins :: [Window]
                     }

-- Immutable state for one iteration of the main loop. Mutates between iterations
data IterationState = IS {display :: Display
                         , eventState :: EventState
                         , event :: Event
                         , dimensions :: (Dimension, Dimension)
                         }

-- An MTL thing because why not? (Not rhetorical)
newtype Xest a = Xest {runXest :: ReaderT  IterationState IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader IterationState)

-- TODO don't use someFunc
someFunc :: IO ()
someFunc = do
  display <- openDisplay ":99"
  let rootWindow = defaultRootWindow display
  selectInput display rootWindow (substructureNotifyMask .|. substructureRedirectMask)
  mainLoop display $ ES []

-- Called on window creation
mapResponse :: Xest EventState
mapResponse = do
  (IS display ES{..} event resolution) <- ask
  liftIO $ do
    (mapWindow display (ev_window event))
    let remainingList = (ev_window event) : wins
    let numWins = Data.List.length remainingList
    Data.List.foldl (\acc (i, w) -> acc >> tileHorizontally display w resolution i numWins)
      (print "mapping") (Data.List.zip [0..] remainingList)
    return $ ES remainingList

-- Essentially just an identity function
configureResponse :: Xest EventState
configureResponse = do
  IS {..} <- ask
  let wc = WindowChanges (ev_x event) (ev_y event) (ev_width event) (ev_height event) (ev_border_width event) (ev_above event) (ev_detail event)
  liftIO $ do
    configureWindow display (ev_window event) (ev_value_mask event) wc
    print "Configuring"
    return eventState

-- Called on window destruction
unmapAction :: Xest EventState
unmapAction = do
  IS display ES{..} event resolution <- ask
  liftIO $ do
    let remainingList = delete (ev_window event) wins
    let numWins = Data.List.length remainingList
    Data.List.foldl (\acc (i, w) -> acc >> tileHorizontally display w resolution i numWins)
      (print "unmapping") (Data.List.zip [0..] remainingList)
    return $ ES remainingList

-- The recursive main loop of the function
mainLoop :: Display -> EventState -> IO ()
mainLoop d es = (allocaXEvent $ \xPtr -> do
  nextEvent d xPtr
  e <- getEvent xPtr
  eType <- get_EventType xPtr
  let screen = defaultScreenOfDisplay d
  case Data.List.find ((==) eType . fst) handlers of
    Nothing -> print "Unknown" >> return es
    Just (_, f) -> runReaderT (runXest f) (IS d es e (widthOfScreen screen, heightOfScreen screen))
  ) >>= mainLoop d

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
  liftIO $ T.putStrLn t
  return eventState

-- TODO Make this signature less terrible
tileHorizontally :: Display -> Window -> (Dimension, Dimension) -> Position -> Int -> IO ()
tileHorizontally display w (width, height) i numWins = do
  moveWindow display w (fromIntegral width `div` fromIntegral numWins * i) 0
  resizeWindow display w (fromIntegral width `div` fromIntegral numWins) $ fromIntegral height
