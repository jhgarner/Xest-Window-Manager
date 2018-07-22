{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core where

import ClassyPrelude
import Graphics.X11.Xlib.Types
import Graphics.X11.Xlib.Window
import Graphics.X11.Xlib.Extras
import Graphics.X11.Types
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict

-- An MTL thing because why not? (Not rhetorical)
newtype Xest a = Xest {runXest :: ReaderT  IterationState IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader IterationState)

-- Immutable state for one iteration of the main loop. Mutates between iterations
data IterationState = IS {display :: Display
                         , eventState :: EventState
                         , event :: Event
                         , dimensions :: (Dimension, Dimension)
                         }


-- Elements of the global state modifiable by an event
newtype EventState = ES {wins :: Tiler}

-- A list of tiling algorithms and the data they store
data Tiler = Vertical [Window] | Horizontal [Window]

-- Adds a new window to a Tiler
addWindow :: Window -> Tiler -> Tiler
addWindow w (Horizontal a) = Horizontal $ w : a
addWindow w (Vertical a) = Vertical $ w : a

-- Deletes a Window from a Tiler
deleteWindow :: Window -> Tiler -> Tiler
deleteWindow w (Horizontal a) = Horizontal $ delete w a
deleteWindow w (Vertical a) = Vertical $ delete w a

-- Used to force the Tiler to give up control over a window.
-- For example, if the Window is moving to a different workplace.
-- TODO Use this
popWindow :: Tiler -> (Window, Tiler)
popWindow (Horizontal (a:as)) = (a, Horizontal as)
popWindow (Vertical (a:as)) = (a, Vertical as)

-- Allow the Tiler to move windows wherever they need to be
placeWindows :: Tiler -> Xest ()
placeWindows (Horizontal ws) = do
  (IS display ES{..} event (width, height)) <- ask
  let numWins = fromIntegral $ length ws
  liftIO $ foldl' (\acc (i, w) -> acc >> do
                      moveWindow display w (fromIntegral width `div` numWins * i) 0
                      resizeWindow display w (width `div` fromIntegral numWins) $ height)
    (pure ()) (zip ((-)numWins <$> [0..]) $ ws)
placeWindows (Vertical ws) = do
  (IS display ES{..} event (width, height)) <- ask
  let numWins = fromIntegral $ length ws
  liftIO $ foldl' (\acc (i, w) -> acc >> do
                      moveWindow display w 0 (fromIntegral height `div` numWins * i)
                      resizeWindow display w (width) (height `div` fromIntegral numWins))
    (pure ()) (zip ((-)numWins <$> [0..]) $ ws)
