{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Core where

import ClassyPrelude
import Graphics.X11.Types
import Graphics.X11.Xlib.Types
import Graphics.X11.Xlib.Window

-- An MTL thing because why not? (Not rhetorical)
newtype Xest a = Xest
  { runXest :: ReaderT IterationState IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader IterationState)

-- Immutable state for one iteration of the main loop. Mutates between iterations
data IterationState = IS
  { display :: Display
  , eventState :: EventState
  , dimensions :: (Dimension, Dimension)
  , config :: [KeyBinding]
  }

data KeyBinding =
  KeyBinding KeySym
             [Action]
  deriving (Read, Show)

data Action
  = ChangeLayoutTo Tiler
  | RunCommand String
  | DoNothing
  deriving (Read, Show)

-- Elements of the global state modifiable by an event
newtype EventState = ES
  { desktop :: Tiler
  }

data Rect = Rect
  { x :: Position
  , y :: Position
  , w :: Dimension
  , h :: Dimension
  }

-- A list of tiling algorithms and the data they store
data Tiler
  = Vertical [Tiler]
  | Horizontal [Tiler]
  | Wrap Window
  | Empty
  deriving (Eq, Read, Show)

-- Adds a new window to a Tiler
addWindow :: Tiler -> Tiler -> Tiler
addWindow w (Horizontal a) = Horizontal $ w : a
addWindow w (Vertical a) = Vertical $ w : a

-- Deletes a Window from a Tiler
deleteWindow :: Tiler -> Tiler -> Tiler
deleteWindow w (Horizontal a) = Horizontal $ delete w a
deleteWindow w (Vertical a) = Vertical $ delete w a

-- Used to force the Tiler to give up control over a window.
-- For example, if the Window is moving to a different workplace.
popWindow :: Tiler -> (Maybe Tiler, Tiler)
popWindow (Horizontal (a:as)) = (Just a, Horizontal as)
popWindow (Horizontal []) = (Nothing, Horizontal [])
popWindow (Vertical (a:as)) = (Just a, Vertical as)
popWindow (Vertical []) = (Nothing, Vertical [])

-- Allow the Tiler to move windows wherever they need to be
placeWindows :: Rect -> Tiler -> Xest ()
placeWindows Rect {..} (Wrap win) = do
  IS {..} <- ask
  liftIO $ moveWindow display win x y
  liftIO $ resizeWindow display win w h
  
placeWindows Rect {..} (Horizontal ws) =
  foldl'
    (\acc (i, t) ->
       acc >>
       placeWindows
         (Rect
            ((fromIntegral w `div` numWins * i) + x)
            y
            (w `div` fromIntegral numWins)
            h)
         t)
    (pure ()) $
  ClassyPrelude.zip ((-) numWins <$> [1 ..]) ws
  where
    numWins = fromIntegral $ ClassyPrelude.length ws
    
placeWindows Rect {..} (Vertical ws) =
  foldl'
    (\acc (i, t) ->
       acc >>
       placeWindows
         (Rect
            x
            (fromIntegral h `div` numWins * i + y)
            w
            (h `div` fromIntegral numWins))
         t)
    (pure ()) $
  zip ((-) numWins <$> [1 ..]) ws
  where
    numWins = fromIntegral $ length ws
    
placeWindows _ _ = return ()
