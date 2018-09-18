{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module Types where

import Control.Lens
import ClassyPrelude
import Graphics.X11.Xlib.Types
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras

-- An MTL thing because why not? (Not rhetorical)
newtype Xest a = Xest
  { runXest :: ReaderT IterationState IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader IterationState)

-- Immutable state for one iteration of the main loop. Mutates between iterations
data IterationState = IS
  { display :: Display
  , rootWin :: Window
  , dimensions :: (Dimension, Dimension)
  , config :: Conf
  , actionTodo :: Maybe Action
  }

-- The user provided configuration. Uses Read for parsing
data Conf = Conf { keyBindings :: [Trigger_ KeyTrigger]
                 , definedModes :: [Mode]
                 }
  deriving (Read, Show)


-- A list of triggers. Each trigger corresponds to an event
data Trigger
  = KeyTrigger
  | ModeTrigger
  | XorgTrigger
  deriving (Eq, Read, Show)

-- Each trigger type requires certain parameters to be filled in by the "caller"
-- Each type instance defines those parameters
type family Trigger_ (a :: Trigger)

-- (The key pressed, the mode where the binding is active, the action to execute)
type instance Trigger_ KeyTrigger = (KeyCode, Mode, Actions)
-- Takes no parameters
type instance Trigger_ XorgTrigger = ()
-- (The relevant mode, the intro actions, the exit actions)
type instance Trigger_ ModeTrigger = (Mode, Actions, Actions)


-- An event fills in the trigger parameters and optionally includes additional ones
data Events
  = KeyboardEvent (Trigger_ KeyTrigger) Bool -- TODO use something other than Bool for keyPressed
  | ModeEvent (Trigger_ ModeTrigger) (Trigger_ ModeTrigger) -- Old mode followed by new mode
  | XorgEvent Event
  | NoEvent


-- A command to be executed
data Action
  = ChangeLayoutTo Tiler
  | RunCommand String
  | ChangeModeTo Mode
  | ShowWindow String
  | HideWindow String
  | Done
  deriving (Read, Show, Eq)

-- A series of commands to be executed
type Actions = [Action]


-- Modes similar to modes in vim
data Mode = NewMode { modeName :: Text
                    , introActions :: Actions
                    , exitActions :: Actions
                    }
  deriving (Read, Show, Eq)

-- A list of tiling algorithms and the data they store
data Tiler
  = Vertical [Tiler]
  | Horizontal [Tiler]
  | Wrap Window
  | Empty
  deriving (Eq, Read, Show)

-- Elements of the global state modifiable by an event
data EventState = ES
  { _desktop :: Tiler
  , currentMode_ :: Mode
  , _keyParser :: Events -> EventState -> Xest (Events, EventState) -- TODO replace with pattern synonym
  }

makeLenses ''EventState

-- A rectangle
data Rect = Rect
  { x :: Position
  , y :: Position
  , w :: Dimension
  , h :: Dimension
  }
