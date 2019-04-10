{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

module Types where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.State.Lazy
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Types
import           Data.Functor.Foldable.TH
import           Data.Functor.Foldable
import           Text.Show.Deriving
import           Data.Eq.Deriving

data Focus = Focused | Unfocused
  deriving (Eq)
data Direction = Front | Back
  deriving (Show, Eq)

data FocusedList a = FL {focusedElement :: Int, elements :: Vector a}
  deriving (Eq, Show, Functor, Foldable, Traversable)
deriveShow1 ''FocusedList
deriveEq1 ''FocusedList

data Axis = X | Y | Z
  deriving (Eq, Show)

data Tiler a
  = Directional Axis (FocusedList a)
  | Wrap Window
  | EmptyTiler
  | InputController a
  deriving (Eq, Show, Functor, Foldable, Traversable)
instance MonoFoldable (Tiler a)
deriveShow1 ''Tiler
deriveEq1 ''Tiler

type instance Element (Tiler a) = a

makeBaseFunctor ''Tiler

-- | Convenience type for keyEvents
type KeyTrigger = (KeyCode, Mode, Actions)

-- Create a junk instantiations for auto-deriving later
instance Eq Event where
  (==) = error "Don't compare events"

-- | Actions/events to be performed
data Action
  = ChangeLayoutTo (Fix Tiler)
  | ChangeNamed String
  | Move Direction
  | RunCommand String
  | ChangeModeTo Mode
  | ShowWindow String
  | HideWindow String
  | ZoomInInput
  | ZoomOutInput
  | KeyboardEvent KeyTrigger Bool -- TODO use something other than Bool for keyPressed
  | XorgEvent Event
  deriving (Eq, Show)

-- | A series of commands to be executed
type Actions = [Action]

-- | Modes similar to modes in vim
data Mode = NewMode { modeName     :: Text
                    , introActions :: Actions
                    , exitActions  :: Actions
                    }
  deriving (Show)
instance Eq Mode where
  n1 == n2 = modeName n1 == modeName n2

-- | The user provided configuration.
data Conf = Conf { keyBindings  :: [KeyTrigger]
                 , definedModes :: [Mode]
                 }
  -- deriving (Show)

-- | Immutable state. TODO consider changing the name
data IterationState = IS
  { display    :: Display
  , rootWin    :: Window
  , dimensions :: (Dimension, Dimension)
  , config     :: Conf
  , actionTodo :: Maybe Action
  }

-- | A standard mtl monad with Reader and State
newtype Xest a = Xest
  { _runXest :: ReaderT IterationState (StateT EventState IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader IterationState, MonadState EventState)

runXest :: IterationState -> EventState -> Xest a -> IO (a, EventState)
runXest r s x = runStateT (runReaderT (_runXest x) r) s

-- | The mutable state
data EventState = ES
  { _desktop     :: Fix Tiler -- ^ The root Tiler
  , _currentMode :: Mode
  , _keyParser   :: Action -> Xest Actions -- ^ The handler
  , _minimizedWins :: Set Window
  }

-- We use lenses for convenience
makeLenses ''EventState

-- | A simple rectangle
data Rect = Rect
  { x :: Position
  , y :: Position
  , w :: Dimension
  , h :: Dimension
  }

