{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Types where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.State.Lazy
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Types
import           Text.Read

-- | A standard mtl monad with Reader and State
newtype Xest a = Xest
  { _runXest :: ReaderT IterationState (StateT EventState IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader IterationState, MonadState EventState)

runXest :: IterationState -> EventState -> Xest a -> IO (a, EventState)
runXest r s x = runStateT (runReaderT (_runXest x) r) s

-- | Immutable state. TODO consider changing the name
data IterationState = IS
  { display    :: Display
  , rootWin    :: Window
  , dimensions :: (Dimension, Dimension)
  , config     :: Conf
  , actionTodo :: Maybe Action
  }

-- | The user provided configuration. Uses Read for parsing
-- TODO make this less terrible for config writers
data Conf = Conf { keyBindings  :: [KeyTrigger]
                 , definedModes :: [Mode]
                 }
  deriving (Read, Show)


-- Create some junk instantiations for auto-deriving later
instance Read Event where
  readsPrec = error "Don't read events"
instance Eq Event where
  (==) = error "Don't compare events"

-- | Convenience type for keyEvents
type KeyTrigger = (KeyCode, Mode, Actions)

-- | Actions/events to be performed
data Action
  = ChangeLayoutTo Tiler
  | RunCommand String
  | ChangeModeTo Mode
  | ShowWindow String
  | HideWindow String
  | ZoomInInput
  | ZoomOutInput
  | KeyboardEvent KeyTrigger Bool -- TODO use something other than Bool for keyPressed
  | XorgEvent Event
  deriving (Read, Show, Eq)

-- | A series of commands to be executed
type Actions = [Action]


-- | Modes similar to modes in vim
data Mode = NewMode { modeName     :: Text
                    , introActions :: Actions
                    , exitActions  :: Actions
                    }
  deriving (Read, Show, Eq)


{- |
A list of tiling algorithms and the data they store

Tiler Laws

Tilers are not black holes
a `addFocused` w != a

deleteWindow undoes an add
a `addFocused` w `deleteWindow` w == a

delete is a noop if w is not in a
a `deleteWindow` w (where w is not in a) == a

addFocused undoes a pop
let (w, b) = popWindow a in a == b `addFocused` w

Nested Tilers are searched
let b = a `addFocused` w
    d = c `addFocused` b
    in d `deleteWindow` w == c `addFocused` a

Adds a new Tiler and make it focused
-}
data Tiler
  = Vertical [Tiler]
  | Horizontal [Tiler]
  | Wrap Window
  | EmptyTiler
  | InputController Tiler
  deriving (Eq, Read, Show)

-- | Make tiler a monofunctor (a functor that can only hold one thing)
-- TODO can we make Tiler a real Functor?
type instance Element Tiler = Tiler
instance MonoFunctor Tiler where
  omap f (Horizontal a)      = Horizontal $ f <$> a
  omap f (Vertical a)        = Vertical $ f <$> a
  omap f (InputController t) = f t
  omap _ t@(Wrap _)          = t
  omap _ EmptyTiler          = EmptyTiler

-- | The mutable state
data EventState = ES
  { _desktop     :: Tiler -- ^ The root Tiler
  , _currentMode :: Mode
  , _keyParser   :: Action -> Xest Actions -- ^ The handler
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
