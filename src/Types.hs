{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings #-}

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
  deriving (Show)


-- Create a junk instantiations for auto-deriving later
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
  deriving (Show, Eq)

-- | A series of commands to be executed
type Actions = [Action]


-- | Modes similar to modes in vim
data Mode = NewMode { modeName     :: Text
                    , introActions :: Actions
                    , exitActions  :: Actions
                    }
  deriving (Show, Eq)


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
  deriving (Eq, Show)

-- | Make tiler a monofunctor (a functor that can only hold one thing)
-- can we make Tiler a real Functor? No because what if we wanted to
-- store Tilers inside of Tilers. The type would get weird.
type instance Element Tiler = Tiler
instance MonoFunctor Tiler where
  omap f (Horizontal a)      = Horizontal $ f <$> a
  omap f (Vertical a)        = Vertical $ f <$> a
  omap f (InputController t) = f t
  omap _ t@(Wrap _)          = t
  omap _ EmptyTiler          = EmptyTiler

-- TODO This looks like something a computer could derive
instance MonoFoldable Tiler where
  ofoldl' f initial (Horizontal ls) = foldl' f initial ls
  ofoldl' f initial (Vertical ls) = foldl' f initial ls
  ofoldl' f initial (InputController t) = foldl' f initial t
  ofoldl' _ initial (Wrap _) = initial
  ofoldl' _ initial EmptyTiler = initial

  ofoldl1Ex' f (Horizontal ls) = ofoldl1Ex' f ls
  ofoldl1Ex' f (Vertical ls) = ofoldl1Ex' f ls
  ofoldl1Ex' f (InputController t) = ofoldl1Ex' f t
  ofoldl1Ex' _ (Wrap _) = error "Data.MonoTraversable.headEx: empty"
  ofoldl1Ex' _ EmptyTiler = error "Data.MonoTraversable.headEx: empty"

  ofoldr1Ex f (Horizontal ls) = ofoldr1Ex f ls
  ofoldr1Ex f (Vertical ls) = ofoldr1Ex f ls
  ofoldr1Ex f (InputController t) = ofoldr1Ex f t
  ofoldr1Ex _ (Wrap _) = error "Data.MonoTraversable.headEx: empty"
  ofoldr1Ex _ EmptyTiler = error "Data.MonoTraversable.headEx: empty"

  ofoldr f initial (Horizontal ls) = foldr f initial ls
  ofoldr f initial (Vertical ls) = foldr f initial ls
  ofoldr f initial (InputController t) = foldr f initial t
  ofoldr _ initial (Wrap _) = initial
  ofoldr _ initial EmptyTiler = initial

  ofoldMap f (Horizontal ls) = foldMap f ls
  ofoldMap f (Vertical ls) = foldMap f ls
  ofoldMap f (InputController t) = foldMap f t
  ofoldMap _ (Wrap _) = mempty
  ofoldMap _ EmptyTiler = mempty

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
