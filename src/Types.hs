{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import           Standard
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Types
import           Data.Functor.Foldable.TH
import           Data.Functor.Foldable
import           Text.Show.Deriving
import           Data.Eq.Deriving
import           FocusList

-- | A simple rectangle
data Rect = Rect
  { x :: Position
  , y :: Position
  , w :: Dimension
  , h :: Dimension
  }
  deriving Show

data Plane = Plane
  { rect :: Rect
  , depth :: Int
  }
  deriving Show

data Axis = X | Y | Z
  deriving (Eq, Show, Generic)

data Sized a = Sized {getSize :: Double, getItem :: a}
  deriving (Show, Functor, Foldable, Traversable)

toTup :: ((Double, a) -> (c, b)) -> Sized a -> (c, b)
toTup f (Sized d a) = f (d, a)

asTup :: ((Double, a) -> (Double, b)) -> Sized a -> Sized b
asTup f (Sized d a) = let (newD, b) = f (d, a) in Sized newD b

instance Eq a => Eq (Sized a) where
  (Sized _ a) == (Sized _ b) = a == b
deriveShow1 ''Sized
deriveEq1 ''Sized

data Tiler a
  = Directional Axis (FocusedList (Sized a))
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
type ButtonTrigger = (Button, Mode, Actions)
                                   
-- Create a junk instantiations for auto-deriving later
instance Eq Event where
  (==) = error "Don't compare XorgEvents"

-- | Actions/events to be performed
data Action
  = ChangeLayoutTo (Fix Tiler)
  | ChangeNamed String
  | ChangePostprocessor KeyStatus
  | Move Direction
  | RunCommand String
  | ChangeModeTo Mode
  | ShowWindow String
  | HideWindow String
  | ZoomInInput
  | ZoomOutInput
  | PopTiler
  | PushTiler
  | KeyboardEvent KeyTrigger Bool -- TODO use something other than Bool for keyPressed
  | XorgEvent Event
  | ChangeSize
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
                 , buttonBindings :: [ButtonTrigger]
                 , definedModes :: [Mode]
                 }

data KeyStatus = New Mode KeyTrigger | Temp Mode KeyTrigger | Default
  deriving (Show, Eq)

type KeyPostprocessor r = Mode -> KeyTrigger -> Action -> Actions

type Borders = (Window, Window, Window, Window)

data MouseButtons = LeftButton (Int, Int) | RightButton (Int, Int) | None
  deriving Show
