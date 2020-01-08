{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns   #-}
{-# LANGUAGE QuantifiedConstraints   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- TODO Yikes to this entire module. None of these types
-- are related. Tiler types should be in Tiler, Actions in
-- Actions, etc. In addition, Config should not be duplicating
-- everything it needs. That's just error prone.
-- 
-- The problem though is I don't know how to deal with circular
-- dependencies. Can I avoid those somehow? 
module Types where

import           Standard
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Types
import           Text.Show.Deriving
import           Data.Eq.Deriving
import           FocusList
import           Dhall (Interpret)
import           Data.Functor.Foldable (embed)
import qualified SDL (Window)
import           TH
import           Data.Kind

data Plane = Plane
  { rect :: Rect
  , depth :: Int
  }
  deriving Show

data Sized a = Sized {getSize :: Double, getItem :: a}
  deriving (Show, Functor, Foldable, Traversable, Generic)

toTup :: ((Double, a) -> (c, b)) -> Sized a -> (c, b)
toTup f (Sized d a) = f (d, a)

asTup :: ((Double, a) -> (Double, b)) -> Sized a -> Sized b
asTup f (Sized d a) = let (newD, b) = f (d, a) in Sized newD b

instance Eq a => Eq (Sized a) where
  (Sized _ a) == (Sized _ b) = a == b
deriveShow1 ''Sized
deriveEq1 ''Sized

data BottomOrTop a = Bottom a | Top (Rect, a)
  deriving (Eq, Show, Functor, Foldable, Traversable)
deriveShow1 ''BottomOrTop
deriveEq1 ''BottomOrTop
instance Comonad BottomOrTop where
  extract (Bottom a) = a
  extract (Top (_,a)) = a
  duplicate = Bottom


getEither :: BottomOrTop a -> a
getEither (Bottom a) = a
getEither (Top (_, a)) = a

data ChildParent = ChildParent Window Window
  deriving Show

inChildParent :: Window -> ChildParent -> Bool
inChildParent win (ChildParent ww ww') = win == ww || win == ww'

instance Eq ChildParent where
  (ChildParent a b) == (ChildParent a' b') = a == a' || b == b'

data TilerF a
  = HorizF (FocusedList (Sized a))
  | FloatingF (NonEmpty (BottomOrTop a))
  | ReflectF a
  | FocusFullF a
  | WrapF ChildParent
  | InputControllerF (Maybe a)
  | MonitorF (Maybe a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
deriveShow1 ''TilerF
deriveEq1 ''TilerF

type instance Element (TilerF a) = a
instance MonoFoldable (TilerF a)

type instance Base (TilerF (Fix TilerF)) = TilerF

-- instance Functor (Base (TilerF a))
instance Recursive (TilerF (Fix TilerF)) where
  project = coerce

instance Corecursive (TilerF (Fix TilerF)) where
  embed = coerce

-- |Used to make type signatures easier to read. A SubTiler is a Tiler that
-- will be a child of another.
type SubTiler = Fix TilerF
type Tiler = TilerF (Fix TilerF)

-- This class represents things that can be transformed to and from
-- the TilerF data type.
class TilerLike a where
  type family PolyA a :: Type

  toFType :: a -> TilerF (PolyA a)
  fromFType :: TilerF (PolyA a) -> a

-- A trivial instance. TilerF can be transformed into itself.
instance TilerLike (TilerF a) where
  type PolyA (TilerF a) = a
  toFType = id
  fromFType = id

-- The interesting instance. A Fix TilerF can be coerced to and from
-- TilerF.
instance TilerLike (Fix TilerF) where
  type PolyA (Fix TilerF) = Fix TilerF
  toFType = coerce
  fromFType = coerce

-- I thought smart contsructors would be cool because I didn't want to
-- think about whether I needed to Fix something or not. View patterns
-- and pattern synonyms let me make that. Creating them by hand seemed
-- tedius so I learned Template Haskell to do it for me.
makeSimpleBase ''TilerF ''TilerLike ''PolyA 'toFType 'fromFType

inputControllerOrMonitor :: TilerF a -> Maybe (Maybe b -> TilerF b, Maybe a)
inputControllerOrMonitor (InputController a) = Just (InputController, a)
inputControllerOrMonitor (Monitor a) = Just (Monitor, a)
inputControllerOrMonitor _ = Nothing

-- If you get panics in GHC try commenting these out. ¯\_(ツ)_/¯
-- They're extremely useful though as they tell GHC not to worry about incomplete
-- pattern matches.
-- {-# COMPLETE Horiz, Floating, Reflect, FocusFull, Wrap, InputControllerOrMonitor :: TilerF #-}
-- {-# COMPLETE Horiz, Floating, Reflect, FocusFull, Wrap, Monitor, InputController :: TilerF #-}

-- I'm not sure why, but the explicit forall is required...
pattern InputControllerOrMonitor :: forall a b. (Maybe b -> TilerF b) -> Maybe a -> TilerF a
pattern InputControllerOrMonitor c a <- (inputControllerOrMonitor -> Just (c, a))


-- | Convenience type for keyEvents
type KeyTrigger = (KeyCode, Mode, Actions)
                                   
-- Create a junk instantiations for auto-deriving later
instance Eq Event where
  (==) = error "Don't compare XorgEvents"


data Insertable
  = Horizontal
  | Hovering
  | Rotate
  | FullScreen
  deriving (Generic, Show, Eq, Interpret)

-- | Actions/events to be performed
data Action
  = Insert Insertable
  -- | ChangeLayoutTo Insertable
  | ChangeNamed String
  | Move Direction
  | RunCommand String
  | ChangeModeTo Mode
  | ShowWindow String
  | HideWindow String
  | ZoomInInput
  | ZoomInInputSkip
  | ZoomOutInput
  | ZoomOutInputSkip
  | ZoomInMonitor
  | ZoomOutMonitor
  | ZoomMonitorToInput
  | ZoomInputToMonitor
  | PopTiler
  | PushTiler
  | MakeSpecial
  | KillActive
  | ExitNow
  | ToggleLogging
  deriving Show

-- | A series of commands to be executed
type Actions = [Action]

-- | Modes similar to modes in vim
data Mode = NewMode { modeName     :: Text
                    , introActions :: Actions
                    , exitActions  :: Actions
                    , hasButtons :: Bool
                    , hasBorders :: Bool
                    }
  deriving Show
instance Eq Mode where
  n1 == n2 = modeName n1 == modeName n2

-- | The user provided configuration.
data Conf = Conf { keyBindings  :: [KeyTrigger]
                 , definedModes :: [Mode]
                 }
  deriving Show

data KeyStatus = New KeyStatus Mode KeyCode | Temp KeyStatus Mode KeyCode | Dead KeyStatus | Default
makeBaseFunctor ''KeyStatus

instance Show KeyStatus where
  show _ = "Key status"

-- getMode :: KeyStatus -> Mode
-- getMode (Default m) = m
-- getMode (New m _) = getMode m
-- getMode (Temp m _) = getMode m

type KeyPostprocessor r = Mode -> KeyTrigger -> Action -> Actions

type Borders = (SDL.Window, SDL.Window, SDL.Window, SDL.Window)

data MouseButtons = LeftButton (Int, Int) | RightButton (Int, Int) | None
  deriving Show
getButtonLoc :: MouseButtons -> (Int, Int)
getButtonLoc (LeftButton l) = l
getButtonLoc (RightButton l) = l
getButtonLoc _ = error "Tried to get a button when none exist"

-- TODO should this all be here
type Reparenter = Maybe SubTiler -> Tiler
type Unparented = Maybe Tiler
data TreeCombo = Neither | Unmovable | Movable (Reparenter, Unparented) | Both

getMovable :: TreeCombo -> Maybe (Reparenter, Unparented)
getMovable (Movable m) = Just m
getMovable _ = Nothing

isUnmovable :: TreeCombo -> Bool
isUnmovable Unmovable = True
isUnmovable _ = False

isBoth :: TreeCombo -> Bool
isBoth Both = True
isBoth _ = False

instance Semigroup TreeCombo where
  Neither <> a = a
  a <> Neither = a
  Both <> _ = Both
  _ <> Both = Both
  _ <> _ = Both
  
instance Monoid TreeCombo where
  mempty = Neither
instance Show TreeCombo where
  show Both = "Both"
  show Neither = "Neither"
  show Unmovable = "Unmovable"
  show (Movable _) = "Movable"
-- data TreeCombo = TreeCombo { hasMovable :: Bool
--                            , hasMonitor :: Bool
--                            , hasWin :: Bool
--                            }
--   deriving (Eq, Show)

-- withController :: TreeCombo
-- withController = TreeCombo True False False
-- withMonitor :: TreeCombo
-- withMonitor = TreeCombo False True False
-- withWin :: TreeCombo
-- withWin = TreeCombo False False True

-- instance Semigroup TreeCombo where
--   TreeCombo a b c <> TreeCombo a2 b2 c2 = TreeCombo (a&&a2) (b&&b2) (c&&c2)

-- instance Monoid TreeCombo where
--   mempty = TreeCombo False False False

data Screen' = Screen' { screenSize :: XRect
                       , screenTree :: Tiler
                       , screenBorders :: Borders
                       }
  deriving Show

type RootWindow = Window
type Pointer = (Int32, Int32)
type Screens = Map Int Screen'

type ActiveScreen = Int
