{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
   TilerF and its associated types are probably some of the most important in
   the project. Understanding them is important to understanding how Xest works
   as a whole. If you're unfamiliar with recursion schemes, definitely read up
   on those. Learing about that library and reading
   https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html
   has probably changed how I write and think about code more than most of the
   other concepts used in this project.
-}
module Tiler.TilerTypes where

import           Standard
import           Graphics.X11.Types
import           FocusList
import           Data.Functor.Foldable (embed)
import           Dhall (Interpret)
import           Tiler.Sized
import           Tiler.ParentChild
import           Tiler.BottomOrTop
import           Text.Show.Deriving
import           Data.Eq.Deriving
import           Data.Kind
import           TH

-- | The tree nodes which make up the bulk of the program. Each
-- constructor provides a way of composing windows. The a type variable is
-- there so we can use recursion schemes on this data type. Every use of a can
-- be thought of as a branch in the tree.
data TilerF a =
    -- |Tiles its elements horizontally while keeping track of which were last
    -- focused. In addition, each element has an associated size. 0 means the
    -- element requests no extra space. Any other number means the element
    -- requests that much more space as a percentage of the total amount of
    -- space.
    HorizF (FocusedList (Sized a))
    -- |Floats multiple elements on top of a background element. The background
    -- element takes up all of the space it can. The foreground elements take up
    -- the space given by some rectangle. Those foreground elements can float
    -- anywhere on the screen, even outside of the parent's allocated space.
  | FloatingF (NonEmpty (BottomOrTop a))
    -- |The name in code is very misleading. Instead of reflecting, this Tiler
    -- rotates its child 90 degrees by swapping the x and y coordinates. TODO The
    -- name is bad because rotate is already taken by the Action. Ideally, the
    -- Action Insertable datatype would be a subset of the TilerF data type. I
    -- think that requires open sum types which would be a big project change.
  | ReflectF a
    -- |FocusFull takes its child and finds the focused child in that. Whatever
    -- was found takes up all of its allocated space. If an Input Controller or
    -- Monitor or in front of FocusFull, nothing happens. When combined with
    -- Horiz or Floating, you get one way to emulate workspaces.
  | FocusFullF a
    -- |The leaf of our Tiler tree. A WrapF holds a window (specifically a window
    -- and its parent) and does nothing else.
  | WrapF ParentChild
    -- |This data type controls where Actions and XEvents happen. For example,
    -- when a new window gets created, it gets placed as a child of whatever
    -- comes immediately after the InputController. Unlike most other Tilers,
    -- this one can be empty.
  | InputControllerF (Maybe a)
    -- |Monitor decides where to start rendering. Only children of Monitor get
    -- rendered. Just like InputController, Monitor can be empty.
  | MonitorF (Maybe a)

  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
deriveShow1 ''TilerF
deriveEq1 ''TilerF

-- TODO I'm not sure I actually like the MonoTraversable hierarchy although I
-- really like the convenient Container stuff it has.
type instance Element (TilerF a) = a

instance MonoFoldable (TilerF a)

-- These instances let us use recursion schemes on Tilers. If I had to pick one
-- library that has had the most impact on Xest, I would easily pick recursion
-- schemes.
type instance Base (TilerF (Fix TilerF)) = TilerF

instance Recursive (TilerF (Fix TilerF)) where
  project = coerce

instance Corecursive (TilerF (Fix TilerF)) where
  embed = coerce

-- |Used to make type signatures easier to read. A SubTiler is a Tiler that
-- will be a child of another.
type SubTiler = Fix TilerF

-- |Another type signature helper. This type represents your standard,
-- recursive Tiler.
type Tiler = TilerF (Fix TilerF)

-- |This class represents things that can be transformed to and from
-- the TilerF data type. It exists so we can ignore whether something actually
-- needs to be Fixed.
class TilerLike a where
  type PolyA a :: Type

  toFType :: a -> TilerF (PolyA a)
  fromFType :: TilerF (PolyA a) -> a

 -- |A trivial instance. TilerF can be transformed into itself.
instance TilerLike (TilerF a) where
  type PolyA (TilerF a) = a

  toFType = id

  fromFType = id

 -- |The interesting instance. A Fix TilerF can be coerced to and from
 -- TilerF.
instance TilerLike (Fix TilerF) where
  type PolyA (Fix TilerF) = Fix TilerF

  toFType = coerce

  fromFType = coerce

-- |Generate some smart patterns that can match both TilerF and (Fix TilerF).
makeSimpleBase ''TilerF ''TilerLike ''PolyA 'toFType 'fromFType

-- |Used to match either an InputController of a Monitor. You should probably
-- use the pattern instead.
inputControllerOrMonitor :: TilerF a -> Maybe (Maybe b -> TilerF b, Maybe a)
inputControllerOrMonitor (InputController a) = Just (InputController, a)
inputControllerOrMonitor (Monitor a) = Just (Monitor, a)
inputControllerOrMonitor _ = Nothing

-- If you get panics in GHC try commenting these out. ¯\_(ツ)_/¯
-- They're extremely useful though as they tell GHC not to worry about incomplete
-- pattern matches. I think this will be fixed in 8.8.2 or 8.10.
-- {-# COMPLETE Horiz, Floating, Reflect, FocusFull, Wrap, InputControllerOrMonitor :: TilerF #-}
-- {-# COMPLETE Horiz, Floating, Reflect, FocusFull, Wrap, Monitor, InputController :: TilerF #-}
-- I'm not sure why, but the explicit forall is required...

-- |The pattern used to match the function from above.
pattern InputControllerOrMonitor :: forall a b.
  (Maybe b -> TilerF b) -> Maybe a -> TilerF a
pattern InputControllerOrMonitor c a
  <- (inputControllerOrMonitor -> Just (c, a))

-- |A function which can make any Subtiler, even those that don't exist, into a
-- real Tiler.
type Reparenter = Maybe SubTiler -> Tiler

-- |A Tiler that no longer sits in the tree. As a result of being removed from
-- the tree, it could have been reduced to nothing.
type Unparented = Maybe Tiler

