{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
--   TilerF and its associated types are probably some of the most important in
--   the project. Understanding them is important to understanding how Xest works
--   as a whole. If you're unfamiliar with recursion schemes, definitely read up
--   on those. Learning about that library and reading
--   https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html
--   has probably changed how I write and think about code more than most of the
--   other concepts used in this project.
module Tiler.TilerTypes where

import Data.Eq.Deriving
import Data.Functor.Foldable (embed)
import Data.Kind
import qualified SDL
import Standard
import TH
import Text.Show.Deriving
import Tiler.ManyHelpers
import Tiler.ParentChild

type Borders = (SDL.Window, SDL.Window, SDL.Window, SDL.Window)

-- | The tree nodes which make up the bulk of the program. Each
-- constructor provides a way of composing windows. The a type variable is
-- there so we can use recursion schemes on this data type. Every use of a can
-- be thought of as a branch in the tree.
data TilerF a
  = -- | Holds many elements. The instance of ManyHolder and the chosen Mods
    --  decides how this one works. The ManyHolder options are to tile
    --  horizontally or floating. The mods let you rotate the tiling (aka make a
    --  vertical tiler) or make the focused Tiler full screen.
    ManyF (ManyHolder a) ManyMods
  | -- | The leaf of our Tiler tree. A WrapF holds a window (specifically a window
    --  and its parent) and does nothing else.
    WrapF ParentChild
  | -- | This data type controls where Actions and XEvents happen. For example,
    --  when a new window gets created, it gets placed as a child of whatever
    --  comes immediately after the InputController. Unlike most other Tilers,
    --  this one can be empty.
    InputControllerF Borders (Maybe a)
  | -- | Monitor decides where to start rendering. Only children of Monitor get
    --  rendered. Just like InputController, Monitor can be empty. The list of
    --  windows in Monitor are the list of "unmanaged" windows in that they
    --  don't exist in the tree.
    MonitorF XRect (Maybe a)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

deriveShow1 ''TilerF
deriveEq1 ''TilerF

type instance Base (TilerF (Fix TilerF)) = TilerF

instance Recursive (TilerF (Fix TilerF)) where
  project = coerce

instance Corecursive (TilerF (Fix TilerF)) where
  embed = coerce

-- | Used to make type signatures easier to read. A SubTiler is a Tiler that
--  will be a child of another.
type SubTiler = Fix TilerF

-- | Another type signature helper. This type represents your standard,
--  recursive Tiler.
type Tiler = TilerF (Fix TilerF)

-- | This class represents things that can be transformed to and from
--  the TilerF data type. It exists so we can ignore whether something actually
--  needs to be Fixed.
class TilerLike a where
  type PolyA a :: Type

  toFType :: a -> TilerF (PolyA a)
  fromFType :: TilerF (PolyA a) -> a

-- | A trivial instance. TilerF can be transformed into itself.
instance TilerLike (TilerF a) where
  type PolyA (TilerF a) = a

  toFType = id

  fromFType = id

-- | The interesting instance. A Fix TilerF can be coerced to and from
--  TilerF.
instance TilerLike (Fix TilerF) where
  type PolyA (Fix TilerF) = Fix TilerF

  toFType = coerce

  fromFType = coerce

-- | Generate some smart patterns that can match both TilerF and (Fix TilerF).
makeSimpleBase ''TilerF ''TilerLike ''PolyA 'toFType 'fromFType

-- | Used to match either an InputController of a Monitor. You should probably
--  use the pattern instead.
inputControllerOrMonitor :: TilerF a -> Maybe (Maybe b -> TilerF b, Maybe a)
inputControllerOrMonitor (InputController bords a) =
  Just (InputController bords, a)
inputControllerOrMonitor (Monitor loc a) = Just (Monitor loc, a)
inputControllerOrMonitor _ = Nothing

-- | The pattern used to match the function from above.
pattern InputControllerOrMonitor ::
  forall a b.
  (Maybe b -> TilerF b) ->
  Maybe a ->
  TilerF a
pattern InputControllerOrMonitor c a <-
  (inputControllerOrMonitor -> Just (c, a))

-- Since we're using pattern synonyms, Haskell can't figure out if any given
-- case expression is total. This tells Haskell which pattern synonyms are
-- needed to make it total.
{-# COMPLETE Many, Wrap, InputControllerOrMonitor :: TilerF #-}

{-# COMPLETE Many, Wrap, Monitor, InputController :: TilerF #-}

-- These aren't super important and just make some type signatures nicer.

-- | A function which can make any Subtiler, even those that don't exist, into a
--  real Tiler.
type Reparenter = Maybe SubTiler -> Tiler

-- | A Tiler that no longer sits in the tree. As a result of being removed from
--  the tree, it could have been reduced to nothing.
type Unparented = Maybe Tiler

-- | Each screen has an index and an associated Tiler.
type Screens = IntMap Tiler
