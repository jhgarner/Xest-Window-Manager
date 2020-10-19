module Tiler.TreeCombo where

import Standard
import Tiler.TilerTypes
import Prelude (show)

-- | TreeCombo is used when trying to find the deepest common parent of two
-- elements. One of the elements is considered unmovable. The other is movable
-- and should be moved so that the unmovable one is a descendent of it.
--
-- The code that actually implements that is in Tiler.hs
--
-- This type kind of looks like a diamond.
--          Both
--         /    \
-- Unmovable     Movable
--         \    /
--         Neither
--
-- A type of TreeCombo starts as Neither and moves towards both. You should not
-- move back down the tree though. I think this forms a Monoid.
data TreeCombo
  = -- | There's nothing special about this TreeCombo... yet.
    Neither
  | -- | We've found the thing we're looking for which should not be moved.
    Unmovable
  | -- | We found the thing that can move. The Unparented is the children of
    -- what we found and Reparenter is a function to add the thing back into
    -- the tree.
    Movable (Reparenter, Unparented)
  | -- | We've already found both of the things we're looking for.
    Both

-- Gets the Movable parameters if the TreeCombo is Movable
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
