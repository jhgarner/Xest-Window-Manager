{-# LANGUAGE TemplateHaskell #-}

module Tiler.WithRect where

import Data.Eq.Deriving
import Standard
import Text.Show.Deriving

-- | A container where the contents are either on the bottom
--  or are floating on top in some rectangle.
data WithRect a = WithRect Rect a
  deriving (Show, Functor, Foldable, Traversable)

instance Eq a => Eq (WithRect a) where
  (WithRect _ a1) == (WithRect _ a2) = a1 == a2

deriveShow1 ''WithRect

deriveEq1 ''WithRect

instance Comonad WithRect where
  extract (WithRect _ a) = a

  duplicate r@(WithRect rect _) = WithRect rect r
