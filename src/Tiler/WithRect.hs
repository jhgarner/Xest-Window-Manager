{-# LANGUAGE TemplateHaskell #-}

module Tiler.WithRect where

import           Standard
import           Text.Show.Deriving
import           Data.Eq.Deriving

-- |A container where the contents are either on the bottom
-- or are floating on top in some rectangle.
data WithRect a = WithRect Rect a
  deriving (Show, Functor, Foldable, Traversable)

instance Eq a => Eq (WithRect a) where
  (WithRect _ a1) == (WithRect _ a2) = a1 == a2

type instance Element (WithRect a) = a

deriveShow1 ''WithRect

deriveEq1 ''WithRect

instance MonoPointed (WithRect a) where
  opoint = WithRect $ Rect 0 0 300 300


instance Comonad WithRect where
  extract (WithRect _ a) = a

  duplicate = opoint
