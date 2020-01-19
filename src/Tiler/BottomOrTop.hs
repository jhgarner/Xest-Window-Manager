{-# LANGUAGE TemplateHaskell #-}

module Tiler.BottomOrTop where

import           Standard
import           Text.Show.Deriving
import           Data.Eq.Deriving

-- |A container where the contents are either on the bottom
-- or are floating on top in some rectangle.
data BottomOrTop a = Bottom a
                   | Top (Rect, a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

deriveShow1 ''BottomOrTop

deriveEq1 ''BottomOrTop

instance Comonad BottomOrTop where
  extract (Bottom a) = a
  extract (Top (_, a)) = a

  duplicate = Bottom

-- |Ignore whether its top or bottom, just get the contained value.
getEither :: BottomOrTop a -> a
getEither (Bottom a) = a
getEither (Top (_, a)) = a
