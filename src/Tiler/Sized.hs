{-# LANGUAGE TemplateHaskell #-}

module Tiler.Sized where

import           Standard
import           Text.Show.Deriving
import           Data.Eq.Deriving

-- |The sized datatype stores some element a and its
-- size relative to something external.
data Sized a = Sized { getSize :: Double, getItem :: a }
  deriving (Show, Functor, Foldable, Traversable, Generic)

deriveShow1 ''Sized

deriveEq1 ''Sized

-- |The size is irrelevant when checking for equality.
instance Eq a => Eq (Sized a) where
  (Sized _ a) == (Sized _ b) = a == b
