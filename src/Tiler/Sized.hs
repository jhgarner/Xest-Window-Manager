{-# LANGUAGE TemplateHaskell #-}

module Tiler.Sized where

import           Standard
import           Text.Show.Deriving
import           Data.Eq.Deriving

-- |The sized datatype stores some element a and its
-- size relative to something external.
data Sized a = Sized { getSize :: Double, getItem :: a }
  deriving stock (Show, Functor, Foldable, Traversable, Generic)

type instance Element (Sized a) = a

deriveShow1 ''Sized

deriveEq1 ''Sized

-- |The size is irrelevant when checking for equality.
instance Eq a => Eq (Sized a) where
  (Sized _ a) == (Sized _ b) = a == b

instance MonoPointed (Sized a) where
  opoint = Sized 0

instance Comonad Sized where
  extract (Sized _ a) = a

  duplicate = opoint
