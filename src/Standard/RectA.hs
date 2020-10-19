{-# LANGUAGE TemplateHaskell #-}

module Standard.RectA where

import BasePrelude
import Data.Bifunctor.TH

-- | A rectangle with any kind of dimension you could every want. The A stands
--  for abstract. You probably want one of the type synonyms below.
data RectA a b = Rect
  { x :: a,
    y :: a,
    w :: b,
    h :: b
  }
  deriving (Show, Eq)

deriveBifunctor ''RectA

-- | A rectangle over Doubles. This is usually what people think of
--  when you say rectangle.
type Rect = RectA Double Double

-- | A rectangle according to X11.
type XRect = RectA Int32 Word32

-- TODO There must be some way to get the Compiler to make this one...
instance (Num n, Num m) => Semigroup (RectA n m) where
  Rect a1 a2 a3 a4 <> Rect b1 b2 b3 b4 = Rect (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4)

-- And this one as well...
instance (Num n, Num m) => Monoid (RectA n m) where
  mempty = Rect 0 0 0 0
