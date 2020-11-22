{-# LANGUAGE TemplateHaskell #-}

module Standard.Transformation where

import BasePrelude
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Standard.RectA

-- | Some transformations you might want to make to a rectangle.
--  Instead of just doing the transformation with something like:
--
--  "slide :: Rect -> Rect -> Rect"
--
--  we create a list of transformations and wait to apply them until the
--  last moment. This lets our drawing functions inspect how they're going
--  to be transformed.
data Transformation = Slide Rect Transformation | Spin Transformation | StartingPoint XRect
  deriving (Eq, Show)

makeBaseFunctor ''Transformation

idTransform :: Transformation
idTransform = StartingPoint $ Rect 0 0 0 0

-- | Actually does the computations to create a new rectangle.
toScreenCoord :: Transformation -> XRect
toScreenCoord =
  bimap floor ceiling . snd . cata \case
    SlideF (Rect dx dy dw dh) (False, Rect {..}) ->
      (False, Rect (x + dx * w) (y + dy * h) (w * dw) (h * dh))
    SlideF (Rect dx dy dw dh) (True, Rect {..}) ->
      (False, Rect (x + dy * w) (y + dx * h) (w * dh) (h * dw))
    SpinF (_, Rect {..}) -> (True, Rect x y w h)
    StartingPointF r -> (False, bimap fromIntegral fromIntegral r)

-- | Extracts the original untransformed rectangle.
getStartingPoint :: Transformation -> XRect
getStartingPoint = cata \case
  StartingPointF r -> r
  SpinF r -> r
  SlideF _ r -> r
