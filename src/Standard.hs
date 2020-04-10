{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}

module Standard
    ( module All
    , mapFold
    , Beam (..)
    , BeamF (..)
    , Path (..)
    , PathF (..)
    , journey
    , pattern (:<~)
    , Void
    , absurd
    , RectA(..)
    , Rect
    , XRect
    , Transformation(..)
    , toScreenCoord
    , getStartingPoint
    , trd
    , fromEither
    , onBoth
    , oFind
    ) where

import ClassyPrelude as All hiding (Reader, ask, asks, find, head, tail, init, last, Vector, log, fromEither)
import Data.Foldable as All (find)
import           Colog.Polysemy as All
import Data.Monoid as All (Sum(..))

import Polysemy.State
import Polysemy
import Data.List as All (elemIndex)
import Control.Comonad.Cofree as All (Cofree((:<)))
-- import Control.Comonad.Cofree as C
import Control.Comonad as All
import qualified Control.Comonad.Trans.Cofree as C hiding (Cofree)
import Data.Functor.Foldable as All hiding (fold, unfold, embed)
import Data.Fixed as All (mod')
import Data.Kind (Type)
import Data.Functor.Foldable.TH as All
import NonEmpty as All
-- import Data.List.NonEmpty as All (NonEmpty(..), nonEmpty)
import Data.Monoid as All (Alt(..), getAlt, getFirst)
import Data.Coerce as All
import Data.Bifunctor.TH
import Data.Bifunctor as All (bimap)
import Control.Monad.Loops as All (untilM_, iterateWhile)



-- TODO I can probably split out a lot of these functions into other places...

-- | Since Cofree already takes :<, this pattern gives :<~ to CofreeF.
-- The symbol name was chosen completely randomly.
{-# COMPLETE (:<~) #-}
pattern (:<~) :: forall (f :: Type -> Type) a b. a -> f b -> C.CofreeF f a b
pattern (:<~) a b = a C.:< b

-- |A rectangle with any kind of dimension you could every want.
data RectA a b = Rect { x :: a
                      , y :: a
                      , w :: b
                      , h :: b
                      }
  deriving (Show, Eq)
deriveBifunctor ''RectA

-- |A rectangle over Doubles. This is usually what people think of
-- when you say rectangle.
type Rect = RectA Double Double

-- |A rectangle according to X11.
type XRect = RectA Int32 Word32

-- There must be some way to get the Compiler to make this one...
instance (Num n, Num m) => Semigroup (RectA n m) where
  Rect a1 a2 a3 a4 <> Rect b1 b2 b3 b4 = Rect (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4)

-- And this one as well...
instance (Num n, Num m) => Monoid (RectA n m) where
  mempty = Rect 0 0 0 0

-- |Some transformations you might want to make to a rectangle.
-- Instead of just doing the transformation with something like:
--
-- "slide :: Rect -> Rect -> Rect"
--
-- we create a list of transformations and wait to apply them until the
-- last moment. This lets our drawing functions inspect how they're going
-- to be transformed.
data Transformation = Slide Rect Transformation | Spin Transformation | StartingPoint XRect
  deriving (Eq, Show)

makeBaseFunctor ''Transformation

-- |Actually does the computations to create a new rectangle.
toScreenCoord :: Transformation -> XRect
toScreenCoord = bimap floor ceiling . snd . cata \case
  SlideF (Rect dx dy dw dh) (False, Rect {..}) ->
    (False, Rect (x + dx * w) (y + dy * h) (w * dw) (h * dh))
  SlideF (Rect dx dy dw dh) (True, Rect {..}) ->
    (False, Rect (x + dy * w) (y + dx * h) (w * dh) (h * dw))
  SpinF (_, Rect {..}) -> (True, Rect x y w h)
  StartingPointF r -> (False, bimap fromIntegral fromIntegral r)

-- |Extracts the original untransformed rectangle.
getStartingPoint :: Transformation -> XRect
getStartingPoint = cata $ \case
  StartingPointF r -> r
  SpinF r -> r
  SlideF _ r -> r


-- |You can think of a beam as the opposite of a list. instead of having
-- 0 or more elements and a guaranteed empty case, Beam has 0 or more
-- empty cases and a guaranteed element wrapped inside.
--
-- You might be thinking, "This looks completely useluss!" and if we think of
-- it as a normal container (like array, tree, list, etc.) then you would
-- probably be right. If you think of Beam not as a container but as control
-- flow, you get some cool results though. Imagine wanting to find the smallest
-- element in a tree. Instead of doing it recursively, you can use a
-- hylomorphism to abstract away the recursion. At this point though, you need
-- to pick a data type for F wich will wrap the intermediate result.  What do
-- you pick? Well Beam makes a great choice. For each recursive call, you just
-- return Continue. Once you reach the leaf, you return End. Then, you can use
-- a simple catamorphism to extract the value from the beam.
data Beam a = End a | Continue (Beam a)
  deriving (Eq, Show, Functor)

makeBaseFunctor ''Beam

-- TBH I only really wanted this for the extract function. I think it follows
-- all of the laws but don't quote me on that...
instance Comonad Beam where
  extract = cata getEnd
    where getEnd (EndF a)      = a
          getEnd (ContinueF a) = a
  duplicate = End

-- Path looks a lot like Beam and List combined. Not only do you have some
-- finish value, you also have intermediate ones you can store. Path uses a fun
-- travelling metaphor to explain its constructors.
data Path a b = Finish a | Road (Path a b) | Break b (Path a b)
  deriving (Eq, Show, Functor)

makeBaseFunctor ''Path

-- |Turn a Path into a list of stops and a final destination.
journey :: Path a b -> ([b], a)
journey = cata step
  where step (FinishF a) = ([], a)
        step (BreakF b (bs, a)) = (b:bs, a)
        step (RoadF result) = result


-- |I'm not totally sure I need this function. I think I made it because I
-- wanted a version of mapFold which used Polysemy instead of the built in
-- State. TODO Either remove this or add better docs.
mapFold :: Traversable t => (acc -> a -> (acc, b)) -> acc -> t a -> t b
mapFold f i ta = snd . run $ runState i $ traverse (\a -> Polysemy.State.get >>= \acc -> let (newAcc, newA) = f acc a in put newAcc >> return newA) ta


-- |Thanks void package on Hackage!
newtype Void = Void Void

-- |If we have an element of type void, we can do anything.
absurd :: Void -> a
absurd a = a `seq` spin a where
   spin (Void b) = spin b


-- |Like fst and snd but for the third element.
trd :: (a, b, c) -> c
trd (_, _, c) = c

fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

onBoth :: forall k a b d. (k a, k b) => Either a b -> (forall c. (k c) => c -> d) -> d
onBoth e f = either f f e

oFind :: (MonoFoldable c, Element c ~ elem) => (elem -> Bool) -> c -> Maybe elem
oFind p = foldl' (\acc elem -> if p elem then Just elem else acc) Nothing
