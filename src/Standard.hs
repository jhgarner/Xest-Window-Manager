{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
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
    , modify
    , Beam (..)
    , BeamF (..)
    , Path (..)
    , PathF (..)
    , journey
    , Stream (..)
    , filterStream
    , repeatStream
    , overStream
    , pattern CofreeF
    , pattern Cofree
    , RectA(..)
    , Rect
    , XRect
    , Transformation(..)
    , toScreenCoord
    , getStartingPoint
    , trd
    , fromEither
    , map
    , show
    , headMay
    , head
    , tailMay
    , lastMay
    , last
    , initMay
    , removeAt
    , remove
    , error
    , pattern Text
    ) where

import BasePrelude as All hiding (gunfold, log, tail, head, init, last, fmap, map, show, lazy, arr, uncons, index, String, error, left, right, appendFile, getContents, getLine, interact, putStrLn, putStr, readFile, writeFile, filter, (!!), unlines)
import Data.IORef
import qualified BasePrelude
import Base.Effects as All
import Control.Monad.Trans.Maybe
import Data.Text as All (Text, unlines)
import Data.Text.IO as All
import Data.List.NonEmpty as All (filter, nonEmpty, (!!), (<|), tail, init)
-- Hiding Text because I define it below with a Complete pragma
import Data.Text.Lens as All hiding (Text)
import qualified BasePrelude as BP (fmap)
import           Data.IntMap.Strict as All (IntMap, (!), update)
import           Data.Map.Strict as All (Map)
import           Data.Set as All (Set)
import GHC.Stack
import Control.Monad.Zip as All

import Control.Comonad.Cofree as All (Cofree)
import qualified Control.Comonad.Cofree as CC (Cofree((:<)))
import Control.Comonad as All hiding (fmap)
import qualified Control.Comonad.Trans.Cofree as C hiding (Cofree)
import Data.Functor.Foldable as All hiding (fold, unfold, embed)
import Data.Kind (Type, Constraint)
import Data.Functor.Foldable.TH as All
import Data.Bifunctor.TH
import Control.Lens as All hiding (para, none, (<|))
import Control.Monad.Reader
import GHC.TypeLits hiding (Text)
import Control.Monad.State.Strict (StateT(runStateT))
import Capability.State as All hiding (zoom, modify)
import Capability.Sink as All hiding (yield)
import Capability.Source as All

-- instance 
-- runInputIO :: IO i 
-- instance MonadIO newM => Input (IO i) (MonadIO) newM i m a where
--   reduce ioAction m = liftIO ioAction >>

-- TODO I can probably split out a lot of these functions into other places...

{-# COMPLETE CofreeF #-}
pattern CofreeF :: forall (f :: Type -> Type) a b. a -> f b -> C.CofreeF f a b
pattern CofreeF a b = a C.:< b

{-# COMPLETE Cofree #-}
pattern Cofree :: forall (f :: Type -> Type) a. a -> f (CC.Cofree f a) -> CC.Cofree f a
pattern Cofree a b = a CC.:< b

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


data Stream m a = Stream a (m (Stream m a))
  -- deriving (Functor, Foldable, Traversable)

filterStream :: Monad m => (a -> Bool) -> Stream m a -> m (Stream m a)
filterStream p (Stream a m)
  | p a = return $ Stream a (filterStream p =<< m)
  | otherwise = filterStream p =<< m

repeatStream :: Functor m => m a -> m (Stream m a)
repeatStream m = fmap (\a -> Stream a $ repeatStream m) m

overStream :: Monad m => (a -> m b) -> Stream m a -> m c
overStream f (Stream a m) = f a >> m >>= overStream f

-- |Like fst and snd but for the third element.
trd :: (a, b, c) -> c
trd (_, _, c) = c

fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

map :: Functor f => (a -> b) -> (f a -> f b)
map = BP.fmap

show :: Show a => a -> Text
show = Text . BasePrelude.show

headMay :: Cons s s a a => s -> Maybe a
headMay = preview _head

head :: Traversable1 t => t a -> a
head = view head1

tailMay :: Cons s s a a => s -> Maybe s
tailMay = preview _tail

-- TODO There must ba a typeclass for this...
-- tail :: Traversable1 t => NonEmpty a -> Maybe (NonEmpty a)

lastMay :: Snoc s s a a => s -> Maybe a
lastMay = preview _last

last :: Traversable1 t => t a -> a
last = view last1

initMay :: Snoc s s a a => s -> Maybe s
initMay = preview _init

-- init :: Traversable1 t => t a -> Maybe (t a)

removeAt :: Int -> NonEmpty a -> Maybe (NonEmpty a)
removeAt i = nonEmpty . map snd . filter (\(i', _) -> if i == i' then False else True) . mzip [0..]

remove :: Eq a => a -> NonEmpty a -> Maybe (NonEmpty a)
remove a = nonEmpty . filter (/= a)

error :: HasCallStack => Text -> a
error (Text s) = BasePrelude.error s


{-# COMPLETE Text #-}
pattern Text :: BasePrelude.String -> Text
pattern Text a <- (view _Text -> a) where
  Text a = review _Text a

modify :: forall a m. HasState a a m => (a -> a) -> m ()
modify = modify' @a
