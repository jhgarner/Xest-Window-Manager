{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Standard
    ( module All
    , untilM
    , topDown
    , mapFold
    , Beam (..)
    , BeamF (..)
    , Path (..)
    , PathF (..)
    , journey
    , pattern (:<~)
    ) where

import ClassyPrelude as All hiding (Reader, ask, asks, find, head, tail, init, last, Vector)
import Data.Foldable as All (find)

import Polysemy.State
import Polysemy
import Data.List as All (elemIndex)
import Control.Comonad.Cofree as All (Cofree((:<)))
-- import Control.Comonad.Cofree as C
import Control.Comonad as All
import qualified Control.Comonad.Trans.Cofree as C hiding (Cofree)
import Data.Functor.Foldable as All hiding (fold, unfold)
import Data.Fixed as All (mod')
import Data.Functor.Foldable.TH as All
import NonEmpty as All

pattern (:<~) a b = a C.:< b

untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM f m = m >>= \a ->
  if f a then return a else untilM f m

-- TODO is there some existing abstraction for this?
topDown :: Functor f => (x -> f (Fix f) -> (x -> x, f (x, Fix f))) -> x -> Fix f -> Cofree f x
topDown propagate initial (Fix f) =
    initial :< mappedF
  where (mapper, newF) = propagate initial f
        mappedF = fmap (fmap mapper . uncurry (topDown propagate)) newF

data Beam a = End a | Continue (Beam a)
  deriving (Eq, Show, Functor)

makeBaseFunctor ''Beam

data Path a b = Finish a | Road (Path a b) | Break b (Path a b)
  deriving (Eq, Show, Functor)

makeBaseFunctor ''Path

journey :: Path a b -> ([b], a)
journey = cata step
  where step (FinishF a) = ([], a)
        step (BreakF b (bs, a)) = (b:bs, a)
        step (RoadF result) = result

instance Comonad Beam where
  extract = cata getEnd
    where getEnd (EndF a)      = a
          getEnd (ContinueF a) = a
  duplicate = End

mapFold :: Traversable t => (acc -> a -> (acc, b)) -> acc -> t a -> t b
mapFold f i ta = snd . run $ runState i $ traverse (\a -> Polysemy.State.get >>= \acc -> let (newAcc, newA) = f acc a in put newAcc >> return newA) ta
