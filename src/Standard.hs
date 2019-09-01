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
    , Transformer (..)
    , overReal
    , overTrans
    , addTrans
    , unTransform
    , journey
    , pattern (:<~)
    , ifM
    ) where

import ClassyPrelude as All hiding (Reader, ask, asks, find, head, tail, init, last, Vector, Index, index)
import Data.Foldable as All (find)
import Control.Lens as All hiding ((:<), (<.>), (<|), _head, _tail, cons, para, snoc, uncons, unsnoc, none)

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

data Transformer a = Transformer (a -> a) (a -> a) a

instance Show a => Show (Transformer a) where
  show (Transformer _ _ a) = "Transformer: " ++ show a

overReal :: (a -> a) -> Transformer a -> Transformer a
overReal f (Transformer trans undo a) = Transformer trans undo $ f a
overTrans :: (a -> a) -> Transformer a -> Transformer a
overTrans f (Transformer trans undo a) = Transformer trans undo . undo . f . trans $ a
addTrans :: (a -> a) -> (a -> a) -> Transformer a -> Transformer a
addTrans input output (Transformer trans undo a) = Transformer (input . trans) (undo . output) a
unTransform :: Transformer a -> a
unTransform (Transformer _ _ a) = a


-- TODO is there some existing abstraction for this?
topDown :: Functor f => (Transformer x -> f (Fix f) -> f (Transformer x, Fix f)) -> Transformer x -> Fix f -> Cofree f (Transformer x)
topDown propagate initial (Fix f) =
    initial :< fmap (uncurry (topDown propagate)) newF
  where newF = propagate initial f

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

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM condM thenM elseM = do
  cond <- condM
  if cond then thenM else elseM
