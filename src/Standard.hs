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
    , allInList
    , anyInList
    , Void
    , absurd
    , DeferedList(..)
    , DeferedListF(..)
    , undefer
    , deferred
    , fixable
    , maybeFixable
    , makeRight
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
import Data.Functor.Foldable as All hiding (fold, unfold, embed)
import Data.Fixed as All (mod')
import Data.Kind (Type)
import Data.Functor.Foldable.TH as All
import NonEmpty as All
import Data.Monoid as All (Alt(..), getAlt, getFirst)
import Data.Coerce as All

{-# COMPLETE (:<~) #-}
pattern (:<~) :: forall (f :: Type -> Type) a b. a -> f b -> C.CofreeF f a b
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

allInList :: (MonoFoldable f, Element f ~ (a -> Bool)) => f -> a -> Bool
allInList ls l = all ($ l) ls

anyInList :: (MonoFoldable f, Element f ~ (a -> Bool)) => f -> a -> Bool
anyInList ls l = all ($ l) ls
-- Thanks void package on Hackage
newtype Void = Void Void
absurd :: Void -> a
absurd a = a `seq` spin a where
   spin (Void b) = spin b


data DeferedList a = DNil | DCons a (DeferedList a) | DActive (DeferedList a)
makeBaseFunctor ''DeferedList

undefer :: DeferedList a -> [a]
undefer = either (const []) id . cata undefer'
  where undefer' DNilF = Left []
        undefer' (DConsF a (Left as)) = Left $ a : as
        undefer' (DConsF _ (Right as)) = Right as
        undefer' (DActiveF (Left as)) = Right as
        undefer' (DActiveF (Right _)) = error "Double deferred!"

deferred :: DeferedList a -> [a]
deferred = either (const []) reverse . cata undefer'
  where undefer' DNilF = Left []
        undefer' (DConsF _ (Left _)) = Left $ error "Derferred broke"
        undefer' (DConsF a (Right as)) = Right $ a : as
        undefer' (DActiveF (Left _)) = Right []
        undefer' (DActiveF (Right _)) = error "Double deferred!"

fixable :: (Fix f -> Fix f) -> (f (Fix f) -> f (Fix f))
fixable f = unfix . f . Fix

maybeFixable :: (Fix f -> Maybe (Fix f)) -> (f (Fix f) -> Maybe (f (Fix f)))
maybeFixable f = fmap unfix . f . Fix

makeRight :: Either a a -> Either a a
makeRight r@(Right _) = r
makeRight (Left l) = Right l
