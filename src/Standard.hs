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
    , DualList (..)
    , DualListF (..)
    , pattern (:<~)
    ) where

import ClassyPrelude as All hiding (Reader, ask, asks, find)
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

pattern (:<~) a b = a C.:< b

untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM f m = m >>= \a ->
  if f a then return a else untilM f m

-- TODO is there some existing abstraction for this?
topDown :: Functor f => (x -> f (Fix f) -> f (x, Fix f)) -> x -> Fix f -> Cofree f x
topDown propagate initial (Fix f) =
    initial :< fmap (uncurry $ topDown propagate) (propagate initial f)

data DualList a = End a | Continue (DualList a)
  deriving (Eq, Show, Functor)

makeBaseFunctor ''DualList

instance Comonad DualList where
  extract = cata getEnd
    where getEnd (EndF a)      = a
          getEnd (ContinueF a) = a
  duplicate = End

mapFold :: Traversable t => (acc -> a -> (acc, b)) -> acc -> t a -> t b
mapFold f i ta = snd . run $ runState i $ traverse (\a -> get >>= \acc -> let (newAcc, newA) = f acc a in put newAcc >> return newA) ta
