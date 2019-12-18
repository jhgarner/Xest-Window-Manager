{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}


module Tiler.Reduce where

import           Standard hiding (assert)
import           Types
import           FocusList
import GDP
import Data.Kind

data ReducedTiler t = ReducedTiler t | EmptyTiler

-- map' :: (a -> b) -> AnyReduced a -> AnyReduced b
-- map' f (AnyReduced (ReducedTiler (The a))) = AnyReduced $ ReducedTiler $ defn $ f a

newtype IsTiler name = IsTiler Defn
type role IsTiler nominal

newtype HasIC name = HasIC Defn
type role HasIC nominal

data Related a b = forall name. Related (a name) (b name)

newtype Child name = Child Defn
type role Child nominal

newtype Cata name = Cata Defn
type role Cata nominal

data SomeName a = forall n. SomeName (Named n a)
addName :: SomeName a -> forall n. Named n a
addName (SomeName a) = coerce a

cataP :: forall (name :: Type) proof f a . (Functor f, Coercible name Defn)
      => (forall n. f (a ? proof) ~~ n -> a ~~ Cata n ::: proof (Cata n)) -> Fix f -> a ~~ Cata name ::: proof (Cata name)
cataP f = f @name . defn . fmap (unname . cataP @(Child name) f) . unfix

paraP :: forall (name :: Type) proof f a . (Functor f, Coercible name Defn)
      => (forall n. f (Fix f, a ? proof) ~~ n -> a ~~ Cata n ::: proof (Cata n)) -> Fix f -> a ~~ Cata name ::: proof (Cata name)
paraP f = f @name . defn . fmap (id &&& (unname . paraP @(Child name) f)) . unfix

solveReduced :: (ReducedTiler (Tiler (Fix Tiler)) ? HasIC) -> (Tiler (Fix Tiler) ? HasIC)
solveReduced (The (ReducedTiler t)) = assert t
solveReduced _ = error "Axiom for solveReduced didn't hold!"

isNonEmptyTiler :: ReducedTiler t -> Bool
isNonEmptyTiler EmptyTiler = False
isNonEmptyTiler _ = True

-- TODO Remove this partial function
fromNonempty :: ReducedTiler t -> t
fromNonempty (ReducedTiler t) = t

maybeToReduced :: Maybe t -> ReducedTiler t
maybeToReduced = maybe EmptyTiler ReducedTiler

reducedToMaybe :: ReducedTiler t -> Maybe t
reducedToMaybe (ReducedTiler t) = Just $ t
reducedToMaybe _ = Nothing


mapFix :: ReducedTiler (Tiler (Fix Tiler)) -> ReducedTiler (Fix Tiler)
mapFix (ReducedTiler (t)) = ReducedTiler $ Fix t
mapFix EmptyTiler = EmptyTiler

mapunFix :: ReducedTiler ((Fix Tiler)) -> ReducedTiler (Tiler (Fix Tiler))
mapunFix (ReducedTiler (t)) = ReducedTiler $ unfix t
mapunFix EmptyTiler = EmptyTiler

-- | Removes empty Tilers
reduce :: (Tiler (ReducedTiler (Fix Tiler)) ~~ IsTiler n) -> ReducedTiler (Tiler (Fix Tiler)) ~~ IsTiler m
-- Yo dog, I head you like fmaps
reduce = defn . maybeToReduced . \case
  The (Horiz fl) -> fmap Horiz $ fmap (fmap fromNonempty) <$> flFilter (isNonEmptyTiler . getItem) fl
  The (Floating ls) -> newTiler ls
  The (InputControllerOrMonitor c (Just (ReducedTiler t))) -> Just . c $ Just t
  The (InputControllerOrMonitor _ _) -> Nothing
  The (Reflect ((ReducedTiler (t)))) -> Just $ Reflect t
  The (Reflect (EmptyTiler)) -> Nothing
  The (FocusFull ((ReducedTiler (t)))) -> Just $ Reflect t
  The (FocusFull (EmptyTiler)) -> Nothing
  The (Wrap w) -> Just $ Wrap w
  _ -> error "t"
  where newTiler ls = Floating . fmap (fmap fromNonempty) <$> newFront ls
        newFront = filterNe (isNonEmptyTiler . getEither)
