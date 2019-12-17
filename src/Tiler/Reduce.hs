{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}


module Tiler.Reduce where

import           Standard
import           Types
import           FocusList
import GDP

data ReducedTiler n t = ReducedTiler (t ~~ IsTiler n) | EmptyTiler
data AnyReduced t = forall n. AnyReduced (ReducedTiler (IsTiler n) t)

-- map' :: (a -> b) -> AnyReduced a -> AnyReduced b
-- map' f (AnyReduced (ReducedTiler (The a))) = AnyReduced $ ReducedTiler $ defn $ f a

newtype IsTiler name = IsTiler Defn
type role IsTiler nominal

data HasIC name
solveReduced :: (ReducedTiler n (Tiler (Fix Tiler)) ::: HasIC n) -> (Tiler (Fix Tiler) ~~ IsTiler n ::: HasIC n)
solveReduced (exorcise -> ReducedTiler t) = t ... axiom
solveReduced _ = error "Axiom for solveReduced didn't hold!"

isNonEmptyTiler :: AnyReduced t -> Bool
isNonEmptyTiler (AnyReduced EmptyTiler) = False
isNonEmptyTiler _ = True

-- TODO Remove this partial function
fromNonempty :: AnyReduced t -> t
fromNonempty (AnyReduced (ReducedTiler t)) = the t

maybeToReduced :: Maybe (t ~~ IsTiler n) -> ReducedTiler n t
maybeToReduced = maybe EmptyTiler ReducedTiler

reducedToMaybe :: ReducedTiler n t -> Maybe t
reducedToMaybe (ReducedTiler t) = Just $ the t
reducedToMaybe _ = Nothing


mapFix :: ReducedTiler n (Tiler (Fix Tiler)) -> ReducedTiler n (Fix Tiler)
mapFix (ReducedTiler (The t)) = ReducedTiler $ defn $ Fix t
mapFix EmptyTiler = EmptyTiler

-- | Removes empty Tilers
reduce :: (Tiler (AnyReduced (Fix Tiler)) ~~ IsTiler n) -> ReducedTiler (IsTiler m) (Tiler (Fix Tiler))
-- Yo dog, I head you like fmaps
reduce = maybeToReduced . \case
  The (Horiz fl) -> fmap (defn . Horiz) $ fmap (fmap fromNonempty) <$> flFilter (isNonEmptyTiler . getItem) fl
  The (Floating ls) -> newTiler ls
  The (InputControllerOrMonitor c (Just (AnyReduced (ReducedTiler (The t))))) -> Just . defn . c $ Just t
  The (InputControllerOrMonitor _ _) -> Nothing
  The (Reflect (AnyReduced (ReducedTiler (The t)))) -> Just $ defn $ Reflect t
  The (Reflect (AnyReduced EmptyTiler)) -> Nothing
  The (FocusFull (AnyReduced (ReducedTiler (The t)))) -> Just $ defn $ Reflect t
  The (FocusFull (AnyReduced EmptyTiler)) -> Nothing
  The (Wrap w) -> Just $ defn $ Wrap w
  _ -> error "t"
  where newTiler ls = defn . Floating . fmap (fmap fromNonempty) <$> newFront ls
        newFront = filterNe (isNonEmptyTiler . getEither)
