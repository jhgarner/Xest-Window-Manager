{-# LANGUAGE TemplateHaskell #-}

module Tiler.ManyHelpers where

import           Text.Show.Deriving
import           Data.Eq.Deriving
import           Standard
import           FocusList
import           Tiler.WithRect
import           Tiler.Sized


-- |A GADT which holds the various options that the Many Tiler can be.
data ManyHolder a where
  Horiz :: FocusedList (Sized a) -> ManyHolder a
  Floating :: FocusedList (WithRect a) -> ManyHolder a
  TwoCols :: Double -> FocusedList (Identity a) -> ManyHolder a
  deriving stock (Foldable, Functor, Traversable, Show, Eq)
deriveShow1 ''ManyHolder
deriveEq1 ''ManyHolder


-- |Given a ManyHolder we don't want to pattern match against, apply some mapping.
-- TODO This looks like a job for lenses.
withFl :: (Functor m)
       => ManyHolder a
       -> (forall f. (Traversable f, Comonad f) => FocusedList (f a) -> m (FocusedList (f b)))
       -> m (ManyHolder b)
withFl (Horiz fl) f = Horiz <$> f fl
withFl (Floating fl) f = Floating <$> f fl
withFl (TwoCols d fl) f = TwoCols d <$> f fl


-- |Nearly identical to withFl, but specialized for Identity to cut down on boilerplate.
-- TODO it would be cool if there was something I could toss at this so that it
-- didn't have to be it's own function but also didn't need the annoying
-- runIdentity's everywhere.
withFl' :: ManyHolder a
        -> (forall f. (Traversable f, Comonad f) => FocusedList (f a) -> FocusedList (f b))
        -> ManyHolder b
withFl' mh f = runIdentity $ withFl mh (Identity . f)


-- |Like the above except you can do whatever you want to the FocusedList. The
-- downside is you can't recreate the ManyHolder afterwards.
-- TODO This really looks like a job for lenses. Unfortunately, I run into
-- problems with Impredicative Types when I try to do that. How can I make this
-- a lense without much extra boilderplate
foldFl :: ManyHolder a
        -> (forall f. (Comonad f) => FocusedList (f a) -> b)
        -> b
foldFl (Horiz fl) f = f fl
foldFl (Floating fl) f = f fl
foldFl (TwoCols _ fl) f = f fl

-- |Converts a holder of Floating things into one of horizontal things.
toFloating :: ManyHolder a -> ManyHolder a
toFloating (Horiz fl) = Floating $ map (WithRect (Rect 0 0 500 500) . extract) fl
toFloating (TwoCols _ fl) = Floating $ map (WithRect (Rect 0 0 500 500) . extract) fl
toFloating mh@(Floating _) = mh

-- |Like the above but in reverse.
toHoriz :: ManyHolder a -> ManyHolder a
toHoriz (Floating fl) = Horiz $ map (Sized (1/len) . extract) fl
  where len = fromIntegral $ flLength fl
toHoriz (TwoCols _ fl) = Horiz $ map (Sized (1/len) . extract) fl
  where len = fromIntegral $ flLength fl
toHoriz mh@(Horiz _) = mh

-- |Like the above but in reverse.
toTwoCols :: ManyHolder a -> ManyHolder a
toTwoCols (Floating fl) = TwoCols 0.6 $ map (Identity . extract) fl
toTwoCols (Horiz fl) = TwoCols 0.6 $ map (Identity . extract) fl
toTwoCols mh@(TwoCols _ _) = mh


-- |Mods can be applied to any ManyHolder type.
data ManyMods = Rotate | Full | NoMods
  deriving stock (Show, Eq)
