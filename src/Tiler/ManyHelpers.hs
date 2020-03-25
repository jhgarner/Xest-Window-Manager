{-# LANGUAGE TemplateHaskell #-}

module Tiler.ManyHelpers where

import           Text.Show.Deriving
import           Data.Eq.Deriving
import           Data.Functor.Classes
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
       -> (forall f. (Element (f b) ~ b, MonoPointed (f b), Traversable f, Eq1 f) => FocusedList (f a) -> m (FocusedList (f b)))
       -> m (ManyHolder b)
withFl (Horiz fl) f = Horiz <$> f fl
withFl (Floating fl) f = Floating <$> f fl
withFl (TwoCols d fl) f = TwoCols d <$> f fl


-- |Nearly identical to withFl, but specialized for Identity to cut down on boilerplate.
-- TODO it would be cool if there was something I could toss at this so that it
-- didn't have to be it's own function but also didn't need the annoying
-- runIdentity's everywhere.
withFl' :: ManyHolder a
        -> (forall f. (Element (f b) ~ b, MonoPointed (f b), Traversable f, Eq1 f) => FocusedList (f a) -> FocusedList (f b))
        -> ManyHolder b
withFl' mh f = runIdentity $ withFl mh (Identity . f)

-- |Like WithFl' expect it requires that the type "a" have Eq on it.
-- TODO Having to duplicate the entire function just to add an Eq constraint
-- seems wrong. Why can't I use Eq1? The reason I need Eq is because other
-- functions depend on Eq (f a). The problems would go away if I could assert
-- that (Eq1 f, Eq a) => Eq (f a) but then I would have a whole lot of
-- overlapping instances and that seems scary.
withFl'Eq :: Eq a
       => ManyHolder a
       -> (forall f. (Element (f b) ~ b, MonoPointed (f b), Traversable f, Eq (f a)) => FocusedList (f a) -> FocusedList (f b))
       -> ManyHolder b
withFl'Eq (Horiz fl) f = Horiz $ f fl
withFl'Eq (Floating fl) f = Floating $ f fl
withFl'Eq (TwoCols d fl) f = TwoCols d $ f fl

-- |Like the above except you can do whatever you want to the FocusedList. The
-- downside is you can't recreate the ManyHolder afterwards.
-- TODO This really looks like a job for lenses. Unfortunately, I run into
-- problems with Impredicative Types when I try to do that. How can I make this
-- a lense without much extra boilderplate
foldFl :: ManyHolder a
        -> (forall f. (Element (f a) ~ a, MonoPointed (f a), Comonad f) => FocusedList (f a) -> b)
        -> b
foldFl (Horiz fl) f = f fl
foldFl (Floating fl) f = f fl
foldFl (TwoCols _ fl) f = f fl

-- |Converts a holder of Floating things into one of horizontal things.
toFloating :: ManyHolder a -> ManyHolder a
toFloating (Horiz fl) = Floating $ fmap (point . extract) fl
toFloating (TwoCols _ fl) = Floating $ fmap (point . extract) fl
toFloating mh@(Floating _) = mh

-- |Like the above but in reverse.
toHoriz :: ManyHolder a -> ManyHolder a
toHoriz (Floating fl) = Horiz $ fmap (point . extract) fl
toHoriz (TwoCols _ fl) = Horiz $ fmap (point . extract) fl
toHoriz mh@(Horiz _) = mh

-- |Like the above but in reverse.
toTwoCols :: ManyHolder a -> ManyHolder a
toTwoCols (Floating fl) = TwoCols 0.6 $ fmap (point . extract) fl
toTwoCols (Horiz fl) = TwoCols 0.6 $ fmap (point . extract) fl
toTwoCols mh@(TwoCols _ _) = mh


-- |Mods can be applied to any ManyHolder type.
data ManyMods = Rotate | Full | NoMods
  deriving stock (Show, Eq)
