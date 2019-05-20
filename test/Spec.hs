{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

import           ClassyPrelude
import           Test.QuickCheck
import           Data.Functor.Foldable
import           Types
import           Tiler
import           FocusList
import           Control.Monad.Free
import           Test.QuickCheck.Arbitrary.Generic

main :: IO ()
main = do
  quickCheck alwaysHasController
  quickCheck pushPopInverse

alwaysHasController :: RootTest -> Bool
alwaysHasController (RootTest t) = cata isController t == 1
 where
  isController (InputController a) = a + 1
  isController a                   = sum a

-- The reverse is not true. Should it be?
pushPopInverse :: Int -> FocusedList Int -> Direction -> Focus -> Bool
pushPopInverse i fl dir foc =
  snd (pop (Left dir) (push dir foc i fl))
    == fl
    && snd (pop (Right foc) (push dir foc i fl))
    == fl

addPopInverse :: Fix Tiler -> RootTest -> Direction -> Focus -> Bool
addPopInverse i (RootTest (Fix t)) dir foc =
  snd (popWindow (Left dir) (add dir foc i t))
    == t
    && snd (popWindow (Right foc) (add dir foc i t))
    == t


-- Instances start here
instance Arbitrary Focus where
  arbitrary = genericArbitrary
  shrink    = genericShrink
instance Arbitrary Direction where
  arbitrary = genericArbitrary
  shrink    = genericShrink
instance Arbitrary Axis where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary a => Arbitrary (FocusedList a) where
  arbitrary = do
    ad <- arbitrary
    i  <- choose (0, length ad - 1)
    return $ if null ad then emptyFL else makeFL ad i
  -- shrink = genericShrink

instance Arbitrary a => Arbitrary (Tiler a) where
  arbitrary = sized make
   where
    make i = frequency
      [ (i, resize (i `div` 2) $ liftM2 Directional arbitrary arbitrary)
      , (1, fmap Wrap arbitrary)
      , (1, return EmptyTiler)
      ]

instance Arbitrary (Fix Tiler) where
  arbitrary = Fix <$> arbitrary

newtype RootTest = RootTest (Fix Tiler)
  deriving Show
instance Arbitrary RootTest where
  arbitrary = do
    tiler         <- Fix <$> arbitrary
    (paths, size) <- cata randPath tiler
    distance      <- choose (0, size)
    return $ RootTest $ apo insertTiler (take distance paths, tiler)

   where
    insertTiler ([], prev) = InputController $ Left prev
    insertTiler (d : ds, Fix (Directional a fl)) =
      Directional a
        $ map (\(v, k) -> if k == d then Right (ds, v) else Left v)
        $ zip fl
        $ makeFL [0 ..] 0
    insertTiler _ = error "Shouldn't get to end"

    randPath (Directional _ fl) = if flLength fl == 0
      then return ([], 0)
      else do
        path            <- choose (0, flLength fl - 1)
        (restOfPath, s) <- fromMaybe (error "No") $ indexFL path fl
        return (path : restOfPath, s + 1)
    randPath _ = return ([], 0)
