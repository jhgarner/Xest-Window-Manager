{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}

import           Standard
import           Test.QuickCheck
import           Data.Functor.Foldable
import           FocusList
import           Control.Monad.Free
import           Test.QuickCheck.Arbitrary.Generic

main :: IO ()
main = do
  quickCheck pushKeepsValid
  quickCheck makeFLWorks
  quickCheck focusWorks

-- alwaysHasController :: RootTest -> Bool
-- alwaysHasController (RootTest t) = cata isController t == 1
--  where
--   isController (InputController a) = a + 1
--   isController a                   = sum a


pushKeepsValid :: Int -> Direction -> Focus -> FocusedList Int -> Property
pushKeepsValid a dir foc fl =
  isValid $ push dir foc a fl

makeFLWorks :: FAndIX NonEmpty Int -> Property
makeFLWorks (FAndIX na i) =
  let fl = makeFL na i
   in isValid fl .&&. visualOrder fl === [0..length na-1] .&&.
        head (focusOrder fl) === visualOrder fl !! i

focusWorks :: FAndIX FocusedList Int -> Property
focusWorks (FAndIX ogfl i) =
  let fl = focusIndex i ogfl
   in isValid fl .&&. (head (focusOrder fl) === i)

isValid :: FocusedList a -> Property
isValid (FL vo fo ad) = label "Is valid" $
  sort (toList vo) === [0..length ad-1] .&&.
  sort (toList fo) === [0..length ad-1]

-- Instances start here
instance Arbitrary Focus where
  arbitrary = genericArbitrary
  shrink    = genericShrink
instance Arbitrary Direction where
  arbitrary = genericArbitrary
  shrink    = genericShrink
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = genericArbitrary
  shrink    = genericShrink
instance Arbitrary a => Arbitrary (FocusedList a) where
  arbitrary = do
    ad <- arbitrary
    vs <- fromJust . nonEmpty <$> shuffle [0..length ad-1]
    fs <- fromJust . nonEmpty <$> shuffle [0..length ad-1]
    return $ FL vs fs ad

data FAndIX f a = FAndIX (f a) Int
  deriving Show

instance (Foldable f, Arbitrary (f a)) => Arbitrary (FAndIX f a) where
  arbitrary = do
    f <- arbitrary
    i <- choose (0, length f-1)
    return $ FAndIX f i

-- instance Arbitrary a => Arbitrary (Tiler a) where
--   arbitrary = sized make
--    where
--     make i = frequency
--       [ (i, resize (i `div` 2) $ liftM2 Directional arbitrary arbitrary)
--       , (1, fmap Wrap arbitrary)
--       , (1, return EmptyTiler)
--       ]

-- instance Arbitrary (Fix Tiler) where
--   arbitrary = Fix <$> arbitrary

-- newtype RootTest = RootTest (Fix Tiler)
--   deriving Show
-- instance Arbitrary RootTest where
--   arbitrary = do
--     tiler         <- Fix <$> arbitrary
--     (paths, size) <- cata randPath tiler
--     distance      <- choose (0, size)
--     return $ RootTest $ apo insertTiler (take distance paths, tiler)

--    where
--     insertTiler ([], prev) = InputController $ Left prev
--     insertTiler (d : ds, Fix (Directional a fl)) =
--       Directional a
--         $ map (\(v, k) -> if k == d then Right (ds, v) else Left v)
--         $ zip fl
--         $ makeFL [0 ..] 0
--     insertTiler _ = error "Shouldn't get to end"

--     randPath (Directional _ fl) = if flLength fl == 0
--       then return ([], 0)
--       else do
--         path            <- choose (0, flLength fl - 1)
--         (restOfPath, s) <- fromMaybe (error "No") $ indexFL path fl
--         return (path : restOfPath, s + 1)
--     randPath _ = return ([], 0)
