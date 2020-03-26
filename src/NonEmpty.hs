{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module NonEmpty where

import ClassyPrelude hiding (head, null, toList, fromList)
import Data.Foldable hiding (find)
import Data.Eq.Deriving
import Text.Show.Deriving
import Data.ChunkedZip
import Data.List as All (elemIndex)
import GHC.Exts
import Control.Lens.At
import Control.Lens hiding (index)

data NonEmpty a = NE a [a]
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
deriveShow1 ''NonEmpty
deriveEq1 ''NonEmpty
type instance Element (NonEmpty a) = a
instance MonoFoldable (NonEmpty a)

instance Zip NonEmpty where
  zipWith f (NE a as) (NE b bs) =
    NE (f a b) $ zipWith f as bs

mkNE :: [a] -> Maybe (NonEmpty a)
mkNE as = do
  h <- headMay as
  t <- tailMay as
  return $ NE h t

head :: NonEmpty a -> a
head (NE a _) = a

last :: NonEmpty a -> a
last = foldr1 const

tail :: NonEmpty a -> Maybe (NonEmpty a)
tail (NE _ as) = mkNE as

init :: NonEmpty a -> Maybe (NonEmpty a)
init (NE _ []) = Nothing
init (NE a as)= mkNE $ a : initDef as

append :: NonEmpty a -> [a] -> NonEmpty a
append (NE a as) bs = NE a $ as ++ bs

remove :: Eq a => a -> NonEmpty a -> Maybe (NonEmpty a)
remove b (NE a as) = mkNE $ filter (/= b) $ a : as

removeI :: Int -> NonEmpty a -> Maybe (NonEmpty a)
removeI i (NE a as) = mkNE . map snd . filter ((/= i) . fst) . zip [0 ..] $ a : as

findNe :: Int -> NonEmpty a -> a
findNe 0 (NE a _) = a
findNe i (NE _ as) = fromMaybe (error "Index out of bounds") $ index as (i-1)

findNeI :: Eq a => a -> NonEmpty a -> Int
findNeI b (NE a as) = fromMaybe (error "Not in List") $ elemIndex b $ a : as

filterNe :: (a -> Bool) -> NonEmpty a -> Maybe (NonEmpty a)
filterNe p (NE a as) = mkNE $ filter p $ a : as

mapMaybeNe :: (a -> Maybe b) -> NonEmpty a -> Maybe (NonEmpty b)
mapMaybeNe f (NE a as) = mkNE $ mapMaybe f $ a : as

move :: Eq a => Int -> a -> NonEmpty a -> NonEmpty a
move i b (NE a as) = fromMaybe (error "move died") 
  $ mkNE $ take i filtered ++ [b] ++ drop i filtered
  where filtered = filter (/= b) $ a : as

moveF :: Int -> (a -> Bool) -> NonEmpty a -> NonEmpty a
moveF i b (NE a as) = fromMaybe (error "move died") 
  $ mkNE $ take i filtered ++ [found] ++ drop i filtered
  where filtered = filter (not . b) $ a : as
        found = fromMaybe (error "Can't find for move") $ find b $ a : as

mkMany :: Int -> a -> NonEmpty a
mkMany n = fromMaybe (error "can't make 0 elements") . mkNE . replicate n 

reverseNe :: NonEmpty a -> NonEmpty a
reverseNe (NE a as) = NE newA newAs
  where (newA : newAs) = reverse $ a : as

updateIx :: (a -> a) -> Int -> NonEmpty a -> NonEmpty a
updateIx f 0 (NE a as) = NE (f a) as
updateIx f n (NE a as) = NE a $ over (ix (n-1)) f as

infixr 7 +:
(+:) :: a -> NonEmpty a -> NonEmpty a
b +: (NE a as) = NE b $ a : as

instance IsList (NonEmpty a) where
  type Item (NonEmpty a) = a
  fromList = fromMaybe (error "Must have at least one element") . mkNE
  toList (NE a as) = a : as
