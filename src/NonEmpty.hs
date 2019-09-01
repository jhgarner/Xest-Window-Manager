{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : NonEmpty
Description : A list which can't be empty
License     : MIT
Stability   : experimental
Portability : POSIX

Why create another nonEmpty list? Well, the one from monotravable only
implements the mono type classes by design. We really need the full
power of Functor though so we can use Recursion Schemes.

The NonEmpty from Base redefines a whole lot of existing names which
is kind of annoying. This module relies on type classes and lenses
instead.

-}
module NonEmpty where

import           ClassyPrelude hiding (head, null, toList, fromList, index, last
                                     , tail)
import           Control.Lens hiding (_head, _tail, filtered)
import           Data.Eq.Deriving
import           Text.Show.Deriving
import           Data.ChunkedZip
import           Data.List as All (elemIndex)
import           GHC.Exts

-- |The actual datatype in question. Head must always exist but tail
-- might be empty.
data NonEmpty a = NE { _head :: a, _tail :: [a] }
-- Begin the massive list of derivings so we can get things for free
  deriving (Show, Eq, Functor, Foldable, Traversable)

deriveShow1 ''NonEmpty

deriveEq1 ''NonEmpty

makeLenses ''NonEmpty

type instance Element (NonEmpty a) = a

instance MonoFoldable (NonEmpty a)

instance Zip NonEmpty where
  zipWith f (NE a as) (NE b bs) = NE (f a b) $ zipWith f as bs

-- |Proof that a nonempty list is equivalent to an element and a normal list.
consNe :: Iso' (NonEmpty a) (a, Maybe (NonEmpty a))

-- What is &&&? Basically, it takes one value and applies two functions to it.
consNe = iso
  (_head &&& (^? tail . usingList))
  (\(a, mNe) -> NE a $ maybe [] (review usingList) mNe)

-- |Proof that a nonempty list is equivalent to an element and a normal list
-- but that element is appended instead of prepended.
snocNe :: Iso' (NonEmpty a) (Maybe (NonEmpty a), a)

-- What's that ^?! mean? We can break it down into parts. First, ^ means "get"
-- , "?" means it might fail, and "!" means nevermind, it won't fail.
snocNe = iso
  (\(NE a as) -> ((a:as) ^? _init . usingList, (a:as) ^?! _last))
  (\(mNe, a) -> maybe [a] ((++ [a]) . review usingList) mNe ^?! usingList)

-- |Convert to and from a list that may be empty.
usingList :: Prism' [a] (NonEmpty a)
usingList = prism' (toListOf traverse)
  $ \as -> do
    h <- headMay as
    t <- tailMay as
    return $ NE h t

-- |Given an element, remove it from the list.
-- TODO is there a typeclass or optic for remove/filter stuff?
remove :: Eq a => a -> NonEmpty a -> Maybe (NonEmpty a)
remove b (NE a as) = filter (/= b) (a:as) ^? usingList

-- |Remove an element at the given index.
-- Preview is synonymous with ^? and might get a value.
-- Why use preview? Since there was function composition happening
-- in the rest of the function, the infix operator would get in
-- the way of which direction you read from.
removeI :: Int -> NonEmpty a -> Maybe (NonEmpty a)
removeI i (NE a as) =
  preview usingList . map snd . filter ((/= i) . fst) . zip [0 ..] $ a:as

-- |Basically just an index function.
-- Why does this error instead of returning a maybe?
-- Only because of how it's used. Any time it fails should
-- crash the program.
findNe :: Int -> NonEmpty a -> a
findNe 0 (NE a _) = a
findNe i (NE _ as) = as ^?! traversed . index (i - 1)

-- |Reverse of the findNe function.
findNeI :: Eq a => a -> NonEmpty a -> Int
findNeI b (NE a as) = fromMaybe (error "Not in List") $ elemIndex b $ a:as

-- |Just the filter function specialized to NonEmpty
filterNe :: (a -> Bool) -> NonEmpty a -> Maybe (NonEmpty a)
filterNe p (NE a as) = filter p (a:as) ^? usingList

-- |Move an element found using some function
moveF :: Int -> (a -> Bool) -> NonEmpty a -> NonEmpty a
moveF i b (NE a as) = fromMaybe (error "move died") . preview usingList
  $ take i filtered ++ [found] ++ drop i filtered
  where
    filtered = filter (not . b) $ a:as

    found = fromMaybe (error "Can't find for move") $ find b $ a:as

-- |Same as above but specialized for convenience.
move :: Eq a => Int -> a -> NonEmpty a -> NonEmpty a
move i a = moveF i (== a)

-- |Like replicate but for Nonempty lists. It would be cool if I could force
-- TODO Force Int to be positive
mkMany :: Int -> a -> NonEmpty a
mkMany n a = replicate n a ^?! usingList

-- |Reverses a nonempty list
reverseNe :: NonEmpty a -> NonEmpty a
reverseNe (NE a as) = NE newA newAs
  -- Technically not all patterns are matched, but reverse of a
  -- list with at least one element will have at least one element.
    where
      (newA:newAs) = reverse $ a:as

instance IsList (NonEmpty a) where
  type Item (NonEmpty a) = a

  fromList = (^?! usingList)

  toList = review usingList
