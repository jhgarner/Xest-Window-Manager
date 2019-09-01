{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : FocusList
Description : A list which has two orderings
License     : MIT
Stability   : experimental
Portability : POSIX

Horizontal Tilers need to keep track of two things: the visual order and the
focus order. We need to keep both of these orderings up to date as we crud the
various elements.

I am not super happy with how this module turned out and have tried to improve
it in a couple of ways. Ultimately, it seems to work so I am leaving it for new.

-}
-- TODO Should I actually list everything?
module FocusList
    ( FocusedList
    , Direction(..)
    , Focus(..)
    , push
    , pop
    , flFilter
    , flLength
    , vOrder
    , fOrder
    , mapOne
    , focusElem
    , focusIndex
    , focusVIndex
    , findNeFocIndex
    , makeFL
    , focusDir
    , indexFL ) where

import           Standard
import           Data.ChunkedZip
import           Text.Show.Deriving
import           Data.Eq.Deriving

-- I am super unattached to all of the code in this module.
-- If someone has a better way to represent this, I would gladly switch.
-- |Meant to represent the Head and Last on the list when sorted in focus order
data Focus = Focused
           | Unfocused
  deriving (Eq, Generic, Show)

-- |Meant to represent the Head and Last on the list when sorted in visual order
data Direction = Front
               | Back
  deriving (Show, Eq, Generic)

-- |Things that are assumed about a Focused List but aren't proven: 
-- 1. The orders actually point to valid indices (non negative, not greater than
-- the size of actualData, etc.)
-- 2. The lists are of the same size
-- 3. Order doesn't contain duplicates
-- 3. Actual data should probably be a set to prevent duplication
data FocusedList a = FL { visualOrder :: NonEmpty Int
                        , focusOrder :: NonEmpty Int
                        , actualData :: NonEmpty a
                        }
-- Begin deriving the laundry list of things we want to use
  deriving (Eq, Show, Functor, Generic, Foldable, Traversable)

instance MonoFoldable (FocusedList a)

deriveShow1 ''FocusedList

deriveEq1 ''FocusedList

type instance Element (FocusedList a) = a

instance Zip FocusedList where
  zipWith f fl@FL { actualData = ad } FL { actualData = add } =
    fl { actualData = zipWith f ad add }

-- Begin the actual code
-- | Pushes something to different ends of a list.
-- This function is super biased towards Horizontal tilers since
-- direction doesn't always make sense for things like Floating.
push :: Direction -> Focus -> a -> FocusedList a -> FocusedList a
push dir foc a FL { visualOrder = vo, focusOrder = fo, actualData = ad } =
  FL { visualOrder = dirSide
     , focusOrder = focSide
     , actualData = (Just ad, a) ^?! from snocNe
     }
  where
    focSide = if foc == Focused
              then (len, Just fo) ^?! from consNe
              else (Just fo, len) ^?! from snocNe

    dirSide = if dir == Front
              then (len, Just vo) ^?! from consNe
              else (Just vo, len) ^?! from snocNe

    len = length ad

-- | Given an ordering and direction to pop from, pops from the list
-- Like for Push, Direction doesn't always make sense which makes this function
-- a little weird
pop :: forall a.
    Either Direction Focus
    -> FocusedList a
    -> (a, Maybe (FocusedList a))
pop
    (Right isFocused)
    FL { visualOrder = vo, focusOrder = fo, actualData = ad } = case isFocused of
    Focused   -> popLogic consNe
    Unfocused -> popLogic (snocNe . swapped)
  where
    -- Takes an iso which can be used like the consNe iso. The individual
    -- element will be removed and the remaining list becomes the new list.
    popLogic :: Iso' (NonEmpty Int) (Int, Maybe (NonEmpty Int)) -> (a, Maybe (FocusedList a))
    popLogic elemF = (findNe (fo ^?! elemF . to fst) ad, )
      $ do
        tailFo <- fo ^?! elemF . to snd
        filteredVo <- remove (fo ^?! elemF . to fst) vo
        filteredAd <- removeI (fo ^?! elemF . to fst) ad
        return
          FL { visualOrder = filteredVo
              , focusOrder = tailFo
              , actualData = filteredAd
              }
pop (Left direction) FL { visualOrder = vo, focusOrder = fo, actualData = ad } =
  case direction of
    Front -> popLogic consNe
    Back  -> popLogic (snocNe . swapped)
  where
    popLogic :: Iso' (NonEmpty Int) (Int, Maybe (NonEmpty Int))
             -> (a, Maybe (FocusedList a))
    popLogic elemV = (findNe (vo ^?! elemV . to fst) ad, )
      $ do
        tailVo <- vo ^?! elemV . to snd
        filteredFo <- remove (vo ^?! elemV . to fst) fo
        filteredAd <- removeI (vo ^?! elemV . to fst) ad
        return
          FL { visualOrder = tailVo
             , focusOrder = filteredFo
             , actualData = filteredAd
             }

-- |We just removed something from the list which did all sorts of bad things to order.
-- This function fixes the indices so we don't have holes.
reduce :: Int -> FocusedList a -> FocusedList a
reduce removed fl@FL { .. } =
  fl { focusOrder = newL focusOrder, visualOrder = newL visualOrder }
  where
    newL = map
      (\i -> if i > removed
             then i - 1
             else i)

-- | Modify one of the 4 ends
mapOne :: Either Direction Focus -> Traversal' (FocusedList a) a
mapOne
  orderAndEnd
  f
  fl@FL { focusOrder = fo, visualOrder = vo, actualData = ad } =
  sequenceA fl { actualData = case orderAndEnd of
         Left Front      -> mapEnd $ vo ^. head
         Left Back       -> mapEnd $ vo ^. snocNe.to snd
         Right Focused   -> mapEnd $ fo ^. head
         Right Unfocused -> mapEnd $ fo ^. snocNe.to snd
     }
  where
    mapEnd tarfindNeI = map
      (\(i, a) -> if i == tarfindNeI
                  then f a
                  else pure a)
      $ zip [0 ..] ad

-- | Filter a focused list. Unfortunately, filter isn't a typeclass anywhere
flFilter :: (a -> Bool) -> FocusedList a -> Maybe (FocusedList a)
flFilter predicate FL { actualData = ad, visualOrder = vo, focusOrder = fo } =
  fmap (\unwrapped -> foldl' (flip reduce) unwrapped gone) newFL
  where
    newFL = do
      newAd <- filterNe predicate ad
      newVo <- foldM removeFrom vo gone
      newFo <- foldM removeFrom fo gone
      return FL { actualData = newAd, visualOrder = newVo, focusOrder = newFo }

    gone = foldl'
      (\acc (i, a) -> if predicate a
                      then acc
                      else i:acc)
      []
      $ zip [0 ..] ad

    removeFrom = flip remove

flLength :: FocusedList a -> Int
flLength FL { .. } = length actualData

vOrder :: Lens (FocusedList a) (FocusedList b) (NonEmpty a) (NonEmpty b)
vOrder = lens (\FL{..} -> map (`findNe` actualData) visualOrder) fromVis

fOrder :: Lens (FocusedList a) (FocusedList b) (NonEmpty a) (NonEmpty b)
fOrder = lens (\FL{..} -> map (`findNe` actualData) focusOrder) fromFoc

focusElem :: Eq a => a -> FocusedList a -> FocusedList a
focusElem a fl@FL { focusOrder = fo, actualData = ad } = fl { focusOrder }
  where
    focusOrder = move 0 (findNeI a ad) fo

focusIndex :: Int -> FocusedList a -> FocusedList a
focusIndex i fl@FL { focusOrder = fo } =
  fl { focusOrder = if length fo > i
                    then newFo
                    else fo
     }
  where
    newFo = move 0 i fo

focusVIndex :: Int -> FocusedList a -> FocusedList a
focusVIndex i fl@FL { visualOrder = vo } = focusIndex (findNe i vo) fl

findNeFocIndex :: FocusedList a -> Int
findNeFocIndex FL { .. } = findNeI (_head focusOrder) visualOrder

makeFL :: NonEmpty a -> Int -> FocusedList a
makeFL actualData focIndex =
  FL { visualOrder = vo, focusOrder = fo, actualData }
  where
    len = length actualData

    vo = [0 .. len - 1]

    fo = move 0 focIndex vo

focusDir :: Direction -> FocusedList a -> FocusedList a
focusDir dir fl@FL { focusOrder = fo, visualOrder = vo } = fromMaybe fl
  $ case dir of
    Front -> switchF Just $ view (consNe . _2)
    Back  -> switchF (view (consNe . _2)) Just
  where
    switchF finding using = do
      findList <- finding vo
      listFromUsing <- using vo
      newLoc <- find ((== _head fo) . fst) $ zip findList listFromUsing
      return $ focusIndex (snd newLoc) fl

indexFL :: Int -> FocusedList a -> a
indexFL i FL { .. } = findNe i actualData

reconcile :: NonEmpty b -> NonEmpty Int -> FocusedList a -> FocusedList b
reconcile newAs order fl@FL { .. } =
  fl { actualData = foldl' updateAt base $ zip order newAs }
  where
    updateAt as (i, a) = map
      (\old -> if fst old == i
               then a
               else snd old)
      $ zip (NE 0 [1 ..]) as

    base = mkMany (length actualData) $ _head newAs

fromFoc oldFl as = reconcile as (focusOrder oldFl) oldFl

fromVis oldFl as = reconcile as (visualOrder oldFl) oldFl
