{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module FocusList
  ( FocusedList (..),
    Direction (..),
    Focus (..),
    push,
    flMapMaybe,
    flLength,
    vOrder,
    fOrder,
    focusElem,
    focusVIndex,
    visualFIndex,
    findNeFocIndex,
    makeFL,
    focusDir,
  )
where

import Data.ChunkedZip
import Data.Eq.Deriving
import Data.List.NonEmpty (fromList, take, drop)
import Dhall (Interpret)
import Standard hiding (zip, zipWith, take, drop)
import Text.Show.Deriving

-- I am super unattached to all of the code in this module.
-- If someone has a better way to represent this, I would gladly switch.
-- This module could use some more documentation and better names, but I think
-- time would be better spent trying to rearchitect the module using lenses or something.

-- | Meant to represent the Head and Last on the list when sorted in focus order
data Focus = Focused | Unfocused
  deriving stock (Eq, Generic, Show)

-- | Meant to represent the Head and Last on the list when sorted in visual order
data Direction = Front | Back
  deriving (Show, Eq, Generic, Interpret)

-- | Things that are assumed about a Focused List but aren't proven:
--  1. The orders actually point to valid indices
--  2. The lists are of the same size
--  3. *Order doesn't contain duplicates
data FocusedList a = FL
  { visualOrder :: NonEmpty Int,
    focusOrder :: NonEmpty Int,
    actualData :: NonEmpty a
  }
  -- Begin deriving the laundry list of things we want to use
  deriving (Eq, Show, Functor, Generic, Foldable, Traversable)

deriveShow1 ''FocusedList
deriveEq1 ''FocusedList

instance Zip FocusedList where
  zipWith f fl@FL {actualData = ad} FL {actualData = add} =
    fl {actualData = zipWith f ad add}

-- Begin the actual code

-- | Pushes something to different ends
push :: Direction -> Focus -> a -> FocusedList a -> FocusedList a
push dir foc a FL {visualOrder = vo, focusOrder = fo, actualData = ad} =
  FL
    { visualOrder = dirSide,
      focusOrder = focSide,
      actualData = ad <> [a]
    }
  where
    focSide = if foc == Focused then len <| fo else fo <> [len]
    dirSide = if dir == Front then len <| vo else vo <> [len]
    len = length ad

-- | We just removed something from the list which did all sorts of bad things to order.
--  This function fixes the indices so we don't have holes.
reduce :: Int -> FocusedList a -> FocusedList a
reduce removed fl@FL {..} =
  fl
    { focusOrder = newL focusOrder,
      visualOrder = newL visualOrder
    }
  where
    newL = map (\i -> if i > removed then i - 1 else i)

-- | Filter a focused list. Unfortunately, filter isn't a typeclass anywhere
-- TODO This looks suspiciously like traverse...
flMapMaybe :: (a -> Maybe b) -> FocusedList a -> Maybe (FocusedList b)
flMapMaybe predicate FL {actualData = ad, visualOrder = vo, focusOrder = fo} =
  map (\unwrapped -> foldl' (flip reduce) unwrapped $ sortOn Down gone) newFL
  where
    newFL = do
      newAd <- nonEmpty $ mapMaybe predicate $ toList ad
      newVo <- foldM removeFrom vo gone
      newFo <- foldM removeFrom fo gone
      return
        FL
          { actualData = newAd,
            visualOrder = newVo,
            focusOrder = newFo
          }
    gone =
      foldl' (\acc (i, a) -> if isJust $ predicate a then acc else i : acc) [] $
        zip [0 ..] ad
    removeFrom = flip remove

flLength :: FocusedList a -> Int
flLength FL {..} = length actualData

-- This is an unlawful lens :(
-- Fun fact: the lens being unlawful led to a bug that was hard to find by
-- analyzing the symptoms. I should really put more time into making sure my
-- instances are lawful...
-- Although I'm not really sure how to make this one lawful. I guess I need to
-- zip up the values with position data and prevent the user from modifying
-- the positional data except by reordering/dropping elements?
-- Actually, that might make pushing a viable lens too...
vOrder :: Lens (FocusedList a) (FocusedList b) (NonEmpty a) (NonEmpty b)
vOrder = lens getter fromVis
  where getter FL {visualOrder = vo, actualData = ad} = map (ad !!) vo

-- This is an unlawful lens :(
fOrder :: Lens (FocusedList a) (FocusedList b) (NonEmpty a) (NonEmpty b)
fOrder = lens getter fromFoc
  where getter FL {focusOrder = fo, actualData = ad} = map (ad !!) fo

focusElem :: (a -> Bool) -> FocusedList a -> FocusedList a
focusElem p fl@FL {actualData = ad} = focusIndex loc fl
  where
    loc = fst $ fromJust $ find (p . snd) $ mzip [0 ..] ad

focusIndex :: Int -> FocusedList a -> FocusedList a
focusIndex i fl@FL {focusOrder = fo} =
  fl
    { focusOrder = if length fo > i then focusNE i fo else fo
    }

focusNE :: Int -> NonEmpty Int -> NonEmpty Int
focusNE i = maybe (pure i) (i <|) . remove i

prependNE :: [a] -> NonEmpty a -> NonEmpty a
prependNE [] ne = ne
prependNE (a:as) ne = (a :| as) <> ne

moveToNE :: Int -> Int -> NonEmpty Int -> NonEmpty Int
moveToNE elem to = maybe (pure elem) (\ne -> prependNE (take to ne) (elem :| drop to ne)) . remove elem

focusVIndex :: Int -> FocusedList a -> FocusedList a
focusVIndex i fl@FL {visualOrder = vo} = focusIndex (vo !! i) fl

visualFIndex :: Int -> Int -> FocusedList a -> FocusedList a
visualFIndex i to fl@FL {focusOrder = fo, visualOrder = vo} =
  fl {visualOrder = moveToNE (fo !! i) to vo}


-- |I really want to get rid of this function...
findNeFocIndex :: FocusedList a -> Int
findNeFocIndex FL {..} = fromJust $ elemIndex (head focusOrder) $ toList visualOrder

makeFL :: NonEmpty a -> Int -> FocusedList a
makeFL actualData focIndex =
  FL
    { visualOrder = vo,
      focusOrder = fo,
      actualData
    }
  where
    len = length actualData
    vo = [0 .. len - 1]
    fo = maybe (pure focIndex) (focIndex <|) $ removeAt focIndex vo

focusDir :: Direction -> FocusedList a -> FocusedList a
focusDir dir fl@FL {focusOrder = fo, visualOrder = vo} = fromMaybe fl $
  case dir of
    Back -> switchF Just (nonEmpty . tail)
    Front -> switchF (nonEmpty . tail) Just
  where
    switchF finding using = do
      findList <- finding vo
      usingList <- using vo
      newLoc <- find ((== head fo) . fst) $ zip findList usingList
      return $ focusIndex (snd newLoc) fl

-- TODO doesn't really handle change of shape
reconcile :: NonEmpty b -> NonEmpty Int -> FocusedList a -> FocusedList b
reconcile newAs order fl@FL {..} =
  fl {actualData = foldl' updateAt base $ zip order newAs}
  where
    updateAt as (i, a) = set (ix i) a as
    base = fromList $ replicate (length actualData) undefined

fromFoc :: FocusedList a -> NonEmpty b -> FocusedList b
fromFoc oldFl as = reconcile as (focusOrder oldFl) oldFl

fromVis :: FocusedList a -> NonEmpty b -> FocusedList b
fromVis oldFl as = reconcile as (visualOrder oldFl) oldFl
