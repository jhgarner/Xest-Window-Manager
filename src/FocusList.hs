{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module FocusList
  ( FocusedList (..),
    Direction (..),
    Focus (..),
    flMapMaybe,
    push,
    vOrder,
    vTraverse,
    fTraverse,
    focusElem,
    focusVIndex,
    visualFIndex,
    findNeFocIndex,
    makeFL,
    focusDir,
    ExtraInfo,
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

-- I'm currently rewriting the export list to rely more on lenses instead of
-- weirdly named functions.

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

-- Visual, Focus, Actual
data ExtraInfo a = EI Int Int a
  deriving (Eq, Show, Functor)

instance Comonad ExtraInfo where
  extract (EI _ _ a) = a
  duplicate ei@(EI v f _) = EI v f ei

-- This lens lets you modify the order/composition of FocusedList. Technically,
-- I think it follows the lens laws if you ignore the Int parameters in ExtraInfo.
-- fOrder could also exist but it isn't used so I don't need it.
vOrder :: forall a b. forall f. Functor f => (NonEmpty (ExtraInfo a) -> f (NonEmpty (ExtraInfo b))) -> FocusedList a -> f (FocusedList b)
vOrder f FL {visualOrder = vo, focusOrder = fo, actualData = ad}  = setter -- lens getter setter
  where inOrder =
          zipWith (\i a -> EI (fromJust $ elemIndexOf folded i vo) (fromJust $ elemIndexOf folded i fo) a) [0..] $ map (ad !!) vo
        setter = do
          f inOrder <&> \newOrder ->
            let actualData = map extract newOrder
                visualOrder = [0..length newOrder - 1]
                focusOrder = map fst $ fromList . sortOn (\(_, EI _ i' _) -> i') . toList $ mzip [0..] newOrder
             in FL {..}

vTraverse :: Traversal1 (FocusedList a) (FocusedList b) a b
vTraverse f fl = fromVis fl <$> traverse1 (f . (actualData fl !!)) (visualOrder fl)

fTraverse :: Traversal1 (FocusedList a) (FocusedList b) a b
fTraverse f fl = fromFoc fl <$> traverse1 (f . (actualData fl !!)) (focusOrder fl)

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
