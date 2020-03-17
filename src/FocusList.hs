{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE UndecidableInstances    #-}

module FocusList
  ( --FocusedList
  Direction(..)
  , Focus(..)
  , MultiList(..)
  , MultiListE(..)
  , FlNode(..)
  , focusOrder
  , visualOrder
  , multiPush
  , multiRemove
  , toNonEmptyML
  , endOf
  , modPointer
  -- , push
  -- , pop
  -- , flFilter
  -- , flLength
  -- , vOrder
  -- , fOrder
  -- , mapOne
  -- , focusElem
  -- , focusIndex
  -- , focusVIndex
  -- , findNeFocIndex
  -- , makeFL
  -- , focusDir
  -- , indexFL
  -- , fromFoc
  -- , fromVis
  )
where

import           Standard
-- import           Data.ChunkedZip
import           Text.Show.Deriving
import           Data.Eq.Deriving
import           Dhall (Interpret)
-- import           Data.Kind
import           Data.IntMap.Strict ((!), foldlWithKey, lookupMax)

-- I am super unattached to all of the code in this module.
-- If someone has a better way to represent this, I would gladly switch.

-- |Meant to represent the Head and Last on the list when sorted in focus order
data Focus = Focused | Unfocused
  deriving (Eq, Generic, Show)

-- |Meant to represent the Head and Last on the list when sorted in visual order
data Direction = Front | Back
  deriving (Show, Eq, Generic, Interpret)

-- |Things that are assumed about a Focused List but aren't proven: 
-- 1. The orders actually point to valid indices
-- 2. The lists are of the same size
-- 3. *Order doesn't contain duplicates
-- data FocusedList a = FL { visualOrder :: NonEmpty Int
--                         , focusOrder :: NonEmpty Int
--                         , actualData :: NonEmpty a
--                         }

-- data ElementInList a = EIL { o}
-- Begin deriving the laundry list of things we want to use
  -- deriving (Eq, Show, Functor, Generic, Foldable, Traversable)
-- instance MonoFoldable (FocusedList a)
-- deriveShow1 ''FocusedList
-- deriveEq1 ''FocusedList
-- type instance Element (FocusedList a) = a

-- instance Zip FocusedList where
--   zipWith f fl@FL { actualData = ad } FL { actualData = add } =
--     fl { actualData = zipWith f ad add }

-- -- Begin the actual code

-- -- | Pushes something to different ends
-- push :: Direction -> Focus -> a -> FocusedList a -> FocusedList a
-- push dir foc a FL { visualOrder = vo, focusOrder = fo, actualData = ad } = FL
--   { visualOrder = dirSide
--   , focusOrder  = focSide
--   , actualData  = append ad [a]
--   }
--  where
--   focSide = if foc == Focused then len +: fo else append fo [len]
--   dirSide = if dir == Front then len +: vo else append vo [len]
--   len     = length ad

-- -- | Given an ordering and direction to pop from, pops from the list
-- pop :: Either Direction Focus -> FocusedList a -> (a, Maybe (FocusedList a))
-- pop (Right isFocused) FL { visualOrder = vo, focusOrder = fo, actualData = ad }
--   = case isFocused of 
--       Focused -> popLogic head tail
--       Unfocused -> popLogic last init
--   where popLogic elemF otherF = 
--           (findNe (elemF fo) ad,) $ do
--             tailFo <- otherF fo
--             filteredVo <- remove (elemF fo) vo
--             filteredAd <- removeI (elemF fo) ad
--             return FL { visualOrder = filteredVo
--                       , focusOrder  = tailFo
--                       , actualData  = filteredAd
--                       }

-- pop (Left direction) FL { visualOrder = vo, focusOrder = fo, actualData = ad } 
--   = case direction of 
--       Front -> popLogic head tail
--       Back -> popLogic last init
--   where popLogic elemV otherV = 
--           (findNe (elemV vo) ad,) $ do
--             tailVo <- otherV vo
--             filteredFo<- remove (elemV vo) fo
--             filteredAd <- removeI (elemV vo) ad
--             return FL { visualOrder = tailVo
--                       , focusOrder  = filteredFo
--                       , actualData  = filteredAd
--                       }
  

-- -- |We just removed something from the list which did all sorts of bad things to order.
-- -- This function fixes the indices so we don't have holes.
-- reduce :: Int -> FocusedList a -> FocusedList a
-- reduce removed fl@FL {..} = fl { focusOrder  = newL focusOrder
--                                , visualOrder = newL visualOrder
--                                }
--   where newL = map (\i -> if i > removed then i - 1 else i)


-- -- | Modify one of the 4 ends
-- mapOne :: Either Direction Focus -> (a -> a) -> FocusedList a -> FocusedList a
-- mapOne orderAndEnd f fl@FL { focusOrder = fo, visualOrder = vo, actualData = ad } = fl
--   { actualData = case orderAndEnd of
--                    Left Front -> mapEnd $ head vo
--                    Left Back -> mapEnd $ last vo
--                    Right Focused -> mapEnd $ head fo
--                    Right Unfocused -> mapEnd $ last fo
--   }
--     where mapEnd tarfindNeI = map (\(i, a) -> if i == tarfindNeI then f a else a) $ zip [0 ..] ad

-- | Filter a focused list. Unfortunately, filter isn't a typeclass anywhere
-- flFilter :: (a -> Bool) -> FocusedList a -> Maybe (FocusedList a)
-- flFilter predicate FL { actualData = ad, visualOrder = vo, focusOrder = fo } =
--   fmap (\unwrapped -> foldl' (flip reduce) unwrapped gone) newFL
--   where
--     newFL = do
--       newAd <- filterNe predicate ad
--       newVo <- foldM removeFrom vo gone
--       newFo <- foldM removeFrom fo gone
--       return FL { actualData  = newAd
--                 , visualOrder = newVo
--                 , focusOrder  = newFo
--                 }
--     gone = foldl' (\acc (i, a) -> if predicate a then acc else i : acc) []
--       $ zip [0 ..] ad
--     removeFrom = flip remove

-- flLength :: FocusedList a -> Int
-- flLength FL {..} = length actualData

-- vOrder :: FocusedList a -> NonEmpty a
-- vOrder FL { visualOrder = vo, actualData = ad } =
--   map (flip findNe ad) vo

-- fOrder :: FocusedList a -> NonEmpty a
-- fOrder FL { focusOrder = fo, actualData = ad } =
--   map (flip findNe ad) fo

-- focusElem :: Eq a => a -> FocusedList a -> FocusedList a
-- focusElem a fl@FL { focusOrder = fo, actualData = ad } = fl { focusOrder }
--   where focusOrder = move 0 (findNeI a ad) fo

-- focusIndex :: Int -> FocusedList a -> FocusedList a
-- focusIndex i fl@FL { focusOrder = fo } = fl
--   { focusOrder = if length fo > i then newFo else fo
--   }
--   where newFo = move 0 i fo

-- focusVIndex :: Int -> FocusedList a -> FocusedList a
-- focusVIndex i fl@FL {visualOrder = vo } = focusIndex (findNe i vo) fl

-- findNeFocIndex :: FocusedList a -> Int
-- findNeFocIndex FL {..} = findNeI (head focusOrder) visualOrder

-- makeFL :: NonEmpty a -> Int -> FocusedList a
-- makeFL actualData focIndex = FL { visualOrder = vo
--                                 , focusOrder  = fo
--                                 , actualData
--                                 }
--  where
--   len = length actualData
--   vo  = [0.. len - 1]
--   fo  = move 0 focIndex vo

-- focusDir :: Direction -> FocusedList a -> FocusedList a
-- focusDir dir fl@FL { focusOrder = fo, visualOrder = vo } = fromMaybe fl $
--   case dir of
--     Front -> switchF Just tail
--     Back -> switchF tail Just
--   where switchF finding using = do
--           findList <- finding vo
--           usingList <- using vo
--           newLoc <- find ((== head fo) . fst) $ zip findList usingList
--           return $ focusIndex (snd newLoc) fl

-- indexFL :: Int -> FocusedList a -> a
-- indexFL i FL {..} = findNe i actualData

-- reconcile :: NonEmpty b -> NonEmpty Int -> FocusedList a -> FocusedList b
-- reconcile newAs order fl@FL{..} =
--     fl {actualData = foldl' updateAt base $ zip order newAs}
--  where updateAt as (i, a) = map (\old -> if fst old == i then a else snd old) $ zip (NE 0 [1..]) as
--        base = mkMany (length actualData) $ head newAs

-- fromFoc oldFl as = reconcile as (focusOrder oldFl) oldFl
-- fromVis oldFl as = reconcile as (visualOrder oldFl) oldFl

data FlNode a = MkNode { flNodeVisual :: (Maybe Int, Maybe Int)
                       , flNodeFocus :: (Maybe Int, Maybe Int)
                       , flNodeLocation :: Int
                       , flNodeElement :: a
                       }
  deriving (Functor, Foldable, Traversable, Eq, Show)
makeFieldLabels ''FlNode
deriveShow1 ''FlNode
deriveEq1 ''FlNode
type instance Element (FlNode a) = a


data MultiList a = MultiList {intMap :: IntMap (FlNode a), visEnds :: (Int, Int), focEnds :: (Int, Int)}
  deriving (Functor, Foldable, Traversable, Eq, Show)

makeFieldLabelsWith noPrefixFieldLabels ''MultiList
deriveShow1 ''MultiList
deriveEq1 ''MultiList
type instance Element (MultiList a) = a
instance MonoFoldable (MultiList a)
instance MonoPointed (MultiList a) where
  opoint a = MultiList (singletonMap 0 node) (0, 0) (0, 0)
    where node = MkNode (Nothing, Nothing) (Nothing, Nothing) 0 a

data MultiListE a = MLE (MultiList a) | EmptyML
  deriving (Functor)

instance Filterable (MultiListE) where
  catMaybes EmptyML = EmptyML
  catMaybes mle@(MLE ml) = fmap unJust $ foldlWithKey deleteIfNothing mle $ intMap ml
    where 
      deleteIfNothing (MLE newMl) i MkNode {flNodeElement = Nothing} =
        multiRemove (intMap newMl ! i) newMl
      deleteIfNothing newMlE _ _ = newMlE
      unJust (Just a) = a

toNonEmptyML :: MultiListE a -> Maybe (MultiList a)
toNonEmptyML EmptyML = Nothing
toNonEmptyML (MLE ml) = Just ml

endOf :: Either Direction Focus -> Lens' (MultiList a) (FlNode a)
endOf  endType =
  lensVL \f ml@(MultiList im (visStart, visEnd) (focStart, focEnd)) ->
    case endType of
      Left Front ->
        let firstThing = f $ im ! visStart
         in (\b -> ml {intMap = insertMap visStart b im}) <$> firstThing
      Left Back ->
        let firstThing = f $ im ! visEnd
         in (\b -> ml {intMap = insertMap visEnd b im}) <$> firstThing
      Right Focused ->
        let firstThing = f $ im ! focStart
         in (\b -> ml {intMap = insertMap focStart b im}) <$> firstThing
      Right Unfocused ->
        let firstThing = f $ im ! focEnd
         in (\b -> ml {intMap = insertMap focEnd b im}) <$> firstThing

modPointer :: forall a. Either Direction Focus -> (FlNode a -> Bool) -> MultiList a -> MultiList a
modPointer endType f ml =
  maybe ml setEnd newEnd
    where newEnd = find f $ intMap ml
          typeToMod = case endType of
                        Left _ -> VisualType
                        Right _ -> FocusType
          endToMod = case endType of
                Left Front -> (#visEnds % _1)
                Left Back -> (#visEnds % _2)
                Right Focused -> (#focEnds % _1)
                Right Unfocused -> (#focEnds % _2)
          pathToMod :: FlNode a -> (Maybe Int, Maybe Int)
          pathToMod node = case endType of
                        Left _ -> flNodeVisual node
                        Right _ -> flNodeFocus node
          setEnd :: FlNode a -> MultiList a
          setEnd node@MkNode {flNodeLocation = i} =
            over #intMap (rewirePath typeToMod $ pathToMod node) $
              set endToMod i ml

focusOrder :: forall a b. Traversal (MultiList a) (MultiList b) a b
focusOrder = traversalVL focusOrder'
  where focusOrder' :: forall f. Applicative f => (a -> f b) -> (MultiList a) -> f (MultiList b)
        focusOrder' f ml@(MultiList im _ (i, _)) =
          let fim = sequenceA $ extract $ ana @(Beam _) (applyToElem im f) (i, mempty)
           in fmap (\newIm -> ml {intMap = newIm}) fim
        applyToElem :: Applicative f => IntMap (FlNode a) -> (a -> f b) -> (Int, IntMap (f (FlNode b))) -> BeamF (IntMap (f (FlNode b))) (Int, IntMap (f (FlNode b)))
        applyToElem oldIm f (i, im) =
          let node = oldIm ! i
            in case snd $ flNodeVisual node of
                 Just newI -> ContinueF (newI, insertMap i (traverse f node) im)
                 Nothing -> EndF im
        
visualOrder :: forall a b. Traversal (MultiList a) (MultiList b) a b
visualOrder = traversalVL visualOrder'
  where visualOrder' :: forall f. Applicative f => (a -> f b) -> (MultiList a) -> f (MultiList b)
        visualOrder' f ml@(MultiList im (i, _) _) =
          let fim = sequenceA $ extract $ ana @(Beam _) (applyToElem im f) (i, mempty)
           in fmap (\newIm -> ml {intMap = newIm}) fim
        applyToElem :: Applicative f => IntMap (FlNode a) -> (a -> f b) -> (Int, IntMap (f (FlNode b))) -> BeamF (IntMap (f (FlNode b))) (Int, IntMap (f (FlNode b)))
        applyToElem oldIm f (i, im) =
          let node = oldIm ! i
            in case snd $ flNodeFocus node of
                 Just newI -> ContinueF (newI, insertMap i (traverse f node) im)
                 Nothing -> EndF $ insertMap i (traverse f node) im


multiPush :: Direction -> Focus -> a -> MultiList a -> MultiList a
multiPush d f a (MultiList im (visStart, visEnd) (focStart, focEnd)) =
  MultiList finalIm newVEnds newFEnds
  where
    finalIm = toVStart . toFStart . insertMap i node $ im
    newVEnds = if d == Front then (i, visEnd) else (visStart, i)
    newFEnds = if f == Focused then (i, focEnd) else (focStart, i)

    toVStart = uncurry (linkIm VisualType)
                        if d == Front 
                          then (Just i, Just visStart)
                          else (Just visEnd, Just i)
    toFStart = uncurry (linkIm FocusType) 
                        if f == Focused 
                          then (Just i, Just focStart)
                          else (Just focEnd, Just i)

    i = maybe 0 ((+1) . fst) $ lookupMax im
    node = MkNode (Nothing, Nothing) (Nothing, Nothing) i a



data LinkType = FocusType | VisualType

linkIm :: forall a. LinkType -> Maybe Int -> Maybe Int -> IntMap (FlNode a) -> IntMap (FlNode a)
linkIm linkType locA locB im =
  case linkType of
    FocusType -> doLink #focus
    VisualType -> doLink #visual
  where doLink lense = linkForward lense . linkBackward lense $ im
        linkForward lense newIm =
          maybe newIm (\i -> adjustMap (set (lense % _1) $ locB) i newIm) locA
        linkBackward lense newIm =
          maybe newIm (\i -> adjustMap (set (lense % _2) $ locA) i newIm) locB

rewire :: FlNode a -> IntMap (FlNode a) -> IntMap (FlNode a)
rewire MkNode{..} = rewirePath FocusType flNodeVisual . rewirePath VisualType flNodeFocus
rewirePath :: LinkType -> (Maybe Int, Maybe Int) -> IntMap (FlNode a) -> IntMap (FlNode a)
rewirePath lt path = linkIm lt (fst path) (snd path)

multiRemove :: FlNode a -> MultiList a -> MultiListE a
multiRemove n@MkNode{..} (MultiList im (visStart, visEnd) (focStart, focEnd)) =
  maybe EmptyML MLE $ do
    -- TODO I wonder if there is some way to make this code less redundant.
    vStartNew <- if visStart == flNodeLocation then snd flNodeVisual else Just visStart
    vEndNew <- if visEnd == flNodeLocation then fst flNodeVisual else Just visEnd
    fStartNew <- if focStart == flNodeLocation then snd flNodeFocus else Just focStart
    fEndNew <- if focEnd == flNodeLocation then fst flNodeFocus else Just focEnd

    Just $ MultiList (rewire n . deleteMap flNodeLocation $ im) (vStartNew, vEndNew) (fStartNew, fEndNew)
