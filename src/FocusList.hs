{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NamedFieldPuns    #-}

module FocusList
  ( FocusedList
  , Direction(..)
  , Focus(..)
  , push
  , pop
  , isNull
  , flFilter
  , flLength
  , vOrder
  , fOrder
  , mapOne
  , focusElem
  , focusIndex
  , getFocIndex
  , makeFL
  , emptyFL
  , focusDir
  , indexFL
  , fromFoc
  , fromVis
  )
where

import           Standard
import           Data.ChunkedZip
import           Text.Show.Deriving
import           Data.Eq.Deriving

data Focus = Focused | Unfocused
  deriving (Eq, Generic, Show)

data Direction = Front | Back
  deriving (Show, Eq, Generic)

-- TODO Currently I trade memory usage for develpment time by just marking stuff as free
-- but would it be better to actually map over the orders and condense the vectors?
data FocusedList a = FL { visualOrder :: [Int]
                        , focusOrder :: [Int]
                        , actualData :: [a]
                        }
  deriving (Eq, Show, Functor, Generic, Foldable, Traversable)
instance MonoFoldable (FocusedList a)
deriveShow1 ''FocusedList
deriveEq1 ''FocusedList
type instance Element (FocusedList a) = a

instance Zip FocusedList where
  zipWith f fl@FL { actualData = ad } FL { actualData = add } =
    fl { actualData = zipWith f ad add }

push :: Direction -> Focus -> a -> FocusedList a -> FocusedList a
push dir foc a FL { visualOrder = vo, focusOrder = fo, actualData = ad } = FL
  { visualOrder = dirSide
  , focusOrder  = focSide
  , actualData  = ad ++ [a]
  }
 where
  focSide = if foc == Focused then len : fo else fo ++ [len]
  dirSide = if dir == Front then len : vo else vo ++ [len]
  len     = length ad

pop :: Either Direction Focus -> FocusedList a -> (Maybe a, FocusedList a)
pop (Right Focused) fl@FL { visualOrder = vo, focusOrder = fo, actualData = ad }
  = fromMaybe (Nothing, fl) $ do
    loc    <- headMay fo
    top    <- index ad loc
    tailFo <- tailMay fo
    let popped = FL { visualOrder = remove loc vo
                    , focusOrder  = tailFo
                    , actualData  = removeI loc ad
                    }
    return (Just top, reduce loc popped)

pop (Right Unfocused) fl@FL { visualOrder = vo, focusOrder = fo, actualData = ad }
  = fromMaybe (Nothing, fl) $ do
    loc    <- lastMay fo
    top    <- index ad loc
    initFo <- initMay fo
    let popped = FL { visualOrder = remove loc vo
                    , focusOrder  = initFo
                    , actualData  = removeI loc ad
                    }
    return (Just top, reduce loc popped)

pop (Left Front) fl@FL { visualOrder = vo, focusOrder = fo, actualData = ad } =
  fromMaybe (Nothing, fl) $ do
    loc    <- headMay vo
    top    <- index ad loc
    tailVo <- tailMay vo
    let popped = FL { visualOrder = tailVo
                    , focusOrder  = remove loc fo
                    , actualData  = removeI loc ad
                    }
    return (Just top, reduce loc popped)

pop (Left Back) fl@FL { visualOrder = vo, focusOrder = fo, actualData = ad } =
  fromMaybe (Nothing, fl) $ do
    loc    <- lastMay vo
    top    <- index ad loc
    initVo <- initMay vo
    let popped = FL { visualOrder = initVo
                    , focusOrder  = remove loc fo
                    , actualData  = removeI loc ad
                    }
    return (Just top, reduce loc popped)

reduce :: Int -> FocusedList a -> FocusedList a
reduce removed fl@FL {..} = fl { focusOrder  = newL focusOrder
                               , visualOrder = newL visualOrder
                               }
  where newL = map (\i -> if i > removed then i - 1 else i)

remove :: Eq a => a -> [a] -> [a]
remove a = filter (/= a)

removeI :: Int -> [a] -> [a]
removeI i = map snd . filter ((/= i) . fst) . zip [0 ..]

mapOne :: Either Direction Focus -> (a -> a) -> FocusedList a -> FocusedList a
mapOne (Right Focused) f fl@FL { focusOrder = mfo, actualData = ad } = fl
  { actualData =
    maybe []
          (\fo -> map (\(i, a) -> if i == fo then f a else a) $ zip [0 ..] ad)
      $ headMay mfo
  }

mapOne (Right Unfocused) f fl@FL { focusOrder = mfo, actualData = ad } = fl
  { actualData =
    maybe []
          (\fo -> map (\(i, a) -> if i == fo then f a else a) $ zip [0 ..] ad)
      $ lastMay mfo
  }

mapOne (Left Front) f fl@FL { visualOrder = mvo, actualData = ad } = fl
  { actualData =
    maybe []
          (\vo -> map (\(i, a) -> if i == vo then f a else a) $ zip [0 ..] ad)
      $ headMay mvo
  }

mapOne (Left Back) f fl@FL { focusOrder = mvo, actualData = ad } = fl
  { actualData =
    maybe []
          (\vo -> map (\(i, a) -> if i == vo then f a else a) $ zip [0 ..] ad)
      $ lastMay mvo
  }

isNull :: FocusedList a -> Bool
isNull FL {..} = null actualData

flFilter :: (a -> Bool) -> FocusedList a -> FocusedList a
flFilter predicate FL { actualData = ad, visualOrder = vo, focusOrder = fo } =
  foldl' (flip reduce) newFL gone
  where
    newFL = FL { actualData  = filter predicate ad
      , visualOrder = foldl' removeFrom vo gone
      , focusOrder  = foldl' removeFrom fo gone
      }
    gone = foldl' (\acc (i, a) -> if predicate a then acc else i : acc) []
      $ zip [0 ..] ad
    removeFrom = flip remove

flLength :: FocusedList a -> Int
flLength FL {..} = length actualData

vOrder :: FocusedList a -> [a]
vOrder FL { visualOrder = vo, actualData = ad } =
  map (fromMaybe (error "focList broken") . index ad) vo

fOrder :: FocusedList a -> [a]
fOrder FL { focusOrder = fo, actualData = ad } =
  map (fromMaybe (error "focList broken") . index ad) fo

focusElem :: Eq a => a -> FocusedList a -> FocusedList a
focusElem a fl@FL { focusOrder = fo, actualData = ad } = fl { focusOrder }
  where focusOrder = maybe fo (\loc -> loc : remove loc fo) $ elemIndex a ad

focusIndex :: Int -> FocusedList a -> FocusedList a
focusIndex i fl@FL { focusOrder = fo } = fl
  { focusOrder = if length fo > i then newFo else fo
  }
  where newFo = i : remove i fo

getFocIndex :: FocusedList a -> Maybe Int
getFocIndex FL {..} = headMay focusOrder

makeFL :: [a] -> Int -> FocusedList a
makeFL actualData focIndex = FL { visualOrder = vo
                                , focusOrder  = fo
                                , actualData
                                }
 where
  len = length actualData
  vo  = [0 .. len - 1]
  fo  = focIndex : filter (/= focIndex) vo

emptyFL :: FocusedList a
emptyFL = FL { visualOrder = [], focusOrder = [], actualData = [] }

focusDir :: Direction -> FocusedList a -> FocusedList a
focusDir Front fl@FL { focusOrder = fo, visualOrder = vo } = fromMaybe fl $ do
  currentFoc <- headMay fo
  fronts     <- zip vo <$> tailMay vo
  newFoc     <- fst <$> find ((== currentFoc) . snd) fronts
  return $ focusIndex newFoc fl

focusDir Back fl@FL { focusOrder = fo, visualOrder = vo } = fromMaybe fl $ do
  currentFoc <- headMay fo
  backs      <- zip vo <$> tailMay vo
  newFoc     <- snd <$> find ((== currentFoc) . fst) backs
  return $ focusIndex newFoc fl

indexFL :: Int -> FocusedList a -> Maybe a
indexFL i FL {..} = index actualData i

reconcile :: [b] -> [Int] -> FocusedList a -> FocusedList b
reconcile newAs order fl@FL{..} =
    fl {actualData = foldl' updateAt base $ zip order newAs}
 where updateAt as (i, a) = map (\old -> if fst old == i then a else snd old) $ zip [0..] as
       base = maybe [] (replicate $ length actualData) $ headMay newAs

fromFoc oldFl as = reconcile as (focusOrder oldFl) oldFl
fromVis oldFl as = reconcile as (visualOrder oldFl) oldFl
