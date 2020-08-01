{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}


{- |
    In here you'll find most of the interesting pure code for Xest. This code
    makes heavy use of recursions schemes (cata, ana, etc.) and lacks
    explicit recursion.
-}

module Tiler.Tiler
  ( module Tiler.Tiler
  , module Tiler.TilerTypes
  , module Tiler.ParentChild
  , module Tiler.TreeCombo
  , module Tiler.Sized
  , module Tiler.WithRect
  , module Tiler.ManyHelpers
  )
where

import           Standard
import           Graphics.X11.Types
import           Tiler.TilerTypes
import           Tiler.ParentChild
import           Tiler.TreeCombo
import           Tiler.Sized
import           Tiler.WithRect
import           Tiler.ManyHelpers
import           FocusList
import           Data.Functor.Foldable          ( embed )


-- | Add a new Tiler wherever it would make the most sense to the user. For
-- example, it's placed at the back for Horiz.
add :: SubTiler -> Tiler -> Tiler
add w (Many (Horiz fl) mods) =
  let ogSize = fromIntegral $ flLength fl
      newFl = push Back Focused (Sized (1 / ogSize) w) fl
      growPercent = 1 / foldl' (\acc s -> acc + getSize s) 0 newFl

      newestFl = map (\(Sized s a) -> Sized (s * growPercent) a) newFl

   in Many (Horiz newestFl) mods

add w (Many mh mods) = Many newMh mods
  where
    newMh =
      case mh of
        Horiz ne -> Horiz $ push Back Focused (Sized 0 w) ne
        Floating ne -> Floating $ push Back Focused (WithRect (Rect (-1) (-1) (-1) (-1)) w) ne
        TwoCols size ne -> TwoCols size $ push Back Focused (Identity w) ne
add w t = Many (Horiz (makeFL ((Sized 0.5 $ Fix t) :| [Sized 0.5 w]) 0)) NoMods


-- | Remove a Window if it exists in the tree.
ripOut :: Window -> Tiler -> Tiler
ripOut toDelete = project . fromMaybe (error "No root!") . cata isEqual
 where
  isEqual :: TilerF (Maybe SubTiler) -> Maybe SubTiler
  isEqual (Wrap (ParentChild parent window))
    | window == toDelete = Nothing
    | otherwise          = Just . embed . Wrap $ ParentChild parent window
  isEqual t = coerce $ reduce t



-- | Removes empty Tilers
reduce :: TilerF (Maybe SubTiler) -> Maybe Tiler
reduce (Many (Horiz fl) mods) = do
  newFl <- flMapMaybe sequence fl
  -- We need to multiply by a growth percentage to get the new sizes.
  let growPercent = 1 / foldl' (\acc s -> acc + getSize s) 0 fl
      newestFl = map (\(Sized s a) -> Sized (s * growPercent) a) newFl
  return $ Many (Horiz newestFl) mods
reduce (Many fl mods) = do
  newFl <- withFl fl (flMapMaybe sequence)
  return $ Many newFl mods
reduce (InputControllerOrMonitor c t) = Just . c $ join t
reduce (Wrap w) = Just $ Wrap w


-- | A combination of top and pop if you're coming from c++.
-- fst . popWindow is like top and snd . popWindow is like pop
popWindow :: Show a
          => Either Direction Focus -> TilerF a -> (a, Maybe (TilerF a))
popWindow howToPop (Many (Horiz fl) mods) = (newElem, newMany)
  where (Sized _ newElem, newFlM) = pop howToPop fl
        newMany = do
          newFl <- newFlM
          let growPercent = 1 / foldl' (\acc s -> acc + getSize s) 0 fl
          return $ Many (Horiz $ map (\(Sized s a) -> Sized (s * growPercent) a) newFl) mods
popWindow howToPop (Many mh mods) = (newElem, map (\newMh -> Many newMh mods) newMhM )
  where newElem = foldFl mh $ extract . fst . pop howToPop
        newMhM = withFl mh $ snd . pop howToPop

popWindow e t =
  error $ "Attempted to pop the unpopable" <> show e <> " " <> show t


-- |Get's the focused window. Throws an error if it can't. This little function
-- is the primary cause of runtime errors that aren't related to Xorg. Usually,
-- an unpopable error means you called getFocused when you shouldn't have.
getFocused :: Show a => TilerF a -> a
getFocused = fst . popWindow (Right Focused)



-- | Given something that wants to modify a Tiler,
-- apply it to the first Tiler after the inputController.
applyInput :: (Maybe SubTiler -> Maybe SubTiler) -> Tiler -> Tiler
applyInput f = toFType . cata \case
  InputController bords t -> InputController bords $ f t
  t                   -> coerce t

-- |Kind of like applyInput but can be used to get arbitrary info out of
-- focused element.
onInput :: (Maybe SubTiler -> a) -> Tiler -> a
onInput f root = f $ extract $ ana @(Beam _) findIC $ coerce root
 where
  findIC :: SubTiler -> BeamF (Maybe SubTiler) SubTiler
  findIC (InputController _ t) = EndF t
  findIC (Monitor _ t) = maybe (EndF Nothing) ContinueF t
  findIC t = ContinueF $ getFocused $ coerce t

-- | Kind of like applyInput but instead of searching for the InputController,
-- it just applies the function to whatever is focused by an individual Tiler.
modFocused :: (a -> a) -> TilerF a -> TilerF a
modFocused f (Many mh mods) = Many (withFl' mh $ mapOne (Right Focused) (map f)) mods
modFocused _ wp@(Wrap      _                 ) = wp
modFocused f t@( InputControllerOrMonitor _ _) = map f t


-- | Change the focus of a Tiler
focus :: SubTiler -> Tiler -> Tiler
focus newF (Many mh mods) = Many (withFl' mh $ focusElem ((== newF) . extract)) mods
focus _    t@(Wrap _) = t
focus _    t@(InputControllerOrMonitor _ _) = t

-- |Sometimes, we end up with a tree that's in an invalid state.
-- Usually, we can fix that by finding the closest parent between
-- two nodes and moving one of the nodes there. This function
-- implements that logic.
--
-- In addition to moving the node, this function also ensures that
-- the movable node is always a parent of the unmovable node.
moveToClosestParent
  :: (TilerF (Maybe SubTiler) -> Bool)
  -> (TilerF (Maybe SubTiler) -> Maybe (Reparenter, Unparented))
  -> Tiler
  -> (Maybe Tiler, TreeCombo)
moveToClosestParent predicateUnmove predicateMove = coerce
  . cata (coerce . moveToClosestParent')
 where
  moveToClosestParent'
    :: TilerF (Maybe SubTiler, TreeCombo) -> (Maybe Tiler, TreeCombo)
  moveToClosestParent' t
    | predicateUnmove $ map fst t =
      -- We are looking at the unmovable part
      case asum $ map (getMovable . snd) t of
        -- We haven't seen the movable part, so just set the Unmovable flag and be done
        Nothing -> (withNewFocus t, Unmovable)
        -- We already saw the movable part so add that in as this thing's parents
        Just (reparentFunction, _) ->
          (Just $ reparentFunction $ coerce $ withNewFocus t, Both)

    | otherwise =
      case predicateMove $ map fst t of
        -- Whatever we found, it's neither of the parts. It might be the parent though.
        Nothing -> case hasBothIndividually t of
          -- We found the parent! Let's make it the parent.
          Just (reparentFunction, _) ->
            (Just $ reparentFunction $ coerce $ withNewFocus t, Both)
          -- This node is uneventful. Let's just make sure its focus is correct
          Nothing -> (withNewFocus t, foldMap snd t)
        -- We found the movable part!
        Just functions@(_, unparented) ->
          if any (isUnmovable . snd) t
             -- If it already contained the unmovable part, we're done!
             then (withNewFocus t, Both)
             -- Otherwise, let's remove it and set the right flag
             else (unparented, Movable functions)

  reduced :: TilerF (Maybe SubTiler, a) -> Maybe Tiler
  reduced t = reduce $ map fst t

  withNewFocus :: TilerF (Maybe SubTiler, TreeCombo) -> Maybe Tiler
  withNewFocus t = fromMaybe (reduced t) $ do
    elementToFoc <- find (\(_, tc) -> isUnmovable tc || isBoth tc) t >>= fst
    Just $ focus elementToFoc <$> reduced t
  hasBothIndividually tiler = if any (isUnmovable . snd) tiler
    then asum $ map (getMovable . snd) tiler
    else Nothing

-- |Specialization of the above for moving an InputController towards a window
moveToWindow :: Window -> Tiler -> Maybe Tiler
moveToWindow window root =
  let (newRoot, b) = second isBoth $ moveToClosestParent isWindow isInputController root
   in if b then newRoot else Nothing
 where
  isInputController :: TilerF (Maybe SubTiler) -> Maybe (Reparenter, Unparented)
  isInputController (InputController  bords t) =
    Just (InputController bords, coerce $ join t)
  isInputController _ = Nothing
  isWindow (Wrap parentChild) = inParentChild window parentChild
  isWindow _                  = False

-- |Specialization of the above for moving a moniter towards the inputController
moveToIC :: Tiler -> Maybe Tiler
moveToIC root =
  let (newRoot, b) = moveToClosestParent isInputController isMonitor root
   in if isBoth b then newRoot else Nothing
 where
  isMonitor (Monitor loc t) = Just (Monitor loc, coerce $ join t)
  isMonitor _ = Nothing
  isInputController (InputController _ _) = True
  isInputController _ = False

-- |Do both of the above in sequence. This is the function that's actually used elsewhere
focusWindow :: Window -> Tiler -> Maybe Tiler
focusWindow window =
  moveToIC <=< moveToWindow window

-- |Specialization of the above for moving a moniter towards the inputController
moveToMon :: Tiler -> Maybe Tiler
moveToMon root =
  let (newRoot, b) = second isBoth $ moveToClosestParent isMonitor isInput root
   in if b then newRoot else Nothing
 where
  isInput (InputController bords t) = Just (InputController bords, coerce $ join t)
  isInput _ = Nothing
  isMonitor (Monitor _ _) = True
  isMonitor _ = False

-- |The EWMH says window managers can list the number of virtual desktops and
-- their names. This function gets that info, although we use a liberal
-- definition of virtual desktop.
getDesktopState :: Tiler -> ([Text], Int)
getDesktopState (Many mh _) = (show @Int <$> [1 .. (foldFl mh flLength)], i)
  where i = foldFl mh findNeFocIndex
getDesktopState _ = (["None"], 0)

-- |Renders the tree to a string which can be displayed on the top border of
-- Xest.
getFocusList :: TilerF Text -> Text
getFocusList (InputController _ s) = "*" <> fromMaybe "" s
getFocusList (Monitor _ s) = "@" <> fromMaybe "" s
getFocusList (Many mh mods) =
  "|" <> modType <> manyType <> "-" <> size <> "-" <> i <> "|" <> child
  where manyType = case mh of
                     Horiz _ -> "H"
                     Floating _ -> "F"
                     TwoCols _ _ -> "T"
        modType = case mods of
                    Rotate -> "r-"
                    Full -> "f-"
                    NoMods -> ""
        child = foldFl mh $ extract . fst . pop (Right Focused)
        size = foldFl mh $ show . flLength
        i = foldFl mh $ show . succ . findNeFocIndex
getFocusList (Wrap            _       ) = "Window"

-- |Given a child, can we find the parent in our tree?
findParent :: Window -> Tiler -> Maybe Window
findParent w = cata step
 where
  step (Wrap (ParentChild ww ww')) | ww' == w  = Just ww
                                   | otherwise = Nothing
  step t = foldl' (<|>) Nothing t

-- |Get all parents from the tree
getAllParents :: Tiler -> [Window]
getAllParents = cata \case
  Wrap (ParentChild p _) -> pure p
  t -> fold t

-- |Do some geometry to figure out which screen we're on. What's up with the
-- Functor f getting tossed around? Well sometimes we want the actual screen
-- that's focused and other times we want just the index. This function can do
-- both of those. If you're looking for just the element, f ~ Identity but if
-- you're looking for the index, f ~ (Int,).
whichScreen
  :: (Eq (f Bool), Functor f) => (Int32, Int32) -> [f XRect] -> Maybe (f XRect)
whichScreen (mx, my) = map getFirst . foldMap' findOverlap
 where
  findOverlap wrapped =
    if (wrapped $> True) == map isInside wrapped then Just $ First wrapped else Nothing
  isInside Rect {..} =
    mx >= x && my >= y && mx < x + fromIntegral w && my < y + fromIntegral h

-- |The monitor must always be in the focus path.
-- If it's already there, this function does nothing.
fixMonitor :: Tiler -> Tiler
fixMonitor = fromMaybe (error "Can't be empty") . moveToIC

findWindow :: Window -> Tiler -> Bool
findWindow w = cata $ \case
      (Wrap w') -> inParentChild w w'
      t -> or t

getBorders :: Tiler -> Borders
getBorders = fromMaybe (error "No IC here!") . cata \case
  InputController b _ -> Just b
  t -> asum t

getScreens :: Tiler -> XRect
getScreens = fromMaybe (error "No IC here!") . cata \case
  Monitor r _ -> Just r
  t -> asum t

putScreens :: XRect -> Tiler -> Tiler
putScreens xRect = unfix . cata \case
  Monitor _ t -> Monitor xRect t
  t -> Fix t

getTilerFromScreen :: (Tiler -> Bool) -> Screens -> Maybe Tiler
getTilerFromScreen p screens =
  find (para (\t -> any snd t || p (map (Fix . fst) t))) $ map snd $ itoList screens

getTilerWithWindow :: Window -> Screens -> Maybe Tiler
getTilerWithWindow w = getTilerFromScreen \case
  Wrap (ParentChild _ c) -> c == w
  _ -> False

screensToTilers :: Screens -> [Tiler]
screensToTilers = map snd . itoList

fixFloating :: Tiler -> Tiler
fixFloating root = unfix $ flip cata root \case
  Many (Floating fl) mods ->
    Many (Floating (map fixRect fl)) mods
  t -> Fix t
  where Just (Rect{..}) = flip cata root \case
          Monitor r _ -> Just $ bimap fromIntegral fromIntegral r
          t -> asum t
        fixRect (WithRect (Rect _ _ (-1) (-1)) t) =
          WithRect (Rect (x + w / 4) (y + h / 4) (w / 2) (h / 2)) t
        fixRect wr = wr

removeDangerous :: Tiler -> Maybe SubTiler
removeDangerous = cata \case
  Monitor _ t -> join t
  InputController _ t -> join t
  t -> coerce reduce t