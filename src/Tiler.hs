{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Tiler where

import           Standard
import           Graphics.X11.Types
import           Types
import           FocusList

-- Tiler Handling Code --


-- | Add a new Tiler at the given location. Is the opposite of popping.
add :: Direction -> Focus -> a -> Tiler a -> Tiler a
add dir foc w (Horiz fl) = Horiz $ push dir foc (Sized 0 w) fl
add _ foc w (Floating ls) = Floating $ if foc == Focused then Top (RRect 0 0 0.2 0.2, w) +: ls else append ls [Top (RRect 0 0 0.2 0.2, w)]
add _ _ _ _ = error "Attempted to add to something that isn't addable"


-- | Remove a Tiler if it exists
ripOut :: Window -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
ripOut toDelete = maybe (error "No root!") unfix 
  . cata isEqual . Fix
  where isEqual :: Tiler (Maybe (Fix Tiler)) -> Maybe (Fix Tiler)
        isEqual (Wrap (ChildParent parent window))
          | window == toDelete = Nothing
          | otherwise = Just . Fix . Wrap $ ChildParent parent window
        isEqual t = fmap Fix . reduce $ t



-- | Removes empty Tilers
reduce :: Tiler (Maybe (Fix Tiler)) -> Maybe (Tiler (Fix Tiler))
-- Yo dog, I head you like fmaps
reduce (Horiz fl) = fmap Horiz $ fmap (fmap $ fromMaybe (error "Impossible")) <$> flFilter (not . null . getItem) fl
reduce (Floating ls) = newTiler
  where newTiler = Floating . fmap (fmap $ fromMaybe (error "Impossible")) <$> newFront
        newFront = filterNe (not . null . getEither) ls
reduce (InputControllerOrMonitor c t) = Just . c $ join t
reduce (Reflect t) = fmap Reflect t
reduce (FocusFull t) = fmap FocusFull t
reduce (Wrap w) = Just $ Wrap w


-- | A combination of top and pop if you're coming from c++.
-- fst . popWindow is like top and snd . popWindow is like pop
popWindow
  :: Show a => Either Direction Focus
  -> Tiler a
  -> (a, Maybe (Tiler a))

popWindow howToPop (Horiz fl) =
  getItem *** fmap Horiz $ pop howToPop fl

popWindow howToPop (Floating ls) = second (fmap Floating) $ case howToPop of
  Right Unfocused -> (getEither $ last ls, init ls)
  _ -> (getEither $ head ls, tail ls)

popWindow _ (Reflect t) = (t, Nothing)
popWindow _ (FocusFull t) = (t, Nothing)

popWindow e t = error $ "Attempted to pop the unpopable" ++ show e ++ " " ++ show t


getFocused :: Show a => Tiler a -> a
getFocused = fst . popWindow (Right Focused)



-- | Given something that wants to modify a Tiler,
-- apply it to the first Tiler after the inputController
-- Often used with cata to create a new root Tiler
applyInput
  :: (Maybe (Tiler (Fix Tiler)) -> Maybe (Tiler (Fix Tiler))) -> Tiler (Fix Tiler) -> Fix Tiler
applyInput f (InputController t) =
  Fix . InputController . fmap Fix . f $ fmap unfix t
applyInput _ t                         = Fix t

onInput :: (Maybe (Fix Tiler) -> a) -> Fix Tiler -> a
onInput f root = f $ extract $ ana @(Beam _) findIC  (Just root)
 where
   findIC (Just (Fix (InputController t))) = EndF t
   findIC (Just (Fix (Monitor t))) = ContinueF t
   findIC (Just t) = ContinueF . Just . getFocused . unfix $ t
   findIC _ = EndF Nothing

-- | Modify the focused Tiler in another Tiler based on a function
modFocused :: (a -> a) -> Tiler a -> Tiler a
modFocused f (Horiz fl) = Horiz $ mapOne (Right Focused) (fmap f) fl
modFocused f (Floating (NE a as)) = Floating $ NE (fmap f a) as
modFocused f (Reflect t) = Reflect $ f t
modFocused f (FocusFull t) = FocusFull $ f t
modFocused _ wp@(Wrap _) = wp
modFocused f t@(InputControllerOrMonitor _ _) = fmap f t


-- | Change the focus of a Tiler
focus :: Eq a => a -> Tiler a -> Tiler a
focus newF (  Horiz fl ) = Horiz $ focusElem (Sized 0 newF) fl
focus newF (  Floating ls ) = Floating $ moveF 0 ((== newF) . getEither) ls
focus _    t@(Wrap _) = t
focus _    t@(Reflect _) = t
focus _    t@(FocusFull _) = t
focus _    t@(InputController _) = t
focus _    t@(Monitor _) = t

-- |Sometimes, we end up with a tree that's in an invalid state.
-- Usually, we can fix that by finding the closest parent between
-- two nodes and moving one of the nodes there. This function
-- implements that logic.
--
-- In addition to moving the node, this function also ensures that
-- the movable node is always a parent of the unmovable node.
moveToClosestParent
  :: (Tiler (Maybe (Fix Tiler)) -> Bool) -- |Function used to find the unmovable part
  -> (Tiler (Maybe (Fix Tiler)) -> Maybe (Reparenter, Unparented)) -- |Function used to find the movable part.
  -> Tiler (Maybe (Fix Tiler), TreeCombo)
  -> (Maybe (Fix Tiler), TreeCombo)
moveToClosestParent predicateUnmove predicateMove t 
  | predicateUnmove $ fmap fst t =
    -- We are looking at the unmovable part
    case asum $ fmap (getMovable . snd) t of
      -- We haven't seen the movable part, so just set the Unmovable flag and be done
      Nothing -> (withNewFocus, Unmovable)
      -- We already saw the movable part so add that in as this thing's parents
      Just (reparentFunction, _) -> (reparentFunction $ fmap unfix withNewFocus, Both)
  | otherwise =
    case predicateMove $ fmap fst t of
      -- Whatever we found, it's neither of the parts. It might be the parent though.
      Nothing ->
        case hasBothIndividually t of
          -- We found the parent! Let's make it the parent.
          Just (reparentFunction, _) -> (reparentFunction $ fmap unfix withNewFocus, Both)
          -- This node is uneventful. Let's just make sure its focus is correct
          Nothing -> (withNewFocus, foldMap snd t)
      -- We found the movable part!
      Just functions@(_, unparented) ->
        if any (isUnmovable . snd) t
           -- If it already contained the unmovable part, we're done!
           then (withNewFocus, Both)
           -- Otherwise, let's remove it and set the right flag
           else (unparented, Movable functions)
  where reduceAndFixed = fmap Fix . reduce $ fmap fst t
        withNewFocus = fromMaybe reduceAndFixed $ do
          elementToFoc :: Fix Tiler <- find (\(_, tc) -> isUnmovable tc || isBoth tc) t >>= fst
          reduced <- unfix <$> reduceAndFixed
          Just $ Just $ Fix $ focus elementToFoc reduced 
        hasBothIndividually tiler = 
          if any (isUnmovable . snd) tiler
             then asum $ fmap (getMovable . snd) tiler
             else Nothing

-- |Specialization of the above for moving an InputController towards a window
moveToWindow
  :: Window
  -> Fix Tiler
  -> (Maybe (Fix Tiler), Bool)
moveToWindow window =
  second isBoth . cata (moveToClosestParent isWindow isInputController)
  where isInputController (InputController t) =
          Just (Just . Fix . InputController . fmap Fix, join t)
        isInputController _ = Nothing
        isWindow (Wrap childParent) = inChildParent window childParent
        isWindow _ = False

-- |Specialization of the above for moving a moniter towards the inputController
moveToIC
  :: Fix Tiler
  -> (Maybe (Fix Tiler), Bool)
moveToIC =
  second isBoth . cata (moveToClosestParent isInputController isMonitor)
  where isMonitor (Monitor t) =
          Just (Just . Fix . Monitor . fmap Fix, join t)
        isMonitor _ = Nothing
        isInputController (InputController _) = True
        isInputController _ = False

-- |Do both of the above in sequence. This is the function that's actually used elsewhere
focusWindow
  :: Window
  -> Fix Tiler
  -> Maybe (Fix Tiler)
focusWindow window root =
  let (newRoot, b1) = moveToWindow window root
      (newestRoot, b2) = maybe (Nothing, False) moveToIC newRoot
   in if b1 && b2 then newestRoot else Nothing

getDesktopState :: Tiler (Fix Tiler) -> ([Text], Int)
getDesktopState (Horiz fl) =
  (pack . show <$> [1 .. flLength fl], i)
  where i = findNeFocIndex fl
getDesktopState _ = (["None"], 0)

getFocusList :: Tiler String -> String
getFocusList (InputController s) = "*" ++ fromMaybe "" s
getFocusList (Monitor s) = "@" ++ fromMaybe "" s
getFocusList (Horiz fl) = "Horiz|" ++ getItem (fst (pop (Right Focused) fl))
getFocusList (Floating (NE t _)) = "Floating|" ++ extract t
getFocusList (Reflect t) = "Rotate|" ++ t
getFocusList (FocusFull t) = "Full|" ++ t
getFocusList (Wrap _) = "Window"

findParent :: Window -> Fix Tiler -> Maybe Window
findParent w = cata step
  where step (Wrap (ChildParent ww ww')) 
          | ww' == w = Just ww
          | otherwise = Nothing
        step t = foldl' (<|>) Nothing t

whichScreen :: (Eq (f Bool), Functor f)
            => (Int32, Int32) 
            -> [f Rect] 
            -> Maybe (f Rect)
whichScreen (mx, my) = getFirst . foldMap findOverlap
  where 
    findOverlap wrapped = 
      if (wrapped $> True) == fmap inside wrapped 
         then return wrapped
         else mempty
    inside Rect {..} = 
      mx >= x && 
        my >= y && 
          mx < x + fromIntegral w && 
            my < y + fromIntegral h

-- |The monitor must always be in the focus path.
-- If it's already there, this function does nothing.
fixMonitor :: Tiler (Fix Tiler) -> Tiler (Fix Tiler)
fixMonitor root =
  if isInPath $ Fix root
     then root
     else maybe (error "Uh oh") unfix $ insertMonitor $ Fix root
    where isInPath = cata $ \case
            Monitor _ -> True
            Wrap _ -> False
            InputController t -> fromMaybe False t
            FocusFull t -> t
            Reflect t -> t
            t@(Horiz _) -> getFocused t
            t@(Floating _) -> getFocused t
          insertMonitor = cata $ \case
            Monitor t -> join t
            InputController t -> Just $ Fix $ Monitor $ Just $ Fix $ InputController $ join t
            t -> Fix <$> reduce t
