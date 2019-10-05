{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}

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
ripOut :: Fix Tiler -> Tiler (Fix Tiler) -> Maybe (Tiler (Fix Tiler))
ripOut toDelete t = reduce $ fmap isEqual t
  where isEqual t = if t == toDelete then Nothing else Just t

-- | Removes empty Tilers
reduce :: Tiler (Maybe (Fix Tiler)) -> Maybe (Tiler (Fix Tiler))
-- Yo dog, I head you like fmaps
reduce (Horiz fl) = fmap Horiz $ fmap (fmap $ fromMaybe (error "Impossible")) <$> flFilter (not . null . getItem) fl
reduce (Floating ls) = newTiler
  where newTiler = Floating . fmap (fmap $ fromMaybe (error "Impossible")) <$> newFront
        newFront = filterNe (not . null . getEither) ls
reduce (InputController i t) = Just . InputController i $ join t
reduce (Monitor i t) = Just . Monitor i $ join t
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

-- | Places a tiler somewhere on the screen without actually placing it
placeWindow
  :: Bool -> Int -> [Rect] -> Transformer Plane
  -> Tiler (Fix Tiler)
  -> Tiler (Transformer Plane, Fix Tiler)
-- | Wraps place their wrapped window filling all available space
-- | If the window has no size, it gets unmapped
placeWindow _ _ _ _ (Wrap win) = Wrap win
placeWindow _ _ _ p (Reflect t) = Reflect (newTransformer, t)
  where trans (Plane (Rect rx ry rw rh) keepD) = Plane (Rect ry rx rh rw) keepD
        newTransformer = overReal (\(Plane r d) -> Plane r $ d+1) $ addTrans trans trans p
placeWindow _ _ _ (Transformer i o (Plane r depth)) (FocusFull (Fix t)) = 
  case t of
    InputController i' realt -> FocusFull (Transformer i o (Plane r $ depth + 1), Fix $ InputController i' realt)
    Monitor i' realt -> FocusFull (Transformer i o (Plane r $ depth + 1), Fix $ Monitor i' realt)
    _ -> modFocused (first $ const (Transformer i o . Plane r $ depth + 2)) $ fmap (Transformer i o . Plane (Rect 0 0 0 0) $ depth + 1,) t

-- | Place tilers along an axis
placeWindow _ _ _ (Transformer input o p) (Horiz fl) =
    let numWins = fromIntegral $ flLength fl -- Find the number of windows
        location i lSize size = Rect (newX i lSize) y (w `div` fromIntegral numWins + round(size * fromIntegral w)) h
        newX i lSize = fromIntegral w `div` numWins * i + x + round (fromIntegral w * lSize)
     in Horiz $ fromVis realfl $ map (\(i, (lSize, size, t)) -> Sized size (Transformer input o . o . Plane (location i lSize size) $ depth + 1, t))
    $ zip [0 ..]
    $ vOrder realfl
  where realfl = fromVis fl . mapFold (\lSize (Sized modS t) -> (lSize + modS, (lSize, modS, t))) 0 $ vOrder fl
        (Plane Rect {..} depth) = input p

placeWindow _ _ _ (Transformer i o p) (Floating ls) =
  Floating $ map (\case
    Top (rr@RRect {..}, t) -> Top (rr, (Transformer i o . o $ Plane (Rect (round (fromIntegral x + fromIntegral w * xp)) (round(fromIntegral y + fromIntegral h * yp)) (round (fromIntegral w * wp)) (round (fromIntegral h * hp))) $ depth + 1, t))
    Bottom t -> Bottom (Transformer i o . o $ Plane r $ depth + 1, t)) ls
      where (Plane r@Rect{..} depth) = i p

-- | Has no effect on the placement
placeWindow True i _ trans (InputController i' t)
  | i == i' = InputController i' $ (overReal (\(Plane Rect{..} depth) -> Plane (Rect (x + 2) (y + 10) (w - 6) (h - 14)) depth) trans,) <$> t
  | otherwise = InputController i' $ (overReal (\(Plane Rect{..} depth) -> Plane (Rect x y w h) depth) trans,) <$> t
placeWindow False _ _ trans (InputController i t) =
  InputController i $ (overReal (\(Plane Rect{..} depth) -> Plane (Rect x y w h) depth) trans,) <$> t

placeWindow _ _ screens trans (Monitor i t) =
   Monitor i $ (overReal (\(Plane _ depth) -> Plane (Rect x y w h) depth) trans,) <$> t
     where Rect x y w h = fromMaybe (error $ "Not enough screens " ++ show i) $ index screens i

-- | Given something that wants to modify a Tiler,
-- apply it to the first Tiler after the inputController
-- Often used with cata to create a new root Tiler
applyInput
  :: Int -> (Maybe (Tiler (Fix Tiler)) -> Maybe (Tiler (Fix Tiler))) -> Tiler (Fix Tiler) -> Fix Tiler
applyInput i f ic@(InputController i' t)
  | i == i' = Fix . InputController i' . fmap Fix . f $ fmap unfix t
  | otherwise = Fix ic
applyInput _ _ t                         = Fix t

onInput :: Int -> (Maybe (Fix Tiler) -> a) -> Fix Tiler -> a
onInput i f root = f $ extract $ ana @(Beam _) findIC  (Just root)
 where
  findIC (Just (Fix (InputController i' t)))
    | i == i' = EndF t
    | otherwise = ContinueF t
  findIC (Just (Fix (Monitor _ t))) = ContinueF t
  findIC (Just t) = ContinueF . Just . getFocused . unfix $ t
  findIC _ = EndF Nothing

-- | Modify the focused Tiler in another Tiler based on a function
modFocused :: (a -> a) -> Tiler a -> Tiler a
modFocused f (Horiz fl) = Horiz $ mapOne (Right Focused) (fmap f) fl
modFocused f (Floating (NE a as)) = Floating $ NE (fmap f a) as
modFocused f (Reflect t) = Reflect $ f t
modFocused f (FocusFull t) = FocusFull $ f t
modFocused _ wp@(Wrap _) = wp
modFocused f (InputController i t) = InputController i $ fmap f t
modFocused f (Monitor i t) = Monitor i $ fmap f t


-- | Change the focus of a Tiler
focus :: Eq a => a -> Tiler a -> Tiler a
focus newF (  Horiz fl ) = Horiz $ focusElem (Sized 0 newF) fl
focus newF (  Floating ls ) = Floating $ moveF 0 ((== newF) . getEither) ls
focus _    t@(Wrap _) = t
focus _    t@(Reflect _) = t
focus _    t@(FocusFull _) = t
focus _    t@(InputController _ _) = t
focus _    t@(Monitor _ _) = t
-- focus _    t@undefined          = t

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
        hasBothIndividually t = 
          if any (isUnmovable . snd) t 
             then asum $ fmap (getMovable . snd) t
             else Nothing

-- |Specialization of the above for moving an InputController towards a window
moveToWindow
  :: Window
  -> Int
  -> Fix Tiler
  -> (Maybe (Fix Tiler), Bool)
moveToWindow window i =
  second isBoth . cata (moveToClosestParent isWindow isInputController)
  where isInputController (InputController i' t)
          | i == i' = Just (Just . Fix . InputController i' . fmap Fix, join t)
          | otherwise = Nothing
        isInputController _ = Nothing
        isWindow (Wrap childParent) = inChildParent window childParent
        isWindow _ = False

-- |Specialization of the above for moving a moniter towards the inputController
moveToIC
  :: Int
  -> Fix Tiler
  -> (Maybe (Fix Tiler), Bool)
moveToIC screenNumber =
  second isBoth . cata (moveToClosestParent isInputController isMonitor)
  where isMonitor (Monitor i t) 
          | i == screenNumber = Just (Just . Fix . Monitor i . fmap Fix, join t)
          | otherwise = Nothing
        isMonitor _ = Nothing
        isInputController (InputController i _) = screenNumber == i
        isInputController _ = False

-- |Do both of the above in sequence. This is the function that's actually used elsewhere
focusWindow
  :: Int
  -> Window
  -> Fix Tiler
  -> Maybe (Fix Tiler)
focusWindow screenNumber window root =
  let (newRoot, b1) = moveToWindow window screenNumber root
      (newestRoot, b2) = maybe (Nothing, False) (moveToIC screenNumber) newRoot
   in if b1 && b2 then newestRoot else Nothing

getDesktopState :: Tiler (Fix Tiler) -> ([Text], Int)
getDesktopState (Horiz fl) =
  (pack . show <$> [1 .. flLength fl], i)
  where i = findNeFocIndex fl
getDesktopState _ = (["None"], 0)

getFocusList :: Tiler String -> String
getFocusList (InputController i s) = "*" ++ show i ++ "*" ++ fromMaybe "" s
getFocusList (Monitor i s) = "@" ++ show i ++ "@" ++ fromMaybe "" s
getFocusList (Horiz fl) = "Horiz|" ++ getItem (fst (pop (Right Focused) fl))
getFocusList (Floating (NE t _)) = "Floating|" ++ extract t
getFocusList (Reflect t) = "Rotate|" ++ t
getFocusList (FocusFull t) = "Full|" ++ t
getFocusList (Wrap _) = "window"

findParent :: Window -> Fix Tiler -> Maybe Window
findParent w = cata step
  where step (Wrap (ChildParent ww ww')) 
          | ww' == w = Just ww
          | otherwise = Nothing
        step t = foldl' (<|>) Nothing t

whichScreen :: (Eq (f Bool), Functor f, Show (f Rect)) => (Int32, Int32) -> [f Rect] -> f Rect
whichScreen (mx, my) rects = fromMaybe (trace ("GOT: " ++ show mx ++ "," ++ show my ++ " " ++ show rects) (error "Where's the mouse?")) $ foldl' findOverlap Nothing rects
  where findOverlap acc wrapped = acc <|> if (wrapped $> True) == fmap inside wrapped then Just wrapped else Nothing
        inside Rect {..} = mx >= x && my >= y && mx < x + fromIntegral w && my < y + fromIntegral h
