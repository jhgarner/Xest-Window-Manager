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
-- add dir foc w r@(Reflect _ _) = fmap (Fix . add dir foc w . unfix) r
-- add _ _ _ (InputController _) =
--   error "Tried to add a window to an Input controller but that isn't allowed"
-- add Front Focused   t       w@(Wrap _) = Horiz $ makeFL [Sized 0 t, Sized 0 $ Fix w] 0
-- add Back  Focused   t       w@(Wrap _) = Horiz $ makeFL [Sized 0 $ Fix w, Sized 0 t] 1
-- add Front Unfocused t       w@(Wrap _) = Horiz $ makeFL [Sized 0 t, Sized 0 $ Fix w] 1
-- add Back  Unfocused t       w@(Wrap _) = Horiz $ makeFL [Sized 0 $ Fix w, Sized 0 t] 0

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
reduce (InputController t) = Just . InputController $ join t
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

-- | Render a given tiler. Operates in a continuous passing style so we can pretend like
-- catamorphisms operate from the root down to the leaves
placeWindow
  :: Transformer Plane
  -> Tiler (Fix Tiler)
  -> Tiler (Transformer Plane, Fix Tiler)
-- | Wraps place their wrapped window filling all available space
-- | If the window has no size, it gets unmapped
placeWindow _ (Wrap win) = Wrap win
placeWindow p (Reflect t) = Reflect (newTransformer, t)
  where trans (Plane (Rect rx ry rw rh) keepD) = Plane (Rect ry rx rh rw) keepD
        newTransformer = overReal (\(Plane r d) -> Plane r $ d+1) $ addTrans trans trans p
placeWindow (Transformer i o (Plane r depth)) (FocusFull (Fix t)) = 
  case t of
    InputController realt -> FocusFull $ (Transformer i o (Plane r $ depth + 1), Fix $ InputController realt)
    _ -> modFocused (first $ const (Transformer i o . Plane r $ depth + 1)) $ fmap (Transformer i o . Plane (Rect 0 0 0 0) $ depth + 1,) t

-- | Place tilers along an axis
placeWindow (Transformer input o p) (Horiz fl) =
    let numWins = fromIntegral $ flLength fl -- Find the number of windows
        location i lSize size = Rect (newX i lSize) y (w `div` fromIntegral numWins + round(size * fromIntegral w)) h
        newX i lSize = fromIntegral w `div` numWins * i + x + round (fromIntegral w * lSize)
     in Horiz $ fromVis realfl $ map (\(i, (lSize, size, t)) -> Sized size (Transformer input o . o . Plane (location i lSize size) $ depth + 1, t))
    $ zip [0 ..]
    $ vOrder realfl
  where realfl = fromVis fl . mapFold (\lSize (Sized modS t) -> (lSize + modS, (lSize, modS, t))) 0 $ vOrder fl
        (Plane Rect {..} depth) = input p

placeWindow (Transformer i o p) (Floating ls) =
  Floating $ map (\case
    Top (rr@RRect {..}, t) -> Top (rr, (Transformer i o . o $ Plane (Rect (round (fromIntegral x + fromIntegral w * xp)) (round(fromIntegral y + fromIntegral h * yp)) (round (fromIntegral w * wp)) (round (fromIntegral h * hp))) $ depth + 1, t))
    Bottom t -> Bottom (Transformer i o . o $ Plane r $ depth + 1, t)) ls
      where (Plane r@Rect{..} depth) = i p

-- | Has no effect on the placement
placeWindow trans (InputController t) =
  InputController $ (overReal (\(Plane Rect{..} depth) -> Plane (Rect (x + 2) (y + 10) (w - 6) (h - 14)) depth) trans,) <$> t
-- Can't be placed but will still take up space in other Tilers


-- | Given something that wants to modify a Tiler,
-- apply it to the first Tiler after the inputController
-- Often used with cata to create a new root Tiler
applyInput
  :: (Maybe (Tiler (Fix Tiler)) -> Maybe (Tiler (Fix Tiler))) -> Tiler (Fix Tiler) -> Fix Tiler
applyInput f (InputController t) = Fix . InputController . fmap Fix . f $ fmap unfix t
applyInput _ t                         = Fix t

onInput :: (Maybe (Fix Tiler) -> a) -> Fix Tiler -> a
onInput f root = f $ extract $ ana @(Beam _) findIC (Just root)
 where
  findIC (Just (Fix (InputController t))) = EndF t
  findIC (Just t) = ContinueF . Just . getFocused . unfix $ t
  findIC _ = EndF Nothing

-- | Modify the focused Tiler in another Tiler based on a function
modFocused :: (a -> a) -> Tiler a -> Tiler a
modFocused f (Horiz fl) = Horiz $ mapOne (Right Focused) (fmap f) fl
modFocused f (Floating (NE a as)) = Floating $ NE (fmap f a) as
modFocused f (Reflect t) = Reflect $ f t
modFocused f (FocusFull t) = FocusFull $ f t
modFocused _ wp@(Wrap _) = wp
modFocused f (InputController t) = InputController $ fmap f t --error "Input controller can't handle focus"


-- | Change the focus of a Tiler
focus :: Eq a => a -> Tiler a -> Tiler a
focus newF (  Horiz fl ) = Horiz $ focusElem (Sized 0 newF) fl
focus newF (  Floating ls ) = Floating $ moveF 0 ((== newF) . getEither) ls
focus _    t@(Wrap            _) = t
focus _    t@(Reflect _) = t
focus _    t@(FocusFull _) = t
focus _    t@(InputController _) = t
-- focus _    t@undefined          = t

-- |Given a window to focus, try to focus it. Should be called with cata.
-- fst returns the new tiler while snd gives some status information
--
-- The first element in the input tuple is the new child. The second
-- holds information about what lies down that child. The first element in
-- the bool tuple holds whether the input controller was originally down
-- there. The second holds whether the newly focused window is down there.
--
-- The returned tuple is similar; the first element is the new tiler and
-- the second is what exists down this path.
--
-- (False, False) is used as both the initial state and the ending state.
-- If we don't end in (False, False), it means the thing the user focused
-- isn't in our tree.
focusWindow
  :: Window
  -> Tiler (Maybe (Fix Tiler), ControllerOrWin)
  -> (Maybe (Fix Tiler), ControllerOrWin)
-- TODO I am particularly unhappy with the code quality of this function
focusWindow w (Wrap            w'             ) = (Just . Fix $ Wrap w', if w == w' then Win else Neither)
focusWindow _ (InputController (Just (Just t, Win)))  = (Just . Fix . InputController $ Just t, Both)
focusWindow _ (InputController t) = (t >>= fst, Controller)
focusWindow _ t                                 = case (hasController, shouldFocus) of
  (True , Just newFoc) -> (Just . Fix . InputController $ Fix . focus newFoc <$> dropExtra, Both)
  (False, Just newFoc) -> (Fix . focus newFoc <$> dropExtra, Win)
  _                   -> (fmap Fix dropExtra, if hasController then Controller else neitherOrBoth)
 where
   hasController = any ((== Controller) . snd) t
   shouldFocus   = find ((== Win) . snd) t >>= fst
   dropExtra     = reduce $ fmap fst t
   neitherOrBoth = if any ((== Both) . snd) t then Both else Neither


getDesktopState :: Tiler (Fix Tiler) -> ([Text], Int)
getDesktopState (Horiz fl) =
  (pack . show <$> [1 .. flLength fl], i)
  where i = findNeFocIndex fl
getDesktopState _ = (["None"], 0)

getFocusList :: Tiler String -> String
getFocusList (InputController s) = "*" ++ fromMaybe "" s
getFocusList (Horiz fl) = "Horiz|" ++ getItem (fst (pop (Right Focused) fl))
getFocusList (Floating (NE t _)) = "Floating|" ++ extract t
getFocusList (Reflect t) = "Rotate-" ++ t
getFocusList (FocusFull t) = "Full-" ++ t
getFocusList (Wrap _) = "window"
