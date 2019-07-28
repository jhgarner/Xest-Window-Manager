{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Tiler where

import           Standard
import           Graphics.X11.Types
import           Types
import           FocusList
import           Base
import           Polysemy
import           Polysemy.Reader
-- import           Data.Foldable (find)

-- Tiler Handling Code --

-- | Add a new Tiler at the given location. Is the opposite of popping.
add :: Direction -> Focus -> Fix Tiler -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
add dir foc w (Directional d fl) = Directional d $ push dir foc (Sized 0 w) fl
add _ foc w (Floating b ls) = Floating b if foc == Focused then Just (RRect 0 0 0.2 0.2, w) : ls else ls ++ [Just (RRect 0 0 0.2 0.2, w)]
add _ _ _ (InputController _) =
  error "Tried to add a window to an Input controller but that isn't allowed"
add _     _         (Fix w) EmptyTiler = w
add Front Focused   t       w@(Wrap _) = Directional X $ makeFL [Sized 0 t, Sized 0 $ Fix w] 0
add Back  Focused   t       w@(Wrap _) = Directional X $ makeFL [Sized 0 $ Fix w, Sized 0 t] 1
add Front Unfocused t       w@(Wrap _) = Directional X $ makeFL [Sized 0 t, Sized 0 $ Fix w] 1
add Back  Unfocused t       w@(Wrap _) = Directional X $ makeFL [Sized 0 $ Fix w, Sized 0 t] 0

-- | Remove a Tiler if it exists
remove :: Fix Tiler -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
remove toDelete = reduce . map isEqual
  where isEqual t = if t == toDelete then Fix EmptyTiler else t

isEmpty :: Tiler (Fix Tiler) -> Bool
isEmpty (Directional _ fl) = isNull fl
isEmpty (Floating b ls)    = (b == Nothing  || b == Just (Fix EmptyTiler)) && null ls
isEmpty EmptyTiler         = True
isEmpty _                  = False

-- | Removes empty Tilers and EmptyTilers
reduce :: Tiler (Fix Tiler) -> Tiler (Fix Tiler)
reduce (Directional d fl) = if isEmpty newTiler then EmptyTiler else newTiler
  where newTiler = Directional d $ flFilter ((/= Fix EmptyTiler) . getItem) fl
reduce (Floating b ls) = if isEmpty newTiler then EmptyTiler else newTiler
  where newTiler = Floating (b >>= (\bb -> if bb == Fix EmptyTiler then Nothing else Just bb)) $ filter (fromMaybe True . fmap ((/= Fix EmptyTiler) . snd)) ls
reduce t@(InputController _) = t
reduce t                     = t


-- | A combination of top and pop if you're coming from c++.
-- fst . popWindow is like top and snd . popWindow is like pop
popWindow
  :: Either Direction Focus
  -> Tiler a
  -> (Maybe a, Tiler a)

popWindow howToPop (Directional d fl) =
  fmap getItem *** Directional d $ pop howToPop fl
  -- TODO refactor all of this
popWindow howToPop (Floating b ls) = case howToPop of
  Left _ -> case fromMaybe (error "woops") $ headMay ls of
             Nothing -> (Just $ fromMaybe (error "woops") b, newLS Nothing)
             Just (_, t) -> (Just t, newLS b)
  Right Focused -> case fromMaybe (error "woops") $ headMay ls of
             Nothing -> (Just $ fromMaybe (error "woops") b, newLS Nothing)
             Just (_, t) -> (Just t, newLS b)
  _ -> case fromMaybe (error "woops") $ lastMay ls of
             Nothing -> (Just $ fromMaybe (error "woops") b, otherLS Nothing)
             Just (_, t) -> (Just t, otherLS b)
  where newLS b = if length ls == 1 then EmptyTiler else (fromMaybe (error "woops") $ Floating b <$> tailMay ls)
        otherLS b = if length ls == 1 then EmptyTiler else (fromMaybe (error "woops") $ Floating b <$> initMay ls)

popWindow _ (InputController   t) = (Just t, EmptyTiler)
popWindow _ (Wrap              _) = (Nothing, EmptyTiler)
popWindow _ EmptyTiler            = (Nothing, EmptyTiler)

getFocused :: Tiler a -> Maybe a
getFocused = fst . popWindow (Right Focused)

-- | Render a given tiler. Operates in a continuous passing style so we can pretend like
-- catamorphisms operate from the root down to the leaves
placeWindow
  :: Plane
  -> Tiler (Fix Tiler)
  -> Tiler (Plane, Fix Tiler)
-- | Wraps place their wrapped window filling all available space
-- | If the window has no size, it gets unmapped
placeWindow _ (Wrap win) = Wrap win

placeWindow (Plane oldR depth) (Directional Z fl) =
    Directional Z $ fromFoc fl fUncons
 where
  fUncons :: [Sized (Plane, Fix Tiler)]
  fUncons = case fOrder fl of
    a:as -> Sized (getSize a) (Plane oldR $ depth + 1, getItem a)
              : map (\(Sized d a) -> Sized d (Plane (Rect 0 0 0 0) $ depth+1, a)) as
    [] -> []

-- | Place tilers along an axis
placeWindow (Plane Rect {..} depth) (Directional d fl) =
  Directional d $ fromVis realfl $ map (\(i, (prev, lSize, size, t)) -> Sized size (Plane (location d i prev lSize size) $ depth + 1, t))
    $ zip [0 ..]
    $ vOrder realfl
 where
  realfl = mapFold (\(lSize, prev) (Sized modS t) -> ((lSize + modS, modS), (prev, lSize, modS, t))) (0, 0) fl

  numWins = fromIntegral $ flLength fl -- Find the number of windows
  location X i prev lSize size = Rect (newX i lSize) y (w `div` fromIntegral numWins + round((size) * fromIntegral w)) h
  location Y i prev lSize size = Rect x (newY i lSize) w (h `div` fromIntegral numWins + round((size) * fromIntegral h))
  location Z _ _ _ _= error "Z shouldn't be here"

  newX i lSize = fromIntegral w `div` numWins * i + x + round (fromIntegral w * lSize)
  newY i lSize = fromIntegral h `div` numWins * i + y + round (fromIntegral h * lSize)

placeWindow (Plane r@Rect{..} depth) (Floating b ls) =
  Floating (Just (Plane r $ depth + 1, fromMaybe (Fix EmptyTiler) b)) $ map (\case
    Just (r@(RRect xp yp wp hp), t) -> Just (r, (Plane (Rect (round (fromIntegral x + fromIntegral w * xp)) (round(fromIntegral y + fromIntegral h * yp)) (round (fromIntegral w * wp)) (round (fromIntegral h * hp))) $ depth + 1, t))
    Nothing -> Nothing) ls

-- | Has no effect on the placement
placeWindow (Plane Rect{..} depth) (InputController t) =
    InputController (Plane (Rect (x + 5) (y + 5) (w - 10) (h - 10)) depth, t)
-- Can't be placed but will still take up space in other Tilers
placeWindow _ EmptyTiler = EmptyTiler


-- | Given something that wants to modify a Tiler,
-- apply it to the first Tiler after the inputController
-- Often used with cata to create a new root Tiler
applyInput
  :: (Tiler (Fix Tiler) -> Tiler (Fix Tiler)) -> Tiler (Fix Tiler) -> Fix Tiler
applyInput f (InputController (Fix t)) = Fix . InputController . Fix $ f t
applyInput _ t                         = Fix t

onInput :: (Tiler (Fix Tiler) -> a) -> Fix Tiler -> a
onInput f root = fromMaybe (error "No Controller found") $ para doInput root
 where
  doInput (InputController (Fix t, _)) = Just $ f t
  doInput t = foldl' (\acc a -> acc <|> snd a) Nothing t

-- | Modify the focused Tiler in another Tiler based on a function
modFocused :: (Fix Tiler -> Fix Tiler) -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
modFocused f (Directional d fl) = Directional d $ mapOne (Right Focused) (fmap f) fl
modFocused f (Floating b ls) = case fromMaybe (error "woops") $ headMay ls of
                                 Just t -> Floating b $ Just (second f t) : fromMaybe (error "woops") (tailMay ls)
                                 Nothing -> Floating (Just $ f $ fromMaybe (Fix EmptyTiler) b) ls
modFocused f (InputController t) = InputController $ f t
modFocused _ wp@(Wrap _) = wp
modFocused _ EmptyTiler = EmptyTiler


-- | Change the focus of a Tiler
focus :: Fix Tiler -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
focus newF (  Directional d fl ) = Directional d $ focusElem (Sized 0 newF) fl
focus newF (  Floating    (Just b) ls ) = Floating (Just b) if b == newF then Nothing : filter (/= Nothing) ls else join (find (isIt False) ls) : filter (not . isIt True) ls
  where isIt def = fromMaybe def . fmap ((== newF) . snd)
focus newF (  Floating    Nothing ls ) = Floating Nothing $ join (find (isIt False) ls) : filter (not . isIt True) ls
  where isIt def = fromMaybe def . fmap ((== newF) . snd)
focus _    t@(InputController _) = t
focus _    t@(Wrap            _) = t
focus _    t@EmptyTiler          = t

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
  -> Tiler (Tiler (Fix Tiler), (Bool, Bool))
  -> (Tiler (Fix Tiler), (Bool, Bool))
-- Todo make this mess of parantheses less terrifying
focusWindow w (Wrap            w'             ) = (Wrap w', (False, w == w'))
focusWindow _ (InputController (t, (_, False))) = (t, (True, False))
focusWindow _ (InputController (t, (_, True)))  = (InputController $ Fix t, (False, False))
focusWindow _ t                                 = case (hasController, shouldFocus) of
  (True , Just newFoc) -> (InputController $ Fix (focus newFoc dropExtra), (False, False))
  (False, Just newFoc) -> (focus newFoc dropExtra, (False, True))
  bs                   -> (dropExtra, (fst bs, False))
 where
  hasController = any (fst . snd) t
  shouldFocus   = Fix . fst <$> find (snd . snd) t
  dropExtra     = fmap (Fix . fst) t


getDesktopState :: Tiler (Fix Tiler) -> ([Text], Int)
getDesktopState (Directional _ fl) =
  (pack . show <$> [1 .. flLength fl], fromMaybe 0 i)
  where i = getFocIndex fl
getDesktopState _ = (["None"], 0)
