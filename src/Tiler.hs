{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Tiler where

import           ClassyPrelude hiding (Reader, ask)
import           Data.Fixed
import           Graphics.X11.Types
import           Data.Functor.Foldable
import           Types
import           FocusList
import           Base
import           Polysemy
import           Polysemy.Reader

-- Tiler Handling Code --

-- | Add a new Tiler at the given location. Is the opposite of popping.
add :: Direction -> Focus -> Fix Tiler -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
add dir foc w (Directional d fl) = Directional d $ push dir foc w fl
add _ _ _ (InputController _) =
  error "Tried to add a window to an Input controller but that isn't allowed"
add _     _         (Fix w) EmptyTiler = w
add Front Focused   t       w@(Wrap _) = Directional X $ makeFL [t, Fix w] 0
add Back  Focused   t       w@(Wrap _) = Directional X $ makeFL [Fix w, t] 1
add Front Unfocused t       w@(Wrap _) = Directional X $ makeFL [t, Fix w] 1
add Back  Unfocused t       w@(Wrap _) = Directional X $ makeFL [Fix w, t] 0

-- | Remove a Tiler if it exists
-- TODO Figure out whether I want to pass around Fixes or Tilers here
remove :: Fix Tiler -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
remove toDelete = reduce . map isEqual
  where isEqual t = if t == toDelete then Fix EmptyTiler else t

isEmpty :: Tiler (Fix Tiler) -> Bool
isEmpty (Directional _ fl) = isNull fl
isEmpty EmptyTiler         = True
isEmpty _                  = False

-- | Removes empty Tilers and EmptyTilers
reduce :: Tiler (Fix Tiler) -> Tiler (Fix Tiler)
reduce (Directional d fl) = if isEmpty newTiler then EmptyTiler else newTiler
  where newTiler = Directional d $ flFilter (/= Fix EmptyTiler) fl
reduce t@(InputController _) = t
reduce t                     = t


-- | A combination of top and pop if you're coming from c++.
-- fst . popWindow is like top and snd . popWindow is like pop
popWindow
  :: Either Direction Focus
  -> Tiler (Fix Tiler)
  -> (Maybe (Fix Tiler), Tiler (Fix Tiler))

popWindow howToPop (Directional d fl) =
  second (Directional d) $ pop howToPop fl

popWindow _ (  InputController t) = (Just t, EmptyTiler)
popWindow _ t@(Wrap            _) = (Just $ Fix t, EmptyTiler)
popWindow _ EmptyTiler            = (Nothing, EmptyTiler)


-- | Render a given tiler. Operates in a continuous passing style so we can pretend like
-- catamorphisms operate from the root down to the leaves
placeWindows
  :: (Member WindowMover r
     , Member WindowMinimizer r
     , Member (Reader Borders) r
     , Member Colorer r
     )
  => Tiler (Plane -> Sem r ())
  -> Plane
  -> Sem r ()
-- | Wraps place their wrapped window filling all available space
-- | If the window has no size, it gets unmapped
placeWindows (Wrap win) (Plane (Rect _ _ 0 0) _) = minimize win
placeWindows (Wrap win) p              = do
  restore win
  changeLocation win $ rect p

-- | Place tilers along an axis
placeWindows (Directional d fl) (Plane oldR@Rect {..} depth) =
  traverse_ (\(i, f) -> f $ Plane (location d i) $ depth + 1)
    $ zip [0 ..]
    $ vOrder fl
 where
  numWins = fromIntegral $ flLength fl -- Find the number of windows
  location X i = Rect (newX i) y (w `div` fromIntegral numWins) h
  location Y i = Rect x (newY i) w (h `div` fromIntegral numWins)
  location Z i = if i == fromIntegral (fromMaybe 1000 (getFocIndex fl))
    then oldR
    else Rect 0 0 0 0

  newX i = fromIntegral w `div` numWins * i + x
  newY i = fromIntegral h `div` numWins * i + y


-- | Has no effect on the placement
placeWindows (InputController f) (Plane Rect {..} depth) = do
  -- Extract the border windows
  (l, u, r, d) <- ask @(Window, Window, Window, Window)
  let winList = [l, u, r, d]

  -- Calculate the color for our depth
  let hue = 360.0 * ((0.5 + (fromIntegral depth * 0.618033988749895)) `mod'` 1)
  color <- getColor $ "TekHVC:"++show hue++"/50/95"

  -- Convince our windows to be redrawn with the right color and position
  traverse_ (`changeLocation` Rect 0 0 1 1) winList
  traverse_ (`changeColor` color) winList
  changeLocation l $ Rect x y 5 h
  changeLocation u $ Rect x y w 5
  changeLocation d $ Rect x (y+fromIntegral h-5) w 5
  changeLocation r $ Rect (x+fromIntegral w-5) y 5 h
  
  
  f $ Plane (Rect (x + 5) (y + 5) (w - 10) (h - 10)) depth
-- | Can't be placed but will still take up space in other Tilers
placeWindows EmptyTiler          _ = return ()

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
modFocused f (Directional d fl) = Directional d $ mapOne (Right Focused) f fl
modFocused f (   InputController t) = InputController $ f t
modFocused _ wp@(Wrap            _) = wp
modFocused _ EmptyTiler             = EmptyTiler


-- | Change the focus of a Tiler
focus :: Fix Tiler -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
focus newF (  Directional d fl ) = Directional d $ focusElem newF fl
focus _    t@(InputController _) = t
focus _    t@(Wrap            _) = t
focus _    t@EmptyTiler          = t

-- | Given a window to focus, try to focus it. Should be called with cata.
-- fst returns the new tiler while snd gives some status information
-- (newTiler, (parent to InputController, parent to new focus))
focusWindow
  :: Window
  -> Tiler (Tiler (Fix Tiler), (Bool, Bool))
  -> (Tiler (Fix Tiler), (Bool, Bool))
focusWindow w (Wrap            w'             ) = (Wrap w', (False, w == w'))
focusWindow _ (InputController (t, (_, False))) = (t, (True, False))
focusWindow _ (InputController (t, (_, True))) =
  -- Reset to (False, False) since we don't need to change anything
  (InputController $ Fix t, (False, False))
focusWindow _ t = case (hasController, hasWind) of
  -- Reset to False False so no other parents try to make Input Controllers
  (True , True) -> (InputController $ Fix (focus findFocused dropExtra), (False, False))
  (False, True) -> (focus findFocused dropExtra, (False, True))
  bs            -> (dropExtra, bs)
 where
  hasController    = any (fst . snd) t
  hasWind          = any (snd . snd) t
  dropExtra        = fmap (Fix . fst) t
  Just findFocused = foldl'
    (\acc (ct, (_, b)) -> acc <|> if b then Just $ Fix ct else Nothing)
    Nothing
    t

getDesktopState :: Tiler (Fix Tiler) -> ([Text], Int)
getDesktopState (Directional _ fl) =
  (pack . show <$> [1 .. flLength fl], fromMaybe 0 i)
  where i = getFocIndex fl
getDesktopState _ = (["None"], 0)
