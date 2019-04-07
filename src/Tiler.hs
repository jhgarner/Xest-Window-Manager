{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tiler where

import           ClassyPrelude
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Window
import           Graphics.X11.Xlib.Event
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as MV
import           Data.Functor.Foldable
import           Types

-- Tiler Handling Code --

-- | Add a new Tiler at the given location. Is the opposite of popping.
add :: Direction -> Focus -> Fix Tiler -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
add Front Focused w (Directional d FL {..}) =
  Directional d $ FL 0 (V.cons w elements)
add Front Focused w t@(Wrap _) = Directional X . FL 0 $ fromList [w, Fix t]
add Back Focused w (Directional d FL {..}) =
  Directional d $ FL (V.length elements) (V.snoc elements w)
add Back Focused w t@(Wrap _) = Directional X . FL 1 $ fromList [Fix t, w]

add Front Unfocused w (Directional d FL {..}) =
  Directional d $ FL (focusedElement + 1) (V.cons w elements)
add Front Unfocused w t@(Wrap _) = Directional X . FL 1 $ fromList [w, Fix t]
add Back Unfocused w (Directional d FL {..}) =
  Directional d $ FL focusedElement (V.snoc elements w)
add Back Unfocused w t@(Wrap _) = Directional X . FL 0 $ fromList [Fix t, w]

add _ _ _ (InputController _) =
  error "Tried to add a window to an Input controller but that isn't allowed"
add _ _ (Fix w) EmptyTiler = w

-- | Remove a Tiler if it exists
-- TODO Figure out whether I want to pass around Fixes or Tilers here
remove :: Tiler (Fix Tiler) -> Tiler (Fix Tiler) -> Fix Tiler
remove toDelete = Fix . reduce . isEqual
  where isEqual t = if t == toDelete then EmptyTiler else t

-- | Removes empty Tilers and EmptyTilers
reduce :: Tiler (Fix Tiler) -> Tiler (Fix Tiler)
reduce (Directional d FL {..}) = if V.length newVec == 0
  then EmptyTiler
  else Directional d $ FL shiftFoc newVec
 where
  newVec   = filter (\(Fix a) -> a /= EmptyTiler) elements
  shiftFoc = if focusedElement >= V.length newVec
    then V.length newVec - 1
    else focusedElement
reduce t@(InputController _) = t
reduce t@(Wrap            _) = t
reduce EmptyTiler            = EmptyTiler


-- | A combination of top and pop if you're coming from c++.
-- fst . popWindow is like top and snd . popWindow is like pop
popWindow
  :: Either Direction Focus
  -> Fix Tiler
  -> (Maybe (Fix Tiler), Tiler (Fix Tiler))

popWindow (Left Front) (Fix (Directional d FL {..})) =
  (elements V.!? 0, Directional d $ FL newFocused newTiler)
 where
  newTiler = if V.null elements then elements else V.tail elements
  newFocused =
    if focusedElement == 0 then focusedElement else focusedElement - 1

popWindow (Left Back) (Fix (Directional d FL {..})) =
  (safeLast, Directional d $ FL newFocused newTiler)
 where
  safeLast   = elements V.!? (V.length elements - 1)
  newTiler   = if V.null elements then elements else V.init elements
  newFocused = if focusedElement == V.length elements - 1
    then focusedElement - 1
    else focusedElement

popWindow (Right Focused) (Fix (Directional d FL {..})) =
  (safeAt, Directional d $ FL newFocused newTiler)
 where
  safeAt   = elements V.!? focusedElement
  newTiler = if V.null elements
    then elements
    else V.ifilter (\i _ -> i /= focusedElement) elements
  newFocused = if focusedElement + 1 == V.length elements
    then focusedElement - 1
    else focusedElement

popWindow _ (Fix (InputController t)) = (Just t, EmptyTiler)
popWindow _ (Fix t@(Wrap _)) = (Just $ Fix t, EmptyTiler)
popWindow _ (Fix EmptyTiler) = (Nothing, EmptyTiler)


-- | Render a given tiler. Operates in a continuous passing style so we can pretend like
-- catamorphisms operate from the root down to the leaves
placeWindows :: Tiler (Rect -> Xest ()) -> Rect -> Xest ()
-- | Wraps place their wrapped window filling all available space
-- | If the window has no size, it gets unmapped
placeWindows (Wrap win) (Rect _ _ 0 0) = do
  IS {..} <- ask
  liftIO $ unmapWindow display win
placeWindows (Wrap win) Rect {..} = do
  IS {..} <- ask
  safeMap win
  liftIO $ moveWindow display win x y
  liftIO $ resizeWindow display win w h

-- | Place tilers along an axis
placeWindows (Directional d FL { focusedElement = fe, elements = e }) Rect {..}
  = V.ifoldl' (\acc i f -> acc >> f (location d (fromIntegral i))) (pure ()) e
 where
  numWins = fromIntegral $ V.length e -- Find the number of windows in ts
  location X i = Rect (newX i) y (w `div` fromIntegral numWins) h
  location Y i = Rect x (newY i) w (h `div` fromIntegral numWins)
  location Z i | fromIntegral i == fe = Rect x y w h
               | otherwise            = Rect 0 0 0 0
  newX i = fromIntegral w `div` numWins * i + x
  newY i = fromIntegral h `div` numWins * i + y

-- | Has no effect on the placement
placeWindows (InputController f) r = f r
-- | Can't be placed but will still take up space in other Tilers
placeWindows EmptyTiler          _ = return ()

-- | Given something that wants to modify a Tiler,
-- apply it to the first Tiler after the inputController
applyInput
  :: (Tiler (Fix Tiler) -> Tiler (Fix Tiler)) -> Tiler (Fix Tiler) -> Fix Tiler
applyInput f (InputController (Fix t)) = Fix . InputController . Fix $ f t
applyInput _ t                         = Fix t

-- | Modify the focused Tiler in another Tiler based on a function
modFocused :: (Fix Tiler -> Fix Tiler) -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
modFocused f (Directional d FL {..}) = Directional d $ FL focusedElement newVec
 where
  oldFoc = elements V.! focusedElement
  newVec = V.modify (\v -> MV.write v focusedElement $ f oldFoc) elements
modFocused f (   InputController t) = InputController $ f t
modFocused _ wp@(Wrap            _) = wp
modFocused _ EmptyTiler             = EmptyTiler


-- | Change the focus of a Tiler
focus :: Fix Tiler -> Fix Tiler -> Fix Tiler
focus newF (Fix (Directional d FL {..})) = Fix . Directional d $ FL newIndex
                                                                    elements
  where newIndex = fromMaybe focusedElement $ V.findIndex (== newF) elements
focus _ t@(Fix (InputController _)) = t
focus _ t@(Fix (Wrap            _)) = t
focus _ t@(Fix EmptyTiler         ) = t

-- | Given a window to focus, try to focus it. Should be called with cata.
focusWindow
  :: Window -> Tiler (Fix Tiler, (Bool, Bool)) -> (Fix Tiler, (Bool, Bool))
focusWindow w (Wrap w') = (Fix $ Wrap w', (False, w == w'))
focusWindow _ (InputController (t, (_, False))) = (t, (True, False))
focusWindow _ (InputController (t, (_, True))) =
  (Fix $ InputController t, (False, False))
focusWindow _ t = case (hasController, hasWind) of
  (True , True) -> (Fix . InputController $ dropExtra, (False, False))
  (False, True) -> (focus findFocused dropExtra, (False, True))
  bs            -> (dropExtra, bs)
 where
  hasController    = any (fst . snd) t
  hasWind          = any (snd . snd) t
  dropExtra        = Fix $ fmap fst t
  Just findFocused = foldl'
    (\acc (ct, (_, b)) -> acc <|> if b then Just ct else Nothing)
    Nothing
    t

safeMap :: Window -> Xest ()
safeMap win = do
  IS {..} <- ask
  WindowAttributes { wa_map_state = mapped } <- liftIO
    $ getWindowAttributes display win
  when (mapped == waIsUnmapped) . liftIO $ mapWindow display win
