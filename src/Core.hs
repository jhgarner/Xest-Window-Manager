{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Core where

import           ClassyPrelude           hiding ( ask
                                                , asks
                                                , Reader
                                                )
import           Base
import           Polysemy
import           Polysemy.State
import           Polysemy.Reader
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Types
import           Types
import           Data.Functor.Foldable
import           Data.Either                    ( )
import           Tiler
import           FocusList

-- Event Handlers --

-- | This postprocessor is used while holding down a key but before another key has been pressed
newModePostprocessor :: KeyPostprocessor r
-- A button was pressed so we need to change how we handle releasing the key
newModePostprocessor oldMode boundT (KeyboardEvent _ True) =
  [ChangePostprocessor $ Temp oldMode boundT]

-- A button was released so go back to the normal handler if it was the key we were watching
newModePostprocessor _ boundT (KeyboardEvent kt False)
  | boundT == kt = [ChangePostprocessor Default]
  | otherwise    = []

-- Otherwise do nothing
newModePostprocessor _ _ _ = []


-- | Postprocessor used when another key is clicked while holding one down
tempModePostprocessor :: KeyPostprocessor r
-- On release, return to the old mode
tempModePostprocessor oldMode boundKey (KeyboardEvent k False)
  | k == boundKey = [ChangePostprocessor Default, ChangeModeTo oldMode]
  | otherwise     = []

-- Otherwise defer to handler
tempModePostprocessor om bk (KeyboardEvent _ True) = [ChangePostprocessor $ Temp om bk]
tempModePostprocessor _ _ _ = []


-- | The bulk of the program
-- Performs some action and returns a list of new actions to be performed
-- Returning a [] means that there aren't any new tasks to do.
handler :: Action -> DoAll r
-- Called on window creation
handler (XorgEvent MapRequestEvent {..}) = do
  -- managing a window allows us to do any number of things to it
  -- Currently we wrap it in a new type and ask for crossing events
  tWin <- manage ev_window
  -- Resend the mapWindow Xorg event.
  -- We don't receive the mapWindow event from this because Xorg knows we sent it.
  restore ev_window
  -- This adds the new window to whatever tiler comes after inputController
  -- If you've zoomed the inputController in, you get nesting as a result
  modify $ cata $ applyInput (add Front Focused tWin)
  -- Make the newly created window into the focused one
  -- We have to keep Xorg's idea of focus in sync with our own
  setFocus ev_window
  return []

-- Called on window destruction
handler (XorgEvent DestroyWindowEvent {..}) = do
  -- Remove the destroyed window from our tree.
  -- Usually, unmap will be called first, but what if a minimized window
  -- gets killed? In that case, we won't get an unmap notifiction.
  modify @(Fix Tiler) $ cata (Fix . remove (Fix $ Wrap ev_window))
  return []

-- Called when a window should no longer be drawn
-- This either happens when the window dies, or when we minimize it
handler (XorgEvent UnmapEvent {..}) = do
  mins <- get @(Set Window)
  -- Remove the destroyed window from our tree if we aren't the
  -- reason it was unmapped.
  unless (member ev_window mins) $ modify $ cata
    (Fix . remove (Fix $ Wrap ev_window))
  return []

-- Tell the window it can configure itself however it wants
-- We send back the Configure Request unmodified
handler (XorgEvent cre@ConfigureRequestEvent{}) = do
  configureWin cre
  return []

-- Called whet a watched key is pressed or released
handler (XorgEvent KeyEvent {..}) = do
  Conf bindings _ <- ask @Conf
  return $ case find (\(k, _, _) -> ev_keycode == k) bindings of
    Nothing -> []
    Just kt -> [KeyboardEvent kt (ev_event_type == keyPress)]

-- Called when the cursor moves between windows
handler (XorgEvent CrossingEvent {..}) = do
  rootT <- get @(Fix Tiler)
  -- Make certain that the focused window is the one we're hovering over
  setFocus ev_window
  let (newRoot, status) = cata (focusWindow ev_window) rootT
  -- If status is anything but (False, False), it means that something super
  -- weird is going on and the newRoot is probably bad.
  -- Usually this happens if the crossed window isn't in our tree and causes
  -- the InputController to disappear.
  when (status == (False, False)) $ put newRoot
  return []

-- Handle all other xorg events as noops
handler (XorgEvent _) = return []

-- Run a shell command
handler (RunCommand s) = execute s >> return []

-- Run a shell command
handler (ChangePostprocessor m) = put m >> return []

-- Perform a keyboard event if we are in the correct mode
handler (KeyboardEvent kt@(_, _, actions) True) = do
  currentMode <- get
  return $ ChangePostprocessor (New currentMode kt) : actions

-- Ignore keyups
-- Note that the postprocessor will handle these if needed
handler (KeyboardEvent _ False) = return []

-- Show a window given its class name
handler (ShowWindow wName) = do
  wins <- getWindowByClass wName
  forM_ wins restore
  return []

-- Hide a window given its class name
handler (HideWindow wName) = do
  wins <- getWindowByClass wName
  forM_ wins minimize
  return []

-- Zoom the inputController towards the focused window
handler ZoomInInput = do
  root <- get @(Fix Tiler)
  put $ cata reorder root
  return []
 where
  reorder (InputController (Fix t)) =
    Fix $ modFocused (Fix . InputController) t
  reorder t = Fix t

-- Move the input controller towards the root
handler ZoomOutInput = do
  root <- get
  -- Don't zoom the controller out of existence
  unless (isController root) $ put $ para reorder root
  return []
 where
  reorder (InputController (_, t)) = t
  reorder t                        = Fix $ if any (isController . fst) t
    then InputController . Fix $ snd <$> t
    else snd <$> t
  isController (Fix (InputController _)) = True
  isController _                         = False

-- Change the given mode to something else
handler (ChangeModeTo newM) = do
  eActions <- gets @Mode exitActions
  rebindKeys newM
  put newM
  --Combine the two lists of actions to be executed. Execute exit actions first.
  return $ eActions ++ introActions newM

-- Change the layout of whatever comes after the input controller to something else
handler (ChangeLayoutTo (Fix newT)) = do
  modify $ cata (applyInput (changeLayout newT))
  return []

-- Change the focus to some named action
handler (ChangeNamed s) = do
  modify $ cata (applyInput changer)
  xFocus
  return []
 where
  changer t@(Directional d fl) = case readMay s of
    Just i  -> Directional d $ focusIndex (i - 1) fl
    Nothing -> t
  changer t = t

-- Change focus in a given direction
handler (Move newD) = do
  modify $ cata (applyInput (changer newD))
  xFocus
  return []
 where
  changer dir (Directional d fl) = Directional d $ focusDir dir fl
  changer _   t                  = t

-- | Move all of the Tilers from root to newT
changeLayout :: Tiler (Fix Tiler) -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
changeLayout newT root = doPopping root newT
 where
  doPopping ot t = case popWindow (Left Front) ot of
    (Nothing, _) -> t
    (Just win, wins) ->
      doPopping wins $ add Back (isFocused win) win t
  isFocused t = if t == focused then Focused else Unfocused
  focused = fromMaybe (Fix EmptyTiler) . fst $ popWindow (Right Focused) root

-- Random stuff --

-- Would be used for reparenting (title bar)
manage :: Member AttributeWriter r => Window -> Sem r (Fix Tiler)
manage w = do
  selectFlags w enterWindowMask
  return . Fix $ Wrap w

-- Find a window with a class name
getWindowByClass
  :: Members [GlobalX, AttributeReader] r
  => String
  -> Sem r [Window]
getWindowByClass wName = do
  childrenList <- getTree
  filterM findWindow childrenList
  where findWindow win = (== wName) <$> getClassName win

-- Moves windows around
render
  :: ( Members (Readers [(Dimension, Dimension), Borders]) r
     , Members [WindowMover, WindowMinimizer, Colorer] r
     )
  => Fix Tiler
  -> Sem r ()
render t = do
  (w, h) <- ask
  cata placeWindows t $ Plane (Rect 0 0 w h) 0

-- |Focus the X window
xFocus
  :: Members [State (Fix Tiler), WindowMinimizer, AttributeWriter] r
  => Sem r ()
xFocus = do
  root <- get @(Fix Tiler)
  let (Fix (Wrap w)) = unsafeLast (ana makeList root :: [Fix Tiler])
  restore w
  setFocus w
 where
  makeList :: Fix Tiler -> ListF (Fix Tiler) (Fix Tiler)
  makeList (Fix (Wrap _)) = Nil
  makeList (Fix t       ) = Cons (getFocused t) (getFocused t)
  getFocused = fromMaybe (error "no focus") . fst . popWindow (Right Focused)
