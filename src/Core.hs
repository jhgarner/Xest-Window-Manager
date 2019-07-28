{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}


module Core where

import           Standard
import           Base
import           Polysemy
import           Polysemy.State
import           Polysemy.Reader
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Types
import           Types
import           Data.Either                    ( )
import           Tiler
import           FocusList
import           Data.Bits

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
-- TODO Make individual handler functions
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
  -- restore ev_window
  -- This adds the new window to whatever tiler comes after inputController
  -- If you've zoomed the inputController in, you get nesting as a result
  modify $ cata $ applyInput (add Front Focused tWin)
  -- Make the newly created window into the focused one
  -- We have to keep Xorg's idea of focus in sync with our own
  -- setFocus ev_window
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

-- Tell the window it can configure itself however it wants.
-- We send back the Configure Request unmodified
handler (XorgEvent cre@ConfigureRequestEvent{}) = do
  configureWin cre
  return []

-- Called when a watched key is pressed or released
handler (XorgEvent KeyEvent {..}) = do
  -- Watched keys are stored in bindings
  Conf bindings _ _ <- ask @Conf
  -- Is ev_keycode (the key that was pressed) equal to k (the bound k)
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

-- Called when a mouse button is pressed
handler (XorgEvent ButtonEvent {..}) = do
  -- Just focus the clicked window. The same as above
  rootT <- get @(Fix Tiler)
  setFocus ev_window
  let (newRoot, status) = cata (focusWindow ev_window) rootT
  when (status == (False, False)) $ put newRoot
  return []

--While in a resize mode, the pointer moved.
handler (XorgEvent MotionEvent {..}) = do
  (width, height) <- ask
  -- Get the locations of every window
  sized::Cofree Tiler Plane <- topDown placeWindow (Plane (Rect 0 0 width height) 0) <$> get
  -- Find the one that comes right after the input controller
  let (Plane {rect = size} :< _) = extract $ (ana getInput sized :: DualList _)
  mb <- get @MouseButtons
  root <- ask @Window
  -- Change Move the tiler based on the mouse movement
  modify $ case mb of
    LeftButton (ox, oy) -> cata (applyInput (changeSize (fromIntegral ev_x - ox, fromIntegral ev_y - oy) size (LeftButton (0, 0))))
    RightButton (ox, oy) -> cata (applyInput (changeSize (fromIntegral ev_x - ox, fromIntegral ev_y - oy) size (RightButton (0, 0))))
    _ -> id

  -- In theory, this is part of the event, but X11 doesn't provide it so let's ask the server directly
  newB <- getButton root
  put newB
  -- We know what button is pressed, now we need to update the location
  updateMouseLoc (fromIntegral ev_x, fromIntegral ev_y)
  return []
 where getInput :: Cofree Tiler Plane -> DualListF (Cofree Tiler Plane) (Cofree Tiler Plane)
       getInput (_ :< InputController a) = EndF a
       -- TODO I wish I didn't need the (error "Will never happen") pattern as much.
       getInput (_ :< b) = ContinueF $ fromMaybe (error "Wrong") $ getFocused b

-- Handle all other xorg events as noops
handler (XorgEvent _) = return []

-- Run a shell command
handler (RunCommand s) = execute s >> return []

-- Swap out the current postprocessor with a different one
handler (ChangePostprocessor m) = put m >> return []

-- TODO is this right?
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
  -- Since a Wrap doesn't hold anything, we'll lose the InputController
  -- if we zoom in.
  reorder t@(InputController (Fix (Wrap _))) = Fix t
  -- Same fo EmptyTiler
  reorder t@(InputController (Fix EmptyTiler)) = Fix t
  -- Now we can safely zoom in
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
  -- The input disappears and will hopefully be added back later
  reorder (InputController (_, t)) = t
  -- If the tiler held the controller, add back the controller around it
  reorder t                        = Fix $ if any (isController . fst) t
    then InputController . Fix $ snd <$> t
    else snd <$> t

  isController (Fix (InputController _)) = True
  isController _                         = False

-- Change the current mode to something else
handler (ChangeModeTo newM) = do
  eActions <- gets @Mode exitActions
  rebindKeys newM
  captureButton newM
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
  -- xFocus
  return []
 where
  changer t@(Directional d fl) = case readMay s of
    Just i  -> Directional d $ focusIndex (i - 1) fl
    Nothing -> t
  changer t = t

-- Change focus in a given direction
handler (Move newD) = do
  modify $ cata (applyInput (changer newD))
  -- xFocus
  return []
 where
  changer dir (Directional d fl) = Directional d $ focusDir dir fl
  changer _   t                  = t

-- Move a tiler from the tree into a stack
handler PopTiler = do
  root <- get
  -- Todo Is this good or bad?
  onInput (modify . (:) . Fix) root
  modify $ cata (applyInput $ const EmptyTiler)
  return [ZoomOutInput]

-- Move a tiler from the stack into the tree
handler PushTiler = do
  popped <- get @[Fix Tiler]
  case popped of
    (t:ts) -> do
      put ts
      modify $ cata (applyInput $ add Front Focused t)
    [] -> return ()
  return []

-- handler ChangeSize = do
--   (width, height) <- ask
--   sized::Cofree Tiler Plane <- topDown placeWindow (Plane (Rect 0 0 width height) 0) <$> get
--   let (size :< _) = extract $ (ana getInput sized :: DualList _)
--   changeSize
--   return []
--  where getInput :: Cofree Tiler Plane -> DualListF (Cofree Tiler Plane) (Cofree Tiler Plane)
--        getInput (_ :< InputController a) = EndF a
--        getInput (_ :< b) = ContinueF $ fromMaybe (error "Wrong") $ getFocused b


-- Random stuff --


-- | Move all of the Tilers from root to newT
-- TODO remove recursion
changeLayout :: Tiler (Fix Tiler) -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
changeLayout newT root = doPopping root newT
 where
  doPopping ot t = case popWindow (Left Front) ot of
    (Nothing, _) -> t
    (Just win, wins) ->
      doPopping wins $ add Back (isFocused win) win t
  isFocused t = if t == focused then Focused else Unfocused
  focused = fromMaybe (Fix EmptyTiler) . fst $ popWindow (Right Focused) root

-- Would be used for reparenting (title bar)
manage :: Member AttributeWriter r => Window -> Sem r (Fix Tiler)
manage w = do
  selectFlags w (enterWindowMask .|. buttonPressMask .|. buttonReleaseMask)
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
type RenderEffect r =
     ( Members (Readers [(Dimension, Dimension), Borders]) r
     , Members [WindowMover, WindowMinimizer, Colorer] r
     )
render
  :: (RenderEffect r, Member (State [Fix Tiler]) r)
  => Fix Tiler
  -> Sem r ()
render t = do
  (width, height) <- ask
  let locations = topDown placeWindow (Plane (Rect 0 0 width height) 0) t
  -- Draw the tiler we've been given
  cataA draw locations
  -- Hide all of the popped tilers
  get @[Fix Tiler]
    >>= traverse_ (cataA draw . topDown placeWindow (Plane (Rect 0 0 0 0) 0))
 where draw :: RenderEffect r => Base (Cofree Tiler Plane) (Sem r ()) -> Sem r ()
       draw (Plane (Rect _ _ 0 0) _ :<~ Wrap win) = minimize win
       draw (Plane r _ :<~ Wrap win) = do
           restore win
           changeLocation win r
       draw (Plane Rect{..} depth :<~ InputController t) = do
          t
          -- Extract the border windows
          (l, u, r, d) <- ask @(Window, Window, Window, Window)
          let winList = [l, u, r, d]

          -- Calculate the color for our depth
          let hue = 360.0 * ((0.5 + (fromIntegral depth * 0.618033988749895)) `mod'` 1)
          color <- getColor $ "TekHVC:"++show hue++"/50/95"

          -- Convince our windows to be redrawn with the right color and position
          traverse_ (`changeLocation` Rect 0 0 1 1) winList
          traverse_ (`changeColor` color) winList

          -- Draw them with the right color and position
          changeLocation l $ Rect x y 5 h
          changeLocation u $ Rect x y w 5
          changeLocation d $ Rect x (y+fromIntegral h-5) w 5
          changeLocation r $ Rect (x+fromIntegral w-5) y 5 h
       draw (_ :<~ t) = sequence_ t
          
          

-- |Focus the X window
xFocus
  :: Members [State (Fix Tiler), WindowMinimizer, AttributeWriter] r
  => Sem r ()
xFocus = do
  root <- get @(Fix Tiler)
  case lastMay (ana makeList (Just root) :: [_]) of
    Just (Wrap w) -> do
      restore w
      setFocus w
    _ -> return ()
 where
  -- makeList :: Maybe (Fix Tiler) -> ListF (Tiler (Fix Tiler)) (Maybe (Fix Tiler))
  makeList Nothing               = Nil
  makeList (Just (Fix t       )) = Cons t (getFocused t)

updateMouseLoc :: Member (State MouseButtons) r => (Int, Int) -> Sem r ()
updateMouseLoc pos = modify @MouseButtons \case
    LeftButton _ -> LeftButton pos
    RightButton _ -> RightButton pos
    None -> None

changeSize :: (Int, Int) -> Rect -> MouseButtons -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
changeSize (dx, dy) Rect{..} m = \case
  Directional d fl ->
    let numWins = fromIntegral $ length fl
        windowSize = 1 / numWins
        mouseDelta = fromIntegral if d == X then dx else dy
        delta = if d == X then mouseDelta / fromIntegral w else mouseDelta / fromIntegral h
        foc = fromIntegral $ maybe (error "How?") (case m of
                                                     RightButton _ -> (+1)
                                                     -- LeftButton == RightButton on the previous window
                                                     LeftButton  _ -> (id)
                                                  ) $ getFocIndex fl
        
        bound a i prev = max (-windowSize) $ min a $ 1 - (i * windowSize + prev)
        propagate = 
          mapFold
          (\(i, prev) (Sized size t) -> if i == foc && foc < numWins
            then ((i+1, bound (size + delta) i prev), Sized (bound (size + delta) i prev) t)
            else if i == foc + 1 && foc > 0
            then ((i+1, bound (size - delta) i prev), Sized (bound (size - delta) i prev) t)
            else ((i+1, bound size i prev), Sized (bound size i prev) t))
          (1, 0)
          fl
    in Directional d propagate
  Floating b (Just ((RRect{..}, t)):ls) -> 
    let (ddx, ddy) = (fromIntegral dx / fromIntegral w, fromIntegral dy / fromIntegral h) in Floating b $ (case m of
                          RightButton _ -> Just (RRect xp yp (wp + ddx) (trace (show dy ++ " " ++ show y) (hp + ddy)), t)
                          LeftButton _ -> Just (RRect (xp + ddx) (yp + ddy) wp hp, t)) : ls
  t -> t
