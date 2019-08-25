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
newModePostprocessor _ (bKey, bMode, _) (KeyboardEvent (key, mode, _) False)
  | bKey == key && bMode == mode = [ChangePostprocessor Default]
  | otherwise    = []

-- Otherwise do nothing
newModePostprocessor _ _ _ = []


-- | Postprocessor used when another key is clicked while holding one down
tempModePostprocessor :: KeyPostprocessor r
-- On release, return to the old mode
tempModePostprocessor oldMode (bKey, bMode, _) (KeyboardEvent (key, mode, _) False)
  | bKey == key && bMode == mode = [ChangePostprocessor Default, ChangeModeTo oldMode]
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
  -- Before we do anything else, let's see if we already manage this window.
  -- I don't know why, but things like Inkscape map windows multiple times.
  tree <- get @(Fix Tiler)
  unless (cata (findWindow ev_window) tree) $ do
    -- managing a window allows us to do any number of things to it
    -- Currently we wrap it in a new type and ask for crossing events
    tWin <- manage ev_window
    -- Resend the mapWindow Xorg event.
    -- We don't receive the mapWindow event from this because Xorg knows we sent it.
    -- restore ev_window
    -- This adds the new window to whatever tiler comes after inputController
    -- If you've zoomed the inputController in, you get nesting as a result
    modify . cata . applyInput $ pushOrAdd tWin
  return []
  where findWindow w (Wrap w') = w == w'
        findWindow _ t = or t

-- Called on window destruction
handler (XorgEvent DestroyWindowEvent {..}) = do
  -- Remove the destroyed window from our tree.
  -- Usually, unmap will be called first, but what if a minimized window
  -- gets killed? In that case, we won't get an unmap notifiction.
  modify @(Fix Tiler) $
    fromMaybe (error "No roooot!") . cata (fmap Fix . (>>= ripOut (Fix $ Wrap ev_window)) . reduce)
  return []

-- Called when a window should no longer be drawn
-- This either happens when the window dies, or when we minimize it
handler (XorgEvent UnmapEvent {..}) = do
  mins <- get @(Set Window)
  -- Remove the destroyed window from our tree if we aren't the
  -- reason it was unmapped.
  unless (member ev_window mins) $ modify @(Fix Tiler) $ 
    fromMaybe (error "No roooot!") . cata (fmap Fix . (>>= ripOut (Fix $ Wrap ev_window)) . reduce)
  return []

-- Tell the window it can configure itself however it wants.
-- We send back the Configure Request unmodified
handler (XorgEvent cre@ConfigureRequestEvent{}) = do
  configureWin cre
  return []

-- Called when a watched key is pressed or released
handler (XorgEvent KeyEvent {..}) = do
  -- Watched keys are stored in bindings
  Conf bindings _ <- ask @Conf
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
  -- If status is anything but Both, it means that something super
  -- weird is going on and the newRoot is probably bad.
  -- Usually this happens if the crossed window isn't in our tree and causes
  -- the InputController to disappear.
  when (status == Both) $ put @(Fix Tiler) $ fromMaybe (error "No root!") newRoot
  return []

-- Called when a mouse button is pressed
handler (XorgEvent ButtonEvent {..}) = do
  -- Just focus the clicked window. The same as above
  rootT <- get @(Fix Tiler)
  setFocus ev_window
  let (newRoot, status) = cata (focusWindow ev_window) rootT
  when (status == Both) $ put @(Fix Tiler) $ fromMaybe (error "No root!") newRoot
  return []

--While in a resize mode, the pointer moved.
handler (XorgEvent MotionEvent {..}) = do
  (width, height) <- ask
  mb <- get @MouseButtons
  root <- ask @Window

  -- Get the locations of every window
  sized::Cofree Tiler (Transformer Plane) <- topDown placeWindow (Transformer id id $ Plane (Rect 0 0 width height) 0) <$> get

  -- Find the one that comes right after the input controller
  case journey $ ana @(Path _ _) getInput sized of
    (rotations, Just (Transformer i o (Plane {rect = size}))) -> do
      let rotation :: Bool = foldr ($) False rotations

      -- Change Move the tiler based on the mouse movement
      modify $ case mb of
        LeftButton (ox, oy) -> cata (applyInput (fmap $ changeSize (adjustedMouse rotation ox oy) size (LeftButton (0, 0))))
        RightButton (ox, oy) -> cata (applyInput (fmap $ changeSize (adjustedMouse rotation ox oy) size (RightButton (0, 0))))
        _ -> id
    _ -> return ()

  -- In theory, this is part of the event, but X11 doesn't provide it so let's ask the server directly
  newB <- getButton root
  put newB
  -- We know what button is pressed, now we need to update the location
  updateMouseLoc (fromIntegral ev_x, fromIntegral ev_y)
  return []
 where getInput :: Cofree Tiler (Transformer Plane) -> PathF (Maybe (Transformer Plane)) (Bool -> Bool) (Cofree Tiler (Transformer Plane))
       getInput (_ :< InputController a) = FinishF $ fmap extract a
       getInput (_ :< Reflect a) = BreakF not a
       getInput (_ :< b) = RoadF $ getFocused b
       adjustedMouse False ox oy = (fromIntegral ev_x - ox, fromIntegral ev_y - oy)
       adjustedMouse True ox oy = (fromIntegral ev_y - oy, fromIntegral ev_x - ox)

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
  reorder t@(InputController (Just (Fix (Wrap _)))) = Fix t
  -- Same fo undefined
  reorder t@(InputController Nothing) = Fix t
  -- Now we can safely zoom in
  reorder (InputController (Just (Fix t))) =
    Fix $ modFocused (Fix . InputController . Just) t

  reorder t = Fix t

-- Move the input controller towards the root
handler ZoomOutInput = do
  root <- get
  -- Don't zoom the controller out of existence
  unless (isController root) $ put $ fromMaybe (error "e") $ para reorder root
  return []
 where
  -- The input disappears and will hopefully be added back later
  reorder :: Tiler (Fix Tiler, Maybe (Fix Tiler)) -> Maybe (Fix Tiler)
  reorder (InputController t) = t >>= snd
  -- If the tiler held the controller, add back the controller around it
  reorder t                        = fmap Fix $ if any (isController . fst) t
    then Just . InputController . fmap Fix . reduce $ fmap snd t
    else reduce $ fmap snd t

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
-- handler (ChangeLayoutTo (Fix insertable)) =
--   modify $ cata (applyInput (changeLayout newT))
--   return []
--     where changeLayout (Left Rotate) = Reflect

-- Change the focus to some named action
handler (ChangeNamed s) = do
  modify $ cata (applyInput $ fmap changer)
  -- xFocus
  return []
 where
  changer t@(Horiz fl) = case readMay s of
    Just i  -> Horiz $ focusVIndex (i - 1) fl
    Nothing -> t
  changer t = t

-- Change focus in a given direction
handler (Move newD) = do
  modify $ cata (applyInput . fmap $ changer newD)
  -- xFocus
  return []
 where
  changer dir (Horiz fl) = Horiz $ focusDir dir fl
  changer _   t                  = t

-- Move a tiler from the tree into a stack
handler PopTiler = do
  root <- get
  sequence_ $ onInput (fmap (modify @[Fix Tiler] . (:))) root
  modify $ cata (applyInput $ const Nothing)
  return [ZoomOutInput]
    -- where popIt :: Fix Tiler -> Sem '[State [Fix Tiler]] ()
    --       popIt = 

-- Move a tiler from the stack into the tree
handler PushTiler = do
  popped <- get @[Fix Tiler]
  case popped of
    (t:ts) -> do
      put ts
      modify $ cata (applyInput $ pushOrAdd t)
    [] -> return ()
  return []

handler (Insert t) = do
  modify @(Fix Tiler) . cata $ applyInput (fmap toTiler)
  return []
    where toTiler w =
            case t of
              Horizontal -> Horiz $ makeFL (NE (Sized 0 $ Fix w) []) 0
              Rotate -> Reflect $ Fix w
              FullScreen -> FocusFull $ Fix w
              Hovering -> Floating $ NE (Bottom $ Fix w) []

handler MakeSpecial = do
  modify @(Fix Tiler) . cata $ applyInput (fmap makeSpecial)
  return []
    where makeSpecial t =
            case t of
              Floating (NE l ls) -> Floating $ NE (mkBottom l) $ fmap mkTop ls
              _ -> t
          mkTop t@(Top _) = t
          mkTop (Bottom t) = Top (RRect 0 0 0.2 0.2, t)
          mkBottom = Bottom . extract 


-- Random stuff --

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
     , Member (State (Fix Tiler)) r
     , Members [WindowMover, WindowMinimizer, Colorer] r
     )
render
  :: (RenderEffect r, Member (State [Fix Tiler]) r)
  => Fix Tiler
  -> Sem r [Window]
render t = do
  (width, height) <- ask
  let locations = topDown placeWindow (Transformer id id $ Plane (Rect 0 0 width height) 0) t
  -- Draw the tiler we've been given
  let (winOrder, io) = cata draw $ fmap unTransform locations
  io
  -- Hide all of the popped tilers
  get @[Fix Tiler]
    >>= traverse_ (snd . cata draw . fmap unTransform . topDown placeWindow (Transformer id id $ Plane (Rect 0 0 0 0) 0))
  return winOrder
 where draw :: RenderEffect r => Base (Cofree Tiler Plane) ([Window], Sem r ()) -> ([Window], Sem r ())
       draw (Plane (Rect _ _ 0 0) _ :<~ Wrap win) = ([], minimize win)
       draw (Plane Rect {..} _ :<~ Wrap win) = ([win], do
           restore win
           changeLocation win $ Rect x y (abs w) (abs h))
       draw (Plane Rect{..} depth :<~ InputController t) = (maybe [] fst t, do
          mapM_ snd t
          -- Extract the border windows
          (l, u, r, d) <- ask @Borders
          let winList = [l, u, r, d]

          -- Calculate the color for our depth
          let hue = 360.0 * ((0.5 + (fromIntegral (depth - 1) * 0.618033988749895)) `mod'` 1)
          -- color <- getColor $ "TekHVC:"++show hue++"/50/95"


          -- Draw them with the right color and position
          changeLocationS l $ Rect x y 2 h
          changeLocationS u $ Rect x y w 10
          changeLocationS d $ Rect x (y+fromIntegral h-2) w 2
          changeLocationS r $ Rect (x+fromIntegral w-2) y 2 h

          -- Convince our windows to be redrawn with the right color and position
          -- traverse_ (`changeLocationS` Rect 0 0 1 1) winList
          traverse_ (`changeColor` hsvToRgb hue 0.5 0.9) winList
          get >>= drawText u . cata getFocusList
          traverse_ bufferSwap winList)

       draw (_ :<~ Floating ls) = 
         (bottoms ++ tops, mapM_ (snd . getEither) ls)
             where tops = foldl' onlyTops [] ls
                   onlyTops acc (Top (_, (ws, _))) = ws ++ acc
                   onlyTops acc _ = acc
                   bottoms = foldl' onlyBottoms [] ls
                   onlyBottoms acc (Bottom (ws, _)) = acc ++ ws
                   onlyBottoms acc _ = acc
       draw (_ :<~ t) = (concatMap fst t, mapM_ snd t)
       hsvToRgb :: Double -> Double -> Double -> (Int, Int, Int)
       hsvToRgb h s v = let c = v * s
                            x = c * (1 - abs ((h / 60) `mod'` 2 - 1))
                            m = v - c
                            (r, g, b) = if
                               | h < 60 -> (c, x, 0)
                               | h < 120 -> (x, c, 0)
                               | h < 180 -> (0, c, x)
                               | h < 240 -> (0, x, c)
                               | h < 300 -> (x, 0, c)
                               | otherwise -> (c, 0, x)
                        in (round $ (r+m)*255, round $ (g+m)*255, round $ (b+m)*255)

          
          
writePath :: Members '[State (Fix Tiler), Reader Borders, Colorer, PropertyReader] r 
          => Sem r ()
writePath = do
  (_, u, _, _) <- ask @Borders
  root <- get @(Fix Tiler)
  drawText u $ cata getFocusList root

-- |Focus the X window
xFocus
  :: Members [State (Fix Tiler), WindowMinimizer, AttributeWriter] r
  => Sem r ()
xFocus = do
  root <- get @(Fix Tiler)
  let w = extract $ ana @(Beam _) makeList root
  traverse_ restore w
  traverse_ setFocus w
 where
  makeList (Fix (Wrap w))              = EndF $ Just w
  makeList (Fix (InputController (Just t))) = ContinueF t
  makeList (Fix (InputController Nothing)) = EndF Nothing
  makeList (Fix t) = ContinueF (getFocused t)

updateMouseLoc :: Member (State MouseButtons) r => (Int, Int) -> Sem r ()
updateMouseLoc pos = modify @MouseButtons $ \case
    LeftButton _ -> LeftButton pos
    RightButton _ -> RightButton pos
    None -> None

changeSize :: (Int, Int) -> Rect -> MouseButtons -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
changeSize (dx, dy) Rect{..} m = \case
  Horiz fl ->
    -- TODO Rewrite in a less scary way
    let numWins = fromIntegral $ length fl
        windowSize = 1 / numWins
        mouseDelta = fromIntegral dx
        sign = if dx < 0 then (-) 0 else id
        delta = mouseDelta / fromIntegral w
        twoPx = 2 / fromIntegral w
        foc = fromIntegral $ (case m of
                                RightButton _ -> (+1)
                                -- LeftButton == RightButton on the previous window
                                LeftButton  _ -> (id)
                             ) $ findNeFocIndex fl
        
        bound = max $ twoPx - windowSize
        (_, trueDelta)  = foldl' 
          (\(i, minS) (Sized size _) -> if i == foc && foc < numWins
            then (i+1, min minS $ abs $ size - bound (size + delta))
            else if i == foc + 1 && foc > 0
            then (i+1, min minS $ abs $ size - bound (size - delta))
            else (i+1, minS)) (1, 2) $ vOrder fl
        propagate = 
          fromVis fl . mapFold
          (\i (Sized size t) -> if i == foc && foc < numWins
            then (i+1, Sized (bound (size + sign trueDelta)) t)
            else if i == foc + 1 && foc > 0
            then (i+1, Sized (bound (size - sign trueDelta)) t)
            else (i+1, Sized (bound size) t))
          1
          $ vOrder fl
    in Horiz propagate
  Floating (NE (Top (RRect{..}, t)) ls) -> 
    let (ddx, ddy) = (fromIntegral dx / fromIntegral w, fromIntegral dy / fromIntegral h) 
        twoPx = 2 / fromIntegral w
        boundedX = max (twoPx - wp) $ min (xp + ddx) (1 - twoPx)
        boundedY = max (twoPx - hp) $ min (yp + ddy) (1 - twoPx)
    in Floating $ NE (case m of
      RightButton _ -> Top (RRect xp yp (wp + ddx) (hp + ddy), t)
      LeftButton _ -> Top (RRect boundedX boundedY wp hp, t)) ls
  t -> t

pushOrAdd :: Fix Tiler -> Maybe (Tiler (Fix Tiler)) -> Maybe (Tiler (Fix Tiler))
pushOrAdd tWin = Just . maybe (Horiz $ makeFL (NE (Sized 0 tWin) []) 0) (\case
  Wrap w -> Horiz $ makeFL (NE (Sized 0 tWin) [Sized 0 . Fix $ Wrap w]) 0
  Reflect w -> Horiz $ makeFL (NE (Sized 0 tWin) [Sized 0 . Fix $ Reflect w]) 0
  FocusFull w -> Horiz $ makeFL (NE (Sized 0 tWin) [Sized 0 . Fix $ FocusFull w]) 0
  t -> add Front Focused tWin t)
