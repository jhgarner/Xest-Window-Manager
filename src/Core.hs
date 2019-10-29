{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}


module Core where

import           Standard
import           Base
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Types
import           Types
import           Data.Either                    ( )
import           Tiler
import           FocusList
import           Data.Bits
import qualified Data.Map.Strict as M

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
-- Why make individual functions? First, it would allow better use of the effect system.
-- Instead of requiring DoAll, I could give this code constrained effects.
--
-- Performs some action and returns a list of new actions to be performed
-- Returning a [] means that there aren't any new tasks to do.
-- TODO Do I actually want to return a list of actions? Only a couple of these handlers
-- actually make use of that and it probably simplifies the code to change it up.
handler :: Action -> DoAll r
-- Called on window creation
handler (XorgEvent MapRequestEvent {..}) = do
  -- Before we do anything else, let's see if we already manage this window.
  -- I don't know why, but things like Inkscape map windows multiple times.
  tree <- get @(Fix Tiler)
  -- TODO You're about to see these 3 lines of code a whole lot. Maybe they should
  -- be a function or something.
  pointer <- input @Pointer
  screens <- input @Screens
  let i = maybe 0 fst $ whichScreen pointer $ zip [0..] screens

  unless (cata (findWindow ev_window) tree) $ do
    -- managing a window allows us to modify it in whatever ways we want.
    -- In our case, we will place it in a parent window and ask for some
    -- events.
    tWin <- manage ev_window

    -- This adds the new window to whatever tiler comes after inputController
    -- If you've zoomed the inputController in, you get nesting as a result
    modify . cata . applyInput i $ Just . pushOrAdd tWin
  return []
  where findWindow w (Wrap w') = inChildParent w w'
        findWindow _ t = or t

-- Called on window destruction
handler (XorgEvent DestroyWindowEvent {..}) = do
  -- Remove the destroyed window from our tree.
  -- Usually, unmap will be called first, but what if a minimized window
  -- gets killed? In that case, we won't get an unmap notifiction.
  oldRoot <- get
  traverse_ (kill True) $ findParent ev_window oldRoot
  -- TODO is there any way we can prove that our Tiler will never be empty
  -- after ripping something out? These errors are kind of annoying.
  modify @(Fix Tiler) $ fromMaybe (error "No roooot!") . 
    cata (fmap Fix . (>>= ripOut (Fix $ Wrap $ ChildParent ev_window ev_window)) . reduce)
  modify $ M.delete ev_window
  return []

-- Called when a window should no longer be drawn
-- This either happens when the window dies, or when we minimize it
handler (XorgEvent UnmapEvent {..}) = do
  -- The Set of windows we think are minimized. Thanks to Xmonad for this
  -- idea. A minimized window should remain in the tree when unmapped.
  -- Windows that weren't minimized but were unmapped are probably dying.
  mins <- get @(Set Window)
  -- Remove the destroyed window from our tree if we aren't the
  -- reason it was unmapped.
  unless (member ev_window mins) $ do
    oldRoot <- get
    moveToRoot ev_window
    traverse_ (kill True) $ findParent ev_window oldRoot
    modify @(Fix Tiler) $ 
      fromMaybe (error "No roooot!") . 
        cata (fmap Fix . (>>= ripOut (Fix $ Wrap $ ChildParent ev_window ev_window)) . reduce)
    modify $ M.delete ev_window
  return []

-- Tell the window it can configure itself however it wants.
-- We send back the Configure Request unmodified
handler (XorgEvent cre@ConfigureRequestEvent{}) = do
  configureWin cre
  return []

-- The root window has changed itself. Usually that tells us there's been a change to
-- the number of monitors connected.
handler (XorgEvent ConfigureEvent{}) = do
  screens <- input @Screens
  let len = length screens

  -- If the user unplugged the monitor, remove it from the tree
  -- Every monitor has a Monitor and a IC associated with it. They both have to go.
  modify @(Fix Tiler) $ fromMaybe (error "Uh oh. Where'd the root go?") . 
    (cata (removeIC len) >=> cata (removeMonitor len))
  -- Unless we've already added the monitor, add it now.
  unlessM (cata (findMonitor (len - 1)) <$> get @(Fix Tiler)) $
    -- TODO I don't think I like having to write Just . Fix . ... All the time. Maybe there's
    -- a better way to do it?
     modify @(Tiler (Fix Tiler)) $ Monitor (len - 1) . Just . Fix . InputController (len - 1) . Just . Fix
  return []
    where removeMonitor i (Monitor i' t)
            | i == i' = join t
            | otherwise = Just . Fix . Monitor i' $ join t
          removeMonitor _ t = Fix <$> reduce t
          removeIC i (InputController i' t)
            | i == i' = join t
            | otherwise = Just . Fix . InputController i' $ join t
          removeIC _ t = Fix <$> reduce t

          findMonitor i (Monitor i' rest) = i == i' || fromMaybe False rest
          findMonitor _ t = or t

-- Called when a watched key is pressed or released
handler (XorgEvent KeyEvent {..}) = do
  -- Watched keys are stored in bindings
  Conf bindings _ <- input @Conf
  mode <- get @Mode
  -- Is ev_keycode (the key that was pressed) equal to k (the bound k)
  return $ case find (\(k, m, _) -> ev_keycode == k && m == mode) bindings of
    Nothing -> []
    Just kt -> [KeyboardEvent kt (ev_event_type == keyPress)]

-- Called when the cursor moves between windows
-- TODO this is the exact same code as the button event
handler (XorgEvent CrossingEvent {..}) = do
  rootT <- get @(Fix Tiler)
  screens <- input @Screens
  -- Change our tree so the focused window is the one we're hovering over
  let mNewRoot = 
        focusWindow (maybe 0 fst $ whichScreen (fromIntegral ev_x_root, fromIntegral ev_y_root) $ zip [0..] screens) ev_window rootT
  -- The root might be none if the newly focused window doesn't exist
  case mNewRoot of
    Just newRoot -> do
      realWin <- getChild ev_window
      traverse_ setFocus realWin
      put @(Fix Tiler) newRoot
    Nothing -> setFocus ev_window
  return []

-- Called when a mouse button is pressed
handler (XorgEvent ButtonEvent {..}) = do
  rootT <- get @(Fix Tiler)
  screens <- input @Screens
  -- Make certain that the focused window is the one we're hovering over
  let mNewRoot = 
        focusWindow (maybe 0 fst $ whichScreen (fromIntegral ev_x_root, fromIntegral ev_y_root) $ zip [0..] screens) ev_window rootT
  case mNewRoot of
    Just newRoot -> do
      realWin <- getChild ev_window
      traverse_ setFocus realWin
      put @(Fix Tiler) newRoot
    Nothing -> setFocus ev_window
  return []

--While in a resize mode, the pointer moved.
-- TODO this code is a little scary
handler (XorgEvent MotionEvent {..}) = do
  -- First, let's figure out what the width and height are for the active screen
  screens <- input @Screens
  pointer <- input @Pointer
  let Identity (Rect _ _ width height) = fromMaybe (Identity $ Rect 0 0 10000 10000) $ whichScreen pointer $ fmap Identity screens
  let i' = maybe 0 fst $ whichScreen pointer $ zip [0..] screens

  -- Now lets get some other stuff we'll need later
  mb <- get @MouseButtons
  mode <- gets @Mode hasBorders

  -- Get the locations of every window
  sized::Cofree Tiler (Transformer Plane) <- topDown (placeWindow mode i' screens) (Transformer id id $ Plane (Rect 0 0 width height) 0) <$> get

  -- Find the one that comes right after the input controller
  case journey $ ana @(Path _ _) (getInput i') sized of
    (rotations, Just (Transformer _ _ Plane {rect = size})) -> do
      let rotation :: Bool = foldr ($) False rotations

      -- Move the tiler based on the mouse movement
      modify $ case mb of
        LeftButton (ox, oy) -> cata (applyInput i' (fmap $ changeSize (adjustedMouse rotation ox oy) size (LeftButton (0, 0))))
        RightButton (ox, oy) -> cata (applyInput i' (fmap $ changeSize (adjustedMouse rotation ox oy) size (RightButton (0, 0))))
        _ -> id
    _ -> return ()

  -- In theory, this is part of the event, but X11 doesn't provide it so let's ask the server directly
  newB <- input @MouseButtons
  put @MouseButtons newB

  -- We know what button is pressed, now we need to update the location
  updateMouseLoc (fromIntegral ev_x, fromIntegral ev_y)
  return []

       -- Gets the thing right after the input controller but also counts the number of rotations
 where getInput :: Int -> Cofree Tiler (Transformer Plane) -> PathF (Maybe (Transformer Plane)) (Bool -> Bool) (Cofree Tiler (Transformer Plane))
       getInput i (_ :< InputController i' a)
         | i == i' = FinishF $ fmap extract a
         | otherwise = maybe (FinishF Nothing) RoadF a
       getInput _ (_ :< Monitor _ (Just a)) = RoadF a
       getInput _ (_ :< Monitor _ Nothing) = FinishF Nothing
       getInput _ (_ :< Reflect a) = BreakF not a
       getInput _ (_ :< b) = RoadF $ getFocused b
       adjustedMouse False ox oy = (fromIntegral ev_x - ox, fromIntegral ev_y - oy)
       adjustedMouse True ox oy = (fromIntegral ev_y - oy, fromIntegral ev_x - ox)

-- Handle all other xorg events as noops
handler (XorgEvent _) = return []

-- Run a shell command
handler (RunCommand s) = execute s >> return []

-- Swap out the current postprocessor with a different one
handler (ChangePostprocessor m) = put m >> return []

-- A keypress needs to be processed
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
  pointer <- input @Pointer
  screens <- input @Screens
  let i = maybe 0 fst $ whichScreen pointer $ zip [0..] screens
  put $ cata (reorder i) root
  return []
 where
  -- Since a Wrap doesn't hold anything, we'll lose the InputController
  -- if we zoom in.
  reorder _ t@(InputController _ (Just (Fix (Wrap _)))) = Fix t
  -- Same for Nothing
  reorder _ t@(InputController _ Nothing) = Fix t
  -- Now we can safely zoom in
  reorder i ic@(InputController i' (Just (Fix t)))
    | i == i' = Fix $ modFocused (Fix . InputController i' . Just) t
    | otherwise = Fix ic

  reorder _ t = Fix t

-- The monitor needs to stay behind the Input Controller
-- which makes zooming in harder
handler ZoomInMonitor = do
  pointer <- input @Pointer
  screens <- input @Screens
  let i = maybe 0 fst $ whichScreen pointer $ zip [0..] screens

  modify @(Fix Tiler) $ cata (reorder i)
  return []
 where
  reorder i m@(Monitor i' (Just (Fix (InputController i'' t))))
    | i'' == i = Fix m
    | i == i' = Fix . InputController i'' . Just . Fix $ Monitor i' t
  reorder _ t@(Monitor _ Nothing) = Fix t
  reorder i m@(Monitor i' (Just (Fix t)))
    | i == i' = Fix $ modFocused (Fix . Monitor i' . Just) t
    | otherwise = Fix m

  reorder _ t = Fix t

-- Move the input controller towards the root
-- This is complicated if the monitor is right behind it
-- TODO How can reorder be made better?
handler ZoomOutInput = do
  pointer <- input @Pointer
  screens <- input @Screens
  root <- get @(Fix Tiler)
  let i = maybe 0 fst $ whichScreen pointer $ zip [0..] screens

  unless (isJust $ isController i root) $ modify (fromMaybe (error "e") . para (reorder i))
  return []
 where
  reorder :: Int -> Tiler (Fix Tiler, Maybe (Fix Tiler)) -> Maybe (Fix Tiler)
  -- The input disappears and will hopefully be added back later
  reorder i (InputController i' t)
    | i == i' = t >>= snd
    | otherwise =
      case t >>= isController i . fst of
        Just controller -> Just . Fix . controller . Just . Fix . InputController i' $ t >>= snd
        Nothing -> Just . Fix . InputController i' $ t >>= snd

  -- The controller should always be in front of the Monitor
  reorder i m@(Monitor i' (Just (Fix (InputController i'' _), t)))
    | i'' == i' && i' == i = Just . Fix $ InputController i'' t
    | i == i'' = Just . Fix . InputController i'' . Just . Fix $ Monitor i' t
    | otherwise = fmap Fix . reduce $ fmap snd m

  -- If the tiler held the controller, add back the controller around it
  reorder i t = fmap Fix $ 
    case asum $ isController i . fst <$> t of
      Just controller -> Just . controller . fmap Fix . reduce $ fmap snd t
      Nothing -> reduce $ fmap snd t

  isController i (Fix (InputController i' _)) = if i == i' then Just $ InputController i' else Nothing
  isController i (Fix (Monitor i' (Just (Fix (InputController i'' _)))))
    | i == i' && i' == i'' = Just $ Monitor i'
    | otherwise = Nothing
  isController _ _                         = Nothing

handler ZoomOutMonitor = do
  root <- get @(Fix Tiler)
  pointer <- input @Pointer
  screens <- input @Screens
  let i = maybe 0 fst $ whichScreen pointer $ zip [0..] screens
  unless (isMonitor i root) $ put $ fromMaybe (error "e") $ para (reorder i) root
  return []
 where
  reorder :: Int -> Tiler (Fix Tiler, Maybe (Fix Tiler)) -> Maybe (Fix Tiler)
  reorder i (Monitor i' t) = 
    if i == i'
       then t >>= snd
       else Just . Fix . Monitor i' $ t >>= snd
  reorder i t =
    if any (isMonitor i . fst) t
       then Just . Fix . Monitor i . fmap Fix . reduce $ fmap snd t
       else fmap Fix . reduce $ fmap snd t

  isMonitor i (Fix (Monitor i' _)) = i == i'
  isMonitor _ _ = False

-- Change the current mode to something else
handler (ChangeModeTo newM) = do
  eActions <- gets @Mode exitActions
  rebindKeys newM
  selectButtons newM
  put newM
  --Combine the two lists of actions to be executed. Execute exit actions first.
  return $ eActions ++ introActions newM

-- Change the focus to some named action
handler (ChangeNamed s) = do
  pointer <- input @Pointer
  screens <- input @Screens
  let i = maybe 0 fst $ whichScreen pointer $ zip [0..] screens
  modify $ cata (applyInput i $ fmap changer)
  return []
 where
  changer t@(Horiz fl) = case readMay s of
    Just i  -> Horiz $ 
      if flLength fl >= i
         then focusVIndex (i - 1) fl
         else fl
    Nothing -> t
  changer t = t

-- Change focus in a given direction
handler (Move newD) = do
  pointer <- input @Pointer
  screens <- input @Screens
  let i = maybe 0 fst $ whichScreen pointer $ zip [0..] screens
  modify $ cata (applyInput i . fmap $ changer newD)
  return []
 where
  changer dir (Horiz fl) = Horiz $ focusDir dir fl
  changer _   t                  = t

-- Move a tiler from the tree into a stack
handler PopTiler = do
  root <- get
  pointer <- input @Pointer
  screens <- input @Screens
  let i = maybe 0 fst $ whichScreen pointer $ zip [0..] screens
  sequence_ $ onInput i (fmap (modify @[Fix Tiler] . (:))) root
  modify $ cata (applyInput i $ const Nothing)
  return []

-- Move a tiler from the stack into the tree
handler PushTiler = do
  popped <- get @[Fix Tiler]
  pointer <- input @Pointer
  screens <- input @Screens
  let i = maybe 0 fst $ whichScreen pointer $ zip [0..] screens
  case popped of
    (t:ts) -> do
      put ts
      modify $ cata (applyInput i $ Just . pushOrAdd t)
    [] -> return ()
  return []

-- Insert a tiler after the Input Controller
handler (Insert t) = do
  pointer <- input @Pointer
  screens <- input @Screens
  let i = maybe 0 fst $ whichScreen pointer $ zip [0..] screens
  modify @(Fix Tiler) . cata $ applyInput i (fmap toTiler)
  return []
    where toTiler w =
            case t of
              Horizontal -> Horiz $ makeFL (NE (Sized 0 $ Fix w) []) 0
              Rotate -> Reflect $ Fix w
              FullScreen -> FocusFull $ Fix w
              Hovering -> Floating $ NE (Bottom $ Fix w) []

-- Perform some special action based on the focused tiler
handler MakeSpecial = do
  pointer <- input @Pointer
  screens <- input @Screens
  let i = maybe 0 fst $ whichScreen pointer $ zip [0..] screens
  modify @(Fix Tiler) . cata $ applyInput i (fmap makeSpecial)
  return []
    where makeSpecial t =
            case t of
              Floating (NE l ls) -> Floating $ NE (mkBottom l) $ fmap mkTop ls
              _ -> t
          mkTop t@(Top _) = t
          mkTop (Bottom t) = Top (RRect 0 0 0.2 0.2, t)
          mkBottom = Bottom . extract 

-- Kill the active window
handler KillActive = do
  root <- get @(Fix Tiler)
  let w = extract $ ana @(Beam _) makeList root
  l <- case w of
         Just (ww, ww') -> do
           _ <- kill True ww'
           shouldKill <- kill True ww
           return $ fmap (,ww') shouldKill
         Nothing -> return Nothing
  case l of
    Nothing -> return ()
    Just (killed, w') ->
      modify @(Fix Tiler) $
        fromMaybe (error "No roooot!") . cata (fmap Fix . (>>= ripOut (Fix $ Wrap $ ChildParent killed w')) . reduce)
  return []
    where 
      makeList (Fix (Wrap (ChildParent w w')))              = EndF $ Just (w, w')
      makeList (Fix (InputController _ (Just t))) = ContinueF t
      makeList (Fix (InputController _ Nothing)) = EndF Nothing
      makeList (Fix (Monitor _ (Just t))) = ContinueF t
      makeList (Fix (Monitor _ Nothing)) = EndF Nothing
      makeList (Fix t) = ContinueF (getFocused t)

handler ExitNow = absurd <$> exit
handler ToggleLogging = toggleLogs >> return []

-- Random stuff --

-- Used for reparenting
manage :: Members [EventFlags, GlobalX] r => Window -> Sem r (Fix Tiler)
manage w = do
  newWin <- newWindow w
  selectFlags newWin (substructureNotifyMask .|. substructureRedirectMask .|. structureNotifyMask .|. enterWindowMask )-- .|. buttonPressMask .|. buttonReleaseMask)
  return . Fix $ Wrap $ ChildParent newWin w

-- Find a window with a class name
getWindowByClass
  :: Members [Property, GlobalX] r
  => String
  -> Sem r [Window]
getWindowByClass wName = do
  childrenList <- getTree
  filterM findWindow childrenList
  where findWindow win = (== wName) <$> getClassName win

-- Moves windows around
type RenderEffect r =
     ( Members (Inputs [Borders, Screens, Pointer]) r
     , Members (States [Fix Tiler, Mode]) r
     , Members [Mover, Minimizer, Colorer, GlobalX] r
     )
render
  :: (RenderEffect r, Member (State [Fix Tiler]) r)
  => Fix Tiler
  -> Sem r [Window]
render t = do
  screens <- input @Screens
  mode <- gets @Mode hasBorders
  pointer <- input @Pointer
  let i' = maybe 0 fst $ whichScreen pointer $ zip [0..] screens
  let locations = topDown (placeWindow mode i' screens) (Transformer id id $ Plane (Rect 0 0 0 0) 0) t
  -- Draw the tiler we've been given
  let (winOrder, io) = cata (draw i') $ fmap unTransform locations
  io
  -- Hide all of the popped tilers
  get @[Fix Tiler]
    >>= traverse_ (snd . cata (draw i') . fmap unTransform . topDown (placeWindow mode i' $ repeat $ Rect 0 0 0 0) (Transformer id id $ Plane (Rect 0 0 0 0) 0))
  return winOrder
 where draw :: RenderEffect r => Int -> Base (Cofree Tiler Plane) ([Window], Sem r ()) -> ([Window], Sem r ())
       draw _ (Plane (Rect _ _ 0 0) _ :<~ Wrap (ChildParent win _)) = ([], minimize win)
       draw _ (Plane Rect {..} _ :<~ Wrap (ChildParent win win')) = ([win], do
           restore win
           restore win'
           changeLocation win $ Rect x y (abs w) (abs h)
           changeLocation win' $ Rect 0 0 (abs w) (abs h))
       draw i' (Plane Rect{..} depth :<~ InputController i t)
         | i == i' = 
           (maybe [] fst t, do
              mapM_ snd t
              -- Extract the border windows
              (l, u, r, d) <- input @Borders
              let winList = [l, u, r, d]

              -- Calculate the color for our depth
              let hue = 360.0 * ((0.5 + (fromIntegral (depth - 1) * 0.618033988749895)) `mod'` 1)
              -- color <- getColor $ "TekHVC:"++show hue++"/50/95"

              currentMode <- get
              if hasBorders currentMode
                then do
                    -- Draw them with the right color and position
                    changeLocationS l $ Rect x y 2 h
                    changeLocationS u $ Rect (x + 2) y (w-2) 10
                    changeLocationS d $ Rect x (y+fromIntegral h-2) w 2
                    changeLocationS r $ Rect (x+fromIntegral w-2) y 2 h

                    -- Convince our windows to be redrawn with the right color and position
                    -- traverse_ (`changeLocationS` Rect 0 0 1 1) winList
                    traverse_ (`changeColor` hsvToRgb hue 0.5 0.9) winList
                    get @(Fix Tiler) >>= drawText u . cata getFocusList
                else do
                    changeLocationS l $ Rect 10000 0 0 0
                    changeLocationS u $ Rect 10000 0 0 0
                    changeLocationS d $ Rect 10000 0 0 0
                    changeLocationS r $ Rect 10000 0 0 0
                    traverse_ (`changeColor` hsvToRgb hue 0.5 0.9) winList
                    get @(Fix Tiler) >>= drawText u . cata getFocusList
                    
              traverse_ bufferSwap winList
           )
         | otherwise = (maybe [] fst t, mapM_ snd t)

       draw _ (_ :<~ Floating ls) = 
          (tops ++ bottoms, mapM_ (snd . getEither) ls)
              where tops = foldl' onlyTops [] ls
                    onlyTops acc (Top (_, (ws, _))) = ws ++ acc
                    onlyTops acc _ = acc
                    bottoms = foldl' onlyBottoms [] ls
                    onlyBottoms acc (Bottom (ws, _)) = acc ++ ws
                    onlyBottoms acc _ = acc
       draw _ (_ :<~ t) = (concatMap fst t, mapM_ snd t)
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

          
          
writePath :: Members '[State (Fix Tiler), Input Borders, Colorer, Property] r 
          => Sem r ()
writePath = do
  (_, u, _, _) <- input @Borders
  root <- get @(Fix Tiler)
  drawText u $ cata getFocusList root

-- |Focus the X window
xFocus
  :: Members [State (Fix Tiler), Minimizer, Input Window] r
  => Sem r ()
xFocus = do
  root <- get @(Fix Tiler)
  rWin <- input @Window
  let w = fromMaybe (rWin, rWin) $ extract $ ana @(Beam _) makeList root
  restore $ fst w
  restore $ snd w
  setFocus $ snd w
 where
  makeList (Fix (Wrap (ChildParent w w')))              = EndF $ Just (w, w')
  -- TODO there's a lot of code duplication here between InputController and Monitor
  makeList (Fix (InputController _ (Just t))) = ContinueF t
  makeList (Fix (InputController _ Nothing)) = EndF Nothing
  makeList (Fix (Monitor _ (Just t))) = ContinueF t
  makeList (Fix (Monitor _ Nothing)) = EndF Nothing
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
                                LeftButton  _ -> id
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

pushOrAdd :: Fix Tiler -> Maybe (Tiler (Fix Tiler)) -> Tiler (Fix Tiler)
pushOrAdd tWin = maybe (unfix tWin) (\case
  t@(Horiz _) -> add Back Focused tWin t
  t@(Floating _) -> add Back Focused tWin t
  t -> mkHoriz t
                                    )
    where mkHoriz t = Horiz $ makeFL (NE (Sized 0 $ Fix t) [Sized 0 tWin]) 0
