{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PartialTypeSignatures #-}


module XEvents where

import           Standard
import           Base
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Graphics.X11.Types
import           Types
import           Data.Either                    ( )
import           Tiler
import Core
import           FocusList
import           Data.Bits
import qualified Data.Map.Strict as M

-- |Called when a new top level window wants to exist
mapWin :: Member (State (Fix Tiler)) r
         => Members (Inputs [Pointer, Screens]) r
         => Members [EventFlags, GlobalX, State (Maybe ())] r
         => Window
         -> Sem r ()
mapWin window = do
  -- TODO You're about to see these 3 lines of code a whole lot. Maybe they should
  -- be a function or something.
  pointer <- input @Pointer
  screens <- input @Screens
  let i = maybe 0 fst $ whichScreen pointer $ zip [0..] screens

  -- Before we do anything else, let's see if we already manage this window.
  -- I don't know why, but things like Inkscape map windows multiple times.
  unlessM (findWindow window <$> get) $ do
    -- Reparent the window inside of a new one.
    -- Originally, Xest didn't do this but then a bunch of bugs came up
    -- where crossing events weren't reported correctly. All of those
    -- bugs went away when reparenting was added.
    newWin <- newWindow window

    -- Think of the new parent as an extension of the root window.
    -- Just like on the root window, we need to register some events
    -- on the parent.
    selectFlags newWin (substructureNotifyMask .|. substructureRedirectMask .|. structureNotifyMask .|. enterWindowMask )-- .|. buttonPressMask .|. buttonReleaseMask)

    -- TODO: I wonder if there's a better way to do all this wrapping...
    let tWin = Fix $ Wrap $ ChildParent newWin window


    -- This adds the new window to whatever tiler comes after inputController
    -- If you've zoomed the inputController in, you get nesting as a result
    modify . cata . applyInput i $ Just . pushOrAdd tWin
    put $ Just ()

  where 
    findWindow :: Window -> Fix Tiler -> Bool
    findWindow w = cata $ \case
          (Wrap w') -> inChildParent w w'
          t -> or t

-- |A window was killed and no longer exists. Remove everything that
-- was related to it.
killed :: Members (States [Fix Tiler, Tiler (Fix Tiler), LocCache, Maybe ()]) r
       => Member GlobalX r
       => Window 
       -> Sem r ()
killed window = do
  -- Find the parent in the tree and kill it.
  (findParent window <$> get) >>= traverse_ (kill True >=> const (put $ Just ()))
  -- Remove the window from the tree.
  modify @(Tiler (Fix Tiler)) $ ripOut window
  -- Remove the window from our cache
  modify @LocCache $ M.delete window

-- |A window is either dying slowly or has been minimized.
unmapWin :: Members (States [Fix Tiler, Tiler (Fix Tiler), Set Window, LocCache, Maybe ()]) r
         => Member GlobalX r
         => Window 
         -> Sem r ()
unmapWin window =
  -- We need to check if we expected the window to be unmapped. Any window
  -- we explicitly minimized ends up in a set (Thanks XMonad for the idea).
  -- If the window is in the set, we don't need to do anything.
  unlessM (member window <$> get @(Set Window)) $ do
    -- Windows that weren't minimized but were unmapped are probably dying.
    -- We need to move the window onto the root so that we can kill the parent
    -- and it can die in its own time.
    moveToRoot window
    killed window

-- |If we get a configure window event on the root, it probably means the user
-- connected a new monitor or removed an old one.
rootChange :: Members '[Input Screens] r
           => Members (States [Tiler (Fix Tiler), Fix Tiler, Maybe ()]) r
           => Sem r ()
rootChange = do
  -- Use Xinerama to figure out how many monitors there are.
  screens <- input @Screens
  let len = length screens

  -- If the user unplugged the monitor, remove it from the tree.
  -- Every monitor has a Monitor and a IC associated with it. They both have to go.
  modify @(Fix Tiler) $ fromMaybe (error "No root!")
                          . (removeIC len >=> removeMonitor len)

  -- Unless we've already added the monitor, add it now.
  unlessM (findMonitor (len - 1) <$> get @(Fix Tiler)) $
    -- TODO I don't think I like having to write Just . Fix . ... All the time. Maybe there's
    -- a better way to do it?
     modify @(Tiler (Fix Tiler)) $ Monitor (len - 1) . Just . Fix . InputController (len - 1) . Just . Fix
  put Nothing

  where removeMonitor i = cata $ \case
          Monitor i' t
            | i == i' -> join t
            | otherwise -> Just . Fix . Monitor i' $ join t
          t -> Fix <$> reduce t
        removeIC i = cata $ \case 
          InputController i' t
            | i == i' -> join t
            | otherwise -> Just . Fix . InputController i' $ join t
          t -> Fix <$> reduce t

        findMonitor i = cata $ \case
          Monitor i' rest -> i == i' || fromMaybe False rest
          t -> or t


-- |Called when the mouse moves between windows or when the user
-- clicks a window.
newFocus :: Members '[Input Screens, Property, Minimizer] r
         => Members (States [Fix Tiler, Maybe ()]) r
         => Window
         -> (Int32, Int32)
         -> Sem r ()
newFocus window mousePos = do
  -- Find the active screen. Sometimes, the mouse won't be on any screen.
  -- In that case, pretend like we're on the first screen.
  screen <- 
    maybe 0 fst . whichScreen mousePos . zip [0..] <$> input @Screens

  -- Change our tree so the focused window is the one we're hovering over
  mNewRoot <- 
    focusWindow screen window <$> get @(Fix Tiler)

  -- The root might be none if the newly focused window doesn't exist
  case mNewRoot of
    Just newRoot -> do
      realWin <- getChild window
      traverse_ setFocus realWin
      put @(Fix Tiler) newRoot
      put $ Just ()
    -- TODO is this pointless right here?
    Nothing -> setFocus window


-- |On key press, execute some actions
keyDown :: Members '[Property, Minimizer] r
       => Members (Inputs [Conf, Pointer, MouseButtons]) r
       => Members (States [Fix Tiler, Mode, KeyStatus, Maybe ()]) r
       => KeyCode
       -> EventType
       -> Sem r Actions
keyDown keycode eventType
  | eventType == keyPress = do
    put $ Just ()
    Conf bindings _ <- input @Conf
    mode <- get @Mode
    -- Is keycode (the key that was pressed) equal to k (the bound key)
    case find (\(k, m, _) -> keycode == k && m == mode) bindings of
              Nothing -> return []
              Just (_, _, actions) -> do
                -- KeyStatus is a state machine which decides if we
                -- need to act like vim or notepad.
                -- If the user holds down a key then clicks another,
                -- act like notepad. If they press a key then release
                -- it, act like vim.
                modify @KeyStatus $ \case
                  Default -> New mode keycode
                  New oldMode watchedKey ->
                    if watchedKey == keycode
                       then New oldMode watchedKey
                       else Temp oldMode watchedKey
                  ks -> ks
                return actions

  | otherwise = do
    put $ Just ()
    (newKS, actions) <- get @KeyStatus <&> \case
      New oldMode watchedKey ->
        if watchedKey == keycode
           then (Default, [])
           else (New oldMode watchedKey, [])
      Temp oldMode watchedKey ->
        if watchedKey == keycode
           then (Default, [ChangeModeTo oldMode])
           else (Temp oldMode watchedKey, [])
      Default -> (Default, [])
    put @KeyStatus newKS
    return actions

-- |When the user moves the mouse in resize mode, this events are triggered.
motion :: Members '[Property, Minimizer] r
       => Members (Inputs [Screens, Pointer, MouseButtons]) r
       => Members (States [Fix Tiler, MouseButtons, Maybe ()]) r
       => Sem r ()
motion = do
  -- First, let's find the current screen and its dimensions.
  screens <- input @Screens
  pointer <- input @Pointer
  let (i', Rect _ _ width height) =
        fromMaybe (0, Rect 0 0 100000 100000) 
          $ whichScreen pointer $ zip [0..] screens

  -- Get the real locations of every window
  sized <- 
    topDown 
      (placeWindow screens) 
      (Transformer id id $ Plane (Rect 0 0 width height) 0)
      <$> get

  -- Find the tiler that comes right after the input controller.
  case journey $ ana @(Path _ _) (getInput i') sized of
    (rotations, Just (Transformer _ _ Plane {rect = size})) -> do
      -- Determines whether we're currently rotated or not.
      let rotation :: Bool = foldr ($) False rotations

      -- Move the tiler based on the mouse movement
      modify
        =<< cata . applyInput i' . fmap . changeSize rotation size 
              <$> get @MouseButtons
    _ -> return ()

  input @MouseButtons >>= put

  -- We know what button is pressed, now we need to update the location
  updateMouseLoc pointer
  put $ Just ()

       -- Gets the thing right after the input controller but also counts the number of rotations
 where getInput :: Int -> Cofree Tiler (Transformer Plane) -> PathF (Maybe (Transformer Plane)) (Bool -> Bool) (Cofree Tiler (Transformer Plane))
       getInput i (_ :< InputController i' a)
         | i == i' = FinishF $ fmap extract a
         | otherwise = maybe (FinishF Nothing) RoadF a
       getInput _ (_ :< Monitor _ (Just a)) = RoadF a
       getInput _ (_ :< Monitor _ Nothing) = FinishF Nothing
       getInput _ (_ :< Reflect a) = BreakF not a
       getInput _ (_ :< b) = RoadF $ getFocused b

       updateMouseLoc :: Member (State MouseButtons) r => (Int32, Int32) -> Sem r ()
       updateMouseLoc (newX, newY) = modify @MouseButtons $ \case
           LeftButton _ -> LeftButton (fromIntegral newX, fromIntegral newY)
           RightButton _ -> RightButton (fromIntegral newX, fromIntegral newY)
           None -> None

-- |Helper function for motion.
-- TODO This function probably belongs in Tiler.
-- TODO This function is awful.
changeSize :: Bool -> Rect -> MouseButtons -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
changeSize _ _ None = id
changeSize rotation Rect{..} m = \case
  Horiz fl ->
    -- TODO The logic in here is terrible to follow...
    -- What's the right way to write this?
    let numWins = fromIntegral $ length fl
        dx = (if rotation then snd else fst) $ getButtonLoc m
        windowSize = 1 / numWins
        mouseDelta = fromIntegral dx
        sign = if dx < 0 then (-) 0 else id
        delta = mouseDelta / fromIntegral w
        twoPx = 2 / fromIntegral w
        foc = fromIntegral $ (case m of
                                RightButton _ -> (+1)
                                -- LeftButton == RightButton on the previous window
                                LeftButton  _ -> id
                                None -> error "Yikes, this shouldn't be possible"
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
    let (dx, dy) = (if rotation then swap else id) $ getButtonLoc m
        (ddx, ddy) = (fromIntegral dx / fromIntegral w, fromIntegral dy / fromIntegral h)
        twoPx = 2 / fromIntegral w
        boundedX = max (twoPx - wp) $ min (xp + ddx) (1 - twoPx)
        boundedY = max (twoPx - hp) $ min (yp + ddy) (1 - twoPx)
    in Floating $ NE (case m of
      RightButton _ -> Top (RRect xp yp (wp + ddx) (hp + ddy), t)
      LeftButton _ -> Top (RRect boundedX boundedY wp hp, t)
      None -> error "Yikes, this shouldn't be possible!") ls
  t -> t
