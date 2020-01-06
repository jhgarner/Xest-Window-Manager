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
import           Graphics.X11.Xinerama
import           Types
import           Data.Either                    ( )
import           Tiler
import Core
import           FocusList
import           Data.Bits
import qualified Data.Map.Strict as M
import           Graphics.X11.Xlib.Extras

-- |Called when a new top level window wants to exist
mapWin :: Members (Inputs '[Pointer]) r
       => Members [State Tiler, EventFlags, GlobalX, State (Maybe ()), Property] r
       => Members '[Input Screens, Property, Minimizer, Input Pointer] r
       => Members (States [Tiler, Maybe (), ActiveScreen, Screens]) r
       => Window
       -> Sem r ()
mapWin window =
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
    selectFlags newWin (substructureNotifyMask .|. substructureRedirectMask .|. structureNotifyMask .|. enterWindowMask .|. leaveWindowMask)-- .|. buttonPressMask .|. buttonReleaseMask)

    let tWin = Wrap $ ChildParent newWin window

    transient <- getTransientFor window
    -- let transient = Nothing
    
    case transient of
      Just parent -> do
        root <- get @Tiler
        SizeHints{..} <- getSizeHints window
        let idealSize = fromMaybe (50, 50) $ sh_base_size <|> sh_min_size
        let tilerParent = Wrap $ ChildParent parent parent
            (worked, newRoot) = usingFloating idealSize tilerParent tWin root
            altNewRoot = makeFloating tilerParent tWin root
        put @Tiler $ if worked then newRoot else altNewRoot
      Nothing ->
        -- This adds the new window to whatever tiler comes after inputController
        -- If you've zoomed the inputController in, you get nesting as a result
        modify $ applyInput $ coerce $ Just . pushOrAdd tWin


    newFocus window

  where 
    findWindow :: Window -> Tiler -> Bool
    findWindow w = cata $ \case
          (Wrap w') -> inChildParent w w'
          t -> or t
    usingFloating :: (Double, Double) -> SubTiler -> SubTiler -> Tiler -> (Bool, Tiler)
    usingFloating size t newTiler = coerce . cata \case
      oldT@(Floating fl) ->
        if (Bottom (False, t)) `elem` fl
          then (True, Floating $ Top (RRect 0 0 0.5 0.5, newTiler) +: fmap (fmap snd) fl)
          else (any fst oldT, Fix $ fmap snd oldT)
      oldT -> (any fst oldT, Fix $ fmap snd oldT)

    makeFloating :: SubTiler -> SubTiler -> Tiler -> Tiler
    makeFloating t newTiler = coerce . cata \oldT ->
      if oldT == unfix t
          then Floating $ NE (Top (RRect 0 0 0.5 0.5, newTiler)) [Bottom $ Fix oldT]
          else Fix oldT

-- |A window was killed and no longer exists. Remove everything that
-- was related to it.
killed :: Members (States [Tiler, LocCache, Maybe ()]) r
       => Member GlobalX r
       => Window 
       -> Sem r ()
killed window = do
  -- Find the parent in the tree and kill it.
  (findParent window <$> get) >>= traverse_ (kill True >=> const (put $ Just ()))
  -- Remove the window from the tree.
  modify @Tiler $ ripOut window
  -- Remove the window from our cache
  modify @LocCache $ M.delete window

-- |A window is either dying slowly or has been minimized.
unmapWin :: Members (States [Tiler, Set Window, LocCache, Maybe ()]) r
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
rootChange :: Members '[Input [XineramaScreenInfo], Input NewBorders, Mover] r
           => Members (States [Tiler, Maybe (), Screens, ActiveScreen, [SubTiler]]) r
           => Sem r ()
rootChange = do
  -- Update the list of screens
  screenInfo <- input @[XineramaScreenInfo]
  oldScreens <- get @Screens
  let defaultTiler = Monitor $ Just $ Fix $ InputController Nothing
  newScreens <-
        mapFromList <$>
          traverse (\(XineramaScreenInfo name x y w h) -> do
                NewBorders newBorders <- input @NewBorders
                return ( fromIntegral name
                  , Screen'
                      (Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))
                      (findWithDefault defaultTiler (fromIntegral name) $ fmap screenTree oldScreens)
                      newBorders
                  )
              ) screenInfo
  put newScreens
  -- Update the active screen if that monitor got disconnected
  modify @ActiveScreen $ \activeScreen ->
    if member activeScreen newScreens then activeScreen else headEx (keys newScreens)

  -- Put all of the dead monitors into the minimized window stack
  traverse_ ((fold . fmap (modify @[SubTiler] . (:))) . screenTree) $ difference oldScreens newScreens
  -- Delete all of the border windows
  traverse_ ((\(a, b, c, d) -> traverse_ destroySDLWindow [a, b, c, d]) . screenBorders) $ difference oldScreens newScreens
  put $ Just ()
  

-- |Called when the mouse moves between windows or when the user
-- clicks a window.
newFocus :: Members '[Input Screens, Property, Minimizer, Input Pointer] r
         => Members (States [Tiler, Maybe (), ActiveScreen, Screens]) r
         => Window
         -> Sem r ()
newFocus window = do
  -- Change our tree so the focused window is the one we're hovering over
  mNewRoot <- 
    focusWindow window <$> get @Tiler
  setScreenFromMouse

  -- The root might be none if the newly focused window doesn't exist
  case mNewRoot of
    Just newRoot -> do
      realWin <- getChild window
      traverse_ setFocus realWin
      put @Tiler newRoot
      put $ Just ()
    -- TODO is this pointless right here?
    Nothing -> setFocus window


-- |On key press, execute some actions
keyDown :: Members '[Property, Minimizer] r
       => Members (Inputs [Conf, Pointer, MouseButtons]) r
       => Members (States [Tiler, Mode, KeyStatus, Maybe ()]) r
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
                  Default -> Temp Default mode keycode
                  Dead _ -> error "Dead got into keyDown"
                  ks@(New oldKS oldMode watchedKey) ->
                    if watchedKey == keycode
                       then ks
                       else Temp ( Temp oldKS oldMode watchedKey) mode keycode
                  ks@(Temp _ _ watchedKey) ->
                    if watchedKey == keycode
                       then ks
                       else Temp ks mode keycode
                return actions

  | otherwise = do
    put $ Just ()
    currentKS <- get @KeyStatus
    let (newKS_, actions) = para doRelease currentKS
    newKS_
    return actions
      where doRelease :: Member (State KeyStatus) r
                      => KeyStatusF (KeyStatus, (Sem r (), [Action]))
                      -> (Sem r (), [Action])
            doRelease = \case
              NewF (_, (cks, as)) _ watchedKey ->
                if watchedKey == keycode
                  then (put @KeyStatus Default, as)
                  else (cks, as)
              TempF (oldKS, (cks, as)) oldMode watchedKey ->
                if watchedKey == keycode
                  then (put @KeyStatus $ Dead oldKS, ChangeModeTo oldMode : as)
                  else (cks, as)
              DefaultF -> (return (), [])
              DeadF _ -> error "Dead got into doRelease"

-- |When the user moves the mouse in resize mode, this events are triggered.
motion :: Members '[Property, Minimizer] r
       => Members (Inputs [Screens, Pointer, MouseButtons, Rect]) r
       => Members (States [Tiler, MouseButtons, Maybe ()]) r
       => Sem r ()
motion = do
  -- First, let's find the current screen and its dimensions.
  Rect _ _ width height <- input @Rect
  pointer@(xPointer, yPointer) <- input @Pointer
  dPointer <- get @MouseButtons <&> \case
    LeftButton (x', y') -> LeftButton (fromIntegral xPointer - x', fromIntegral yPointer - y')
    RightButton (x', y') -> RightButton (fromIntegral xPointer - x', fromIntegral yPointer - y')
    None -> None
  

  -- Get the real locations of every window
  sized <- placeWindow (Rect 0 0 width height) <$> get @Tiler

  -- Find the tiler that comes right after the input controller.
  case journey $ ana @(Path _ _) getInput sized of
    (rotations, Just (Transformer _ _ Plane {rect = size})) -> do
      -- Determines whether we're currently rotated or not.
      let rotation :: Bool = foldr ($) False rotations

      -- Move the tiler based on the mouse movement
      modify
        $ applyInput $ fmap $ coerce (changeSize rotation size dPointer)
    _ -> return ()

  input @MouseButtons >>= put

  -- We know what button is pressed, now we need to update the location
  updateMouseLoc pointer
  put $ Just ()

       -- Gets the thing right after the input controller but also counts the number of rotations
 where getInput :: Cofree TilerF (Transformer Plane) -> PathF (Maybe (Transformer Plane)) (Bool -> Bool) (Cofree TilerF (Transformer Plane))
       getInput (_ :< InputController a) = FinishF $ fmap extract a
       getInput (_ :< Monitor (Just a)) = RoadF a
       getInput (_ :< Monitor Nothing) = FinishF Nothing
       getInput (_ :< Reflect a) = BreakF not a
       getInput (_ :< b) = RoadF $ getFocused b

       updateMouseLoc :: Member (State MouseButtons) r => (Int32, Int32) -> Sem r ()
       updateMouseLoc (newX, newY) = modify @MouseButtons $ \case
           LeftButton _ -> LeftButton (fromIntegral newX, fromIntegral newY)
           RightButton _ -> RightButton (fromIntegral newX, fromIntegral newY)
           None -> None

-- |Helper function for motion.
-- TODO This function probably belongs in Tiler.
-- TODO This function is awful.
changeSize :: Bool -> Rect -> MouseButtons -> Tiler -> Tiler
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
