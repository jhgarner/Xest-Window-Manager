{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PartialTypeSignatures #-}


module XEvents where

import           Standard
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Graphics.X11.Types
import           Graphics.X11.Xinerama
import           Data.Either                    ( )
import           Tiler.Tiler
import           Base.DoAll
import Core
import           FocusList
import           Data.Bits
import qualified Data.Map.Strict as M
import           Graphics.X11.Xlib.Extras
import Config
import Actions.ActionTypes

-- |Called when we want to reparent a window
reparentWin :: Members (Inputs '[Pointer]) r
       => Members [State Tiler, EventFlags, GlobalX, State (Maybe ()), Property] r
       => Members '[Input Screens, Property, Minimizer, Input Pointer, Executor] r
       => Members (States [Tiler, Maybe (), ActiveScreen, Screens, LostWindow]) r
       => Window
       -> Sem r ParentChild
reparentWin window = do
  -- Reparent the window inside of a new one.
  -- Originally, Xest didn't do this but then a bunch of bugs came up
  -- where crossing events weren't reported correctly. All of those
  -- bugs went away when reparenting was added.
  newWin <- newWindow window
  printMe $ "\n\n window: " ++ show window ++ " with parent " ++ show newWin ++ "\n\n"

  -- Think of the new parent as an extension of the root window.
  -- Just like on the root window, we need to register some events
  -- on the parent.
  selectFlags newWin (substructureNotifyMask .|. substructureRedirectMask .|. structureNotifyMask .|. enterWindowMask .|. leaveWindowMask .|. buttonPressMask .|. buttonReleaseMask)
  return $ ParentChild newWin window

-- |Called when a new top level window wants to exist
mapWin :: Members (Inputs '[Pointer]) r
       => Members [State Tiler, EventFlags, GlobalX, State (Maybe ()), Property] r
       => Members '[Input Screens, Property, Minimizer, Input Pointer, Executor] r
       => Members (States [Tiler, Maybe (), ActiveScreen, Screens, LostWindow]) r
       => ParentChild
       -> Sem r ()
mapWin pc@(ParentChild newWin window) = do
  transient <- getTransientFor window

  let tWin :: SubTiler = Wrap $ ParentChild newWin window

  case transient of
    Just parent -> do
      root <- get @Tiler
      SizeHints{..} <- getSizeHints window
      let idealSize = fromMaybe (500, 500) $ sh_base_size <|> sh_min_size
      let tilerParent = Wrap $ ParentChild parent parent
          (worked, newRoot) = usingFloating (bimap fromIntegral fromIntegral idealSize) tilerParent tWin root
          (workedAlt, altNewRoot) = makeFloating (bimap fromIntegral fromIntegral idealSize) tilerParent tWin root
      printMe $ "Checking transient " ++ show transient ++ " with parent " ++ show newWin ++ "\nWith newRoot = " ++ show newRoot ++ "\n and worked = " ++ show worked ++ " and altRoot = " ++ show altNewRoot ++ "\n\n"
      if
          | worked -> put @Tiler newRoot >> focusPC
          | workedAlt -> put @Tiler altNewRoot >> focusPC
          | otherwise -> modify @LostWindow $ insertWith (++) parent [pc]
    Nothing -> do
      -- This adds the new window to whatever tiler comes after inputController
      -- If you've zoomed the inputController in, you get nesting as a result
      modify $ applyInput $ coerce $ \tiler -> fmap (add tWin) tiler <|> Just (coerce tWin)
      focusPC



  -- Were any lost children expecting to find this window?
  lostChildren <- lookup window <$> get @LostWindow
  traverse_ (traverse_ mapWin) lostChildren

  where 
    usingFloating :: (Double, Double) -> SubTiler -> SubTiler -> Tiler -> (Bool, Tiler)
    usingFloating (newW, newH) t newTiler = coerce . cata (\case
      oldT@(Floating fl) ->
        if Bottom (False, t) `elem` fl
          then (True, Floating $ Top (Rect 0 0 newW newH, newTiler) +: fmap (fmap snd) fl)
          else (any fst oldT, Fix $ fmap snd oldT)
      oldT -> (any fst oldT, Fix $ fmap snd oldT))

    makeFloating :: (Double, Double) -> SubTiler -> SubTiler -> Tiler -> (Bool, Tiler)
    makeFloating (newW, newH) t newTiler = coerce . cata (\oldTAndB ->
      let oldT = fmap snd oldTAndB
          wasFound = any fst oldTAndB
       in if oldT == unfix t
            then (True, Floating $ NE (Top (Rect 0 0 newW newH, newTiler)) [Bottom $ Fix oldT])
            else (wasFound, Fix oldT))
    focusPC = restore newWin >> restore window >> newFocus newWin

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
       -> Sem r [Action]
keyDown keycode eventType
  | eventType == keyPress = do
    put $ Just ()
    Conf bindings _ _ <- input @Conf
    mode <- get @Mode
    -- Is keycode (the key that was pressed) equal to k (the bound key)
    case find (\(KeyTrigger k m _ _) -> keycode == k && m == mode) bindings of
              Nothing -> return []
              Just (KeyTrigger _ _ actions newEa) -> do
                -- KeyStatus is a state machine which decides if we
                -- need to act like vim or notepad.
                -- If the user holds down a key then clicks another,
                -- act like notepad. If they press a key then release
                -- it, act like vim.
                modify @KeyStatus $ \case
                  Default -> Temp Default mode keycode newEa
                  Dead _ -> error "Dead got into keyDown"
                  ks@(New oldKS oldMode watchedKey ea) ->
                    if watchedKey == keycode
                       then ks
                       else Temp ( Temp oldKS oldMode watchedKey ea) mode keycode newEa
                  ks@(Temp _ _ watchedKey _) ->
                    if watchedKey == keycode
                       then ks
                       else Temp ks mode keycode newEa
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
              NewF (_, (cks, as)) _ watchedKey _ ->
                if watchedKey == keycode
                  then (put @KeyStatus Default, as)
                  else (cks, as)
              TempF (oldKS, (cks, as)) oldMode watchedKey ea ->
                if watchedKey == keycode
                  then (put @KeyStatus $ Dead oldKS, ChangeModeTo oldMode : ea ++ as)
                  else (cks, as)
              DefaultF -> (return (), [])
              DeadF _ -> error "Dead got into doRelease"

-- |When the user moves the mouse in resize mode, this events are triggered.
motion :: Members '[Property, Minimizer] r
       => Members (Inputs [Screens, Pointer, MouseButtons]) r
       => Members (States [Tiler, MouseButtons, Maybe ()]) r
       => Sem r ()
motion = do
  -- First, let's find the current screen and its dimensions.
  pointer@(xPointer, yPointer) <- bimap fromIntegral fromIntegral <$> input @Pointer
  dPointer <- get @MouseButtons <&> \case
    LeftButton (x', y') -> LeftButton (xPointer - x', yPointer - y')
    RightButton (x', y') -> RightButton (xPointer - x', yPointer - y')
    None -> None
  
  -- Are we rotated?
  rotated <- cata numRotations <$> get @Tiler

  -- Move the tiler based on the mouse movement
  modify
    $ applyInput $ fmap $ coerce (changeSize rotated dPointer)

  input @MouseButtons >>= put

  -- We know what button is pressed, now we need to update the location
  updateMouseLoc pointer
  put $ Just ()

 where numRotations :: TilerF Bool -> Bool
       numRotations (InputControllerOrMonitor _ a) = fromMaybe False a
       numRotations (Reflect a) = not a
       numRotations (Wrap _) = False
       numRotations b = getFocused b

       updateMouseLoc :: Member (State MouseButtons) r => (Int, Int) -> Sem r ()
       updateMouseLoc (newX, newY) = modify @MouseButtons $ \case
           LeftButton _ -> LeftButton (newX, newY)
           RightButton _ -> RightButton (newX, newY)
           None -> None

-- |Helper function for motion.
-- TODO This function probably belongs in Tiler.
-- TODO This function is awful.
changeSize :: Bool -> MouseButtons -> Tiler -> Tiler
changeSize _ None = id
changeSize rotation m = \case
  Horiz fl ->
    -- TODO The logic in here is terrible to follow...
    -- What's the right way to write this?
    let numWins = fromIntegral $ length fl
        delta = (if rotation then snd else fst) $ bimap fromIntegral fromIntegral $ getButtonLoc m
        windowSize = 1 / numWins
        sign = if delta < 0 then (-) 0 else id
        foc = fromIntegral $ (case m of
                                RightButton _ -> (+1)
                                -- LeftButton == RightButton on the previous window
                                LeftButton  _ -> id
                                None -> error "Yikes, this shouldn't be possible"
                             ) $ findNeFocIndex fl
        
        bound = max (-windowSize)
        (_, trueDelta)  = foldl' 
          (\(i, minS) (Sized size _) -> if i == foc && foc < numWins
            then (i+1, min minS $ abs $ size - bound (size + delta))
            else if i == foc + 1 && foc > 0
            then (i+1, min minS $ abs $ size - bound (size - delta))
            else (i+1, minS)) (1, 0.01) $ vOrder fl
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
  Floating (NE (Top (Rect{..}, t)) ls) -> 
    let (dx, dy) = (if rotation then swap else id) $ bimap fromIntegral fromIntegral $ getButtonLoc m
    in Floating $ NE (case m of
      RightButton _ -> Top (Rect x y (w + dx) (h + dy), t)
      LeftButton _ -> Top (Rect (x+dx) (y+dy) w h, t)
      None -> error "Yikes, this shouldn't be possible!") ls
  t -> t
