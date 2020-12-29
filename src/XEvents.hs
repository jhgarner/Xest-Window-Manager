{-# LANGUAGE StandaloneDeriving #-}

module XEvents where

import Actions.ActionTypes
import Base.DoAll
import Config
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Core
import Data.Either ()
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M
import FocusList
import Graphics.X11.Types
import Graphics.X11.Xinerama
import Graphics.X11.Xlib.Atom
import Graphics.X11.Xlib.Extras
import Standard
import Tiler.Tiler

-- | Called when we want to reparent a window
reparentWin ::
  Members '[EventFlags, GlobalX, Log LogData, Property] m =>
  Window ->
  m ParentChild
reparentWin window = do
  -- Reparent the window inside of a new one.
  -- Originally, Xest didn't do this but then a bunch of bugs came up
  -- where crossing events weren't reported correctly. All of those
  -- bugs went away when reparenting was added.
  (newWin, pWin) <- newWindow window
  log $ LD "ReparentWin" $ show window <> " with parent " <> show newWin

  -- Think of the new parent as an extension of the root window.
  -- Just like on the root window, we need to register some events
  -- on the parent.
  selectFlags newWin (substructureNotifyMask .|. substructureRedirectMask .|. enterWindowMask .|. leaveWindowMask)
  return $ ParentChild newWin window pWin

deriving instance Show SizeHints

-- | Called when a new top level window wants to exist
mapWin ::
  Members (Inputs '[Pointer, Screens]) m =>
  Members [EventFlags, GlobalX, Property, Log LogData, Mover] m =>
  Members (States [Screens, Tiler, ShouldRedraw, ActiveScreen, Screens, LostWindow, OldTime, DockState]) m =>
  ParentChild ->
  m ()
mapWin pc@(ParentChild newWin window _) = do
  log $ LD "MapWin" "Mapping a window"
  let tWin :: SubTiler = Wrap pc

  -- This code will try two paths (one for each side of the <|>).
  -- If the "left" side fails, then that means we aren't a transient window.
  -- The "right" side will always succeed.
  void $
    runMaybeT $
      do
        -- Get the parent window if it exists as an Xlib window. We don't know
        -- that the parent actually exists yet.
        parent <- MaybeT $ getTransientFor window
        -- Some random programs run in Wine did this and tripped me up a lot. Is
        -- there a real way to handle it?
        guard $ parent /= window
        
        -- If the parent exists, then focus on the screen with the parent.
        setScreenFromWindow parent
        
        -- At this point we can no longer fail and definitely have a transient
        -- window.
        lift do
          root <- get @Tiler
          log $ LD "MapWin" "Found a transient window!"
          SizeHints {..} <- getSizeHints window
          let idealSize = maybe (-1, -1) (over both fromIntegral) sh_min_size
              tilerParent = Wrap $ ParentChild parent parent parent
              -- First try to run usingFloating on the parameters and see if it
              -- succeeds. If it succeeds then we're done. Otherwise, run
              -- makeFloating.
              newRoot =
                foldMap1 (\f -> f idealSize tilerParent tWin root) $
                  usingFloating :| [makeFloating]
          -- If the parent existed, the left side of the <> will succeed.
          -- Otherwise, call this a lost window and wait for the parent to get created.
          extract $
            map (\t -> put @Tiler t >> newFocus newWin) newRoot
              <> Succeeded (modify @LostWindow (M.insertWith (++) parent [pc]))
      <|> lift do
        -- In this case we don't have a transient window. Start by placing the
        -- window.
        modify @Tiler $ applyInput $ coerce $ \tiler -> map (add tWin) tiler <|> Just (coerce tWin)
        newFocus newWin

        -- Make the window full screen if needed
        wm_state <- getAtom False "_NET_WM_STATE"
        full_screen <- getAtom False "_NET_WM_STATE_FULLSCREEN"
        isFullScreen <- (== Just full_screen) . headMay <$> getProperty 32 wm_state window
        when isFullScreen $
          makeFullscreen window 1

        -- Were any lost children expecting to find this window?
        lostChildren <- view (at window) <$> get @LostWindow
        traverse_ (traverse_ mapWin) lostChildren
  put @ShouldRedraw $ Just UnsafeRedraw
  where
    -- TODO Yikes to these two functions
    usingFloating :: (Double, Double) -> SubTiler -> SubTiler -> Tiler -> Tagged Tiler
    usingFloating (newW, newH) t newTiler =
      coerce . cata \case
        oldT@(Many (Floating fl) mods) ->
          let bottom = first1Of vTraverse fl
           in if Failed t == extract bottom
                then Succeeded $ Many (Floating $ push Back Focused (WithRect (Rect 0 0 newW newH) newTiler) $ map (map extract) fl) mods
                else Fix <$> sequenceA oldT
        oldT -> Fix <$> sequenceA oldT

    makeFloating :: (Double, Double) -> SubTiler -> SubTiler -> Tiler -> Tagged Tiler
    makeFloating (newW, newH) t newTiler =
      coerce . cata \oldTAndB ->
        let oldT = map extract oldTAndB
         in if oldT == unfix t
              then Succeeded $ Many (Floating $ makeFL (WithRect (Rect (-1) (-1) (-1) (-1)) (Fix oldT) :| [WithRect (Rect 0 0 newW newH) newTiler]) 1) NoMods
              else Fix <$> sequenceA oldTAndB

-- | A window was killed and no longer exists. Remove everything that
--  was related to it.
killed ::
  Members (GlobalX ': States [Screens, LocCache, ShouldRedraw, Docks]) m =>
  Window ->
  m ()
killed window = do
  -- Find the parent in the tree and kill it.
  parentM <- asum . map (findParent window) <$> gets @Screens screensToTilers
  case parentM of
    Just parent -> do
      kill True parent
      put @ShouldRedraw $ Just UnsafeRedraw
    Nothing -> return ()
  -- Remove the window from our cache
  modify @LocCache $ M.delete window
  -- Remove the window from the tree.
  modify @Screens $ map (ripOut window)
  -- Remove the window from the dock's cache.
  modify @Docks $ Docks . mfilter (/= window) . undock

-- | A window is either dying slowly or has been minimized.
unmapWin ::
  Members (States [Screens, LocCache, ShouldRedraw, Docks]) m =>
  Members [GlobalX, Property] m =>
  Window ->
  m ()
unmapWin window = do
  roots <- get @Screens

  unless (isNothing (getTilerWithWindow window roots)) $ do
    -- Windows that were unmapped are probably dying.
    -- We need to move the window onto the root so that we can kill the parent
    -- and it can die in its own time.
    moveToRoot window
    killed window

-- | If we get a configure window event on the root, it probably means the user
--  connected a new monitor or removed an old one.
rootChange ::
  Members '[Input [XineramaScreenInfo], Input NewBorders] m =>
  Members (States [ShouldRedraw, Screens, ActiveScreen, [SubTiler], FocusedCache]) m =>
  m ()
rootChange = do
  -- Update the list of screens
  screenInfo <- input @[XineramaScreenInfo]
  oldScreens <- get @Screens
  newScreens <-
    IM.fromList
      <$> traverse
        ( \(XineramaScreenInfo name x y w h) -> do
            NewBorders newBorders <- input @NewBorders
            -- TODO This line is long...
            let defaultTiler = Monitor (Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)) $ Just $ Fix $ InputController newBorders Nothing
            return
              ( fromIntegral name,
                IM.findWithDefault defaultTiler (fromIntegral name) oldScreens
              )
        )
        screenInfo
  put @Screens newScreens

  -- Update the active screen if that monitor got disconnected
  modify @ActiveScreen $ \activeScreen ->
    if notNullOf (at activeScreen) newScreens then activeScreen else fromJust $ headMay (IM.keys newScreens)

  -- Put all of the dead monitors into the minimized window stack
  -- TODO is there a function that does nested traversals for me?
  traverse_ (traverse_ (modify @[SubTiler] . (:)) . removeDangerous) $ IM.difference oldScreens newScreens

  -- Ask Xest to redraw and refocus
  put @FocusedCache $ FocusedCache 0
  put @ShouldRedraw $ Just UnsafeRedraw

-- | Called when the mouse moves between windows or when the user
--  clicks a window.
newFocus ::
  Members '[Input Screens, Property, Input Pointer, Log LogData] m =>
  Members (States [Screens, Tiler, ShouldRedraw, ActiveScreen, Screens, OldTime]) m =>
  Window ->
  m ()
newFocus window = do
  -- Change our tree so the focused window is the one we're hovering over
  -- It will get focused next time we redraw
  runMaybeT $ setScreenFromWindow window <|> lift setScreenFromMouse
  modify @Tiler \tiler -> fromMaybe tiler $ focusWindow window tiler
  put @ShouldRedraw $ Just SafeRedraw

-- | On key press, execute some actions
keyDown ::
  Members '[Property, Executor] m =>
  Members (Inputs [Conf, Pointer, MouseButtons]) m =>
  Members (States [Tiler, Mode, KeyStatus, ShouldRedraw]) m =>
  Monoid (m ()) =>
  KeyCode ->
  EventType ->
  m [Action]
keyDown keycode eventType
  | eventType == keyPress = do
    put @ShouldRedraw $ Just UnsafeRedraw
    Conf bindings _ _ _ <- input @Conf
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
          Default -> Temp NotMod Default mode keycode newEa
          ks@(New oldKS oldMode watchedKey ea) ->
            if watchedKey == keycode
              then ks
              else Temp NotMod (Temp FromMod oldKS oldMode watchedKey $ ChangeModeTo oldMode : ea) mode keycode newEa
          ks@(Temp _ _ _ watchedKey _) ->
            if watchedKey == keycode
              then ks
              else Temp NotMod ks mode keycode newEa
        return actions
  | otherwise = do
    put @ShouldRedraw $ Just UnsafeRedraw
    currentKS <- get @KeyStatus
    let (newKS_, actions) = (\(a, b) -> (a, b)) $ foldMap (\(a, b) -> (a, b)) $ para doRelease currentKS
    newKS_
    return actions
  where
    doRelease ::
      State KeyStatus m =>
      KeyStatusF (KeyStatus, Maybe (m (), [Action])) ->
      Maybe (m (), [Action])
    doRelease = \case
      NewF (_, otherActions) _ watchedKey _ ->
        case otherActions of
          Just _ -> Just (put @KeyStatus Default, [])
          Nothing ->
            if watchedKey == keycode
              then Just (put @KeyStatus Default, [])
              else Nothing
      TempF _ (oldKS, otherActions) _ watchedKey ea ->
        case otherActions of
          Just (cks, as) -> Just (put @KeyStatus oldKS >> cks, ea ++ as)
          Nothing ->
            if watchedKey == keycode
              then Just (put @KeyStatus oldKS, ea)
              else Nothing
      DefaultF -> Nothing

-- | When the user moves the mouse in resize mode, this events are triggered.
motion ::
  Members '[Property] m =>
  Members (Inputs [Pointer, MouseButtons]) m =>
  Members (States [Tiler, OldMouseButtons, ShouldRedraw]) m =>
  m ()
motion = do
  -- First, let's find the current screen and its dimensions.
  Rect _ _ screenW screenH <- gets @Tiler getScreens
  realButtonState <- input @MouseButtons
  OMB lastButtonState <- get @OldMouseButtons
  case (getButtonLoc realButtonState, getButtonLoc lastButtonState) of
    (Just (xNow, yNow), Just (xLast, yLast)) -> do
      let direction = case realButtonState of
            LeftButton _ -> Left
            RightButton _ -> Right
          change = direction (xNow - xLast, yNow - yLast)
       in do
            modify @Tiler $ applyInput $ map $ coerce (changeSize change (fromIntegral screenW, fromIntegral screenH))
            put @ShouldRedraw $ Just UnsafeRedraw
    (_, _) -> return ()

  input @MouseButtons >>= put @OldMouseButtons . OMB

-- | Helper function for motion.
--  TODO This function probably belongs in Tiler.
changeSize :: Either (Int, Int) (Int, Int) -> (Int, Int) -> Tiler -> Tiler
changeSize mouseLoc screen (Many mh mods) =
  flip Many mods case mh of
    Horiz fl ->
      -- I think the trick to reading this one is to start with the "in" then
      -- work your way up into the "let" bindings.
      let direction = fromIntegral . if mods == Rotate then snd else fst
          delta = direction $ fromEither mouseLoc
          screenSize = direction screen
          deltaPercent = delta / screenSize
          focLoc =
              ( case mouseLoc of
                  Right _ -> id
                  Left _ -> (\n -> n -1)
              )
                $ findNeFocIndex fl
          maxChange = fl ^. singular (elementOf vTraverse (focLoc + 1)) . to getSize
          currentSize = fl ^. singular (elementOf vTraverse focLoc) . to getSize
          bounded = max (0.01 - currentSize) $ min maxChange deltaPercent
          withFocChange = fl & elementOf vTraverse focLoc %~ (\(Sized s a) -> Sized (s + bounded) a)
          withPredChange = withFocChange & elementOf vTraverse (focLoc + 1) %~ (\(Sized s a) -> Sized (s - bounded) a)
       in if focLoc > -1 && focLoc < length fl - 1
            then Horiz withPredChange
            else Horiz fl
    Floating fl ->
      let (dx, dy) = bimap fromIntegral fromIntegral $ fromEither mouseLoc
       in Floating $
            fl & headOf fTraverse %~
              ( \(WithRect Rect {..} t) ->
                  case mouseLoc of
                    Right _ -> WithRect (Rect x y (w + dx) (h + dy)) t
                    Left _ -> WithRect (Rect (x + dx) (y + dy) w h) t
              )
    TwoCols colSize fl ->
      let direction = fromIntegral . if mods == Rotate then snd else fst
          delta = direction $ fromEither mouseLoc
          screenSize = direction screen
          deltaPercent = delta / screenSize
          newColSize = max 0 $ min 1 $ colSize + deltaPercent
       in TwoCols newColSize fl
changeSize _ _ t = t

makeFullscreen ::
  Members '[State Screens, State Tiler, Property, State ActiveScreen, State DockState, State ShouldRedraw, Mover] m =>
  Window ->
  Int ->
  m ()
makeFullscreen window isSet = do
  put @ShouldRedraw $ Just UnsafeRedraw
  runMaybeT $ setScreenFromWindow window
  -- Get the static parameters on Monitor and IC
  loc <-
    gets @Tiler $
      fromMaybe (error "Lost Mon") . cata \case
        Monitor loc _ -> Just loc
        t -> asum t
  bords <-
    gets @Tiler $
      fromMaybe (error "Lost Mon") . cata \case
        InputController bords _ -> Just bords
        t -> asum t

  -- Get some useful atoms and window data
  wm_state <- getAtom False "_NET_WM_STATE"
  wm_full <- getAtom False "_NET_WM_STATE_FULLSCREEN"
  currentState <- getProperty 32 wm_state window

  let shouldSet =
        if isSet == 2
          then wm_full `notElem` currentState
          else isSet == 1

  if shouldSet
    then do
      putProperty 32 wm_state window aTOM $ map fromIntegral (wm_full : currentState)

      -- Modify the tree with the newly zoomed in location
      root <- get @Tiler
      modify @Tiler $
        coerce . fromMaybe (coerce root) . cata \case
          Wrap pc@(ParentChild _ child _)
            | child == window -> Just $ Monitor loc $ Just $ InputController bords $ Just $ Wrap pc
          InputControllerOrMonitor _ t -> coerce $ join t
          t -> coerce $ reduce t

      -- Hide the docks
      put @DockState Hidden
    else do
      putProperty 32 wm_state window aTOM $ map fromIntegral (mfilter (/= wm_full) currentState)
      changeLocation (ParentChild window window window) $ Rect 1 1 1 1
      put @DockState Visible

setScreenFromWindow :: Members '[State Screens, State ActiveScreen] m => Window -> MaybeT m ()
setScreenFromWindow window = do
  tilers <- lift $ gets @Screens $ zip [0 ..] . screensToTilers
  (i, _) <- MaybeT $ return $ find snd $ map (second $ findWindow window) tilers
  lift $ put @ActiveScreen i
