{-# LANGUAGE TupleSections #-}

-- |
--   In the config file, users can specify actions to perform on keypresses or
--   mode changes. This file contains the functions and types used to implement
--   those actions. Each function roughly corresponds to a config action although
--   a few are made generic so that they can implement multiple actions.
module Actions.Actions (module Actions.Actions, module Actions.ActionTypes) where

import Actions.ActionTypes
import Base.DoAll
import Data.Either ()
import FocusList
import Standard
import Tiler.Tiler
import Graphics.X11.Xinerama

-- | Zooms inwards, AKA away from the root. This function operates on
-- the Input Controller, not the Monitor. This means that although the layout
-- usually won't change, you will change the Tiler that will receive actions
-- and events.
zoomInInput ::
  Member (State Tiler) m =>
  Eff m ()
zoomInInput =
  modify @Tiler $
    unfix . cata \case
      t@(InputController _ (Just (Wrap _))) -> Fix t
      InputController bords (Just (Monitor loc Nothing)) -> Monitor loc . Just $ InputController bords Nothing
      InputController bords (Just (Fix t)) -> Fix $ modFocused (Fix . InputController bords . Just) t
      t -> Fix t

-- | Nearly identical to zooming in the input controller.
zoomInMonitor ::
  Member (State Tiler) m =>
  Eff m ()
zoomInMonitor =
  modify @Tiler $
    unfix . cata \case
      t@(Monitor _ (Just (Fix (Wrap _)))) -> Fix (t :: Tiler)
      Monitor loc (Just (Fix t)) -> Fix $ modFocused (Fix . Monitor loc . Just) t
      t -> Fix t

-- | Move the input controller towards the root
zoomOutInput ::
  Member (State Tiler) m =>
  Eff m ()
zoomOutInput = do
  -- The unless guards against zooming the controller out of existence
  rootTiler <- gets @Tiler Fix
  unless (isJust $ isController rootTiler) $
    modify @Tiler $
      fromMaybe (error "e")
        . coerce
          ( para $ \case
              InputController _ t -> t >>= snd
              t -> map Fix $ case asum $ map (isController . fst) t of
                Just makeIC -> Just . makeIC . map Fix . reduce $ map snd t
                Nothing -> reduce $ map snd t
          )
  where
    isController :: SubTiler -> Maybe (Maybe SubTiler -> Tiler)
    isController (Fix (InputControllerF bords _)) =
      Just $ InputController bords
    isController _ = Nothing

-- | A smart zoomer which moves the monitor to wherever the input controller is.
zoomMonitorToInput ::
  Member (State Tiler) m =>
  Eff m ()
zoomMonitorToInput = do
  loc <-
    gets @Tiler $
      fromMaybe (error "Lost Mon") . cata \case
        Monitor loc _ -> Just loc
        t -> asum t
  modify @Tiler $
    coerce . fromMaybe (error "Can't be empty") . cata \case
      InputController bords t ->
        Just . Monitor loc . Just . InputController bords $ join t
      Monitor _ childT -> join childT
      t -> coerce $ reduce t

-- | A smart zoomer which moves the input controller to wherever the monitor is.
zoomInputToMonitor ::
  Member (State Tiler) m =>
  Eff m ()
zoomInputToMonitor = do
  bords <-
    gets @Tiler $
      fromMaybe (error "Lost Mon") . cata \case
        InputController bords _ -> Just bords
        t -> asum t
  modify @Tiler $
    coerce . fromMaybe (error "Can't be empty") . cata \case
      Monitor loc t ->
        Just . InputController bords . Just . Monitor loc $ join t
      InputController _ child -> join child
      Monitor _ childT -> join childT
      t -> coerce $ reduce t

-- | Very similar to zoomOutInput.
zoomOutMonitor ::
  Member (State Tiler) m =>
  Eff m ()
zoomOutMonitor = do
  -- Don't want to zoom the monitor out of existence
  rootTiler <- gets @Tiler Fix
  unless (isJust $ isMonitor rootTiler) $
    modify @Tiler $
      fromMaybe (error "e")
        . coerce
          ( para $ \case
              Monitor _ t -> t >>= snd
              t -> map Fix $ case asum $ map (isMonitor . fst) t of
                Just makeIC -> Just . makeIC . map Fix . reduce $ map snd t
                Nothing -> reduce $ map snd t
          )
  where
    isMonitor :: SubTiler -> Maybe (Maybe SubTiler -> Tiler)
    isMonitor (Fix (Monitor loc _)) =
      Just $ Monitor loc
    isMonitor _ = Nothing

-- | changes a mode. For example, I usually configure the windows key to change
--  from Insert mode to Normal mode.
changeModeTo :: Members '[State Mode, EventFlags, State KeyStatus] m => Mode -> Eff m ()
changeModeTo newM = do
  -- Unbind the keys from the old mode and bind the ones for the new mode.
  currentMode <- get @Mode
  rebindKeys currentMode newM

  put @Mode newM

  -- Whatever key is on top of the KeyStatus stack should be New instead
  -- of Temp.
  modify @KeyStatus $ \case
    Temp NotMod oldKS oldMode kc ea -> New oldKS oldMode kc ea
    ks -> ks

-- | Make a change to a Many Tiler if it comes after the InputController.
changeMany ::
  Member (State Tiler) m =>
  (ManyHolder SubTiler -> ManyHolder SubTiler) ->
  Eff m ()
changeMany f =
  modify @Tiler $
    applyInput $ map \case
      Many mh mods -> Many (f mh) mods
      t -> t

changeIndex :: Int -> ManyHolder SubTiler -> ManyHolder SubTiler
changeIndex changeTo mh =
  if foldFl mh length >= changeTo
    then withFl' mh $ focusVIndex (changeTo - 1)
    else mh

-- | Same as above but either adds or subtract one from the current index.
moveDir :: Direction -> ManyHolder SubTiler -> ManyHolder SubTiler
moveDir dir mh = withFl' mh $ focusDir dir

moveToLoc :: Int -> ManyHolder SubTiler -> ManyHolder SubTiler
moveToLoc toLoc mh = withFl' mh $ visualFIndex 0 (toLoc - 1)

changeMods ::
  Member (State Tiler) m =>
  ManyMods ->
  Eff m ()
changeMods newMod =
  modify @Tiler $
    applyInput $ map \case
      Many mh _ -> Many mh newMod
      t -> t

-- | Move the input controller to create a new, empty item.
makeEmptySpot ::
  Member (State Tiler) m =>
  Eff m ()
makeEmptySpot =
  modify @Tiler $ \root ->
    let newInput = flip InputController Nothing $ getIC root
     in onInput (coerce $ maybe root (makeEmptySpot' newInput root)) root
  where
    makeEmptySpot' :: SubTiler -> Tiler -> Tiler -> Tiler
    makeEmptySpot' newInput root t = case t of
      Many (Horiz fl) mods ->
        -- TODO I don't like the fromMaybe
        fromMaybe (error "We know it's not just an IC") $
          removeIC $
            applyInput
              ( \_ ->
                  let ogSize = fromIntegral $ length fl
                      newSize = ogSize + 1
                      growPercent = ogSize / newSize

                      newFl = map (\(Sized s a) -> Sized (s * growPercent) a) fl
                      newestFl = push Back Focused (Sized (1 / newSize) newInput) newFl
                   in Just $ Many (Horiz newestFl) mods
              )
              root
      Many (Floating fl) mods ->
        fromMaybe (error "We know it's not just an IC") $
          removeIC $
            applyInput
              ( \_ ->
                  Just $ Many (Floating (push Back Focused (WithRect (Rect 0 0 500 500) newInput) fl)) mods
              )
              root
      _ -> root

    removeIC = cata $ \case
      InputController _ (Just t@(Just (Many _ _))) -> t
      t -> reduce $ coerce t

    getIC =
      fromMaybe (error "Lost IC") . cata \case
        InputController bords _ -> Just bords
        t -> asum t

-- | Move a tiler from the tree into a stack. Later, we can push
--  from the stack back into the tree. This function accomplishes
--  something similar to minimization.
popTiler ::
  Members (States '[Tiler, [SubTiler]]) m =>
  Eff m ()
popTiler = do
  root <- get @Tiler

  sequence_ $ onInput (map (modify @[SubTiler] . (:))) root
  modify @Tiler $ applyInput $ const Nothing

-- | Move a tiler from the stack into the tree. Should be the inverse
--  of popTiler.
pushTiler ::
  Members (States '[Tiler, [SubTiler]]) m =>
  Eff m ()
pushTiler = do
  popped <- get @[SubTiler]

  case popped of
    (uncleanT : ts) -> do
      put @[SubTiler] ts
      case cleanTiler $ coerce uncleanT of
        Just t ->
          -- TODO These coercions are probably a sign that I should change something
          modify @Tiler $ applyInput $ coerce $ \tiler -> map (add t) tiler <|> Just (coerce t)
        Nothing -> return ()
    [] -> return ()
  where
    cleanTiler :: Tiler -> Maybe SubTiler
    cleanTiler = cata $ \case
      InputControllerOrMonitor _ t -> join t
      t -> coerce $ reduce t

-- | Insert a tiler after the Input Controller.
--  Note that this is not called when a new window is created.
--  That would be the pushOrAdd function.
insertTiler ::
  Member (State Tiler) m =>
  Eff m ()
insertTiler =
  modify @Tiler $ applyInput (map toTiler)
  where
    toTiler focused = Many (Horiz $ makeFL (Sized 1 focused :| []) 0) NoMods

toggleDocks ::
  Member (State DockState) m =>
  Eff m ()
toggleDocks =
  modify @DockState \d -> if d == Visible then Hidden else Visible

-- | Kill the active window
killActive ::
  Members '[State Tiler, State Tiler, GlobalX, Log LogData, State ShouldRedraw] m =>
  Eff m ()
killActive = do
  root <- get @Tiler

  -- We use the Beam data type to get the window at the end of
  -- the tree if it exists.
  let window = hylo getEnd makeList root
  -- TODO This seems convoluted and should be simplified...
  l <- case window of
    Just (parent, child) -> do
      -- Kill the actual window. If it supports the ICCCM spec, send it
      -- a nice message. Otherwise, disconnect the client application.
      shouldKill <- kill False child

      log $ LD "KillActive" $ show child <> " and " <> show parent <> " and got " <> show shouldKill <> "\n"
      -- If we had to disconnect the client, we won't (TODO is that true) get a Destroyed window event.
      -- This does what Destroy window would do.
      return $ map (,parent) shouldKill
    Nothing -> return Nothing
  case l of
    Nothing -> put @ShouldRedraw Nothing
    Just (killed, parent) -> do
      _ <- kill True parent
      modify @Tiler $ ripOut killed
  where
    makeList (Wrap (ParentChild window w' _)) = EndF $ Just (window, w')
    makeList (InputControllerOrMonitor _ (Just t)) = ContinueF $ coerce t
    makeList (InputControllerOrMonitor _ Nothing) = EndF Nothing
    makeList t = ContinueF (coerce $ getFocused t)

changeActiveScreen :: Members '[State ActiveScreen, Input [XineramaScreenInfo]] m => Direction -> Eff m ()
changeActiveScreen d = do
  active <- gets @ActiveScreen fromIntegral
  screens <- input @[XineramaScreenInfo]
  let sorted = sortOn (\(XineramaScreenInfo _ x y _ _) -> (y, x)) screens
  let (_, XineramaScreenInfo newId _ _ _ _) =
        fromMaybe (undefined, XineramaScreenInfo active 0 0 0 0) $ case d of
          Front -> find (\(XineramaScreenInfo i _ _ _ _, _) -> i == active) $ zip (fromJust $ tailMay sorted) sorted
          Back -> find (\(XineramaScreenInfo i _ _ _ _, _) -> i == active) $ zip sorted (fromJust $ tailMay sorted)
  put @ActiveScreen $ fromIntegral newId
