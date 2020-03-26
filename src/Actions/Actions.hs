{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}


{-|
   In the config file, users can specify actions to perform on keypresses or
   mode changes. This file contains the functions and types used to implement
   those actions. Each function roughly corresponds to a config action although
   a few are made generic so that they can implement multiple actions.
-}

module Actions.Actions (module Actions.Actions, module Actions.ActionTypes) where

import           Standard
import           Base.DoAll
import           Polysemy
import           Polysemy.State
import           Data.Either                    ( )
import           Tiler.Tiler
import           FocusList
import Actions.ActionTypes

-- * Zooming
-- $What is it?
-- Zooming is one of the most important concepts in Xest. The WM would be
-- unusable without it.
--
-- Note that I am still finding what primitive set of operations create the right mix of power and
-- intuition. If you have any ideas, definitely let me know.

-- | Helper type for the zoomDirInputSkip function. It would be cool if I could
-- put this in the zoomDirInputSkip where block...
type DeferedPath = DeferedListF Tiler Tiler

-- | This "smart" zoom function skips past the useless Tilers that would only slow you down.
-- Instead of zooming in or out one layer at a time, this function skips over the Tilers you probably
-- don't want to stop at. Note the use of probably. You should still bind the non smart versions as backup
-- in case you really do want to zoom into something boring.
zoomDirInputSkip
  :: Members (States '[Tiler]) r
  => (forall a . DeferedList a -> [a]) -- ^ Generally used to decide which side of the deffered list we're interested in
  -> Sem r () -- ^ The action to perform for every interesting Tiler in our path
  -> Sem r ()
zoomDirInputSkip trans action = do
  root <- get @Tiler

  replicateM_
    ( numToSkip -- Turns a list of Tilers into a number of things to skip
    . trans -- transforms our deffered list into a real one
    $ getPath root) -- Creates a deffered list based on the current focussed path

    action -- The second parameter to replicateM_

 where
    -- Turns a tree into a path of focused elements.
    -- The InputController marks where the path might want to be split
  getPath
    :: Tiler
    -> DeferedList Tiler
  getPath = apo $ \case
    InputController t ->
      DActiveF $ maybe (Left DNil) (Right . unfix) t
    mt@(Monitor t) -> maybe DNilF (DConsF mt . Right . unfix) t
    Wrap _ -> DNilF
    t                -> DConsF t . Right . unfix $ getFocused t

  -- The number of jumps to make.
  numToSkip = cata $ \case 
    Nil                    -> 1
    Cons (Many _ _) _ -> 1
    Cons (Monitor   _) i -> i + 1
    Cons _             i -> i + 1

-- | Zooms inwards, AKA away from the root. This function operates on
-- the Input Controller, not the Monitor. This means that although the layout
-- usually won't change, you will change the Tiler that will receive actions
-- and events.
zoomInInput
  :: Member (State Tiler) r
  => Sem r ()
zoomInInput =
  modify $ unfix . cata \case
    t@(InputController (Just (Wrap _))) -> Fix t
    InputController (Just (Monitor Nothing)) -> Monitor . Just $ InputController Nothing
    InputController (Just (Fix t)) -> Fix $ modFocused (Fix . InputController . Just) t
    t -> Fix t


-- |Nearly identical to zooming in the input controller. The only
-- differene is the Monitor needs to stay behind the input controller.
zoomInMonitor
  :: Member (State Tiler) r
  => Sem r ()
zoomInMonitor =
  modify @Tiler $ unfix . cata \case
    t@(Monitor (Just (Fix (Wrap _)))) -> Fix (t :: Tiler)
    Monitor (Just (Fix t)) -> Fix $ modFocused (Fix . Monitor . Just) t
    t -> Fix t


-- |Move the input controller towards the root
-- This is complicated if the monitor is right behind it
zoomOutInput
  :: Member (State Tiler) r
  => Sem r ()
zoomOutInput =
  -- The unless guards against zooming the controller out of existence
  unlessM (isController <$> gets @Tiler Fix)
    $ modify @Tiler
    $ fromMaybe (error "e")
    . coerce (para $ \case
        InputController t -> t >>= snd
        t -> fmap Fix $ if any (isController . fst) t
                           then Just . InputController . fmap Fix . reduce $ fmap snd t
                           else reduce $ fmap snd t)

 where
  -- Is something the controller? Instead of returning a boolean, we return
  -- a function used to construct a controller.
  isController (Fix (InputController _)) = True
  isController _ = False

-- |A smart zoomer which moves the monitor to wherever the input controller is.
zoomMonitorToInput
  :: Member (State Tiler) r
  =>  Sem r ()
zoomMonitorToInput =
  modify $ coerce . fromMaybe (error "Can't be empty") . cata (\case
    InputController t ->
      Just . Monitor . Just . InputController  $ join t
    Monitor childT -> join childT
    t -> coerce $ reduce t)

-- |A smart zoomer which moves the monitor to wherever the input controller is.
zoomInputToMonitor
  :: Member (State Tiler) r
  =>  Sem r ()
zoomInputToMonitor =
  modify $ coerce . fromMaybe (error "Can't be empty") . cata (\case
    Monitor t ->
      Just . Monitor . Just . InputController $ join t
    InputController childT -> join childT
    t -> coerce $ reduce t)

-- |Very similar to zoomOutInput but less confusing to implement.
zoomOutMonitor
  :: Member (State Tiler) r
  => Sem r ()
zoomOutMonitor =
  -- Don't want to zoom the monitor out of existence
  unlessM (isMonitor . Fix <$> get @Tiler)
    $ modify
    $ fromMaybe (error "e")
    . coerce (para $ \case
        Monitor t -> t >>= snd
        t -> fmap Fix $ if any (isMonitor . fst) t
                           then Just . Monitor . fmap Fix . reduce $ fmap snd t
                           else reduce $ fmap snd t)

  where isMonitor (Fix (Monitor _)) = True
        isMonitor _           = False

-- |changes a mode. For example, I have the windows key configured to change
-- from Insert mode to Normal mode.
changeModeTo :: Members '[State Mode, EventFlags, State KeyStatus] r => Mode -> Sem r ()
changeModeTo newM = do
  -- Unbind the keys from the old mode and bind the ones for the new mode.
  currentMode  <- get
  rebindKeys currentMode newM

  -- If this mode supports mouse actions, also capture the mouse.
  -- This is needed because while we've captured the mouse, no one else can
  -- use it.
  selectButtons newM
  put newM

  -- Whatever key is on top of the KeyStatus stack should be New instead
  -- of Temp.
  modify @KeyStatus $ \case
    Temp NotMod oldKS oldMode kc ea -> New oldKS oldMode kc ea
    ks -> ks

-- |Make a change to a Many Tiler if it comes after the InputController.
changeMany
  :: Member (State Tiler) r
  => (ManyHolder SubTiler -> ManyHolder SubTiler)
  -> Sem r ()
changeMany f =
  modify $ applyInput $ fmap \case
    Many mh mods -> Many (f mh) mods
    t -> t

changeIndex :: Int -> ManyHolder SubTiler -> ManyHolder SubTiler
changeIndex changeTo mh =
  if foldFl mh flLength >= changeTo
     then withFl' mh $ focusVIndex (changeTo - 1)
     else mh

-- |Same as above but either adds or subtract one from the current index.
moveDir :: Direction -> ManyHolder SubTiler -> ManyHolder SubTiler
moveDir dir mh = withFl' mh $ focusDir dir

moveToFront :: ManyHolder SubTiler -> ManyHolder SubTiler
moveToFront mh = withFl' mh $ visualFIndex 0

changeMods :: Member (State Tiler) r
           => ManyMods
           -> Sem r ()
changeMods newMod =
  modify $ applyInput $ fmap \case
    Many mh _ -> Many mh newMod
    t -> t

-- |Move the input controller to create a new, empty item.
makeEmptySpot
  :: Member (State Tiler) r
  => Sem r ()
makeEmptySpot =
  modify @Tiler $ \root -> onInput (coerce $ maybe root (makeEmptySpot' root)) root
 where
  makeEmptySpot' :: Tiler -> Tiler -> Tiler
  makeEmptySpot' root t = case t of
    Many (Horiz _) _ ->
      -- TODO I don't like the fromMaybe
      fromMaybe (error "We know it's not just an IC") $ removeIC
        $ applyInput (\(Just (Many (Horiz fl) mods)) ->
          let ogSize = fromIntegral $ flLength fl
              newSize = ogSize + 1
              growPercent = ogSize / newSize

              newFl = fmap (\(Sized s a) -> Sized (s * growPercent) a) fl
              newestFl = push Back Focused (Sized (1 / newSize) $ InputController Nothing) newFl
           in Just $ Many (Horiz newestFl) mods) root
    Many _ _ ->
      fromMaybe (error "We know it's not just an IC") $ removeIC
        $ applyInput (\(Just (Many mh mods)) ->
          Just $ Many (withFl' mh $ push Back Focused (point $ InputController Nothing)) mods) root
    _ -> root

  removeIC = cata $ \case 
    InputController (Just t@(Just (Many _ _))) -> t
    t -> reduce $ coerce t


-- |Move a tiler from the tree into a stack. Later, we can push
-- from the stack back into the tree. This function accomplishes
-- something similar to minimization.
popTiler
  :: Members (States '[Tiler, [SubTiler]]) r
  => Sem r ()
popTiler = do
  root <- get

  sequence_ $ onInput (fmap (modify @[SubTiler] . (:))) root
  modify $ applyInput $ const Nothing

-- |Move a tiler from the stack into the tree. Should be the inverse
-- of popTiler.
pushTiler
  :: Members (States '[Tiler, [SubTiler]]) r
  => Sem r ()
pushTiler = do
  popped <- get @[SubTiler]

  case popped of
    (uncleanT : ts) -> do
      put ts
      case cleanTiler $ coerce uncleanT of
        Just t ->
          -- TODO These coercions are probably a sign that I should change something
          modify $ applyInput $ coerce $ \tiler -> fmap (add t) tiler <|> Just (coerce t)
        Nothing -> return ()
    [] -> return ()
    where cleanTiler :: Tiler -> Maybe SubTiler
          cleanTiler = cata $ \case 
            InputControllerOrMonitor _ t -> join t
            t -> coerce $ reduce t

-- |Insert a tiler after the Input Controller.
-- Note that this is not called when a new window is created.
-- That would be the pushOrAdd function.
insertTiler
  :: Member (State Tiler) r
  => Sem r ()
insertTiler =
  modify @Tiler $ applyInput (fmap toTiler)

 where
  toTiler focused = Many (Horiz $ makeFL (NE (Sized 1 focused) []) 0) NoMods

-- |Kill the active window
killActive
  :: Members '[State Tiler, State Tiler, GlobalX, Log String, State (Maybe ())] r
  => Sem r ()
killActive = do
  root <- get @Tiler

  -- We use the Beam data type to get the window at the end of
  -- the tree if it exists.
  let window = extract $ ana @(Beam _) makeList root
  -- TODO This seems convoluted and should be simplified...
  l <- case window of
    Just (parent, child) -> do
      -- Kill the actual window. If it supports the ICCCM spec, send it
      -- a nice message. Otherwise, disconnect the client application.
      shouldKill <- kill False child

      log $ "[KillActive] " ++ show child ++ " and " ++ show parent ++ " and got " ++ show shouldKill ++ "\n"
      -- If we had to disconnect the client, we won't (TODO is that true) get a Destroyed window event.
      -- This does what Destroy window would do.
      return $ fmap (, parent) shouldKill
    Nothing -> return Nothing
  case l of
    Nothing               -> put Nothing
    Just (killed, parent) -> do
      _ <- kill True parent
      modify $ ripOut killed
 where
  makeList (Wrap (ParentChild window w')) = EndF $ Just (window, w')
  makeList (InputControllerOrMonitor _ (Just t)  ) = ContinueF $ coerce t
  makeList (InputControllerOrMonitor _ Nothing   ) = EndF Nothing
  makeList t                              = ContinueF (coerce $ getFocused t)
