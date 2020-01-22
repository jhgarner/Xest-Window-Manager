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
    Cons (Horiz     _) _ -> 1
    Cons (Floating  _) _ -> 1
    Cons (FocusFull _) _ -> 1
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
  modify $ fixable $ cata $ \case
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
  modify $ fixable $ cata $ \case
    t@(Monitor (Just (Fix (Wrap _)))) -> Fix t
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
    $ modify
    $ fromMaybe (error "e")
    . maybeFixable (para $ \case
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
    . maybeFixable (para $ \case
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


-- |If the current Tiler can hold multiple children, change which one
-- is focused. Typically bound to the number keys.
changeIndex
  :: Member (State Tiler) r
  => Int -> Sem r ()
changeIndex changeTo =
  modify $ applyInput $ fmap changer

 where
  changer (Horiz fl) = Horiz
    $ if flLength fl >= changeTo then focusVIndex (changeTo - 1) fl else fl
  changer t = t

-- |Same as above but either adds or subtract one from the current index.
moveDir
  :: Member (State Tiler) r
  => Direction -> Sem r ()
moveDir dir =
  modify $ applyInput $ fmap changer

 where
  changer (Horiz fl) = Horiz $ focusDir dir fl
  changer t          = t

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
    (t : ts) -> do
      put ts
      -- TODO These coercions are probably a sign that I should change something
      modify $ applyInput $ coerce $ \tiler -> fmap (add t) tiler <|> Just (coerce t)
    [] -> return ()

-- |Insert a tiler after the Input Controller.
-- Note that this is not called when a new window is created.
-- That would be the pushOrAdd function.
insertTiler
  :: Member (State Tiler) r
  => Insertable -> Sem r ()
insertTiler t =
  modify @Tiler $ applyInput (fmap toTiler)

 where
  toTiler focused = case t of
    Horizontal -> Horiz $ makeFL (NE (Sized 0 focused) []) 0
    Rotate     -> Reflect focused
    FullScreen -> FocusFull focused
    Hovering   -> Floating $ NE (Bottom focused) []

-- |Perform some special action based on the focused tiler
doSpecial
  :: Member (State Tiler) r
  => Sem r ()
doSpecial =
  modify @Tiler $ \root -> onInput (coerce $ maybe root (makeSpecial root)) root
 where
  makeSpecial :: Tiler -> Tiler -> Tiler
  makeSpecial root t = case t of
    Floating (NE l ls) ->
      applyInput (\_ -> Just $ Floating $ NE (mkBottom l) $ fmap mkTop ls) root
    Horiz _ ->
      -- TODO rewrite this in a less bad way.
      -- maybe (error "We know it's not just an IC") (fromMaybe (error "Yikes") . fst . moveToIC i) $ removeIC i
      fromMaybe (error "We know it's not just an IC") $ removeIC
        $ applyInput (\(Just (Horiz fl)) -> Just $ Horiz $ push Back Focused (Sized 0 . Fix $ InputController Nothing) fl) root
    _                  -> root

  mkTop t@(Top    _) = t
  mkTop (  Bottom t) = Top (Rect 0 0 300 300, t)
  mkBottom = Bottom . extract

  removeIC = cata $ \case 
    InputController (Just t@(Just (Horiz _))) -> t
    t -> reduce $ coerce t

-- |Kill the active window
killActive
  :: Members '[State Tiler, State Tiler, GlobalX, Executor, State (Maybe ())] r
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

      printMe $ "Killing " ++ show child ++ " and " ++ show parent ++ " and got " ++ show shouldKill ++ "\n"
      -- If we had to disconnect the client, we won't (TODO is that true) get a Destroyed window event.
      -- This does what Destroy window would do.
      return $ fmap (, parent) shouldKill
    Nothing -> return Nothing
  printMe $ "about to do something... " ++ show l ++ "\n"
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
