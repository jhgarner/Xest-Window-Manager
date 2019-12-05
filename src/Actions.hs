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

-- TODO can I move the Action type in here? Can I do so without creating
-- a recursive mess?
module Actions where

import           Standard
import           Base
import           Polysemy
import           Polysemy.State
import           Types
import           Data.Either                    ( )
import           Tiler
import           Core
import           FocusList

-- * Zooming
-- $What is it?
-- Zooming is one of the most important concepts in Xest. The WM would be
-- unusable without it.
--
-- Note that I am still finding what primitive set of operations create the right mix of power and
-- intuition. If you have any ideas, definitely let me know.

-- | Helper type for the zoomDirInputSkip function. It would be cool if I could
-- put this in the zoomDirInputSkip where block...
type DeferedPath = DeferedListF (Tiler (Fix Tiler)) (Tiler (Fix Tiler))

-- | This "smart" zoom function skips past the useless Tilers that would only slow you down.
-- Instead of zooming in or out one layer at a time, this function skips over the Tilers you probably
-- don't want to stop at. Note the use of probably. You should still bind the non smart versions as backup
-- in case you really do want to zoom into something boring.
zoomDirInputSkip
  :: Members (Inputs '[Pointer, Screens]) r
  => Members (States '[Fix Tiler, Tiler (Fix Tiler)]) r
  => (forall a . DeferedList a -> [a]) -- ^ Generally used to decide which side of the deffered list we're interested in
  -> Sem r () -- ^ The action to perform for every interesting Tiler in our path
  -> Sem r ()
zoomDirInputSkip trans action = do
  root <- get @(Tiler (Fix Tiler))
  i    <- maybe 0 fst <$> getCurrentScreen

  replicateM_
    ( cata (numToSkip i) -- Turns a list of Tilers into a number of things to skip
    . trans -- transforms our deffered list into a real one
    $ apo (getPath i) root) -- Creates a deffered list based on the current focussed path

    action -- The second parameter to replicateM_

 where
    -- Turns a tree into a path of focused elements.
    -- The InputController marks where the path might want to be split
  getPath
    :: Int
    -> Tiler (Fix Tiler)
    -> DeferedListF (Tiler (Fix Tiler)) (Either (DeferedList (Tiler (Fix Tiler))) (Tiler (Fix Tiler)))
  getPath i it@(InputController i' t)
    | i == i'   = DActiveF $ maybe (Left DNil) (Right . unfix) t
    -- TODO I really don't like having to duplicate code like this all of the time
    -- If i != i' then this should be treated just like any other Tiler
    | otherwise = maybe DNilF (DConsF it . Right . unfix) t
  getPath _ mt@(Monitor _ t) = maybe DNilF (DConsF mt . Right . unfix) t
  getPath _ (   Wrap _     ) = DNilF
  getPath _ t                = DConsF t . Right $ unfix (getFocused t)

  -- The number of jumps to make.
  numToSkip _ Nil                    = 0
  numToSkip _ (Cons (Horiz     _) _) = 1
  numToSkip _ (Cons (Floating  _) _) = 1
  numToSkip _ (Cons (FocusFull _) _) = 1
  numToSkip i (Cons (Monitor  i' _) j)
    | i == i' = j
    | otherwise = j + 1
  numToSkip _ (Cons _             i) = i + 1

-- | Zooms inwards, AKA away from the root. This function operates on
-- the Input Controller, not the Monitor. This means that although the layout
-- usually won't change, you will change the Tiler that will receive actions
-- and events.
zoomInInput
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
zoomInInput = do
  i <- maybe 0 fst <$> getCurrentScreen
  modify $ cata (reorder i)

 where
  -- Since a Wrap doesn't hold anything, we'll lose the InputController
  -- if we zoom in.
  reorder _ t@(InputController _ (Just (Fix (Wrap _)))) = Fix t
  -- Same for Nothing
  reorder _ t@(InputController _ Nothing              ) = Fix t
  -- Now we can safely zoom in assuming it's the right monitor
  reorder i ic@(InputController i' (Just (Fix t)))
    | i == i'   = Fix $ modFocused (Fix . InputController i' . Just) t
    | otherwise = Fix ic
  -- Everything else remains the same
  reorder _ t = Fix t

-- |Nearly identical to zooming in the input controller. The only
-- differene is the Monitor needs to stay behind the input controller.
zoomInMonitor
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
zoomInMonitor = do
  i <- maybe 0 fst <$> getCurrentScreen
  modify $ cata (reorder i)

 where
  reorder i m@(Monitor i' (Just (Fix (InputController i'' t))))
    -- In this case, the monitor is already zoomed in as far as it can go.
    | i'' == i = Fix m
    -- Somewhat annoyingly, this is identical to finding a monitor and any
    -- other tiler. Yet another example of the TODO from above.
    | i == i'  = Fix . InputController i'' . Just . Fix $ Monitor i' t
  -- Don't zoom in if there's nothing beyond the monitor.
  reorder _ t@(Monitor _ Nothing) = Fix t
  -- If this is the monitor we're looking for, zoom in.
  reorder i m@(Monitor i' (Just (Fix t)))
    | i == i'   = Fix $ modFocused (Fix . Monitor i' . Just) t
    | otherwise = Fix m

  reorder _ t = Fix t

-- |Move the input controller towards the root
-- This is complicated if the monitor is right behind it
zoomOutInput
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
zoomOutInput = do
  i <- maybe 0 fst <$> getCurrentScreen

  -- The unless guards against zooming the controller out of existence
  unlessM (isJust . isController i <$> get @(Fix Tiler))
    $ modify
    $ fromMaybe (error "e")
    . para (reorder i)

 where
  -- This function makes me think I should figure out better semantices for
  -- zooming.
  reorder :: Int -> Tiler (Fix Tiler, Maybe (Fix Tiler)) -> Maybe (Fix Tiler)
  -- The controller should always be in front of the Monitor
  reorder i m@(Monitor i' (Just (Fix (InputController i'' _), t)))
    |
    -- | i'' == i' && i' == i = Just . Fix $ InputController i'' t
      i'' == i' && i' == i = t
    | i == i'' = Just . Fix . InputController i'' . Just . Fix $ Monitor i' t
    | otherwise = fmap Fix . reduce $ fmap snd m

  -- The input disappears and will hopefully be added back later
  reorder i (InputController i' t)
    | i == i' = t >>= snd
    | otherwise = case t >>= isController i . fst of
      Just controller ->
        Just . Fix . controller . Just . Fix . InputController i' $ t >>= snd
      Nothing -> Just . Fix . InputController i' $ t >>= snd

  -- If the tiler held the controller, add back the controller around it
  reorder i t = fmap Fix $ case asum $ isController i . fst <$> t of
    Just controller -> Just . controller . fmap Fix . reduce $ fmap snd t
    Nothing         -> reduce $ fmap snd t

  -- Is something the controller? Instead of returning a boolean, we return
  -- a function used to construct a controller.
  isController i (Fix (InputController i' _)) =
    if i == i' then Just $ InputController i' else Nothing
  isController i (Fix (Monitor i' (Just (Fix (InputController i'' _)))))
    | i == i' && i' == i''
    = Just $ Monitor i' . Just . Fix . InputController i''
    | otherwise = Nothing
  isController _ _ = Nothing

-- |A smart zoomer which moves the monitor to wherever the input controller is.
zoomMonitorToInput
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
zoomMonitorToInput = do
  i <- maybe 0 fst <$> getCurrentScreen
  modify $ \root -> fromMaybe root (cata (insertIt i) root)

 where
  insertIt :: Int -> Tiler (Maybe (Fix Tiler)) -> Maybe (Fix Tiler)
  insertIt i (InputController i' t)
    | i == i' = Just . Fix . Monitor i . Just . Fix . InputController i' $ join
      t
    | otherwise = Just . Fix . InputController i' $ join t
  insertIt i (Monitor i' childT)
    | i == i'   = join childT
    | otherwise = Just . Fix . Monitor i' $ join childT
  insertIt _ t = Fix <$> reduce t

-- |Very similar to zoomOutInput but less confusing to implement.
zoomOutMonitor
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
zoomOutMonitor = do
  i <- maybe 0 fst <$> getCurrentScreen

  -- Don't want to zoom the monitor out of existence
  unlessM (isMonitor i <$> get @(Fix Tiler))
    $ modify
    $ fromMaybe (error "e")
    . para (reorder i)
 where
  reorder :: Int -> Tiler (Fix Tiler, Maybe (Fix Tiler)) -> Maybe (Fix Tiler)
  reorder i (Monitor i' t) =
    if i == i' then t >>= snd else Just . Fix . Monitor i' $ t >>= snd
  reorder i t = if any (isMonitor i . fst) t
    then Just . Fix . Monitor i . fmap Fix . reduce $ fmap snd t
    else fmap Fix . reduce $ fmap snd t

  isMonitor i (Fix (Monitor i' _)) = i == i'
  isMonitor _ _                    = False

-- |changes a mode. For example, I have the windows key configured to change
-- from Insert mode to Normal mode.
changeModeTo :: Members '[State Mode, EventFlags] r => Mode -> Sem r Actions
changeModeTo newM = do
  eActions <- gets @Mode exitActions
  oldMode  <- get
  -- Unbind the keys from the old mode and bind the ones for the new mode.
  rebindKeys oldMode newM
  -- If this mode supports mouse actions, also capture the mouse.
  -- This is needed because while we've captured the mouse, no one else can
  -- use it.
  selectButtons newM

  put newM

  --Combine the two lists of actions to be executed. Execute exit actions first.
  return $ eActions ++ introActions newM

-- |If the current Tiler can hold multiple children, change which one
-- is focused. Typically bound to the number keys.
changeIndex
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Int -> Sem r ()
changeIndex changeTo = do
  i <- maybe 0 fst <$> getCurrentScreen
  modify $ cata (applyInput i $ fmap changer)

 where
  changer (Horiz fl) = Horiz
    $ if flLength fl >= changeTo then focusVIndex (changeTo - 1) fl else fl
  changer t = t

-- |Same as above but either adds or subtract one from the current index.
moveDir
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Direction -> Sem r ()
moveDir dir = do
  i <- maybe 0 fst <$> getCurrentScreen
  modify $ cata (applyInput i $ fmap changer)

 where
  changer (Horiz fl) = Horiz $ focusDir dir fl
  changer t          = t

-- |Move a tiler from the tree into a stack. Later, we can push
-- from the stack back into the tree. This function accomplishes
-- something similar to minimization.
popTiler
  :: Members (States '[Fix Tiler, [Fix Tiler]]) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
popTiler = do
  root <- get
  i    <- maybe 0 fst <$> getCurrentScreen

  sequence_ $ onInput i (fmap (modify @[Fix Tiler] . (:))) root
  modify $ cata (applyInput i $ const Nothing)

-- |Move a tiler from the stack into the tree. Should be the inverse
-- of popTiler.
pushTiler
  :: Members (States '[Fix Tiler, [Fix Tiler]]) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
pushTiler = do
  popped <- get @[Fix Tiler]
  i      <- maybe 0 fst <$> getCurrentScreen

  case popped of
    (t : ts) -> do
      put ts
      modify $ cata (applyInput i $ Just . pushOrAdd t)
    [] -> return ()

-- |Insert a tiler after the Input Controller.
-- Note that this is not called when a new window is created.
-- That would be the pushOrAdd function.
insertTiler
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Insertable -> Sem r ()
insertTiler t = do
  i <- maybe 0 fst <$> getCurrentScreen
  modify @(Fix Tiler) . cata $ applyInput i (fmap toTiler)

 where
  toTiler focused = case t of
    Horizontal -> Horiz $ makeFL (NE (Sized 0 $ Fix focused) []) 0
    Rotate     -> Reflect $ Fix focused
    FullScreen -> FocusFull $ Fix focused
    Hovering   -> Floating $ NE (Bottom $ Fix focused) []

-- |Perform some special action based on the focused tiler
doSpecial
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
doSpecial = do
  i <- maybe 0 fst <$> getCurrentScreen
  modify @(Fix Tiler) $ (\root -> (onInput i (maybe root (makeSpecial i root))) root)
  newT <- get
  traceShowM newT
 where
  makeSpecial :: Int -> Fix Tiler -> Fix Tiler -> Fix Tiler
  makeSpecial i (Fix root) (Fix t) = case t of
    Floating (NE l ls) ->
      applyInput i (\_ -> Just $ Floating $ NE (mkBottom l) $ fmap mkTop ls) root
    Horiz _ ->
      -- TODO rewrite this in a less bad way.
      -- maybe (error "We know it's not just an IC") (fromMaybe (error "Yikes") . fst . moveToIC i) $ removeIC i
      maybe (error "We know it's not just an IC") (id) $ removeIC i
        $ (\l -> trace ("\n\nTT\n" ++ show l ++ "\n") l) $ cata (applyInput i (\(Just ((Horiz fl))) -> Just $ Horiz $ push Back Focused (Sized 0 . Fix $ InputController i Nothing) fl)) $ Fix root
    _                  -> Fix root

  mkTop t@(Top    _) = t
  mkTop (  Bottom t) = Top (RRect 0 0 0.2 0.2, t)
  mkBottom = Bottom . extract

  removeIC i = cata $ \case 
    ic@(InputController i' (Just t@(Just (Fix (Horiz _)))))
      | i == i' -> t
      | otherwise -> Fix <$> reduce ic
    t -> Fix <$> reduce t

-- |Kill the active window
killActive
  :: Members '[State (Fix Tiler), State (Tiler (Fix Tiler)), GlobalX, Executor, State (Maybe ())] r
  => Sem r ()
killActive = do
  root <- get @(Fix Tiler)

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
  makeList (Fix (Wrap (ChildParent window w'))) = EndF $ Just (window, w')
  makeList (Fix (InputController _ (Just t)  )) = ContinueF t
  makeList (Fix (InputController _ Nothing   )) = EndF Nothing
  makeList (Fix (Monitor         _ (Just t)  )) = ContinueF t
  makeList (Fix (Monitor         _ Nothing   )) = EndF Nothing
  makeList (Fix t                             ) = ContinueF (getFocused t)
