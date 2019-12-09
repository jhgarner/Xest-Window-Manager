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

  replicateM_
    ( cata numToSkip -- Turns a list of Tilers into a number of things to skip
    . trans -- transforms our deffered list into a real one
    $ apo getPath root) -- Creates a deffered list based on the current focussed path

    action -- The second parameter to replicateM_

 where
    -- Turns a tree into a path of focused elements.
    -- The InputController marks where the path might want to be split
  getPath
    :: Tiler (Fix Tiler)
    -> DeferedListF (Tiler (Fix Tiler)) (Either (DeferedList (Tiler (Fix Tiler))) (Tiler (Fix Tiler)))
  getPath it@(InputController t) =
    DActiveF $ maybe (Left DNil) (Right . unfix) t
  getPath mt@(Monitor t) = maybe DNilF (DConsF mt . Right . unfix) t
  getPath (   Wrap _     ) = DNilF
  getPath t                = DConsF t . Right $ unfix (getFocused t)

  -- The number of jumps to make.
  numToSkip Nil                    = 0
  numToSkip (Cons (Horiz     _) _) = 1
  numToSkip (Cons (Floating  _) _) = 1
  numToSkip (Cons (FocusFull _) _) = 1
  numToSkip (Cons (Monitor   _) j) = j
  numToSkip (Cons _             i) = i + 1

-- | Zooms inwards, AKA away from the root. This function operates on
-- the Input Controller, not the Monitor. This means that although the layout
-- usually won't change, you will change the Tiler that will receive actions
-- and events.
zoomInInput
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
zoomInInput =
  modify $ cata reorder

 where
  -- Since a Wrap doesn't hold anything, we'll lose the InputController
  -- if we zoom in.
  reorder t@(InputController (Just (Fix (Wrap _)))) = Fix t
  -- Same for Nothing
  reorder t@(InputController Nothing              ) = Fix t
  -- Now we can safely zoom in assuming it's the right monitor
  reorder ic@(InputController (Just (Fix t))) =
    Fix $ modFocused (Fix . InputController . Just) t
  -- Everything else remains the same
  reorder t = Fix t

-- |Nearly identical to zooming in the input controller. The only
-- differene is the Monitor needs to stay behind the input controller.
zoomInMonitor
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
zoomInMonitor =
  modify $ cata reorder

 where
  -- In this case, the monitor is already zoomed in as far as it can go.
  reorder m@(Monitor (Just (Fix (InputController t)))) =  Fix m

  -- Don't zoom in if there's nothing beyond the monitor.
  reorder t@(Monitor Nothing) = Fix t
  -- If this is the monitor we're looking for, zoom in.
  reorder m@(Monitor (Just (Fix t))) =
    Fix $ modFocused (Fix . Monitor . Just) t

  reorder t = Fix t

-- |Move the input controller towards the root
-- This is complicated if the monitor is right behind it
zoomOutInput
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
zoomOutInput =
  -- The unless guards against zooming the controller out of existence
  unlessM (isJust . isController <$> get @(Fix Tiler))
    $ modify
    $ fromMaybe (error "e")
    . para reorder

 where
  -- This function makes me think I should figure out better semantices for
  -- zooming.
  reorder :: Tiler (Fix Tiler, Maybe (Fix Tiler)) -> Maybe (Fix Tiler)
  -- The controller should always be in front of the Monitor
  reorder m@(Monitor (Just (Fix (InputController _), t))) = t

  -- The input disappears and will hopefully be added back later
  reorder (InputController t) = t >>= snd

  -- If the tiler held the controller, add back the controller around it
  reorder t = fmap Fix $ case asum $ isController . fst <$> t of
    Just controller -> Just . controller . fmap Fix . reduce $ fmap snd t
    Nothing         -> reduce $ fmap snd t

  -- Is something the controller? Instead of returning a boolean, we return
  -- a function used to construct a controller.
  isController (Fix (InputController _)) = Just InputController
  isController (Fix (Monitor (Just (Fix (InputController _))))) =
    Just $ Monitor . Just . Fix . InputController
  isController _ = Nothing

-- |A smart zoomer which moves the monitor to wherever the input controller is.
zoomMonitorToInput
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
zoomMonitorToInput =
  modify $ \root -> fromMaybe root (cata insertIt root)

 where
  insertIt :: Tiler (Maybe (Fix Tiler)) -> Maybe (Fix Tiler)
  insertIt (InputController t) =
    Just . Fix . Monitor . Just . Fix . InputController  $ join t
  insertIt (Monitor childT) = join childT
  insertIt t = Fix <$> reduce t

-- |Very similar to zoomOutInput but less confusing to implement.
zoomOutMonitor
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
zoomOutMonitor =
  -- Don't want to zoom the monitor out of existence
  unlessM (isMonitor <$> get @(Fix Tiler))
    $ modify
    $ fromMaybe (error "e")
    . para reorder
 where
  reorder :: Tiler (Fix Tiler, Maybe (Fix Tiler)) -> Maybe (Fix Tiler)
  reorder (Monitor t) = t >>= snd
  reorder t = if any (isMonitor . fst) t
    then Just . Fix . Monitor . fmap Fix . reduce $ fmap snd t
    else fmap Fix . reduce $ fmap snd t

  isMonitor (Fix (Monitor _)) = True
  isMonitor _                 = False

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
changeIndex changeTo =
  modify $ cata (applyInput $ fmap changer)

 where
  changer (Horiz fl) = Horiz
    $ if flLength fl >= changeTo then focusVIndex (changeTo - 1) fl else fl
  changer t = t

-- |Same as above but either adds or subtract one from the current index.
moveDir
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Direction -> Sem r ()
moveDir dir =
  modify $ cata (applyInput $ fmap changer)

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

  sequence_ $ onInput (fmap (modify @[Fix Tiler] . (:))) root
  modify $ cata (applyInput $ const Nothing)

-- |Move a tiler from the stack into the tree. Should be the inverse
-- of popTiler.
pushTiler
  :: Members (States '[Fix Tiler, [Fix Tiler]]) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
pushTiler = do
  popped <- get @[Fix Tiler]

  case popped of
    (t : ts) -> do
      put ts
      modify $ cata (applyInput $ Just . pushOrAdd t)
    [] -> return ()

-- |Insert a tiler after the Input Controller.
-- Note that this is not called when a new window is created.
-- That would be the pushOrAdd function.
insertTiler
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Insertable -> Sem r ()
insertTiler t =
  modify @(Fix Tiler) . cata $ applyInput (fmap toTiler)

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
  modify @(Fix Tiler) (\root -> (onInput (maybe root (makeSpecial root))) root)
  newT <- get
  traceShowM newT
 where
  makeSpecial :: Fix Tiler -> Fix Tiler -> Fix Tiler
  makeSpecial (Fix root) (Fix t) = case t of
    Floating (NE l ls) ->
      applyInput (\_ -> Just $ Floating $ NE (mkBottom l) $ fmap mkTop ls) root
    Horiz _ ->
      -- TODO rewrite this in a less bad way.
      -- maybe (error "We know it's not just an IC") (fromMaybe (error "Yikes") . fst . moveToIC i) $ removeIC i
      maybe (error "We know it's not just an IC") (id) $ removeIC
        $ (\l -> trace ("\n\nTT\n" ++ show l ++ "\n") l) $ cata (applyInput (\(Just ((Horiz fl))) -> Just $ Horiz $ push Back Focused (Sized 0 . Fix $ InputController Nothing) fl)) $ Fix root
    _                  -> Fix root

  mkTop t@(Top    _) = t
  mkTop (  Bottom t) = Top (RRect 0 0 0.2 0.2, t)
  mkBottom = Bottom . extract

  removeIC = cata $ \case 
    InputController (Just t@(Just (Fix (Horiz _)))) -> t
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
  makeList (Fix (InputControllerOrMonitor _ (Just t)  )) = ContinueF t
  makeList (Fix (InputControllerOrMonitor _ Nothing   )) = EndF Nothing
  makeList (Fix t                             ) = ContinueF (getFocused t)
