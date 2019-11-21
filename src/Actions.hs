{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}


{-|
   Actions contains code to handle all of the things the user
   might want done on a key press. For example, moving a window,
   cutting/pasting, zooming, and others are all here.
-}
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


-- |Zoom in or out to the next interesting Tiler
zoomDirInputSkip
  :: Members (Inputs '[Pointer, Screens]) r
  => Members (States '[Fix Tiler, Tiler (Fix Tiler)]) r
  => (forall a . DeferedList a -> [a])
  -> Sem r ()
  -> Sem r ()
zoomDirInputSkip trans action = do
  root <- get @(Tiler (Fix Tiler))
  i    <- maybe 0 fst <$> getCurrentScreen

  replicateM_ (cata numToSkip . trans $ ana (getPath i) root) action

 where
    -- Turns a tree into a path of focused elements.
    -- The InputController is marked differently.
  getPath
    :: Int
    -> Tiler (Fix Tiler)
    -> DeferedListF (Tiler (Fix Tiler)) (Tiler (Fix Tiler))
  getPath i it@(InputController i' t)
    | i == i'   = maybe DNilF (DActiveF . unfix) t
    | otherwise = maybe DNilF (DConsF it . unfix) t
  getPath _ mt@(Monitor _ t) = maybe DNilF (DConsF mt . unfix) t
  getPath _ (   Wrap _     ) = DNilF
  getPath _ t                = DConsF t $ unfix (getFocused t)

  -- The distance to jump
  numToSkip Nil                    = 0
  numToSkip (Cons (Horiz     _) _) = 1
  numToSkip (Cons (Floating  _) _) = 1
  numToSkip (Cons (FocusFull _) _) = 1
  numToSkip (Cons _             i) = i + 1

-- |Moves the input controller for a given screen away from the root.
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

-- The monitor needs to stay behind the Input Controller
-- when zooming in.
zoomInMonitor
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
zoomInMonitor = do
  i <- maybe 0 fst <$> getCurrentScreen
  modify $ cata (reorder i)

 where
  reorder i m@(Monitor i' (Just (Fix (InputController i'' t))))
    | i'' == i = Fix m
    | i == i'  = Fix . InputController i'' . Just . Fix $ Monitor i' t
  reorder _ t@(Monitor _ Nothing) = Fix t
  reorder i m@(Monitor i' (Just (Fix t)))
    | i == i'   = Fix $ modFocused (Fix . Monitor i' . Just) t
    | otherwise = Fix m

  reorder _ t = Fix t

-- Move the input controller towards the root
-- This is complicated if the monitor is right behind it
zoomOutInput
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
zoomOutInput = do
  i <- maybe 0 fst <$> getCurrentScreen

  unlessM (isJust . isController i <$> get @(Fix Tiler))
    $ modify
    $ fromMaybe (error "e")
    . para (reorder i)

 where
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

  isController i (Fix (InputController i' _)) =
    if i == i' then Just $ InputController i' else Nothing
  isController i (Fix (Monitor i' (Just (Fix (InputController i'' _)))))
    | i == i' && i' == i''
    = Just $ Monitor i' . Just . Fix . InputController i''
    | otherwise
    = Nothing
  isController _ _ = Nothing

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

changeModeTo :: Members '[State Mode, EventFlags] r => Mode -> Sem r Actions
changeModeTo newM = do
  eActions <- gets @Mode exitActions
  oldMode  <- get
  rebindKeys oldMode newM
  selectButtons newM
  put newM
  --Combine the two lists of actions to be executed. Execute exit actions first.
  return $ eActions ++ introActions newM

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

moveDir
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Direction -> Sem r ()
moveDir dir = do
  i <- maybe 0 fst <$> getCurrentScreen
  modify $ cata (applyInput i $ fmap changer)
 where
  changer (Horiz fl) = Horiz $ focusDir dir fl
  changer t          = t

-- Move a tiler from the tree into a stack
popTiler
  :: Members (States '[Fix Tiler, [Fix Tiler]]) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
popTiler = do
  root <- get
  i    <- maybe 0 fst <$> getCurrentScreen
  sequence_ $ onInput i (fmap (modify @[Fix Tiler] . (:))) root
  modify $ cata (applyInput i $ const Nothing)

-- Move a tiler from the stack into the tree
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

-- Insert a tiler after the Input Controller
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

-- Perform some special action based on the focused tiler
doSpecial
  :: Member (State (Fix Tiler)) r
  => Members (Inputs '[Pointer, Screens]) r => Sem r ()
doSpecial = do
  i <- maybe 0 fst <$> getCurrentScreen
  modify @(Fix Tiler) . cata $ applyInput i (fmap makeSpecial)
 where
  makeSpecial t = case t of
    Floating (NE l ls) -> Floating $ NE (mkBottom l) $ fmap mkTop ls
    _                  -> t
  mkTop t@(Top    _) = t
  mkTop (  Bottom t) = Top (RRect 0 0 0.2 0.2, t)
  mkBottom = Bottom . extract

-- Kill the active window
killActive
  :: Members '[State (Fix Tiler), State (Tiler (Fix Tiler)), GlobalX, Executor, State (Maybe ())] r
  => Sem r ()
killActive = do
  root <- get @(Fix Tiler)
  let window = extract $ ana @(Beam _) makeList root
  l <- case window of
    Just (parent, child) -> do
      trace ("\n=========== " ++ show child ++ "\n") return ()
      trace ("\n=========== " ++ show parent ++ "\n") return ()
      shouldKill <- kill False child
      trace ("\n ==============" ++ show shouldKill ++ "\n") return ()
      printMe $ "Killing " ++ show child ++ " and " ++ show parent ++ " and got " ++ show shouldKill ++ "\n"
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
