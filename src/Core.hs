{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Core where

import           ClassyPrelude
import           Control.Lens                   ( (.~)
                                                , view
                                                , set
                                                )
import           Control.Monad.State.Lazy       ( get
                                                , gets
                                                , modify
                                                )
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Window
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Atom
import           System.Process
import           Types
import qualified Data.Vector                   as V
import           Data.Functor.Foldable
import           Data.Either                    ( )
import           Tiler
-- import Data.


-- drawLine x y st = do
--   p <- readSTRef st
--   return ()

-- Event Handlers --

-- | This handler is used while holding down a key but before another key has been pressed
newModeHandler :: Mode -> KeyTrigger -> Action -> Xest Actions

-- A button was pressed so we need to change how we handle releasing the key
newModeHandler oldMode boundT (KeyboardEvent (_, targetMode, actions) True) =
  do
    modify (keyParser .~ tempModeRunner oldMode boundT)
    -- TODO put in function instead of copy and pasting following code between handlers
    activeMode <- gets $ view currentMode
    -- Check if the current mode is the mode that the keybinding is defined for
    -- Because we constantly rebind keys, I think this should always be true so TODO check that
    if activeMode == targetMode then return actions else return []

-- A button was released so go back to the normal handler if it was the key we were watching
newModeHandler _ boundT ke@(KeyboardEvent kt False)
  | boundT == kt = modify (set keyParser handler) >> return []
  | otherwise    = handler ke

-- Otherwise defer to the normal handler
newModeHandler _ _ e = handler e


-- | Handler used when another key is clicked while holding one down
tempModeRunner :: Mode -> KeyTrigger -> Action -> Xest Actions

-- On release, return to the old mode
tempModeRunner oldMode boundKey ke@(KeyboardEvent k False)
  | k == boundKey = do
    modify $ set keyParser handler
    return [ChangeModeTo oldMode]
  | otherwise = handler ke

-- Nearly identical to the handler version except it doesn't change the keyParser
-- This is part of that TODO up above
tempModeRunner _ _ (KeyboardEvent (_, targetMode, actions) True) = do
  activeMode <- gets $ view currentMode
  if activeMode == targetMode then return actions else return []

-- Otherwise defer to handler
tempModeRunner _ _ e = handler e


-- | The bulk of the program
-- Performs some action and returns a list of new actions to be performed
-- Returning a [] means that there aren't any new tasks to do.
handler :: Action -> Xest Actions
-- Called on window creation
handler (XorgEvent MapRequestEvent {..}) = do
  display <- asks display
  -- managing a window allows us to do any number of things to it
  -- Currently we wrap it in a new type and do nothing else
  tWin    <- manage ev_window
  -- Recall the mapWindow Xorg event.
  -- We don't receive the mapWindow event from this because Xorg knows we sent it.
  liftIO $ mapWindow display ev_window
  -- This adds the new window to whatever tiler comes after inputController
  -- If you've zoomed the inputController in, you get nesting as a result
  modify $ \es -> set
    desktop
    (cata (applyInput (add Front Focused tWin)) $ view desktop es)
    es
  -- Make the newly created window into the focused one
  -- We have to keep Xorg's idea of focus in sync with our own
  liftIO $ setInputFocus display ev_window revertToNone currentTime
  return []

-- Called on window destruction
handler (XorgEvent DestroyWindowEvent {..}) = do
  -- Remove the destroyed window from our tree.
  -- Usually, unmap will be called first, but what if a minimized window
  -- gets killed? In that case, we won't get an unmap notifiction.
  modify $ \es -> desktop .~ cata (remove $ Wrap ev_window) (_desktop es) $ es
  return []

-- Called when a window should no longer be drawn
-- This either happens when the window dies, or when we minimize it
handler (XorgEvent UnmapEvent {..}) = do
  mins <- gets $ view minimizedWins
  -- Remove the destroyed window from our tree if we aren't the
  -- reason it was unmapped.
  unless (member ev_window mins) $ modify $ \es ->
    desktop .~ cata (remove $ Wrap ev_window) (_desktop es) $ es
  return []

-- Tell the window it can configure itself however it wants
-- We send back the Configure Request unmodified
handler (XorgEvent ConfigureRequestEvent {..}) = do
  IS {..} <- ask
  liftIO $ configureWindow display ev_window ev_value_mask wc
  return []
 where
  wc = WindowChanges ev_x
                     ev_y
                     ev_width
                     ev_height
                     ev_border_width
                     ev_above
                     ev_detail

-- Determine if we care about the key event
-- Because we rebind keys, I think we always should, so, TODO look at that
handler (XorgEvent KeyEvent {..}) = do
  Conf bindings _ <- asks config
  return $ case find (\(k, _, _) -> ev_keycode == k) bindings of
    Nothing -> []
    Just kt -> [KeyboardEvent kt (ev_event_type == keyPress)]

-- Called when the cursor moves between windows
handler (XorgEvent CrossingEvent {..}) = do
  d     <- asks display
  rootT <- gets $ view desktop
  -- Make certain that the focused window is the one we're hovering over
  liftIO $ setInputFocus d ev_window revertToNone currentTime
  let (newRoot, status) = cata (focusWindow ev_window) rootT
  -- If status is anything but (False, False), it means that something super
  -- weird is going on and the newRoot is probably bad.
  -- Usually this happens if the crossed window isn't in our tree and causes
  -- the InputController to disappear.
  when (status == (False, False)) . modify $ set desktop newRoot
  return []

-- Handle all other xorg events as noops
handler (XorgEvent  _) = return []

-- Run a shell command
handler (RunCommand s) = liftIO (spawnCommand s) >> return []

-- Perform a keyboard event if we are in the correct mode
handler (KeyboardEvent kt@(_, targetMode, actions) True) = do
  -- See the previous copies of this code for more info
  activeMode <- view currentMode <$> get
  if activeMode == targetMode
    then do
    -- The only difference is we set the keyParser to the newModeHandler
    -- This is completely safe even if the key doesn't trigger a new mode
      modify $ set keyParser (newModeHandler activeMode kt)
      return actions
    else return []
-- Ignore keyups
-- Note that either tmp or new handler will capture this if needed
-- The default handler though doesn't care about keyup
handler (KeyboardEvent _ False) = return []

-- Show a window given its class name
handler (ShowWindow wName     ) = do
  wins     <- getWindowByClass wName
  display <- asks display
  forM_ wins $ liftIO . mapWindow display
  return []

-- Hide a window given its class name
handler (HideWindow wName) = do
  wins     <- getWindowByClass wName
  display <- asks display
  forM_ wins $ liftIO . unmapWindow display
  return []

-- Zoom the inputController towards the focused window
handler ZoomInInput = do
  root <- gets $ view desktop
  modify . set desktop $ cata reorder root
  return []
 where
  reorder (InputController (Fix t)) =
    Fix $ modFocused (Fix . InputController) t
  reorder t = Fix t

-- Move the input controller towards the root
handler ZoomOutInput = do
  root <- gets $ view desktop
  -- Don't zoom the controller out of existence
  unless (isController root) $ modify . set desktop $ para reorder root
  return []
 where
  reorder (InputController (_, t)) = t
  reorder t                        = Fix $ if any (isController . fst) t
    then InputController . Fix $ snd <$> t
    else snd <$> t
  isController (Fix (InputController _)) = True
  isController _                         = False

-- Change the given mode to something else
handler (ChangeModeTo newM) = do
  eActions <- gets $ exitActions . view currentMode
  rebindKeys newM
  modify $ set currentMode newM
  --Combine the two lists of actions to be executed. Execute exit actions first.
  return $ eActions ++ introActions newM

-- Change the layout of whatever comes after the input controller to something else
handler (ChangeLayoutTo (Fix newT)) = do
  root <- gets $ view desktop
  modify . set desktop $ cata (applyInput (changeLayout newT)) root
  return []

-- Change the focus to some named action
handler (ChangeNamed s) = do
  root <- gets $ view desktop
  modify . set desktop $ cata (applyInput changer) root
  xFocus
  return []
 where
  changer t@(Directional d fl) = case readMay s of
    Just a -> Directional d $ fl { focusedElement = a - 1 }
    Nothing -> t
  changer t = t

-- Change focus in a given direction
handler (Move newD) = do
  root    <- gets $ view desktop
  modify . set desktop $ cata (applyInput (changer newD)) root
  xFocus
  return []
 where
  changer Front (Directional d (FL fe e)) =
    Directional d $ FL (max 0 $ fe - 1) e
  changer Back (Directional d (FL fe e)) =
    Directional d $ FL (min (V.length e - 1) $ fe + 1) e
  changer _ t = t

-- | Move all of the Tilers from root to newT
changeLayout :: Tiler (Fix Tiler) -> Tiler (Fix Tiler) -> Tiler (Fix Tiler)
changeLayout newT root = unfix $ doPopping root newT
 where
  doPopping ot t = case popWindow (Left Front) (Fix ot) of
    (Nothing , _   ) -> Fix t
    (Just win, wins) -> doPopping wins $ add Back (isFocused win) win t
  isFocused t = if t == focused then Focused else Unfocused
  focused =
    fromMaybe (Fix EmptyTiler) . fst . popWindow (Right Focused) $ Fix root

-- Random stuff --

-- Would be used for reparenting (title bar)
manage :: Window -> Xest (Fix Tiler)
manage w = do
  IS {..} <- ask
  liftIO $ selectInput display w enterWindowMask
  wmState  <- liftIO $ internAtom display "_NET_WM_WINDOW_TYPE" False
  normal  <- (liftIO $ internAtom display "_NET_WM_WINDOW_TYPE_NORMAL" False) :: Xest Word64
  return $ Fix $ Wrap w
  -- prop <- liftIO $ rawGetWindowProperty 32 display wmState w :: Xest (Maybe [Word64])
  -- print $ "t " ++ show prop
  -- return $ case prop of
  --   Nothing -> Fix EmptyTiler
  --   Just states -> if isJust $ find (== normal) states then Fix $ Wrap w else Fix EmptyTiler

-- Chang the keybindings depending on the mode
rebindKeys :: Mode -> Xest ()
rebindKeys activeMode = do
  Conf kb _ <- asks config
  d         <- asks display
  win       <- asks rootWin
  liftIO . forM_ kb $ toggleModel activeMode d win
 where
  toggleModel :: Mode -> Display -> Window -> KeyTrigger -> IO ()
  toggleModel m d win (k, km, _) = if m == km
    then grabKey d k anyModifier win False grabModeAsync grabModeAsync
    else ungrabKey d k anyModifier win

getWindows :: Xest [Window]
getWindows = do
  display         <- asks display
  root            <- asks rootWin
  -- At this point things aren't really wrapped so we need to manage memory manually
  numChildrenPtr  <- liftIO malloc
  childrenListPtr <- liftIO malloc
  uselessPtr      <- liftIO . alloca $ \x -> return x
  _               <- liftIO $ xQueryTree display
                                         root
                                         uselessPtr
                                         uselessPtr
                                         childrenListPtr
                                         numChildrenPtr
  numChildren  <- liftIO $ peek numChildrenPtr
  childrenList <- liftIO $ peek childrenListPtr >>= peekArray
    (fromIntegral numChildren)
  _ <- if not (null childrenList)
    then liftIO $ peek childrenListPtr >>= xFree
    else return 0
  liftIO $ free childrenListPtr
  liftIO $ free numChildrenPtr
  return childrenList

-- Find a window with a class name
-- TODO make the C interface less terrifying
getWindowByClass :: String -> Xest [Window]
getWindowByClass wName = do
  display      <- asks display
  childrenList <- getWindows
  let findWindow win = do
        ClassHint _ className <- getClassHint display win
        return $ className == wName
  liftIO $ filterM findWindow childrenList

-- Moves windows around
render :: EventState -> Xest ()
render (ES t _ _ _) = do
  (w, h) <- asks dimensions
  cata placeWindows t (Rect 0 0 w h)

-- |Focus the X window
xFocus :: Xest ()
xFocus = do
  root <- fmap unfix . gets $ view desktop
  d    <- asks display
  focWin d $ unsafeLast (ana makeList root :: [Tiler (Fix Tiler)])
 where
  focWin d (Wrap w) =
    safeMap w >> liftIO (setInputFocus d w revertToNone currentTime)
  focWin _ _ = error "How can I focus this!"
  makeList :: Tiler (Fix Tiler) -> ListF (Tiler (Fix Tiler)) (Tiler (Fix Tiler))
  makeList (Wrap _) = Nil
  makeList t = Cons (unfix . getFocused $ Fix t) (unfix . getFocused $ Fix t)
  getFocused = fromMaybe (error "no focus") . fst . popWindow (Right Focused)
