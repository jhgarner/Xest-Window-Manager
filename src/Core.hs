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
import           System.Process
import           Types
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as MV
import           Data.Functor.Foldable
import           Data.Either                    ( )
import           Tiler


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
    (cata (applyInput $ add Front Focused tWin) $ view desktop es)
    es
  -- Make the newly created window into the focused one
  liftIO $ setInputFocus display ev_window revertToNone currentTime
  return []

-- Called on window destruction
handler (XorgEvent DestroyWindowEvent {..}) = do
  -- Remove the destroyed window from our tree
  modify $ \es -> desktop .~ cata (remove $ Wrap ev_window) (_desktop es) $ es
  return []

-- Tell the window it can configure itself however it wants
-- We send back the Configure Request unmodified
handler (XorgEvent ConfigureRequestEvent {..}) = do
  liftIO $ say "COnfiguring window"
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
-- Because we rebind keys, I think we always should so TODO look at that
handler (XorgEvent KeyEvent {..}) = do
  Conf bindings _ <- asks config
  return $ case find (\(k, _, _) -> ev_keycode == k) bindings of
    Nothing -> []
    Just kt -> [KeyboardEvent kt (ev_event_type == keyPress)]

handler (XorgEvent CrossingEvent {..}) = do
  d     <- asks display
  rootT <- gets $ view desktop
  liftIO $ setInputFocus d ev_window revertToNone currentTime
  let newFocus = cata (focusWindow ev_window) rootT
  when (snd newFocus == (False, False)) . modify $ set desktop (fst newFocus)
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
handler (KeyboardEvent _ False) = return []

-- Show a window given its class name
handler (ShowWindow wName     ) = do
  win     <- getWindowByClass wName
  display <- asks display
  case win of
    Just w  -> liftIO $ mapWindow display w
    Nothing -> liftIO $ say "Window to be shown does not exist"
  return []

-- Hide a window given its class name
handler (HideWindow wName) = do
  win     <- getWindowByClass wName
  display <- asks display
  case win of
    Just w  -> liftIO $ unmapWindow display w
    Nothing -> liftIO $ say "Window to be hidden does not exist"
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
  modify . set desktop $ para reorder root
  return []
 where
  reorder (InputController (_, t)) = t
  reorder t                        = Fix $ if any (isController . fst) t
    then InputController . Fix $ getSnd t
    else getSnd t
  getSnd = fmap snd
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
  modify $ set desktop (cata (applyInput $ changeLayout newT) root)
  return []

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
  return . Fix $ Wrap w

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

-- Find a window with a class name
-- TODO make the C interface less terrifying
getWindowByClass :: String -> Xest (Maybe Window)
getWindowByClass wName = do
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
  let findWindow (win : wins) = do
        ClassHint _ className <- getClassHint display win
        if className == wName then return $ Just win else findWindow wins
      findWindow [] = return Nothing
  liftIO $ findWindow childrenList

-- Moves windows around
render :: EventState -> Xest ()
render (ES t _ _) = do
  (w, h) <- asks dimensions
  cata placeWindows t (Rect 0 0 w h)
