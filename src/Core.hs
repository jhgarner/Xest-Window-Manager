{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}


module Core where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.State.Lazy (get, gets, modify)
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Window
import           System.Process
import           Types


-- Tiler Handling Code --

-- | Add a new focused Tiler with laws
addFocused :: Tiler -> Tiler -> Tiler
addFocused w (Horizontal a)      = Horizontal $ w:a
addFocused w (Vertical a)        = Vertical $ w:a
addFocused w (InputController a) = InputController $ addFocused w a
addFocused w t@(Wrap _)          = Horizontal [w, t]
addFocused w EmptyTiler          = w

-- | Adds a new Tiler and store it at the back
-- If the Tiler is empty, this is equivalent to addFocused
addUnFocused :: Tiler -> Tiler -> Tiler
addUnFocused w (Horizontal a)      = Horizontal $ a ++ [w]
addUnFocused w (Vertical a)        = Vertical $ a ++ [w]
addUnFocused w (InputController a) = InputController $ addUnFocused w a
addUnFocused w t@(Wrap _)          = Horizontal [t, w]
addUnFocused w EmptyTiler          = w

-- | Deletes a Window from a Tiler
deleteWindow :: Window -> Tiler -> Tiler
deleteWindow w (Horizontal a) = Horizontal $ filterMap' (/= EmptyTiler) (deleteWindow w) a
deleteWindow w (Vertical a) = Vertical $ filterMap' (/= EmptyTiler) (deleteWindow w) a
deleteWindow w (InputController a) = InputController $ deleteWindow w a
deleteWindow w t@(Wrap a) = if w == a then EmptyTiler else t
deleteWindow _ EmptyTiler = EmptyTiler

-- | Deletes a Window from a Tiler
deleteTiler :: Tiler -> Tiler -> Tiler
deleteTiler t root
  | t == root = EmptyTiler
  | otherwise = case root of
      (Horizontal a) -> Horizontal $ filterMap' (/= EmptyTiler) (deleteTiler t) a
      (Vertical a) -> Vertical $ filterMap' (/= EmptyTiler) (deleteTiler t) a
      (InputController a) -> InputController $ deleteTiler t a
      a -> a


-- | Used to force the Tiler to give up control over the focused window.
-- For example, if the Window is moving to a different workplace.
popWindow :: Tiler -> (Maybe Tiler, Tiler)
popWindow (Horizontal (a:as)) = (Just a, Horizontal as)
popWindow (Horizontal [])     = (Nothing, Horizontal [])
popWindow (Vertical (a:as))   = (Just a, Vertical as)
popWindow (Vertical [])       = (Nothing, Vertical [])
popWindow (InputController t) = popWindow t
popWindow t@(Wrap _)          = (Just t, EmptyTiler)
popWindow EmptyTiler          = (Nothing, EmptyTiler)


-- | Render all of the windows in the correct location based on nesting
placeWindows :: Rect -> Tiler -> Xest ()
-- | Wraps place their wrapped window and end the recursion
placeWindows Rect {..} (Wrap win) = do
  IS {..} <- ask
  liftIO $ moveWindow display win x y
  liftIO $ resizeWindow display win w h

-- | Place inner tilers horizontally
placeWindows Rect {..} (Horizontal ts) =
  foldl' (\acc tiler -> acc >> place tiler) (pure ())
    $ zip [0..] ts -- Zip the list of Tilers with their indexes
  where
    numWins = fromIntegral $ length ts -- Find the number of windows in ts
    place (i, t) = placeWindows (Rect (newX i) y (w `div` fromIntegral numWins) h) t
    newX i = (fromIntegral w `div` numWins * i) + x

-- | Place inner tilers vertically
placeWindows Rect {..} (Vertical ws) =
  foldl'
    (\acc (i, t) ->
       acc >> placeWindows
       (Rect x (fromIntegral h `div` numWins * i + y)
             w (h `div` fromIntegral numWins)) t)
    (pure ()) $ zip [0..] ws
  where
    numWins = fromIntegral $ length ws

-- | Input controllers are transparent
placeWindows r (InputController t) = placeWindows r t
placeWindows _ EmptyTiler = return ()

-- | Given something that wants to modify a Tiler,
-- apply it to the first Tiler after the inputController
-- TODO actually make this a better function
checkEvent :: Either (Tiler -> Tiler) (Tiler -> Tiler) -> Tiler -> Tiler
checkEvent (Left e) (InputController t) = InputController $ checkEvent (Right e) t
checkEvent le@(Left _) (Horizontal h) = Horizontal $ map (checkEvent le) h
checkEvent le@(Left _) (Vertical h) = Vertical $ map (checkEvent le) h
checkEvent (Right action) t = action t
checkEvent _ wt@(Wrap _) = wt
checkEvent _ EmptyTiler = EmptyTiler


-- Event Handlers --

-- | Handler used while holding down a key but before another key has been pressed
newModeHandler :: Mode -> KeyTrigger -> Action -> Xest Actions

-- A button was pressed so we need to change how we handle releasing the key
newModeHandler oldMode boundT (KeyboardEvent (_, targetMode, actions) True) = do
  modify (keyParser .~ tempModeRunner oldMode boundT)
  -- TODO put in function instead of copy and pasting following code between handlers
  activeMode <- gets $ view currentMode
  -- Check if the current mode is the mode that the keybinding is defined for
  -- Because we constantly rebind keys, I think this should always be true so TODO check that
  if activeMode == targetMode then
    return actions
  else return []

-- A button was released so go back to the normal handler if it was the key we were watching
newModeHandler _ boundT ke@(KeyboardEvent kt False)
  | boundT == kt = modify (set keyParser handler) >> return []
  | otherwise = handler ke

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
  if activeMode == targetMode then
    return actions
  else return []

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
  tWin <- manage ev_window
  -- Recall the mapWindow Xorg event.
  -- We don't receive the mapWindow event from this because Xorg knows we sent it.
  liftIO $ mapWindow display ev_window
  -- Essentially, this adds the new window to whatever tiler comes after inputController
  -- If you've zoomed the inputController in, you get nesting as a result
  modify $ \es -> set desktop (checkEvent (Left $ addFocused tWin) $ view desktop es) es
  return []

-- Called on window destruction
-- TODO handle minimizing as unmapping
handler (XorgEvent UnmapEvent {..}) = do
  -- Remove the destroyed window from our tree
  modify $ \es -> desktop .~ deleteWindow ev_window (_desktop es) $ es
  return []

-- Tell the window it can configure itself however it wants
-- We send back the Configure Request unmodified
handler (XorgEvent ConfigureRequestEvent {..}) = do
  liftIO $ say "COnfiguring window"
  IS {..} <- ask
  liftIO $ configureWindow display ev_window ev_value_mask wc
  return []
  where
    wc =
      WindowChanges
        ev_x
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

-- Handle all other xorg events as noops
handler (XorgEvent _) = return []

-- Run a shell command
handler (RunCommand s) = liftIO (spawnCommand s) >> return []

-- Perform a keyboard event if we are in the correct mode
handler (KeyboardEvent kt@(_, targetMode, actions) True) = do
  -- See the previous copies of this code for more info
  activeMode <- view currentMode <$> get
  if activeMode == targetMode then do
    -- The only difference is we set the keyParser to the newModeHandler
    -- This is completely safe even if the key doesn't trigger a new mode
    modify $ set keyParser (newModeHandler activeMode kt)
    return actions
  else return []
-- Ignore keyups
-- Note that either tmp or new handler will capture this if needed
handler (KeyboardEvent _ False) = return []

-- Show a window given its class name
handler (ShowWindow wName) = do
  win <- getWindowByClass wName
  display <- asks display
  case win of
    Just w  -> liftIO $ mapWindow display w
    Nothing -> liftIO $ say "Window to be shown does not exist"
  return []

-- Hide a window given its class name
handler (HideWindow wName) = do
  win <- getWindowByClass wName
  display <- asks display
  case win of
    Just w  -> liftIO $ unmapWindow display w
    Nothing -> liftIO $ say "Window to be hidden does not exist"
  return []

-- Zoom the inputController towards the focused window
handler ZoomInInput = do
  modify $ \es -> (desktop .~ reorder (es ^. desktop)) es
  return []
  where reorder (InputController ic) = case popWindow ic of
          (Nothing, tiler) -> InputController tiler
          (Just w, tiler)  -> addFocused (InputController w) tiler
        reorder other = reorder `omap` other

-- Move the input controller towards the root
handler ZoomOutInput = do
  root <- gets $ view desktop
  modify . set desktop $ reorder root
  return []
  where
    reorder t = case findIC t of
                  Nothing -> reorder `omap` t
                  Just inner -> InputController . addFocused inner $ deleteTiler (InputController inner) t
    findIC = foldl' (\acc a -> acc <|> isIC a) Nothing
    isIC (InputController t) = Just t
    isIC _ = Nothing

-- Change the given mode to something else
handler (ChangeModeTo newM) = do
  eActions <- gets $ exitActions . view currentMode
  rebindKeys newM
  modify $ set currentMode newM
  --Combine the two lists of actions to be executed. Execute exit actions first.
  return $ eActions ++ introActions newM

-- Change the layout of whatever comes after the input controller to something else
handler (ChangeLayoutTo newT) = do
  modify $ \es -> set desktop (checkEvent (Left changeLayout) $ view desktop es) es
  return []
  where changeLayout ot = doPopping ot newT
        doPopping ot t =
          case popWindow ot of
            (Nothing, _)     -> t
            (Just win, wins) -> doPopping wins $ addUnFocused win t

-- Random stuff --

-- | filterMap over monoFoldables
-- Check out classyPrelude and monotraversal libraries for more info
filterMap' :: (MonoFoldable (f a), Applicative f, Monoid (f b)) => (b -> Bool) -> (Element (f a) -> b) -> f a -> f b
filterMap' p f = foldl' folder mempty
  where folder acc a = let b = f a
                       in if p b then acc `mappend` pure (f a) else acc

-- Would be used for reparenting (title bar)
manage :: Window -> Xest Tiler
manage w = do
  IS {..} <- ask
  return $ Wrap w

-- Chang the keybindings depending on the mode
rebindKeys :: Mode -> Xest ()
rebindKeys activeMode = do
  Conf kb _ <- asks config
  d <- asks display
  win <- asks rootWin

  liftIO . forM_ kb $ toggleModel activeMode d win
  where
    toggleModel :: Mode -> Display -> Window -> KeyTrigger -> IO ()
    toggleModel m d win (k, km, _) =
          if m == km then grabKey d k anyModifier win False grabModeAsync grabModeAsync
          else ungrabKey d k anyModifier win

-- Find a window with a class name
-- TODO make the C interface less terrifying
getWindowByClass :: String -> Xest (Maybe Window)
getWindowByClass wName = do
  display <- asks display
  root <- asks rootWin
  -- At this point things aren't really wrapped so we need to manage memory manually
  numChildrenPtr <- liftIO malloc
  childrenListPtr <- liftIO malloc
  uselessPtr <- liftIO . alloca $ \x -> return x
  _ <- liftIO $ xQueryTree display root uselessPtr uselessPtr childrenListPtr numChildrenPtr
  numChildren <- liftIO $ peek numChildrenPtr
  childrenList <- liftIO $ peek childrenListPtr >>= peekArray (fromIntegral numChildren)
  liftIO $ free childrenListPtr
  liftIO $ free numChildrenPtr
  let findWindow (win:wins) = do
        ClassHint _ className <- getClassHint display win
        if className == wName
          then return $ Just win
          else findWindow wins
      findWindow [] = return Nothing
  liftIO $ findWindow childrenList

-- Moves windows around
render :: EventState -> Xest ()
render (ES t _ _) = do
  (w, h) <- asks dimensions
  placeWindows (Rect 0 0 w h) t

