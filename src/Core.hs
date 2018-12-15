{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
-- # LANGUAGE OverloadedStrings #
{-# LANGUAGE TupleSections     #-}


-- Almost all of the logic resides in here
-- TODO make the above statement false
module Core where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.State.Lazy (get, gets, modify, put)
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Window
import           System.Process
import           Types


filterMap :: (MonoFoldable (f a), Applicative f, Monoid (f b)) => (b -> Bool) -> (Element (f a) -> b) -> f a -> f b
filterMap p f = foldl' folder mempty
  where folder acc a = let b = f a
                       in if p b then acc `mappend` pure (f a) else acc

-- Tiler Handling Code

-- Tiler Laws

-- Tilers are not black holes
-- a `addFocused` w != a

-- deleteWindow undoes an add
-- a `addFocused` w `deleteWindow` w == a

-- delete is a noop if w is not in a
-- a `deleteWindow` w (where w is not in a) == a

-- addFocused undoes a pop
-- let (w, b) = popWindow a in a == b `addFocused` w

-- Nested Tilers are searched
-- let b = a `addFocused` w
--     d = c `addFocused` b
--     in d `deleteWindow` w == c `addFocused` a

-- Adds a new Tiler and make it focused
addFocused :: Tiler -> Tiler -> Tiler
addFocused w (Horizontal a)      = Horizontal $ w:a
addFocused w (Vertical a)        = Vertical $ w:a
addFocused w (InputController a) = InputController $ addFocused w a
addFocused w t@(Wrap _)          = Horizontal [w, t]
addFocused w EmptyTiler          = w

-- Adds a new Tiler and store it in the back
addUnFocused :: Tiler -> Tiler -> Tiler
addUnFocused w (Horizontal a)      = Horizontal $ a ++ [w]
addUnFocused w (Vertical a)        = Vertical $ a ++ [w]
addUnFocused w (InputController a) = InputController $ addUnFocused w a
addUnFocused w t@(Wrap _)          = Horizontal [t, w]
addUnFocused w EmptyTiler          = w

-- Deletes a Window from a Tiler
deleteWindow :: Tiler -> Tiler -> Tiler
deleteWindow w (Horizontal a) = Horizontal $ filterMap (/= EmptyTiler) (deleteWindow w) a
deleteWindow w (Vertical a) = Vertical $ filterMap (/= EmptyTiler) (deleteWindow w) a
deleteWindow w (InputController a) = InputController $ deleteWindow w a
deleteWindow w a = if w == a then EmptyTiler else a


-- Used to force the Tiler to give up control over a window.
-- For example, if the Window is moving to a different workplace.
popWindow :: Tiler -> (Maybe Tiler, Tiler)
popWindow (Horizontal (a:as)) = (Just a, Horizontal as)
popWindow (Horizontal [])     = (Nothing, Horizontal [])
popWindow (Vertical (a:as))   = (Just a, Vertical as)
popWindow (Vertical [])       = (Nothing, Vertical [])
popWindow (InputController t) = popWindow t
popWindow t@(Wrap _)          = (Just t, EmptyTiler)
popWindow EmptyTiler          = (Nothing, EmptyTiler)


-- Allow the Tiler to move windows wherever they need to be
placeWindows :: Rect -> Tiler -> Xest ()
-- Wraps place their actual windows and end the recursion
placeWindows Rect {..} (Wrap win) = do
  IS {..} <- ask
  liftIO $ moveWindow display win x y
  liftIO $ resizeWindow display win w h

-- Horizontals and verticals sort tile their windows
placeWindows Rect {..} (Horizontal ts) =
  foldl' (\acc tiler -> acc >> place tiler) (pure ())
    $ zip [0..] ts -- Zip the list of Tilers with their indexes
  where
    numWins = fromIntegral $ length ts -- Find the number of windows in ts
    place (i, t) = placeWindows (Rect (newX i) y (w `div` fromIntegral numWins) h) t
    newX i = (fromIntegral w `div` numWins * i) + x

placeWindows Rect {..} (Vertical ws) =
  foldl'
    (\acc (i, t) ->
       acc >> placeWindows
       (Rect x (fromIntegral h `div` numWins * i + y)
             w (h `div` fromIntegral numWins)) t)
    (pure ()) $ zip [0..] ws
  where
    numWins = fromIntegral $ length ws

placeWindows r (InputController t) = (liftIO $ print "PLACING WINDOW") >> placeWindows r t
placeWindows _ EmptyTiler = return ()

-- Parse an event
checkEvent:: Either Events Events -> Tiler -> Xest Tiler
checkEvent (Left e) (InputController t) = checkEvent (Right e) t >>= return . InputController
checkEvent le@(Left _) (Horizontal h) = foldl' (\a x -> a >>= checkEvent le >>= nxt x) (return $ Horizontal []) h
  where nxt (Horizontal horiz) newH = return $ Horizontal (newH:horiz)
checkEvent le@(Left _) (Vertical h) = foldl' (\a x -> a >>= checkEvent le >>= nxt x) (return $ Vertical []) h
  where nxt (Vertical horiz) newH = return $ Vertical (newH:horiz)
checkEvent (Right ke@(KeyboardEvent (_, _, actions) _)) t = liftIO (print "Checked") >> runActions t actions ke


-- Event Handlers

-- Handler used while holding down a change mode key.
newModeHandler :: Mode -> Trigger_ KeyTrigger -> Events -> Xest Events

-- A button was pressed so we need to change how we handle releasing the key
newModeHandler oldMode boundT ke@(KeyboardEvent _ True) =
  modify (keyParser .~ tempModeRunner oldMode boundT) >> handler ke


newModeHandler _ boundT ke@(KeyboardEvent kt False)
  | boundT == kt = modify (set keyParser handler) >> return NoEvent
  | otherwise = handler ke

newModeHandler _ _ e = handler e

-- Handler used when a mode should return after the key is released
tempModeRunner :: Mode -> Trigger_ KeyTrigger -> Events -> Xest Events
tempModeRunner oldMode boundKey ke@(KeyboardEvent k False)
  | k == boundKey = do
      rebindKeys oldMode
      modify (set keyParser handler)
      updateMode oldMode
      return NoEvent
  | otherwise = handler ke
tempModeRunner _ _ e = handler e

-- Acts on events
-- Called on window creation
handler :: Events -> Xest Events
handler (XorgEvent MapRequestEvent {..}) = do
  display <- asks display
  tWin <- manage ev_window
  liftIO $ mapWindow display ev_window
  modify $ \es -> desktop .~ addFocused tWin (_desktop es) $ es
  return NoEvent

-- Called on window destruction
-- TODO handle minimizing as unmapping
handler (XorgEvent UnmapEvent {..}) = do
  modify $ \es -> desktop .~ deleteWindow (Wrap ev_window) (_desktop es) $ es
  return NoEvent

-- Tell the window it can configure itself however it wants
handler (XorgEvent ConfigureRequestEvent {..}) = do
  IS {..} <- ask
  liftIO $ configureWindow display ev_window ev_value_mask wc
  return NoEvent
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

-- Run any key configurations
handler (XorgEvent KeyEvent {..}) = do
  Conf bindings _ <- asks config
  return $ case find (\(k, _, _) -> ev_keycode == k) bindings of
    Nothing -> NoEvent
    Just kt -> KeyboardEvent kt (ev_event_type == keyPress)

handler ke@(KeyboardEvent _ True) = get >>= checkEvent (Left ke) . _desktop >>= (\t -> modify $ set desktop t ) >> return NoEvent
handler ke@(ModeEvent (_, ia, _) (_, _, ea)) = liftIO $ print "wat" >> return undefined--(runActions ea ke es >>= runActions ia ke) >> return NoEvent

-- handler (ActionEvent aes) =

-- Finds an Xorg Event and submits it
handler NoEvent = do
  render <$> get
  display <- asks display
  ptr <- liftIO . allocaXEvent $ \p -> nextEvent display p >> getEvent p
  return $ XorgEvent ptr

handler _ = return NoEvent

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
    toggleModel :: Mode -> Display -> Window -> Trigger_ KeyTrigger -> IO ()
    toggleModel m d win (k, km, _) =
          if m == km then grabKey d k anyModifier win False grabModeAsync grabModeAsync
          else ungrabKey d k anyModifier win

-- Find a window with a class name
-- TODO make the C interface less bad
getWindowByClass :: String -> Xest (Maybe Window)
getWindowByClass wName = do
  display <- asks display
  root <- asks rootWin
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

-- Executes a list of actions in order
-- Returns a new tiler
runActions :: Tiler -> Actions -> Events -> Xest Tiler
runActions t as e = foldl' (\acc a -> acc >>= \newT -> runAction newT a e) (return t) as

-- Executes a single action
runAction :: Tiler -> Action -> Events -> Xest Tiler

runAction ot (ChangeLayoutTo t) e =
  case popWindow ot of
    (Nothing, _) -> return t
    (Just win, wins) -> runAction wins (ChangeLayoutTo $ addUnFocused win t) e

runAction t (RunCommand s) _ = liftIO (spawnCommand s) >> liftIO (print "Runnign command") >> return t

runAction t (ChangeModeTo m) (KeyboardEvent kt True) = do
  rebindKeys m
  activeMode <- view currentMode <$> get
  updateMode activeMode
  modify $ set keyParser (newModeHandler activeMode kt)
  return t

runAction t (ChangeModeTo m) _ = do
  updateMode m
  modify $ set keyParser handler
  return t

runAction t (ShowWindow wName) _ = do
  win <- getWindowByClass wName
  display <- asks display
  case win of
    Just w  -> liftIO $ mapWindow display w
    Nothing -> return ()
  return t

runAction t (HideWindow wName) _ = do
  win <- getWindowByClass wName
  display <- asks display
  case win of
    Just w  -> liftIO $ unmapWindow display w
    Nothing -> return ()
  return t

runAction t ZoomInInput _ = do
  modify $ \es -> (desktop .~ reorder (es ^. desktop)) es
  return t
  where reorder (InputController ic) = case popWindow ic of
          (Nothing, tiler) -> InputController tiler
          (Just w, tiler)  -> addFocused (InputController w) tiler
        reorder other = reorder `omap` other

runAction t Done _ = return t

-- A lens with side effects
updateMode :: Mode -> Xest ()
updateMode newM = do
  orig <- get
  newT <- runActions (_desktop orig) (exitActions . currentMode_ $ orig) NoEvent
  finalT <- runActions newT (introActions newM) NoEvent
  modify $ \e -> e {currentMode_ = newM, _desktop = finalT}

-- A normal lens
currentMode :: Getter EventState Mode
currentMode = lens currentMode_ (\orig newM -> orig {currentMode_ = newM})
