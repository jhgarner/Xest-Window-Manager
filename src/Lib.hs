{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Lib
  ( startWM
  )
where

import           Standard
import           Config
import           Core
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Window
import           SDL hiding (get, Window, Display, trace, Mode)
import qualified SDL.Font as Font
import           Base.DoAll
import           Tiler.Tiler
import qualified System.Environment as Env
import qualified Data.Map                      as M
import qualified Data.IntMap                      as IM
import XEvents
import Actions.ActionTypes
import Actions.Actions
import qualified Control.Exception as E
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Reader (ReaderT(runReaderT))

-- | Starting point of the program. This function should never return
startWM :: IO ()
startWM = do
  -- We want antialiasing on our text and normal Xlib can't give us that.
  -- We use SDL for the window borders to get around that problem.
  -- Here we initialize SDL and its cool fonts.
  initializeAll
  Font.initialize

  -- Grab a display to capture. The chosen display cannot have a WM already running.
  -- We grab it based on the arguments passed to Xest.
  -- By default we pick 1 since that seems to be what GDM offers most
  -- of the time. If you launch it with startx, you probably want 0.
  displayEnv <- Text . fromJust . tailMay <$> Env.getEnv "DISPLAY"
  args <- map Text <$> getArgs
  let displayNumber = fromMaybe displayEnv $ headMay args
  display <- openDisplay . view _Text $ ":" <> displayNumber

  -- Read the config file from the user's home directory.
  -- If using display number 0 or 1, you're probably launching a real
  -- environment using startx or some display manager. In that case,
  -- we just launch the config.dhall file. If you run Xest on a different
  -- display, it likely means you're testing and want a slightly different
  -- config. For example, my testing config uses Alt instead of the Super
  -- (Windows) key.
  homeDir <- Text <$> Env.getEnv "HOME"
  print displayNumber
  c <- if displayNumber == "0" || displayNumber == "1"
          then readConfig display $ homeDir <> "/.config/xest/config.dhall"
          else readConfig display $ homeDir <> "/.config/xest/config." <> displayNumber <> ".dhall"
  let startingMode = initialMode c
  font <- Font.load (view _Text $ fontLocation c) 18

  -- X orders windows like a tree.
  -- This gets the root of said tree.
  let root = defaultRootWindow display

  -- EWMH wants us to create an extra window to verify we really
  -- can handle some of the EWMH spec. This window does that.
  -- Since we don't want the window to be visible, we give it a crazy
  -- location. We alse set_override_redirect because Xest shouldn't be
  -- alerted if the window gets moved around.
  ewmhWin <- createSimpleWindow display root
          10000 10000 1 1 0 0
          $ whitePixel display (defaultScreen display)
  allocaSetWindowAttributes $ \wa ->
    set_override_redirect wa True
    >> changeWindowAttributes display ewmhWin cWOverrideRedirect wa
  mapWindow display ewmhWin

  logHistory <- newIORef []
  
  -- Find and register ourselves with the root window
  -- These masks allow us to intercept various Xorg events useful for a WM
  selectInput display root
    $   substructureNotifyMask
    .|. substructureRedirectMask
    .|. structureNotifyMask
    .|. leaveWindowMask
    .|. enterWindowMask
    .|. buttonPressMask
    .|. buttonReleaseMask
    .|. keyPressMask
    .|. keyReleaseMask

  -- Grabs the initial keybindings and screen list while also setting up EWMH
  screens <- doAll logHistory IM.empty c startingMode display root font $ do
    initEwmh root ewmhWin
    rebindKeys startingMode startingMode
    rootChange
    get @Screens

  -- Normally, Xlib will crash on any error. Calling this function 
  -- asks Xlib to print recoverable errors instead of crashing on them.
  setDefaultErrorHandler
  -- xSetErrorHandler

  -- Focus the root window so we can receive our keybindings.
  setInputFocus display root revertToNone currentTime

  -- Execute the main loop. Will never return unless Xest exits
  -- The finally makes sure we write the last 100 log messages on exit to the
  -- err file.
  E.catch (doAll logHistory screens c startingMode display root font (forever mainLoop)) \(e :: SomeException) -> do
    lastLog <- unlines . reverse <$> readIORef logHistory
    let header = "Xest crashed with the exception: " <> show e <> "\n"
    writeFile "/tmp/xest.err" $ header <> lastLog <> "\n"



-- | Performs the main logic. Does it all!
mainLoop :: M ()
mainLoop = do
  log $ LD "Loop" "\n\n========================"

  -- Check how many events are in the queue and whether someone
  -- has asked us to replace the windows.
  numEvents <- (== 0) <$> checkXEvent
  refreshRequested <- isJust <$> get @(Maybe ())
  when (numEvents && refreshRequested) refresh
  
  -- Here we have the bulk of the program. Most of the events given to us
  -- by the server are just handed off to something in the XEvents file.
  -- A handful of them have slightly more complicated logic.
  getXEvent >>= (\x -> log (LD "Event" $ show x) >> return x) >>= \case
    -- Called when a new window is created by someone
    MapRequestEvent {..} -> do
      -- First, check if it's a dock which should be unmanaged
      nwwtd <- getAtom False "_NET_WM_WINDOW_TYPE_DOCK"
      nwwt <- getAtom False "_NET_WM_WINDOW_TYPE"
      windowType <- getProperty 32 nwwt ev_window
      if elem nwwtd windowType
        then
          addUM ev_window >> put @(Maybe ()) (Just ())
        else do
          rootTiler <- get @Tiler
          unless (findWindow ev_window rootTiler) $
            reparentWin ev_window >>= mapWin
        
    -- Called when a window actually dies.
    DestroyWindowEvent {..} -> killed ev_window
    -- Called when a window is dying. Because X is asynchronous, there's a chance
    -- the window will be dead by the time we get this event.
    UnmapEvent {..} -> unmapWin ev_window
    -- The window tried to change it's own position. Here, we try to deny the
    -- request in a way that doesn't upset the caller.
    cre@ConfigureRequestEvent {} -> configureWin cre
    -- This is usually called when a monitor is connected or disconnected.
    ConfigureEvent {} -> rootChange
    -- The mouse moved from one window to another.
    CrossingEvent {..} -> do
      put @OldTime $ OldTime ev_time
      root <- input @RootWindow
      -- Why the if statement? Well we want to focus the root window
      -- if no other windows are currently focused.
      if | ev_event_type == enterNotify -> newFocus ev_window
         | ev_window == root -> newFocus root
         | otherwise -> return ()
    -- Button in this case means mouse button. Used to trigger click to focus.
    ButtonEvent {..} ->
      put @OldTime (OldTime ev_time) >> newFocus ev_window
    -- The pointer moved and we probably want to resize something.
    MotionEvent {..} -> motion
    -- A press of the keyboard.
    KeyEvent {..} -> put @OldTime (OldTime ev_time) >> keyDown ev_keycode ev_event_type >>= unwrapMonad . foldMap (WrapMonad . executeActions)
    -- This usually means the keyboard layout changed.
    MappingNotifyEvent {} -> reloadConf
    -- Some other window sent us a message. Currently, we only care if they
    -- ask to be fullscreen.
    ClientMessageEvent {..} -> do
      wm_state <- getAtom False "_NET_WM_STATE"
      full <- fromIntegral <$> getAtom False "_NET_WM_STATE_FULLSCREEN"
      let isSet = maybe 0 fromIntegral $ headMay ev_data
      messageType <- fromAtom ev_message_type

      -- messageC <- traverse (fromAtom . fromIntegral) ev_data
      log $ LD "ClientMessage" $ messageType <> "\n\t[MessageData]" <> show ev_data
      log $ LD "MessageData" $ show ev_data

      when (wm_state == ev_message_type && full `elem` ev_data) $
        makeFullscreen ev_window isSet


    -- 21 == reparent event. If a window decides to reparent itself,
    -- it's practically unmapped and dead.
    AnyEvent {ev_event_type = 21, ev_window = window} -> killed window

    _ -> void $ log $ LD "Event" "Got unknown event"

  where
    -- Here we have executors for the various actions a user might
    -- have in their config. These go to Actions/Actions.hs
    executeActions :: Action -> _ ()
    executeActions action = log (LD "Action" $ show action) >> case action of
      RunCommand command -> execute command
      ShowWindow wName -> getWindowByClass wName >>= mapM_ restore
      HideWindow wName -> getWindowByClass wName >>= mapM_ minimize
      ZoomInInput -> zoomInInput
      ZoomOutInput -> zoomOutInput
      ZoomInMonitor -> zoomInMonitor
      ZoomOutMonitor -> zoomOutMonitor
      ZoomMonitorToInput -> zoomMonitorToInput
      ZoomInputToMonitor -> zoomInputToMonitor
      ChangeModeTo mode -> changeModeTo mode
      Move dir -> changeMany $ moveDir dir
      ChangeNamed (Text name) -> maybe (return ()) (changeMany . changeIndex) $ readMaybe name
      PopTiler -> popTiler
      PushTiler -> pushTiler
      Insert -> insertTiler
      MoveToFront -> changeMany moveToFront
      MakeEmpty -> makeEmptySpot
      KillActive -> killActive
      ExitNow -> absurd <$> exit
      ToggleLogging -> toggleLogs
      ChangeToHorizontal -> changeMany toHoriz
      ChangeToFloating -> changeMany toFloating
      ChangeToTwoCols -> changeMany toTwoCols
      SetRotate -> changeMods Rotate
      SetFull -> changeMods Full
      SetNoMod -> changeMods NoMods
      ToggleDocks -> toggleDocks