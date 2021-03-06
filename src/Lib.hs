{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( startWM,
  )
where

import Actions.ActionTypes
import Actions.Actions
import Base.DoAll
import Config
import qualified Control.Exception as E
import Core
import qualified Data.IntMap as IM
import Graphics.X11.Types
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Window
import SDL hiding (Display, Event, Mode, Window, createWindow, get, trace)
import qualified SDL.Font as Font
import Standard
import qualified System.Environment as Env
import Text.Regex (mkRegex, subRegex)
import Tiler.Tiler
import XEvents

-- | Wraps Xest in some basic logging and error handling.
startWM :: IO ()
startWM = do
  -- This should only get filled if some part of initialization fails
  E.catch runWM \(e :: SomeException) -> do
    writeFile "/tmp/xest_init.err" $ Text $ filterAnsi $ displayException e
  where
    ansiRegex = mkRegex "\\[[0-9;?]+m"
    filterAnsi line = subRegex ansiRegex stripped ""
      where
        stripped = mfilter (/= '\ESC') line

-- | Starting point of the program. This function should never return
runWM :: IO ()
runWM = do
  -- We want antialiasing on our text and normal Xlib can't give us that.
  -- We use SDL for the window borders to get around that problem.
  -- Here we initialize SDL and its cool fonts.
  initializeAll
  Font.initialize

  -- Grab a display to capture. The chosen display cannot have a WM already running.
  -- We grab it based on the arguments passed to Xest.
  -- By default we pick 1 since that seems to be what GDM offers most
  -- of the time. If you launch it with startx, you probably want 0.
  displayNumber <- fromJust . tailMay <$> Env.getEnv "DISPLAY"
  display <- openDisplay . view _Text $ ":" <> displayNumber

  -- Read the config file from the user's home directory or /etc/ If you run
  -- Xest on a different display, it likely means you're testing and want a
  -- slightly different config. For example, my testing config uses Alt instead
  -- of the Super (Windows) key.
  c <- readConfig display

  let startingMode = initialMode c
  font <- Font.load (view _Text $ fontLocation c) 18

  -- X orders windows like a tree.
  -- This gets the root of said tree.
  let root = defaultRootWindow display

  -- Set the cursor for the root window
  -- 132 is the magic number for the normal arrow
  cursor <- createFontCursor display 132
  defineCursor display root cursor

  -- EWMH wants us to create an extra window to verify we really
  -- can handle some of the EWMH spec. This window does that.
  -- Since we don't want the window to be visible, we give it a crazy
  -- location. We alse set_override_redirect because Xest shouldn't be
  -- alerted if the window gets moved around.
  -- We also use this window to capture the mouse in Normal mode. That's why
  -- it's inputOnly and really big.
  ewmhWin <-
    allocaSetWindowAttributes $ \wa -> do
      set_override_redirect wa True
      createWindow
        display
        root
        100000
        100000
        1
        1
        0
        copyFromParent
        inputOutput
        -- Visual should take copyFromParent as a parameter I think...
        (unsafeCoerce copyFromParent)
        cWOverrideRedirect
        wa
  mapWindow display ewmhWin

  logHistory <- newIORef []

  -- Find and register ourselves with the root window
  -- These masks allow us to intercept various Xorg events useful for a WM
  selectInput display root $
    substructureNotifyMask
      .|. substructureRedirectMask
      .|. structureNotifyMask
      .|. leaveWindowMask
      .|. enterWindowMask
      .|. buttonPressMask
      .|. buttonReleaseMask
      .|. keyPressMask
      .|. keyReleaseMask

  -- Grabs the initial keybindings and screen list while also setting up EWMH
  screens <- doAll logHistory IM.empty c startingMode display root font cursor $ do
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
  E.catch (doAll logHistory screens c startingMode display root font cursor (getXEvents >>= overStream mainLoop)) \(e :: SomeException) -> do
    lastLog <- unlines . reverse <$> readIORef logHistory
    let header = "Xest crashed with the exception: " <> Text (displayException e) <> "\n"
    writeFile "/tmp/xest.err" $ header <> lastLog <> "\n"

-- | Performs the main logic. Does it all!
mainLoop :: Event -> Eff _ ()
mainLoop event = do
  log $ LD "Loop" "\n\n========================"

  -- Here we have the bulk of the program. Most of the events given to us
  -- by the server are just handed off to something in the XEvents file.
  -- A handful of them have slightly more complicated logic.
  log (LD "Event" $ show event)
  case event of
    -- Called when a new window is created by someone
    MapRequestEvent {..} -> do
      -- First, check if it's a dock which should be unmanaged
      nwwtd <- getAtom False "_NET_WM_WINDOW_TYPE_DOCK"
      nwwt <- getAtom False "_NET_WM_WINDOW_TYPE"
      windowType <- getProperty 32 nwwt ev_window
      if nwwtd `elem` windowType
        then addUM ev_window >> put @ShouldRedraw (Just UnsafeRedraw)
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
      if
          | ev_event_type == enterNotify -> newFocus ev_window
          -- This means we left a window. We're either going to get an
          -- enterNotify event which immediately overwrites this action, or we
          -- moved from one monitor to another and want to change screens.
          | otherwise -> setScreenFromMouse

    -- Button in this case means mouse button. Used to trigger click to focus.
    ButtonEvent {..} -> do
      put @OldTime (OldTime ev_time)
      put @OldMouseButtons $ OMB None
      newFocus ev_window

    -- The pointer moved and we probably want to resize something.
    MotionEvent {..} -> motion

    -- A press of the keyboard.
    KeyEvent {..} -> put @OldTime (OldTime ev_time) >> keyDown ev_keycode ev_event_type >>= foldMap executeActions

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

  -- Move all of the windows based on how our internal state changed
  refreshRequested <- isJust <$> get @ShouldRedraw
  when refreshRequested refresh
  where
    -- Here we have executors for the various actions a user might
    -- have in their config. These go to Actions/Actions.hs
    executeActions :: Action -> Eff _ ()
    executeActions action =
      log (LD "Action" $ show action) >> case action of
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
        MoveToLoc i -> changeMany (moveToLoc $ fromIntegral i)
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
        ChangeActiveScreen d -> changeActiveScreen d
