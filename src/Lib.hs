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
import           Polysemy                hiding ( )
import           Polysemy.State
import           Polysemy.Input
import           Core
import           Data.Bits
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
import XEvents
import Actions.ActionTypes
import Actions.Actions
import qualified Control.Exception as E

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
  args <- getArgs
  let displayNumber = fromMaybe "1" $ headMay args
  display <- openDisplay $ ":" ++ unpack displayNumber

  -- Read the config file from the user's home directory.
  -- If using display number 0 or 1, you're probably launching a real
  -- environment using startx or some display manager. In that case,
  -- we just launch the config.dhall file. If you run Xest on a different
  -- display, it likely means you're testing and want a slightly different
  -- config. For example, my testing config uses Alt instead of the Super
  -- (Windows) key.
  homeDir <- Env.getEnv "HOME"
  print displayNumber
  c <- if displayNumber == "0" || displayNumber == "1"
          then readConfig display . pack $ homeDir ++ "/.config/xest/config.dhall"
          else readConfig display . pack $ homeDir ++ "/.config/xest/config." ++ unpack displayNumber ++ ".dhall"
  let startingMode = initialMode c
  font <- Font.load (fontLocation c) 18

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

  -- Here we have our first look at Polysemy. All those evalState calls
  -- might seem weird, but they're just setting up the caching environment
  -- that the property effects like to use. All of this ceremony is just
  -- needed to initialize all of the ewmh code.
  runM 
    $ runInputConst display
    $ runInputConst root
    $ evalState (M.empty @String @Atom)
    $ evalState (M.empty @Atom @[Int])
    $ evalState (M.empty @Window @XRect)
    $ evalState False
    $ evalState c
    $ runExecutor 
    $ runProperty 
    $ initEwmh root ewmhWin

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

  -- Grabs the initial keybindings and screen list.
  -- Once again we have a lot of Polysemy stuff going on which can
  -- be safely ignored. The only interesting finction is the last one
  -- in the chain.
  (screens :: Screens) <-
    runM
    $ runInputConst c
    $ runInputConst display
    $ runInputConst root
    $ evalState (M.empty @String @Atom)
    $ evalState (M.empty @Atom @[Int])
    $ evalState (M.empty @Window @XRect)
    $ evalState (M.empty @Int @Screen')
    $ evalState ([] @SubTiler)
    $ evalState @Tiler (InputController Nothing)
    $ evalState Nothing
    $ evalState (0 :: Int)
    $ runGetScreens
    $ runNewBorders
    $ evalState False
    $ runEventFlags
    $ evalState c
    $ runExecutor 
    $ runProperty
    $ runMoverFake
    $ rebindKeys startingMode startingMode >> rootChange >> get @Screens
  print ("Got Screens" ++ show screens)

  -- Normally, Xlib will crash on any error. Calling this function 
  -- asks Xlib to print recoverable errors instead of crashing on them.
  setDefaultErrorHandler
  -- xSetErrorHandler

  -- Focus the root window so we can receive our keybindings.
  setInputFocus display root revertToNone currentTime

  -- Execute the main loop. Will never return unless Xest exits
  -- The finally makes sure we write the last 100 log messages on exit to the
  -- err file.
  logHistory <- newIORef []
  E.catch (doAll logHistory screens c startingMode display root font (forever mainLoop)) \(e :: SomeException) -> do
    lastLog <- unlines . reverse <$> readIORef logHistory
    let header = "Xest crashed with the exception: " ++ show e ++ "\n"
    writeFile "/tmp/xest.err" (fromString $ header ++ lastLog ++ "\n")



-- | Performs the main logic. Does it all!
-- NOTE: Because I'm using a wildcard, mainLoop tends to completely blow up on
-- any kind of type error. If that happens and you can't figure out what is
-- actually wrong, you need to figure out the type of doAll and paste that in
-- here. Once you've finished debugging, you can replace it with the _ again or
-- find a better way to do this.
mainLoop :: Sem _ ()
mainLoop = do
  -- These debug lines have saved me countless times.
  log "\n\n========================"

  -- Check how many events are in the queue and whether someone
  -- has asked us to replace the windows.
  numEvents <- (== 0) <$> checkXEvent
  refreshRequested <- isJust <$> get @(Maybe ())
  when (numEvents && refreshRequested) refresh
  
  -- Note that this doesn't correspond to an X11 screen but to an
  -- Xrandr or Xinerama screen.
  currentScreen <- get @ActiveScreen

  -- Here we have the bulk of the program. Most of the events given to us
  -- by the server are just handed off to something in the XEvents file.
  -- A handful of them have slightly more complicated logic.
  runInputConst currentScreen $ getXEvent >>= (\x -> log ("[Event] " ++ show x) >> return x) >>= \case
    -- Called when a new window is created by someone
    MapRequestEvent {..} ->
      -- If we think the window is currently managed, unmap it first.
      unlessM (findWindow ev_window <$> get @Tiler) $
        -- unmapWin ev_window
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
      put ev_time
      root <- input @RootWindow
      -- Why the if statement? Well we want to focus the root window
      -- if no other windows are currently focused.
      if | ev_event_type == enterNotify -> newFocus ev_window
         | ev_window == root -> newFocus root
         | otherwise -> return ()
    -- Button in this case means mouse button. Used to trigger click to focus.
    ButtonEvent {..} ->
      put ev_time >> newFocus ev_window
    -- The pointer moved and we probably want to resize something.
    MotionEvent {..} -> motion
    -- A press of the keyboard.
    KeyEvent {..} -> put ev_time >> keyDown ev_keycode ev_event_type >>= foldMap executeActions
    -- This usually means the keyboard layout changed.
    MappingNotifyEvent {} -> reloadConf
    -- Some other window sent us a message. Currently, we only care if they
    -- ask to be fullscreen.
    ClientMessageEvent {..} -> do
      wm_state <- getAtom False "_NET_WM_STATE"
      full_screen <- getAtom False "_NET_WM_STATE_FULLSCREEN"
      let isSet = (== Just 1) $ headMay ev_data
      messageType <- fromAtom ev_message_type

      -- messageC <- traverse (fromAtom . fromIntegral) ev_data
      log $ "[ClientMessage] " ++ messageType ++ "\n\t[MessageData]" ++ show ev_data

      when (wm_state == ev_message_type) $
        when (fromIntegral full_screen `elem` ev_data) $ do
          if isSet
             then makeFullscreen ev_window >> newFocus ev_window
             -- Why change the location? Well Chrome seems to expect that we
             -- restore it to it's original size. By making it small then
             -- changing back to the full size, we force it to render
             -- correctly.
             else changeLocation (ParentChild ev_window ev_window) $ Rect 1 1 1 1
          put $ Just ()


    -- 21 == reparent event. If a window decides to reparent itself,
    -- it's practically unmapped and dead.
    AnyEvent {ev_event_type = 21, ev_window = window} -> killed window

    _ -> void $ log "Got unknown event"

  where
    -- Here we have executors for the various actions a user might
    -- have in their config. These go to Actions/Actions.hs
    executeActions :: Action -> Sem _ ()
    executeActions action = log ("[Action] " ++ show action) >> case action of
      RunCommand command -> execute command
      ShowWindow wName -> getWindowByClass wName >>= mapM_ restore
      HideWindow wName -> getWindowByClass wName >>= mapM_ minimize
      ZoomInInput -> zoomInInput
      ZoomInInputSkip -> zoomDirInputSkip undefer zoomInInput
      ZoomOutInput -> zoomOutInput
      ZoomOutInputSkip -> zoomDirInputSkip (fromMaybe [] . tailMay . deferred) zoomOutInput
      ZoomInMonitor -> zoomInMonitor
      ZoomOutMonitor -> zoomOutMonitor
      ZoomMonitorToInput -> zoomMonitorToInput
      ZoomInputToMonitor -> zoomInputToMonitor
      ChangeModeTo mode -> changeModeTo mode
      Move dir -> changeMany $ moveDir dir
      ChangeNamed name -> maybe (return ()) (changeMany . changeIndex) $ readMay name
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
      

