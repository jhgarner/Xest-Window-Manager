{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import           Foreign.C.String
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Window
import           SDL hiding (get, Window, Display, trace)
import qualified SDL.Raw.Video as Raw
import qualified SDL.Font as Font
-- import qualified SDL.Internal.Types as SI (Window(..))
import           Types
import           Base
import           Tiler
import           Data.Char                      ( ord, chr )
import qualified System.Environment as Env
import qualified Data.Map                      as M
import XEvents
import Actions

-- | Starting point of the program. Should never return
startWM :: IO ()
startWM = do
  -- We want antialiasing on our text and normal Xlib can't give us that.
  -- We use SDL for the window borders to get around that problem.
  initializeAll
  Font.initialize

  -- Grab a display to capture. The chosen display cannot have a WM already running.
  args <- getArgs
  let displayNumber = fromMaybe "0" $ headMay args
  display <- openDisplay $ ":" ++ unpack displayNumber

  -- Read the config file
  homeDir <- Env.getEnv "HOME"
  print displayNumber
  c <- if displayNumber == "0" || displayNumber == "1"
          then readConfig display . pack $ homeDir ++ "/.config/xest/config.dhall"
          else readConfig display . pack $ homeDir ++ "/.config/xest/config.dhall." ++ unpack displayNumber
  print $ if displayNumber == "0" || displayNumber == "1"
            then homeDir ++ "/.config/xest/config.dhall"
            else pack $ homeDir ++ "/.config/xest/config.dhall." ++ unpack displayNumber

  -- X orders windows like a tree.
  -- This gets the root.
  let root = defaultRootWindow display


  -- The initial mode is whatever comes first
  let initialMode = fromMaybe (error "No modes") . headMay $ definedModes c

  -- Create our border windows which will follow the InputController
  -- [lWin, dWin, uWin, rWin] <- replicateM 4 $
  --   SI.Window <$> withCString "fakeWindowDontManage" (\s -> Raw.createWindow s 10 10 10 10 524288)

  -- EWMH wants us to create an extra window to verify we really
  -- can handle some of the EWMH spec. This window does that.
  ewmhWin <- createSimpleWindow display root
          10000 10000 1 1 0 0
          $ whitePixel display (defaultScreen display)
  allocaSetWindowAttributes $ \wa ->
    set_override_redirect wa True
    >> changeWindowAttributes display ewmhWin cWOverrideRedirect wa
  mapWindow display ewmhWin

  runM 
    $ runInputConst display
    $ runInputConst root
    $ evalState (M.empty @String @Atom)
    $ evalState (M.empty @Atom @[Int])
    $ evalState (M.empty @Window @Rect)
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

  -- Grabs the initial keybindings and screen list
  (screens :: Screens) <-
    runM
    $ runInputConst c
    $ runInputConst display
    $ runInputConst root
    $ evalState (M.empty @String @Atom)
    $ evalState (M.empty @Atom @[Int])
    $ evalState (M.empty @Window @Rect)
    $ evalState (M.empty @Int @Screen')
    $ evalState ([] @(Fix Tiler))
    $ evalState (InputController Nothing)
    $ evalState Nothing
    $ evalState (0 :: Int)
    $ runGetScreens
    $ runNewBorders
    $ evalState False
    $ runEventFlags
    $ runProperty
    $ runMoverFake
    $ rebindKeys initialMode initialMode >> rootChange >> get @Screens
  print ("Got Screens" ++ show screens)

  setDefaultErrorHandler

  setInputFocus display root revertToNone currentTime
  -- Execute the main loop. Will never return unless Xest exits
  doAll screens c initialMode display root (forever mainLoop)
    >> say "Exiting"


-- | Performs the main logic. Does it all!
mainLoop :: DoAll r
mainLoop = do
  printMe "\n\n========================"
  printMe "Tiler state at beginning of loop:\n"
  get @ActiveScreen >>= \as -> printMe $ show as ++ " is the active screen \n"
  get @Screens >>= \as -> printMe $ show as ++ " \n\n"

  whenM (isJust <$> get @(Maybe ())) refresh
  
  currentScreen <- get @ActiveScreen

  runInputConst currentScreen $ getXEvent >>= (\x -> printMe ("evaluating event: " ++ show x) >> return x) >>= \case
    MapRequestEvent {..} -> mapWin ev_window
    DestroyWindowEvent {..} -> killed ev_window
    UnmapEvent {..} -> unmapWin ev_window
    cre@ConfigureRequestEvent {} -> configureWin cre
    ConfigureEvent {} -> rootChange
    CrossingEvent {..} -> 
      if ev_event_type == enterNotify
         then newFocus ev_window
         else input @RootWindow >>= newFocus
    ButtonEvent {..} -> 
      newFocus ev_window
    MotionEvent {..} -> motion
    KeyEvent {..} -> keyDown ev_keycode ev_event_type >>= foldMap executeActions
    MappingNotifyEvent {} -> reloadConf
    _ -> return ()

  where
    executeActions :: Action -> DoAll r
    executeActions action = printMe ("Action: " ++ show action) >> case action of
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
      -- TODO recursion alert!
      ChangeModeTo mode -> changeModeTo mode >>= foldMap executeActions
      Move dir -> moveDir dir
      ChangeNamed name -> maybe (return ()) changeIndex $ readMay name
      PopTiler -> popTiler
      PushTiler -> pushTiler
      Insert t -> insertTiler t
      MakeSpecial -> doSpecial
      KillActive -> killActive
      ExitNow -> absurd <$> exit
      ToggleLogging -> toggleLogs
      

