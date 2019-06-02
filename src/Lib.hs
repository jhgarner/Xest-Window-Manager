{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startWM
  )
where

import           ClassyPrelude           hiding ( Reader
                                                , asks
                                                , ask
                                                )
import           Config
import           Polysemy                hiding ( raise )
import           Polysemy.State
import           Polysemy.Reader
import           Core
import           Data.Bits
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Screen
import           Graphics.X11.Xlib.Atom
import           Graphics.X11.Xlib.Window
import           Graphics.X11.Xlib.Misc
import           Types
import           Base
import           Tiler
import           FocusList
import           Data.Functor.Foldable
import           Data.Char                      ( ord )

-- | Starting point of the program. Should never return
startWM :: IO ()
startWM = do
  -- Grab a display to capture. The chosen display cannot have a WM already running.
  -- TODO use variables to determine display number
  display <- openDisplay ":99"

  -- Read the config file
  c <- readConfig display "./config.conf"

  -- X orders windows like a tree
  let root = defaultRootWindow display

  -- Perform various pure actions for getting state
  let screen      = defaultScreenOfDisplay display
      initialMode = head . impureNonNull $ definedModes c
      dims        = (widthOfScreen screen, heightOfScreen screen)
      rootTiler   = InputController . Fix . Directional X $ emptyFL
  -- Create our border windows which will follow the InputController
  lWin <- createSimpleWindow display root 10 10 300 300 0 0 (whitePixel display (defaultScreen display))
  rWin <- createSimpleWindow display root 10 10 300 300 0 0 (whitePixel display (defaultScreen display))
  dWin <- createSimpleWindow display root 10 10 300 300 0 0 (whitePixel display (defaultScreen display))
  uWin <- createSimpleWindow display root 10 10 300 300 0 0 (whitePixel display (defaultScreen display))
  allocaSetWindowAttributes $ \wa -> set_override_redirect wa True >> changeWindowAttributes display lWin cWOverrideRedirect wa
  allocaSetWindowAttributes $ \wa -> set_override_redirect wa True >> changeWindowAttributes display rWin cWOverrideRedirect wa
  allocaSetWindowAttributes $ \wa -> set_override_redirect wa True >> changeWindowAttributes display dWin cWOverrideRedirect wa
  allocaSetWindowAttributes $ \wa -> set_override_redirect wa True >> changeWindowAttributes display uWin cWOverrideRedirect wa
  mapWindow display lWin
  mapWindow display dWin
  mapWindow display uWin
  mapWindow display rWin

  -- Find and register ourselves with the root window
  -- These two masks allow us to intercept various Xorg events useful for a WM
  initEwmh display root
  selectInput display root
    $   substructureNotifyMask
    .|. substructureRedirectMask
    .|. enterWindowMask



  -- xSetErrorHandler
  -- Grabs the initial keybindings
  _ <-
    runM
    $ runReader c
    $ runReader display
    $ runReader root
    $ runGlobalX
    $ rebindKeys initialMode

  -- Execute the main loop. Will never return unless Xest exits
  doAll rootTiler c initialMode dims display root (lWin, dWin, uWin, rWin) (chain mainLoop [])
    >> say "Exiting"

    -- Chain takes a function and calls it over and over tying it into itself
  where chain f initial = f initial >>= chain f


-- | Performs the main logic. The return of one call becomes the input for the next
mainLoop :: Actions -> DoAll r
-- When there are no actions to perform, render the windows and find new actions to do
mainLoop [] = do
  modify $ cata $ Fix . reduce
  get >>= render
  get @(Tiler (Fix Tiler)) >>= \d -> trace (show d) return ()
  makeTopWindows
  get >>= writeWorkspaces . onInput getDesktopState
  doIt
    where isConfNot ConfigureEvent {} = True
          isConfNot _ = False
          -- TODO switch to a prelude with UntilM
          doIt = do
            ptr <- getXEvent
            -- trace (show ptr) return ()
            if isConfNot ptr then doIt else return [XorgEvent ptr]


-- When there are actions to perform, do them and add the results to the list of actions
mainLoop (a : as) = do
  newActions <- handler a
  -- Post processors can override events
  postResult <-
    \case
        Default  -> []
        New  o k -> newModePostprocessor o k a
        Temp o k -> tempModePostprocessor o k a
      <$> get
  return $ newActions ++ as ++ postResult

-- TODO move to Polysemy
getAtom :: Display -> String -> IO Atom
getAtom display t = internAtom display t False

initEwmh :: Display -> Window -> IO ()
initEwmh display root = do
  a    <- getAtom display "_NET_SUPPORTED"
  c    <- getAtom display "ATOM"
  supp <- mapM
    (getAtom display)
    [ "_NET_NUMBER_OF_DESKTOPS"
    , "_NET_NUMBER_OF_DESKTOPS"
    , "_NET_CURRENT_DESKTOP"
    ]
  changeProperty32 display root a c propModeReplace (fmap fromIntegral supp)


writeWorkspaces
  :: (Members '[PropertyWriter, Reader Window] r)
  => ([Text], Int)
  -> Sem r ()
writeWorkspaces (names, i) = do
  root <- ask
  setProperty8 "_NET_DESKTOP_NAMES" "UTF8_STRING" root
    $ concatMap ((++ [0]) . fmap ord . unpack) names
  setProperty32 "_NET_NUMBER_OF_DESKTOPS" "CARDINAL" root [length names, 0]
  setProperty32 "_NET_CURRENT_DESKTOP"    "CARDINAL" root [i, 0]

makeTopWindows
  :: (Members '[PropertyReader, GlobalX, WindowMover] r)
  => Sem r ()
makeTopWindows = do
  wins <- getTree
  forM_ wins $ \win -> do
    prop <- getProperty 32 "_NET_WM_STATE" win
    case prop of
      Nothing -> return ()
      Just states ->
        whenM
            (   any (&& True)
            <$> traverse (isSameAtom "_NET_WM_STATE_ABOVE") states
            )
          $ raise win
