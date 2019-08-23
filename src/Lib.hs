{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startWM
  )
where

import           Standard
import           Config
import           Polysemy                hiding ( raise )
import           Polysemy.State
import           Polysemy.Reader
import           Core
import           Data.Bits
import           Foreign.Ptr
import           Foreign.C.String
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Screen
import           Graphics.X11.Xlib.Atom
import           Graphics.X11.Xlib.Window (createSimpleWindow, mapWindow)
import           Graphics.X11.Xlib.Misc
import           SDL hiding (get, Window, Display, trace)
import qualified SDL.Raw.Video as Raw
import qualified SDL.Font as Font
import qualified SDL.Internal.Types as SI (Window(..))
import           Types
import           Base
import           Tiler
import           FocusList
import           Data.Char                      ( ord )

-- | Starting point of the program. Should never return
startWM :: IO ()
startWM = do
  -- We want antialiasing on our text and normal Xlib can't give us that.
  -- We use SDL for the window borders to get around that problem.
  initializeAll
  Font.initialize

  -- Grab a display to capture. The chosen display cannot have a WM already running.
  -- TODO use variables to determine display number
  display <- openDisplay ":99"

  -- Read the config file
  c <- readConfig display "./config.conf"

  -- X orders windows like a tree
  let root = defaultRootWindow display

  -- Perform various pure actions for getting state
  let screen      = defaultScreenOfDisplay display
      initialMode = fromMaybe (error "No modes") . headMay $ definedModes c
      dims        = (widthOfScreen screen, heightOfScreen screen)
      rootTiler   = InputController Nothing

  -- Create our border windows which will follow the InputController
  [lWin, dWin, uWin, rWin] <- replicateM 4 $ do
    print "Yay"
    -- win <- createSimpleWindow display root
    --        10 10 300 300 0 0
    --        $ whitePixel display (defaultScreen display)
    win <- SI.Window <$> withCString "fakeWindowDontManage" (\s -> Raw.createWindow s 10 10 10 10 524288)
    -- win <- SI.Window <$> withCString "test" (\s -> Raw.createWindow s 0 0 100 100 0)
    winSurface <- SDL.getWindowSurface win

    SDL.surfaceFillRect winSurface Nothing $ SDL.V4 255 255 255 0
    SDL.updateWindowSurface win
    -- id <- Raw.getWindowID sWin
    -- print id
    -- allocaSetWindowAttributes $ \wa ->
    --   set_override_redirect wa True
    --   >> changeWindowAttributes display win cWOverrideRedirect wa
    -- mapWindow display win
    print win
    -- print (fromIntegral win :: Word32)
    -- window <- undefined--SI.Window <$> getWindowFromID (fromIntegral win)

    -- print window
    return win

  -- Find and register ourselves with the root window
  -- These two masks allow us to intercept various Xorg events useful for a WM
  initEwmh display root
  selectInput display root
    $   substructureNotifyMask
    .|. substructureRedirectMask
    .|. enterWindowMask
    .|. buttonPressMask
    .|. buttonReleaseMask
  -- grabButton display anyButton anyModifier root False (buttonPressMask .|. buttonReleaseMask) grabModeSync grabModeAsync none none

  -- allowEvents display (replayPointer .|. replayKeyboard .|. asyncBoth) currentTime
  -- xSetErrorHandler
  -- Grabs the initial keybindings
  _ <-
    runM
    $ runReader c
    $ runReader display
    $ runReader root
    $ runGlobalX
    $ rebindKeys initialMode

  setDefaultErrorHandler

  -- Execute the main loop. Will never return unless Xest exits
  doAll rootTiler c initialMode dims display root (lWin, dWin, uWin, rWin) (chain mainLoop [])
    >> say "Exiting"

    -- Chain takes a function and calls it over and over tying it into itself
  where chain f initial = f initial >>= chain f

-- | Performs the main logic. The return of one call becomes the input for the next
mainLoop :: Actions -> DoAll r
-- When there are no actions to perform, render the windows and find new actions to do
mainLoop [] = do
  -- modify $ cata $ Fix . reduce
  xFocus
  -- get @(Tiler (Fix Tiler)) >>= \t -> trace ("\n"++show t) return ()
  get >>= render
  makeTopWindows
  writePath
  get >>= writeWorkspaces . fromMaybe (["Nothing"], 0) . onInput (fmap (getDesktopState . unfix))
  l <- sequence [XorgEvent <$> getXEvent]
  -- trace ((\[XorgEvent t] -> show t) l) return l
  return l
  -- return []

-- When there are actions to perform, do them and add the results to the list of actions
mainLoop (a : as) = do
  -- trace (show a) return ()
  newActions <- handler a
  -- Post processors can override state changes
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
