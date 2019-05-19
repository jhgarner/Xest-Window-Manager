{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startWM
  )
where

import           ClassyPrelude hiding (Reader, asks, ask)
import           Config
import Polysemy hiding (raise)
import Polysemy.State
import Polysemy.Reader
import           Core
import           Data.Bits
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Screen
import           Graphics.X11.Xlib.Atom
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

  -- Find and register ourselves with the root window
  -- These two masks allow us to intercept various Xorg events useful for a WM
  let root = defaultRootWindow display
  initEwmh display root
  selectInput
    display
    root
    $ substructureNotifyMask .|. substructureRedirectMask .|. enterWindowMask

  -- Read the config file
  c <- readConfig display "./config.conf"

  -- Perform various pure actions for getting the iteration state
  let screen      = defaultScreenOfDisplay display
      initialMode = head . impureNonNull $ definedModes c
      dims = (widthOfScreen screen, heightOfScreen screen)
      rootTiler = InputController . Fix . Directional X $ emptyFL

  -- Grabs the initial keybindings
  _ <- runM $ runReader c $ runReader display $ runReader root $ runGlobalX $ rebindKeys initialMode

  -- Execute the main loop. Will never return unless Xest exits
  doAll rootTiler c initialMode dims display root (chain mainLoop []) >> say "Exiting"
  
    -- Chain takes a function and calls it over and over tying it into itself
    where chain f initial = f initial >>= chain f


-- | Performs the main logic. The return of one call becomes the input for the next
mainLoop :: Actions -> DoAll r
-- When there are no actions to perform, render the windows and find new actions to do
mainLoop [] = do
  modify $ cata $ Fix . reduce
  get @((Fix Tiler)) >>= \r -> trace (show r) return ()
  trace (show . reduce $ add Back Focused (Fix EmptyTiler) (Wrap 5)) return ()
  get >>= render
  makeTopWindows
  get >>= writeWorkspaces . onInput getDesktopState
  ptr <- getXEvent
  return [XorgEvent ptr]

-- When there are actions to perform, do them and add the results to the list of actions
mainLoop (a : as) = do
  preResult <- \case
    Default -> []
    New o k -> newModePreprocessor o k a
    Temp o k -> tempModePreprocessor o k a
    <$> get
  newActions <- handler a
  return $ as ++ preResult ++ newActions

getAtom :: Display -> String -> IO Atom
getAtom display t = internAtom display t False

initEwmh :: Display -> Window -> IO ()
initEwmh display root = do
  a    <- getAtom display "_NET_SUPPORTED"
  c    <- getAtom display "ATOM"
  supp <- mapM (getAtom display)
               ["_NET_NUMBER_OF_DESKTOPS", "_NET_NUMBER_OF_DESKTOPS", "_NET_CURRENT_DESKTOP"]
  changeProperty32 display root a c propModeReplace (fmap fromIntegral supp)


writeWorkspaces :: (Member PropertyWriter r, Member (Reader Window) r) => ([Text], Int) -> Semantic r ()
writeWorkspaces (names, i) = do
  root    <- ask
  setProperty8 "_NET_DESKTOP_NAMES" "UTF8_STRING" root
    $ concatMap ((++ [0]) . fmap ord . unpack) names
  setProperty32 "_NET_NUMBER_OF_DESKTOPS" "CARDINAL" root [length names, 0]
  setProperty32 "_NET_CURRENT_DESKTOP" "CARDINAL" root [i, 0]

makeTopWindows :: (Member PropertyReader r, Member GlobalX r, Member WindowMover r) => Semantic r ()
makeTopWindows = do
  wins <- getTree
  forM_ wins $ \win -> do
    prop <- getProperty 32 "_NET_WM_STATE" win
    case prop of
      Nothing -> return ()
      Just states -> 
        whenM (any (&& True) <$> traverse (isSameAtom  "_NET_WM_STATE_ABOVE") states) $ raise win
