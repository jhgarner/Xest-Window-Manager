{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
  ( startWM
  )
where

import           ClassyPrelude
import           Config
import           Control.Lens
import           Control.Monad.State.Lazy (get, gets, fix)
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
import           Types
import           Tiler
import           Data.Functor.Foldable
import           Data.Char                      ( ord )
import qualified Data.Vector                   as V
import qualified Data.Set                      as S

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
    (substructureNotifyMask .|. substructureRedirectMask .|. enterWindowMask)

  -- Read the config file
  c <- readConfig display "./config.conf"

  -- Perform various pure actions for getting the iteration state
  let screen      = defaultScreenOfDisplay display
      -- TODO don't use impure functions here
      initialMode = head . impureNonNull $ definedModes c
      iState =
        IS display root (widthOfScreen screen, heightOfScreen screen) c Nothing

  -- Grabs the initial keybindings
  _ <- runXest iState (error "No event state") $ rebindKeys initialMode

  -- Execute the main loop. Will never return unless Xest exits
  mainLoop iState $ ES
    (Fix . InputController . Fix . Directional X $ FL 0 V.empty)
    initialMode
    handler
    S.empty

getAtom :: Display -> String -> IO Atom
getAtom display t = internAtom display t False

initEwmh :: Display -> Window -> IO ()
initEwmh display root = do
  a    <- getAtom display "_NET_SUPPORTED"
  c    <- getAtom display "ATOM"
  supp <- mapM (getAtom display)
               ["_NET_NUMBER_OF_DESKTOPS", "_NET_NUMBER_OF_DESKTOPS", "_NET_CURRENT_DESKTOP"]
  changeProperty32 display root a c propModeReplace (fmap fromIntegral supp)


writeWorkspaces :: ([Text], Int) -> Xest ()
writeWorkspaces (names, i) = do
  root    <- asks rootWin
  display <- asks display
  dNames  <- liftIO $ internAtom display "_NET_DESKTOP_NAMES" False
  numD  <- liftIO $ internAtom display "_NET_NUMBER_OF_DESKTOPS" False
  currentD  <- liftIO $ internAtom display "_NET_CURRENT_DESKTOP" False
  utf8    <- liftIO $ internAtom display "UTF8_STRING" False
  card    <- liftIO $ internAtom display "CARDINAL" False
  liftIO
    $ changeProperty8 display root dNames utf8 propModeReplace
    $ map fromIntegral
    $ concatMap ((++ [0]) . fmap ord . unpack) names
  liftIO $ changeProperty32 display root numD card propModeReplace [fromIntegral $ length names, 0]
  liftIO $ changeProperty32 display root currentD card propModeReplace [fromIntegral i, 0]

makeTopWindows :: Xest ()
makeTopWindows = do
  display <- asks display
  wins <- getWindows
  forM_ wins $ \win -> do
    wmState  <- liftIO $ internAtom display "_NET_WM_STATE" False
    above  <- (liftIO $ internAtom display "_NET_WM_STATE_ABOVE" False) :: Xest Word64
    WindowAttributes { wa_map_state = ms } <- liftIO
      $ getWindowAttributes display win
    when (ms /= waIsUnmapped) $ do
      prop <- liftIO $ rawGetWindowProperty 32 display wmState win :: Xest (Maybe [Word64])
      case prop of
        Nothing -> return ()
        Just states ->  do
          print win
          when (isJust $ find (== above) states) $ liftIO $ raiseWindow display win
-- | Performs the event loop recursion inside of the Xest Monad
-- The return value of one iteration becomes the input for the next
mainLoop :: IterationState -> EventState -> IO ()
mainLoop iState@IS {..} eventState =
  runXest iState eventState (chain recurse []) >> say "Exiting"
 where
  chain f initial = fix (\rec b -> b >>= rec . f) $ return initial

  -- Performs the actual looping
  recurse :: Actions -> Xest Actions
  -- When there are no actions to perform, find new ones
  recurse [] = do
    gets _desktop >>= liftIO . print
    get >>= render
    makeTopWindows
    gets _desktop >>= writeWorkspaces . onInput getDesktopState
    ptr <- liftIO . allocaXEvent $ \p -> nextEvent display p >> getEvent p
    return [XorgEvent ptr]

  -- When there are actions to perform, do them and add the results to the list of actions
  recurse (a : as) = do
    -- liftIO $ print a
    es       <- get
    newEvent <- view keyParser es a
    return $ as ++ newEvent
