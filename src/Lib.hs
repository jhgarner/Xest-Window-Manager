{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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
import           Foreign.C.String
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Atom
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Window
import           SDL hiding (get, Window, Display, trace)
import qualified SDL.Raw.Video as Raw
import qualified SDL.Font as Font
import qualified SDL.Internal.Types as SI (Window(..))
import           Types
import           Base
import           Tiler
import           Data.Char                      ( ord, chr )
import qualified System.Environment as Env

-- | Starting point of the program. Should never return
startWM :: IO ()
startWM = do
  -- We want antialiasing on our text and normal Xlib can't give us that.
  -- We use SDL for the window borders to get around that problem.
  initializeAll
  Font.initialize

  -- Grab a display to capture. The chosen display cannot have a WM already running.
  -- TODO use variables to determine display number
  args <- getArgs
  let displayNumber = fromMaybe "0" $ headMay args
  display <- openDisplay $ ":" ++ unpack displayNumber

  -- Read the config file
  homeDir <- Env.getEnv "HOME"
  c <- readConfig display . pack $ homeDir ++ "/.config/xest/config.conf"

  -- X orders windows like a tree
  let root = defaultRootWindow display


  -- Perform various pure actions for getting state
  let screen      = defaultScreenOfDisplay display
      initialMode = fromMaybe (error "No modes") . headMay $ definedModes c

  -- Create our border windows which will follow the InputController
  [lWin, dWin, uWin, rWin] <- replicateM 4 $
    SI.Window <$> withCString "fakeWindowDontManage" (\s -> Raw.createWindow s 10 10 10 10 524288)

  -- EWMH wants us to create an extra window to verify we really
  -- can handle some of the EWMH spec. This window does that.
  ewmhWin <- createSimpleWindow display root
          10000 10000 1 1 0 0
          $ whitePixel display (defaultScreen display)
  allocaSetWindowAttributes $ \wa ->
    set_override_redirect wa True
    >> changeWindowAttributes display ewmhWin cWOverrideRedirect wa
  mapWindow display ewmhWin

  -- Find and register ourselves with the root window
  -- These two masks allow us to intercept various Xorg events useful for a WM
  initEwmh display root ewmhWin
  selectInput display root
    $   substructureNotifyMask
    .|. substructureRedirectMask
    .|. structureNotifyMask
    .|. leaveWindowMask
    .|. enterWindowMask
    .|. buttonPressMask
    .|. buttonReleaseMask
  -- grabButton display anyButton anyModifier root False (buttonPressMask .|. buttonReleaseMask) grabModeSync grabModeAsync none none

  -- allowEvents display (replayPointer .|. replayKeyboard .|. asyncBoth) currentTime
  -- xSetErrorHandler
  -- Grabs the initial keybindings
  screens <-
    runM
    $ runReader c
    $ runReader display
    $ runReader root
    $ runGlobalX
    $ rebindKeys initialMode  >> getScreens
  let Just (Fix rootTiler)   = foldl' (\acc (i, _) -> Just . Fix . Monitor i . Just . Fix . InputController i $ acc) Nothing $ zip [0..] screens

  setDefaultErrorHandler

  -- Execute the main loop. Will never return unless Xest exits
  doAll rootTiler c initialMode display root (lWin, dWin, uWin, rWin) (chain mainLoop [])
    >> say "Exiting"

    -- Chain takes a function and calls it over and over tying it into itself
  where chain f initial = f initial >>= chain f

-- | Performs the main logic. The return of one call becomes the input for the next
mainLoop :: Actions -> DoAll r
-- When there are no actions to perform, render the windows and find new actions to do
mainLoop [] = do
  pointer <- getPointer
  screens <- getScreens
  let i = maybe 0 fst $ whichScreen pointer $ zip [0..] screens
  -- modify $ cata $ Fix . reduce
  get @(Tiler (Fix Tiler)) >>= \t -> printMe (show t ++ "\n\n")
  screens <- getScreens
  printMe $ "Got screens" ++ show screens ++ "\n\n"
  whenM (not <$> checkXEvent) $ do
    -- tell X to focus whatever we're focusing
    xFocus

    -- Write the path to the upper border
    writePath

    -- restack all of teh windows
    topWindows <- makeTopWindows
    bottomWindows <- getBottomWindows
    get >>= render >>= restack . \wins -> topWindows ++ bottomWindows ++ wins


    -- Do some EWMH stuff
    setClientList
    writeActiveWindow
    get >>= writeWorkspaces . fromMaybe (["Nothing"], 0) . onInput i (fmap (getDesktopState . unfix))
  -- waiting <- checkXEvent
  l <- pure . XorgEvent <$> getXEvent
  tree <- getTree
  props <- traverse (\w -> (,w) . fmap (fmap (chr . fromIntegral)) <$> (getProperty @_ @Word8) 8 "WM_NAME" w) tree
  printMe ((\[XorgEvent t] -> "\n==================\n\n" ++ show props ++ "\n\n" ++ show t) l)
  return l

-- When there are actions to perform, do them and add the results to the list of actions
mainLoop (a : as) = do
  printMe ("On action: "++ show a ++ "\n\n")
  printMe ("Rest is: " ++ show as ++ "\n\n")
  get @(Tiler (Fix Tiler)) >>= \t -> printMe (show t ++ "\n\n")
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

initEwmh :: Display -> Window -> Window -> IO ()
initEwmh display root upper = do
  a    <- getAtom display "_NET_SUPPORTED"
  nswc <- getAtom display "_NET_SUPPORTING_WM_CHECK"
  xestName <- getAtom display "xest"
  c    <- getAtom display "ATOM"
  supp <- mapM
    (getAtom display)
    [ "_NET_NUMBER_OF_DESKTOPS"
    , "_NET_CURRENT_DESKTOP"
    , "_NET_CLIENT_LIST"
    , "_NET_ACTIVE_WINDOW"
    , "_NET_SUPPORTING_WM_CHECK"
    ]
  changeProperty32 display root a c propModeReplace (fmap fromIntegral supp)
  changeProperty32 display root a c propModeReplace [fromIntegral upper]
  changeProperty32 display upper a c propModeReplace [fromIntegral xestName]


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

-- Some windows (like Polybar) want to be on top of everything else
-- This function finds those windows and returns them in a list.
makeTopWindows
  :: (Members '[PropertyReader, GlobalX, WindowMover] r)
  => Sem r [Window]
makeTopWindows = do
  -- Get a list of all windows
  wins <- getTree
  higherWins <- for wins $ \win -> do
    -- EWMH defines how to do this.
    -- Check out their spec if you're curious.
    prop <- getProperty 32 "_NET_WM_STATE" win
    case prop of
      Nothing -> return []
      Just states ->
        ifM (or <$> traverse (isSameAtom "_NET_WM_STATE_ABOVE") states)
            (return [win])
            (return [])
  return $ join higherWins

getBottomWindows
  :: (Members '[PropertyReader, GlobalX, WindowMover] r)
  => Sem r [Window]
getBottomWindows = do
  -- Get a list of all windows
  wins <- getTree
  lowerWindows <- for wins $ \win -> do
    -- EWMH defines how to do this.
    -- Check out their spec if you're curious.
    prop <- (getProperty @_ @Word8) 8 "WM_NAME" win
    case prop of
      Nothing -> return []
      Just states ->
        return if (== "fakeWindowDontManage") $ fmap (chr . fromIntegral) states
          then [win]
          else []
  return $ join lowerWindows

setClientList :: (Members '[State (Fix Tiler), Reader Window, PropertyWriter] r)
              => Sem r ()
setClientList = do
  root <- ask
  tilers <- get
  setProperty32 "_NET_CLIENT_LIST" "WINDOW[]" root $ cata winList tilers
    where winList (Wrap (ChildParent _ w)) = [fromIntegral w]
          winList t = concat t

writeActiveWindow :: (Members '[State (Fix Tiler), Reader Window, PropertyWriter] r)
              => Sem r ()
writeActiveWindow = do
  root <- ask
  tilers <- get
  setProperty32 "_NET_ACTIVE_WINDOW" "WINDOW" root [fromMaybe (fromIntegral root) . extract $ ana @(Beam _) makeList tilers]
    where makeList (Fix (Wrap (ChildParent _ w))) = EndF . Just $ fromIntegral w
          makeList (Fix (InputController _ (Just t))) = ContinueF t
          makeList (Fix (InputController _ Nothing)) = EndF Nothing
          makeList (Fix (Monitor _ (Just t))) = ContinueF t
          makeList (Fix (Monitor _ Nothing)) = EndF Nothing
          makeList (Fix t) = ContinueF (getFocused t)
