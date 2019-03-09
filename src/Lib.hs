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
import           Control.Monad.State.Lazy
import           Core
import           Data.Bits
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Screen
import           Types
import           Data.Functor.Foldable
import           Data.List                      ( iterate )
import qualified Data.Vector                   as V

-- | Starting point of the program. Should never return
startWM :: IO ()
startWM = do
  -- Grab a display to capture. The chosen display cannot have a WM already running.
  -- TODO use variables to determine display number
  display <- openDisplay ":99"

  -- Find and register ourselves with the root window
  -- These two masks allow us to intercept various Xorg events useful for a WM
  let root = defaultRootWindow display
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
    (Fix . InputController . Fix . Horizontal $ FL 0 V.empty)
    initialMode
    handler

-- | Performs the event loop recursion inside of the Xest Monad
-- The return value of one iteration becomes the input for the next
mainLoop :: IterationState -> EventState -> IO ()
mainLoop iState@IS {..} eventState =
  runXest iState eventState (iterateM recurse []) >> say "Exiting"
 where
  iterateM f initial = sequence $ iterate (>>= f) $ return initial

  -- Performs the actual looping
  recurse :: Actions -> Xest Actions
  -- When there are no actions to perform, find new ones
  recurse [] = do
    gets _desktop >>= liftIO . print
    liftIO $ putStrLn ""
    get >>= render
    ptr <- liftIO . allocaXEvent $ \p -> nextEvent display p >> getEvent p
    return [XorgEvent ptr]

  -- When there are actions to perform, do them and add the results to the list of actions
  recurse (a : as) = do
    es       <- get
    newEvent <- view keyParser es a
    return $ as ++ newEvent
