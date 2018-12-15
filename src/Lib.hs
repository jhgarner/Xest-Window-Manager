{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib
  ( startWM
  ) where

import ClassyPrelude
import Core
import Types
import Data.Bits
import Control.Lens
import Control.Monad.State.Lazy
import Graphics.X11.Types
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Screen

startWM :: IO ()
startWM = do
  display <- openDisplay ":99" -- TODO use a real display number

  -- Find and register ourselves with the root window
  let root = defaultRootWindow display
  selectInput display root (substructureNotifyMask .|. substructureRedirectMask)

  -- Create the initial state based on the configuration and some defaults
  c <- getConfig "config.conf"
  let screen = defaultScreenOfDisplay display
      initialMode = head . impureNonNull $ definedModes c
      state = IS display root (widthOfScreen screen, heightOfScreen screen) c Nothing
      initialized = runXest state undefined $ rebindKeys initialMode
  -- Grabs the initial keybindings
  _ <- initialized

  -- Execute the main loop. Will never return unless Xest exits
  mainLoop state $ ES (InputController $ Horizontal []) initialMode handler

-- Performs the event loop recursion inside of the Xest Monad
-- The return value of one iteration becomes the input for the next
mainLoop :: IterationState -> EventState -> IO ()
mainLoop state@IS{..} eventState = runXest state eventState (recurse NoEvent) >> say "Exiting"
  where
    recurse :: Events -> Xest Events
    recurse me = do
      es <- get
      newEvent <- view keyParser es me
      placeWindows (uncurry (Rect 0 0) dimensions) (_desktop es)
      recurse newEvent

-- Grabs the config file from a path and attempts to read it
-- TODO Add real parsing and error handling
getConfig :: FilePath -> IO Conf
getConfig file = readFileUtf8 file >>=
    maybe (error "Failed to parse \"config.conf\"" :: IO Conf) return . readMay
