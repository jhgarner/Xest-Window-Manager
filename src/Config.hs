{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Config where

import           Standard hiding (input)
import           Dhall
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Types
import           Actions.ActionTypes
import           System.Process
import qualified System.Environment as Env
import System.Directory

-- |A config file. It contains a list of startup scripts, a command to run, and
-- an initial mode. The a type decides how you want to represent keys.
-- internally, we store those as KeyCodes but users would prefer to specify
-- them as strings.
data ConfA a = Conf { keyBindings  :: [KeyTrigger a]
                 , startupScript :: Text
                 , initialMode :: Mode
                 , fontLocation :: Text
                 }
  deriving (Generic, Show, Interpret)

-- |The internal Config file.
type Conf = ConfA KeyCode

-- |The user style config file.
type ConfUser = ConfA Text

-- | Convert a ConfUser to a Conf. The Bool tells us if we're reloading or
-- launching the config for the first time. If we're only reloading, don't run
-- the startup script.
confToType :: Display -> ConfUser -> Bool -> IO Conf
confToType display (Conf kb startupScript@(Text script) initialMode fontLocation) isReload = do
  keyBindings <- traverse (traverse $ keysymToKeycode display . stringToKeysym . view _Text) kb

  unless isReload $ do
    exitCode <- runCommand script >>= waitForProcess
    unless (exitCode == ExitSuccess)
      $ die "Error while running startup script" 
  return Conf { .. }

-- | Represents a keybinding in Xest.
data KeyTrigger a = KeyTrigger { key     :: a
                             , mode    :: Mode
                             , actions :: [Action]
                             , exitActions :: [Action]
                             }
  deriving (Generic, Show, Interpret, Functor, Traversable, Foldable)

findConfFile :: IO Text
findConfFile = do
  displayNumber <- fromJust . tailMay <$> Env.getEnv "DISPLAY"
  homeDir <- Env.getEnv "HOME"
  let baseDirs = [homeDir ++ "/.config", "/etc"]
  let suffixes = [displayNumber ++ ".dhall", "dhall"]
  print $ baseDirs ++ suffixes
  Just configLoc <- findMOf each doesFileExist [ base <> "/xest/config." <> suffix | base <- baseDirs, suffix <- suffixes]
  return $ Text configLoc

-- | A simple function to open a config file and parse it.
readConfig :: Display -> IO Conf
readConfig display = do
  fileName <- findConfFile
  x <- detailed $ input (genericAutoWith $ defaultInterpretOptions {singletonConstructors = Bare}) fileName
  confToType display x False

-- | Reloads an existing config
reloadConfig :: Display -> IO Conf
reloadConfig display = do
  fileName <- findConfFile
  x <- detailed $ input (genericAutoWith $ defaultInterpretOptions {singletonConstructors = Bare}) fileName
  confToType display x True
