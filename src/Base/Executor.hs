{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Base.Executor where

import           Standard
import qualified System.Environment as Env
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           System.IO
import           Graphics.X11.Xlib.Types
import           System.Process
import Config


-- |Actions that modify the world outside of X11 go here
data Executor m a where
  -- |Run a command
  Execute :: String -> Executor m ()
  -- |Toggle logging
  ToggleLogs :: Executor m ()
  -- |Prints in a controlled way
  PrintMe :: String -> Executor m ()
  -- |Prints in a controlled way
  ReloadConf :: Executor m ()
makeSem ''Executor

-- |Do it in IO
runExecutor :: Members [Embed IO, State Bool, State Conf, Input Display] r => Sem (Executor ': r) a -> Sem r a
runExecutor = interpret $ \case
  Execute s -> void . embed @IO $ spawnCommand s

  ToggleLogs -> do
    unlessM (get @Bool) $
      -- Empty the last run of logging
      embed @IO $ Standard.writeFile "/tmp/xest.log" ""
    modify not

  -- PrintMe s -> say $ pack s
  PrintMe s -> whenM (get @Bool) $ embed @IO $ appendFile "/tmp/xest.log" s
  ReloadConf -> do
    display <- input @Display
    (homeDir, displayNumber) <- embed @IO $ do
      args <- getArgs
      let displayNumber = fromMaybe "0" $ headMay args

      -- Read the config file
      homeDir <- Env.getEnv "HOME"
      return (homeDir, displayNumber)
    newConf <- embed @IO $
      if displayNumber == "0" || displayNumber == "1"
          then reloadConfig display . pack $ homeDir ++ "/.config/xest/config.dhall"
          else reloadConfig display . pack $ homeDir ++ "/.config/xest/config." ++ unpack displayNumber ++ ".dhall"
    put newConf

