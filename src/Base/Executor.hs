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
import           Graphics.X11.Xlib.Types
import           System.Process
import Config


-- |Actions that modify the world outside of X11 go here
data Executor m a where
  -- |Run a command
  Execute :: Text -> Executor m ()
  -- |Toggle logging
  ToggleLogs :: Executor m ()
  -- |Prints in a controlled way
  ReloadConf :: Executor m ()
makeSem ''Executor

-- |Do it in IO
runExecutor :: Members [Embed IO, State Bool, State Conf, Input Display] r => Sem (Executor ': r) a -> Sem r a
runExecutor = interpret $ \case
  Execute (Text s) -> void . embed @IO $ spawnCommand s

  ToggleLogs -> do
    shouldLog <- get @Bool
    unless shouldLog $
      -- Empty the last run of logging
      embed @IO $ Standard.writeFile "/tmp/xest.log" ""
    modify not

  ReloadConf -> do
    display <- input @Display
    newConf <- embed @IO $ do
      args <- fmap fromString <$> getArgs
      let displayNumber = fromMaybe "0" $ headMay args

      -- Read the config file
      homeDir <- fromString <$> Env.getEnv "HOME"
      if displayNumber == "0" || displayNumber == "1"
          then reloadConfig display $ homeDir <> "/.config/xest/config.dhall"
          else reloadConfig display $ homeDir <> "/.config/xest/config." <> displayNumber <> ".dhall"
    put newConf
