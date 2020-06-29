{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Base.Executor where

import           Standard
import qualified System.Environment as Env
import           Graphics.X11.Xlib.Types
import           System.Process
import Config


-- |Actions that modify the world outside of X11 go here
class Executor m where
  -- |Run a command
  execute :: Text -> m ()
  -- |Toggle logging
  toggleLogs :: m ()
  -- |Prints in a controlled way
  reloadConf :: m ()

-- |Do it in IO
instance Members [MonadIO, State Bool, State Conf, Input Display] m => Executor m where
  execute (Text s) = void . liftIO $ spawnCommand s

  toggleLogs = do
    shouldLog <- get @Bool
    unless shouldLog $
      -- Empty the last run of logging
      liftIO $ Standard.writeFile "/tmp/xest.log" ""
    modify @Bool not

  reloadConf = do
    display <- input @Display
    newConf <- liftIO $ do
      args <- map fromString <$> getArgs
      let displayNumber = fromMaybe "0" $ headMay args

      -- Read the config file
      homeDir <- fromString <$> Env.getEnv "HOME"
      if displayNumber == "0" || displayNumber == "1"
          then reloadConfig display $ homeDir <> "/.config/xest/config.dhall"
          else reloadConfig display $ homeDir <> "/.config/xest/config." <> displayNumber <> ".dhall"
    put @Conf newConf
