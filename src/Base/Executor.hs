{-# LANGUAGE TemplateHaskell #-}

module Base.Executor where

import Config
import Graphics.X11.Xlib.Types
import Standard
import System.Process

-- | Actions that modify the world outside of X11 go here
data Executor a where
  -- | Run a command
  Execute :: Text -> Executor ()

  -- | Toggle logging
  ToggleLogs :: Executor ()

  -- | Prints in a controlled way
  ReloadConf :: Executor ()
makeEffect ''Executor

-- | Do it in IO
runExecutor :: Members [IO, State Bool, State Conf, Input Display] m => Eff (Executor ': m) a -> Eff m a
runExecutor = interpret \case
  Execute (Text s) -> void . liftIO $ spawnCommand s

  ToggleLogs -> do
    shouldLog <- get @Bool
    unless shouldLog $
      -- Empty the last run of logging
      liftIO $ Standard.writeFile "/tmp/xest.log" ""
    modify @Bool not

  ReloadConf -> do
    display <- input @Display
    newConf <- liftIO $ reloadConfig display
    put @Conf newConf
