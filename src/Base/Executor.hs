{-# LANGUAGE UndecidableInstances #-}

module Base.Executor where

import Config
import Graphics.X11.Xlib.Types
import Standard
import System.Process

-- | Actions that modify the world outside of X11 go here
class Executor m where
  -- | Run a command
  execute :: Text -> m ()

  -- | Toggle logging
  toggleLogs :: m ()

  -- | Prints in a controlled way
  reloadConf :: m ()

-- | Do it in IO
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
    newConf <- liftIO $ reloadConfig display
    put @Conf newConf
