{-# LANGUAGE RecordWildCards #-}

module Config
  ( getConfig
  , initKeyBindings
  , parseActions
  ) where

import ClassyPrelude
import Core
import Graphics.X11.Types
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Types
import Data.Maybe
import System.Process

-- Interprets actions
parseActions :: [Action] -> Xest EventState
parseActions l = do
  ES {..} <- asks eventState
  newDesktop <- foldl' parse (return desktop) l
  return $ ES newDesktop
  where
    parse accIO (ChangeLayoutTo t) = accIO >>= \acc ->
      case popWindow acc of
        (Nothing, _) -> return t
        (Just w, ws) -> parse (return ws) . ChangeLayoutTo $ addWindow w t
    parse acc (RunCommand s) = acc >>= \t -> liftIO $ spawnCommand s >> return t
    parse acc DoNothing = acc

-- TODO Get the configuration from a file
getConfig :: Text -> IO [KeyBinding]
getConfig _ = readFileUtf8 "config.conf" >>=
  maybe (undefined :: IO [KeyBinding]) return . readMay 

-- Turn on global keybind watching
initKeyBindings :: Display -> Window -> [KeyBinding] -> IO ()
initKeyBindings display rootWindow =
  mapM_ $ \(KeyBinding ks _) -> do
    k <- keysymToKeycode display ks
    grabKey display k anyModifier rootWindow False grabModeAsync grabModeAsync
