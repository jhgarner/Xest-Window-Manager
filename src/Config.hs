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

-- Interprets actions
parseActions :: [Action] -> Xest EventState
parseActions l = do
  ES {..} <- asks eventState
  return . ES $ foldl' parse wins l
  where
    parse acc (ChangeLayoutTo t) =
      case popWindow acc of
        (Nothing, _) -> t
        (Just w, ws) -> parse ws . ChangeLayoutTo $ addWindow w t
    parse acc DoNothing = acc

-- TODO Get the configuration from a file
getConfig :: Text -> IO [KeyBinding]
getConfig _ =
  maybe (undefined :: IO [KeyBinding]) return $ readMay "[KeyBinding 118 [ChangeLayoutTo (Vertical [])], KeyBinding 104 [ChangeLayoutTo (Horizontal [])]]"

-- Turn on global keybind watching
initKeyBindings :: Display -> Window -> [KeyBinding] -> IO ()
initKeyBindings display rootWindow =
  mapM_ $ \(KeyBinding ks _) -> do
    k <- keysymToKeycode display ks
    grabKey display k anyModifier rootWindow False grabModeAsync grabModeAsync
