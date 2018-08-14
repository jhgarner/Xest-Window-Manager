{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config
  ( getConfig
  , parseActions
  ) where

import ClassyPrelude
import Core
import Graphics.X11.Types
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Types
import System.Process

-- Interprets actions
parseActions :: [Action] -> Xest EventState
parseActions l = do
  es <- asks eventState
  foldl' (\acc a -> (acc >>= flip parse a)) (return es) l
  where
    parse :: EventState -> Action -> Xest EventState
    parse acc (ChangeLayoutTo t) =
      case popWindow $ desktop acc of -- change to not reverse
        (Nothing, _) -> return acc { desktop = t }
        (Just w, ws) -> parse (acc { desktop = ws }) . ChangeLayoutTo $ addWindow w t
    parse acc (RunCommand s) = liftIO $ spawnCommand s >> return acc
    parse acc (ChangeModeTo m) = return acc { currentMode = m }
    parse acc DoNothing = return acc

getConfig :: FilePath -> IO Conf
getConfig file = readFileUtf8 file >>=
    maybe (error "Failed to parse \"config.conf\"" :: IO Conf) return . readMay
