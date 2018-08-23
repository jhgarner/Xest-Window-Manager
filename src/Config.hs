{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( getConfig
  , parseActions
  , rebindKeys
  ) where

import ClassyPrelude
import Core
import Graphics.X11.Types
import Graphics.X11.Xlib.Misc
import System.Process
import Control.Lens


-- Key Parsers
-- Generic parser that executes an associated action
parseActions :: KeyBinding -> Bool -> Xest EventState
parseActions (KeyBinding key action _) et = do
  es <- asks eventState
  foldl' (\acc a -> (acc >>= parse a et key)) (return es) action

-- Parser used while holding down a change mode key.
newModeParser :: KeyCode -> KeyBinding -> Bool -> Xest EventState
newModeParser boundKey kb True = (keyParser .~ tempModeParser boundKey)
                                 <$> parseActions kb True
newModeParser boundKey kb@(KeyBinding k _ _) False
  | k == boundKey = set keyParser parseActions <$> asks eventState
  | otherwise = parseActions kb False

-- Parser used when a mode should return after the key is released
tempModeParser :: KeyCode -> KeyBinding -> Bool -> Xest EventState
tempModeParser boundKey kb@(KeyBinding k _ _) False
  | k == boundKey = set keyParser parseActions . set currentMode InsertMode
    <$> (rebindKeys InsertMode >> asks eventState)
  | otherwise = parseActions kb False
tempModeParser _ kb True = parseActions kb True

parse :: Action -> Bool -> KeyCode -> EventState -> Xest EventState -- TODO replace Bool with pattern synonym
parse (ChangeLayoutTo t) True kc acc =
  case popWindow $ _desktop acc of -- change to not reverse
    (Nothing, _) -> return $ set desktop t acc
    (Just win, wins) -> parse (ChangeLayoutTo $ addWindow win t) True kc
                        $ set desktop wins acc

parse (RunCommand s) True _ acc = liftIO $ spawnCommand s >> return acc

parse (ChangeModeTo m) True kc acc =
  rebindKeys m >> return (set currentMode m . (keyParser .~ newModeParser kc) $ acc)

parse DoNothing True _ acc = return acc

parse _ _ _ acc = return acc


getConfig :: FilePath -> IO Conf
getConfig file = readFileUtf8 file >>=
    maybe (error "Failed to parse \"config.conf\"" :: IO Conf) return . readMay

-- Chang the keybindings depending on the mode
rebindKeys :: Mode -> Xest ()
rebindKeys activeMode = do
  kb <- asks config
  d <- asks display
  win <- asks rootWin

  liftIO . forM_ kb $ toggleModel activeMode d win
  where toggleModel m d win (KeyBinding k _ km) =
          if m == km then grabKey d k anyModifier win False grabModeAsync grabModeAsync
          else ungrabKey d k anyModifier win
