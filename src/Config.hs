{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Config where

import           Standard
import           Dhall
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Types
import           Actions.ActionTypes
import           System.Process
import           System.Exit

-- Mirror many of the datatypes from Types but with easy to parse versions
-- | Same as Type
data ConfA a = Conf { keyBindings  :: [KeyTrigger a]
                 , startupScript :: String
                 , initialMode :: Mode
                 }
  deriving (Generic, Show, Interpret)

type Conf = ConfA KeyCode
type ConfUser = ConfA Text

-- | Convert a parsed conf to a useful conf
confToType :: Display -> ConfUser -> Bool -> IO Conf
confToType display (Conf kb startupScript initialMode) isReload = do
  -- Convert the defined modes to a map of modeNames to modes
  keyBindings <- traverse (traverse $ keysymToKeycode display . stringToKeysym . unpack) kb
  unless isReload $
    unlessM ((== ExitSuccess) <$> (waitForProcess <=< runCommand $ startupScript))
      $ die "Error while running startup script" 
  return Conf { .. }

-- | Switch key from KeyCode to Text and make mode a Text as well
data KeyTrigger a = KeyTrigger { key     :: a
                             , mode    :: Mode
                             , actions :: [Action]
                             , exitActions :: [Action]
                             }
  deriving (Generic, Show, Interpret, Functor, Traversable, Foldable)

-- | A simple function to wrap everything else up
readConfig :: Display -> Text -> IO Conf
readConfig display fileName = do
  x <- detailed $ input (autoWith $ defaultInterpretOptions {singletonConstructors = Bare}) fileName
  confToType display x False

-- | Reloads an existing config
reloadConfig :: Display -> Text -> IO Conf
reloadConfig display fileName = do
  x <- detailed $ input (autoWith $ defaultInterpretOptions {singletonConstructors = Bare}) fileName
  confToType display x True
