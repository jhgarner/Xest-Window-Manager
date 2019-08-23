{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Config
  ( readConfig
  )
where

import           Standard
import           Dhall
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Types
import qualified Types                         as T
import           FocusList

-- Mirror many of the datatypes from Types but with easy to parse versions
-- | Same as Type
data Conf = Conf { keyBindings  :: [KeyTrigger]
                 , definedModes :: [Mode]
                 }
  deriving (Generic, Show, Interpret)

-- | Convert a parsed conf to a useful conf
confToType :: Display -> Conf -> IO T.Conf
confToType display (Conf kb dm) = do
  -- Convert the defined modes to a map of modeNames to modes
  let mapModes     = foldl' (\mm m -> insertMap (modeName m) m mm) mempty dm
      definedModes = map (modeToType mapModes) dm
  keyBindings <- traverse (keyTriggerToType display mapModes) kb
  return T.Conf { .. }

-- | Switch key from KeyCode to Text and make mode a Text as well
data KeyTrigger = KeyTrigger { key     :: Text
                             , mode    :: Text
                             , actions :: [Action]
                             }
  deriving (Generic, Show, Interpret)

-- | Similar to confToType. Needs display to convert the key symbol to a number and mm to convert Text to a Mode
keyTriggerToType :: Display -> Map Text Mode -> KeyTrigger -> IO T.KeyTrigger
keyTriggerToType display mm (KeyTrigger k m as) = do
  kc <- keysymToKeycode display $ stringToKeysym (unpack k)
  return (kc, modeToType mm $ getMode m mm, map (actionToType mm) as)

-- | Remove some actions the user shouldn't be trying to include
data Action
  = Insert T.Insertable
  -- | ChangeLayoutTo T.Insertable
  | ChangeNamed String
  | Move Bool
  | RunCommand Text
  | ChangeModeTo Text
  | ShowWindow Text
  | HideWindow Text
  | ZoomInInput
  | ZoomOutInput
  | PopTiler
  | PushTiler
  | MakeSpecial
  deriving (Generic, Show, Eq, Interpret)

-- | See other *ToType functions
actionToType :: Map Text Mode -> Action -> T.Action
actionToType _ (Insert t) = T.Insert t
actionToType _ (RunCommand     a) = T.RunCommand $ unpack a
actionToType _ (ShowWindow     a) = T.ShowWindow $ unpack a
actionToType _ (HideWindow     a) = T.HideWindow $ unpack a
actionToType _ ZoomInInput        = T.ZoomInInput
actionToType _ ZoomOutInput       = T.ZoomOutInput
actionToType _ PopTiler           = T.PopTiler
actionToType _ PushTiler          = T.PushTiler
actionToType _ MakeSpecial           = T.MakeSpecial
actionToType _ (ChangeNamed s)    = T.ChangeNamed s
-- actionToType _ (ChangeLayoutTo s)    = T.ChangeLayoutTo s
actionToType _ (Move        s)    = if s then T.Move Front else T.Move Back
actionToType modeList (ChangeModeTo a) =
  T.ChangeModeTo . modeToType modeList $ getMode a modeList

-- | Remove Tilers the user shouldn't be creating
data Tiler
  = Vertical
  | Horizontal
  | Workspace
  | Floating
  deriving (Eq, Generic, Show, Interpret)

-- | Pretty much the same as T.Mode already
data Mode = NewMode { modeName     :: Text
                    , introActions :: [Action]
                    , exitActions  :: [Action]
                    , hasButtons :: Bool
                    }
  deriving (Generic, Show, Eq, Interpret)

-- | See other *ToType functions
modeToType :: Map Text Mode -> Mode -> T.Mode
modeToType m (NewMode a b c d) =
  T.NewMode a (actionToType m <$> b) (actionToType m <$> c) d

-- | Lookup a mode and crash if it doesn't exist
getMode :: Text -> Map Text Mode -> Mode
getMode s modeList =
  fromMaybe (error $ "Mode " ++ unpack s ++ " is not defined")
    $ lookup s modeList

-- | A simple function to wrap everything else up
readConfig :: Display -> Text -> IO T.Conf
readConfig display fileName = do
  x <- input auto fileName
  confToType display x
