{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveAnyClass   #-}

module Config (readConfig) where

import           ClassyPrelude
import           Dhall
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Types
import qualified Types                   as T

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
  mapModes <- return $ foldl' (\mm m -> insertMap (modeName m) m mm) (mempty) dm
  keyBindings <- traverse (keyTriggerToType display mapModes) kb
  definedModes <- return $ map (modeToType mapModes) dm
  return T.Conf {..}

-- | Switch key from KeyCode to Text and make mode a Text as well
data KeyTrigger = KeyTrigger { key     :: Text
                             , mode    :: Text
                             , actions :: [Action]
                             }
  deriving (Generic, Show, Interpret)

-- | Similar to confToType. Needs display to convert the key symbol to a number and mm to convert Text to a Mode
keyTriggerToType :: Display -> Map Text Mode -> KeyTrigger -> IO T.KeyTrigger
keyTriggerToType display mm (KeyTrigger k m as)= do
  kc <- keysymToKeycode display $ stringToKeysym (unpack k)
  return (kc, modeToType mm $ getMode m mm, map (actionToType mm) as)

-- | Remove some actions the user shouldn't be trying to include
data Action
  = ChangeLayoutTo Tiler
  | RunCommand Text
  | ChangeModeTo Text
  | ShowWindow Text
  | HideWindow Text
  | ZoomInInput
  | ZoomOutInput
  deriving (Generic, Show, Eq, Interpret)

-- | See other *ToType functions
actionToType :: Map Text Mode -> Action -> T.Action
actionToType _ (ChangeLayoutTo t) = T.ChangeLayoutTo $ tilerToType t
actionToType _ (RunCommand a) = T.RunCommand $ unpack a
actionToType _ (ShowWindow a) = T.ShowWindow $ unpack a
actionToType _ (HideWindow a) = T.HideWindow $ unpack a
actionToType _ ZoomInInput = T.ZoomInInput
actionToType _ ZoomOutInput = T.ZoomOutInput
actionToType modeList (ChangeModeTo a) = T.ChangeModeTo . modeToType modeList $ getMode a modeList


-- | Remove Tilers the user shouldn't be creating
data Tiler
  = Vertical
  | Horizontal
  deriving (Eq, Generic, Show, Interpret)

-- | See other *ToType functions
tilerToType :: Tiler -> T.Tiler
tilerToType Vertical   = T.Vertical []
tilerToType Horizontal = T.Horizontal []

-- | Pretty much the same as T.Mode already
data Mode = NewMode { modeName     :: Text
                    , introActions :: [Action]
                    , exitActions  :: [Action]
                    }
  deriving (Generic, Show, Eq, Interpret)

-- | See other *ToType functions
modeToType :: Map Text Mode -> Mode -> T.Mode
modeToType m (NewMode a b c) = T.NewMode a (actionToType m <$> b) (actionToType m <$> c)

-- | *IMPURE* Lookup a mode and crash if it doesn't exist
getMode :: Text -> Map Text Mode -> Mode
getMode s modeList = fromMaybe (error $ "Mode " ++ unpack s ++ " is not defined") $ lookup s modeList

-- | A simple function to wrap everything else up
readConfig :: Display -> Text -> IO T.Conf
readConfig display fileName = do
  x <- input auto fileName
  confToType display x