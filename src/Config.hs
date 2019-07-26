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
import           Graphics.X11.Types
import qualified Types                         as T
import           FocusList

-- Mirror many of the datatypes from Types but with easy to parse versions
-- | Same as Type
data Conf = Conf { keyBindings  :: [KeyTrigger]
                 , buttonBindings :: [ButtonTrigger]
                 , definedModes :: [Mode]
                 }
  deriving (Generic, Show, Interpret)

-- | Convert a parsed conf to a useful conf
confToType :: Display -> Conf -> IO T.Conf
confToType display (Conf kb bb dm) = do
  -- Convert the defined modes to a map of modeNames to modes
  let mapModes     = foldl' (\mm m -> insertMap (modeName m) m mm) mempty dm
      definedModes = map (modeToType mapModes) dm
      buttonBindings = map (buttonToType mapModes) bb
  keyBindings <- traverse (keyTriggerToType display mapModes) kb
  return T.Conf { .. }

-- | Switch key from KeyCode to Text and make mode a Text as well
data KeyTrigger = KeyTrigger { key     :: Text
                             , mode    :: Text
                             , actions :: [Action]
                             }
  deriving (Generic, Show, Interpret)
data ButtonTrigger = ButtonTrigger { button :: Text
                                   , mode   :: Text
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
  = ChangeLayoutTo Tiler
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
  | ChangeSize
  deriving (Generic, Show, Eq, Interpret)

-- | See other *ToType functions
actionToType :: Map Text Mode -> Action -> T.Action
actionToType _ (ChangeLayoutTo t) = T.ChangeLayoutTo $ tilerToType t
actionToType _ (RunCommand     a) = T.RunCommand $ unpack a
actionToType _ (ShowWindow     a) = T.ShowWindow $ unpack a
actionToType _ (HideWindow     a) = T.HideWindow $ unpack a
actionToType _ ZoomInInput        = T.ZoomInInput
actionToType _ ZoomOutInput       = T.ZoomOutInput
actionToType _ PopTiler           = T.PopTiler
actionToType _ PushTiler          = T.PushTiler
actionToType _ (ChangeNamed s)    = T.ChangeNamed s
actionToType _ (Move        s)    = if s then T.Move Front else T.Move Back
actionToType _ ChangeSize         = T.ChangeSize
actionToType modeList (ChangeModeTo a) =
  T.ChangeModeTo . modeToType modeList $ getMode a modeList

buttonToType :: Map Text Mode -> ButtonTrigger -> T.ButtonTrigger
buttonToType modeMap (ButtonTrigger b m a) =
  (toButton b, modeToType modeMap $ getMode m modeMap, map (actionToType modeMap) a)
 where toButton "1" = button1
       toButton "2" = button2
       toButton "3" = button3
       toButton _ = error "Buttons must be between 1 and 3"

-- | Remove Tilers the user shouldn't be creating
data Tiler
  = Vertical
  | Horizontal
  | Workspace
  deriving (Eq, Generic, Show, Interpret)

-- | See other *ToType functions
tilerToType :: Tiler -> Fix T.Tiler
tilerToType Vertical   = Fix . T.Directional T.Y $ emptyFL
tilerToType Horizontal = Fix . T.Directional T.X $ emptyFL
tilerToType Workspace  = Fix . T.Directional T.Z $ emptyFL

-- | Pretty much the same as T.Mode already
data Mode = NewMode { modeName     :: Text
                    , introActions :: [Action]
                    , exitActions  :: [Action]
                    }
  deriving (Generic, Show, Eq, Interpret)

-- | See other *ToType functions
modeToType :: Map Text Mode -> Mode -> T.Mode
modeToType m (NewMode a b c) =
  T.NewMode a (actionToType m <$> b) (actionToType m <$> c)

-- | *IMPURE* Lookup a mode and crash if it doesn't exist
getMode :: Text -> Map Text Mode -> Mode
getMode s modeList =
  fromMaybe (error $ "Mode " ++ unpack s ++ " is not defined")
    $ lookup s modeList

-- | A simple function to wrap everything else up
readConfig :: Display -> Text -> IO T.Conf
readConfig display fileName = do
  x <- input auto fileName
  confToType display x
