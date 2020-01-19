{-# LANGUAGE DeriveAnyClass   #-}

module Actions.ActionTypes where

import           Standard
import           Dhall (Interpret)
import           FocusList

data Insertable
  = Horizontal
  | Hovering
  | Rotate
  | FullScreen
  deriving (Generic, Show, Eq, Interpret)

-- | Actions/events to be performed
data Action
  = Insert Insertable
  | ChangeNamed String
  | Move Direction
  | RunCommand String
  | ChangeModeTo Mode
  | ShowWindow String
  | HideWindow String
  | ZoomInInput
  | ZoomInInputSkip
  | ZoomOutInput
  | ZoomOutInputSkip
  | ZoomInMonitor
  | ZoomOutMonitor
  | ZoomMonitorToInput
  | ZoomInputToMonitor
  | PopTiler
  | PushTiler
  | MakeSpecial
  | KillActive
  | ExitNow
  | ToggleLogging
  deriving (Show, Generic, Interpret)

-- | Modes similar to modes in vim
data Mode = NewMode { modeName     :: Text
                    , hasButtons :: Bool
                    , hasBorders :: Bool
                    }
  deriving (Show, Generic, Interpret)
instance Eq Mode where
  n1 == n2 = modeName n1 == modeName n2

