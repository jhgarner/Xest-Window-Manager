{-# LANGUAGE DeriveAnyClass   #-}

module Actions.ActionTypes where

import           Standard
import           Dhall (Interpret)
import           FocusList

-- | Actions/events to be performed
data Action
  = Insert
  | ChangeNamed String
  | Move Direction
  | RunCommand String
  | ChangeModeTo Mode
  | ShowWindow String
  | HideWindow String
  | ZoomInInput
  | ZoomOutInput
  | ZoomInMonitor
  | ZoomOutMonitor
  | ZoomMonitorToInput
  | ZoomInputToMonitor
  | PopTiler
  | PushTiler
  | MakeEmpty
  | MoveToFront
  | KillActive
  | ExitNow
  | ToggleLogging
  | ChangeToHorizontal
  | ChangeToFloating
  | ChangeToTwoCols
  | SetRotate
  | SetFull
  | SetNoMod
  | ToggleDocks
  deriving (Show, Generic, Interpret)

-- | Modes similar to modes in vim
data Mode = NewMode { modeName     :: Text
                    , hasButtons :: Bool
                    , hasBorders :: Bool
                    }
  deriving (Show, Generic, Interpret)
instance Eq Mode where
  n1 == n2 = modeName n1 == modeName n2

