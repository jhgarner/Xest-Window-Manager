{-# LANGUAGE DeriveAnyClass #-}

module Actions.ActionTypes where

import Dhall (Interpret)
import FocusList
import Standard

-- | Actions/events to be performed
data Action
  = Insert
  | ChangeNamed Text
  | Move Direction
  | RunCommand Text
  | ChangeModeTo Mode
  | ShowWindow Text
  | HideWindow Text
  | ZoomInInput
  | ZoomOutInput
  | ZoomInMonitor
  | ZoomOutMonitor
  | ZoomMonitorToInput
  | ZoomInputToMonitor
  | PopTiler
  | PushTiler
  | MakeEmpty
  | MoveToLoc Natural
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
  | ChangeActiveScreen Direction
  deriving (Show, Generic, Interpret)

-- | Modes similar to modes in vim
data Mode = NewMode
  { modeName :: Text,
    hasButtons :: Bool,
    hasBorders :: Bool
  }
  deriving (Show, Generic, Interpret)

instance Eq Mode where
  n1 == n2 = modeName n1 == modeName n2
