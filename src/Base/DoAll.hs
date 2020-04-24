{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleInstances #-}

module Base.DoAll
  ( module Base.Helpers
  , module Base.Other
  , module Base.Mover
  , module Base.Property
  , module Base.Minimizer
  , module Base.Executor
  , module Base.Global
  , module Base.EventFlags
  , module Base.Colorer
  , module Base.Unmanaged
  , module Base.DoAll
  )
where

import           Standard
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Types
import qualified Data.Set                      as S
import qualified Data.Map                      as M
import qualified SDL
import           Base.Helpers
import           Base.Other
import           Base.Mover
import           Base.Property
import           Base.Minimizer
import           Base.Executor
import           Base.Global
import           Base.Colorer
import           Base.EventFlags
import           Base.Unmanaged
import           Tiler.TilerTypes
import           Tiler.ParentChild
import           Config
import           Actions.ActionTypes
import           Colog.Core
import qualified SDL.Font as Font
import Data.Time
import Data.Time.Format.ISO8601


type LostWindow = Map Window [ParentChild]

-- Want to do everything in IO? Use this!
doAll
  :: IORef [Text]
  -> Screens
  -> Conf
  -> Mode
  -> Display
  -> Window
  -> Font.Font
  -> _ -- The super long Sem list which GHC can figure out on its own
  -> IO ()
doAll ioref t c m d w f =
  void
    . runM
    . stateToIO False
    . runStateIORef ioref
    . runLogActionSem logger
    . runStateLogged (m, "Mode")
    . runStateLogged (S.empty @Window, "Minimized set")
    . runStateLogged (Default, "KeyStatus")
    . runStateLogged (t, "Screens")
    . runStateLogged ([] @SubTiler, "Popped Tilers")
    . runStateLogged (None, "Mouse Button")
    . runStateLogged (M.empty @Text, "name->Atom")
    . runStateLogged (M.empty @Atom @[Int], "Atom->value")
    . runStateLogged (FocusedCache 0, "Focused window")
    . runStateLogged (M.empty @SDL.Window, "SDL->location")
    . runStateLogged (M.empty @Window @XRect, "Window->location")
    . runStateLogged (M.empty @Window @[ParentChild], "Window->transients")
    . runStateLogged ([] @Window, "Window stack")
    . runStateLogged (Just (), "Redraw Flag")
    . runStateLogged (c, "Config")
    . runStateLogged ((0 :: ActiveScreen), "Active screen")
    . runStateLogged (currentTime, "Time")
    . runStateLogged (Docks [], "Docks")
    . runStateLogged (Visible, "Dock State")
    . runInputs (w ::: d ::: f ::: HNil)
    . stateToInput @Conf
    . stateToInput @Screens
    . runNewBorders
    . runGetScreens
    . indexedState
    . runGetButtons
    . runGetPointer
    . runExecutor
    . runProperty
    . runEventFlags
    . runGlobalX
    . runMinimizer
    . runMover
    . runColorer
    . runUnmanaged
 where
  logger :: Members '[State Bool, State [Text], Embed IO] r => LogAction (Sem r) LogData
  logger = LogAction $ \(LD prefix msg) -> do
    timeZone <- embed getCurrentTimeZone
    timeUtc <- embed getCurrentTime
    let timeStamp = "[" <> Text (formatShow iso8601Format (utcToLocalTime timeZone timeUtc)) <> "]"
        prefixWrap = "[" <> prefix <> "]"
        fullMsg:: Text = prefixWrap <> timeStamp <> msg
    modify @[Text] $ (:) fullMsg
    modify @[Text] $ take 100
    shouldLog <- get @Bool
    when shouldLog $
      embed @IO $ appendFile "/tmp/xest.log" (fullMsg <> "\n")

  stateToInput :: Member (State a) r => Sem (Input a ': r) b -> Sem r b
  stateToInput = interpret $ \case
    Input -> get

data TempType = FromMod | NotMod
  deriving Show

data KeyStatus = New KeyStatus Mode KeyCode [Action] | Temp TempType KeyStatus Mode KeyCode [Action] | Default
  deriving Show
makeBaseFunctor ''KeyStatus
