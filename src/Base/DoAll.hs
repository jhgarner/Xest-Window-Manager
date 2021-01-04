{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module combines all of the effects into one large monad.
module Base.DoAll
  ( module Base.Helpers,
    module Base.Other,
    module Base.Mover,
    module Base.Property,
    module Base.Minimizer,
    module Base.Executor,
    module Base.Global,
    module Base.EventFlags,
    module Base.Colorer,
    module Base.Unmanaged,
    module Base.DoAll,
  )
where

import Actions.ActionTypes
import Base.Colorer
import Base.EventFlags
import Base.Executor
import Base.Global
import Base.Helpers
import Base.Minimizer
import Base.Mover
import Base.Other
import Base.Property
import Base.Unmanaged
import Config
import qualified Data.Map as M
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types
import qualified SDL
import qualified SDL.Font as Font
import Standard
import Tiler.ParentChild
import Tiler.TilerTypes

-- | Defines what kind of action triggered the KeyStatus Temp mode.
data TempType = FromMod | NotMod
  deriving (Show)

-- | Defines the states for the keybinding state machine. For more info about
--  how this is used, look in XEvents.
data KeyStatus = New KeyStatus Mode KeyCode [Action] | Temp TempType KeyStatus Mode KeyCode [Action] | Default
  deriving (Show)

makeBaseFunctor ''KeyStatus

data SafeRedraw = SafeRedraw | UnsafeRedraw
  deriving (Show, Eq, Enum)

-- Type aliases that should be used elsewhere but for now are just used to get
-- easy to access names when deriving Input/Output/State over these types.
type ShouldRedraw = Maybe SafeRedraw

type Yanked = [SubTiler]

type LostWindow = Map Window [ParentChild]

-- Want to do everything in IO? Use this!
doAll ::
  IORef [Text] ->
  Screens ->
  Conf ->
  Mode ->
  Display ->
  Window ->
  Font.Font ->
  Cursor ->
  Eff _ a -> -- The super long effect list which GHC can figure out on its own
  IO a
doAll ioref t c m d w f cur = do
  -- This is a bit of a yikes function. Basically, it executes a very large
  -- Freer monad in terms of IO by stripping off each layer.
  runM
    . evalState False
    . runStateIORef ioref
    . runLogger
    . runStateLogged (m, "Mode")
    . runStateLogged (Default, "KeyStatus")
    . runStateLogged (t, "Screens")
    . runStateLogged ([] @SubTiler, "Popped Tilers")
    . runStateLogged (None, "Mouse Button")
    . runStateLogged (M.empty @Text @Atom, "name->Atom")
    . runStateLogged (M.empty @Atom @[Int], "Atom->value")
    . runStateLogged (FocusedCache 0, "Focused window")
    . runStateLogged (M.empty @SDL.Window @XRect, "SDL->location")
    . runStateLogged (M.empty @Window @XRect, "Window->location")
    . runStateLogged (M.empty @Window @[ParentChild], "Window->transients")
    . runStateLogged ([] @Window, "Window stack")
    . runStateLogged (Just UnsafeRedraw, "Redraw Flag")
    . runStateLogged (c, "Config")
    . runStateLogged (0 :: ActiveScreen, "Active screen")
    . runStateLogged (currentTime, "Time")
    . runStateLogged (Docks [], "Docks")
    . runStateLogged (Visible, "Dock State")
    . runStateLogged (OMB None, "Old Mouse Buttons")
    . runStateLogged (OldTime currentTime, "Old Time")
    . runInput w
    . runInput d
    . runInput f
    . runInput (XCursor cur)
    . stateToInput @ActiveScreen
    . stateToInput @Conf
    . stateToInput @Screens
    . runNewBorders
    . runFakeScreens
    . runFakeMouseButtons
    . runFakePointer
    . runFakeTiler
    . runExecutor
    . runProperty
    . runEventFlags
    . runGlobalX
    . runMinimizer
    . runMover
    . runColorer
    . runUnmanaged
  where
    stateToInput :: Member (State a) r => Eff (Input a ': r) b -> Eff r b
    stateToInput = interpret $ \case
      Input -> get
