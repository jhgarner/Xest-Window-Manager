{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import qualified SDL.Font as Font
import qualified Control.Monad.Reader as R
import Graphics.X11.Xinerama (XineramaScreenInfo)
import Capability.Reader ()
import Capability.State ()


data Ctx = Ctx
  { shouldLog :: IORef (Bool)
  , logHistory :: IORef [Text]
  , activeMode :: IORef Mode
  , minimizedWindows :: IORef (S.Set Window)
  , screenList :: IORef Screens
  , keyStatus :: IORef  KeyStatus
  , yankBuffer :: IORef  [SubTiler]
  , oldMouseButtons :: IORef  OldMouseButtons
  , atomNameCache :: IORef  (M.Map Text Atom)
  , atomValueCache :: IORef  (M.Map Atom [Int])
  , focusedWindow :: IORef  FocusedCache
  , borderLocations :: IORef  (M.Map SDL.Window XRect)
  , windowLocations :: IORef  (M.Map Window XRect)
  , windowChildren :: IORef  (M.Map Window [ParentChild])
  , stackCache :: IORef  [Window]
  , shouldRedraw :: IORef  (Maybe ())
  , configuration :: IORef  Conf
  , activeScreen :: IORef  ActiveScreen
  , lastTime :: IORef OldTime
  , knownDocks :: IORef Docks
  , dockState :: IORef DockState
  , rootWindow :: Window
  , display :: Display
  , fontChoice :: Font.Font
  , cursor :: XCursor
  } deriving (Generic)

type Logged name s = LoggedSink name s (ReaderIORef ((Rename name (Field name () (MonadReader M))))) M
type From name = ReaderIORef ((Rename name (Field name () (MonadReader M))))
type FromInput name = Rename name (Field name () (MonadReader M))
type ShouldRedraw = Maybe ()

newtype M a = M { runM :: R.ReaderT Ctx IO a }
  deriving (Functor, Applicative, Monad, MonadIO, R.MonadReader Ctx)
  deriving (Input Mode, Output Mode, State Mode) via (Logged "activeMode" Mode)
  deriving (Input  [Text], Output [Text], State [Text]) via (From "logHistory")
  deriving (Input Bool, Output Bool, State Bool) via (From "shouldLog")
  deriving (Input (Set Window), Output (Set Window), State (Set Window)) via (Logged "minimizedWindows" (Set Window))
  deriving (Input Screens, Output Screens, State Screens) via (Logged "screenList" Screens)
  deriving (Input [SubTiler], Output [SubTiler], State [SubTiler]) via (Logged "yankBuffer" [SubTiler])
  deriving (Input OldMouseButtons, Output OldMouseButtons, State OldMouseButtons) via (Logged "oldMouseButtons" OldMouseButtons)
  deriving (Input (M.Map Text Atom), Output (M.Map Text Atom), State (M.Map Text Atom)) via (Logged "atomNameCache" (M.Map Text Atom))
  deriving (Input (M.Map Atom [Int]), Output (M.Map Atom [Int]), State (M.Map Atom [Int])) via (Logged "atomValueCache" (M.Map Atom [Int]))
  deriving (Input [Window], Output [Window], State [Window]) via (Logged "stackCache" [Window])
  deriving (Input FocusedCache, Output FocusedCache, State FocusedCache) via (Logged "focusedWindow" FocusedCache)
  deriving (Input (M.Map SDL.Window XRect), Output (M.Map SDL.Window XRect), State (M.Map SDL.Window XRect)) via (Logged "borderLocations" (M.Map SDL.Window XRect))
  deriving (Input (M.Map Window XRect), Output (M.Map Window XRect), State (M.Map Window XRect)) via (Logged "windowLocations" (M.Map Window XRect))
  deriving (Input (M.Map Window [ParentChild]), Output (M.Map Window [ParentChild]), State (M.Map Window [ParentChild])) via (Logged "windowChildren" (M.Map Window [ParentChild]))
  deriving (Input ShouldRedraw, Output ShouldRedraw, State ShouldRedraw) via (Logged "shouldRedraw" ShouldRedraw)
  deriving (Input Conf, Output Conf, State Conf) via (Logged "configuration" Conf)
  deriving (Input ActiveScreen, Output ActiveScreen, State ActiveScreen) via (Logged "activeScreen" ActiveScreen)
  deriving (Input OldTime, Output OldTime, State OldTime) via (Logged "lastTime" OldTime)
  deriving (Input Docks, Output Docks, State Docks) via (Logged "knownDocks" Docks)
  deriving (Input DockState, Output DockState, State DockState) via (Logged "dockState" DockState)
  deriving (Input KeyStatus, Output KeyStatus, State KeyStatus) via (Logged "keyStatus" KeyStatus)
  deriving (Input Tiler, Output Tiler, State Tiler) via FakeTiler M
  deriving (Input NewBorders) via FakeBorders M
  deriving (Input MouseButtons) via FakeMouseButtons M
  deriving (Input (Int32, Int32)) via FakePointer M
  deriving (Input [XineramaScreenInfo]) via FakeScreens M
  deriving (Input SubTiler, Output SubTiler, State SubTiler) via Coerce SubTiler M
  deriving (Input RootWindow) via (FromInput "rootWindow")
  deriving (Input Display) via (FromInput "display")
  deriving (Input Font.Font) via (FromInput "fontChoice")
  deriving (Input XCursor) via (FromInput "cursor")
  deriving (Log LogData) via (Logger M)

instance Semigroup a => Semigroup (M a) where
  a <> b = liftM2 (<>) a b
  
instance Monoid a => Monoid (M a) where
  mempty = return mempty

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
  -> Cursor
  -> M a -- The super long Monad which GHC can figure out on its own
  -> IO a
doAll ioref t c m d w f cur mon = do
  shouldLog <- newIORef False
  logHistory <- pure ioref
  activeMode <- newIORef m
  minimizedWindows <- newIORef S.empty
  yankBuffer <- newIORef [] 
  oldMouseButtons <- newIORef $ OMB None
  atomNameCache <- newIORef M.empty
  atomValueCache <- newIORef M.empty
  focusedWindow <- newIORef $ FocusedCache 0
  borderLocations <- newIORef M.empty
  windowLocations <- newIORef M.empty
  windowChildren <- newIORef M.empty
  shouldRedraw <- newIORef $ Just ()
  configuration <- newIORef c
  activeScreen <- newIORef 0
  lastTime <- newIORef $ OldTime currentTime
  knownDocks <- newIORef $ Docks []
  dockState <- newIORef Visible
  keyStatus <- newIORef Default
  stackCache <- newIORef []
  screenList <- newIORef t
  rootWindow <- pure w
  display <- pure d
  fontChoice <- pure f
  cursor <- pure $ XCursor cur
  R.runReaderT (runM mon) $ Ctx {..}

data TempType = FromMod | NotMod
  deriving Show

data KeyStatus = New KeyStatus Mode KeyCode [Action] | Temp TempType KeyStatus Mode KeyCode [Action] | Default
  deriving Show
makeBaseFunctor ''KeyStatus
