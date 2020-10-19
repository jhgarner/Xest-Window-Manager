{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | There's a lot going on in this module and it can be really confusing if you
--  aren't familiar with some fairly new Haskell extensions. Xest's effect
--  system is provided by the Capability library. Capability provides what is
--  essentially the ReaderT pattern with some code for reducing
--  boilerplate. The boilerplate reducing code depends on deriving via, a GHC
--  extension, and a bunch of newtypes exported by the library. To further
--  reduce the boilerplate, I've used a Template Haskell function for some of
--  the derivations.
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
import Capability.Reader ()
import Capability.State ()
import Config
import qualified Control.Monad.Reader as R
import qualified Data.Map as M
import Graphics.X11.Types
import Graphics.X11.Xinerama (XineramaScreenInfo)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types
import qualified SDL
import qualified SDL.Font as Font
import Standard
import TH
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

-- | The large record Xest uses in the ReaderT type.
data Ctx = Ctx
  { shouldLog :: IORef (Bool),
    logHistory :: IORef [Text],
    activeMode :: IORef Mode,
    screenList :: IORef Screens,
    keyStatus :: IORef KeyStatus,
    yankBuffer :: IORef [SubTiler],
    oldMouseButtons :: IORef OldMouseButtons,
    atomNameCache :: IORef (M.Map Text Atom),
    atomValueCache :: IORef (M.Map Atom [Int]),
    focusedWindow :: IORef FocusedCache,
    borderLocations :: IORef (M.Map SDL.Window XRect),
    windowLocations :: IORef (M.Map Window XRect),
    windowChildren :: IORef (M.Map Window [ParentChild]),
    stackCache :: IORef [Window],
    shouldRedraw :: IORef (Maybe ()),
    configuration :: IORef Conf,
    activeScreen :: IORef ActiveScreen,
    lastTime :: IORef OldTime,
    knownDocks :: IORef Docks,
    dockState :: IORef DockState,
    rootWindow :: Window,
    display :: Display,
    fontChoice :: Font.Font,
    cursor :: XCursor
  }
  deriving (Generic)

-- Some helper type synonyms for creating Input/Output/State instances for
-- items in Ctx.
type FromInput name = Rename name (Field name () (MonadReader M))

type From name = ReaderIORef (FromInput name)

type Logged name s = LoggedSink name s (From name) M

-- Type aliases that should be used elsewhere but for now are just used to get
-- easy to access names when deriving Input/Output/State over these types.
type ShouldRedraw = Maybe ()

type Yanked = [SubTiler]

type LostWindow = Map Window [ParentChild]

-- The Main Monad for Xest.
newtype M a = M {runM :: R.ReaderT Ctx IO a}
  deriving (Functor, Applicative, Monad, MonadIO, R.MonadReader Ctx)
  deriving (Input NewBorders) via FakeBorders M
  deriving (Input MouseButtons) via FakeMouseButtons M
  deriving (Input (Int32, Int32)) via FakePointer M
  deriving (Input [XineramaScreenInfo]) via FakeScreens M
  deriving (Input RootWindow) via (FromInput "rootWindow")
  deriving (Input Display) via (FromInput "display")
  deriving (Input Font.Font) via (FromInput "fontChoice")
  deriving (Input XCursor) via (FromInput "cursor")
  deriving (Semigroup, Monoid) via Ap M a

-- Generates Input, Output, and State for various types on M. Don't worry too
-- much about the syntax/meta programming that's going on here. The template
-- haskell code expands to roughly the deriving lines up above but with Output
-- and State added to the deriving list.
generateIOS ''M ''LogLines [t|(From "logHistory")|]
generateIOS ''M ''Bool [t|(From "shouldLog")|]
generateIOS ''M ''Mode [t|(Logged "activeMode" Mode)|]
generateIOS ''M ''Screens [t|(Logged "screenList" Screens)|]
generateIOS ''M ''Yanked [t|(Logged "yankBuffer" [SubTiler])|]
generateIOS ''M ''OldMouseButtons [t|(Logged "oldMouseButtons" OldMouseButtons)|]
generateIOS ''M ''AtomCache [t|(Logged "atomNameCache" (M.Map Text Atom))|]
generateIOS ''M ''RootPropCache [t|(Logged "atomValueCache" (M.Map Atom [Int]))|]
generateIOS ''M ''WindowStack [t|(Logged "stackCache" [Window])|]
generateIOS ''M ''FocusedCache [t|(Logged "focusedWindow" FocusedCache)|]
generateIOS ''M ''SDLLocCache [t|(Logged "borderLocations" SDLLocCache)|]
generateIOS ''M ''LocCache [t|(Logged "windowLocations" LocCache)|]
generateIOS ''M ''LostWindow [t|(Logged "windowChildren" LostWindow)|]
generateIOS ''M ''ShouldRedraw [t|(Logged "shouldRedraw" ShouldRedraw)|]
generateIOS ''M ''Conf [t|(Logged "configuration" Conf)|]
generateIOS ''M ''ActiveScreen [t|(Logged "activeScreen" ActiveScreen)|]
generateIOS ''M ''OldTime [t|(Logged "lastTime" OldTime)|]
generateIOS ''M ''Docks [t|(Logged "knownDocks" Docks)|]
generateIOS ''M ''DockState [t|(Logged "dockState" DockState)|]
generateIOS ''M ''KeyStatus [t|(Logged "keyStatus" KeyStatus)|]
generateIOS ''M ''Tiler [t|FakeTiler M|]
generateIOS ''M ''SubTiler [t|Coerce SubTiler M|]

deriving via Logger M instance HasSink LogData LogData M

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
  M a -> -- The super long Monad which GHC can figure out on its own
  IO a
doAll ioref t c m d w f cur mon = do
  shouldLog <- newIORef False
  logHistory <- pure ioref
  activeMode <- newIORef m
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
