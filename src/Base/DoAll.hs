{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
-- import           Control.Monad.State
-- import           Polysemy.State
-- import           Polysemy.Input
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
import Control.Monad.State.Strict (runStateT, StateT)
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.Trans.Control
import Control.Monad.Cont


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
  -> _ -- The super long Monad which GHC can figure out on its own
  -> IO ()
doAll ioref t c m d w f mon = do
  bIORef <- newIORef False
  t <- travHList newIORef $ m ::: (S.empty @Window) ::: Default ::: t ::: ([] @SubTiler) ::: (OMB None) ::: (M.empty @Text @Atom) ::: (M.empty @Atom @[Int]) ::: (FocusedCache 0) ::: (M.empty @SDL.Window @XRect) ::: (M.empty @Window @XRect) ::: (M.empty @Window @[ParentChild]) ::: ([] @Window) ::: (Just ()) ::: c ::: (0 :: ActiveScreen) ::: currentTime ::: (Docks []) ::: Visible ::: HNil
  void
    . flip runReaderT bIORef
    . flip runReaderT ioref
    . flip runStateT ([] :: [Text])
    . runLogger
    -- . runStateLogged @"Mode" @Mode m
    -- . runStateLogged @"Minimized set" (S.empty @Window)
    -- . runStateLogged @"KeyStatus" Default
    -- . runStateLogged @"Screens" t
    -- . runStateLogged @"Popped Tilers" ([] @SubTiler)
    -- . runStateLogged @"Mouse Button" (OMB None)
    -- . runStateLogged @"name->Atom" (M.empty @Text @Atom)
    -- . runStateLogged @"Atom->value" (M.empty @Atom @[Int])
    -- . runStateLogged @"Focused window" (FocusedCache 0)
    -- . runStateLogged @"SDL->location" (M.empty @SDL.Window @XRect)
    -- . runStateLogged @"Window->location" (M.empty @Window @XRect)
    -- . runStateLogged @"Window->transients" (M.empty @Window @[ParentChild])
    -- . runStateLogged @"Window stack" ([] @Window)
    -- . runStateLogged @"Redraw Flag" (Just ())
    -- . runStateLogged @"Config" c
    -- . runStateLogged @"Active screen" (0 :: ActiveScreen)
    -- . runStateLogged @"Time" currentTime
    -- . runStateLogged @"Docks" (Docks [])
    -- . runStateLogged @"Dock State" Visible
-- '[Mode, Set Window, KeyStatus, Screens,
--                                                      [SubTiler], OldMouseButtons, Map Text Atom,
--                                                      Map Atom [Int], FocusedCache,
--                                                      Map SDL.Window XRect, Map Window XRect,
--                                                      Map Window [ParentChild], [Window], Maybe (),
--                                                      Conf, ActiveScreen, Time, Docks, DockState]
    . flip runReaderT t
    -- . (runReads :: Reads '[IORef Mode, IORef (Set Window), IORef KeyStatus, IORef (IntMap Tiler), IORef [SubTiler], IORef OldMouseButtons, IORef (Map Text Atom), IORef (Map Atom [Int]), IORef FocusedCache, IORef (Map SDL.Window XRect), IORef (Map Window XRect), IORef (Map Window [ParentChild]), IORef [Window], IORef (Maybe ()), IORef (ConfA KeyCode), IORef Int, IORef Word64, IORef Docks, IORef DockState] m a -> m a)
    . (runReads :: Reads '[Mode, (Set Window), KeyStatus, (IntMap Tiler), [SubTiler], OldMouseButtons, (Map Text Atom), (Map Atom [Int]), FocusedCache, (Map SDL.Window XRect), (Map Window XRect), (Map Window [ParentChild]), [Window], (Maybe ()), (ConfA KeyCode), Int, Word64, Docks, DockState] m a -> m a)
    -- . dropAReads @Conf
    -- . dropAReads @Mode
    -- . dropAReads @(Set Window)
    -- . dropAReads @KeyStatus
    -- . dropAReads @Screens
    -- . dropAReads @[SubTiler]
    -- . dropAReads @OldMouseButtons
    -- . dropAReads @(Map Text Atom)
    -- . dropAReads @(Map Atom [Int])
    -- . dropAReads @FocusedCache
    -- . dropAReads @(Map SDL.Window XRect)
    -- . dropAReads @(Map Window XRect)
    -- . dropAReads @(Map Window [ParentChild])
    -- . dropAReads @[Window]
    -- . dropAReads @(Maybe ())
    -- . dropAReads @ActiveScreen
    -- . dropAReads @Time
    -- . dropAReads @Docks
    -- . dropAReads @DockState
    
    . flip runReaderT w
    . flip runReaderT d
    . flip runReaderT f
    . (runFakeInputState :: FakeInputState (ConfA KeyCode) m a -> m a)
    . (runFakeInputState :: FakeInputState (IntMap Tiler) m a -> m a)
    . runFakeBorders
    . runFakeTiler
    . runFakePointer
    . runFakeScreens
    . runFakeMouseButtons
    $ mon
    -- . stateToInput @Conf
    -- . stateToInput @Screens
    -- . runNewBorders
    -- . runGetScreens
    -- . indexedState
    -- . runGetButtons
    -- . runGetPointer
    -- . runExecutor
    -- . runProperty
    -- . runEventFlags
    -- . runGlobalX
    -- . runMinimizer
    -- . runMover
    -- . runColorer
    -- . runUnmanaged
 where
  -- logger :: Members '[State Bool, State [Text],  IO] r => LogAction (Sem r) LogData
  -- logger = LogAction $ \(LD prefix msg) -> do
  --   timeZone <- embed getCurrentTimeZone
  --   timeUtc <- embed getCurrentTime
  --   let timeStamp = "[" <> Text (formatShow iso8601Format (utcToLocalTime timeZone timeUtc)) <> "]"
  --       prefixWrap = "[" <> prefix <> "]"
  --       fullMsg:: Text = prefixWrap <> timeStamp <> msg
  --   modify' @[Text] $ force . take 100 . (:) fullMsg
  --   shouldLog <- get @Bool
  --   when shouldLog $
  --     embed @IO $ appendFile "/tmp/xest.log" (fullMsg <> "\n")
  -- {-# INLINE logger #-}


newtype FakeInputState s m a = FakeInputState { runFakeInputState :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)
instance MonadTrans (FakeInputState s) where
  lift = FakeInputState
instance MonadTransControl (FakeInputState s) where
  type instance StT (FakeInputState s) a = a
  liftWith f = FakeInputState $ f runFakeInputState
  restoreT = lift
instance (Monad m, State a m) => Input a (FakeInputState a m) where
    input = get

data TempType = FromMod | NotMod
  deriving Show

data KeyStatus = New KeyStatus Mode KeyCode [Action] | Temp TempType KeyStatus Mode KeyCode [Action] | Default
  deriving Show
makeBaseFunctor ''KeyStatus
