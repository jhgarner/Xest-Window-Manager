{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Monad.State.Strict (runStateT, StateT)
import Control.Monad.Reader (ReaderT(runReaderT))
import Graphics.X11.Xinerama (XineramaScreenInfo)


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
  void
    . flip runReaderT bIORef
    . flip runReaderT ioref
    . flip runStateT ([] :: [Text])
    . runLogger
    . runStateLogged @"Mode" @Mode m
    . runStateLogged @"Minimized set" (S.empty @Window)
    . runStateLogged @"KeyStatus" Default
    . runStateLogged @"Screens" t
    . runStateLogged @"Popped Tilers" ([] @SubTiler)
    . runStateLogged @"Mouse Button" (OMB None)
    . runStateLogged @"name->Atom" (M.empty @Text @Atom)
    . runStateLogged @"Atom->value" (M.empty @Atom @[Int])
    . runStateLogged @"Focused window" (FocusedCache 0)
    . runStateLogged @"SDL->location" (M.empty @SDL.Window @XRect)
    . runStateLogged @"Window->location" (M.empty @Window @XRect)
    . runStateLogged @"Window->transients" (M.empty @Window @[ParentChild])
    . runStateLogged @"Window stack" ([] @Window)
    . runStateLogged @"Redraw Flag" (Just ())
    . runStateLogged @"Config" c
    . runStateLogged @"Active screen" (0 :: ActiveScreen)
    . runStateLogged @"Time" currentTime
    . runStateLogged @"Docks" (Docks [])
    . runStateLogged @"Dock State" Visible
    . flip runReaderT w
    . flip runReaderT d
    . flip runReaderT f
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


instance Monad m => Input a (StateT a m) where
    input = get

data TempType = FromMod | NotMod
  deriving Show

data KeyStatus = New KeyStatus Mode KeyCode [Action] | Temp TempType KeyStatus Mode KeyCode [Action] | Default
  deriving Show
makeBaseFunctor ''KeyStatus
