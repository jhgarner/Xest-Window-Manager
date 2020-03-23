{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

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
import           Tiler.TilerTypes
import           Tiler.ParentChild
import           Config
import           Actions.ActionTypes
import           System.IO (appendFile)
import           Colog.Polysemy.Effect
import           Colog.Core
import qualified SDL.Font as Font


type LostWindow = Map Window [ParentChild]

-- Want to do everything in IO? Use this!
doAll
  :: Screens
  -> Conf
  -> Mode
  -> Display
  -> Window
  -> Font.Font
  -> _ -- The super long Sem list which GHC can figure out on its own
  -> IO ()
doAll t c m d w f =
  void
    . runM
    . runState False
    . runState ([] @String)
    . runLogActionSem logger
    . runStateLogged (m, "Mode")
    . runStateLogged (S.empty @Window, "Minimized set")
    . runStateLogged (Default, "KeyStatus")
    . runStateLogged (t, "Screens")
    . runStateLogged ([] @SubTiler, "Popped Tilers")
    . runStateLogged (None, "Mouse Button")
    . runStateLogged (M.empty @String, "name->Atom")
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
    . runInputs (w ::: d ::: f ::: HNil)
    . stateToInput @Conf
    . stateToInput @Screens
    . smartBorders
    . runNewBorders
    . runGetScreens
    . listOfScreens
    . indexedState
    . runInputScreen
    . runGetButtons
    . runGetPointer
    . runExecutor
    . runProperty
    . runEventFlags
    . runGlobalX
    . runMinimizer
    . runMover
    . runColorer
 where
  logger :: Members '[State Bool, State [String], Embed IO] r => LogAction (Sem r) String
  logger = LogAction $ \msg -> do
    shouldLog <- get @Bool
    modify @[String] $ (:) msg
    modify @[String] $ take 100
    when shouldLog $
      embed @IO $ appendFile "/tmp/xest.log" (msg ++ "\n")



  -- Get the screens from Xinerama
  runInputScreen
    :: Members (States '[ActiveScreen, Screens]) r
    => Sem (Input XRect ': r) a
    -> Sem r a
  runInputScreen = runInputSem $ do
    activeScreen <- get @ActiveScreen
    gets @Screens $ screenSize . fromMaybe screenError . lookup activeScreen

  listOfScreens
    :: Member (Input Screens) r => Sem (Input [XRect] ': r) a -> Sem r a
  listOfScreens = interpret $ \case
    Input -> toList . fmap screenSize <$> input @Screens
  smartBorders
    :: Members '[Input Screens, State ActiveScreen] r
    => Sem (Input Borders ': r) a
    -> Sem r a
  smartBorders = interpret $ \case
    Input ->
      get
        >>= (\activeScreen ->
              screenBorders
                .   fromMaybe screenError
                .   lookup activeScreen
                <$> input @Screens
            )
  stateToInput :: Member (State a) r => Sem (Input a ': r) b -> Sem r b
  stateToInput = interpret $ \case
    Input -> get

data TempType = FromMod | NotMod
  deriving Show

data KeyStatus = New KeyStatus Mode KeyCode [Action] | Temp TempType KeyStatus Mode KeyCode [Action] | Default
  deriving Show
makeBaseFunctor ''KeyStatus
