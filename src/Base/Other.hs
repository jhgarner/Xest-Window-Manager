{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Base.Other where

import           Standard
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xinerama
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import qualified SDL
import qualified SDL.Internal.Types as SI (Window(..))
import qualified SDL.Raw.Video as Raw
import           Foreign.C.String
import Base.Helpers
import Tiler.TilerTypes
import System.Mem.Weak
import Control.Monad.Trans
import Control.Monad.Trans.Control 

-- * Fake Inputs

-- $Fake
--
-- The following provide an Input-like interface. Because of that,
-- we're just going to pretend like they are inputs from the code's
-- point of view.

-- |Gets the current button presses from the Xserver
newtype FakeMouseButtons m a = FakeMouseButtons { runFakeMouseButtons :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)
instance MonadTrans FakeMouseButtons where
  lift = FakeMouseButtons
instance MonadTransControl FakeMouseButtons where
  type instance StT FakeMouseButtons a = a
  liftWith f = FakeMouseButtons $ f runFakeMouseButtons
  restoreT = lift
instance Members (MonadIO ': Inputs [RootWindow, Display]) m => HasSource MouseButtons MouseButtons (FakeMouseButtons m) where
  await_ _ = FakeMouseButtons do
    d <- input @Display
    root <- input @RootWindow
    liftIO $ do 
      (_, _, _, px, py, _, _, b) <- queryPointer d root
      let xLoc = fromIntegral px
      let yLoc = fromIntegral py

      allowEvents d asyncPointer currentTime
      return $ case b of
              _ | b .&. button1Mask /= 0-> LeftButton (xLoc, yLoc)
                | b .&. button3Mask /= 0-> RightButton (xLoc, yLoc)
                | otherwise -> None


newtype FakeScreens m a = FakeScreens { runFakeScreens :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)
instance MonadTrans FakeScreens where
  lift = FakeScreens
instance MonadTransControl FakeScreens where
  type instance StT FakeScreens a = a
  liftWith f = FakeScreens $ f runFakeScreens
  restoreT = lift
  
instance Members [Input Display, MonadIO] m => HasSource [XineramaScreenInfo] [XineramaScreenInfo] (FakeScreens m) where
  await_ _ = FakeScreens $ do
    d <- input @Display
    liftIO $ sync d False
    liftIO $ join . toList <$> xineramaQueryScreens d


newtype FakeBorders m a = FakeBorders { runFakeBorders :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)
instance MonadTrans FakeBorders where
  lift = FakeBorders
instance MonadTransControl FakeBorders where
  type instance StT FakeBorders a = a
  liftWith f = FakeBorders $ f runFakeBorders
  restoreT = lift
newtype NewBorders = NewBorders Borders
newtype XestBorders = XestBorders [Window]
instance MonadIO m => HasSource NewBorders NewBorders (FakeBorders m) where
  await_ _ = do
    windows <- liftIO $ replicateM 4 $ SI.Window <$> withCString "fakeWindowDontManage" (\s -> Raw.createWindow s 10 10 10 10 524288)
    -- TODO this way of handling borders is a little sketchy...
    let [lWin, dWin, uWin, rWin] = windows
    let nb = NewBorders (lWin, dWin, uWin, rWin)
    liftIO $ addFinalizer nb $ forM_ windows SDL.destroyWindow >> putStrLn "Finalized"
    return nb


newtype FakePointer m a = FakePointer { runFakePointer :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)
instance MonadTrans FakePointer where
  lift = FakePointer
instance MonadTransControl FakePointer where
  type instance StT FakePointer a = a
  liftWith f = FakePointer $ f runFakePointer
  restoreT = lift

-- |Gets the pointer location
instance Members (MonadIO ': Inputs [RootWindow, Display]) m => HasSource (Int32, Int32) (Int32, Int32) (FakePointer m) where
  await_ _ = FakePointer do
    d <- input
    root <- input @RootWindow
    (_, _, _, px, py, _, _, _) <- liftIO $ queryPointer d root
    return (fromIntegral px, fromIntegral py)

screenError :: a
screenError = error "Tried to switch to a screen that doesn't exist"


newtype FakeTiler m a = FakeTiler { runFakeTiler :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)
instance MonadTrans FakeTiler where
  lift = FakeTiler
instance MonadTransControl FakeTiler where
  type instance StT FakeTiler a = a
  liftWith f = FakeTiler $ f runFakeTiler
  restoreT = lift

instance Members [State ActiveScreen, State Screens] m => HasSink Tiler Tiler (FakeTiler m) where
  yield_ _ p = FakeTiler do
    activeScreen <- get @ActiveScreen
    modify @Screens $ set (at activeScreen) $ Just p
instance Members [State ActiveScreen, State Screens] m => HasSource Tiler Tiler (FakeTiler m) where
  await_ _ = FakeTiler do
    activeScreen <- get @ActiveScreen
    fromMaybe screenError . view (at activeScreen) <$> get @Screens
instance Members [State ActiveScreen, State Screens] m => HasState Tiler Tiler (FakeTiler m) where
  state_ p s = do
    oldState <- await_ p
    let (a, newState) = s oldState
    yield_ p newState
    pure a

newtype OldMouseButtons = OMB MouseButtons
  deriving Show
data MouseButtons = LeftButton (Int, Int) | RightButton (Int, Int) | None
  deriving Show
getButtonLoc :: MouseButtons -> Maybe (Int, Int)
getButtonLoc (LeftButton l) = Just l
getButtonLoc (RightButton l) = Just l
getButtonLoc _ = Nothing

type Screens = IntMap Tiler
type ActiveScreen = Int
type Pointer = (Int32, Int32)
