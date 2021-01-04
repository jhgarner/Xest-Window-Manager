module Base.Other where

import Base.Helpers
import Control.Monad.Trans
import Foreign.C.String
import Graphics.X11.Types
import Graphics.X11.Xinerama
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Types
import qualified SDL
import qualified SDL.Internal.Types as SI (Window (..))
import qualified SDL.Raw.Video as Raw
import Standard
import System.Mem.Weak
import Tiler.TilerTypes

-- The following IO actions provide an Input-like interface. Because of that,
-- we're just going to pretend like they are inputs from the code's
-- point of view.
-- TODO There's not a lot of similarity between these and a lack of docs. This
-- module might need a closer look to improve it.

-- | Gets the current button presses from the Xserver
runFakeMouseButtons :: Members (IO ': Inputs [RootWindow, Display]) m => Eff (Input MouseButtons ': m) a -> Eff m a
runFakeMouseButtons = interpret \case
  Input -> do
    d <- input @Display
    root <- input @RootWindow
    liftIO $ do
      (_, _, _, px, py, _, _, b) <- queryPointer d root
      let xLoc = fromIntegral px
      let yLoc = fromIntegral py

      allowEvents d asyncPointer currentTime
      return $ case b of
        _
          | b .&. button1Mask /= 0 -> LeftButton (xLoc, yLoc)
          | b .&. button3Mask /= 0 -> RightButton (xLoc, yLoc)
          | otherwise -> None

runFakeScreens :: Members [Input Display, IO] m => Eff (Input [XineramaScreenInfo] ': m) a -> Eff m a
runFakeScreens = interpret \case
  Input -> do
    d <- input @Display
    liftIO $ sync d False
    liftIO $ join . toList <$> xineramaQueryScreens d

newtype NewBorders = NewBorders Borders

newtype XestBorders = XestBorders [Window]

runNewBorders :: (Members '[Input Display, IO] m) => Eff (Input NewBorders ': m) a -> Eff m a
runNewBorders = interpret \case
  Input -> do
    display <- input @Display
    windows <- liftIO $ replicateM 4 $ SI.Window <$> withCString "fakeWindowDontManage" (\s -> Raw.createWindow s 10 10 10 10 524288)
    -- TODO this way of handling borders is a little sketchy...
    let [lWin, dWin, uWin, rWin] = windows
    let nb = NewBorders (lWin, dWin, uWin, rWin)
    liftIO $ addFinalizer nb $ forM_ windows SDL.destroyWindow >> putStrLn "Finalized"
    liftIO $ sync display False
    return nb

-- | Gets the pointer location
runFakePointer :: Members (IO ': Inputs [RootWindow, Display]) m => Eff (Input (Int32, Int32) ': m) a -> Eff m a
runFakePointer = interpret \case
  Input -> do
    d <- input
    root <- input @RootWindow
    (_, _, _, px, py, _, _, _) <- liftIO $ queryPointer d root
    return (fromIntegral px, fromIntegral py)

screenError :: a
screenError = error "Tried to switch to a screen that doesn't exist"

runFakeTiler :: Members [State ActiveScreen, State Screens] m => Eff (State Tiler ': m) a -> Eff m a
runFakeTiler = interpret \case
  Put p -> do
    activeScreen <- get @ActiveScreen
    modify @Screens $ set (at activeScreen) $ Just p

  Get -> do
    activeScreen <- get @ActiveScreen
    fromMaybe screenError . view (at activeScreen) <$> get @Screens

-- class Members [State ActiveScreen, State Screens] m => HasState Tiler Tiler (FakeTiler m) where
--   state_ p s = do
--     oldState <- await_ p
--     let (a, newState) = s oldState
--     yield_ p newState
--     pure a

newtype OldMouseButtons = OMB MouseButtons
  deriving (Show)

data MouseButtons = LeftButton (Int, Int) | RightButton (Int, Int) | None
  deriving (Show)

getButtonLoc :: MouseButtons -> Maybe (Int, Int)
getButtonLoc (LeftButton l) = Just l
getButtonLoc (RightButton l) = Just l
getButtonLoc _ = Nothing

type ActiveScreen = Int

type Pointer = (Int32, Int32)
