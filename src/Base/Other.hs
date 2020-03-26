{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Base.Other where

import           Standard
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Data.Bits
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

-- * Fake Inputs

-- $Fake
--
-- The following provide an Input-like interface. Because of that,
-- we're just going to pretend like they are inputs from the code's
-- point of view.

-- |Gets the current button presses from the Xserver
runGetButtons :: Members (Inputs [RootWindow, Display]) r 
              => Member (Embed IO) r
              => Sem (Input MouseButtons ': r) a -> Sem r a
runGetButtons = runInputSem $ do
  d <- input @Display
  root <- input @RootWindow
  embed @IO $ do 
    (_, _, _, px, py, _, _, b) <- queryPointer d root
    let x = fromIntegral px
    let y = fromIntegral py

    allowEvents d asyncPointer currentTime
    return $ case b of
            _ | b .&. button1Mask /= 0-> LeftButton (x, y)
              | b .&. button3Mask /= 0-> RightButton (x, y)
              | otherwise -> None

runGetScreens :: Members [Input Display, Embed IO] r
              => Sem (Input [XineramaScreenInfo] ': r) a -> Sem r a
runGetScreens = interpret $ \case
  Input -> do
    d <- input @Display
    embed $ join . toList <$> xineramaQueryScreens d

newtype NewBorders = NewBorders Borders
runNewBorders :: Member (Embed IO) r
              => Sem (Input NewBorders ': r) a -> Sem r a
runNewBorders = runInputSem $ do
  wins <- embed @IO (replicateM 4 $
    SI.Window <$> withCString "fakeWindowDontManage" (\s -> Raw.createWindow s 10 10 10 10 524288))
  -- TODO this way of handling borders is a little sketchy...
  let [lWin, dWin, uWin, rWin] = wins
  return $ NewBorders (lWin, dWin, uWin, rWin)

-- |Gets the pointer location
runGetPointer :: Members (Inputs [RootWindow, Display]) r 
              => Member (Embed IO) r
              => Sem (Input (Int32, Int32) ': r) a -> Sem r a
runGetPointer = runInputSem $ input >>= \d -> do
  root <- input @RootWindow
  (_, _, _, px, py, _, _, _) <- embed $ queryPointer d root
  return (fromIntegral px, fromIntegral py)

screenError :: a
screenError = error "Tried to switch to a screen that doesn't exist"

indexedState
  :: Members [State ActiveScreen, State Screens] r
  => Sem (State Tiler : r) a
  -> Sem r a
indexedState = interpret $ \case
  Get -> do
    activeScreen <- get @ActiveScreen
    screenTree . fromMaybe screenError . lookup activeScreen <$> get @Screens
  Put p -> do
    activeScreen <- get @ActiveScreen
    modify @Screens $ adjustMap (\s -> s {screenTree = p}) activeScreen
      
data MouseButtons = LeftButton (Int, Int) | RightButton (Int, Int) | None
  deriving Show
getButtonLoc :: MouseButtons -> Maybe (Int, Int)
getButtonLoc (LeftButton l) = Just l
getButtonLoc (RightButton l) = Just l
getButtonLoc _ = Nothing

type Screens = Map Int Screen'
type ActiveScreen = Int
data Screen' = Screen' { screenSize :: XRect
                       , screenTree :: Tiler
                       , screenBorders :: Borders
                       }
  deriving Show

type Borders = (SDL.Window, SDL.Window, SDL.Window, SDL.Window)
type Pointer = (Int32, Int32)
