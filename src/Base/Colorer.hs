{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Base.Colorer where

import Graphics.X11.Xlib.Color
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Types
import qualified SDL
import qualified SDL.Font as Font
import Standard
import Control.Monad.Freer.TH

-- TODO It might be worth replacing SDL with a raw X11 alternative. The entire
-- reason for using SDL is to get nice text rendering without much work.

-- | Handle any color stuff
data Colorer a where
  -- Converts a textual representation of a color into an Xlib color object
  GetColor :: Text -> Colorer Color
  -- Changes the background color of an SDL window
  ChangeColor :: SDL.Window -> (Int, Int, Int) -> Colorer ()
  -- Draws text in the top left corner of an SDL window
  DrawText :: SDL.Window -> Text -> Colorer ()
  -- Renders the most recent changes to an SDL window
  BufferSwap :: SDL.Window -> Colorer ()
  
makeEffect ''Colorer

-- | More IO
runColorer :: Members '[IO, Input Font.Font, Input Display] m => Eff (Colorer ': m) a -> Eff m a
runColorer = interpret \case
  GetColor (Text color) -> do
    display <- input @Display
    let colorMap = defaultColormap display (defaultScreen display)
    send $ fst <$> allocNamedColor display colorMap color

  ChangeColor w (h, s, v) -> do
    winSurface <- SDL.getWindowSurface w
    SDL.surfaceFillRect winSurface Nothing $ SDL.V4 (fromIntegral h) (fromIntegral s) (fromIntegral v) 0

  DrawText w s -> do
    font <- input @Font.Font
    surface <- Font.blended font (SDL.V4 0 0 0 0) s
    winSurface <- SDL.getWindowSurface w
    SDL.surfaceBlit surface Nothing winSurface Nothing
    SDL.freeSurface surface

  BufferSwap w -> SDL.updateWindowSurface w
