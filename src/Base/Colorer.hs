{-# LANGUAGE UndecidableInstances #-}

module Base.Colorer where

import Graphics.X11.Xlib.Color
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Types
import qualified SDL
import qualified SDL.Font as Font
import Standard

-- TODO It might be worth replacing SDL with a raw X11 alternative. The entire
-- reason for using SDL is to get nice text rendering without much work.

-- | Handle any color stuff
class Colorer m where
  -- Converts a textual representation of a color into an Xlib color object
  getColor :: Text -> m Color
  -- Changes the background color of an SDL window
  changeColor :: SDL.Window -> (Int, Int, Int) -> m ()
  -- Draws text in the top left corner of an SDL window
  drawText :: SDL.Window -> Text -> m ()
  -- Renders the most recent changes to an SDL window
  bufferSwap :: SDL.Window -> m ()

-- | More IO
instance Members '[Input Font.Font, MonadIO, Input Display] m => Colorer m where
  getColor (Text color) = do
    display <- input @Display
    let colorMap = defaultColormap display (defaultScreen display)
    liftIO $ fst <$> allocNamedColor display colorMap color

  changeColor w (h, s, v) = do
    winSurface <- liftIO $ SDL.getWindowSurface w
    liftIO $ SDL.surfaceFillRect winSurface Nothing $ SDL.V4 (fromIntegral h) (fromIntegral s) (fromIntegral v) 0

  drawText w s = do
    font <- input @Font.Font
    surface <- liftIO $ Font.blended font (SDL.V4 0 0 0 0) s
    winSurface <- liftIO $ SDL.getWindowSurface w
    liftIO $ SDL.surfaceBlit surface Nothing winSurface Nothing
    SDL.freeSurface surface

  bufferSwap w = liftIO $ SDL.updateWindowSurface w
