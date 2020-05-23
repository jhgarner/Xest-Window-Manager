{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UndecidableInstances #-}

module Base.Colorer where

import           Standard
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Color
import qualified SDL
import qualified SDL.Font as Font

-- |Handle any color stuff
class Colorer m where
  getColor :: Text -> m Color
  changeColor :: SDL.Window -> (Int, Int, Int) -> m ()
  drawText :: SDL.Window -> Text -> m ()
  bufferSwap :: SDL.Window -> m ()

-- |More IO
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
    void . liftIO $ SDL.surfaceBlit surface Nothing winSurface Nothing

  bufferSwap w = liftIO $ SDL.updateWindowSurface w
