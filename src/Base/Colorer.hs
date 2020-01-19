{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Base.Colorer where

import           Standard
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Color
import qualified SDL
import qualified SDL.Font as Font
import Base.Helpers

-- |Handle any color stuff
data Colorer m a where
  GetColor :: String -> Colorer m Color
  ChangeColor :: SDL.Window -> (Int, Int, Int) -> Colorer m ()
  DrawText :: SDL.Window -> String -> Colorer m ()
  BufferSwap :: SDL.Window -> Colorer m ()
makeSem ''Colorer

-- |More IO
runColorer :: Member (State (Maybe Font.Font)) r => Interpret Colorer r a
runColorer = interpret $ \case
  GetColor color -> do
    display <- input @Display
    let colorMap = defaultColormap display (defaultScreen display)
    embed @IO $ fst <$> allocNamedColor display colorMap color

  ChangeColor w (h, s, v) -> do
    winSurface <- embed @IO $ SDL.getWindowSurface w
    embed @IO $ SDL.surfaceFillRect winSurface Nothing $ SDL.V4 (fromIntegral h) (fromIntegral s) (fromIntegral v) 0

  DrawText w s -> do
    whenM (null <$> get @(Maybe Font.Font)) $ do
      font <- embed @IO $ Font.load "/usr/share/fonts/adobe-source-code-pro/SourceCodePro-Medium.otf" 10
      put $ Just font
    mfont <- get @(Maybe Font.Font)
    -- If this ever fails, something has gone very wrong...
    let Just font = mfont
    surface <- embed @IO $ Font.blended font (SDL.V4 0 0 0 0) $ pack s
    winSurface <- embed @IO $ SDL.getWindowSurface w
    void . embed @IO $ SDL.surfaceBlit surface Nothing winSurface Nothing

  BufferSwap w -> embed @IO $ SDL.updateWindowSurface w
