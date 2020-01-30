{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Base.Mover where

import           Standard
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Window
import           Graphics.X11.Xlib.Event
import qualified Data.Map                      as M
import qualified SDL
import Base.Helpers
import Base.Minimizer
import Tiler.Tiler

-- |Anything that changes where a window sits goes in here.
data Mover m a where
  ChangeLocation :: Window -> XRect -> Mover m ()
  -- |Like the above but asks on SDL windows
  ChangeLocationS :: SDL.Window -> XRect -> Mover m ()
  Restack :: [Window] -> Mover m ()
  -- |A super light wrapper around actually configuring stuff
  ConfigureWin :: Event -> Mover m ()
  DestroySDLWindow :: SDL.Window -> Mover m ()
makeSem ''Mover

type LocCache = Map Window XRect
type SDLLocCache = Map SDL.Window XRect
type WindowStack = [Window]
-- Move windows using IO
runMover :: Members (States [LocCache, SDLLocCache, WindowStack, Tiler]) r
         => Member Minimizer r
         => Interpret Mover r a
runMover = interpret $ \case
  ChangeLocation win r@(Rect x y h w) -> do
    unlessM ((== Just r) . (M.!? win) <$> get) $ do
      d <- input
      void $ embed (moveWindow d win x y) >>
        if h == 0 || w == 0 then minimize win else embed (resizeWindow d win h w)
    modify $ M.insert win r

  ChangeLocationS win r@(Rect x y h w) -> do
    -- Avoid moving the SDL windows more than we need to
    unlessM ((== Just r) . (M.!? win) <$> get) $ do
      embed @IO $ SDL.setWindowPosition win $ SDL.Absolute $ SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))
      SDL.windowSize win SDL.$= SDL.V2 (fromIntegral h) (fromIntegral w)
    embed @IO $ SDL.raiseWindow win
    modify $ M.insert win r

  Restack wins -> do
    unlessM ((== wins) <$> get) $ do
      d <- input
      void . embed $ restackWindows d wins
    put wins


  ConfigureWin ConfigureRequestEvent {..} -> do
    d <- input
    tiler <- get @Tiler
    WindowAttributes {..} <- embed @IO $ getWindowAttributes d ev_window
    let wc = WindowChanges ev_x
                          ev_y
                          ev_width
                          ev_height
                          ev_border_width
                          ev_above
                          ev_detail
    if findWindow ev_window tiler
       then
         embed @IO $ allocaXEvent $ \pe -> do
           setEventType pe configureNotify
           setConfigureEvent pe ev_window ev_window wa_x wa_y wa_width wa_height wa_border_width none False
           sendEvent d ev_window False noEventMask pe
       else
         embed @IO $ configureWindow d ev_window ev_value_mask wc

  ConfigureWin _ -> error "Don't call Configure Window with other events"

  DestroySDLWindow window -> embed @IO $ SDL.destroyWindow window

-- |Run a mover when you don't actually want to do anything
runMoverFake :: Sem (Mover ': r) a -> Sem r a
runMoverFake = interpret $ \case
  ChangeLocation _ _ -> return ()

  ChangeLocationS _ _ -> return ()

  Restack _ -> return ()
  ConfigureWin _ -> return ()
  DestroySDLWindow _ -> return ()
