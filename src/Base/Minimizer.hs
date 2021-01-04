{-# LANGUAGE TemplateHaskell #-}

module Base.Minimizer where

import Base.Executor
import Base.Property
import Graphics.X11 (Display)
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Window
import Standard
import Tiler.TilerTypes

-- | Anything that changes a window state but doesn't actually move the window
--  goes here
data Minimizer a where
  Minimize :: Window -> Minimizer ()
  Restore :: Window -> Minimizer ()
makeEffect ''Minimizer

-- | Do the actions in IO
runMinimizer :: Members [State Tiler, Property, Executor, Input Display, IO] m => Eff (Minimizer ': m) a -> Eff m a
runMinimizer = interpret \case
  Minimize win -> do
    d <- input
    -- We only want to minimize if it hasn't already been minimized
    mappedWin <-
      liftIO $
        either (const waIsUnmapped) wa_map_state <$> try @SomeException (getWindowAttributes d win)
    when (mappedWin /= waIsUnmapped) $ do
      liftIO $ unmapWindow d win
      wm_state <- getAtom False "WM_STATE"
      putProperty 32 wm_state win wm_state [0, fromIntegral none]

  Restore win -> do
    d <- input
    -- Only restore if it needs to be restored
    mappedWin <-
      liftIO $
        either (const waIsViewable) wa_map_state <$> try @SomeException (getWindowAttributes d win)
    when (mappedWin == waIsUnmapped) $ do
      liftIO $ mapWindow d win
      wm_state <- getAtom False "WM_STATE"
      putProperty 32 wm_state win wm_state [1, fromIntegral none]
