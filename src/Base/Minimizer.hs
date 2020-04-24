{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Base.Minimizer where

import           Standard
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Window
import qualified Data.Set                      as S
import           Base.Property
import           Base.Executor
import           Base.Helpers
import           Tiler.TilerTypes


-- |Anything that changes a window state but doesn't actually move the window
-- goes here
data Minimizer m a where
  Minimize ::Window -> Minimizer m ()
  Restore ::Window -> Minimizer m ()
makeSem ''Minimizer

-- |Do the actions in IO
runMinimizer
  :: Members (States '[Set Window, Tiler]) r
  => Members [Property, Executor] r => Interpret Minimizer r a
runMinimizer = interpret $ \case
  Minimize win -> do
    d <- input
    -- We only want to minimize if it hasn't already been minimized
    mappedWin <- embed
      $ either (const waIsUnmapped) wa_map_state <$> try @SomeException (getWindowAttributes d win)
    when (mappedWin /= waIsUnmapped) $ do
      modify $ S.insert win
      embed @IO $ unmapWindow d win
      wm_state           <- getAtom False "WM_STATE"
      -- win_state :: [Int] <- getProperty 32 wm_state win
      -- unless (null win_state) $
      putProperty 32 wm_state win wm_state [0, fromIntegral none]

  Restore win -> do
    d <- input
    -- Only restore if it needs to be restored
    mappedWin <- embed
      $ either (const waIsViewable) wa_map_state <$> try @SomeException (getWindowAttributes d win)
    when (mappedWin == waIsUnmapped) $ do
      modify $ S.delete win
      embed @IO $ mapWindow d win
      wm_state           <- getAtom False "WM_STATE"
      -- win_state :: [Int] <- getProperty 32 wm_state win
      --unless (null win_state) $
      putProperty 32 wm_state win wm_state [1, fromIntegral none]
