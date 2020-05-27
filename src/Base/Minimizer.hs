{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Base.Minimizer where

import           Standard
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Window
import qualified Data.Set                      as S
import           Base.Property
import           Base.Executor
import           Base.Helpers
import           Tiler.TilerTypes
import Graphics.X11 (Display)


-- |Anything that changes a window state but doesn't actually move the window
-- goes here
class Minimizer m where
  minimize :: Window -> m ()
  restore :: Window -> m ()

-- |Do the actions in IO
instance Members (States '[Set Window, Tiler] ++ [Property, Executor, Input Display, MonadIO]) m => Minimizer m where
  minimize win = do
    d <- input
    -- We only want to minimize if it hasn't already been minimized
    mappedWin <- liftIO
      $ either (const waIsUnmapped) wa_map_state <$> try @SomeException (getWindowAttributes d win)
    when (mappedWin /= waIsUnmapped) $ do
      modify $ S.insert win
      liftIO $ unmapWindow d win
      wm_state           <- getAtom False "WM_STATE"
      -- win_state :: [Int] <- getProperty 32 wm_state win
      -- unless (null win_state) $
      putProperty 32 wm_state win wm_state [0, fromIntegral none]

  restore win = do
    d <- input
    -- Only restore if it needs to be restored
    mappedWin <- liftIO
      $ either (const waIsViewable) wa_map_state <$> try @SomeException (getWindowAttributes d win)
    when (mappedWin == waIsUnmapped) $ do
      modify $ S.delete win
      liftIO $ mapWindow d win
      wm_state           <- getAtom False "WM_STATE"
      -- win_state :: [Int] <- getProperty 32 wm_state win
      --unless (null win_state) $
      putProperty 32 wm_state win wm_state [1, fromIntegral none]
