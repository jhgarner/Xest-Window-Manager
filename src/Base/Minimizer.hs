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
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Window
import qualified Data.Set                      as S
import           Data.Bits
import           Base.Property
import           Base.Helpers
import           Tiler.TilerTypes
import           Tiler.ParentChild


-- |Anything that changes a window state but doesn't actually move the window
-- goes here
data Minimizer m a where
  Minimize ::Window -> Minimizer m ()
  Restore ::Window -> Minimizer m ()
  SetFocus ::Window -> Minimizer m ()
makeSem ''Minimizer

newtype FocusedCache = FocusedCache Window
  deriving Eq
-- |Do the actions in IO
runMinimizer
  :: Members (States '[Set Window, FocusedCache, Tiler]) r
  => Member Property r => Interpret Minimizer r a
runMinimizer = interpret $ \case
  Minimize win -> do
    d <- input
    -- We only want to minimize if it hasn't already been minimized
    WindowAttributes { wa_map_state = mapped } <- embed
      $ getWindowAttributes d win
    when (mapped /= waIsUnmapped) $ do
      modify $ S.insert win
      embed @IO $ unmapWindow d win
      wm_state           <- getAtom False "WM_STATE"
      win_state :: [Int] <- getProperty 32 wm_state win
      unless (null win_state)
        $ putProperty 32 wm_state win wm_state [0, fromIntegral none]

  Restore win -> do
    d <- input
    -- Only restore if it needs to be restored
    WindowAttributes { wa_map_state = mapped } <- embed
      $ getWindowAttributes d win
    when (mapped == waIsUnmapped) $ do
      modify $ S.delete win
      embed @IO $ mapWindow d win
      wm_state           <- getAtom False "WM_STATE"
      win_state :: [Int] <- getProperty 32 wm_state win
      unless (null win_state)
        $ putProperty 32 wm_state win wm_state [1, fromIntegral none]
    embed $ sync d False


  SetFocus w -> input @Display >>= \d -> do
    root <- get @Tiler
    unlessM ((== FocusedCache w) <$> get @FocusedCache) $ embed @IO $ do
      setInputFocus d w revertToNone currentTime
      cata (grabOthers d w) root

    embed @IO $ allowEvents d replayPointer currentTime
    put $ FocusedCache w
   where
    grabOthers d target (Wrap (ParentChild parent child))
      | child == target = ungrabButton d anyButton anyModifier parent
      | otherwise = grabButton d
                               anyButton
                               anyModifier
                               parent
                               False
                               (buttonPressMask .|. buttonReleaseMask)
                               grabModeSync
                               grabModeSync
                               none
                               none
    grabOthers _ _ t = sequence_ t

