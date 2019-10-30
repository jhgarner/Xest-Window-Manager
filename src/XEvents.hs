{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}


module XEvents where

import           Standard
import           Base
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Types
import           Types
import           Data.Either                    ( )
import           Tiler
import Core
import           FocusList
import           Data.Bits
import qualified Data.Map.Strict as M

mapEvent :: Member (State (Fix Tiler)) r
         => Members (Inputs [Pointer, Screens]) r
         => Members [EventFlags, GlobalX] r
         => Window -> Sem r ()
mapEvent window = do
  -- Before we do anything else, let's see if we already manage this window.
  -- I don't know why, but things like Inkscape map windows multiple times.
  tree <- get @(Fix Tiler)
  -- TODO You're about to see these 3 lines of code a whole lot. Maybe they should
  -- be a function or something.
  pointer <- input @Pointer
  screens <- input @Screens
  let i = maybe 0 fst $ whichScreen pointer $ zip [0..] screens

  unless (cata (findWindow window) tree) $ do
    -- managing a window allows us to modify it in whatever ways we want.
    -- In our case, we will place it in a parent window and ask for some
    -- events.
    tWin <- manage window

    -- This adds the new window to whatever tiler comes after inputController
    -- If you've zoomed the inputController in, you get nesting as a result
    modify . cata . applyInput i $ Just . pushOrAdd tWin
  where findWindow w (Wrap w') = inChildParent w w'
        findWindow _ t = or t
        -- Used for reparenting
        manage :: Members [EventFlags, GlobalX] r => Window -> Sem r (Fix Tiler)
        manage w = do
          newWin <- newWindow w
          selectFlags newWin (substructureNotifyMask .|. substructureRedirectMask .|. structureNotifyMask .|. enterWindowMask )-- .|. buttonPressMask .|. buttonReleaseMask)
          return . Fix $ Wrap $ ChildParent newWin w

