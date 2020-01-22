{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Base.EventFlags where

import           Standard
import           Polysemy
import           Polysemy.Input
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import           Data.Bits
import Actions.ActionTypes
import Base.Helpers
import Config



-- |Controls the various event flags we can set on windows. See the Xlib docs for
-- a description of what those are.
data EventFlags m a where
  -- |Directly ask for flags on a window
  SelectFlags :: Window -- ^ The Window to set these flags on
              -> Mask -- ^ The flags (represented as a bitmask) to grab
              -> EventFlags m ()

  -- |Grab all mouse events on the root window
  SelectButtons :: Mode -- ^ The mode we want to bind buttons for
                -> EventFlags m ()

  -- |Grab all of the key evetns on the root window
  RebindKeys :: Mode -- ^ The mode we want to unbind keys for
             -> Mode -- ^ The mode we want to bind keys for
             -> EventFlags m ()
makeSem ''EventFlags

-- |Runs the event using IO
runEventFlags
  :: Members (Inputs [RootWindow, Conf]) r
  => Interpret EventFlags r a
runEventFlags = interpret $ \case
  SelectFlags w flags -> input >>= \d -> embed @IO $
    selectInput d w flags
    -- This is just grabs button presses on a window
    -- TODO why was I doing this here?

  SelectButtons NewMode {hasButtons = hb}  -> do
      d <- input @Display
      root <- input @RootWindow
      -- If the current mode wants to listen to the mouse, let it.
      -- Otherwise, don't because capturing the mouse prevents everyone
      -- else from using it.
      void . embed @IO $
        if not hb
           then ungrabPointer d currentTime
           else void $ grabPointer d root False pointerMotionMask grabModeAsync
                                   grabModeAsync root none currentTime

  RebindKeys oldMode activeMode -> do
    Conf kb _ _ <- input @Conf
    d           <- input @Display
    win         <- input @RootWindow

    -- Unbind the old keys
    embed $ forM_ kb $
      \(KeyTrigger k km _ _) -> when (oldMode == km)
        (ungrabKey d k anyModifier win)

    -- bind the new ones
    embed $ forM_ kb $
      \(KeyTrigger k km _ _) -> when (activeMode == km)
        (grabKey d k anyModifier win True grabModeAsync grabModeAsync)
