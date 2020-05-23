{-# LANGUAGE UndecidableInstances #-}
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
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import Actions.ActionTypes
import Base.Helpers
import Config



-- |Controls the various event flags we can set on windows. See the Xlib docs for
-- a description of what those are.
class EventFlags m where
  -- |Directly ask for flags on a window
  selectFlags :: Window -- ^ The Window to set these flags on
              -> Mask -- ^ The flags (represented as a bitmask) to grab
              -> m ()

  -- |Grab all mouse events on the root window
  selectButtons :: Mode -- ^ The mode we want to bind buttons for
                -> m ()

  -- |Grab all of the key evetns on the root window
  rebindKeys :: Mode -- ^ The mode we want to unbind keys for
             -> Mode -- ^ The mode we want to bind keys for
             -> m ()

-- |Runs the event using IO
instance Members (MonadIO ': Inputs [RootWindow, Conf, Display]) m => EventFlags m where
  selectFlags w flags = input >>= \d -> liftIO $
    selectInput d w flags
    -- This is just grabs button presses on a window
    -- TODO why was I doing this here?

  selectButtons NewMode {hasButtons = hb} = do
      d <- input @Display
      root <- input @RootWindow
      -- If the current mode wants to listen to the mouse, let it.
      -- Otherwise, don't because capturing the mouse prevents everyone
      -- else from using it.
      void . liftIO $
        if not hb
           then ungrabPointer d currentTime
           else void $ grabPointer d root False pointerMotionMask grabModeAsync
                                   grabModeAsync root none currentTime

  rebindKeys oldMode activeMode = do
    Conf kb _ _ _ <- input @Conf
    d           <- input @Display
    win         <- input @RootWindow

    -- Unbind the old keys
    liftIO $ forM_ kb $
      \(KeyTrigger k km _ _) -> when (oldMode == km)
        (ungrabKey d k anyModifier win)

    -- bind the new ones
    liftIO $ forM_ kb $
      \(KeyTrigger k km _ _) -> when (activeMode == km)
        (grabKey d k anyModifier win True grabModeAsync grabModeAsync)
