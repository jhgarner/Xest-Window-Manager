{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Tiler.Tiler
import Base.Other



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

  -- |Grab all of the key events on the root window
  rebindKeys :: Mode -- ^ The mode we want to unbind keys for
             -> Mode -- ^ The mode we want to bind keys for
             -> m ()

newtype XCursor = XCursor Cursor
-- |Runs the event using IO
instance Members (MonadIO ': States [Screens, OldMouseButtons] ++ Inputs [RootWindow, Conf, Display, XCursor]) m => EventFlags m where
  selectFlags w flags = input >>= \d -> liftIO $ do
    sync d False
    selectInput d w flags

  selectButtons NewMode {hasButtons = hb} = do
      d <- input @Display
      root <- input @RootWindow
      XCursor cursor <- input @XCursor
      allWindows <- gets @Screens $ concatMap getAllParents . map snd . itoList
      put @OldMouseButtons $ OMB None
      forM_ allWindows \window -> selectFlags window
        $   substructureNotifyMask
        .|. substructureRedirectMask
        .|. buttonPressMask
        .|. buttonReleaseMask
      -- If the current mode wants to listen to the mouse, let it.
      -- Otherwise, don't because capturing the mouse prevents everyone
      -- else from using it.
      void . liftIO $
        if not hb
           then ungrabPointer d currentTime
           else void $ grabPointer d root True pointerMotionMask grabModeAsync
                          grabModeAsync root cursor currentTime
            --  grabButton d (button1 .|. button2) anyModifier root True buttonReleaseMask grabModeAsync grabModeAsync cursor currentTime
      forM_ allWindows \window -> selectFlags window
        $   substructureNotifyMask
        .|. substructureRedirectMask
        .|. leaveWindowMask
        .|. enterWindowMask
        .|. buttonPressMask
        .|. buttonReleaseMask

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
