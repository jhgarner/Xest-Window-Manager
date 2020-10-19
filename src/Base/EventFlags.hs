{-# LANGUAGE UndecidableInstances #-}

module Base.EventFlags where

import Actions.ActionTypes
import Base.Helpers
import Base.Mover
import Base.Other
import Config
import Graphics.X11 (raiseWindow)
import Graphics.X11.Types
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Types
import Standard
import Tiler.Tiler

newtype PointerTaker = PointerTaker Window

-- | Controls the various event flags we can set on windows. See the Xlib docs for
--  a description of what those are.
class EventFlags m where
  -- | Directly ask for flags on a window
  selectFlags ::
    -- | The Window to set these flags on
    Window ->
    -- | The flags (represented as a bitmask) to grab
    Mask ->
    m ()

  -- | Grab all mouse events on the root window
  selectButtons ::
    -- | The mode we want to bind buttons for
    Mode ->
    m ()

  -- | Grab all of the key events on the root window
  rebindKeys ::
    -- | The mode we want to unbind keys for
    Mode ->
    -- | The mode we want to bind keys for
    Mode ->
    m ()

newtype XCursor = XCursor Cursor

-- | Runs the event using IO
instance Members (MonadIO ': States [Screens, OldMouseButtons, FocusedCache, WindowStack] ++ Inputs [RootWindow, Conf, Display, XCursor, PointerTaker]) m => EventFlags m where
  selectFlags w flags =
    input >>= \d -> liftIO $ do
      sync d False
      selectInput d w flags

  selectButtons NewMode {hasButtons = hb} = do
    d <- input @Display
    root <- input @RootWindow
    PointerTaker pWin <- input @PointerTaker
    allWindows <- gets @Screens $ concatMap getAllParents . map snd . itoList
    put @OldMouseButtons $ OMB None
    forM_ allWindows \window ->
      selectFlags window $
        substructureNotifyMask
          .|. substructureRedirectMask
          .|. buttonPressMask
          .|. buttonReleaseMask
    -- If the current mode wants to listen to the mouse, let it.
    -- Otherwise, don't because capturing the mouse prevents everyone
    -- else from using it.
    when hb do
      put @FocusedCache $ FocusedCache root
      put @WindowStack []
      -- TODO determine which window is under the mouse and focus that one.
      liftIO $ setInputFocus d pWin revertToParent currentTime
      liftIO $ raiseWindow d pWin
    forM_ allWindows \window ->
      selectFlags window $
        substructureNotifyMask
          .|. substructureRedirectMask
          .|. leaveWindowMask
          .|. enterWindowMask
          .|. buttonPressMask
          .|. buttonReleaseMask

  rebindKeys oldMode activeMode = do
    Conf kb _ _ _ <- input @Conf
    d <- input @Display
    win <- input @RootWindow

    -- Unbind the old keys
    liftIO $
      forM_ kb $
        \(KeyTrigger k km _ _) ->
          when
            (oldMode == km)
            (ungrabKey d k anyModifier win)

    -- bind the new ones
    liftIO $
      forM_ kb $
        \(KeyTrigger k km _ _) ->
          when
            (activeMode == km)
            (grabKey d k anyModifier win True grabModeAsync grabModeAsync)
