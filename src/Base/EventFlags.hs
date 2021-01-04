{-# LANGUAGE TemplateHaskell #-}

module Base.EventFlags where

import Actions.ActionTypes
import Base.Helpers
import Base.Other
import Config
import Graphics.X11.Types
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Types
import Standard
import Tiler.Tiler

newtype PointerTaker = PointerTaker Window

-- | Controls the various event flags we can set on windows. See the Xlib docs for
--  a description of what those are.
data EventFlags a where
  -- | Directly ask for flags on a window
  SelectFlags ::
    -- | The Window to set these flags on
    Window ->
    -- | The flags (represented as a bitmask) to grab
    Mask ->
    EventFlags ()

  -- | Grab all of the key events on the root window
  RebindKeys ::
    -- | The mode we want to unbind keys for
    Mode ->
    -- | The mode we want to bind keys for
    Mode ->
    EventFlags ()
makeEffect ''EventFlags

newtype XCursor = XCursor Cursor

-- | Runs the event using IO
runEventFlags :: Members (IO ': States [Screens, OldMouseButtons] ++ Inputs [RootWindow, Conf, Display, XCursor]) m => Eff (EventFlags ': m) a -> Eff m a
runEventFlags = interpret \case
  SelectFlags w flags ->
    input >>= \d -> liftIO $ do
      sync d False
      selectInput d w flags

  RebindKeys oldMode activeMode -> do
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
