{-# LANGUAGE RecordWildCards #-}

module Lib
    ( someFunc
    ) where

import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Window
import Graphics.X11.Xlib.Types
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Types
import Data.Bits
import Foreign.Marshal.Alloc
import Control.Monad
import Control.Monad.Trans.Reader


data IterationState = IS {display :: Display
                         , numWindows :: Int
                         , event :: Event
                         }


someFunc :: IO ()
someFunc = do
  display <- openDisplay ":99"
  let rootWindow = defaultRootWindow display
  selectInput display rootWindow (substructureNotifyMask .|. substructureRedirectMask)
  forever . allocaXEvent $ handleEvent display

handleEvent :: Display -> XEventPtr -> IO ()
handleEvent d ep = do
  nextEvent d ep
  e <- getEvent ep
  xe <- get_EventType ep
  case xe of
    _ | createNotify == xe -> print "Creating"
      | destroyNotify == xe -> print "Destroying"
      | reparentNotify == xe -> print "parenting"
      | configureRequest == xe -> configureResponse d e
      | mapRequest == xe -> mapResponse d e
      | otherwise -> print "Unknown"

configureResponse :: Display -> Event -> IO ()
configureResponse d e = configureWindow d (ev_window e) (ev_value_mask e) wc >> print "Configuring"
  where wc = WindowChanges (ev_x e) (ev_y e) (ev_width e) (ev_height e) (ev_border_width e) (ev_above e) (ev_detail e)

mapResponse :: Display -> Event -> IO ()
mapResponse d e = mapWindow d (ev_window e) >> print "Mapping"

-- configureResponse :: IterationState -> IO ()
-- configureResponse IS {..} = configureWindow display (ev_window event) (ev_value_mask event) wc >> print "Configuring"
--   where wc = WindowChanges (ev_x e) (ev_y e) (ev_width e) (ev_height e) (ev_border_width e) (ev_above e) (ev_detail e)


-- mainLoop :: Display -> Int -> IO ()
-- mainLoop d numW= allocaXEvent $ \xPtr -> do
--   nextEvent d xPtr
--   e <- getEvent xPtr
--   eType <- get_EventType xPtr
--   IS {..} <- ask
--   newS <- handleEvents event
--   mainLoop newS

-- -- handleEvents :: ReaderT IterationState IO ()
-- -- handleEvents = do
-- --   IS {..} <- ask

-- someEvent :: Display -> Event -> theState -> IO ()
-- someEvent = undefined


-- handlers :: [(EventType, IterationState -> IO Int)]
-- handlers = [(createNotify, \IS {..} -> print "Creating" >> return numWindows)
--            , (destroyNotify, \IS {..} -> print "Destroying" >> return (numWindows - 1))]
--            --, (configureNotify, )]
