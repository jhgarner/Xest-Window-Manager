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
import Data.List


data IterationState = IS {display :: Display
                         , numWindows :: Int
                         , event :: Event
                         }


someFunc :: IO ()
someFunc = do
  display <- openDisplay ":99"
  let rootWindow = defaultRootWindow display
  selectInput display rootWindow (substructureNotifyMask .|. substructureRedirectMask)
  mainLoop display 0

mapResponse :: IterationState -> IO Int
mapResponse IS {..} = mapWindow display (ev_window event) >> print "Mapping" >> return numWindows

-- TODO Probably move all of this to map?
configureResponse :: IterationState -> IO Int
configureResponse IS {..} = configureWindow display (ev_window event) (ev_value_mask event) wc >> moveWindow display (ev_window event) (fromIntegral numWindows * 100) 0 >> print numWindows >> return (numWindows + 1)
  where wc = WindowChanges (1000) (ev_y event) (ev_width event) (ev_height event) (ev_border_width event) (ev_above event) (ev_detail event)


mainLoop :: Display -> Int -> IO ()
mainLoop d numW = (allocaXEvent $ \xPtr -> do
  nextEvent d xPtr
  e <- getEvent xPtr
  eType <- get_EventType xPtr
  case find ((==) eType . fst) handlers of
    Nothing -> print "Unknown" >> return numW
    Just (_, f) -> f $ IS d numW e
  ) >>= mainLoop d

someEvent :: Display -> Event -> theState -> IO ()
someEvent = undefined


handlers :: [(EventType, IterationState -> IO Int)]
handlers = [(createNotify, \IS {..} -> print "Creating" >> return numWindows)
           , (destroyNotify, \IS {..} -> print "Destroying" >> return (numWindows - 1))
           , (configureRequest, configureResponse)
           , (mapRequest, mapResponse)]
