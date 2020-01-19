{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Base.Global where

import           Standard
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Window
import           Graphics.X11.Xlib.Atom
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           System.Exit
import Base.Helpers
import Config
import Tiler.TilerTypes

-- |Anything that doesn't fit somewhere else. This should probably be
-- split up at some point.
data GlobalX m a where
   GetTree :: GlobalX m [Window]
   NewWindow :: Window -> GlobalX m Window
   MoveToRoot :: Window -> GlobalX m ()
   ClearQueue :: GlobalX m ()
   GetXEvent :: GlobalX m Event
   CheckXEvent :: GlobalX m Bool
   -- |Bool True == kill softly. False == kill hard
   Kill :: Bool -> Window -> GlobalX m (Maybe Window)
   -- If you look into the void, you can find anything
   Exit :: GlobalX m Void
makeSem ''GlobalX

-- |Filter out events we don't care about
eventFilter :: RootWindow -> Event -> Bool
eventFilter root ConfigureEvent {ev_window=win} = win == root
eventFilter _ ButtonEvent {ev_button=button} = button `notElem` [button5, button4]
eventFilter _ CrossingEvent {ev_detail=detail} = detail /= 2
eventFilter _ _ = True

-- |IO
runGlobalX
  :: (Members [Input RootWindow, Input Conf, State Bool, State Tiler] r)
  => Interpret GlobalX r a
runGlobalX = interpret $ \case
  GetTree -> do
    display <- input @Display
    root    <- input @RootWindow
    embed @IO $ alloca
      (\numChildrenPtr -> alloca
        (\childrenListPtr -> do
          uselessPtr <- alloca $ \x -> return x
          _          <- xQueryTree display
                                   root
                                   uselessPtr
                                   uselessPtr
                                   childrenListPtr
                                   numChildrenPtr
          numChildren <- peek numChildrenPtr
          peek childrenListPtr >>= peekArray (fromIntegral numChildren)
        )
      )

  NewWindow w -> do
    d         <- input @Display
    rootWin       <- input @RootWindow
    let defScr = defaultScreen d
    xwin <- embed $ createSimpleWindow d rootWin
      0 0 400 200 0
      (blackPixel d defScr)
      (blackPixel d defScr)
    embed $ mapWindow d xwin
    embed $ reparentWindow d w xwin 0 0
    return xwin

  MoveToRoot w -> do
    d         <- input @Display
    rootWin       <- input @RootWindow
    embed $ reparentWindow d w rootWin 0 0
  ClearQueue -> do
    d <- input @Display
    embed $ sync d True

  GetXEvent -> do
    d <- input
    root <- input
    embed @IO $
      untilM (eventFilter root) $
        allocaXEvent $ \p -> do
          
          -- eventsQueued d queuedAfterReading >>= System.IO.print
          nextEvent d p
          getEvent p 
  CheckXEvent -> do
    d <- input 
    root <- input
    embed @IO $ do
      -- Sync ourselves with the server
      sync d False

      -- If p > 0, getting the next event won't block
      -- and that would be true except we filter out Configure notifications.
      -- So if the queue were full of configure notifications, we would still
      -- end up blocking.
      p <- eventsQueued d queuedAfterFlush

      -- I decided to write this super imperitively.
      -- Basically, we want to remove the top p events if they would be filtered
      pRef <- newIORef $ fromIntegral p :: IO (IORef Int)
      -- If p < 1, we get to take the easy way out.
      if p < 1 
         then return False
         else do
          -- Otherwise, we loop for a while
          untilM (<1) $ allocaXEvent $ \p -> do
            event <- peekEvent d p >> getEvent p
            if eventFilter root event 
               -- We got something that won't be filtered so stop looping.
              then writeIORef pRef (-1)
              -- We got something that will be filtered so drop it.
              else nextEvent d p >> (eventsQueued d queuedAfterReading
                      >>= (writeIORef pRef . fromIntegral))
            readIORef pRef

          -- If P ended at -1, return True because the queue wasn't empty
          (/= 0) <$> readIORef pRef

  Kill isSoft w -> input >>= \d -> embed @IO $ do
    deleteName  <- internAtom d "WM_DELETE_WINDOW" False
    protocols <- internAtom d "WM_PROTOCOLS" True
    supportedProtocols <- getWMProtocols d w
    -- Thanks Xmonad for the kill window code
    if deleteName `elem` supportedProtocols
      then allocaXEvent $ \ev -> do
              Standard.putStrLn "Deleting using protocol"
              setEventType ev clientMessage
              setClientMessageEvent ev w protocols 32 deleteName currentTime
              sendEvent d w False noEventMask ev
              flush d
              return Nothing
      else if isSoft then destroyWindow d w >> Standard.putStrLn "Deleting using destroy window" >> return (Just w)
      else killClient d w >> Standard.putStrLn "Deleting using killClient" >> return (Just w)

  Exit -> embed exitSuccess

