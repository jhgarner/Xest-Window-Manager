{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

module Base.Unmanaged where

import           Standard
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Extras
import Base.Helpers
import Base.Mover
import Base.Minimizer
import Base.Property
import Foreign.C.Types (CLong)
import Tiler.ParentChild

data Docks = Docks {undock :: [Window]}
  deriving Show

data DockState = Hidden | Visible
  deriving (Show, Eq)

-- |Manages unmanaged windows such as docks
data Unmanaged m a where
  FocusUM :: Window -> Unmanaged m ()
  AddUM :: Window -> Unmanaged m ()
  ConstrainRect :: XRect -> Unmanaged m XRect
makeSem ''Unmanaged


runUnmanaged :: Members [State Docks, Input RootWindow, State DockState] r
             => Members '[Mover, Property, Minimizer] r
             => Interpret Unmanaged r a
runUnmanaged = interpret $ \case
  FocusUM win -> do
    root <- input @RootWindow
    setFocus (ParentChild root win)

  AddUM win -> do
    restore win
    modify @Docks (Docks . (:) win . undock)

  ConstrainRect rect -> do
    Docks docks <- get @Docks
    dockState <- get @DockState
    root <- input @RootWindow
    display <- input @Display
    WindowAttributes _ _ sw sh _ _ _ _ _ <- embed @IO $ getWindowAttributes display root

    nwsp <- getAtom False "_NET_WM_STRUT_PARTIAL"
    strutRects <-
      if (dockState == Visible)
        then
          forM docks \win -> do
            restore win
            struts <- map fromIntegral <$> getProperty @_ @CLong 32 nwsp win
            return $ map (bimap fromIntegral fromIntegral)
              case struts of
                -- Yikes, this is a big line...
                [left, right, top, bottom, leftStarty, leftEndy, rightStarty, rightEndy, topStartx, topEndx, bottomStartx, bottomEndx] ->
                  [ Rect 0 leftStarty left (leftEndy - leftStarty)
                  , Rect (sw-right) rightStarty right (rightEndy - rightStarty)
                  , Rect topStartx 0 (topEndx - topStartx) top
                  , Rect bottomStartx (sh - bottom) (bottomEndx - bottomStartx) bottom
                  ]
                _ -> [mempty, mempty, mempty, mempty]
        else
          forM_ docks minimize >> return []
    return $ foldl' constrain rect $ strutRects
      where
        -- Yikes, I don't like this function
        constrain :: XRect -> [XRect] -> XRect
        constrain oldRect [left, right, top, bottom] =
          let leftBounds :: XRect -> Maybe XRect
              leftBounds (Rect nx ny nw nh)
                | w left > fromIntegral nx && w left < fromIntegral nx + nw && ny < y left + fromIntegral (h left) && ny + fromIntegral nh > y left =
                  Just $ Rect (fromIntegral $ w left) ny (fromIntegral nx + nw - w left) nh
                | otherwise = Nothing
              rightBounds :: XRect -> Maybe XRect
              rightBounds (Rect nx ny nw nh)
                | x right > nx && x right < nx + fromIntegral nw && ny < y right + fromIntegral (h right) && ny + fromIntegral nh > y right =
                  Just $ Rect nx ny (fromIntegral $ x right - nx) nh
                | otherwise = Nothing
              topBounds :: XRect -> Maybe XRect
              topBounds (Rect nx ny nw nh)
                | h top > fromIntegral ny && h top < fromIntegral ny + nh && nx < x top + fromIntegral (w top) && nx + fromIntegral nw > x top =
                  Just $ Rect nx (fromIntegral $ h top) nw (fromIntegral ny + nh - h top)
                | otherwise = Nothing
              bottomBounds :: XRect -> Maybe XRect
              bottomBounds (Rect nx ny nw nh)
                | y bottom > ny && y bottom < ny + fromIntegral nh && nx < x bottom + fromIntegral (w bottom) && nx + fromIntegral nw > x bottom =
                  Just $ Rect nx ny nw (fromIntegral $ y bottom - ny)
                | otherwise = Nothing
              pass :: (a -> Maybe a) -> a -> a
              pass f1 a =  fromMaybe a $ f1 a
           in pass leftBounds . pass rightBounds . pass topBounds . pass bottomBounds $ oldRect

        constrain oldRect _ = oldRect

