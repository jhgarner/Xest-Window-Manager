{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}


{-|
   Core contains important Xlib interfacing code that doesn't belong
   in any other file.
-}
module Core where

import           Standard
import           Base
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Graphics.X11.Types
import           Types
import           Data.Either                    ( )
import           Data.Char
import           Tiler
import           FocusList

-- |Find a window with a class name. This is used when
-- showing or hiding a window.
getWindowByClass
  :: Members [Property, GlobalX] r
  => String
  -> Sem r [Window]
getWindowByClass wName = do
  childrenList <- getTree
  filterM findWindow childrenList
  where findWindow win = (== wName) <$> getClassName win

type RenderEffect r =
     ( Members (Inputs [Borders, Screens, Pointer]) r
     , Members (States [Fix Tiler, Mode]) r
     , Members [Mover, Minimizer, Colorer, GlobalX] r
     )
-- |Moves windows around based on where they are in the tiler.
render
  :: (RenderEffect r, Member (State [Fix Tiler]) r)
  => Fix Tiler
  -> Sem r [Window]
render t = do
  screens <- input @Screens
  pointer <- input @Pointer
  let i' = maybe 0 fst $ whichScreen pointer $ zip [0..] screens

  let locations = topDown (placeWindow screens) (Transformer id id $ Plane (Rect 0 0 0 0) 0) t

  -- Draw the tiler we've been given. winOrder will be used by restackWindows
  -- while io coantains the io action which moves the windows.
  let (winOrder, io) = cata (draw i') $ fmap unTransform locations
  io

  -- Hide all of the popped tilers
  get @[Fix Tiler]
    >>= traverse_ (snd . cata (draw i') . fmap unTransform . topDown (placeWindow $ repeat $ Rect 0 0 0 0) (Transformer id id $ Plane (Rect 0 0 0 0) 0))

  return winOrder

       -- The main part of this function.
 where draw :: RenderEffect r => Int -> Base (Cofree Tiler Plane) ([Window], Sem r ()) -> ([Window], Sem r ())
       draw _ (Plane (Rect _ _ 0 0) _ :<~ Wrap (ChildParent win _)) = ([], minimize win)
       draw _ (Plane Rect {..} _ :<~ Wrap (ChildParent win win')) = ([win], do
           restore win
           restore win'
           changeLocation win $ Rect x y (abs w) (abs h)
           changeLocation win' $ Rect 0 0 (abs w) (abs h))
       draw i' (Plane Rect{..} depth :<~ InputController i t)
         | i == i' = 
           (maybe [] fst t, do
              mapM_ snd t
              -- Extract the border windows
              (l, u, r, d) <- input @Borders
              let winList = [l, u, r, d]

              -- Calculate the color for our depth
              let hue = 360.0 * ((0.5 + (fromIntegral (depth - 1) * 0.618033988749895)) `mod'` 1)

              currentMode <- get
              if hasBorders currentMode
                then do
                    -- Draw them with the right color and position
                    changeLocationS l $ Rect x y 2 h
                    changeLocationS u $ Rect (x + 2) y (w-2) 10
                    changeLocationS d $ Rect x (y+fromIntegral h-2) w 2
                    changeLocationS r $ Rect (x+fromIntegral w-2) y 2 h

                    traverse_ (`changeColor` hsvToRgb hue 0.5 0.9) winList
                    get @(Fix Tiler) >>= drawText u . cata getFocusList
                else do
                    changeLocationS l $ Rect 10000 0 0 0
                    changeLocationS u $ Rect 10000 0 0 0
                    changeLocationS d $ Rect 10000 0 0 0
                    changeLocationS r $ Rect 10000 0 0 0
                    traverse_ (`changeColor` hsvToRgb hue 0.5 0.9) winList
                    get @(Fix Tiler) >>= drawText u . cata getFocusList
                    
              traverse_ bufferSwap winList
           )
         | otherwise = (maybe [] fst t, mapM_ snd t)

       draw _ (_ :<~ Floating ls) = 
          (tops ++ bottoms, mapM_ (snd . getEither) ls)
              where tops = foldl' onlyTops [] ls
                    onlyTops acc (Top (_, (ws, _))) = ws ++ acc
                    onlyTops acc _ = acc
                    bottoms = foldl' onlyBottoms [] ls
                    onlyBottoms acc (Bottom (ws, _)) = acc ++ ws
                    onlyBottoms acc _ = acc
       draw _ (_ :<~ tiler) = (concatMap fst tiler, mapM_ snd tiler)
       hsvToRgb :: Double -> Double -> Double -> (Int, Int, Int)
       hsvToRgb h s v = let c = v * s
                            x = c * (1 - abs ((h / 60) `mod'` 2 - 1))
                            m = v - c
                            (r, g, b) = if
                               | h < 60 -> (c, x, 0)
                               | h < 120 -> (x, c, 0)
                               | h < 180 -> (0, c, x)
                               | h < 240 -> (0, x, c)
                               | h < 300 -> (x, 0, c)
                               | otherwise -> (c, 0, x)
                        in (round $ (r+m)*255, round $ (g+m)*255, round $ (b+m)*255)

          
-- |Writes the path to the topmost border.
writePath :: Members '[State (Fix Tiler), Input Borders, Colorer, Property] r 
          => Sem r ()
writePath = do
  (_, u, _, _) <- input @Borders
  root <- get @(Fix Tiler)
  drawText u $ cata getFocusList root

-- |Focus the window our Tilers are focusing
xFocus
  :: Members [State (Fix Tiler), Minimizer, Input Window] r
  => Sem r ()
xFocus = do
  root <- get @(Fix Tiler)
  rWin <- input @Window
  let w = fromMaybe (rWin, rWin) $ extract $ ana @(Beam _) makeList root
  restore $ fst w
  restore $ snd w
  setFocus $ snd w
 where
  makeList (Fix (Wrap (ChildParent w w')))              = EndF $ Just (w, w')
  -- TODO there's a lot of code duplication here between InputController and Monitor
  makeList (Fix (InputController _ (Just t))) = ContinueF t
  makeList (Fix (InputController _ Nothing)) = EndF Nothing
  makeList (Fix (Monitor _ (Just t))) = ContinueF t
  makeList (Fix (Monitor _ Nothing)) = EndF Nothing
  makeList (Fix t) = ContinueF (getFocused t)

-- |Push something if it can be pushed. Add it manually otherwise.
pushOrAdd :: Fix Tiler -> Maybe (Tiler (Fix Tiler)) -> Tiler (Fix Tiler)
pushOrAdd tWin = maybe (unfix tWin) (\case
  t@(Horiz _) -> add Back Focused tWin t
  t@(Floating _) -> add Back Focused tWin t
  t -> mkHoriz t
                                    )
    where mkHoriz t = Horiz $ makeFL (NE (Sized 0 $ Fix t) [Sized 0 tWin]) 0

-- |Get the current screen number based on pointer position.
getCurrentScreen :: Members (Inputs [Pointer, Screens]) r
                 => Sem r (Maybe (Int, Rect))
getCurrentScreen = do
  pointer <- input @Pointer
  screens <- input @Screens
  return $ whichScreen pointer $ zip [0..] screens

-- |Add a bunch of properties to our root window to comply with EWMH
initEwmh :: Member Property r
         => RootWindow -> Window -> Sem r ()
initEwmh root upper = do
  a    <- getAtom False "_NET_SUPPORTED"
  nswc <- getAtom False "_NET_SUPPORTING_WM_CHECK"
  xestName <- getAtom False "xest"
  c    <- getAtom False "ATOM"
  supp <- mapM
    (getAtom False)
    [ "_NET_NUMBER_OF_DESKTOPS"
    , "_NET_CURRENT_DESKTOP"
    , "_NET_CLIENT_LIST"
    , "_NET_ACTIVE_WINDOW"
    , "_NET_SUPPORTING_WM_CHECK"
    ]
  putProperty 32 a root c (fmap fromIntegral supp)
  putProperty 32 nswc root c [fromIntegral upper]
  putProperty 32 a upper c [fromIntegral xestName]


-- |Write workspaces in a EWMH compatible way
writeWorkspaces
  :: (Members '[Property, Input Window] r)
  => ([Text], Int)
  -> Sem r ()
writeWorkspaces (names, i) = do
  root <- input
  ndn <- getAtom False "_NET_DESKTOP_NAMES" 
  utf8 <- getAtom False "UTF8_STRING"
  cardinal <- getAtom False "CARDINAL"
  nnod <- getAtom False "_NET_NUMBER_OF_DESKTOPS"
  ncd <- getAtom False "_NET_CURRENT_DESKTOP"
  putProperty 8 ndn root utf8
    $ concatMap (fmap ord . unpack) names
  putProperty 32 nnod root cardinal [length names]
  putProperty 32 ncd root cardinal [i]

-- |Some windows (like Polybar) want to be on top of everything else
-- This function finds those windows and returns them in a list.
makeTopWindows
  :: (Members '[Property, GlobalX, Mover] r)
  => Sem r [Window]
makeTopWindows = do
  -- Get a list of all windows
  wins <- getTree
  higherWins <- for wins $ \win -> do
    -- EWMH defines how to do this.
    -- Check out their spec if you're curious.
    nws <- getAtom False "_NET_WM_STATE"
    prop <- getProperty @_ @Atom 32 nws win
    nwsa <- getAtom False "_NET_WM_STATE_ABOVE"
    return $ case prop of
      [] -> []
      states ->
        [win | nwsa `elem` states]
  return $ join higherWins

-- |Like makeTopWindows but the opposite.
getBottomWindows
  :: (Members '[Property, GlobalX, Mover] r)
  => Sem r [Window]
getBottomWindows = do
  -- Get a list of all windows
  wins <- getTree
  lowerWindows <- for wins $ \win -> do
    -- EWMH defines how to do this.
    -- Check out their spec if you're curious.
    wn <- getAtom False "WM_NAME"
    prop <- getProperty @_ @Word8 8 wn win
    return $ case prop of
      [] -> []
      states ->
        [win | (== "fakeWindowDontManage") $ fmap (chr . fromIntegral) states]
  return $ join lowerWindows

-- |Writes all of the clients we're managing for others to see.
setClientList :: (Members '[State (Fix Tiler), Input Window, Property] r)
              => Sem r ()
setClientList = do
  root <- input
  tilers <- get @(Fix Tiler)
  ncl <- getAtom False "_NET_CLIENT_LIST"
  warray <- getAtom False "WINDOW[]"
  putProperty 32 ncl root warray $ cata winList tilers
    where winList (Wrap (ChildParent _ w)) = [fromIntegral w]
          winList t = concat t

-- |Writes the active window to the root window.
writeActiveWindow :: (Members '[State (Fix Tiler), Input Window, Property] r)
              => Sem r ()
writeActiveWindow = do
  root <- input
  tilers <- get
  window <- getAtom False "WINDOW"
  naw <- getAtom False "_NET_ACTIVE_WINDOW"
  putProperty 32 naw root window [fromMaybe (fromIntegral root) . extract $ ana @(Beam _) makeList tilers]
    where makeList (Fix (Wrap (ChildParent _ w))) = EndF . Just $ fromIntegral w
          makeList (Fix (InputController _ (Just t))) = ContinueF t
          makeList (Fix (InputController _ Nothing)) = EndF Nothing
          makeList (Fix (Monitor _ (Just t))) = ContinueF t
          makeList (Fix (Monitor _ Nothing)) = EndF Nothing
          makeList (Fix t) = ContinueF (getFocused t)
