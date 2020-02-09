{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}


{-|
   Core contains important Xlib interfacing code that doesn't belong
   in any other file.
-}
module Core where

import           Standard
import Base.DoAll
import Actions.ActionTypes
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Atom
import           Data.Either                    ( )
import           Data.Char
import           Tiler.Tiler
import           FocusList
import Control.Comonad.Trans.Cofree as C hiding (Cofree)
import qualified SDL (Window)

-- |Creates a new "frame" for the window manager. Note that this function isn't
-- called every literal frame; Xest doesn't have to do anything if it's
-- children want to change their contents. Xest only gets involved when they
-- need to move.
refresh :: Members [Mover, Minimizer, Property, Colorer, GlobalX, Executor] r
        => Members (Inputs [Window, Screens, Borders, (Int32, Int32)]) r
        => Members (States [Tiler, Mode, [SubTiler], Maybe (), Time]) r
        => Sem r ()
refresh = do
    printMe "\n\nRefreshing\n"
    put @(Maybe ()) Nothing
    -- Fix the Monitor if the Input Controller moved in a weird way
    modify @Tiler fixMonitor

    -- Write the path to the upper border
    writePath

    -- restack all of the windows
    topWindows <- makeTopWindows
    bottomWindows <- getBottomWindows
    render >>= traverse_ (restack . \wins -> topWindows ++ bottomWindows ++ wins)

    -- tell X to focus whatever we're focusing
    xFocus


    -- Do some EWMH stuff
    setClientList
    writeActiveWindow
    get >>= writeWorkspaces . fromMaybe (["Nothing"], 0) . onInput (fmap (getDesktopState . unfix))
    clearQueue
    printMe "\n\nDone refreshing\n"


-- | Places a tiler somewhere on the screen without actually placing it. The
-- Cofree thing just means every node in our Tiler tree also contains
-- instructions for how it should be placed on the screen.
placeWindow
  :: Rect
  -> Tiler
  -> Cofree TilerF (Transformation, Int)
placeWindow screenSize root =
  ana buildUp (StartingPoint mempty, 0, Fix root)

    where
      -- The meat of the placeWindow function.
      placeWindow' :: Transformation -> Int -> SubTiler -> TilerF (Transformation, Int, SubTiler)
      placeWindow' trans depth = \case
        -- Wraps are super simple.
        Wrap win -> Wrap win

        -- Reflects are straightforward. Increase the depth by 1 and add a spin
        -- transformation. It's worth noting that Reflect is 100% the wrong
        -- word for what is actually happening. Unfortunately, Rotate is
        -- already taken by the Config stuff.
        Reflect t -> Reflect (Spin trans, depth + 1, t)

        -- A FocusFull looks in its child and extracts the focused element.
        -- Everything but the focused element is ignored. This gives you one
        -- way to implement workspaces.
        FocusFull (Fix t) ->
          modFocused ((trans, depth + 2,) . trd) $ fmap (StartingPoint mempty, depth + 1,) t

        -- Do some math to figure out how to place the various windows. The
        -- main problem is that each element of a Horiz is resizable using a
        -- percentage of the total space available to the Horiz. If you have a
        -- better way to represent that, I would happily change.
        Horiz fl ->
          let numWins = fromIntegral $ flLength fl
              location i lSize size = Slide (Rect (newX i lSize) 0 (1 / numWins + size) 1) trans
              newX i lSize = 1.0 / numWins * i + lSize
              realfl = fromVis fl . mapFold (\lSize (Sized modS t) -> (lSize + modS, (lSize, modS, t))) 0 $ vOrder fl

           in Horiz $ fromVis realfl $ map (\(index, (lSize, size, t)) -> Sized size (location index lSize size, depth + 1, t))
                    $ zip [0 ..]
                    $ vOrder realfl

        -- Floating windows sit on top of a background window. There are some
        -- problems here. TODO How can I make sure windows never get drawn off
        -- the screen?
        Floating ls ->
           Floating $ map (\case
                Top (rr@Rect {..}, t) ->
                  let starting@(Rect realX realY realW realH) = getStartingPoint trans
                   in Top (rr, (Slide (Rect ((x - realX) / realW) ((y - realY) / realH) (w / realW) (h / realH)) $ StartingPoint starting, depth + 1, t))
                Bottom t -> Bottom (trans, depth + 1, t)) ls

        -- Input controllers don't really do anything.
        InputController t ->
          InputController $ fmap (trans, depth, ) t

        -- Monitors specify the starting point for the transformations that
        -- follow.
        Monitor t ->
          Monitor $ fmap (StartingPoint screenSize, depth, ) t

      -- Runs the above function after shuffling the types around.
      buildUp :: (Transformation, Int, SubTiler) -> CofreeF TilerF (Transformation, Int) (Transformation, Int, SubTiler)
      buildUp (trans, depth, t) = (trans, depth) C.:< placeWindow' trans depth t
  

-- |Find a window with a class name. This is used when
-- showing or hiding a window. TODO Can I just ask the user to use some random
-- bash utility instead?
getWindowByClass
  :: Members [Property, GlobalX] r
  => String
  -> Sem r [Window]
getWindowByClass wName = do
  childrenList <- getTree
  filterM findWindow childrenList
  where findWindow win = (== wName) <$> getClassName win

-- |Moves windows around based on where they are in the tiler.
render
  :: ( Members (Inputs [Pointer, Screens]) r
     , Members (States [Tiler, Mode, [SubTiler]]) r
     , Members [Mover, Minimizer, Colorer, GlobalX] r
     )
  => Sem r [[Window]]
render = do
  screens <- input @Screens

  let locations :: [(Cofree TilerF (Transformation, Int), Borders)] = toList $ fmap (\(Screen' rect t b) -> (placeWindow (bimap fromIntegral fromIntegral rect) t, b)) screens

  -- Draw the tiler we've been given. winOrder will be used by restackWindows
  -- while io coantains the io action which moves the windows.
  let (winOrder, io) = unzip . toList $ fmap (\(location, border) -> cata (draw $ Just border) $ fmap (first $ bimap round round . toScreenCoord) location) locations
  sequence_ io

  -- Hide all of the popped tilers
  minimized <- get @[SubTiler]
  traverse_ (snd . cata (draw Nothing) . fmap (first $ bimap round round . toScreenCoord) . placeWindow mempty . unfix) minimized

  return winOrder

       -- The main part of this function.
 where draw :: Maybe Borders -> Base (Cofree TilerF (XRect, Int)) ([Window], Sem _ ()) -> ([Window], Sem _ ())
       -- Windows without any height or width become invisible.
       draw _ ((Rect _ _ _ 0, _) :<~ Wrap (ParentChild win _)) = ([], minimize win)
       draw _ ((Rect _ _ 0 _, _) :<~ Wrap (ParentChild win _)) = ([], minimize win)

       -- Otherwise, the windows get moved and unminimized.
       draw _ ((Rect {..}, _) :<~ Wrap (ParentChild win win')) = ([win], do
           restore win
           restore win'
           changeLocation win $ Rect x y (abs w) (abs h)
           changeLocation win' $ Rect 0 0 (abs w) (abs h))
       
       -- The InputController places the SDL window borders around the focused
       -- Tiler if you're in a mode that wants borders.
       draw maybeBorders ((Rect{..}, depth) :<~ InputController t) =
           (maybe [] fst t, do
              -- Do the movements requested by the children.
              mapM_ snd t

              -- Extract the border windows
              flip (maybe $ return ()) maybeBorders $ \(l, u, r, d) -> do
                let winList :: [SDL.Window] = [l, u, r, d]

                -- Calculate the color for our depth
                -- Basically, add the golden ratio to the hue depending on how
                -- deep we are.
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
                      gets @Tiler Fix >>= drawText u . cata getFocusList
                  else do
                      changeLocationS l $ Rect 10000 0 0 0
                      changeLocationS u $ Rect 10000 0 0 0
                      changeLocationS d $ Rect 10000 0 0 0
                      changeLocationS r $ Rect 10000 0 0 0
                      traverse_ (`changeColor` hsvToRgb hue 0.5 0.9) winList
                      gets @Tiler Fix >>= drawText u . cata getFocusList
                      
                traverse_ bufferSwap winList
           )

       -- Draw the background with the floating windows on top.
       draw _ (_ :<~ Floating ls) = 
          (tops ++ bottoms, mapM_ (snd . getEither) ls)
              where tops = foldl' onlyTops [] ls
                    onlyTops acc (Top (_, (ws, _))) = ws ++ acc
                    onlyTops acc _ = acc
                    bottoms = foldl' onlyBottoms [] ls
                    onlyBottoms acc (Bottom (ws, _)) = acc ++ ws
                    onlyBottoms acc _ = acc

       -- Everything else just needs to draw it's children
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
writePath :: Members '[State Tiler, Input Borders, Colorer, Property] r 
          => Sem r ()
writePath = do
  (_, u, _, _) <- input @Borders
  root <- get @Tiler
  drawText u $ cata getFocusList $ Fix root

-- |Focus the window our Tilers are focusing
xFocus
  :: Members [State Tiler, Minimizer, Input Window, State Time] r
  => Sem r ()
xFocus = do
  root <- get @Tiler
  rWin <- input @Window
  let w = fromMaybe (rWin, rWin) $ extract $ ana @(Beam _) makeList root
  restore $ fst w
  restore $ snd w
  setFocus $ snd w
 where
  makeList (Wrap (ParentChild w w'))              = EndF $ Just (w, w')
  makeList (InputControllerOrMonitor _ (Just (Fix t))) = ContinueF t
  makeList (InputControllerOrMonitor _ Nothing) = EndF Nothing
  makeList t = ContinueF (unfix $ getFocused t)

-- |Set the current screen number based on pointer position.
setScreenFromMouse :: Members [Input Pointer, State ActiveScreen, State Screens] r
                 => Sem r ()
setScreenFromMouse = do
  pointer <- input @Pointer
  screens <- get @Screens
  put @ActiveScreen $ maybe 0 fst $ whichScreen pointer $ zip [0..] $ toList $ fmap screenSize screens

-- |Add a bunch of properties to our root window to comply with EWMH
initEwmh :: Member Property r
         => RootWindow -> Window -> Sem r ()
initEwmh root upper = do
  a    <- getAtom False "_NET_SUPPORTED"
  nswc <- getAtom False "_NET_SUPPORTING_WM_CHECK"
  nwname <- getAtom False "_NET_WM_NAME"
  utf8 <- getAtom False "UTF8_STRING"
  xestName <- getAtom False "xest"
  supp <- mapM
    (getAtom False)
    [ "_NET_NUMBER_OF_DESKTOPS"
    , "_NET_CURRENT_DESKTOP"
    , "_NET_CLIENT_LIST"
    , "_NET_ACTIVE_WINDOW"
    , "_NET_SUPPORTING_WM_CHECK"
    , "_NET_WM_STATE"
    , "_NET_WM_STATE_FULLSCREEN"
    ]
  putProperty 32 a root aTOM (fmap fromIntegral supp)
  putProperty 32 nswc root wINDOW [fromIntegral upper]
  putProperty 32 nswc upper wINDOW [fromIntegral upper]
  putProperty 8 nwname upper utf8 $ fmap ord "xest"


-- |Write workspaces in a EWMH compatible way
writeWorkspaces
  :: (Members '[Property, Input Window] r)
  => ([Text], Int)
  -> Sem r ()
writeWorkspaces (names, i) = do
  root <- input
  ndn <- getAtom False "_NET_DESKTOP_NAMES" 
  utf8 <- getAtom False "UTF8_STRING"
  nnod <- getAtom False "_NET_NUMBER_OF_DESKTOPS"
  ncd <- getAtom False "_NET_CURRENT_DESKTOP"
  putProperty 8 ndn root utf8
    $ concatMap (fmap ord . unpack) names
  putProperty 32 nnod root cARDINAL [length names]
  putProperty 32 ncd root cARDINAL [i]

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
    prop <- getProperty @_ @Word8 8 wM_NAME win
    return $ case prop of
      [] -> []
      states ->
        [win | (== "fakeWindowDontManage") $ fmap (chr . fromIntegral) states]
  return $ join lowerWindows

-- |Writes all of the clients we're managing for others to see.
setClientList :: (Members '[State Tiler, Input Window, Property] r)
              => Sem r ()
setClientList = do
  root <- input
  tilers <- get @Tiler
  ncl <- getAtom False "_NET_CLIENT_LIST"
  putProperty 32 ncl root wINDOW $ cata winList $ Fix tilers
    where winList (Wrap (ParentChild _ w)) = [fromIntegral w]
          winList t = concat t

-- |Writes the active window to the root window.
writeActiveWindow :: (Members '[State Tiler, Input Window, Property] r)
              => Sem r ()
writeActiveWindow = do
  root <- input
  tilers <- gets Fix
  naw <- getAtom False "_NET_ACTIVE_WINDOW"
  putProperty 32 naw root wINDOW [fromMaybe (fromIntegral root) . extract $ ana @(Beam _) makeList tilers]
    where makeList (Fix (Wrap (ParentChild _ w))) = EndF . Just $ fromIntegral w
          makeList (Fix (InputControllerOrMonitor _ (Just t))) = ContinueF t
          makeList (Fix (InputControllerOrMonitor _ Nothing)) = EndF Nothing
          makeList (Fix t) = ContinueF (getFocused t)
