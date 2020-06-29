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
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Atom
import           Graphics.X11.Xinerama
import           Data.Either                    ( )
import           Tiler.Tiler
import           FocusList
import Control.Comonad.Trans.Cofree as C hiding (Cofree)
import qualified SDL (Window)

-- |Creates a new "frame" for the window manager. Note that this function isn't
-- called every literal frame; Xest doesn't have to do anything if it's
-- children want to change their contents. Xest only gets involved when they
-- need to move.
refresh :: Members [Mover, Property, Colorer, GlobalX, Log LogData, Minimizer, Unmanaged] m
        => Members (Inputs [Window, Screens, Pointer, [XineramaScreenInfo]]) m
        => Members (States [Tiler, Mode, [SubTiler], Maybe (), OldTime, Screens, DockState, Docks]) m
        => m ()
refresh = do
    log $ LD "Refreshing" "Has started"
    put @(Maybe ()) Nothing

    -- Fix the Monitor if the Input Controller moved in a weird way
    modify @Tiler fixMonitor

    -- Update the Monitors in case docks were created
    oldScreens <- gets @Screens $ map fst . itoList
    forM_ oldScreens $ \i -> do
      screenInfo <- input @[XineramaScreenInfo]
      case find ((== fromIntegral i) . xsi_screen_number) screenInfo of
        Just (XineramaScreenInfo _ x y w h) -> do
          newRect <- constrainRect $ Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
          modify @Screens $ over (at i) (map $ putScreens newRect)
        Nothing -> put @(Maybe ()) $ Just ()
    -- Write the path to the upper border
    writePath

    -- restack all of the windows
    Docks topWindows <- get @Docks
    log $ LD "TOP WINDOWS" $ show topWindows
    log $ LD "Rendering" "Has started"
    middleWins <- render
    restack $ topWindows ++ map getParent middleWins
    allBorders <- inputs @Screens $ map getBorders . toList
    forM_ allBorders \(a, b, c, d) -> bufferSwap a >> bufferSwap b >> bufferSwap c >> bufferSwap d
    log $ LD "Rendering" "Has finished"

    -- tell X to focus whatever we're focusing
    log $ LD "Focusing" "Has started"
    xFocus
    log $ LD "Focusing" "Has finished"


    -- Do some EWMH stuff
    setClientList
    writeActiveWindow
    get @Tiler >>= writeWorkspaces . fromMaybe (["Nothing"], 0) . onInput (map (getDesktopState . unfix))
    -- clearQueue
    log $ LD "Refreshing" "Has finished"


-- | Places a tiler somewhere on the screen without actually placing it. The
-- Cofree thing just means every node in our Tiler tree also contains
-- instructions for how it should be placed on the screen.
placeWindow
  :: Tiler
  -> Cofree TilerF (Transformation, Int)
placeWindow root =
  ana buildUp (StartingPoint mempty, 0, Fix root)

    where
      -- The meat of the placeWindow function.
      placeWindow' :: Transformation -> Int -> SubTiler -> (TilerF (Transformation, Int, SubTiler))
      placeWindow' trans depth = \case
        -- Wraps are super simple.
        Wrap win -> Wrap win

        -- Do some math to figure out how to place the various windows. The
        -- main problem is that each element of a Horiz is resizable using a
        -- percentage of the total space available to the Horiz. If you have a
        -- better way to represent that, I would happily change.
        Many mh mods -> flip Many mods
          case (mods, mh) of
            (Full, _) ->
              withFl' (map (\t -> (StartingPoint $ Rect 0 0 0 0, depth + 1, t)) mh) $ mapOne (Right Focused) (map (\(_, d, t) -> (trans, d, t)))
            (_, Horiz fl) ->
              let wrapTrans = if mods == Rotate then Spin trans else trans
                  placed lSize size t = Sized size (Slide (Rect lSize 0 size 1) wrapTrans, depth+1, t)

              in Horiz $ fromVis fl . snd . mapAccumL (\lSize (Sized s t) -> (lSize + s, placed lSize s t)) 0 $ vOrder fl

            (_, TwoCols colSize fl) ->
              let numWins = fromIntegral $ flLength fl - 1
                  wrapTrans = if mods == Rotate then Spin trans else trans
                  location i = Slide (Rect colSize (1.0 / numWins * i) (1-colSize) (1.0 / numWins)) wrapTrans
                  bigLoc = Slide (Rect 0 0 (colSize) 1) wrapTrans
                  allRight = fromVis fl $ map (\(i, Identity t) -> Identity (location i, depth+1, t)) $ mzip [-1 ..] (vOrder fl)

               in TwoCols colSize $ mapOne (Left Front) (\(Identity (_, d, t)) -> Identity (bigLoc, d, t)) allRight

            (_, Floating ls) ->
              let allFloating = flip map ls $ \case
                    WithRect rr@Rect {..} t ->
                      let wrapTrans = if mods == Rotate then Spin trans else trans
                          starting@(Rect realX realY realW realH) = bimap fromIntegral fromIntegral $ getStartingPoint wrapTrans
                      in WithRect rr (Slide (Rect ((x - realX) / realW) ((y - realY) / realH) (w / realW) (h / realH)) $ StartingPoint $ bimap floor ceiling starting, depth + 1, t)
                  withBottom = mapOne (Left Front) (\(WithRect r (_, d, t)) -> WithRect r (trans, d, t)) allFloating
               in Floating withBottom

        -- Input controllers don't really do anything.
        InputController bords t ->
          InputController bords $ map (trans, depth, ) t

        -- Monitors specify the starting point for the transformations that
        -- follow.
        Monitor screenSize t ->
          Monitor screenSize $ map (StartingPoint screenSize, depth, ) t

      -- Runs the above function after shuffling the types around.
      buildUp :: (Transformation, Int, SubTiler) -> CofreeF TilerF (Transformation, Int) (Transformation, Int, SubTiler)
      buildUp (trans, depth, t) = (trans, depth) C.:< placeWindow' trans depth t
  

-- |Find a window with a class name. This is used when
-- showing or hiding a window. TODO Can I just ask the user to use some random
-- bash utility instead?
getWindowByClass
  :: Members [Property, GlobalX] m
  => Text
  -> m [Window]
getWindowByClass wName = do
  childrenList <- getTree
  filterM findWindow' childrenList
  where findWindow' win = (== wName) <$> getClassName win

-- |Moves windows around based on where they are in the tiler.
render
  :: forall m.
     ( Members (Inputs [Pointer, Screens]) m
     , Members (States [Tiler, Mode, [SubTiler]]) m
     , Members [Mover, Colorer, GlobalX, Log LogData, Minimizer, Property] m
     )
  => m [ParentChild]
render = do
  screens <- input @Screens

  let locations :: [(Cofree TilerF (XRect, Int))] =
        toList $ map (map (first toScreenCoord) . placeWindow) screens

  -- Draw the tiler we've been given. winOrder will be used by restackWindows
  -- while io coantains the io action which moves the windows.
  let (winOrder, io) = unzip . toList $ map (cata draw) locations
  sequence_ io

  -- Hide all of the popped tilers
  minimized <- get @[SubTiler]
  traverse_ (snd . cata draw . map (first toScreenCoord) . placeWindow . unfix) minimized

  return $ join winOrder

       -- The main part of this function.
 where draw :: Base (Cofree TilerF (XRect, Int)) ([ParentChild], m ()) -> ([ParentChild], m ())
       draw (CofreeF (Rect {..}, _) (Wrap pc)) = ([pc],
           changeLocation pc $ Rect x y (abs w) (abs h))
       
       -- The InputController places the SDL window borders around the focused
       -- Tiler if you're in a mode that wants borders.
       draw (CofreeF (Rect{..}, depth) (InputController (l, u, r, d) t)) =
           (maybe [] fst t, do
              -- Do the movements requested by the children.
              mapM_ snd t

              let winList :: [SDL.Window] = [l, u, r, d]

              -- Calculate the color for our depth
              -- Basically, add the golden ratio to the hue depending on how
              -- deep we are.
              let hue = 360.0 * ((0.5 + (fromIntegral (depth - 1) * 0.618033988749895)) `mod'` 1)

              currentMode <- get @Mode
              if hasBorders currentMode
                then do
                    -- Draw them with the right color and position
                    changeLocationS l $ Rect x y 2 h
                    changeLocationS u $ Rect (x + 2) y (w-2) 20
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
           )

       -- Draw the background with the floating windows on top.
       draw (CofreeF _ (Many mh _)) =
         case mh of
           Floating fl ->
             let (bottom, tops) = pop (Left Front) $ map (fst . extract) fl
              in (join $ maybe [] (toList . fOrder) tops ++ [bottom], mapM_ (snd . extract) fl)
           _ ->
             (foldFl mh $ join . toList . fOrder . map (fst . extract), foldFl mh $ mapM_ (snd . extract))

       -- Everything else just needs to draw it's children
       draw (CofreeF _ tiler) = (concatMap fst tiler, mapM_ snd tiler)

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
writePath :: Members '[State Tiler, Colorer, Property] m
          => m ()
writePath = do
  (_, u, _, _) <- gets @Tiler getBorders
  root <- get @Tiler
  drawText u $ cata getFocusList $ Fix root

-- |Focus the window our Tilers are focusing
xFocus
  :: Members [State Tiler, Mover, Input Window, State OldTime] m
  => m ()
xFocus = do
  root <- get @Tiler
  rWin <- input @Window
  let w = fromMaybe (ParentChild rWin rWin) $ extract $ ana @(Beam _) makeList root
  setFocus w
 where
  makeList (Wrap pc)              = EndF $ Just pc
  makeList (InputControllerOrMonitor _ t) = maybe (EndF Nothing) (ContinueF . unfix) t
  makeList t = ContinueF (unfix $ getFocused t)

-- |Set the current screen number based on pointer position.
setScreenFromMouse :: Members [Input Pointer, State ActiveScreen, State Screens] m
                 => m ()
setScreenFromMouse = do
  pointer <- input @Pointer
  screens <- get @Screens
  put @ActiveScreen $ maybe 0 fst $ whichScreen pointer $ zip [0..] $ toList $ map getScreens screens

-- |Add a bunch of properties to our root window to comply with EWMH
initEwmh :: (Property m, Monad m)
         => RootWindow -> Window -> m ()
initEwmh root upper = do
  a    <- getAtom False "_NET_SUPPORTED"
  nswc <- getAtom False "_NET_SUPPORTING_WM_CHECK"
  nwname <- getAtom False "_NET_WM_NAME"
  utf8 <- getAtom False "UTF8_STRING"
  supp <- mapM
    (getAtom False)
    [ "_NET_NUMBER_OF_DESKTOPS"
    , "_NET_CURRENT_DESKTOP"
    , "_NET_CLIENT_LIST"
    , "_NET_ACTIVE_WINDOW"
    , "_NET_SUPPORTING_WM_CHECK"
    , "_NET_WM_STRUT_PARTIAL"
    , "_NET_WM_STATE"
    , "_NET_WM_STATE_FULLSCREEN"
    ]
  putProperty 32 a root aTOM (map fromIntegral supp)
  putProperty 32 nswc root wINDOW [fromIntegral upper]
  putProperty 32 nswc upper wINDOW [fromIntegral upper]
  putProperty 8 nwname upper utf8 $ map ord "xest"


-- |Write workspaces in a EWMH compatible way
writeWorkspaces
  :: Members '[Property, Input Window] m
  => ([Text], Int)
  -> m ()
writeWorkspaces (names, i) = do
  root <- input
  ndn <- getAtom False "_NET_DESKTOP_NAMES" 
  utf8 <- getAtom False "UTF8_STRING"
  nnod <- getAtom False "_NET_NUMBER_OF_DESKTOPS"
  ncd <- getAtom False "_NET_CURRENT_DESKTOP"
  putProperty 8 ndn root utf8
    $ concatMap (map ord . view _Text) names
  putProperty 32 nnod root cARDINAL [length names]
  putProperty 32 ncd root cARDINAL [i]


-- |Writes all of the clients we're managing for others to see.
setClientList :: Members '[State Tiler, Input Window, Property] m
              => m ()
setClientList = do
  root <- input
  tilers <- get @Tiler
  ncl <- getAtom False "_NET_CLIENT_LIST"
  putProperty 32 ncl root wINDOW $ cata winList $ Fix tilers
    where winList (Wrap (ParentChild _ w)) = [fromIntegral w]
          winList t = concat t

-- |Writes the active window to the root window.
writeActiveWindow :: Members '[State Tiler, Input Window, Property] m
              => m ()
writeActiveWindow = do
  root <- input
  tilers <- gets @Tiler Fix
  naw <- getAtom False "_NET_ACTIVE_WINDOW"
  putProperty 32 naw root wINDOW [fromMaybe (fromIntegral root) . extract $ ana @(Beam _) makeList tilers]
    where makeList (Fix (Wrap (ParentChild _ w))) = EndF . Just $ fromIntegral w
          makeList (Fix (InputControllerOrMonitor _ (Just t))) = ContinueF t
          makeList (Fix (InputControllerOrMonitor _ Nothing)) = EndF Nothing
          makeList (Fix t) = ContinueF (getFocused t)
