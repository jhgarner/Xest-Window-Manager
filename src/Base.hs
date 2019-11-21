{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

{-|
   Base contains all of the IO facing code in project (except for the main
   method). We use Polysemy for this. Each possible action is wrapped in a GADT
   and an interpretter is defined down below. Ideally, this will be made
   testable in the near future.
-}
module Base where

import           Standard
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Polysemy.Several
import           Data.Bits
import           Data.Kind
import           System.IO
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xinerama
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Window
import           Graphics.X11.Xlib.Atom
import           Graphics.X11.Xlib.Color
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           System.Exit
import qualified Data.Set                      as S
import qualified Data.Map                      as M
import           System.Process
import           Types
import qualified SDL
import qualified SDL.Font as Font


-- * Helper Polysemy functions
-- $RunningSeveral
--
-- See "Polysemy.RunSeveral" for some more info about what's going on here.
--
-- If you're writing code which needs to operate on multiple inputs or
-- states, you should probably use these.

-- |A type level function which takes a list of Types and turns them into
-- inputs.
type Inputs (a :: [Type]) = TypeMap Input a
runInputs :: HList t -> Sem (TypeConcat (Inputs t) r) a -> Sem r a
-- |Runs an HList of values as if they were values for a Reader.
runInputs = runSeveral runInputConst

-- |Same as above but for State.
type States (a :: [Type]) = TypeMap State a
-- |Same as above but for State. Throws away the state that would be returned
runStates :: HList t -> Sem (TypeConcat (States t) r) a -> Sem r a
runStates = runSeveral evalState

-- |Pretty much everything needs effects when being run. This type alias makes
-- that easiear to type.
type Interpret e r a = Members [Embed IO, Input Display] r => Sem (e ': r) a -> Sem r a

inputs :: Member (Input i) r => (i -> a) -> Sem r a
inputs f = f <$> input

-- * Effects
--
-- $WhyEffects
--
-- Xest makes use of the "Polysemy" package. Polysemy uses a ton of fairly
-- advanced Haskell features so it can sometimes look a little scary. Why
-- then do we use it?
--
--   * Effect systems like Polysemy make it easier to apply the principle
--   of least privilage to the code. For example, we can have a function
--   which can resize windows but would fail to compile if they tried to
--   change any other properties on the window.
--
--   * Effect systems make it easy to swap out the implementation details
--   without having to touch the code that uses the effects.
--
--   * Polysemy will have no performance cost once GHC 8.10 is released.

-- * Properties

-- |X11 defines properties as a dictionary like object from Atoms and windows
-- to bits. This effect allows you to read or write to those dictionaries.
--
-- It's worth noting that these properties are very untyped so be careful
-- with what you pass in and try to get out.
data Property m a where
  -- |Gets a property from a window's dictionary
  GetProperty :: Storable a
              => Int -- ^ The number of bits in the property
              -> Atom  -- ^ The property key
              -> Window -- ^ The window who's properties we want to look at
              -> Property m [a] -- ^ A list of values found at the key

  -- |Writes to a property in a window's dictionary
  PutProperty :: Int -- ^ The number of bits in the property
              -> Atom -- ^ The property key we want to write to
              -> Window  -- ^ The window who's properties we want to look at
              -> Atom -- ^ A string representing the type of the data
              -> [Int] -- The data we want to write
              -> Property m ()

  -- |Although most properties are only loosely defined, the class property
  -- is built into Xlib for some reason.
  GetClassName :: Window -- ^ The Window we're interested in
               -> Property m String -- ^ The window's class

  -- |Technically not a property but it kind of fits...
  GetChild :: Window -- ^ The parent
           -> Property m (Maybe Window) -- ^ The child

  -- |Turns a String into an X11 managed Atom. Think of atoms as pointers to
  -- strings.
  GetAtom :: Bool -- ^ Should we not create it if it doesn't exist?
          -> String -- ^ The atom's name
          -> Property m Atom -- ^ The atom created/retrieved from X11
makeSem ''Property

type AtomCache = Map String Atom
type RootPropCache = Map Atom [Int]

-- |Runs a propertiy using IO
runProperty :: Members (States [AtomCache, RootPropCache]) r
            => Member (Input RootWindow) r
            => Interpret Property r a
runProperty = interpret $ \case
  GetProperty i atom win -> input >>= \d -> embed @IO $
    fromMaybe [] <$> rawGetWindowProperty i d atom win

  PutProperty i atomContent w atomType msg -> do
    d <- input
    rootWin <- input
    rootPropCache <- get @RootPropCache
    unless (rootWin == w && (== Just msg) (rootPropCache M.!? atomContent)) $ do
      let longMsg = fmap fromIntegral msg
          charMsg = fmap fromIntegral msg
      embed @IO $ case i of
        8 -> changeProperty8 d w atomContent atomType propModeReplace $ charMsg ++ [0]
        32 -> changeProperty32 d w atomContent atomType propModeReplace $ longMsg ++ [0]
        _ -> error "Can only write 32 and 8 bit values to properties right now"
    when (rootWin == w) $
      modify @RootPropCache (M.insert atomContent msg)
      

  GetClassName win ->
    input >>= \d -> embed $ getClassHint d win >>= \(ClassHint _ n) -> return n

  GetChild win -> do
    display <- input @Display
    -- For some reason, xQueryTree hasn't been abstracted at all
    embed @IO $ alloca
      (\numChildrenPtr -> alloca
        (\childrenListPtr -> do
          uselessPtr <- alloca $ \x -> return x
          _          <- xQueryTree display
                                   win
                                   uselessPtr
                                   uselessPtr
                                   childrenListPtr
                                   numChildrenPtr
          numChildren <- peek numChildrenPtr
          headMay <$> (peek childrenListPtr >>= peekArray (fromIntegral numChildren))
        )
      )

  GetAtom shouldCreate name -> input >>= \d -> do
    maybeAtom <- (M.!? name) <$> get
    case maybeAtom of
      Nothing -> do
        atom <- embed @IO $ internAtom d name shouldCreate
        modify $ M.insert name atom
        return atom
      Just atom -> return atom

-- * Event Flags

-- |Controls the various event flags we can set on windows. See the Xlib docs for
-- a description of what those are.
data EventFlags m a where
  -- |Directly ask for flags on a window
  SelectFlags :: Window -- ^ The Window to set these flags on
              -> Mask -- ^ The flags (represented as a bitmask) to grab
              -> EventFlags m ()

  -- |Grab all mouse events on the root window
  SelectButtons :: Mode -- ^ The mode we want to bind buttons for
                -> EventFlags m ()

  -- |Grab all of the key evetns on the root window
  RebindKeys :: Mode -- ^ The mode we want to unbind keys for
             -> Mode -- ^ The mode we want to bind keys for
             -> EventFlags m ()
makeSem ''EventFlags

-- |Runs the event using IO
runEventFlags
  :: Members (Inputs [RootWindow, Conf]) r
  => Interpret EventFlags r a
runEventFlags = interpret $ \case
  SelectFlags w flags -> input >>= \d -> embed @IO $
    selectInput d w flags
    -- This is just grabs button presses on a window
    -- TODO why am I doing this here?
    -- grabButton d anyButton anyModifier w False 
    --            (buttonPressMask .|. buttonReleaseMask) 
    --            grabModeAsync grabModeAsync none none

  SelectButtons m@NewMode {modeName = name, hasButtons = hb}  -> do
      d <- input @Display
      ms <- inputs definedModes
      root <- input @RootWindow
      -- If the current mode wants to listen to the mouse, let it.
      -- Otherwise, don't because capturing the mouse prevents everyone
      -- else from using it.
      void . embed @IO $
        if not hb
           then ungrabPointer d currentTime
           else void $ grabPointer d root False pointerMotionMask grabModeAsync
                                   grabModeAsync root none currentTime

  RebindKeys oldMode activeMode -> do
    Conf kb _ <- input @Conf
    d         <- input @Display
    win       <- input @RootWindow

    -- Unbind the old keys
    embed $ forM_ kb $
      \(k, km, _) -> when (oldMode == km)
        (ungrabKey d k anyModifier win)

    -- bind the new ones
    embed $ forM_ kb $
      \(k, km, _) -> when (activeMode == km)
        (grabKey d k anyModifier win True grabModeAsync grabModeAsync)


-- * Mover

-- |Anything that changes where a window sits goes in here.
data Mover m a where
  ChangeLocation :: Window -> Rect -> Mover m ()
  -- |Like the above but asks on SDL windows
  ChangeLocationS :: SDL.Window -> Rect -> Mover m ()
  Restack :: [Window] -> Mover m ()
  -- |A super light wrapper around actually configuring stuff
  ConfigureWin :: Event -> Mover m ()
makeSem ''Mover

type LocCache = Map Window Rect
type SDLLocCache = Map SDL.Window Rect
type WindowStack = [Window]
-- Move windows using IO
runMover :: Members (States [LocCache, SDLLocCache, WindowStack]) r
         => Interpret Mover r a
runMover = interpret $ \case
  ChangeLocation win r@(Rect x y h w) -> do
    unlessM ((== Just r) . (M.!? win) <$> get) $ do
      d <- input
      void . embed $ moveWindow d win x y >> resizeWindow d win h w
    modify $ M.insert win r

  ChangeLocationS win r@(Rect x y h w) -> do
    -- Avoid moving the SDL windows more than we need to
    unlessM ((== Just r) . (M.!? win) <$> get) $ do
      embed @IO $ SDL.setWindowPosition win $ SDL.Absolute $ SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))
      SDL.windowSize win SDL.$= SDL.V2 (fromIntegral h) (fromIntegral w)
    modify $ M.insert win r

  Restack wins -> do
    unlessM ((== wins) <$> get) $ do
      d <- input
      void . embed $ restackWindows d wins
    put wins


  ConfigureWin ConfigureRequestEvent {..} ->
    let wc = WindowChanges ev_x
                           ev_y
                           ev_width
                           ev_height
                           ev_border_width
                           ev_above
                           ev_detail
    in  input >>= \d -> embed @IO $ configureWindow d ev_window ev_value_mask wc
  ConfigureWin _ -> error "Don't call Configure Window with other events"

-- * Minimizer

-- |Anything that changes a state but doesn't actually move it goes here
data Minimizer m a where
  Minimize :: Window -> Minimizer m ()
  Restore :: Window -> Minimizer m ()
  SetFocus :: Window -> Minimizer m ()
makeSem ''Minimizer

newtype FocusedCache = FocusedCache Window
  deriving Eq
-- |Do the actions in IO
runMinimizer
  :: Members (States [Set Window, FocusedCache]) r => Interpret Minimizer r a
runMinimizer = interpret $ \case
  Minimize win -> do
    d <- input
    -- We only want to minimize if it hasn't already been minimized
    WindowAttributes { wa_map_state = mapped } <- embed
      $ getWindowAttributes d win
    when (mapped /= waIsUnmapped) $ do
      modify $ S.insert win
      embed @IO $ unmapWindow d win

  Restore win -> do
    d <- input
    -- Only restore if it needs to be restored
    WindowAttributes { wa_map_state = mapped } <- embed
      $ getWindowAttributes d win
    when (mapped == waIsUnmapped) $ do
      modify $ S.delete win
      embed @IO $ mapWindow d win

  SetFocus w -> input @Display >>= \d -> do
    unlessM ((== FocusedCache w) <$> get @FocusedCache) $ embed @IO $ do
      setInputFocus d w revertToNone currentTime
      allowEvents d replayPointer currentTime
    put $ FocusedCache w

-- * Executor

-- |Actions that modify the world outside of X11 go here
data Executor m a where
  -- |Run a command
  Execute :: String -> Executor m ()
  -- |Toggle logging
  ToggleLogs :: Executor m ()
  -- |Prints in a controlled way
  PrintMe :: String -> Executor m ()
makeSem ''Executor

-- |Do it in IO
runExecutor :: Members [Embed IO, State Bool] r => Sem (Executor ': r) a -> Sem r a
runExecutor = interpret $ \case
  Execute s -> void . embed @IO $ spawnCommand s

  ToggleLogs -> do
    unlessM (get @Bool) $
      -- Empty the last run of logging
      embed @IO $ Standard.writeFile "/tmp/xest.log" ""
    modify not

  -- PrintMe s -> say $ pack s
  PrintMe s -> whenM (get @Bool) $ embed @IO $ appendFile "/tmp/xest.log" s

-- * Colorer

-- |Handle any color stuff
data Colorer m a where
  GetColor :: String -> Colorer m Color
  ChangeColor :: SDL.Window -> (Int, Int, Int) -> Colorer m ()
  DrawText :: SDL.Window -> String -> Colorer m ()
  BufferSwap :: SDL.Window -> Colorer m ()
makeSem ''Colorer

-- |More IO
runColorer :: Member (State (Maybe Font.Font)) r => Interpret Colorer r a
runColorer = interpret $ \case
  GetColor color -> do
    display <- input @Display
    let colorMap = defaultColormap display (defaultScreen display)
    embed @IO $ fst <$> allocNamedColor display colorMap color
  ChangeColor w (h, s, v) -> do
    winSurface <- embed @IO $ SDL.getWindowSurface w
    embed @IO $ SDL.surfaceFillRect winSurface Nothing $ SDL.V4 (fromIntegral h) (fromIntegral s) (fromIntegral v) 0
  DrawText w s -> do
    whenM (null <$> get @(Maybe Font.Font)) $ do
      font <- embed @IO $ Font.load "/usr/share/fonts/adobe-source-code-pro/SourceCodePro-Medium.otf" 10
      put $ Just font
    mfont <- get @(Maybe Font.Font)
    let Just font = mfont
    surface <- embed @IO $ Font.blended font (SDL.V4 0 0 0 0) $ pack s
    winSurface <- embed @IO $ SDL.getWindowSurface w
    void . embed @IO $ SDL.surfaceBlit surface Nothing winSurface Nothing
  BufferSwap w -> embed @IO $ SDL.updateWindowSurface w

-- * Global X

-- |Anything that doesn't fit somewhere else. This should probably be
-- split up at some point.
data GlobalX m a where
   GetTree :: GlobalX m [Window]
   NewWindow :: Window -> GlobalX m Window
   MoveToRoot :: Window -> GlobalX m ()
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
-- TODO why does capturing either of these break everything?
eventFilter _ ButtonEvent {ev_button=button} = button `notElem` [button5, button4]
eventFilter _ CrossingEvent {ev_detail=detail} = detail /= 2
eventFilter _ _ = True

-- |IO
runGlobalX
  :: (Members [Input RootWindow, Input Conf, State Bool, State (Fix Tiler)] r)
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
    System.IO.print $ show supportedProtocols
    System.IO.print $ show protocols
    System.IO.print $ show deleteName
    System.IO.print $ "\nTesting\n===========\n"
    trace ( show supportedProtocols) return ()
    trace ( show protocols) return ()
    trace ( show deleteName) return ()
    trace ( "\nTesting\n===========\n") return ()
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

-- * Fake Inputs

-- $Fake
--
-- The following provide an Input-like interface. Because of that,
-- we're just going to pretend like they are inputs from the code's
-- point of view.

-- |Gets the current button presses from the Xserver
runGetButtons :: Members (Inputs [RootWindow, Display]) r 
              => Member (Embed IO) r
              => Sem (Input MouseButtons ': r) a -> Sem r a
runGetButtons = runInputSem $ do
  d <- input @Display
  w <- input @RootWindow
  embed @IO $ do 
    (_, _, _, _, _, _, _, b) <- queryPointer d w

    allowEvents d asyncPointer currentTime
    return $ case b of
            _ | b .&. button1Mask /= 0-> LeftButton (0, 0)
              | b .&. button3Mask /= 0-> RightButton (0, 0)
              | otherwise -> None


-- |Get the screens from Xinerama
runGetScreens :: Members (Inputs [RootWindow, Display]) r 
              => Member (Embed IO) r
              => Sem (Input [Rect] ': r) a -> Sem r a
runGetScreens = runInputSem $ 
  input >>= \d -> embed @IO $
    fmap (\(Rectangle x y w h) -> Rect {..}) <$> getScreenInfo d

-- |Gets the pointer location
runGetPointer :: Members (Inputs [RootWindow, Display]) r 
              => Member (Embed IO) r
              => Sem (Input (Int32, Int32) ': r) a -> Sem r a
runGetPointer = runInputSem $ input >>= \d -> do
  root <- input @RootWindow
  (_, _, _, x, y, _, _, _) <- embed $ queryPointer d root
  return (fromIntegral x, fromIntegral y)

-- There's a lot of effects here. This type has them all!
type DoAll r
  = (Members (States
        [ Tiler (Fix Tiler), Fix Tiler, KeyStatus, Mode, Set Window, [Fix Tiler]
        , MouseButtons, Maybe Font.Font, Bool, Map Window Rect, Maybe ()
        ]) r
    , Members (Inputs
        [ Conf, Window, Borders, Display, [Rect], (Int32, Int32), MouseButtons ]) r
    , Members
        [ Mover, Property, Minimizer , Executor , GlobalX , Colorer, EventFlags, Embed IO ] r
    )
  => Sem r ()

-- Want to do everything in IO? Use this!
doAll
  :: Tiler (Fix Tiler)
  -> Conf
  -> Mode
  -> Display
  -> Window
  -> Borders
  -> _ -- The super long Sem list which GHC can figure out on its own
  -> IO ()
doAll t c m d w borders =
  void
    . runM
    . runStates (m ::: S.empty @RootWindow ::: Default ::: t ::: [] @(Fix Tiler) ::: None ::: Nothing ::: False ::: M.empty @String ::: M.empty @Atom @[Int] ::: FocusedCache 0 ::: M.empty @SDL.Window ::: M.empty @Window @Rect ::: [] @Window ::: Just () ::: HNil)
    . fixState
    . runInputs (w ::: d ::: borders ::: c ::: HNil)
    . runGetScreens
    . runGetButtons
    . runGetPointer
    . runProperty
    . runEventFlags
    . runGlobalX
    . runMinimizer
    . runMover
    . runExecutor
    . runColorer
 where
  -- TODO Is this bad? It allows us to refer to the root tiler as either fix or unfixed.
  fixState
    :: Member (State (Tiler (Fix Tiler))) r
    => Sem (State (Fix Tiler) ': r) a
    -> Sem r a
  fixState = interpret $ \case
    Get         -> Fix <$> get
    Put (Fix s) -> put s


instance Semigroup a => Semigroup (Sem r a) where
  (<>) = liftA2 (<>)
instance Monoid a => Monoid (Sem r a) where
  mempty = pure mempty
