{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Polysemy.Reader
import           Data.Bits
import           Data.Kind
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Types
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
import qualified Data.Set                      as S
import           System.Process
import           Types


-- Super list
infixr 5 :::
data HList a where
    HNil :: HList '[]
    (:::) :: a -> HList (b :: [Type]) -> HList (a ': b)
-- x = HCons 5 (HCons "Test" HNil)

type family TMap (f :: a -> b) (xs :: [a]) where
    TMap _ '[]       = '[]
    TMap f (x ': xs) = f x ': TMap f xs

type family TConcat (a :: [t]) (b :: [t]) where
    TConcat '[] b = b
    TConcat (a ': as) b = a ': TConcat as b

type Readers (a :: [Type]) = TMap Reader a
-- toSem :: Members (TTail (TMap Reader t)) r => HList t -> Sem (Reader (THead t) ': r) a -> Sem r a
runReaders :: HList t -> Sem (TConcat (Readers t) r) a -> Sem r a
runReaders (a ::: as) = runReaders as . runReader a
runReaders HNil = id

type States (a :: [Type]) = TMap State a
runStates :: HList t -> Sem (TConcat (States t) r) a -> Sem r a
runStates (a ::: as) = fmap snd . runStates as . runState a
runStates HNil = id


data PropertyReader m a where
  GetProperty ::Storable a => Int -> String -> Window -> PropertyReader m (Maybe [a])
  IsSameAtom ::String -> Atom -> PropertyReader m Bool
makeSem ''PropertyReader

data PropertyWriter m a where
  SetProperty32 ::String -> String -> Window -> [Int] -> PropertyWriter m ()
  SetProperty8 ::String -> String -> Window -> [Int] -> PropertyWriter m ()
makeSem ''PropertyWriter

data AttributeWriter m a where
  SelectFlags ::Window -> Mask -> AttributeWriter m ()
  SetFocus ::Window -> AttributeWriter m ()
  CaptureButton ::Mode -> AttributeWriter m ()
  GetButton ::Window -> AttributeWriter m MouseButtons
makeSem ''AttributeWriter

data AttributeReader m a where
  GetClassName ::Window -> AttributeReader m String
makeSem ''AttributeReader

data WindowMover m a where
  ChangeLocation :: Window -> Rect -> WindowMover m ()
  Raise ::Window -> WindowMover m ()
  ConfigureWin ::Event -> WindowMover m ()
makeSem ''WindowMover

data WindowMinimizer m a where
  Minimize ::Window -> WindowMinimizer m ()
  Restore ::Window -> WindowMinimizer m ()
makeSem ''WindowMinimizer

data Executor m a where
  Execute ::String -> Executor m ()
makeSem ''Executor

data GlobalX m a where
   GetTree ::GlobalX m [Window]
   RebindKeys ::Mode -> GlobalX m ()
   GetXEvent ::GlobalX m Event
makeSem ''GlobalX

data Colorer m a where
  GetColor :: String -> Colorer m Color
  ChangeColor :: Window -> Color -> Colorer m ()
makeSem ''Colorer

type Interpret e r a = Members [Lift IO, Reader Display] r => Sem (e ': r) a -> Sem r a

runPropertyReader :: Interpret PropertyReader r a
runPropertyReader = interpret $ \case
  GetProperty size aName w -> ask >>= \d -> sendM @IO $ do
    atom <- liftIO $ internAtom d aName False
    liftIO $ rawGetWindowProperty size d atom w
  IsSameAtom aName atom -> ask >>= \d -> sendM @IO $ do
    otherAtom <- liftIO $ internAtom d aName False
    return $ atom == otherAtom

runPropertyWriter :: Interpret PropertyWriter r a
runPropertyWriter = interpret $ \case
  SetProperty32 aName mType w msg -> ask >>= \d -> sendM @IO $ do
    atom  <- internAtom d aName False
    aType <- internAtom d mType False
    void
      .  changeProperty32 d w atom aType propModeReplace
      $  fmap fromIntegral msg
      ++ [0]
  SetProperty8 aName mType w msg -> ask >>= \d -> sendM @IO $ do
    atom  <- liftIO $ internAtom d aName False
    aType <- liftIO $ internAtom d mType False
    liftIO
      $  changeProperty8 d w atom aType propModeReplace
      $  fmap fromIntegral msg
      ++ [0]
    return ()

runAttributeWriter
  :: (Members [Reader Window, Reader Conf] r)
  => Interpret AttributeWriter r a
runAttributeWriter = interpret $ \case
  SelectFlags w flags -> ask >>= \d -> sendM @IO $ do
    selectInput d w flags
    grabButton d anyButton anyModifier w False (buttonPressMask .|. buttonReleaseMask) grabModeSync grabModeAsync none none

  SetFocus w          -> ask @Display
    >>= \d -> sendM @IO $ do
      setInputFocus d w revertToNone currentTime
      allowEvents d (replayPointer) currentTime
  CaptureButton NewMode {modeName = name}  -> ask @Display >>= \d -> do
      ms <- asks definedModes
      root <- ask @Window
      when (not . null $ filter (\(NewMode m _ _ hasButtons) -> m == name && hasButtons) ms)
        $ void . sendM @IO $ grabPointer d root False pointerMotionMask grabModeAsync grabModeAsync root none currentTime
      when (null $ filter (\(NewMode m _ _ hasButtons) -> m == name && hasButtons) ms)
        $ void . sendM @IO $ ungrabPointer d currentTime
  GetButton w -> ask @Display >>= \d -> sendM @IO $ do 
    (_, _, _, _, _, _, _, b) <- queryPointer d w

    allowEvents d (syncPointer) currentTime
    return case b of
             _ | b .&. button1Mask /= 0-> LeftButton (0, 0)
               | b .&. button3Mask /= 0-> RightButton (0, 0)
               | otherwise -> None

runAttributeReader :: Interpret AttributeReader r a
runAttributeReader = interpret $ \case
  GetClassName win ->
    ask >>= \d -> sendM $ getClassHint d win >>= \(ClassHint _ n) -> return n

runWindowMover :: Interpret WindowMover r a
runWindowMover = interpret $ \case
  ChangeLocation win (Rect x y h w) -> do
    d <- ask
    void . sendM $ moveWindow d win x y >> resizeWindow d win h w
  Raise win -> do
    d <- ask
    sendM $ raiseWindow d win
    return ()
  ConfigureWin ConfigureRequestEvent {..} ->
    let wc = WindowChanges ev_x
                           ev_y
                           ev_width
                           ev_height
                           ev_border_width
                           ev_above
                           ev_detail
    in  ask >>= \d -> sendM @IO $ configureWindow d ev_window ev_value_mask wc
  ConfigureWin _ -> error "Don't call Configure Window with other events"

runWindowMinimizer
  :: Member (State (Set Window)) r => Interpret WindowMinimizer r a
runWindowMinimizer = interpret $ \case
  Minimize win -> do
    d <- ask
    WindowAttributes { wa_map_state = mapped } <- sendM
      $ getWindowAttributes d win
    when (mapped /= waIsUnmapped) $ do
      modify $ S.insert win
      sendM @IO $ unmapWindow d win
  Restore win -> do
    d <- ask
    WindowAttributes { wa_map_state = mapped } <- sendM
      $ getWindowAttributes d win
    when (mapped == waIsUnmapped) $ do
      modify $ S.delete win
      sendM @IO $ mapWindow d win
      sendM @IO $ setInputFocus d win revertToNone currentTime
runExecutor :: Member (Lift IO) r => Sem (Executor ': r) a -> Sem r a
runExecutor = interpret $ \case
  Execute s -> void . sendM $ spawnCommand s

runGlobalX
  :: (Members [Reader Window, Reader Conf] r)
  => Interpret GlobalX r a
runGlobalX = interpret $ \case
  GetTree -> do
    display <- ask @Display
    root    <- ask @Window
    sendM @IO $ alloca
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
  RebindKeys activeMode -> do
    Conf kb _ <- ask @Conf
    d         <- ask @Display
    win       <- ask @Window
    sendM $ forM_ kb $ toggleModel activeMode d win

   where
    toggleModel :: Mode -> Display -> Window -> KeyTrigger -> IO ()
    toggleModel m d win (k, km, _) = if m == km
      then grabKey d k anyModifier win False grabModeAsync grabModeAsync
      else ungrabKey d k anyModifier win
  GetXEvent ->
    ask >>= \d -> sendM @IO $
      untilM isConfNot $ allocaXEvent \p -> nextEvent d p >> getEvent p
   where isConfNot ConfigureEvent {} = False
         isConfNot _ = True

runColorer :: Interpret Colorer r a
runColorer = interpret $ \case
  GetColor color -> do
    display <- ask @Display
    let colorMap = defaultColormap display (defaultScreen display)
    sendM @IO $ fmap fst $ allocNamedColor display colorMap color
  ChangeColor w (Color pix _ _ _ _) -> do
    display <- ask @Display
    sendM $ setWindowBackground display w pix
    -- sendM $ sync display True

type DoAll r
  = (Members (States
        [ Tiler (Fix Tiler), Fix Tiler, KeyStatus, Mode, Set Window, [Fix Tiler]
        , MouseButtons
        ]) r
    , Members (Readers
        [ Conf, Window, (Dimension, Dimension), Borders ]) r
    , Members
        [ WindowMover, PropertyReader, PropertyWriter, AttributeReader
        , AttributeWriter , WindowMinimizer , Executor , GlobalX , Colorer
        ] r
    )
  => Sem r [Action]

doAll
  :: Tiler (Fix Tiler)
  -> Conf
  -> Mode
  -> (Dimension, Dimension)
  -> Display
  -> Window
  -> Borders
  -> _ -- The super long Sem list which GHC can figure out on its own
  -> IO ()
doAll t c m dims d w borders =
  void
    . runM
    . runStates (m ::: S.empty @Window ::: Default ::: t ::: [] ::: None ::: HNil)
    . fixState
    . runReaders (w ::: d ::: dims ::: borders ::: c ::: HNil)
    . runPropertyReader
    . runPropertyWriter
    . runAttributeReader
    . runAttributeWriter
    . runGlobalX
    . runWindowMinimizer
    . runWindowMover
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
