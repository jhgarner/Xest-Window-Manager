{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}

{-|
   Base contains all of the IO facing code in project (except for the main
   method). We use Polysemy for this. Each possible action is wrapped in a GADT
   and an interpretter is defined down below. Ideally, this will be made
   testable in the near future.
-}
module Base where

import           ClassyPrelude           hiding ( ask
                                                , Reader
                                                )
import           Polysemy
import           Polysemy.State
import           Polysemy.Reader
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Window
import           Graphics.X11.Xlib.Atom
import           Graphics.X11.Xlib.Color
import           Graphics.X11.Xlib.Screen
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           Data.Functor.Foldable
import qualified Data.Set                      as S
import           System.Process
import           Types                          ( Action
                                                , Mode
                                                , KeyTrigger
                                                , KeyStatus(Default)
                                                , Tiler
                                                , Conf(Conf)
                                                , Rect(Rect)
                                                )

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

type DIO r = (Member (Lift IO) r, Member (Reader Display) r)

runPropertyReader :: DIO r => Sem (PropertyReader ': r) a -> Sem r a
runPropertyReader = interpret $ \case
  GetProperty size aName w -> ask >>= \d -> sendM @IO $ do
    atom <- liftIO $ internAtom d aName False
    liftIO $ rawGetWindowProperty size d atom w
  IsSameAtom aName atom -> ask >>= \d -> sendM @IO $ do
    otherAtom <- liftIO $ internAtom d aName False
    return $ atom == otherAtom

runPropertyWriter :: DIO r => Sem (PropertyWriter ': r) a -> Sem r a
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

runAttributeWriter :: DIO r => Sem (AttributeWriter ': r) a -> Sem r a
runAttributeWriter = interpret $ \case
  SelectFlags w flags -> ask >>= \d -> sendM @IO $ selectInput d w flags
  SetFocus w          -> ask @Display
    >>= \d -> sendM @IO $ setInputFocus d w revertToNone currentTime

runAttributeReader :: DIO r => Sem (AttributeReader ': r) a -> Sem r a
runAttributeReader = interpret $ \case
  GetClassName win ->
    ask >>= \d -> sendM $ getClassHint d win >>= \(ClassHint _ n) -> return n

runWindowMover :: DIO r => Sem (WindowMover ': r) a -> Sem r a
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
  :: (DIO r, Member (State (Set Window)) r)
  => Sem (WindowMinimizer ': r) a
  -> Sem r a
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

runExecutor :: Member (Lift IO) r => Sem (Executor ': r) a -> Sem r a
runExecutor = interpret $ \case
  Execute s -> void . sendM $ spawnCommand s

runGlobalX
  :: (DIO r, Member (Reader Window) r, Member (Reader Conf) r)
  => Sem (GlobalX ': r) a
  -> Sem r a
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
    ask >>= \d -> sendM $ allocaXEvent $ \p -> nextEvent d p >> getEvent p

runColorer :: DIO r => Sem (Colorer ': r) a -> Sem r a
runColorer = interpret $ \case
  GetColor color -> do
    display <- ask @Display
    let colorMap = defaultColormap display (defaultScreen display)
    sendM @IO $ fmap fst $ allocNamedColor display colorMap color
  ChangeColor w (Color pix _ _ _ _) -> do
    display <- ask @Display
    sendM $ setWindowBackground display w pix
    -- sendM $ sync display True

-- TODO make this less incredibly verbose...
type DoAll r
  = Members
 '[ WindowMover
  , State (Tiler (Fix Tiler))
  , State (Fix Tiler)
  , State KeyStatus
  , State Mode
  , State (Set Window)
  , Reader Conf
  , Reader Window
  , Reader (Dimension, Dimension)
  , Reader (Window, Window, Window, Window)
  , PropertyReader
  , PropertyWriter
  , AttributeReader
  , AttributeWriter
  , WindowMinimizer
  , Executor
  , GlobalX
  , Colorer
  ] r
  => Sem r [Action]

-- TODO same as above. Ideally I wouldn't need to write runState a thousand times as well
doAll
  :: Tiler (Fix Tiler)
  -> Conf
  -> Mode
  -> (Dimension, Dimension)
  -> Display
  -> Window
  -> (Window, Window, Window, Window)
  -> Sem
       '[Colorer, Executor, WindowMover, WindowMinimizer, GlobalX, 
         AttributeWriter, AttributeReader, PropertyWriter, PropertyReader, Reader
         Window, Reader Display, Reader (Dimension, Dimension), 
         (Reader (Window, Window, Window, Window)), Reader Conf, State Mode, 
         State (Set Window), State KeyStatus, State
         (Fix Tiler), State (Tiler (Fix Tiler)), Lift IO]
       ()
  -> IO ()
doAll t c m dims d w borders =
  void
    . runM
    . runState t
    . fixState
    . runState Default
    . runState (S.empty @Window)
    . runState m
    . runReader c
    . runReader borders
    . runReader dims
    . runReader d
    . runReader w
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
