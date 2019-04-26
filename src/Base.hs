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
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Window
import           Graphics.X11.Xlib.Atom
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
makeSemantic ''PropertyReader

data PropertyWriter m a where
  SetProperty32 ::String -> String -> Window -> [Int] -> PropertyWriter m ()
  SetProperty8 ::String -> String -> Window -> [Int] -> PropertyWriter m ()
makeSemantic ''PropertyWriter

data AttributeWriter m a where
  SelectFlags ::Window -> Mask -> AttributeWriter m ()
  SetFocus ::Window -> AttributeWriter m ()
makeSemantic ''AttributeWriter

data AttributeReader m a where
  GetClassName ::Window -> AttributeReader m String
makeSemantic ''AttributeReader

data WindowMover m a where
  ChangeLocation ::Rect -> Window -> WindowMover m ()
  Raise ::Window -> WindowMover m ()
  ConfigureWin ::Event -> WindowMover m ()
makeSemantic ''WindowMover

data WindowMinimizer m a where
  Minimize ::Window -> WindowMinimizer m ()
  Restore ::Window -> WindowMinimizer m ()
makeSemantic ''WindowMinimizer

data Executor m a where
  Execute ::String -> Executor m ()
makeSemantic ''Executor

data GlobalX m a where
   GetTree ::GlobalX m [Window]
   RebindKeys ::Mode -> GlobalX m ()
   GetXEvent ::GlobalX m Event
makeSemantic ''GlobalX

type DIO r = (Member (Lift IO) r, Member (Reader Display) r)

runPropertyReader :: DIO r => Semantic (PropertyReader ': r) a -> Semantic r a
runPropertyReader = interpret $ \case
  GetProperty size aName w -> ask >>= \d -> sendM @IO $ do
    atom <- liftIO $ internAtom d aName False
    liftIO $ rawGetWindowProperty size d atom w
  IsSameAtom aName atom -> ask >>= \d -> sendM @IO $ do
    otherAtom <- liftIO $ internAtom d aName False
    return $ atom == otherAtom

runPropertyWriter :: DIO r => Semantic (PropertyWriter ': r) a -> Semantic r a
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

runAttributeWriter :: DIO r => Semantic (AttributeWriter ': r) a -> Semantic r a
runAttributeWriter = interpret $ \case
  SelectFlags w flags -> ask >>= \d -> sendM @IO $ selectInput d w flags
  SetFocus w          -> ask @Display
    >>= \d -> sendM @IO $ setInputFocus d w revertToNone currentTime

runAttributeReader :: DIO r => Semantic (AttributeReader ': r) a -> Semantic r a
runAttributeReader = interpret $ \case
  GetClassName win ->
    ask >>= \d -> sendM $ getClassHint d win >>= \(ClassHint _ n) -> return n

runWindowMover :: DIO r => Semantic (WindowMover ': r) a -> Semantic r a
runWindowMover = interpret $ \case
  ChangeLocation (Rect x y h w) win -> do
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
  => Semantic (WindowMinimizer ': r) a
  -> Semantic r a
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

runExecutor :: Member (Lift IO) r => Semantic (Executor ': r) a -> Semantic r a
runExecutor = interpret $ \case
  Execute s -> void . sendM $ spawnCommand s

runGlobalX
  :: (DIO r, Member (Reader Window) r, Member (Reader Conf) r)
  => Semantic (GlobalX ': r) a
  -> Semantic r a
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

-- TODO make this less garbage
type DoAll r
  =  ( Member WindowMover r
  , Member (State (Fix Tiler)) r
  , Member (State KeyStatus) r
  , Member (State Mode) r
  , Member (State (Set Window)) r
  , Member (Reader Conf) r
  , Member (Reader Window) r
  , Member (Reader (Dimension, Dimension)) r
  , Member PropertyReader r
  , Member PropertyWriter r
  , Member AttributeReader r
  , Member AttributeWriter r
  , Member WindowMinimizer r
  , Member Executor r
  , Member GlobalX r
  )
  => Semantic r [Action]
doAll
  :: Fix Tiler
  -> Conf
  -> Mode
  -> (Dimension, Dimension)
  -> Display
  -> Window
  -> Semantic
       '[Executor, WindowMover, WindowMinimizer, GlobalX, AttributeWriter, AttributeReader, PropertyWriter, PropertyReader, Reader
         Window, Reader Display, Reader (Dimension, Dimension), Reader
         Conf, State Mode, State (Set Window), State KeyStatus, State
         (Fix Tiler), Lift IO]
       ()
  -> IO ()
doAll t c m dims d w =
  void
    . runM
    . runState t
    . runState Default
    . runState (S.empty @Window)
    . runState m
    . runReader c
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
