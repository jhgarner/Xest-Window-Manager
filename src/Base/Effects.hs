{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Strict #-}


module Base.Effects where

import Prelude hiding (appendFile, log)
import Data.IORef
import Control.Monad.Reader
import Control.DeepSeq
import qualified Control.Monad.State.Strict as S
import Control.Monad.Except
import Control.Monad.Trans.Control
import Data.Kind (Constraint, Type)
import Data.Text
import Data.Text.IO (appendFile)
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Format, formatShow)
import Control.Monad.State.Strict (MonadState)
import GHC.TypeLits
import Data.Proxy (Proxy(Proxy))

type family TypeMap (f :: a -> b) (xs :: [a]) where
    TypeMap _ '[]       = '[]
    TypeMap f (x ': xs) = f x ': TypeMap f xs


infixr 5 :::
data HList a where
    HNil  :: HList '[]
    (:::) :: a -> HList (b :: [Type]) -> HList (a ': b)

travHList :: (forall a. a -> IO (IORef a)) -> HList ls -> IO (HList (TypeMap IORef ls))
travHList _ HNil = return HNil
travHList f (a ::: as) = (:::) <$> f a <*> travHList f as

newtype Reads (ls :: [Type]) m a = Reads { runReads :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)
instance MonadTrans (Reads ls) where
  lift = Reads
instance MonadTransControl (Reads ls) where
  type instance StT (Reads ls) a = a
  liftWith f = Reads $ f runReads
  restoreT = lift

data TNum = Z | S TNum

type family Where a (ls :: [Type]) where
  Where a (a ': _) = Z
  Where a (_ ': as) = S (Where a as)

class Contains a (n :: TNum) (ls :: [Type]) where
  pullOut :: Proxy n -> HList ls -> a

instance Contains a Z (a ': as) where
  pullOut _ (a ::: _) = a
instance Contains b n as => Contains b (S n) (a ': as) where
  pullOut _ (_ ::: as) = pullOut (Proxy @n) as

type family Members (effects :: [(Type -> Type) -> Constraint]) (m :: Type -> Type) :: Constraint where
  Members '[] m = Monad m
  Members (effect ': rest) m = (effect m, Members rest m)

class Monad m => Input i m where
  input :: m i

instance Monad m => Input i (ReaderT i m) where
  input = ask
  
instance (MonadIO m, Monad m) => Input i (ReaderT (IORef i) m) where
  input = ask >>= \a -> liftIO $ readIORef a
  
instance {-# OVERLAPPABLE #-} (MonadTransControl t, Input i m, Monad m, Monad (t m)) => Input i (t m) where
  input = lift input



class Monad m => Output o m where
  output :: o -> m ()

instance Monad m => Output i (S.StateT i m) where
  output = S.put
  
instance (MonadIO m, Monad m) => Output i (ReaderT (IORef i) m) where
  output o = ask >>= \a -> liftIO $ writeIORef a o

instance {-# OVERLAPPABLE #-} (MonadTransControl t, Output i m, Monad m, Monad (t m)) => Output i (t m) where
  output = lift . output


class Monad m => State s m where
  get :: m s
  put :: s -> m ()

instance Monad m => State s (S.StateT s m) where
  get = S.get
  put = S.put
  
instance (MonadIO m, Monad m) => State s (ReaderT (IORef s) m) where
  put o = ask >>= \a -> liftIO $ writeIORef a o
  get = ask >>= \a -> liftIO $ readIORef a
  
-- instance (MonadIO m, Monad m) => State s (ReaderT (HList (IORef s ': ls)) m) where
--   put o = ask >>= \(a ::: _) -> liftIO $ writeIORef a o
--   get = ask >>= \(a ::: _) -> liftIO $ readIORef a

-- instance {-# OVERLAPPABLE #-} (MonadIO m, Monad m, State s (ReaderT (HList ls) m)) => State s (ReaderT (HList (l ': ls)) m) where
--   put o = ask >>= \(_ ::: ls) -> runReaderT (put o) ls
--   get = ask >>= \(_ ::: ls) -> runReaderT get ls

-- instance (MonadIO m, Monad m, State s m) => State s (ReaderT (HList '[]) m) where
--   get = lift get
--   put = lift . put

instance (Monad m, MonadIO m, Contains (IORef s) (Where (IORef s) ls) ls) => State s (Reads (s ': as) (ReaderT (HList ls) m)) where
  get = lift $ ask >>= \hls -> liftIO $ readIORef $ pullOut (Proxy @(Where (IORef s) ls)) hls
  put newA = lift $ ask >>= \hls -> liftIO $  writeIORef (pullOut (Proxy @(Where (IORef s) ls)) hls) newA
  
instance {-# OVERLAPPABLE #-} (Monad m, State s (Reads as (ReaderT (HList ls) m))) => State s (Reads (a ': as) (ReaderT (HList ls) m)) where
  get = Reads $ runReads @as $ get
  put newA = Reads $ runReads @as $ put newA

dropAReads :: Reads (s ': as) m a -> Reads as m a
dropAReads = Reads . runReads


instance {-# OVERLAPPABLE #-} (MonadTransControl t, State s m, Monad m, Monad (t m)) => State s (t m) where
  get = lift get
  put = lift . put

modify :: State s m => (s -> s) -> m ()
modify f = put . f =<< get

gets :: State s m => (s -> a) -> m a
gets f = f <$> get


class Monad m => Log l m where
  log :: l -> m ()

newtype LoggedState (name :: Symbol) m a = LoggedState { runLoggedState :: (m a) }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState s, MonadReader r)

instance MonadTrans (LoggedState name) where
  lift = LoggedState
  
instance MonadTransControl (LoggedState n) where
  type instance StT (LoggedState n) a = a
  liftWith f = LoggedState $ f runLoggedState
  restoreT = lift

instance (Log LogData m, State s m, Show s, KnownSymbol name) => State s (LoggedState name m) where
  put a = lift do
    oldValue <- (get @s)
    log (LD (pack $ symbolVal (Proxy @name) <> " From") $ pack $ show oldValue)
    log (LD "to" $ pack $ show a)
    put a
  get = lift get

data LogData = LD { prefix :: Text
                  , logMsg :: Text
                  }
  deriving Show

-- instance Log LogData IO where
--   log s = print s
  
instance {-# OVERLAPPABLE #-} (MonadTransControl t, Log s m, Monad m, Monad (t m)) => Log s (t m) where
  log = lift . log
    
newtype Logger m a = Logger { runLogger :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)
instance MonadTrans Logger where
  lift = Logger
instance MonadTransControl Logger where
  type instance StT Logger a = a
  liftWith f = Logger $ f runLogger
  restoreT = lift
  
instance Members '[State Bool, State [Text], MonadIO] m => Log LogData (Logger m) where
  log (LD prefix msg) = do
    timeZone <- liftIO getCurrentTimeZone
    timeUtc <- liftIO getCurrentTime
    let timeStamp = "[" <> pack (formatShow iso8601Format (utcToLocalTime timeZone timeUtc)) <> "]"
        prefixWrap = "[" <> prefix <> "]"
        fullMsg = prefixWrap <> timeStamp <> msg
    modify @[Text] $ force . Prelude.take 100 . (:) fullMsg
    shouldLog <- get @Bool
    when shouldLog $
      liftIO $ appendFile "/tmp/xest.log" (fullMsg <> "\n")

-- class Monad m => Error e m where
--   throw :: e -> m a
--   catch :: m a -> (e -> m a) -> m a

-- instance Monad m => Error e (ExceptT e m) where
--   throw = throwError
--   catch = catchError

-- instance {-# OVERLAPPABLE #-} (MonadTransControl t, Error e m, Monad m, Monad (t m)) => Error e (t m) where
--   throw e = lift $ throw e
--   catch a f = (liftWith (\run -> catch (run a) (run . f))) >>= restoreT . return
