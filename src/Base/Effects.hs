{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}


module Base.Effects where

import Prelude hiding (appendFile, log)
import Data.IORef
import Control.Monad.Reader
import Control.DeepSeq
import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans.Control
import Data.Kind (Constraint, Type)
import Data.Text
import Data.Text.IO (appendFile)
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Format, formatShow)
import Control.Monad.State.Strict (MonadState)
import GHC.TypeLits (KnownSymbol, symbolVal, Symbol)
import Data.Proxy (Proxy(Proxy))

-- This module, and the others in Base, implement a Tagless final interface.
-- Unfortunately, the compiler really hates this unless I compilet with -O0

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

instance {-# OVERLAPPABLE #-} (MonadTransControl t, State s m, Monad m, Monad (t m)) => State s (t m) where
  get = lift get
  put = lift . put

modify :: State s m => (s -> s) -> m ()
modify f = do
  s' <- get
  put $! f s'

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
