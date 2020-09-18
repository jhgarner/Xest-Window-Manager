{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}


module Base.Effects where

import Prelude hiding (appendFile, log)
import Control.Monad.Reader
import Data.Kind (Constraint, Type)
import Data.Text
import Data.Text.IO (appendFile)
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Format, formatShow)
import GHC.TypeLits (KnownSymbol, symbolVal, Symbol)
import Data.Proxy (Proxy(Proxy))
import Capability.Source
import Capability.State
import Capability.Sink
import Data.Coerce (coerce, Coercible)
import Control.DeepSeq (force)


-- Defines a series of effects.
-- Used all over the effectful code.
type family Members (effects :: [(Type -> Type) -> Constraint]) (m :: Type -> Type) :: Constraint where
  Members '[] m = Monad m
  Members (effect ': rest) m = (effect m, Members rest m)

-- These are alternative names that match Polysemy. They also hide the tag
-- parameter to match Polysemy even more.
type Input a = HasSource a a
input :: forall i m. Input i m => m i
input = await @i

inputs :: Input i m => (i -> a) -> m a
inputs f = f <$> input


type Output a = HasSink a a
output :: forall i m. Output i m => i -> m ()
output o = yield @i o

type State s = HasState s s

type Log l = HasSink l l
log :: forall l m. Log l m => l -> m ()
log = yield @l


-- This defines a Sink where every write gets logged.
newtype LoggedSink (name :: Symbol) (s :: Type) (m :: Type -> Type) n a = LoggedSink { runLoggedState :: (m a) }
  deriving (Functor, Applicative, Monad)
instance HasSource k s m => HasSource k s (LoggedSink name s m n) where
  await_ p = LoggedSink $ await_ p

instance (forall a. Coercible (n a) (m a), Log LogData n, State s m, Show s, KnownSymbol name) => HasSink k s (LoggedSink name s m n) where
  yield_ _ a = LoggedSink do
    oldValue <- (await @s)
    () <- coerce $ log @LogData @n (LD (pack $ symbolVal (Proxy @name) <> " From") $ pack $ show oldValue)
    () <- coerce $ log @LogData @n (LD "to" $ pack $ show a)
    put @s a

instance (State s m, HasSink s s (LoggedSink name s m n)) => HasState s s (LoggedSink name s m n) where
  state_ p s = LoggedSink $ state_ p s


data LogData = LD { prefix :: Text
                  , logMsg :: Text
                  }
  deriving Show

type LogLines = [Text]

-- The logging implementation
newtype Logger m a = Logger { runLogger :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)
  
instance Members '[Input Bool, State [Text], MonadIO] m => HasSink k LogData (Logger m) where
  yield_ _ (LD prefix msg) = Logger do
    timeZone <- liftIO getCurrentTimeZone
    timeUtc <- liftIO getCurrentTime
    let timeStamp = "[" <> pack (formatShow iso8601Format (utcToLocalTime timeZone timeUtc)) <> "]"
        prefixWrap = "[" <> prefix <> "]"
        -- This memory leaks without a force
        fullMsg = force $ prefixWrap <> timeStamp <> msg
    modify @[Text] $ force . Prelude.take 100 . (:) fullMsg
    shouldLog <- input @Bool
    when shouldLog $
      liftIO $ appendFile "/tmp/xest.log" (fullMsg <> "\n")
