{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- This module contains some miscellaneous functions and types that are used in
-- other places.
module Base.Effects where

import Control.DeepSeq (force)
import Data.Text
import Data.Text.IO (appendFile)
import Data.Time
import Data.Time.Format.ISO8601 (formatShow, iso8601Format)
import Prelude hiding (appendFile, log)
import Control.Monad.Freer hiding (Members)
import Control.Monad.Freer.State
import Control.Monad
import Data.Kind
import Data.IORef
import Control.Applicative

type family Members (as :: [Type -> Type]) r :: Constraint where
  Members '[] _ = ()
  Members (IO ': as) r = (LastMember IO r, Members as r)
  Members (a ': as) r = (Member a r, Members as r)

data Input i a where
  Input :: Input i i

input :: Member (Input i) m => Eff m i
input = send Input

runInput :: i -> Eff (Input i ': m) a -> Eff m a
runInput i = interpret \case
  Input -> pure i


inputs :: Member (Input i) m => (i -> a) -> Eff m a
inputs f = f <$> input

data Output o a where
  Output :: o -> Output o ()

output :: Member (Output o) m => o -> Eff m ()
output o = send $ Output o

runOutput :: Eff (Output i ': m) a -> Eff m a
runOutput = interpret \case
  Output _ -> pure ()

runStateIORef :: LastMember IO m => IORef s -> Eff (State s ': m) a -> Eff m a
runStateIORef io = interpret \case
  Put s -> send $ writeIORef io s
  Get -> send $ readIORef io


type Log = Output 

log :: forall l m. Member (Log l) m => l -> Eff m ()
log = output @l

runStateLogged :: forall r s a. (Show s, Member (Log LogData) r) => (s, Text) -> Eff (State s ': r) a -> Eff r a
runStateLogged (s, t) = evalState s . logState t
  where
        logState :: Member (Log LogData) r' => Text -> Eff (State s ': r') x -> Eff (State s ': r') x
        logState name = reinterpret $ \case
          Put a -> do
            oldValue <- get @s
            log (LD (name <> " From") $ pack $ show oldValue) >> log (LD "to" $ pack $ show a) >> put a
          Get -> get

data LogData = LD
  { prefix :: Text,
    logMsg :: Text
  }
  deriving (Show)


runLogger :: Members '[State Bool, State [Text], IO] m => Eff (Log LogData ': m) a -> Eff m a
runLogger = interpret \case
  Output (LD prefix msg) -> do
    timeZone <- send getCurrentTimeZone
    timeUtc <- send getCurrentTime
    let timeStamp = "[" <> pack (formatShow iso8601Format (utcToLocalTime timeZone timeUtc)) <> "]"
        prefixWrap = "[" <> prefix <> "]"
        -- This memory leaks without a force
        fullMsg = force $ prefixWrap <> timeStamp <> msg
    modify @[Text] $ force . Prelude.take 100 . (:) fullMsg
    shouldLog <- get @Bool
    when shouldLog $
      send $ appendFile "/tmp/xest.log" (fullMsg <> "\n")

instance Semigroup a => Semigroup (Eff r a) where
  (<>) = liftA2 (<>)
instance Monoid a => Monoid (Eff r a) where
  mempty = pure mempty
