module Standard.Tagged where

import BasePrelude
import Control.Comonad

-- |Tagged is a lot like Either except more optimistic. In Either, a single
-- failure propogates upwards. With Tagged, a single Success propogates
-- upwards.
data Tagged a = Failed a | Succeeded a
  deriving (Show, Eq, Read, Functor, Foldable, Traversable)

instance Semigroup (Tagged a) where
  Failed _ <> a = a
  a <> _ = a

-- Fun fact: I actually had pure flipped the first time I implemented this.
-- Instead of wrapping the input in a Failed, it wrapped the input in a
-- Success. This caused some big problems in other parts of my code and took a
-- while to find/debug. Once I caught the error, I realized that the instance I
-- had defined wasn't lawful. Once I made it lawful, all of my bugs went away.
-- That made me realize the power of laws. If I had actually proven the
-- Applicative laws instead of handwaving them away, I would have caught the
-- bug immediately. Instead I wasted tons of time searching elsewhere for the
-- bug.
instance Applicative Tagged where
  pure a = Failed a
  Succeeded f <*> ta = Succeeded $ f $ extract ta
  Failed f <*> ta = fmap f ta

instance Comonad Tagged where
  extract (Failed a) = a
  extract (Succeeded a) = a

  duplicate (Failed a) = Failed $ Failed a
  duplicate (Succeeded a) = Succeeded $ Succeeded a
