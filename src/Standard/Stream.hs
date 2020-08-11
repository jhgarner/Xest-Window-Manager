module Standard.Stream where

import BasePrelude

-- |This stream lets me return an infinite list of values where each value
-- comes from some monadic computation. To understand why I need this, consider
-- implementing filterStream on something of type [m a] where filterStream
-- remains lazy.
data Stream m a = Stream a (m (Stream m a))

filterStream :: Monad m => (a -> Bool) -> Stream m a -> m (Stream m a)
filterStream p (Stream a m)
  | p a = return $ Stream a (filterStream p =<< m)
  | otherwise = filterStream p =<< m

repeatStream :: Functor m => m a -> m (Stream m a)
repeatStream m = fmap (\a -> Stream a $ repeatStream m) m

overStream :: Monad m => (a -> m b) -> Stream m a -> m c
overStream f (Stream a m) = f a >> m >>= overStream f
