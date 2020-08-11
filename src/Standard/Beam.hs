{-# LANGUAGE TemplateHaskell #-}

module Standard.Beam where

import Data.Functor.Foldable.TH
import BasePrelude

-- |You can think of a beam as the opposite of a list. instead of having
-- 0 or more elements and a guaranteed empty case, Beam has 0 or more
-- empty cases and a guaranteed element wrapped inside.
--
-- You might be thinking, "This looks completely useluss!" and if we think of
-- it as a normal container (like array, tree, list, etc.) then you would
-- probably be right. If you think of Beam not as a container but as control
-- flow, you get some cool results though. For an example, look at onInput .
-- Instead of doing it recursively, you can use a hylomorphism to abstract away
-- the recursion. At this point though, you need to pick a data type for F wich
-- will wrap the intermediate result.  What do you pick? Well Beam makes a
-- great choice. For each recursive call, you just return Continue. Once you
-- reach the leaf, you return End. Then, you can use a simple catamorphism to
-- extract the value from the beam.
data Beam a = End a | Continue (Beam a)
  deriving (Eq, Show, Functor)

makeBaseFunctor ''Beam

getEnd :: BeamF a a -> a
getEnd (EndF a)      = a
getEnd (ContinueF a) = a
