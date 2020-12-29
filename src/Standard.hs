{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module (and the ones in Standard/*) are all part of my custom prelude.
--  All of it is super adhoc but makes things either more "pure" or fixes little
--  annoyances. Many of these changes assume that breaking convention has no
--  cost. As a result, there are probably some contraversial changes in here.
module Standard
  ( module All,
    modify,
    pattern CofreeF,
    pattern Cofree,
    trd,
    fromEither,
    map,
    show,
    headMay,
    head,
    tailMay,
    lastMay,
    last,
    initMay,
    removeAt,
    remove,
    error,
    pattern Text,
  )
where

import Base.Effects as All
import BasePrelude as All hiding (String, appendFile, arr, error, filter, fmap, getContents, getLine, gunfold, head, index, init, interact, last, lazy, left, log, map, putStr, putStrLn, readFile, right, show, tail, uncons, unlines, writeFile, (!!), mapMaybe, filter, catMaybes)
import Data.Witherable as All
import qualified BasePrelude
-- Hiding Text because I define it below with a Complete pragma

import qualified BasePrelude as BP (fmap)
import Capability.Sink as All hiding (yield)
import Capability.Source as All
import Capability.State as All hiding (modify, zoom)
import Control.Comonad as All hiding (fmap)
import Control.Comonad.Cofree as All (Cofree)
import qualified Control.Comonad.Cofree as CC (Cofree ((:<)))
import qualified Control.Comonad.Trans.Cofree as C hiding (Cofree)
import Control.Lens as All hiding (none, para, (<|), uncons)
import Control.Monad.Zip as All
import Data.Functor.Foldable as All hiding (embed, fold, unfold)
import Data.Functor.Foldable.TH as All
import Data.IntMap.Strict as All (IntMap, update, (!))
import Data.Kind (Type)
import Data.List.NonEmpty as All (init, nonEmpty, tail, (!!), (<|), uncons)
import Data.Map.Strict as All (Map)
import Data.Semigroup.Foldable as All
import Data.Set as All (Set)
import Data.Text as All (Text, unlines)
import Data.Text.IO as All
import Data.Text.Lens as All hiding (Text)
import GHC.Stack
import Standard.Beam as All
import Standard.RectA as All
import Standard.Stream as All
import Standard.Tagged as All
import Standard.Transformation as All

{-# COMPLETE CofreeF #-}

pattern CofreeF :: forall (f :: Type -> Type) a b. a -> f b -> C.CofreeF f a b
pattern CofreeF a b = a C.:< b

{-# COMPLETE Cofree #-}

pattern Cofree :: forall (f :: Type -> Type) a. a -> f (CC.Cofree f a) -> CC.Cofree f a
pattern Cofree a b = a CC.:< b

-- | Like fst and snd but for the third element.
trd :: (a, b, c) -> c
trd (_, _, c) = c

fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

-- | The f in fmap seems to be historical baggage.
map :: Functor f => (a -> b) -> (f a -> f b)
map = BP.fmap

-- TODO replace this with a real Text alternative that doesn't create a String.
show :: Show a => a -> Text
show = Text . BasePrelude.show

headMay :: Cons s s a a => s -> Maybe a
headMay = preview _head

head :: Traversable1 t => t a -> a
head = view head1

tailMay :: Cons s s a a => s -> Maybe s
tailMay = preview _tail

-- TODO Given the amount of symmetry here, I'm amazed this one has stumped me.
-- tail :: Traversable1 t => NonEmpty a -> Maybe (NonEmpty a)

lastMay :: Snoc s s a a => s -> Maybe a
lastMay = preview _last

last :: Traversable1 t => t a -> a
last = view last1

initMay :: Snoc s s a a => s -> Maybe s
initMay = preview _init

-- TODO Symmetry reveals itself again. Since tail was really hard to write,
-- init is too.
-- init :: Traversable1 t => t a -> Maybe (t a)

removeAt :: Int -> NonEmpty a -> Maybe (NonEmpty a)
removeAt i = nonEmpty . map snd . filter (\(i', _) -> i /= i') . toList . mzip [0 ..]

remove :: Eq a => a -> NonEmpty a -> Maybe (NonEmpty a)
remove a = nonEmpty . filter (/= a) . toList

error :: HasCallStack => Text -> a
error (Text s) = BasePrelude.error s

{-# COMPLETE Text #-}

pattern Text :: BasePrelude.String -> Text
pattern Text a <-
  (view _Text -> a)
  where
    Text a = review _Text a

modify :: forall a m. HasState a a m => (a -> a) -> m ()
modify = modify' @a
