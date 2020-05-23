{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Base.Helpers where

import           Standard
import           Data.Kind
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Types
import GHC.TypeLits (Symbol)
import Control.Monad.State.Strict (runStateT, StateT)


-- infixr 5 :::
-- data HList a where
--     HNil  :: HList '[]
--     (:::) :: a -> HList (b :: [Type]) -> HList (a ': b)

type family TypeMap (f :: a -> b) (xs :: [a]) where
    TypeMap _ '[]       = '[]
    TypeMap f (x ': xs) = f x ': TypeMap f xs

type family (++) (a :: [t]) (b :: [t]) where
  (++) '[] b = b
  (++) (a ': as) b = a ': (as ++ b)

-- class RunMany t m effects where
--   type family ReturnMany effects t m :: Type -> Type
--   type family InputMany effects t m :: Type -> Type
--   runSeveral
--       :: (forall k x. t k m x -> k -> m x)
--       -> HList effects
--       -> InputMany
--       -> ReturnMany a

-- instance RunMany t m '[] where
--   type instance ReturnMany '[] t m = t
--   runSeveral = 
--   runSeveral f (a ::: as) = runSeveral f as . f a
--   runSeveral _ HNil       = id

-- * Helper Polysemy functions
-- $RunningSeveral
--
-- See "Polysemy.RunSeveral" for some more info about what's going on here.
--
-- If you're writing code which needs to operate on multiple inputs or
-- states, you should probably use these.

-- |A type level function which takes a list of Types and turns them into
-- inputs.
type Inputs (a :: [Type]) = TypeMap Input a
-- runInputs :: HList t -> Sem ((Inputs t) ++ r) a -> Sem r a
-- -- |Runs an HList of values as if they were values for a Reader.
-- runInputs = runSeveral runInputConst


-- |Same as above but for State.
type States (a :: [Type]) = TypeMap State a
-- |Same as above but for State. Throws away the state that would be returned
-- runStates :: HList t -> Sem ((States t) ++ r) a -> Sem r a
-- runStates = runSeveral evalState

runStateLogged :: forall (name :: Symbol) s a m. (Members '[Log LogData, MonadIO] m) => s -> LoggedState name (StateT s m) a -> m a
runStateLogged s = map fst . flip runStateT s . runLoggedState


-- |Pretty much everything needs effects when being run. This type alias makes
-- that easiear to type.
-- type Interpret e r a = Members [MonadIO, Input Display] r => Sem (e ': r) a -> Sem r a

inputs :: Input i m => (i -> a) -> m a
inputs f = f <$> input

-- instance Semigroup a => Semigroup (m a) where
--   (<>) = liftA2 (<>)
-- instance Monoid a => Monoid (m a) where
--   mempty = pure mempty

-- |Just makes it more clear when you say Input RootWindow.
type RootWindow = Window
