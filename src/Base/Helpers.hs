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
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Data.Kind
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Types


infixr 5 :::
data HList a where
    HNil  :: HList '[]
    (:::) :: a -> HList (b :: [Type]) -> HList (a ': b)

type family TypeMap (f :: a -> b) (xs :: [a]) where
    TypeMap _ '[]       = '[]
    TypeMap f (x ': xs) = f x ': TypeMap f xs

type family (++) (a :: [t]) (b :: [t]) where
  (++) '[] b = b
  (++) (a ': as) b = a ': (as ++ b)

runSeveral
    :: (forall r' k x. k -> Sem (e k ': r') x -> Sem r' x)
    -> HList t
    -> Sem ((TypeMap e t) ++ r) a
    -> Sem r a
runSeveral f (a ::: as) = runSeveral f as . f a
runSeveral _ HNil       = id

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
runInputs :: HList t -> Sem ((Inputs t) ++ r) a -> Sem r a
-- |Runs an HList of values as if they were values for a Reader.
runInputs = runSeveral runInputConst


-- |Same as above but for State.
type States (a :: [Type]) = TypeMap State a
-- |Same as above but for State. Throws away the state that would be returned
runStates :: HList t -> Sem ((States t) ++ r) a -> Sem r a
runStates = runSeveral evalState

runStateLogged :: forall r s a. (Show s, Members [Log LogData, Embed IO] r) => (s, Text) -> Sem ((State s) ': r) a -> Sem r a
runStateLogged (s, t) = map snd . stateToIO s . logState t
  where 
        logState :: Member (Log LogData) r' => Text -> Sem (State s ': r') x -> Sem (State s ': r') x
        logState name = intercept $ \case
          Put a -> do
            oldValue <- get
            log (LD (name <> " From") $ show oldValue) *> log (LD "to" $ show a) *> put a
          Get -> get
        {-# INLINE logState #-}
{-# INLINE runStateLogged #-}


-- |Pretty much everything needs effects when being run. This type alias makes
-- that easiear to type.
type Interpret e r a = Members [Embed IO, Input Display] r => Sem (e ': r) a -> Sem r a

inputs :: Member (Input i) r => (i -> a) -> Sem r a
inputs f = f <$> input

instance Semigroup a => Semigroup (Sem r a) where
  (<>) = liftA2 (<>)
instance Monoid a => Monoid (Sem r a) where
  mempty = pure mempty

-- |Just makes it more clear when you say Input RootWindow.
type RootWindow = Window

data LogData = LD { prefix :: Text
                  , logMsg :: Text
                  }
