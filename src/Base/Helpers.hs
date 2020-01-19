{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Base.Helpers where

import           Standard
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Polysemy.Several
import           Data.Kind
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Types


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
runInputs :: HList t -> Sem (TypeConcat (Inputs t) r) a -> Sem r a
-- |Runs an HList of values as if they were values for a Reader.
runInputs = runSeveral runInputConst

-- |Same as above but for State.
type States (a :: [Type]) = TypeMap State a
-- |Same as above but for State. Throws away the state that would be returned
runStates :: HList t -> Sem (TypeConcat (States t) r) a -> Sem r a
runStates = runSeveral evalState

-- |Pretty much everything needs effects when being run. This type alias makes
-- that easiear to type.
type Interpret e r a = Members [Embed IO, Input Display] r => Sem (e ': r) a -> Sem r a

inputs :: Member (Input i) r => (i -> a) -> Sem r a
inputs f = f <$> input

-- * Effects
--
-- $WhyEffects
--
-- Xest makes use of the "Polysemy" package. Polysemy uses a ton of fairly
-- advanced Haskell features so it can sometimes look a little scary. Why
-- then do we use it?
--
--   * Effect systems like Polysemy make it easier to apply the principle
--   of least privilage to the code. For example, we can have a function
--   which can resize windows but would fail to compile if they tried to
--   change any other properties on the window.
--
--   * Effect systems make it easy to swap out the implementation details
--   without having to touch the code that uses the effects.
--
--   * Polysemy will have no performance cost once GHC 8.10 is released.

-- * Properties

-- * Event Flags

-- * Minimizer

-- * Mover

-- * Executor

-- * Colorer

-- * Global X



instance Semigroup a => Semigroup (Sem r a) where
  (<>) = liftA2 (<>)
instance Monoid a => Monoid (Sem r a) where
  mempty = pure mempty

-- |Just makes it more clear when you say Input RootWindow.
type RootWindow = Window
