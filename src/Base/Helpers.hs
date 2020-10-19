module Base.Helpers where

import Data.Kind
import Graphics.X11.Types
import Standard

type family TypeMap (f :: a -> a -> b) (xs :: [a]) where
  TypeMap _ '[] = '[]
  TypeMap f (x ': xs) = f x x ': TypeMap f xs

type family (++) (a :: [t]) (b :: [t]) where
  (++) '[] b = b
  (++) (a ': as) b = a ': (as ++ b)

-- | A type level function which takes a list of Types and turns them into
--  inputs.
type Inputs (a :: [Type]) = TypeMap HasSource a

-- | Same as above but for State.
type States (a :: [Type]) = TypeMap HasState a

newtype OldTime = OldTime Time
  deriving (Show)

-- | Just makes it more clear when you say Input RootWindow.
type RootWindow = Window
