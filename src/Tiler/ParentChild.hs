module Tiler.ParentChild where

import           Standard
import           Text.Show.Deriving
import           Data.Eq.Deriving
import           Graphics.X11.Types

-- |A child parent relationship between two windows.
data ParentChild = ParentChild Window Window
  deriving Show

-- |Is some window in the family?
inParentChild :: Window -> ParentChild -> Bool
inParentChild win (ParentChild ww ww') = win == ww || win == ww'

-- |If either the child or the parent are equal, then the whole typ
-- is equal.
instance Eq ParentChild where
  (ParentChild a b) == (ParentChild a' b') = a == a' || b == b'

