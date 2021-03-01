module Tiler.ParentChild where

import Graphics.X11.Types
import Standard

-- | A child parent relationship between two windows.
data ParentChild = ParentChild {getParent :: Window, getChild :: Window, getPointerWin :: Window, getConfiguredSize :: Rect}
  deriving (Show)

-- | Is some window in the family?
inParentChild :: Window -> ParentChild -> Bool
inParentChild win (ParentChild ww ww' ww'' _) = win == ww || win == ww' || win == ww''

-- | If either the child or the parent are equal, then the whole type
--  is equal.
instance Eq ParentChild where
  (ParentChild a b _ _) == (ParentChild a' b' _ _) = a == a' || b == b'
