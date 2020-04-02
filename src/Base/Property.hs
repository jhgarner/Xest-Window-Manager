{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Base.Property where

import           Standard
import           Polysemy
import           Polysemy.State
import           Polysemy.Input
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Atom
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import qualified Data.Map                      as M
import Base.Helpers
import Base.Executor
import qualified Control.Exception as E


-- |X11 defines properties as a dictionary like object from Atoms and windows
-- to bits. This effect allows you to read or write to those dictionaries.
--
-- It's worth noting that these properties are very untyped so be careful
-- with what you pass in and try to get out.
data Property m a where
  -- |Gets a property from a window's dictionary
  GetProperty :: Storable a
              => Int -- ^ The number of bits in the property
              -> Atom  -- ^ The property key
              -> Window -- ^ The window who's properties we want to look at
              -> Property m [a] -- ^ A list of values found at the key

  -- |Writes to a property in a window's dictionary
  PutProperty :: Int -- ^ The number of bits in the property
              -> Atom -- ^ The property key we want to write to
              -> Window  -- ^ The window who's properties we want to look at
              -> Atom -- ^ A string representing the type of the data
              -> [Int] -- The data we want to write
              -> Property m ()

  -- |Although most properties are only loosely defined, the class property
  -- is built into Xlib for some reason.
  GetClassName :: Window -- ^ The Window we're interested in
               -> Property m String -- ^ The window's class

  -- |Technically not a property but it kind of fits...
  GetChildX :: Window -- ^ The parent
           -> Property m (Maybe Window) -- ^ The child
  IsOverrideRedirect :: Window
                     -> Property m Bool

  -- |Turns a String into an X11 managed Atom. Think of atoms as pointers to
  -- strings.
  GetAtom :: Bool -- ^ Should we not create it if it doesn't exist?
          -> String -- ^ The atom's name
          -> Property m Atom -- ^ The atom created/retrieved from X11

  -- |Gives you the name of some Atom.
  FromAtom :: Atom
           -> Property m String

  -- |Check if a window is transient for something else based on the
  -- ICCM protocol.
  GetTransientFor :: Window -- ^ The window being analyzed
                  -> Property m (Maybe Window) -- ^ The parent window
  
  GetSizeHints :: Window -- ^ The window being analyzed
               -> Property m SizeHints

makeSem ''Property

type AtomCache = Map String Atom
type RootPropCache = Map Atom [Int]

-- |Runs a propertiy using IO
runProperty :: Members (States [AtomCache, RootPropCache]) r
            => Members [Input RootWindow, Executor] r
            => Interpret Property r a
runProperty = interpret $ \case
  GetProperty i atom win -> input >>= \d -> embed @IO $
    fromMaybe [] <$> rawGetWindowProperty i d atom win

  PutProperty i atomContent w atomType msg -> do
    d <- input
    rootWin <- input
    rootPropCache <- get @RootPropCache
    unless (rootWin == w && (== Just msg) (rootPropCache M.!? atomContent)) $ do
      let longMsg = fmap fromIntegral msg
          charMsg = fmap fromIntegral msg
      embed @IO $ case i of
        8 -> changeProperty8 d w atomContent atomType propModeReplace $ charMsg
        32 -> changeProperty32 d w atomContent atomType propModeReplace $ longMsg
        _ -> error "Can only write 32 and 8 bit values to properties right now"
    when (rootWin == w) $
      modify @RootPropCache (M.insert atomContent msg)
      

  GetClassName win ->
    input >>= \d -> embed $ getClassHint d win >>= \(ClassHint _ n) -> return n

  IsOverrideRedirect win ->
    input >>= \d -> embed $
      either (const False) wa_override_redirect <$> tryAny (getWindowAttributes d win)

  GetChildX win -> do
    display <- input @Display
    -- For some reason, xQueryTree hasn't been abstracted at all
    embed @IO $ alloca
      (\numChildrenPtr -> alloca
        (\childrenListPtr -> do
          uselessPtr <- alloca $ \x -> return x
          _          <- xQueryTree display
                                   win
                                   uselessPtr
                                   uselessPtr
                                   childrenListPtr
                                   numChildrenPtr
          numChildren <- peek numChildrenPtr
          headMay <$> (peek childrenListPtr >>= peekArray (fromIntegral numChildren))
        )
      )

  GetAtom shouldCreate name -> input >>= \d -> do
    maybeAtom <- (M.!? name) <$> get
    case maybeAtom of
      Nothing -> do
        atom <- embed @IO $ internAtom d name shouldCreate
        modify $ M.insert name atom
        return atom
      Just atom -> return atom

  FromAtom atom -> input >>= \d ->
    embed @IO $ fromMaybe "" <$> getAtomName d atom

  GetTransientFor w -> do
    d <- input @Display
    embed $ getTransientForHint d w

  GetSizeHints w -> do
    d <- input @Display
    embed $ getWMNormalHints d w

