{-# LANGUAGE UndecidableInstances #-}

module Base.Property where

import Base.Executor
import Base.Helpers
import qualified Data.Map as M
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Graphics.X11.Types
import Graphics.X11.Xlib.Atom
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types
import Standard

-- | X11 defines properties as a dictionary like object from Atoms and windows
--  to bits. This effect allows you to read or write to those dictionaries.
--
--  It's worth noting that these properties are very untyped so be careful
--  with what you pass in and try to get out.
class Property m where
  -- | Gets a property from a window's dictionary
  getProperty ::
    Storable a =>
    -- | The number of bits in the property
    Int ->
    -- | The property key
    Atom ->
    -- | The window who's properties we want to look at
    Window ->
    -- | A list of values found at the key
    m [a]

  -- | Writes to a property in a window's dictionary
  putProperty ::
    -- | The number of bits in the property
    Int ->
    -- | The property key we want to write to
    Atom ->
    -- | The window who's properties we want to look at
    Window ->
    -- | A string representing the type of the data
    Atom ->
    [Int] -> -- The data we want to write
    m ()

  -- | Although most properties are only loosely defined, the class property
  --  is built into Xlib for some reason.
  getClassName ::
    -- | The Window we're interested in
    Window ->
    -- | The window's class
    m Text

  -- | Technically not a property but it kind of fits...
  getChildX ::
    -- | The parent
    Window ->
    -- | The child
    m (Maybe Window)

  isOverrideRedirect ::
    Window ->
    m Bool

  -- | Turns a Text into an X11 managed Atom. Think of atoms as pointers to
  --  strings.
  getAtom ::
    -- | Should we not create it if it doesn't exist?
    Bool ->
    -- | The atom's name
    Text ->
    -- | The atom created/retrieved from X11
    m Atom

  -- | Gives you the name of some Atom.
  fromAtom ::
    Atom ->
    m Text

  -- | Check if a window is transient for something else based on the
  --  ICCM protocol.
  getTransientFor ::
    -- | The window being analyzed
    Window ->
    -- | The parent window
    m (Maybe Window)

  getSizeHints ::
    -- | The window being analyzed
    Window ->
    m SizeHints

type AtomCache = Map Text Atom

type RootPropCache = Map Atom [Int]

-- | Runs a propertiy using IO
instance Members (Executor ': MonadIO ': States [AtomCache, RootPropCache] ++ Inputs '[RootWindow, Display]) m => Property m where
  getProperty i atom win =
    input >>= \d ->
      liftIO $
        fromMaybe [] <$> rawGetWindowProperty i d atom win

  putProperty i atomContent w atomType msg = do
    d <- input
    rootWin <- input
    rootPropCache <- get @RootPropCache
    unless (rootWin == w && (== Just msg) (rootPropCache M.!? atomContent)) $ do
      let longMsg = map fromIntegral msg
          charMsg = map fromIntegral msg
      liftIO $ case i of
        8 -> changeProperty8 d w atomContent atomType propModeReplace $ charMsg
        32 -> changeProperty32 d w atomContent atomType propModeReplace $ longMsg
        _ -> error "Can only write 32 and 8 bit values to properties right now"
    when (rootWin == w) $
      modify @RootPropCache (M.insert atomContent msg)

  getClassName win =
    input >>= \d -> liftIO $ getClassHint d win >>= \(ClassHint _ n) -> return (fromString n)

  isOverrideRedirect win =
    input >>= \d ->
      liftIO $
        either (const False) wa_override_redirect <$> try @SomeException (getWindowAttributes d win)

  getChildX win = do
    display <- input @Display
    -- For some reason, xQueryTree hasn't been abstracted at all
    liftIO $ alloca
      \numChildrenPtr -> alloca
        \childrenListPtr -> alloca
          \uselessPtr -> do
            _ <- xQueryTree display win uselessPtr uselessPtr childrenListPtr numChildrenPtr
            numChildren <- peek numChildrenPtr
            headMay <$> (peek childrenListPtr >>= peekArray (fromIntegral numChildren))

  getAtom shouldCreate name@(Text sName) =
    input >>= \d -> do
      maybeAtom <- (M.!? name) <$> get @(Map Text Atom)
      case maybeAtom of
        Nothing -> do
          atom <- liftIO $ internAtom d sName shouldCreate
          modify @(Map Text Atom) $ M.insert name atom
          return atom
        Just atom -> return atom

  fromAtom atom =
    input >>= \d ->
      liftIO $ fromMaybe "" . map fromString <$> getAtomName d atom

  getTransientFor w = do
    d <- input @Display
    liftIO $ getTransientForHint d w

  getSizeHints w = do
    d <- input @Display
    liftIO $ getWMNormalHints d w

getRealName ::
  (Monad m, Property m) =>
  -- | The Window we're interested in
  Window ->
  -- | The window's name
  m Text
getRealName win = do
  netwmname <- getAtom False "_NET_WM_NAME"
  wmname <- getAtom False "WM_NAME"
  net_name <- fromString <$> getProperty 32 netwmname win
  name <- fromString <$> getProperty 32 wmname win
  return if nullOf text net_name then name else net_name
