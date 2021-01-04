{-# LANGUAGE TemplateHaskell #-}

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
data Property m where
  -- | Gets a property from a window's dictionary
  GetProperty ::
    Storable a =>
    -- | The number of bits in the property
    Int ->
    -- | The property key
    Atom ->
    -- | The window who's properties we want to look at
    Window ->
    -- | A list of values found at the key
    Property [a]

  -- | Writes to a property in a window's dictionary
  PutProperty ::
    -- | The number of bits in the property
    Int ->
    -- | The property key we want to write to
    Atom ->
    -- | The window who's properties we want to look at
    Window ->
    -- | A string representing the type of the data
    Atom ->
    [Int] -> -- The data we want to write
    Property ()

  -- | Although most properties are only loosely defined, the class property
  --  is built into Xlib for some reason.
  GetClassName ::
    -- | The Window we're interested in
    Window ->
    -- | The window's class
    Property Text

  -- | Technically not a property but it kind of fits...
  GetChildX ::
    -- | The parent
    Window ->
    -- | The child
    Property (Maybe Window)

  IsOverrideRedirect ::
    Window ->
    Property Bool

  -- | Turns a Text into an X11 managed Atom. Think of atoms as pointers to
  --  strings.
  GetAtom ::
    -- | Should we not create it if it doesn't exist?
    Bool ->
    -- | The atom's name
    Text ->
    -- | The atom created/retrieved from X11
    Property Atom

  -- | Gives you the name of some Atom.
  FromAtom ::
    Atom ->
    Property Text

  -- | Check if a window is transient for something else based on the
  --  ICCM protocol.
  GetTransientFor ::
    -- | The window being analyzed
    Window ->
    -- | The parent window
    Property (Maybe Window)

  GetSizeHints ::
    -- | The window being analyzed
    Window ->
    Property SizeHints

makeEffect ''Property

type AtomCache = Map Text Atom

type RootPropCache = Map Atom [Int]

-- | Runs a propertiy using IO
runProperty :: Members (Executor ': IO ': States [AtomCache, RootPropCache] ++ Inputs '[RootWindow, Display]) m => Eff (Property ': m) a -> Eff m a
runProperty = interpret \case
  GetProperty i atom win ->
    input >>= \d ->
      liftIO $
        fromMaybe [] <$> rawGetWindowProperty i d atom win

  PutProperty i atomContent w atomType msg -> do
    d <- input
    rootWin <- input
    rootPropCache <- get @RootPropCache
    unless (rootWin == w && (== Just msg) (rootPropCache M.!? atomContent)) $ do
      let longMsg = map fromIntegral msg
          charMsg = map fromIntegral msg
      liftIO $ case i of
        8 -> changeProperty8 d w atomContent atomType propModeReplace charMsg
        32 -> changeProperty32 d w atomContent atomType propModeReplace longMsg
        _ -> error "Can only write 32 and 8 bit values to properties right now"
    when (rootWin == w) $
      modify @RootPropCache (M.insert atomContent msg)

  GetClassName win ->
    input >>= \d -> liftIO $ getClassHint d win >>= \(ClassHint _ n) -> return (fromString n)

  IsOverrideRedirect win ->
    input >>= \d ->
      liftIO $
        either (const False) wa_override_redirect <$> try @SomeException (getWindowAttributes d win)

  GetChildX win -> do
    display <- input @Display
    -- For some reason, xQueryTree hasn't been abstracted at all
    liftIO $ alloca
      \numChildrenPtr -> alloca
        \childrenListPtr -> alloca
          \uselessPtr -> do
            _ <- xQueryTree display win uselessPtr uselessPtr childrenListPtr numChildrenPtr
            numChildren <- peek numChildrenPtr
            headMay <$> (peek childrenListPtr >>= peekArray (fromIntegral numChildren))

  GetAtom shouldCreate name@(Text sName) ->
    input >>= \d -> do
      maybeAtom <- (M.!? name) <$> get @(Map Text Atom)
      case maybeAtom of
        Nothing -> do
          atom <- liftIO $ internAtom d sName shouldCreate
          modify @(Map Text Atom) $ M.insert name atom
          return atom
        Just atom -> return atom

  FromAtom atom ->
    input >>= \d ->
      liftIO $ fromMaybe "" . map fromString <$> getAtomName d atom

  GetTransientFor w -> do
    d <- input @Display
    liftIO $ getTransientForHint d w

  GetSizeHints w -> do
    d <- input @Display
    liftIO $ getWMNormalHints d w

getRealName ::
  Member Property m =>
  -- | The Window we're interested in
  Window ->
  -- | The window's name
  Eff m Text
getRealName win = do
  netwmname <- getAtom False "_NET_WM_NAME"
  wmname <- getAtom False "WM_NAME"
  net_name <- fromString <$> getProperty 32 netwmname win
  name <- fromString <$> getProperty 32 wmname win
  return if nullOf text net_name then name else net_name
