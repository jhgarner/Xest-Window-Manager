{-# LANGUAGE UndecidableInstances #-}
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
import           Graphics.X11.Types
import           Graphics.X11.Xlib.Types
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Atom
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import qualified Data.Map                      as M
import Base.Helpers
import Base.Executor


-- |X11 defines properties as a dictionary like object from Atoms and windows
-- to bits. This effect allows you to read or write to those dictionaries.
--
-- It's worth noting that these properties are very untyped so be careful
-- with what you pass in and try to get out.
class Property m where
  -- |Gets a property from a window's dictionary
  getProperty :: Storable a
              => Int -- ^ The number of bits in the property
              -> Atom  -- ^ The property key
              -> Window -- ^ The window who's properties we want to look at
              -> m [a] -- ^ A list of values found at the key

  -- |Writes to a property in a window's dictionary
  putProperty :: Int -- ^ The number of bits in the property
              -> Atom -- ^ The property key we want to write to
              -> Window  -- ^ The window who's properties we want to look at
              -> Atom -- ^ A string representing the type of the data
              -> [Int] -- The data we want to write
              -> m ()

  -- |Although most properties are only loosely defined, the class property
  -- is built into Xlib for some reason.
  getClassName :: Window -- ^ The Window we're interested in
               -> m Text -- ^ The window's class

  -- |Technically not a property but it kind of fits...
  getChildX :: Window -- ^ The parent
           -> m (Maybe Window) -- ^ The child
  isOverrideRedirect :: Window
                     -> m Bool

  -- |Turns a Text into an X11 managed Atom. Think of atoms as pointers to
  -- strings.
  getAtom :: Bool -- ^ Should we not create it if it doesn't exist?
          -> Text -- ^ The atom's name
          -> m Atom -- ^ The atom created/retrieved from X11

  -- |Gives you the name of some Atom.
  fromAtom :: Atom
           -> m Text

  -- |Check if a window is transient for something else based on the
  -- ICCM protocol.
  getTransientFor :: Window -- ^ The window being analyzed
                  -> m (Maybe Window) -- ^ The parent window
  
  getSizeHints :: Window -- ^ The window being analyzed
               -> m SizeHints
  
type AtomCache = Map Text Atom
type RootPropCache = Map Atom [Int]

-- |Runs a propertiy using IO
instance Members (Executor ': MonadIO ': States [AtomCache, RootPropCache] ++ Inputs '[RootWindow, Display]) m => Property m where
  getProperty i atom win = input >>= \d -> liftIO $
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
    input >>= \d -> liftIO $
      either (const False) wa_override_redirect <$> try @SomeException (getWindowAttributes d win)

  getChildX win = do
    display <- input @Display
    -- For some reason, xQueryTree hasn't been abstracted at all
    liftIO $ alloca
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

  getAtom shouldCreate name@(Text sName) = input >>= \d -> do
    maybeAtom <- (M.!? name) <$> get
    case maybeAtom of
      Nothing -> do
        atom <- liftIO $ internAtom d sName shouldCreate
        modify $ M.insert name atom
        return atom
      Just atom -> return atom

  fromAtom atom = input >>= \d ->
    liftIO $ fromMaybe "" . map fromString <$> getAtomName d atom

  getTransientFor w = do
    d <- input @Display
    liftIO $ getTransientForHint d w

  getSizeHints w = do
    d <- input @Display
    liftIO $ getWMNormalHints d w



getRealName :: (Monad m, Property m) => Window -- ^ The Window we're interested in
            -> m Text -- ^ The window's name
getRealName win = do
  netwmname <- getAtom False "_NET_WM_NAME"
  wmname <- getAtom False "WM_NAME"
  net_name <- fromString <$> getProperty 32 netwmname win
  name <- fromString <$> getProperty 32 wmname win
  return if nullOf text net_name then name else net_name
