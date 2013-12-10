module Access.Control.Concurrent.MVar
    ( module Control.Concurrent.MVar

    , MVarAccess(..)
    , WeakMVarAccess(..)
    ) where


import Control.Concurrent.MVar

import Access.System.Mem.Weak

class Monad io => MVarAccess io where
    newEmptyMVar'       :: io (MVar a)
    newMVar'            :: a -> io (MVar a)
    takeMVar'           :: MVar a -> io a
    putMVar'            :: MVar a -> a -> io ()
    readMVar'           :: MVar a -> io a
    swapMVar'           :: MVar a -> a -> io a
    tryTakeMVar'        :: MVar a -> io (Maybe a)
    tryPutMVar'         :: MVar a -> a -> io Bool
    isEmptyMVar'        :: MVar a -> io Bool
    withMVar'           :: MVar a -> (a -> IO b) -> io b
    modifyMVar_'        :: MVar a -> (a -> IO a) -> io ()
    modifyMVar'         :: MVar a -> (a -> IO (a, b)) -> io b
    modifyMVarMasked_'  :: MVar a -> (a -> IO a) -> io ()
    modifyMVarMasked'   :: MVar a -> (a -> IO (a, b)) -> io b

class (WeakMemAccess io, MVarAccess io) => WeakMVarAccess io where
    mkWeakMVar' :: MVar a -> IO () -> io (Weak (MVar a))


instance MVarAccess IO where
    newEmptyMVar'      = newEmptyMVar
    newMVar'           = newMVar
    takeMVar'          = takeMVar
    putMVar'           = putMVar
    readMVar'          = readMVar
    swapMVar'          = swapMVar
    tryTakeMVar'       = tryTakeMVar
    tryPutMVar'        = tryPutMVar
    isEmptyMVar'       = isEmptyMVar
    withMVar'          = withMVar
    modifyMVar_'       = modifyMVar_
    modifyMVar'        = modifyMVar
    modifyMVarMasked_' = modifyMVarMasked_
    modifyMVarMasked'  = modifyMVarMasked

instance WeakMVarAccess IO where
    mkWeakMVar' = mkWeakMVar
