module Access.Data.IORef
    ( module Data.IORef

    , IORefAccess(..)
    , WeakIORefAccess(..)
    ) where


import Data.IORef

import Access.Core
import Access.System.Mem.Weak


class Access io => IORefAccess io where
    newIORef'           :: a -> io (IORef a)
    readIORef'          :: IORef a -> io a
    writeIORef'         :: IORef a -> a -> io ()
    modifyIORef_        :: IORef a -> (a -> a) -> io ()
    modifyIORef_'       :: IORef a -> (a -> a) -> io ()
    atomicModifyIORef_  :: IORef a -> (a -> (a, b)) -> io b
    atomicModifyIORef_' :: IORef a -> (a -> (a, b)) -> io b
    atomicWriteIORef'   :: IORef a -> a -> io ()


class (WeakMemAccess io, IORefAccess io) => WeakIORefAccess io where
    mkWeakIORef'    :: IORef a -> IO () -> io (Weak (IORef a))


instance IORefAccess IO where
    newIORef'           = newIORef
    readIORef'          = readIORef
    writeIORef'         = writeIORef
    modifyIORef_        = modifyIORef
    modifyIORef_'       = modifyIORef'
    atomicModifyIORef_  = atomicModifyIORef
    atomicModifyIORef_' = atomicModifyIORef'
    atomicWriteIORef'   = atomicWriteIORef

instance WeakIORefAccess IO where
    mkWeakIORef' = mkWeakIORef
