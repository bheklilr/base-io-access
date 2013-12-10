{-# LANGUAGE Safe       #-}
{-# LANGUAGE RankNTypes #-}
module Access.Control.Concurrent
    ( module Control.Concurrent

    , module Access.Control.Concurrent.MVar
    , module Access.Control.Concurrent.Chan
    , module Access.Control.Concurrent.QSem
    , module Access.Control.Concurrent.QSemN

    , ThreadAccess(..)
    , BoundThreadAccess(..)
    , WeakThreadAccess(..)
    ) where


import Control.Concurrent
import System.Posix.Types (Fd)

import Access.System.Mem.Weak
import Access.Control.Exception
import Access.Control.Concurrent.MVar
import Access.Control.Concurrent.Chan
import Access.Control.Concurrent.QSem
import Access.Control.Concurrent.QSemN


class ExceptionAccess io => ThreadAccess io where
    myThreadId'         :: io ThreadId
    forkIO'             :: io () -> io ThreadId
    forkFinally'        :: io a -> (Either SomeException a -> io ()) -> io ThreadId
    forkIOWithUnmask'   :: ((forall a. io a -> io a) -> io ()) -> io ThreadId
    killThread'         :: ThreadId -> io ()
    throwTo'            :: Exception e => ThreadId -> e -> io ()
    throwTo' = Access.Control.Exception.throwTo'
    forkOn'             :: Int -> io () ->  io ThreadId
    forkOnWithUnmask'   :: Int -> ((forall a. io a -> io a) -> io ()) -> io ThreadId
    getNumCapabilities' :: io Int
    setNumCapabilities' :: Int -> io ()
    threadCapability'   :: ThreadId -> io (Int, Bool)
    yield'              :: io ()
    threadDelay'        :: Int -> io ()
    threadWaitRead'     :: Fd -> io ()
    threadWaitWrite'    :: Fd -> io ()


class ThreadAccess io => BoundThreadAccess io where
    forkOS'                 :: io () -> io ThreadId
    isCurrentThreadBound'   :: io Bool
    runInBoundThread'       :: io a -> io a
    runInUnboundThread'     :: io a -> io a


class WeakMemAccess io => WeakThreadAccess io where
    mkWeakThreadId' :: ThreadId -> io (Weak ThreadId)


instance ThreadAccess IO where
    myThreadId'         = myThreadId
    forkIO'             = forkIO
    forkFinally'        = forkFinally
    forkIOWithUnmask'   = forkIOWithUnmask
    killThread'         = killThread
    throwTo'            = throwTo
    forkOn'             = forkOn
    forkOnWithUnmask'   = forkOnWithUnmask
    getNumCapabilities' = getNumCapabilities
    setNumCapabilities' = setNumCapabilities
    threadCapability'   = threadCapability
    yield'              = yield
    threadDelay'        = threadDelay
    threadWaitRead'     = threadWaitRead
    threadWaitWrite'    = threadWaitWrite

instance BoundThreadAccess IO where
    forkOS'               = forkOS
    isCurrentThreadBound' = isCurrentThreadBound
    runInBoundThread'     = runInBoundThread
    runInUnboundThread'   = runInUnboundThread

instance WeakThreadAccess IO where
    mkWeakThreadId' = mkWeakThreadId
