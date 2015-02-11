{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
-- |
-- Module       :   Access.Control.Concurrent
-- Copyright    :   (c) Aaron Stevens, 2014
-- License      :   GPL2
--
-- Maintainer   :   bheklilr2@gmail.com
--------------------------------------------------------------------------------

module Access.Control.Concurrent
    ( module Control.Concurrent

    , module Access.Control.Concurrent.MVar
    , module Access.Control.Concurrent.Chan

    , ThreadAccess(..)
    , BoundThreadAccess(..)
    , WeakThreadAccess(..)
    ) where


import           Control.Concurrent
import           System.Posix.Types             (Fd)

import           Access.Control.Concurrent.Chan
import           Access.Control.Concurrent.MVar
import           Access.Control.Exception
import           Access.System.Mem.Weak


-- | Inherits from 'ExceptionAccess', and gives access to Thread related functions
class ExceptionAccess io => ThreadAccess io where
    -- | Wraps 'Control.Concurrent.myThreadId'
    --
    -- Returns the 'ThreadId' of the calling thread (GHC only)
    myThreadId'         :: io ThreadId
    -- | Wraps 'Control.Concurrent.forkIO'
    --
    -- Sparks off a new thread to run the 'IO' computation passed as the
    -- first argument, and returns the 'ThreadId' of the newly created
    -- thread.
    --
    -- The new thread will be a lightweight thread; if you want to use a foreign
    -- library that uses thread-local storage, use 'forkOS'' instead.
    --
    -- GHC note: the new thread inherits the /masked/ state of the parent
    -- (see 'Access.Control.Exception.mask'').
    --
    -- The newly created thread has an exception handler that discards the
    -- exceptions 'BlockedIndefinitelyOnMVar', 'BlockedIndefinitelyOnSTM', and
    -- 'ThreadKilled', and passes all other exceptions to the uncaught
    -- exception handler.
    forkIO'             :: io () -> io ThreadId
    -- | Wraps 'Control.Concurrent.forkFinally'
    --
    -- Fork a thread and call the supplied function when the thread is about
    -- to terminate, with an exception or a returned value.  The function is
    -- called with asynchronous exceptions masked.
    --
    -- > forkFinally' action and_then =
    -- >   mask $ \restore ->
    -- >     forkIO' $ try (restore action) >>= and_then
    --
    -- This function is useful for informing the parent when a child
    -- terminates, for example.
    forkFinally'        :: io a -> (Either SomeException a -> io ()) -> io ThreadId
    -- | Wraps 'Control.Concurrent.forkIOWithUnmask'
    --
    -- Like 'forkIO'', but the child thread is passed a function that can
    -- be used to unmask asynchronous exceptions.  This function is
    -- typically used in the following way
    --
    -- >  ... mask_ $ forkIOWithUnmask' $ \unmask ->
    -- >                 catch' (unmask ...) handler
    --
    -- so that the exception handler in the child thread is established
    -- with asynchronous exceptions masked, meanwhile the main body of
    -- the child thread is executed in the unmasked state.
    --
    -- Note that the unmask function passed to the child thread should
    -- only be used in that thread; the behaviour is undefined if it is
    -- invoked in a different thread.
    forkIOWithUnmask'   :: ((forall a. io a -> io a) -> io ()) -> io ThreadId
    -- | Wraps 'Control.Concurrent.killThread'
    --
    -- 'killThread'' raises the 'ThreadKilled' exception in the given
    -- thread (GHC only).
    --
    -- > killThread' tid = throwTo' tid ThreadKilled
    killThread'         :: ThreadId -> io ()
    -- | Wraps 'Control.Concurrent.throwTo'
    --
    -- 'throwTo'' raises an arbitrary exception in the target thread (GHC only).
    --
    -- 'throwTo'' does not return until the exception has been raised in the
    -- target thread.
    -- The calling thread can thus be certain that the target
    -- thread has received the exception.  This is a useful property to know
    -- when dealing with race conditions: eg. if there are two threads that
    -- can kill each other, it is guaranteed that only one of the threads
    -- will get to kill the other.
    --
    -- Whatever work the target thread was doing when the exception was
    -- raised is not lost: the computation is suspended until required by
    -- another thread.
    --
    -- If the target thread is currently making a foreign call, then the
    -- exception will not be raised (and hence 'throwTo'' will not return)
    -- until the call has completed.  This is the case regardless of whether
    -- the call is inside a 'mask'' or not.  However, in GHC a foreign call
    -- can be annotated as @interruptible@, in which case a 'throwTo'' will
    -- cause the RTS to attempt to cause the call to return; see the GHC
    -- documentation for more details.
    --
    -- Important note: the behaviour of 'throwTo'' differs from that described in
    -- the paper \"Asynchronous exceptions in Haskell\"
    -- (<http://research.microsoft.com/~simonpj/Papers/asynch-exns.htm>).
    -- In the paper, 'throwTo'' is non-blocking; but the library implementation
    -- adopts a more synchronous design in which 'throwTo'' does not return
    -- until the exception is received by the target thread.  The trade-off is
    -- discussed in Section 9 of the paper. Like any blocking operation,
    -- 'throwTo'' is therefore interruptible (see Section 5.3 of the paper).
    -- Unlike other interruptible operations, however, 'throwTo'' is /always/
    -- interruptible, even if it does not actually block.
    --
    -- There is no guarantee that the exception will be delivered promptly,
    -- although the runtime will endeavour to ensure that arbitrary
    -- delays don't occur.  In GHC, an exception can only be raised when a
    -- thread reaches a /safe point/, where a safe point is where memory
    -- allocation occurs.  Some loops do not perform any memory allocation
    -- inside the loop and therefore cannot be interrupted by a 'throwTo''.
    --
    -- If the target of 'throwTo'' is the calling thread, then the behaviour
    -- is the same as 'Access.Control.Exception.throwIO'', except that the
    -- exception is thrown as an asynchronous exception.  This means that if
    -- there is an enclosing pure computation, which would be the case if the
    -- current IO operation is inside 'unsafePerformIO' or 'unsafeInterleaveIO',
    -- that computation is not permanently replaced by the exception, but is
    -- suspended as if it had received an asynchronous exception.
    --
    -- Note that if 'throwTo'' is called with the current thread as the
    -- target, the exception will be thrown even if the thread is currently
    -- inside 'mask'' or 'uninterruptibleMask''.
    throwTo'            :: Exception e => ThreadId -> e -> io ()
    throwTo' = Access.Control.Exception.throwTo'
    -- | Wraps 'Control.Concurrent.forkOn'
    --
    -- Like ''forkIO'', but lets you specify on which processor the thread
    -- should run.  Unlike a `'forkIO'` thread, a thread created by `forkOn'`
    -- will stay on the same processor for its entire lifetime (`'forkIO'`
    -- threads can migrate between processors according to the scheduling
    -- policy).  `forkOn'` is useful for overriding the scheduling policy when
    -- you know in advance how best to distribute the threads.
    --
    -- The `Int` argument specifies a /capability number/ (see
    -- 'getNumCapabilities'').  Typically capabilities correspond to physical
    -- processors, but the exact behaviour is implementation-dependent.  The
    -- value passed to 'forkOn'' is interpreted modulo the total number of
    -- capabilities as returned by 'getNumCapabilities''.
    --
    -- GHC note: the number of capabilities is specified by the @+RTS -N@
    -- option when the program is started.  Capabilities can be fixed to
    -- actual processor cores with @+RTS -qa@ if the underlying operating
    -- system supports that, although in practice this is usually unnecessary
    -- (and may actually degrade perforamnce in some cases - experimentation
    -- is recommended).
    forkOn'             :: Int -> io () ->  io ThreadId
    -- | Wraps 'Control.Concurrent.forkOnWithUnmask'
    --
    -- Like 'forkIOWithUnmask'', but the child thread is pinned to the
    -- given CPU, as with 'forkOn''.
    forkOnWithUnmask'   :: Int -> ((forall a. io a -> io a) -> io ()) -> io ThreadId
    -- | Wraps 'Control.Concurrent.getNumCapabilities'
    --
    -- Returns the number of Haskell threads that can run truly
    -- simultaneously (on separate physical processors) at any given time.  To
    -- change this value, use 'setNumCapabilities''.
    getNumCapabilities' :: io Int
    -- | Wraps 'Control.Concurrent.setNumCapabilities'
    --
    -- Set the number of Haskell threads that can run truly simultaneously
    -- (on separate physical processors) at any given time.  The number
    -- passed to `forkOn'` is interpreted modulo this value.  The initial
    -- value is given by the @+RTS -N@ runtime flag.
    --
    -- This is also the number of threads that will participate in parallel
    -- garbage collection.  It is strongly recommended that the number of
    -- capabilities is not set larger than the number of physical processor
    -- cores, and it may often be beneficial to leave one or more cores free
    -- to avoid contention with other processes in the machine.
    setNumCapabilities' :: Int -> io ()
    -- | Wraps 'Control.Concurrent.threadCapability'
    --
    -- Returns the number of the capability on which the thread is currently
    -- running, and a boolean indicating whether the thread is locked to
    -- that capability or not.  A thread is locked to a capability if it
    -- was created with @forkOn'@.
    threadCapability'   :: ThreadId -> io (Int, Bool)
    -- | Wraps 'Control.Concurrent.yield'
    --
    -- The 'yield'' action allows (forces, in a co-operative multitasking
    -- implementation) a context-switch to any other currently runnable
    -- threads (if any), and is occasionally useful when implementing
    -- concurrency abstractions.
    yield'              :: io ()
    -- | Wraps 'Control.Concurrent.threadDelay'
    --
    -- Suspends the current thread for a given number of microseconds
    -- (GHC only).
    --
    -- There is no guarantee that the thread will be rescheduled promptly
    -- when the delay has expired, but the thread will never continue to
    -- run /earlier/ than specified.
    threadDelay'        :: Int -> io ()
    -- | Wraps 'Control.Concurrent.threadWaitRead'
    --
    -- Block the current thread until data is available to read on the
    -- given file descriptor (GHC only).
    --
    -- This will throw an 'IOError' if the file descriptor was closed
    -- while this thread was blocked.  To safely close a file descriptor
    -- that has been used with 'threadWaitRead', use
    -- 'GHC.Conc.closeFdWith'.
    threadWaitRead'     :: Fd -> io ()
    -- | Wraps 'Control.Concurrent.threadWaitWrite'
    --
    -- Block the current thread until data can be written to the
    -- given file descriptor (GHC only).
    --
    -- This will throw an 'IOError' if the file descriptor was closed
    -- while this thread was blocked.  To safely close a file descriptor
    -- that has been used with 'threadWaitWrite', use
    -- 'GHC.Conc.closeFdWith'.
    threadWaitWrite'    :: Fd -> io ()


-- | Provides access to bounded thread functions
class ThreadAccess io => BoundThreadAccess io where
    -- | Wraps 'Control.Concurrent.forkOS'
    --
    -- Like 'forkIO'', this sparks off a new thread to run the 'IO'
    -- computation passed as the first argument, and returns the 'ThreadId'
    -- of the newly created thread.
    --
    -- However, 'forkOS'' creates a /bound/ thread, which is necessary if you
    -- need to call foreign (non-Haskell) libraries that make use of
    -- thread-local state, such as OpenGL (see "Control.Concurrent#boundthreads").
    --
    -- Using 'forkOS'' instead of 'forkIO'' makes no difference at all to the
    -- scheduling behaviour of the Haskell runtime system.  It is a common
    -- misconception that you need to use 'forkOS'' instead of 'forkIO'' to
    -- avoid blocking all the Haskell threads when making a foreign call;
    -- this isn't the case.  To allow foreign calls to be made without
    -- blocking all the Haskell threads (with GHC), it is only necessary to
    -- use the @-threaded@ option when linking your program, and to make sure
    -- the foreign import is not marked @unsafe@.
    forkOS'                 :: io () -> io ThreadId
    -- | Wraps 'Control.Concurrent.isCurrentThreadBound'
    --
    -- Returns 'True' if the calling thread is /bound/, that is, if it is
    -- safe to use foreign libraries that rely on thread-local state from the
    -- calling thread.
    isCurrentThreadBound'   :: io Bool
    -- | Wraps 'Control.Concurrent.runInBoundThread'
    --
    -- Run the 'IO' computation passed as the first argument. If the calling
    -- thread is not /bound/, a bound thread is created temporarily.
    -- @runInBoundThread'@ doesn't finish until the 'IO' computation finishes.
    --
    -- You can wrap a series of foreign function calls that rely on thread-local
    -- state with @runInBoundThread'@ so that you can use them without knowing
    -- whether the current thread is /bound/.
    runInBoundThread'       :: io a -> io a
    -- | Wraps 'Control.Concurrent.runInUnboundThread'
    --
    --Run the 'IO' computation passed as the first argument. If the calling
    -- thread is /bound/, an unbound thread is created temporarily using
    -- 'forkIO''. @runInBoundThread'@ doesn't finish until the 'IO' computation
    -- finishes.
    --
    -- Use this function /only/ in the rare case that you have actually observed
    -- a performance loss due to the use of bound threads. A program that
    -- doesn't need it's main thread to be bound and makes /heavy/ use of
    -- concurrency (e.g. a web server), might want to wrap it's @main@ action in
    -- @runInUnboundThread'@.
    --
    -- Note that exceptions which are thrown to the current thread are thrown in
    -- turn to the thread that is executing the given computation. This ensures
    -- there's always a way of killing the forked thread.
    runInUnboundThread'     :: io a -> io a


-- | Provides access to the 'mkWeakThreadId' function, inherits from 'WeakMemAccess'
class WeakMemAccess io => WeakThreadAccess io where
    -- | Wraps 'Control.Concurrent.mkWeakThreadId'
    --
    -- Make a weak pointer to a 'ThreadId'.  It can be important to do
    -- this if you want to hold a reference to a 'ThreadId' while still
    -- allowing the thread to receive the @BlockedIndefinitely@ family of
    -- exceptions (e.g. 'BlockedIndefinitelyOnMVar').  Holding a normal
    -- 'ThreadId' reference will prevent the delivery of
    -- @BlockedIndefinitely@ exceptions because the reference could be
    -- used as the target of 'throwTo'' at any time, which would unblock
    -- the thread.
    --
    -- Holding a @Weak ThreadId@, on the other hand, will not prevent the
    -- thread from receiving @BlockedIndefinitely@ exceptions.  It is
    -- still possible to throw an exception to a @Weak ThreadId@, but the
    -- caller must use @deRefWeak'@ first to determine whether the thread
    -- still exists.
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
