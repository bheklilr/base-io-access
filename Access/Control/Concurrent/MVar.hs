module Access.Control.Concurrent.MVar
    ( module Control.Concurrent.MVar

    , MVarAccess(..)
    , WeakMVarAccess(..)
    ) where


import           Control.Concurrent.MVar

import           Access.Core
import           Access.System.Mem.Weak

class Access io => MVarAccess io where
    -- |Create an 'MVar' which is initially empty.
    newEmptyMVar'       :: io (MVar a)
    -- |Create an 'MVar' which contains the supplied value.
    newMVar'            :: a -> io (MVar a)
    -- |Return the contents of the 'MVar'.  If the 'MVar' is currently
    -- empty, 'takeMVar' will wait until it is full.  After a 'takeMVar',
    -- the 'MVar' is left empty.
    --
    -- There are two further important properties of 'takeMVar':
    --
    --   * 'takeMVar' is single-wakeup.  That is, if there are multiple
    --     threads blocked in 'takeMVar', and the 'MVar' becomes full,
    --     only one thread will be woken up.  The runtime guarantees that
    --     the woken thread completes its 'takeMVar' operation.
    --
    --   * When multiple threads are blocked on an 'MVar', they are
    --     woken up in FIFO order.  This is useful for providing
    --     fairness properties of abstractions built using 'MVar's.
    --
    takeMVar'           :: MVar a -> io a
    -- |Put a value into an 'MVar'.  If the 'MVar' is currently full,
    -- 'putMVar' will wait until it becomes empty.
    --
    -- There are two further important properties of 'putMVar':
    --
    --   * 'putMVar' is single-wakeup.  That is, if there are multiple
    --     threads blocked in 'putMVar', and the 'MVar' becomes empty,
    --     only one thread will be woken up.  The runtime guarantees that
    --     the woken thread completes its 'putMVar' operation.
    --
    --   * When multiple threads are blocked on an 'MVar', they are
    --     woken up in FIFO order.  This is useful for providing
    --     fairness properties of abstractions built using 'MVar's.
    --
    putMVar'            :: MVar a -> a -> io ()
    -- |Atomically read the contents of an 'MVar'.  If the 'MVar' is
    -- currently empty, 'readMVar' will wait until its full.
    -- 'readMVar' is guaranteed to receive the next 'putMVar'.
    --
    -- 'readMVar' is multiple-wakeup, so when multiple readers are
    -- blocked on an 'MVar', all of them are woken up at the same time.
    --
    -- /Compatibility note:/ Prior to base 4.7, 'readMVar' was a combination
    -- of 'takeMVar' and 'putMVar'.  This mean that in the presence of
    -- other threads attempting to 'putMVar', 'readMVar' could block.
    -- Furthermore, 'readMVar' would not receive the next 'putMVar' if there
    -- was already a pending thread blocked on 'takeMVar'.  The old behavior
    -- can be recovered by implementing 'readMVar as follows:
    --
    -- @
    --  readMVar :: MVar a -> IO a
    --  readMVar m =
    --    mask_ $ do
    --      a <- takeMVar m
    --      putMVar m a
    --      return a
    -- @
    readMVar'           :: MVar a -> io a
    {-|
      Take a value from an 'MVar', put a new value into the 'MVar' and
      return the value taken. This function is atomic only if there are
      no other producers for this 'MVar'.
    -}
    swapMVar'           :: MVar a -> a -> io a
    -- |A non-blocking version of 'takeMVar'.  The 'tryTakeMVar' function
    -- returns immediately, with 'Nothing' if the 'MVar' was empty, or
    -- @'Just' a@ if the 'MVar' was full with contents @a@.  After 'tryTakeMVar',
    -- the 'MVar' is left empty.
    tryTakeMVar'        :: MVar a -> io (Maybe a)
    -- |A non-blocking version of 'putMVar'.  The 'tryPutMVar' function
    -- attempts to put the value @a@ into the 'MVar', returning 'True' if
    -- it was successful, or 'False' otherwise.
    tryPutMVar'         :: MVar a -> a -> io Bool
    -- |A non-blocking version of 'readMVar'.  The 'tryReadMVar' function
    -- returns immediately, with 'Nothing' if the 'MVar' was empty, or
    -- @'Just' a@ if the 'MVar' was full with contents @a@.
    --
    -- /Since: 4.7.0.0/
    tryReadMVar'        :: MVar a -> io (Maybe a)
    -- |Check whether a given 'MVar' is empty.
    --
    -- Notice that the boolean value returned  is just a snapshot of
    -- the state of the MVar. By the time you get to react on its result,
    -- the MVar may have been filled (or emptied) - so be extremely
    -- careful when using this operation.   Use 'tryTakeMVar' instead if possible.
    isEmptyMVar'        :: MVar a -> io Bool
    {-|
      'withMVar' is an exception-safe wrapper for operating on the contents
      of an 'MVar'.  This operation is exception-safe: it will replace the
      original contents of the 'MVar' if an exception is raised (see
      "Control.Exception").  However, it is only atomic if there are no
      other producers for this 'MVar'.
    -}
    withMVar'           :: MVar a -> (a -> IO b) -> io b
    {-|
      Like 'withMVar', but the @IO@ action in the second argument is executed
      with asynchronous exceptions masked.

      /Since: 4.7.0.0/
    -}
    withMVarMasked'     :: MVar a -> (a -> IO b) -> io b
    {-|
      An exception-safe wrapper for modifying the contents of an 'MVar'.
      Like 'withMVar', 'modifyMVar' will replace the original contents of
      the 'MVar' if an exception is raised during the operation.  This
      function is only atomic if there are no other producers for this
      'MVar'.
    -}
    modifyMVar_'        :: MVar a -> (a -> IO a) -> io ()
    {-|
      A slight variation on 'modifyMVar_' that allows a value to be
      returned (@b@) in addition to the modified value of the 'MVar'.
    -}
    modifyMVar'         :: MVar a -> (a -> IO (a, b)) -> io b
    {-|
      Like 'modifyMVar_', but the @IO@ action in the second argument is executed with
      asynchronous exceptions masked.

      /Since: 4.6.0.0/
    -}
    modifyMVarMasked_'  :: MVar a -> (a -> IO a) -> io ()
    {-|
      Like 'modifyMVar', but the @IO@ action in the second argument is executed with
      asynchronous exceptions masked.

      /Since: 4.6.0.0/
    -}
    modifyMVarMasked'   :: MVar a -> (a -> IO (a, b)) -> io b

class (WeakMemAccess io, MVarAccess io) => WeakMVarAccess io where
    -- | Make a 'Weak' pointer to an 'MVar', using the second argument as
    -- a finalizer to run when 'MVar' is garbage-collected
    --
    -- /Since: 4.6.0.0/
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
    tryReadMVar'       = tryReadMVar
    isEmptyMVar'       = isEmptyMVar
    withMVar'          = withMVar
    withMVarMasked'    = withMVarMasked
    modifyMVar_'       = modifyMVar_
    modifyMVar'        = modifyMVar
    modifyMVarMasked_' = modifyMVarMasked_
    modifyMVarMasked'  = modifyMVarMasked

instance WeakMVarAccess IO where
    mkWeakMVar' = mkWeakMVar
