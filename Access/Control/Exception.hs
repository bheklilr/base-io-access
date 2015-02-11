{-# LANGUAGE RankNTypes #-}
module Access.Control.Exception
    ( module Control.Exception

    , ExceptionAccess(..)
    , MaskingAccess(..)
    ) where


import           Control.Concurrent (ThreadId)
import           Control.Exception

import           Access.Core

class Access io => ExceptionAccess io where
    throwIO'        :: Exception e => e -> io a
    ioError'        :: IOError -> io a
    throwTo'        :: Exception e => ThreadId -> e -> io ()
    catch'          :: Exception e => io a -> (e -> io a) -> io a
    catches'        :: io a -> [Handler a] -> io a
    catchJust'      :: Exception e => (e -> Maybe b) -> io a -> (b -> io a) -> io a
    handle'         :: Exception e => (e -> io a) -> io a -> io a
    handleJust'     :: Exception e => (e -> Maybe b) -> (b -> io a) -> io a -> io a
    try'            :: Exception e => io a -> io (Either e a)
    tryJust'        :: Exception e => (e -> Maybe b) -> io a -> io (Either b a)
    evaluate'       :: a -> io a
    bracket'        :: io a -> (a -> io b) -> (a -> io c) -> io c
    bracket_'       :: io a -> io b -> io c -> io c
    bracketOnError' :: io a -> (a -> io b) -> (a -> io c) -> io c
    finally'        :: io a -> io b -> io a
    onException'    :: io a -> io b -> io a


class ExceptionAccess io => MaskingAccess io where
    mask'                   :: ((forall a. io a -> io a) -> io b) -> io b
    mask_'                  :: io a -> io a
    uninterruptibleMask'    :: ((forall a. io a -> io a) -> io b) -> io b
    uninterruptibleMask_'   :: io a -> io a
    getMaskingState'        :: io MaskingState
    allowInterrupt'         :: io ()


instance ExceptionAccess IO where
    throwIO'        = throwIO
    ioError'        = ioError
    throwTo'        = throwTo
    catch'          = catch
    catches'        = catches
    catchJust'      = catchJust
    handle'         = handle
    handleJust'     = handleJust
    try'            = try
    tryJust'        = tryJust
    evaluate'       = evaluate
    bracket'        = bracket
    bracket_'       = bracket_
    bracketOnError' = bracketOnError
    finally'        = finally
    onException'    = onException

instance MaskingAccess IO where
    mask'                 = mask
    mask_'                = mask_
    uninterruptibleMask'  = uninterruptibleMask
    uninterruptibleMask_' = uninterruptibleMask_
    getMaskingState'      = getMaskingState
    allowInterrupt'       = allowInterrupt
