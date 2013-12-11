module Access.System.IO.Error
    ( module System.IO.Error

    , IOErrorAccess(..)
    ) where


import System.IO.Error

import Access.Core


class Access io => IOErrorAccess io where
    ioError'        :: IOError -> io a
    catchIOError'   :: io a -> (IOError -> io a) -> io a
    tryIOError'     :: io a -> io (Either IOError a)
    modifyIOError'  :: (IOError -> IOError) -> io a -> io a


instance IOErrorAccess IO where
    ioError'       = ioError
    catchIOError'  = catchIOError
    tryIOError'    = tryIOError
    modifyIOError' = modifyIOError
