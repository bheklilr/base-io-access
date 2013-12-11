module Access.System.Timeout
    ( module System.Timeout

    , TimeoutAccess(..)
    ) where


import System.Timeout

import Access.Core


class Access io => TimeoutAccess io where
    timeout' :: Int -> IO a -> io (Maybe a)


instance TimeoutAccess IO where
    timeout' = timeout
