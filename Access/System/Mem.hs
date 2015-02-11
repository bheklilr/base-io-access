module Access.System.Mem
    ( module System.Mem

    , MemAccess(..)
    ) where


import           System.Mem

import           Access.Core


class Access io => MemAccess io where
    performGC' :: io ()


instance MemAccess IO where
    performGC' = performGC
