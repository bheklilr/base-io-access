module Access.System.Mem
    ( module System.Mem

    , MemAccess(..)
    ) where


import           System.Mem

import           Access.Core


class Access io => MemAccess io where
    -- | Triggers an immediate garbage collection.
    performGC'      :: io ()
    -- | Triggers an immediate garbage collection.
    --
    -- /Since: 4.7.0.0/
    performMajorGC' :: io ()
    -- | Triggers an immediate minor garbage collection.
    --
    -- /Since: 4.7.0.0/
    performMinorGC' :: io ()


instance MemAccess IO where
    performGC' = performGC
    performMajorGC' = performMajorGC
    performMinorGC' = performMinorGC
