module Access.System.CPUTime
    ( module System.CPUTime

    , CPUTimeAccess(..)
    ) where


import           System.CPUTime

import           Access.Core


class Access io => CPUTimeAccess io where
    -- |Computation 'getCPUTime' returns the number of picoseconds CPU time
    -- used by the current program.  The precision of this result is
    -- implementation-dependent.
    getCPUTime' :: io Integer


instance CPUTimeAccess IO where
    getCPUTime' = getCPUTime
