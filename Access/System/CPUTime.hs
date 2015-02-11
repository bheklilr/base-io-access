module Access.System.CPUTime
    ( module System.CPUTime

    , CPUTimeAccess(..)
    ) where


import           System.CPUTime

import           Access.Core


class Access io => CPUTimeAccess io where
    getCPUTime' :: io Integer


instance CPUTimeAccess IO where
    getCPUTime' = getCPUTime
