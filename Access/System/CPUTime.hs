module Access.System.CPUTime
    ( module System.CPUTime

    , CPUTimeAccess(..)
    ) where


import System.CPUTime


class Monad io => CPUTimeAccess io where
    getCPUTime' :: io Integer


instance CPUTimeAccess IO where
    getCPUTime' = getCPUTime
