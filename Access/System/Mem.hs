module Access.System.Mem
    ( module System.Mem

    , MemAccess(..)
    ) where


import System.Mem


class Monad io => MemAccess io where
    performGC' :: io ()


instance MemAccess IO where
    performGC' = performGC
