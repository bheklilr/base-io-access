module Access.Control.Concurrent.QSem
    ( module Control.Concurrent.QSem

    , QSemAccess(..)
    ) where


import Control.Concurrent.QSem

import Access.Core


class Access io => QSemAccess io where
    -- |Build a new 'QSem' with a supplied initial quantity.
    --  The initial quantity must be at least 0.
    newQSem'    :: Int -> io QSem
    -- |Wait for a unit to become available
    waitQSem'   :: QSem -> io ()
    -- |Signal that a unit of the 'QSem' is available
    signalQSem' :: QSem -> io ()


instance QSemAccess IO where
    newQSem'    = newQSem
    waitQSem'   = waitQSem
    signalQSem' = signalQSem
