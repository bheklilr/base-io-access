module Access.Control.Concurrent.QSemN
    ( module Control.Concurrent.QSemN

    , QSemNAccess(..)
    ) where


import Control.Concurrent.QSemN

import Access.Core


class Access io => QSemNAccess io where
    -- |Build a new 'QSemN' with a supplied initial quantity.
    --  The initial quantity must be at least 0.
    newQSemN'    :: Int -> io QSemN
    -- |Wait for the specified quantity to become available
    waitQSemN'   :: QSemN -> Int -> io ()
    -- |Signal that a given quantity is now available from the 'QSemN'.
    signalQSemN' :: QSemN -> Int -> io ()


instance QSemNAccess IO where
    newQSemN'    = newQSemN
    waitQSemN'   = waitQSemN
    signalQSemN' = signalQSemN
