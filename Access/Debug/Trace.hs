module Access.Debug.Trace
    ( module Debug.Trace

    , TraceAccess(..)
    ) where


import           Debug.Trace

import           Access.Core


class Access io => TraceAccess io where
    traceIO'        :: String -> io ()
    traceEventIO'   :: String -> io ()


instance TraceAccess IO where
    traceIO'      = traceIO
    traceEventIO' = traceEventIO
