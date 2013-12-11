module Access.System.Exit
    ( module System.Exit

    , ExitAccess(..)
    ) where


import System.Exit

import Access.Core


class Access io => ExitAccess io where
    exitWith'       :: ExitCode -> io a
    exitFailure'    :: io a
    exitSuccess'    :: io a


instance ExitAccess IO where
    exitWith'    = exitWith
    exitFailure' = exitFailure
    exitSuccess' = exitSuccess
