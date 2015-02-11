module Access.System.Exit
    ( module System.Exit

    , ExitAccess(..)
    ) where


import           System.Exit

import           Access.Core


class Access io => ExitAccess io where
    -- | Computation 'exitWith' @code@ throws 'ExitCode' @code@.
    -- Normally this terminates the program, returning @code@ to the
    -- program's caller.
    --
    -- On program termination, the standard 'Handle's 'stdout' and
    -- 'stderr' are flushed automatically; any other buffered 'Handle's
    -- need to be flushed manually, otherwise the buffered data will be
    -- discarded.
    --
    -- A program that fails in any other way is treated as if it had
    -- called 'exitFailure'.
    -- A program that terminates successfully without calling 'exitWith'
    -- explicitly is treated as it it had called 'exitWith' 'ExitSuccess'.
    --
    -- As an 'ExitCode' is not an 'IOError', 'exitWith' bypasses
    -- the error handling in the 'IO' monad and cannot be intercepted by
    -- 'catch' from the "Prelude".  However it is a 'SomeException', and can
    -- be caught using the functions of "Control.Exception".  This means
    -- that cleanup computations added with 'Control.Exception.bracket'
    -- (from "Control.Exception") are also executed properly on 'exitWith'.
    --
    -- Note: in GHC, 'exitWith' should be called from the main program
    -- thread in order to exit the process.  When called from another
    -- thread, 'exitWith' will throw an 'ExitException' as normal, but the
    -- exception will not cause the process itself to exit.
    --
    exitWith'       :: ExitCode -> io a
    -- | The computation 'exitFailure' is equivalent to
    -- 'exitWith' @(@'ExitFailure' /exitfail/@)@,
    -- where /exitfail/ is implementation-dependen
    exitFailure'    :: io a
    -- | The computation 'exitSuccess' is equivalent to
    -- 'exitWith' 'ExitSuccess', It terminates the program
    -- successfully.
    exitSuccess'    :: io a


instance ExitAccess IO where
    exitWith'    = exitWith
    exitFailure' = exitFailure
    exitSuccess' = exitSuccess
