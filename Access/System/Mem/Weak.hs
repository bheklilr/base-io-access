module Access.System.Mem.Weak
    ( module System.Mem.Weak

    , WeakMemAccess(..)
    ) where


import System.Mem.Weak


class Monad io => WeakMemAccess io where
    mkWeak'     :: k -> v -> Maybe (IO ()) -> io (Weak v)
    deRefWeak'  :: Weak v -> io (Maybe v)
    finalize'   :: Weak v -> io ()


instance WeakMemAccess IO where
    mkWeak'    = mkWeak
    deRefWeak' = deRefWeak
    finalize'  = finalize
