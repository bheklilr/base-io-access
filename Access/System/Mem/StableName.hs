module Access.System.Mem.StableName
    ( module System.Mem.StableName

    , StableNameAccess(..)
    ) where


import           System.Mem.StableName

import           Access.Core


class Access io => StableNameAccess io where
    makeStableName' :: a -> io (StableName a)


instance StableNameAccess IO where
    makeStableName' = makeStableName
