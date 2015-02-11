module Access.System.Environment
    ( module System.Environment

    , EnvironmentAccess(..)
    ) where


import           System.Environment

import           Access.Core


class Access io => EnvironmentAccess io where
    getArgs'            :: io [String]
    getProgName'        :: io String
    getExecutablePath'  :: io FilePath
    getEnv'             :: String -> io String
    lookupEnv'          :: String -> io (Maybe String)
    setEnv'             :: String -> String -> io ()
    unsetEnv'           :: String -> io ()
    withArgs'           :: [String] -> IO a -> io a
    withProgName'       :: String -> IO a -> io a
    getEnvironment'     :: io [(String, String)]


instance EnvironmentAccess IO where
    getArgs'           = getArgs
    getProgName'       = getProgName
    getExecutablePath' = getExecutablePath
    getEnv'            = getEnv
    lookupEnv'         = lookupEnv
    setEnv'            = setEnv
    unsetEnv'          = unsetEnv
    withArgs'          = withArgs
    withProgName'      = withProgName
    getEnvironment'    = getEnvironment
