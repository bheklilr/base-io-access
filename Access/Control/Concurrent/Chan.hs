module Access.Control.Concurrent.Chan
    ( module Control.Concurrent.Chan

    , ChanAccess(..)
    ) where


import           Control.Concurrent.Chan

import           Access.Core


class Access io => ChanAccess io where
    newChan'            :: io (Chan a)
    writeChan'          :: Chan a -> a -> io ()
    readChan'           :: Chan a -> io a
    dupChan'            :: Chan a -> io (Chan a)
    getChanContents'    :: Chan a -> io [a]
    writeList2Chan'     :: Chan a -> [a] -> io ()


instance ChanAccess IO where
    newChan'         = newChan
    writeChan'       = writeChan
    readChan'        = readChan
    dupChan'         = dupChan
    getChanContents' = getChanContents
    writeList2Chan'  = writeList2Chan
