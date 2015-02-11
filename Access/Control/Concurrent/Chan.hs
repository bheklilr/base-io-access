module Access.Control.Concurrent.Chan
    ( module Control.Concurrent.Chan

    , ChanAccess(..)
    ) where


import           Control.Concurrent.Chan

import           Access.Core


class Access io => ChanAccess io where
    -- |Build and returns a new instance of 'Chan'.
    newChan'            :: io (Chan a)
    -- |Write a value to a 'Chan'.
    writeChan'          :: Chan a -> a -> io ()
    -- |Read the next value from the 'Chan'.
    readChan'           :: Chan a -> io a
    -- |Duplicate a 'Chan': the duplicate channel begins empty, but data written to
    -- either channel from then on will be available from both.  Hence this creates
    -- a kind of broadcast channel, where data written by anyone is seen by
    -- everyone else.
    --
    -- (Note that a duplicated channel is not equal to its original.
    -- So: @fmap (c /=) $ dupChan c@ returns @True@ for all @c@.)
    dupChan'            :: Chan a -> io (Chan a)
    -- |Return a lazy list representing the contents of the supplied
    -- 'Chan', much like 'System.IO.hGetContents'.
    getChanContents'    :: Chan a -> io [a]
    -- |Write an entire list of items to a 'Chan'.
    writeList2Chan'     :: Chan a -> [a] -> io ()


instance ChanAccess IO where
    newChan'         = newChan
    writeChan'       = writeChan
    readChan'        = readChan
    dupChan'         = dupChan
    getChanContents' = getChanContents
    writeList2Chan'  = writeList2Chan
