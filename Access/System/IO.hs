--------------------------------------------------------------------------------
-- |
-- Module       :   Access.System.IO
-- Copyright    :   (c) Aaron Stevens, 2014
-- License      :   GPL2
--
-- Maintainer   :   bheklilr2@gmail.com
--------------------------------------------------------------------------------

module Access.System.IO
    ( module System.IO

    , HandleWriteAccess(..)
    , HandleReadAccess(..)
    , HandleAccess(..)
    , StdIOAccess(..)
    , FileReadAccess(..)
    , FileWriteAccess(..)
    , FileAccess(..)
    , TempFileAccess(..)
    , TextEncodingAccess(..)
    ) where


import System.IO
import Foreign.Ptr (Ptr)

import Access.Core

-- | Provides access to Handle write functions
class Access io => HandleWriteAccess io where
    -- | Wraps 'System.IO.hPutChar'
    --
    -- Computation 'hPutChar'' @hdl ch@ writes the character @ch@ to the
    -- file or channel managed by @hdl@.  Characters may be buffered if
    -- buffering is enabled for @hdl@.
    --
    -- This operation may fail with:
    --
    --  * 'System.IO.Error.isFullError' if the device is full; or
    --
    --  * 'System.IO.Error.isPermissionError' if another system resource limit would be exceeded
    hPutChar'           :: Handle -> Char -> io ()
    -- | Wraps 'System.IO.hPutStr'
    --
    -- Computation 'hPutStr'' @hdl s@ writes the string @s@ to the file or
    -- channel managed by @hdl@
    --
    -- This operation may fail with:
    --
    --  * 'System.IO.Error.isFullError' if the device is full; or
    --
    --  * 'System.IO.Error.isPermissionError' if another system resource limit would be exceeded
    hPutStr'            :: Handle -> String -> io ()
    -- | Wraps 'System.IO.hPutStrLn'
    --
    -- The same as 'hPutStr'', but adds a newline character
    hPutStrLn'          :: Handle -> String -> io ()
    -- | Wraps 'System.IO.hPrint'
    --
    -- Computation 'hPrint'' @hdl t@ writes the string representation of @t@
    -- given by the 'Text.Show.shows' function to the file or channel managed
    -- by @hdl@ and appends a newline.
    --
    -- This operation may fail with:
    --
    --  * 'System.IO.Error.isFullError' if the device is full; or
    --
    --  * 'System.IO.Error.isPermissionError' if another system resource limit would be exceeded
    hPrint'             :: Show a => Handle -> a -> io ()
    -- | Wraps 'System.IO.hPutBuf'
    --
    -- 'hPutBuf'' @hdl buf count@ writes @count@ 8-bit bytes from the
    -- buffer @buf@ to the handle @hdl@.  It returns ().
    --
    -- 'hPutBuf'' ignores any text encoding that applies to the 'Handle',
    -- writing the bytes directly to the underlying file or device.
    --
    -- 'hPutBuf'' ignores the prevailing 'TextEncoding' and
    -- 'NewlineMode' on the 'Handle', and writes bytes directly.
    --
    -- This operation may fail with:
    --
    --  * 'ResourceVanished' if the handle is a pipe or socket, and the
    --    reading end is closed.  (If this is a POSIX system, and the program
    --    has not asked to ignore SIGPIPE, then a SIGPIPE may be delivered
    --    instead, whose default action is to terminate the program).
    hPutBuf'            :: Handle -> Ptr a -> Int -> io ()
    -- | Wraps 'System.IO.hPutBufNonBlocking'
    --
    -- 'hGetBufNonBlocking'' @hdl buf count@ reads data from the handle @hdl@
    -- into the buffer @buf@ until either EOF is reached, or
    -- @count@ 8-bit bytes have been read, or there is no more data available
    -- to read immediately.
    --
    -- 'hGetBufNonBlocking'' is identical to 'hGetBuf'', except that it will
    -- never block waiting for data to become available, instead it returns
    -- only whatever data is available.  To wait for data to arrive before
    -- calling 'hGetBufNonBlocking'', use 'hWaitForInput'.
    --
    -- If the handle is a pipe or socket, and the writing end
    -- is closed, 'hGetBufNonBlocking'' will behave as if EOF was reached.
    --
    -- 'hGetBufNonBlocking'' ignores the prevailing 'TextEncoding' and
    -- 'NewlineMode' on the 'Handle', and reads bytes directly.
    --
    -- NOTE: on Windows, this function does not work correctly; it
    -- behaves identically to 'hGetBuf''.
    hPutBufNonBlocking' :: Handle -> Ptr a -> Int -> io Int


class Access io => HandleReadAccess io where
    hWaitForInput'      :: Handle -> Int -> io Bool
    hGetChar'           :: Handle -> io Char
    hGetLine'           :: Handle -> io String
    hLookAhead'         :: Handle -> io Char
    hGetContents'       :: Handle -> io String
    hGetBuf'            :: Handle -> Ptr a -> Int -> io Int
    hGetBufSome'        :: Handle -> Ptr a -> Int -> io Int
    hGetBufNonBlocking' :: Handle -> Ptr a -> Int -> io Int


class (HandleWriteAccess io, HandleReadAccess io) => HandleAccess io where
    hClose'             :: Handle -> io ()
    hFileSize'          :: Handle -> io Integer
    hSetFileSize'       :: Handle -> Integer -> io ()
    hIsEOF'             :: Handle -> io Bool
    hSetBuffering'      :: Handle -> BufferMode -> io ()
    hGetBuffering'      :: Handle -> io BufferMode
    hFlush'             :: Handle -> io ()
    hGetPosn'           :: Handle -> io HandlePosn
    hSetPosn'           :: HandlePosn -> io ()
    hSeek'              :: Handle -> SeekMode -> Integer -> io ()
    hTell'              :: Handle -> io Integer
    hIsOpen'            :: Handle -> io Bool
    hIsClosed'          :: Handle -> io Bool
    hIsReadable'        :: Handle -> io Bool
    hIsWritable'        :: Handle -> io Bool
    hIsSeekable'        :: Handle -> io Bool
    hIsTerminalDevice'  :: Handle -> io Bool
    hSetEcho'           :: Handle -> Bool -> io ()
    hGetEcho'           :: Handle -> io Bool
    hShow'              :: Handle -> io String
    hReady'             :: Handle -> io Bool
    hSetEncoding'       :: Handle -> TextEncoding -> io ()
    hGetEncoding'       :: Handle -> io (Maybe TextEncoding)
    hSetNewlineMode'    :: Handle -> NewlineMode -> io ()


class HandleAccess io => StdIOAccess io where
    isEOF'          :: io Bool
    interact'       :: (String -> String) -> io ()
    putChar'        :: Char -> io ()
    putStr'         :: String -> io ()
    putStrLn'       :: String -> io ()
    print'          :: Show a => a -> io ()
    getChar'        :: io Char
    getLine'        :: io String
    getContents'    :: io String
    readLn'         :: Read a => io a


class HandleReadAccess io => FileReadAccess io where
    readFile' :: FilePath -> io String


class HandleWriteAccess io => FileWriteAccess io where
    writeFile'  :: FilePath -> String -> io ()
    appendFile' :: FilePath -> String -> io ()


class (FileReadAccess io, FileWriteAccess io) => FileAccess io where
    withFile'       :: FilePath -> IOMode -> (Handle -> io r) -> io r
    openFile'       :: FilePath -> IOMode -> io Handle
    withBinaryFile' :: FilePath -> IOMode -> (Handle -> io r) -> io r
    openBinaryFile' :: FilePath -> IOMode -> io Handle


class HandleAccess io => TempFileAccess io where
    openTempFile'       :: FilePath -> String -> io (FilePath, Handle)
    openBinaryTempFile' :: FilePath -> String -> io (FilePath, Handle)
    openTempFileWithDefaultPermissions'         :: FilePath -> String -> io (FilePath, Handle)
    openBinaryTempFileWithDefaultPermissions'   :: FilePath -> String -> io (FilePath, Handle)


class Access io => TextEncodingAccess io where
    mkTextEncoding' :: String -> io TextEncoding


instance HandleWriteAccess IO where
    hPutChar'           = hPutChar
    hPutStr'            = hPutStr
    hPutStrLn'          = hPutStrLn
    hPrint'             = hPrint
    hPutBuf'            = hPutBuf
    hPutBufNonBlocking' = hPutBufNonBlocking

instance HandleReadAccess IO where
    hWaitForInput'      = hWaitForInput
    hGetChar'           = hGetChar
    hGetLine'           = hGetLine
    hLookAhead'         = hLookAhead
    hGetContents'       = hGetContents
    hGetBuf'            = hGetBuf
    hGetBufSome'        = hGetBufSome
    hGetBufNonBlocking' = hGetBufNonBlocking

instance HandleAccess IO where
    hClose'             = hClose
    hFileSize'          = hFileSize
    hSetFileSize'       = hSetFileSize
    hIsEOF'             = hIsEOF
    hSetBuffering'      = hSetBuffering
    hGetBuffering'      = hGetBuffering
    hFlush'             = hFlush
    hGetPosn'           = hGetPosn
    hSetPosn'           = hSetPosn
    hSeek'              = hSeek
    hTell'              = hTell
    hIsOpen'            = hIsOpen
    hIsClosed'          = hIsClosed
    hIsReadable'        = hIsReadable
    hIsWritable'        = hIsWritable
    hIsSeekable'        = hIsSeekable
    hIsTerminalDevice'  = hIsTerminalDevice
    hSetEcho'           = hSetEcho
    hGetEcho'           = hGetEcho
    hShow'              = hShow
    hReady'             = hReady
    hSetEncoding'       = hSetEncoding
    hGetEncoding'       = hGetEncoding
    hSetNewlineMode'    = hSetNewlineMode

instance StdIOAccess IO where
    isEOF'       = isEOF
    interact'    = interact
    putChar'     = putChar
    putStr'      = putStr
    putStrLn'    = putStrLn
    print'       = print
    getChar'     = getChar
    getLine'     = getLine
    getContents' = getContents
    readLn'      = readLn

instance FileReadAccess IO where
    readFile'       = readFile

instance FileWriteAccess IO where
    writeFile'      = writeFile
    appendFile'     = appendFile

instance FileAccess IO where
    withFile'       = withFile
    openFile'       = openFile
    withBinaryFile' = withBinaryFile
    openBinaryFile' = openBinaryFile

instance TempFileAccess IO where
    openTempFile'                             = openTempFile
    openBinaryTempFile'                       = openBinaryTempFile
    openTempFileWithDefaultPermissions'       = openTempFileWithDefaultPermissions
    openBinaryTempFileWithDefaultPermissions' = openBinaryTempFileWithDefaultPermissions

instance TextEncodingAccess IO where
    mkTextEncoding' = mkTextEncoding
