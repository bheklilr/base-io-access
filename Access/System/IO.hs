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
    , StdInAccess(..)
    , StdOutAccess(..)
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

-- | Provides access to 'Handle' write functions
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


-- | Provides access to 'Handle' read functions
class Access io => HandleReadAccess io where
    -- | Wraps 'System.IO.hWaitForInput'
    --
    -- Computation 'hWaitForInput'' @hdl t@
    -- waits until input is available on handle @hdl@.
    -- It returns 'True' as soon as input is available on @hdl@,
    -- or 'False' if no input is available within @t@ milliseconds.  Note that
    -- 'hWaitForInput'' waits until one or more full /characters/ are available,
    -- which means that it needs to do decoding, and hence may fail
    -- with a decoding error.
    --
    -- If @t@ is less than zero, then @hWaitForInput@ waits indefinitely.
    --
    -- This operation may fail with:
    --
    --  * 'System.IO.Error.isEOFError' if the end of file has been reached.
    --
    --  * a decoding error, if the input begins with an invalid byte sequence
    --    in this Handle's encoding.
    --
    -- NOTE for GHC users: unless you use the @-threaded@ flag,
    -- @hWaitForInput t@ where @t >= 0@ will block all other Haskell
    -- threads for the duration of the call.  It behaves like a
    -- @safe@ foreign call in this respect.
    hWaitForInput'      :: Handle -> Int -> io Bool
    -- | Wraps 'System.IO.hGetChar'
    --
    -- Computation 'hGetChar'' @hdl@ reads a character from the file or
    -- channel managed by @hdl@, blocking until a character is available.
    --
    -- This operation may fail with:
    --
    --  * 'System.IO.Error.isEOFError' if the end of file has been reached.
    hGetChar'           :: Handle -> io Char
    -- | Wraps 'System.IO.hGetLine'
    --
    -- Computation 'hGetLine'' @hdl@ reads a line from the file or
    -- channel managed by @hdl@.
    --
    -- This operation may fail with:
    --
    --  * 'System.IO.Error.isEOFError' if the end of file is encountered when reading
    --    the /first/ character of the line.
    --
    -- If 'hGetLine'' encounters end-of-file at any other point while reading
    -- in a line, it is treated as a line terminator and the (partial)
    -- line is returned.
    hGetLine'           :: Handle -> io String
    -- | Wraps 'System.IO.hLookAhead'
    --
    -- Computation 'hLookAhead' returns the next character from the handle
    -- without removing it from the input buffer, blocking until a character
    -- is available.
    --
    -- This operation may fail with:
    --
    --  * 'System.IO.Error.isEOFError' if the end of file has been reached.
    hLookAhead'         :: Handle -> io Char
    -- | Wraps 'System.IO.hGetContents'
    --
    -- Computation 'hGetContents'' @hdl@ returns the list of characters
    -- corresponding to the unread portion of the channel or file managed
    -- by @hdl@, which is put into an intermediate state, /semi-closed/.
    -- In this state, @hdl@ is effectively closed,
    -- but items are read from @hdl@ on demand and accumulated in a special
    -- list returned by 'hGetContents'' @hdl@.
    --
    -- Any operation that fails because a handle is closed,
    -- also fails if a handle is semi-closed.  The only exception is 'hClose''.
    -- A semi-closed handle becomes closed:
    --
    --  * if 'hClose'' is applied to it;
    --
    --  * if an I\/O error occurs when reading an item from the handle;
    --
    --  * or once the entire contents of the handle has been read.
    --
    -- Once a semi-closed handle becomes closed, the contents of the
    -- associated list becomes fixed.  The contents of this final list is
    -- only partially specified: it will contain at least all the items of
    -- the stream that were evaluated prior to the handle becoming closed.
    --
    -- Any I\/O errors encountered while a handle is semi-closed are simply
    -- discarded.
    --
    -- This operation may fail with:
    --
    --  * 'System.IO.Error.isEOFError' if the end of file has been reached.
    hGetContents'       :: Handle -> io String
    -- | Wraps 'System.IO.hGetBuf'
    --
    -- 'hGetBuf' @hdl buf count@ reads data from the handle @hdl@ into the
    -- buffer @buf@ until either EOF is reached or @count@ 8-bit bytes have been
    -- read.  It returns the number of bytes actually read.  This may be zero if
    -- EOF was reached before any data was read (or if @count@ is zero).
    --
    -- 'hGetBuf' never raises an EOF exception, instead it returns a value
    -- smaller than @count@.
    --
    -- If the handle is a pipe or socket, and the writing end
    -- is closed, 'hGetBuf' will behave as if EOF was reached.
    --
    -- 'hGetBuf' ignores the prevailing 'TextEncoding' and 'NewlineMode'
    -- on the 'Handle', and reads bytes directly.
    hGetBuf'            :: Handle -> Ptr a -> Int -> io Int
    -- | Wraps 'System.IO.hGetBufSome'
    --
    --'hGetBufSome' @hdl buf count@ reads data from the handle @hdl@
    -- into the buffer @buf@.  If there is any data available to read,
    -- then 'hGetBufSome' returns it immediately; it only blocks if there
    -- is no data to be read.
    --
    -- It returns the number of bytes actually read.  This may be zero if
    -- EOF was reached before any data was read (or if @count@ is zero).
    --
    -- 'hGetBufSome' never raises an EOF exception, instead it returns a value
    -- smaller than @count@.
    --
    -- If the handle is a pipe or socket, and the writing end
    -- is closed, 'hGetBufSome' will behave as if EOF was reached.
    --
    -- 'hGetBufSome' ignores the prevailing 'TextEncoding' and 'NewlineMode'
    -- on the 'Handle', and reads bytes directly.
    hGetBufSome'        :: Handle -> Ptr a -> Int -> io Int
    -- | Wraps 'System.IO.hGetBufNonBlocking'
    --
    -- 'hGetBufNonBlocking' @hdl buf count@ reads data from the handle @hdl@
    -- into the buffer @buf@ until either EOF is reached, or
    -- @count@ 8-bit bytes have been read, or there is no more data available
    -- to read immediately.
    --
    -- 'hGetBufNonBlocking' is identical to 'hGetBuf', except that it will
    -- never block waiting for data to become available, instead it returns
    -- only whatever data is available.  To wait for data to arrive before
    -- calling 'hGetBufNonBlocking', use 'hWaitForInput'.
    --
    -- If the handle is a pipe or socket, and the writing end
    -- is closed, 'hGetBufNonBlocking' will behave as if EOF was reached.
    --
    -- 'hGetBufNonBlocking' ignores the prevailing 'TextEncoding' and
    -- 'NewlineMode' on the 'Handle', and reads bytes directly.
    --
    -- NOTE: on Windows, this function does not work correctly; it
    -- behaves identically to 'hGetBuf'.
    hGetBufNonBlocking' :: Handle -> Ptr a -> Int -> io Int


-- | Combines the 'HandleWriteAccess' and 'HandleReadAccess' classes and adds
-- additional miscellaneous functions for 'Handle' manipulation
class (HandleWriteAccess io, HandleReadAccess io) => HandleAccess io where
    -- | Wraps 'System.IO.hClose'
    --
    -- Computation 'hClose'' @hdl@ makes handle @hdl@ closed.  Before the
    -- computation finishes, if @hdl@ is writable its buffer is flushed as
    -- for 'hFlush'.
    -- Performing 'hClose'' on a handle that has already been closed has no
    -- effect; doing so is not an error.  All other operations on a closed
    -- handle will fail.  If 'hClose'' fails for any reason, any further
    -- operations (apart from 'hClose'') on the handle will still fail as if
    -- @hdl@ had been successfully closed.
    hClose'             :: Handle -> io ()
    -- | Wraps 'System.IO.hFileSize'
    --
    -- For a handle @hdl@ which attached to a physical file, 'hFileSize' @hdl@
    -- returns the size of that file in 8-bit bytes.
    hFileSize'          :: Handle -> io Integer
    -- | Wraps 'System.IO.hSetFileSize'
    --
    -- 'hSetFileSize'' @hdl@ @size@ truncates the physical file with handle
    -- @hdl@ to @size@ bytes.
    hSetFileSize'       :: Handle -> Integer -> io ()
    -- | Wraps 'System.IO.hIsEOF'
    --
    -- For a readable handle @hdl@, 'hIsEOF'' @hdl@ returns
    -- 'True' if no further input can be taken from @hdl@ or for a
    -- physical file, if the current I\/O position is equal to the length of
    -- the file.  Otherwise, it returns 'False'.
    --
    -- NOTE: 'hIsEOF'' may block, because it has to attempt to read from
    -- the stream to determine whether there is any more data to be read.
    hIsEOF'             :: Handle -> io Bool
    -- | Wraps 'System.IO.hSetBuffering'
    --
    -- Computation 'hSetBuffering'' @hdl mode@ sets the mode of buffering for
    -- handle @hdl@ on subsequent reads and writes.
    --
    -- If the buffer mode is changed from 'BlockBuffering' or
    -- 'LineBuffering' to 'NoBuffering', then
    --
    --  * if @hdl@ is writable, the buffer is flushed as for 'hFlush'';
    --
    --  * if @hdl@ is not writable, the contents of the buffer is discarded.
    --
    -- This operation may fail with:
    --
    --  * 'System.IO.Error.isPermissionError' if the handle has already been
    --    used for reading or writing and the implementation does not allow the
    --    buffering mode to be changed.
    hSetBuffering'      :: Handle -> BufferMode -> io ()
    -- | Wraps 'System.IO.hGetBuffering'
    --
    -- Computation 'hGetBuffering'' @hdl@ returns the current buffering mode
    hGetBuffering'      :: Handle -> io BufferMode
    -- | Wraps 'System.IO.hFlush'
    --
    -- The action 'hFlush'' @hdl@ causes any items buffered for output
    -- in handle @hdl@ to be sent immediately to the operating system.
    --
    -- This operation may fail with:
    --
    --  * 'System.IO.Error.isFullError' if the device is full;
    --
    --  * 'System.IO.Error.isPermissionError' if a system resource limit would
    --    be exceeded.  It is unspecified whether the characters in the buffer
    --    are discarded or retained under these circumstances.
    hFlush'             :: Handle -> io ()
    -- | Wraps 'System.IO.hGetPosn'
    --
    -- Computation 'hGetPosn'' @hdl@ returns the current I\/O position of
    -- @hdl@ as a value of the abstract type 'HandlePosn'.
    hGetPosn'           :: Handle -> io HandlePosn
    -- | Wraps 'System.IO.hSetPosn'
    --
    -- If a call to 'hGetPosn'' @hdl@ returns a position @p@,
    -- then computation 'hSetPosn'' @p@ sets the position of @hdl@
    -- to the position it held at the time of the call to 'hGetPosn''.
    --
    -- This operation may fail with:
    --
    --  * 'System.IO.Error.isPermissionError' if a system resource limit would
    --    be exceeded.
    hSetPosn'           :: HandlePosn -> io ()
    -- | Wraps 'System.IO.hSeek'
    --
    -- Computation 'hSeek'' @hdl mode i@ sets the position of handle
    -- @hdl@ depending on @mode@.
    -- The offset @i@ is given in terms of 8-bit bytes.
    --
    -- If @hdl@ is block- or line-buffered, then seeking to a position which is
    -- not in the current buffer will first cause any items in the output buffer
    -- to be written to the device, and then cause the input buffer to be
    -- discarded. Some handles may not be seekable (see 'hIsSeekable''), or only
    -- support a subset of the possible positioning operations (for instance, it
    -- may only be possible to seek to the end of a tape, or to a positive
    -- offset from the beginning or current position).
    -- It is not possible to set a negative I\/O position, or for
    -- a physical file, an I\/O position beyond the current end-of-file.
    --
    -- This operation may fail with:
    --
    --  * 'System.IO.Error.isIllegalOperationError' if the Handle is not
    --    seekable, or does not support the requested seek mode.
    --
    --  * 'System.IO.Error.isPermissionError' if a system resource limit would
    --    be exceeded.
    hSeek'              :: Handle -> SeekMode -> Integer -> io ()
    -- | Wraps 'System.IO.hTell'
    --
    -- Computation 'hTell'' @hdl@ returns the current position of the
    -- handle @hdl@, as the number of bytes from the beginning of
    -- the file.  The value returned may be subsequently passed to
    -- 'hSeek' to reposition the handle to the current position.
    --
    -- This operation may fail with:
    --
    --  * 'System.IO.Error.isIllegalOperationError' if the Handle is not seekable.
    hTell'              :: Handle -> io Integer
    -- | Wraps 'System.IO.hIsOpen'
    hIsOpen'            :: Handle -> io Bool
    -- | Wraps 'System.IO.hIsClosed'
    hIsClosed'          :: Handle -> io Bool
    -- | Wraps 'System.IO.hIsReadable'
    hIsReadable'        :: Handle -> io Bool
    -- | Wraps 'System.IO.hIsWritable'
    hIsWritable'        :: Handle -> io Bool
    -- | Wraps 'System.IO.hIsSeekable'
    hIsSeekable'        :: Handle -> io Bool
    -- | Wraps 'System.IO.hIsTerminalDevice'
    --
    -- Is the handle connected to a terminal?
    hIsTerminalDevice'  :: Handle -> io Bool
    -- | Wraps 'System.IO.hSetEcho'
    --
    -- Set the echoing status of a handle connected to a terminal.
    hSetEcho'           :: Handle -> Bool -> io ()
    -- | Wraps 'System.IO.hGetEcho'
    --
    -- Get the echoing status of a handle connected to a terminal.
    hGetEcho'           :: Handle -> io Bool
    -- | Wraps 'System.IO.hShow'
    --
    -- 'hShow'' is in the 'IO' monad, and gives more comprehensive output
    -- than the (pure) instance of 'Show' for 'Handle'.
    hShow'              :: Handle -> io String
    -- | Wraps 'System.IO.hReady'
    --
    --  Computation 'hReady'' @hdl@ indicates whether at least one item is
    -- available for input from handle @hdl@.
    --
    -- This operation may fail with:
    --
    --  * 'System.IO.Error.isEOFError' if the end of file has been reached.
    hReady'             :: Handle -> io Bool
    -- | Wraps 'System.IO.hSetEncoding'
    --
    -- The action 'hSetEncoding'' @hdl@ @encoding@ changes the text encoding
    -- for the handle @hdl@ to @encoding@.  The default encoding when a 'Handle'
    -- is created is 'localeEncoding', namely the default encoding for the
    -- current locale.
    --
    -- To create a 'Handle' with no encoding at all, use 'openBinaryFile''.  To
    -- stop further encoding or decoding on an existing 'Handle', use
    -- 'hSetBinaryMode''.
    --
    -- 'hSetEncoding'' may need to flush buffered data in order to change
    -- the encoding.
    hSetEncoding'       :: Handle -> TextEncoding -> io ()
    -- | Wraps 'System.IO.hGetEncoding'
    --
    -- Return the current 'TextEncoding' for the specified 'Handle', or
    -- 'Nothing' if the 'Handle' is in binary mode.
    --
    -- Note that the 'TextEncoding' remembers nothing about the state of
    -- the encoder/decoder in use on this 'Handle'.  For example, if the
    -- encoding in use is UTF-16, then using 'hGetEncoding'' and
    -- 'hSetEncoding'' to save and restore the encoding may result in an
    -- extra byte-order-mark being written to the file.
    hGetEncoding'       :: Handle -> io (Maybe TextEncoding)
    -- | Wraps 'System.IO.hSetNewlineMode'
    --
    -- Set the 'NewlineMode' on the specified 'Handle'.  All buffered data is
    -- flushed first.
    hSetNewlineMode'    :: Handle -> NewlineMode -> io ()


-- | Provides access to functions to read from 'stdin'
class Access io => StdInAccess io where
    -- | Wraps 'System.IO.getChar'
    --
    -- Read a character from the standard input device
    -- (same as 'hGetChar'' 'stdin').
    getChar'     :: io Char
    -- | Wraps 'System.IO.getLine'
    --
    -- Read a line from the standard input device (same as 'hGetLine'' 'stdin').
    getLine'     :: io String
    -- | Wraps 'System.IO.getContents'
    --
    -- The 'getContents' operation returns all user input as a single string,
    -- which is read lazily as it is needed (same as 'hGetContents'' 'stdin').
    getContents' :: io String
    -- | Wraps 'System.IO.readLn'
    --
    -- The 'readLn'' function combines 'getLine'' and 'readIO'.
    readLn'      :: Read a => io a
    -- | Wraps 'System.IO.interact'
    --
    -- The 'interact'' function takes a function of type @String->String@
    -- as its argument.  The entire input from the standard input device is
    -- passed to this function as its argument, and the resulting string is
    -- output on the standard output device.
    interact'    :: (String -> String) -> io ()


-- | Provides access to functions to write to 'stdout'
class Access io => StdOutAccess io where
    -- | Wraps 'System.IO.putChar'
    --
    -- Write a character to the standard output device
    -- (same as 'hPutChar'' 'stdout').
    putChar'  :: Char -> io ()
    -- | Wraps 'System.IO.putStr'
    --
    -- Write a string to the standard output device
    -- (same as 'hPutStr'' 'stdout').
    putStr'   :: String -> io ()
    -- | Wraps 'System.IO.putStrLn'
    --
    -- The same as 'putStr'', but adds a newline character.
    putStrLn' :: String -> io ()
    -- | Wraps 'System.IO.print'
    --
    -- The 'print'' function outputs a value of any printable type to the
    -- standard output device.
    -- Printable types are those that are instances of class 'Show'; 'print''
    -- converts values to strings for output using the 'show' operation and
    -- adds a newline.
    --
    -- For example, a program to print' the first 20 integers and their
    -- powers of 2 could be written as:
    --
    -- > main = print' ([(n, 2^n) | n <- [0..19]])
    print'    :: Show a => a -> io ()


-- | Combines the 'StdInAccess' and 'StdOutAccess' into a single class
class (StdInAccess io, StdOutAccess io) => StdIOAccess io where
    -- | Wraps 'System.IO.isEOF'
    --
    -- For a readable handle @hdl@, 'hIsEOF'' @hdl@ returns
    -- 'True' if no further input can be taken from @hdl@ or for a
    -- physical file, if the current I\/O position is equal to the length of
    -- the file.  Otherwise, it returns 'False'.
    --
    -- NOTE: 'hIsEOF'' may block, because it has to attempt to read from
    -- the stream to determine whether there is any more data to be read.
    isEOF' :: io Bool


-- | Provides the function 'readFile'' for reading the contents of a file
class FileReadAccess io where
    -- | Wraps 'System.IO.readFile'
    --
    -- The 'readFile'' function reads a file and
    -- returns the contents of the file as a string.
    -- The file is read lazily, on demand, as with 'getContents''.
    readFile' :: FilePath -> io String


-- | Provides functions for writing to files
class Access io => FileWriteAccess io where
    -- | Wraps 'System.IO.writeFile'
    --
    -- The computation 'writeFile'' @file str@ function writes the string @str@,
    -- to the file @file@.
    writeFile'  :: FilePath -> String -> io ()
    -- | Wraps 'System.IO.appendFile'
    --
    -- The computation 'appendFile'' @file str@ function appends the string
    -- @str@, to the file @file@.
    --
    -- Note that 'writeFile'' and 'appendFile'' write a literal string
    -- to a file.  To write a value of any printable type, as with 'print'',
    -- use the 'show' function to convert the value to a string first.
    --
    -- > main = appendFile' "squares" (show [(x,x*x) | x <- [0,0.1..2]])
    appendFile' :: FilePath -> String -> io ()


-- | Combines 'HandleAccess', 'FileReadAccess', and 'FileWriteAccess' for
-- manipulating files (this does allow for general 'Handle' access and should be
-- considered unsafe)
class (HandleAccess io, FileReadAccess io, FileWriteAccess io) => FileAccess io where
    -- | Wraps 'System.IO.withFile'
    --
    -- @'withFile'' name mode act@ opens a file using 'openFile'' and passes
    -- the resulting handle to the computation @act@.  The handle will be
    -- closed on exit from 'withFile'', whether by normal termination or by
    -- raising an exception.  If closing the handle raises an exception, then
    -- this exception will be raised by 'withFile'' rather than any exception
    -- raised by 'act'.
    withFile'       :: FilePath -> IOMode -> (Handle -> io r) -> io r
    -- | Wraps 'System.IO.openFile'
    --
    -- Computation 'openFile'' @file mode@ allocates and returns a new, open
    -- handle to manage the file @file@.  It manages input if @mode@
    -- is 'ReadMode', output if @mode@ is 'WriteMode' or 'AppendMode',
    -- and both input and output if mode is 'ReadWriteMode'.
    --
    -- If the file does not exist and it is opened for output, it should be
    -- created as a new file.  If @mode@ is 'WriteMode' and the file
    -- already exists, then it should be truncated to zero length.
    -- Some operating systems delete empty files, so there is no guarantee
    -- that the file will exist following an 'openFile'' with @mode@
    -- 'WriteMode' unless it is subsequently written to successfully.
    -- The handle is positioned at the end of the file if @mode@ is
    -- 'AppendMode', and otherwise at the beginning (in which case its
    -- internal position is 0).
    -- The initial buffer mode is implementation-dependent.
    --
    -- This operation may fail with:
    --
    --  * 'System.IO.Error.isAlreadyInUseError' if the file is already open and
    --    cannot be reopened;
    --
    --  * 'System.IO.Error.isDoesNotExistError' if the file does not exist; or
    --
    --  * 'System.IO.Error.isPermissionError' if the user does not have
    --    permission to open the file.
    --
    -- Note: if you will be working with files containing binary data, you'll want to
    -- be using 'openBinaryFile''.
    openFile'       :: FilePath -> IOMode -> io Handle
    -- | Wraps 'System.IO.withBinaryFile'
    --
    -- @'withBinaryFile'' name mode act@ opens a file using 'openBinaryFile''
    -- and passes the resulting handle to the computation @act@.  The handle
    -- will be closed on exit from 'withBinaryFile'', whether by normal
    -- termination or by raising an exception.
    withBinaryFile' :: FilePath -> IOMode -> (Handle -> io r) -> io r
    -- | Wraps 'System.IO.openBinaryFile'
    --
    -- Like 'openFile'', but open the file in binary mode.
    -- On Windows, reading a file in text mode (which is the default)
    -- will translate CRLF to LF, and writing will translate LF to CRLF.
    -- This is usually what you want with text files.  With binary files
    -- this is undesirable; also, as usual under Microsoft operating systems,
    -- text mode treats control-Z as EOF.  Binary mode turns off all special
    -- treatment of end-of-line and end-of-file characters.
    -- (See also 'hSetBinaryMode''.)
    openBinaryFile' :: FilePath -> IOMode -> io Handle


-- | Provides access to functions for opening temporary file 'Handle's
class Access io => TempFileAccess io where
    -- | Wraps 'System.IO.openTempFile'
    --
    -- The function creates a temporary file in 'ReadWrite' mode.
    -- The created file isn\'t deleted automatically, so you need to delete it
    -- manually.
    --
    -- The file is creates with permissions such that only the current
    -- user can read\/write it.
    --
    -- With some exceptions (see below), the file will be created securely
    -- in the sense that an attacker should not be able to cause
    -- 'openTempFile'' to overwrite another file on the filesystem using your
    -- credentials, by putting symbolic links (on Unix) in the place where
    -- the temporary file is to be created.  On Unix the @O_CREAT@ and
    -- @O_EXCL@ flags are used to prevent this attack, but note that
    -- @O_EXCL@ is sometimes not supported on NFS filesystems, so if you
    -- rely on this behaviour it is best to use local filesystems only.
    openTempFile'       :: FilePath -> String -> io (FilePath, Handle)
    -- | Wraps 'System.IO.openBinaryTempFile'
    --
    -- Like 'openTempFile'', but opens the file in binary mode.
    -- See 'openBinaryFile'' for more comments.
    openBinaryTempFile' :: FilePath -> String -> io (FilePath, Handle)
    -- | Wraps 'System.IO.openTempFileWithDefaultPermissions'
    --
    -- Like 'openTempFile'', but uses the default file permissions
    openTempFileWithDefaultPermissions'         :: FilePath -> String -> io (FilePath, Handle)
    -- | Wraps 'System.IO.openBinaryTempFileWithDefaultPermissions'
    --
    -- Like 'openBinaryTempFile'', but uses the default file permissions
    openBinaryTempFileWithDefaultPermissions'   :: FilePath -> String -> io (FilePath, Handle)


-- | Provides access to 'mkTextEncoding''
class Access io => TextEncodingAccess io where
    -- | Wraps 'System.IO.mkTextEncoding'
    --
    -- Look up the named Unicode encoding.  May fail with
    --
    --  * 'System.IO.Error.isDoesNotExistError' if the encoding is unknown
    --
    -- The set of known encodings is system-dependent, but includes at least:
    --
    --  * @UTF-8@
    --
    --  * @UTF-16@, @UTF-16BE@, @UTF-16LE@
    --
    --  * @UTF-32@, @UTF-32BE@, @UTF-32LE@
    --
    -- On systems using GNU iconv (e.g. Linux), there is additional
    -- notation for specifying how illegal characters are handled:
    --
    --  * a suffix of @\/\/IGNORE@, e.g. @UTF-8\/\/IGNORE@, will cause
    --    all illegal sequences on input to be ignored, and on output
    --    will drop all code points that have no representation in the
    --    target encoding.
    --
    --  * a suffix of @\/\/TRANSLIT@ will choose a replacement character
    --    for illegal sequences or code points.
    --
    -- On Windows, you can access supported code pages with the prefix
    -- @CP@; for example, @\"CP1250\"@.
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

instance StdInAccess IO where
    interact'    = interact
    getChar'     = getChar
    getLine'     = getLine
    getContents' = getContents
    readLn'      = readLn

instance StdOutAccess IO where
    putChar'     = putChar
    putStr'      = putStr
    putStrLn'    = putStrLn
    print'       = print

instance StdIOAccess IO where
    isEOF'       = isEOF

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
