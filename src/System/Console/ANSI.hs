#include "Common-Safe-Haskell.hs"

{-| Through this module, this library provides platform-independent support for
control character sequences following the \'ANSI\' standards (see further below)
for terminal software that supports those sequences, running on a Unix-like
operating system or Windows 10.

The sequences of control characters (also referred to as \'escape\' sequences or
codes) provide a rich range of functionality for terminal control, which
includes:

 * Colored text output, with control over both foreground and background colors

 * Clearing parts of a line or the screen

 * Hiding or showing the cursor

 * Moving the cursor around

 * Reporting the position of the cursor

 * Scrolling the screen up or down

 * Changing the title of the terminal

A terminal that supports control character sequences acts on them when they
are flushed from the output buffer (with a newline character @\"\\n\"@ or, for
the standard output channel, @hFlush stdout@).

The functions moving the cursor to an absolute position are 0-based (the
top-left corner is considered to be at row 0 column 0) (see 'setCursorPosition')
and so is 'getCursorPosition0'. The \'ANSI\' standards themselves are 1-based
(that is, the top-left corner is considered to be at row 1 column 1) and some
functions reporting the position of the cursor are too (see
'reportCursorPosition').

The native terminal software on Windows is \'Command Prompt\' or \`PowerShell\`.
Before Windows 10 version 1511 (known as the \'November [2015] Update\' or
\'Threshold 2\') that software did not support such control sequences.

Terminal software other than the native software exists for Windows. One example
is the \'mintty\' terminal emulator for \'Cygwin\', \'MSYS\' or \'MSYS2\', and
dervied projects, and for \'WSL\' (Windows Subsystem for Linux).

The \'ANSI\' standards refer to (1) standard ECMA-48 \`Control Functions for
Coded Character Sets\' (5th edition, 1991); (2) extensions in ITU-T
Recommendation (previously CCITT Recommendation) T.416 (03/93) \'Information
Technology – Open Document Architecture (ODA) and Interchange Format: Character
Content Architectures\` (also published as ISO/IEC International Standard
8613-6); and (3) further extensions used by \'XTerm\', a terminal emulator for
the X Window System. The escape codes are described in a Wikipedia article at
<http://en.wikipedia.org/wiki/ANSI_escape_code> and those codes supported on
current versions of Windows at
<https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences>.

The whole of the \'ANSI\' standards are not supported by this library but most
(if not all) of the parts that are popular and well-supported by terminal
software are supported. Every function exported by this module comes in three
variants, namely:

 * A variant that has an @IO ()@ type and doesn't take a @Handle@ (for example,
   @clearScreen :: IO ()@). This variant just outputs the \`ANSI\` command
   directly to the standard output channel ('stdout') and any terminal
   corresponding to it.

 * An \'@h@...\' variant that has an @IO ()@ type but takes a @Handle@ (for
   example, @hClearScreen :: Handle -> IO ()@). This variant outputs the
   \`ANSI\` command to the supplied handle and any terminal corresponding to it.

 * A \'...@Code@\' variant that has a @String@ type (for example,
   @clearScreenCode :: String@). This variant outputs the sequence of control
   characters as a 'String', which can be added to any other bit of text before
   being output.

Example:

> module Main where
>
> import System.Console.ANSI
>
> -- Set colors and write some text in those colors.
> main :: IO ()
> main = withANSI $ do
>   setSGR [SetColor Foreground Vivid Red]
>   setSGR [SetColor Background Vivid Blue]
>   putStrLn "Red-On-Blue"
>   setSGR [Reset]  -- Reset to default colour scheme
>   putStrLn "Default colors."

Another example:

> module Main where
>
> import System.IO (hFlush, stdout)
> import System.Console.ANSI
>
> main :: IO ()
> main = withANSI $ do
>   setSGR [SetColor Foreground Dull Blue]
>   putStr "Enter your name: "
>   setSGR [SetColor Foreground Dull Yellow]
>   hFlush stdout  -- flush the output buffer before getLine
>   name <- getLine
>   setSGR [SetColor Foreground Dull Blue]
>   putStrLn $ "Hello, " ++ name ++ "!"
>   setSGR [Reset]  -- reset to default colour scheme

For many more examples, see the project's extensive
<https://github.com/feuerbach/ansi-terminal/blob/master/app/Example.hs Example.hs> file.
-}

module System.Console.ANSI
  ( -- * Enable ANSI
    withANSI
  , withANSIh

    -- * Basic data types
  , module System.Console.ANSI.Types

    -- * Cursor movement by character
  , cursorUp
  , cursorDown
  , cursorForward
  , cursorBackward
  , hCursorUp
  , hCursorDown
  , hCursorForward
  , hCursorBackward
  , cursorUpCode
  , cursorDownCode
  , cursorForwardCode
  , cursorBackwardCode

    -- * Cursor movement by line
    -- | The difference between movements \"by character\" and \"by line\" is
    -- that @*Line@ functions additionally move the cursor to the start of the
    -- line, while functions like @cursorUp@ and @cursorDown@ keep the column
    -- the same.
    --
    -- Also keep in mind that @*Line@ functions are not as portable. See
    -- <https://github.com/feuerbach/ansi-terminal/issues/10> for the details.
  , cursorUpLine
  , cursorDownLine
  , hCursorUpLine
  , hCursorDownLine
  , cursorUpLineCode
  , cursorDownLineCode

    -- * Directly changing cursor position
  , setCursorColumn
  , hSetCursorColumn
  , setCursorColumnCode

  , setCursorPosition
  , hSetCursorPosition
  , setCursorPositionCode

    -- * Saving, restoring and reporting cursor position
  , saveCursor
  , hSaveCursor
  , saveCursorCode

  , restoreCursor
  , hRestoreCursor
  , restoreCursorCode

  , reportCursorPosition
  , hReportCursorPosition
  , reportCursorPositionCode

    -- * Clearing parts of the screen
    -- | Note that these functions only clear parts of the screen. They do not move the
    -- cursor.
  , clearFromCursorToScreenEnd
  , clearFromCursorToScreenBeginning
  , clearScreen
  , hClearFromCursorToScreenEnd
  , hClearFromCursorToScreenBeginning
  , hClearScreen
  , clearFromCursorToScreenEndCode
  , clearFromCursorToScreenBeginningCode
  , clearScreenCode

  , clearFromCursorToLineEnd
  , clearFromCursorToLineBeginning
  , clearLine
  , hClearFromCursorToLineEnd
  , hClearFromCursorToLineBeginning
  , hClearLine
  , clearFromCursorToLineEndCode
  , clearFromCursorToLineBeginningCode
  , clearLineCode

    -- * Scrolling the screen
  , scrollPageUp
  , scrollPageDown
  , hScrollPageUp
  , hScrollPageDown
  , scrollPageUpCode
  , scrollPageDownCode

    -- * Select Graphic Rendition mode: colors and other whizzy stuff
  , setSGR
  , hSetSGR
  , setSGRCode

    -- * Cursor visibilty changes
  , hideCursor
  , showCursor
  , hHideCursor
  , hShowCursor
  , hideCursorCode
  , showCursorCode

    -- * Changing the title
  , setTitle
  , hSetTitle
  , setTitleCode

    -- * Checking if handle supports ANSI (not portable: GHC only)
  , hSupportsANSI
  , hSupportsANSIColor
  , hSupportsANSIWithoutEmulation

    -- * Getting the cursor position
  , getCursorPosition0
  , hGetCursorPosition
  , getReportedCursorPosition
  , cursorPosition

    -- * Getting the terminal size
  , getTerminalSize
  , hGetTerminalSize
  ) where

import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (mapMaybe)
import Control.Exception.Base (IOException, SomeException, bracket, catch, throw, try)
import System.IO (BufferMode (..), Handle, hFlush, hGetBuffering, hIsTerminalDevice,
  hIsWritable, hPutStr, hSetBuffering, stderr, stdin, stdout)
import Text.ParserCombinators.ReadP (readP_to_S)

import System.Console.ANSI.Codes
import System.Console.ANSI.Types
#ifdef WINDOWS
import System.Console.ANSI.Windows.Foreign (DWORD, HANDLE, INPUT_RECORD_EVENT (..),
  cWcharsToChars, eNABLE_VIRTUAL_TERMINAL_PROCESSING, getConsoleMode,
  getNumberOfConsoleInputEvents, inputEvent, inputEventType, keyEventChar,
  keyEventKeyDown, readConsoleInput, setConsoleMode, unicodeAsciiChar, iNVALID_HANDLE_VALUE, nullHANDLE,
  withHandleToHANDLE)
#endif

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>))
import Data.Functor ((<$>))
#endif

import Control.Monad (void)
import Data.Char (isDigit)
import System.Environment (getEnvironment)
import Text.ParserCombinators.ReadP (char, many1, ReadP, satisfy)

#ifdef WINDOWS
#else
import Data.Maybe (froMaybe)
import System.IO (hGetEcho, hSetEcho)
import System.Timeout (timeout)
#endif

hCursorUp, hCursorDown, hCursorForward, hCursorBackward
  :: Handle
  -> Int -- ^ Number of lines or characters to move
  -> IO ()
cursorUp, cursorDown, cursorForward, cursorBackward
  :: Int -- ^ Number of lines or characters to move
  -> IO ()
cursorUp = hCursorUp stdout
cursorDown = hCursorDown stdout
cursorForward = hCursorForward stdout
cursorBackward = hCursorBackward stdout

hCursorDownLine, hCursorUpLine :: Handle
                               -> Int -- ^ Number of lines to move
                               -> IO ()
cursorDownLine, cursorUpLine :: Int -- ^ Number of lines to move
                             -> IO ()
cursorDownLine = hCursorDownLine stdout
cursorUpLine = hCursorUpLine stdout

hSetCursorColumn :: Handle
                 -> Int -- ^ 0-based column to move to
                 -> IO ()

-- | Move the cursor to the specified column. The column numbering is 0-based
-- (that is, the left-most column is numbered 0).
setCursorColumn :: Int -- ^ 0-based column to move to
                -> IO ()
setCursorColumn = hSetCursorColumn stdout

hSetCursorPosition :: Handle
                   -> Int -- ^ 0-based row to move to
                   -> Int -- ^ 0-based column to move to
                   -> IO ()

-- | Move the cursor to the specified position (row and column). The position is
-- 0-based (that is, the top-left corner is at row 0 column 0).
setCursorPosition :: Int -- ^ 0-based row to move to
                  -> Int -- ^ 0-based column to move to
                  -> IO ()
setCursorPosition = hSetCursorPosition stdout

hSaveCursor, hRestoreCursor, hReportCursorPosition :: Handle -> IO ()

-- | Save the cursor position in memory. The only way to access the saved value
-- is with the 'restoreCursor' command.
--
-- @since 0.7.1
saveCursor :: IO ()

-- | Restore the cursor position from memory. There will be no value saved in
-- memory until the first use of the 'saveCursor' command.
--
-- @since 0.7.1
restoreCursor :: IO ()

-- | Looking for a way to get the cursors position? See
-- 'getCursorPosition0'.
--
-- Emit the cursor position into the console input stream, immediately after
-- being recognised on the output stream, as:
-- @ESC [ \<cursor row> ; \<cursor column> R@
--
-- Note that the information that is emitted is 1-based (the top-left corner is
-- at row 1 column 1) but 'setCursorColumn' and 'setCursorPosition' are
-- 0-based.
--
-- In isolation of 'getReportedCursorPosition' or 'getCursorPosition0', this
-- function may be of limited use on Windows operating systems because of
-- difficulties in obtaining the data emitted into the console input stream.
-- The function 'hGetBufNonBlocking' in module "System.IO" does not work on
-- Windows. This has been attributed to the lack of non-blocking primatives in
-- the operating system (see the GHC bug report #806 at
-- <https://ghc.haskell.org/trac/ghc/ticket/806>).
--
-- @since 0.7.1
reportCursorPosition :: IO ()

saveCursor = hSaveCursor stdout
restoreCursor = hRestoreCursor stdout
reportCursorPosition = hReportCursorPosition stdout

hHideCursor, hShowCursor :: Handle
                         -> IO ()
hideCursor, showCursor :: IO ()
hideCursor = hHideCursor stdout
showCursor = hShowCursor stdout

-- | Set the terminal window title
hSetTitle :: Handle
          -> String -- ^ New title
          -> IO ()
-- | Set the terminal window title
setTitle :: String -- ^ New title
         -> IO ()
setTitle = hSetTitle stdout

-- | Use heuristics to determine whether the functions defined in this
-- package will work with a given handle. This function assumes that the handle
-- is writable (that is, it manages output - see 'hIsWritable').
--
-- For Unix-like operating systems, the current implementation checks
-- that: (1) the handle is a terminal; and (2) a @TERM@
-- environment variable is not set to @dumb@ (which is what the GNU Emacs text
-- editor sets for its integrated terminal).
--
-- For Windows, the current implementation performs the same checks as for
-- Unix-like operating systems and, as an alternative, checks whether the
-- handle is connected to a \'mintty\' terminal. (That is because the function
-- 'hIsTerminalDevice' is used to check if the handle is a
-- terminal. However, where a non-native Windows terminal (such as \'mintty\')
-- is implemented using redirection, that function will not identify a
-- handle to the terminal as a terminal.) On Windows 10, if the handle is
-- identified as connected to a native terminal, this function does /not/ enable
-- the processing of \'ANSI\' control characters in output (see
-- 'hSupportsANSIWithoutEmulation').
--
-- @since 0.6.2
hSupportsANSI :: Handle -> IO Bool

-- | Some terminals (e.g. Emacs) are not fully ANSI compliant but can support
-- ANSI colors. This can be used in such cases, if colors are all that is
-- needed.
--
-- @since 0.9
hSupportsANSIColor :: Handle -> IO Bool
hSupportsANSIColor h = (||) <$> hSupportsANSI h <*> isEmacsTerm
  where
    isEmacsTerm = (\env -> (insideEmacs env) && (isDumb env)) <$> getEnvironment
    insideEmacs env = any (\(k, _) -> k == "INSIDE_EMACS") env
    isDumb env = Just "dumb" == lookup "TERM" env

-- | Use heuristics to determine whether a given handle will support \'ANSI\'
-- control characters in output. (On Windows versions before Windows 10, that
-- means \'support without emulation\'.)
--
-- If the handle is not writable (that is, it cannot manage output - see
-- 'hIsWritable'), then @return (Just False)@ is returned.
--
-- On Unix-like operating systems, with one exception, the function is
-- consistent with 'hSupportsANSI'. The exception is if the handle is not
-- writable.
--
-- On Windows, what is returned will depend on what the handle is connected to
-- and the version of the operating system. If the handle is identified as
-- connected to a \'mintty\' terminal, @return (Just True)@ is
-- returned. If it is identifed as connected to a native terminal, then, on
-- Windows 10, the processing of \'ANSI\' control characters will be enabled and
-- @return (Just True)@ returned; and, on versions of Windows before Windows 10,
-- @return (Just False)@ is returned. Otherwise, if a @TERM@ environment
-- variable is set to @dumb@, @return (Just False)@ is returned. In all other
-- cases of a writable handle, @return Nothing@ is returned; this indicates that
-- the heuristics cannot assist - the handle may be connected to a file or
-- to another type of terminal.
--
-- @since 0.8.1
hSupportsANSIWithoutEmulation :: Handle -> IO (Maybe Bool)

-- | Parses the characters emitted by 'reportCursorPosition' into the console
-- input stream. Returns the cursor row and column as a tuple.
--
-- For example, if the characters emitted by 'reportCursorPosition' are in
-- 'String' @input@ then the parser could be applied like this:
--
-- > let result = readP_to_S cursorPosition input
-- > case result of
-- >     [] -> putStrLn $ "Error: could not parse " ++ show input
-- >     [((row, column), _)] -> putStrLn $ "The cursor was at row " ++ show row
-- >         ++ " and column" ++ show column ++ "."
-- >     (_:_) -> putStrLn $ "Error: parse not unique"
--
-- @since 0.7.1
cursorPosition :: ReadP (Int, Int)
cursorPosition = do
  void $ char '\ESC'
  void $ char '['
  row <- decimal -- A non-negative whole decimal number
  void $ char ';'
  col <- decimal -- A non-negative whole decimal number
  void $ char 'R'
  return (read row, read col)
 where
  digit = satisfy isDigit
  decimal = many1 digit

-- | Attempts to get the reported cursor position data from the console input
-- stream. The function is intended to be called immediately after
-- 'reportCursorPosition' (or related functions) have caused characters to be
-- emitted into the stream.
--
-- For example, on a Unix-like operating system:
--
-- > hSetBuffering stdin NoBuffering -- set no buffering (the contents of the
-- >                                 -- buffer will be discarded, so this needs
-- >                                 -- to be done before the cursor positon is
-- >                                 -- emitted)
-- > reportCursorPosition
-- > hFlush stdout -- ensure the report cursor position code is sent to the
-- >               -- operating system
-- > input <- getReportedCursorPosition
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Win32 console of the Windows API.
-- (Command Prompt and PowerShell are based on the Win32 console.)
--
-- @since 0.7.1
getReportedCursorPosition :: IO String

-- | Attempts to get the reported cursor position, combining the functions
-- 'reportCursorPosition', 'getReportedCursorPosition' and 'cursorPosition'. Any
-- position @(row, column)@ is translated to be 0-based (that is, the top-left
-- corner is at @(0, 0)@), consistent with `setCursorColumn` and
-- `setCursorPosition`. (Note that the information emitted into the console
-- input stream by 'reportCursorPosition' is 1-based.) Returns 'Nothing' if any
-- data emitted by 'reportCursorPosition', obtained by
-- 'getReportedCursorPosition', cannot be parsed by 'cursorPosition'. Uses
-- 'stdout'. If 'stdout' will be redirected, see 'hGetCursorPosition' for a more
-- general function.
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Win32 console of the Windows API.
-- (Command Prompt and PowerShell are based on the Win32 console.)
--
-- @since 0.8.2
getCursorPosition0 :: IO (Maybe (Int, Int))
getCursorPosition0 = hGetCursorPosition stdout

-- | Attempts to get the reported cursor position, combining the functions
-- 'hReportCursorPosition' (with the specified handle),
-- 'getReportedCursorPosition' and 'cursorPosition'. Any position
-- @(row, column)@ is translated to be 0-based (that is, the top-left corner is
-- at @(0, 0)@), consistent with 'hSetCursorColumn' and 'hSetCursorPosition'.
-- (Note that the information emitted into the console input stream by
-- 'hReportCursorPosition' is 1-based.) Returns 'Nothing' if any data emitted by
-- 'hReportCursorPosition', obtained by 'getReportedCursorPosition', cannot be
-- parsed by 'cursorPosition'.
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Win32 console of the Windows API.
-- (Command Prompt and PowerShell are based on the Win32 console.)
--
-- @since 0.10.1
hGetCursorPosition :: Handle -> IO (Maybe (Int, Int))

-- | Attempts to get the current terminal size (height in rows, width in
-- columns), by using 'getCursorPosition0' to query the console input stream
-- after attempting to set the cursor position beyond the bottom right corner of
-- the terminal. Uses 'stdout'. If 'stdout' will be redirected, see
-- 'hGetTerminalSize' for a more general function.
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Win32 console of the Windows API.
-- (Command Prompt and PowerShell are based on the Win32 console.)
--
-- @since 0.9
getTerminalSize :: IO (Maybe (Int, Int))
getTerminalSize = hGetTerminalSize stdout

-- | Attempts to get the current terminal size (height in rows, width in
-- columns), by writing control character sequences to the specified handle
-- (which will typically be 'stdout' or 'stderr') and using 'hGetCursorPosition'
-- to query the console input stream after attempting to set the cursor position
-- beyond the bottom right corner of the terminal.
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Win32 console of the Windows API.
-- (Command Prompt and PowerShell are based on the Win32 console.)
--
-- @since 0.10.1
hGetTerminalSize :: Handle -> IO (Maybe (Int, Int))
hGetTerminalSize h = do
  hSaveCursor h
  hSetCursorPosition h 9999 9999  -- Attempt to set the cursor position beyond
                                  -- the bottom right corner of the terminal.
  mPos <- hGetCursorPosition h
  hRestoreCursor h
  hFlush h -- ensure the restore cursor position code is sent to the
           -- operating system
  return $ fmap (\(r, c) -> (r + 1, c + 1)) mPos

-- | Set the Select Graphic Rendition mode
hSetSGR
  :: Handle
  -> [SGR] -- ^ Commands: these will typically be applied on top of the
           -- current console SGR mode. An empty list of commands is
           -- equivalent to the list @[Reset]@. Commands are applied left to
           -- right.
  -> IO ()

-- | Set the Select Graphic Rendition mode
setSGR
  :: [SGR] -- ^ Commands: these will typically be applied on top of the
           -- current console SGR mode. An empty list of commands is
           -- equivalent to the list @[Reset]@. Commands are applied left to
           -- right.
  -> IO ()
setSGR = hSetSGR stdout

hClearFromCursorToScreenEnd, hClearFromCursorToScreenBeginning, hClearScreen
  :: Handle
  -> IO ()

clearFromCursorToScreenEnd, clearFromCursorToScreenBeginning, clearScreen
  :: IO ()
clearFromCursorToScreenEnd = hClearFromCursorToScreenEnd stdout
clearFromCursorToScreenBeginning = hClearFromCursorToScreenBeginning stdout
clearScreen = hClearScreen stdout

hClearFromCursorToLineEnd, hClearFromCursorToLineBeginning, hClearLine
  :: Handle
  -> IO ()

clearFromCursorToLineEnd, clearFromCursorToLineBeginning, clearLine
  :: IO ()
clearFromCursorToLineEnd = hClearFromCursorToLineEnd stdout
clearFromCursorToLineBeginning = hClearFromCursorToLineBeginning stdout
clearLine = hClearLine stdout

-- | Scroll the displayed information up or down the terminal: not widely
-- supported
hScrollPageUp, hScrollPageDown
  :: Handle
  -> Int -- ^ Number of lines to scroll by
  -> IO ()

-- | Scroll the displayed information up or down the terminal: not widely
-- supported
scrollPageUp, scrollPageDown
  :: Int -- ^ Number of lines to scroll by
  -> IO ()
scrollPageUp = hScrollPageUp stdout
scrollPageDown = hScrollPageDown stdout

hCursorUp h n = hPutStr h $ cursorUpCode n
hCursorDown h n = hPutStr h $ cursorDownCode n
hCursorForward h n = hPutStr h $ cursorForwardCode n
hCursorBackward h n = hPutStr h $ cursorBackwardCode n

hCursorDownLine h n = hPutStr h $ cursorDownLineCode n
hCursorUpLine h n = hPutStr h $ cursorUpLineCode n

hSetCursorColumn h n = hPutStr h $ setCursorColumnCode n
hSetCursorPosition h n m = hPutStr h $ setCursorPositionCode n m

hSaveCursor h = hPutStr h saveCursorCode
hRestoreCursor h = hPutStr h restoreCursorCode
hReportCursorPosition h = hPutStr h reportCursorPositionCode

hClearFromCursorToScreenEnd h = hPutStr h clearFromCursorToScreenEndCode
hClearFromCursorToScreenBeginning h
    = hPutStr h clearFromCursorToScreenBeginningCode
hClearScreen h = hPutStr h clearScreenCode

hClearFromCursorToLineEnd h = hPutStr h clearFromCursorToLineEndCode
hClearFromCursorToLineBeginning h = hPutStr h clearFromCursorToLineBeginningCode
hClearLine h = hPutStr h clearLineCode

hScrollPageUp h n = hPutStr h $ scrollPageUpCode n
hScrollPageDown h n = hPutStr h $ scrollPageDownCode n

hSetSGR h sgrs = hPutStr h $ setSGRCode sgrs

hHideCursor h = hPutStr h hideCursorCode
hShowCursor h = hPutStr h showCursorCode

hSetTitle h title = hPutStr h $ setTitleCode title

-- hSupportsANSI :: Handle -> IO Bool
-- (See Common-Include.hs for Haddock documentation)
--
-- Borrowed from an HSpec patch by Simon Hengel
-- (https://github.com/hspec/hspec/commit/d932f03317e0e2bd08c85b23903fb8616ae642bd)
hSupportsANSI h = (&&) <$> hIsTerminalDevice h <*> isNotDumb
 where
  -- cannot use lookupEnv since it only appeared in GHC 7.6
  isNotDumb = (/= Just "dumb") . lookup "TERM" <$> getEnvironment

-- hSupportsANSIWithoutEmulation :: Handle -> IO (Maybe Bool)
-- (See Common-Include.hs for Haddock documentation)
hSupportsANSIWithoutEmulation h =
  Just <$> ((&&) <$> hIsWritable h <*> hSupportsANSI h)

-- getReportedCursorPosition :: IO String
-- (See Common-Include.hs for Haddock documentation)
getReportedCursorPosition =
#ifdef WINDOWS
  catch getReportedCursorPosition' getCPExceptionHandler
 where
  getReportedCursorPosition' = withHandleToHANDLE stdin action
   where
    action hdl = do
      n <- getNumberOfConsoleInputEvents hdl
      if n == 0
        then return ""
        else do
          es <- readConsoleInput hdl n
          return $ stringFromInputEvents es
    stringFromInputEvents = cWcharsToChars . wCharsFromInputEvents
    wCharsFromInputEvents = mapMaybe wCharFromInputEvent
    wCharFromInputEvent e = if isKeyDownEvent
      then Just (unicodeAsciiChar $ keyEventChar keyEventRecord)
      else Nothing
     where
      eventType = inputEventType e
      InputKeyEvent keyEventRecord = inputEvent e
      isKeyDown = keyEventKeyDown keyEventRecord
      isKeyDownEvent = eventType == 1 && isKeyDown

getCPExceptionHandler :: IOException -> IO a
getCPExceptionHandler e = error msg
 where
  msg = "Error: " ++ show e ++ "\nThis error may be avoided by using a " ++
        "console based on the Win32 console of the Windows API, such as " ++
        "Command Prompt or PowerShell."
#else
  bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
    hSetEcho stdin False   -- Turn echo off
    -- If, unexpectedly, no data is available on the console input stream then
    -- the timeout will prevent the getChar blocking. For consistency with the
    -- Windows equivalent, returns "" if the expected information is unavailable.
    fromMaybe "" <$> timeout 500000 get -- 500 milliseconds
 where
  get = do
    c <- getChar
    if c == '\ESC'
      then get' [c]
      else return [c] -- If the first character is not the expected \ESC then
                      -- give up. This provides a modicom of protection against
                      -- unexpected data in the input stream.
  get' s = do
    c <- getChar
    if c /= 'R'
      then get' (c:s) -- Continue building the list, until the expected 'R'
                      -- character is obtained. Build the list in reverse order,
                      -- in order to avoid O(n^2) complexity.
      else return $ reverse (c:s) -- Reverse the order of the built list.
#endif

-- hGetCursorPosition :: Handle -> IO (Maybe (Int, Int))
-- (See Common-Include.hs for Haddock documentation)
hGetCursorPosition h = fmap to0base <$> getCursorPosition
 where
  to0base (row, col) = (row - 1, col - 1)
  getCursorPosition = do
    input <- bracket (hGetBuffering stdin) (hSetBuffering stdin) $ \_ -> do
      hSetBuffering stdin NoBuffering -- set no buffering (the contents of the
                                      -- buffer will be discarded, so this needs
                                      -- to be done before the cursor positon is
                                      -- emitted)
      hReportCursorPosition h
      hFlush h -- ensure the report cursor position code is sent to the
               -- operating system
      getReportedCursorPosition
    case readP_to_S cursorPosition input of
      [] -> return Nothing
      [((row, col),_)] -> return $ Just (row, col)
      (_:_) -> return Nothing

withANSI :: IO a -> IO a
withANSI = (withANSIh stderr) . (withANSIh stdout)

withANSIh :: Handle -> IO a -> IO a
withANSIh h action =
#ifdef WINDOWS
  bracket
    (enableANSIh h)
    resetANSI
    (const action)
#else
  action
#endif

enableANSIh :: Handle -> IO (Maybe HANDLE)
enableANSIh h = do
  isWritable <- hIsWritable h
  if isWritable
    then withHandleToHANDLE h $ withHANDLE
      (throw $ userError "No or no valid handle")  -- Invalid handle or no handle
      enableANSI
    else throw $ userError "Handle not writeable"  -- Not an output handle

-- | This function applies another to the Windows handle, if the handle is
-- valid. If it is invalid, the specified default action is returned.
withHANDLE :: IO a -> (HANDLE -> IO a) -> HANDLE -> IO a
withHANDLE invalid action h =
  if h == iNVALID_HANDLE_VALUE || h == nullHANDLE
    then invalid  -- Invalid handle or no handle
    else action h

-- | This function assumes that the Windows handle is writable.
enableANSI :: HANDLE -> IO (Maybe HANDLE)
enableANSI h = do
  tryMode <- try (getConsoleMode h) :: IO (Either SomeException DWORD)
  case tryMode of
    Left _     -> throw $ userError "No ConHost mode"  -- No ConHost mode
    Right mode -> if mode .&. eNABLE_VIRTUAL_TERMINAL_PROCESSING /= 0
      then pure Nothing  -- VT processing already enabled
      else do
        let mode' = mode .|. eNABLE_VIRTUAL_TERMINAL_PROCESSING
        trySetMode <- try (setConsoleMode h mode')
          :: IO (Either SomeException ())
        case trySetMode of
          Left _   -> throw $ userError "Can't enable VT processing"
          -- Can't enable VT processing
          Right () -> pure $ Just h -- VT processing enabled

resetANSI :: Maybe HANDLE -> IO ()
resetANSI Nothing = pure ()
resetANSI (Just h) = do
  tryMode <- try (getConsoleMode h) :: IO (Either SomeException DWORD)
  case tryMode of
    Left _     -> throw $ userError "No ConHost mode"  -- No ConHost mode
    Right mode -> do
        let mode' = mode .&. (complement eNABLE_VIRTUAL_TERMINAL_PROCESSING)
        trySetMode <- try (setConsoleMode h mode')
          :: IO (Either SomeException ())
        case trySetMode of
          Left _   -> throw $ userError "Can't enable VT processing"
          -- Can't enable VT processing
          Right () -> pure () -- VT processing reset
