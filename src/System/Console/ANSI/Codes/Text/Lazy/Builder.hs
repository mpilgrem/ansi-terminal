#include "Common-Safe-Haskell.hs"
{-# LANGUAGE OverloadedStrings #-}

{-| This module exports functions that return 'Data.Text.Lazy.Builder.Builder'
values containing codes in accordance with the \'ANSI\' standards for control
character sequences described in the documentation of module
"System.Console.ANSI".

The module "System.Console.ANSI" exports functions with the same names as those
in this module and that return 'String' values. On some versions of Windows, the
terminal in use may not be ANSI-capable. When that is the case, the same-named
functions exported by module "System.Console.ANSI" return \"\", for the reasons
set out in the documentation of that module.

Consequently, if module "System.Console.ANSI" is also imported, this module is
intended to be imported qualified, to avoid name clashes with those functions.
For example:

> import qualified System.Console.ANSI.Codes.Text.Lazy.Builder as ANSI
-}
module System.Console.ANSI.Codes.Text.Lazy.Builder
  (
#include "Common-Exports-Codes.hs"
  ) where

import Data.List (intersperse)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid (mconcat)
#if MIN_VERSION_base(4,5,0)
import Data.Monoid ((<>))
#else
import Data.Monoid (Monoid, mappend)
#endif
#endif

import Data.Text (Text)
import qualified Data.Text as T (filter)
import Data.Text.Lazy.Builder (Builder, fromText)
import Data.Text.Lazy.Builder.Int (decimal)
import System.Console.ANSI.Codes.Internal (colorToCode, sgrToCode)
import System.Console.ANSI.Types

#if !MIN_VERSION_base(4,5,0)
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif

-- | 'csi' @parameters controlFunction@, where @parameters@ is a list of 'Int',
-- returns the control sequence comprising the control function CONTROL
-- SEQUENCE INTRODUCER (CSI) followed by the parameter(s) (separated by \';\')
-- and ending with the @controlFunction@ character(s) that identifies the
-- control function.
--
-- @since 0.11.1
csi :: [Int]  -- ^ List of parameters for the control sequence
    -> Builder -- ^ Character(s) that identify the control function
    -> Builder
csi args code = "\ESC[" <> mconcat (intersperse ";" (map decimal args)) <> code

-- | @since 0.11.1
cursorUpCode, cursorDownCode, cursorForwardCode, cursorBackwardCode
  :: Int -- ^ Number of lines or characters to move
  -> Builder
cursorUpCode n = csi [n] "A"
cursorDownCode n = csi [n] "B"
cursorForwardCode n = csi [n] "C"
cursorBackwardCode n = csi [n] "D"

-- | @since 0.11.1
cursorDownLineCode, cursorUpLineCode :: Int -- ^ Number of lines to move
                                     -> Builder
cursorDownLineCode n = csi [n] "E"
cursorUpLineCode n = csi [n] "F"

-- | Code to move the cursor to the specified column. The column numbering is
-- 0-based (that is, the left-most column is numbered 0).
--
-- @since 0.11.1
setCursorColumnCode :: Int -- ^ 0-based column to move to
                    -> Builder
setCursorColumnCode n = csi [n + 1] "G"

-- | Code to move the cursor to the specified position (row and column). The
-- position is 0-based (that is, the top-left corner is at row 0 column 0).
--
-- @since 0.11.1
setCursorPositionCode :: Int -- ^ 0-based row to move to
                      -> Int -- ^ 0-based column to move to
                      -> Builder
setCursorPositionCode n m = csi [n + 1, m + 1] "H"

-- | @since 0.11.1
saveCursorCode, restoreCursorCode :: Builder
saveCursorCode = "\ESC7"
restoreCursorCode = "\ESC8"

-- | Code to emit the cursor position into the console input stream, immediately
-- after being recognised on the output stream, as:
-- @ESC [ \<cursor row> ; \<cursor column> R@
--
-- Note that the information that is emitted is 1-based (the top-left corner is
-- at row 1 column 1) but 'setCursorPositionCode' is 0-based.
--
-- In isolation of 'getReportedCursorPosition' or 'getCursorPosition', this
-- function may be of limited use on Windows operating systems because of
-- difficulties in obtaining the data emitted into the console input stream.
-- The function 'hGetBufNonBlocking' in module "System.IO" does not work on
-- Windows. This has been attributed to the lack of non-blocking primatives in
-- the operating system (see the GHC bug report #806 at
-- <https://ghc.haskell.org/trac/ghc/ticket/806>).
--
-- @since 0.11.1
reportCursorPositionCode :: Builder

reportCursorPositionCode = csi [] "6n"

-- | @since 0.11.1
clearFromCursorToScreenEndCode, clearFromCursorToScreenBeginningCode,
  clearScreenCode, clearFromCursorToLineEndCode,
  clearFromCursorToLineBeginningCode,
  clearLineCode :: Builder

clearFromCursorToScreenEndCode = csi [0] "J"
clearFromCursorToScreenBeginningCode = csi [1] "J"
clearScreenCode = csi [2] "J"
clearFromCursorToLineEndCode = csi [0] "K"
clearFromCursorToLineBeginningCode = csi [1] "K"
clearLineCode = csi [2] "K"

-- | @since 0.11.1
scrollPageUpCode, scrollPageDownCode :: Int -- ^ Number of lines to scroll by
                                     -> Builder
scrollPageUpCode n = csi [n] "S"
scrollPageDownCode n = csi [n] "T"

-- | @since 0.11.1
setSGRCode :: [SGR] -- ^ Commands: these will typically be applied on top of the
                    -- current console SGR mode. An empty list of commands is
                    -- equivalent to the list @[Reset]@. Commands are applied
                    -- left to right.
           -> Builder
setSGRCode sgrs = csi (concatMap sgrToCode sgrs) "m"

-- | @since 0.11.1
hideCursorCode, showCursorCode :: Builder
hideCursorCode = csi [] "?25l"
showCursorCode = csi [] "?25h"


-- | XTerm control sequence to set the Icon Name and Window Title.
--
-- @since 0.11.1
setTitleCode :: Text -- ^ New Icon Name and Window Title
             -> Builder
setTitleCode title = "\ESC]0;" <> fromText (T.filter (/= '\007') title) <>
                     "\007"
