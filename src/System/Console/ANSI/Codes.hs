-- | This module exports functions that return 'String' values containing codes
-- in accordance with the \'ANSI\' standards for control character sequences
-- described in the documentation of module "System.Console.ANSI".
--
-- The module "System.Console.ANSI" exports functions with the same names as
-- those in this module. On versions of Windows before Windows 10, the terminal
-- in use may not be ANSI-capable. When that is the case, the same-named
-- functions exported by module "System.Console.ANSI" return \"\", for the
-- reasons set out in the documentation of that module.
--
-- Consequently, if module "System.Console.ANSI" is also imported, this module
-- is intended to be imported qualified, to avoid name clashes with those
-- functions. For example:
--
-- > import qualified System.Console.ANSI.Codes as ANSI
--
-- This module also exports some utility functions, including functions that
-- facilitate the direct use of \'ANSI\' codes on native Windows 10 terminals.
--
module System.Console.ANSI.Codes
  (
    -- * Basic data types
    module System.Console.ANSI.Types

    -- * Cursor movement by character
  , cursorUpCode, cursorDownCode, cursorForwardCode, cursorBackwardCode

    -- * Cursor movement by line
  , cursorUpLineCode, cursorDownLineCode

    -- * Directly changing cursor position
  , setCursorColumnCode, setCursorPositionCode

    -- * Saving, restoring and reporting cursor position
  , saveCursorCode, restoreCursorCode, reportCursorPositionCode

    -- * Clearing parts of the screen
  , clearFromCursorToScreenEndCode, clearFromCursorToScreenBeginningCode
  , clearScreenCode, clearFromCursorToLineEndCode
  , clearFromCursorToLineBeginningCode, clearLineCode

    -- * Scrolling the screen
  , scrollPageUpCode, scrollPageDownCode

    -- * Select Graphic Rendition mode: colors and other whizzy stuff
  , setSGRCode

    -- * Cursor visibilty changes
  , hideCursorCode, showCursorCode

    -- * Changing the title
    -- | Thanks to Brandon S. Allbery and Curt Sampson for pointing me in the
    -- right direction on xterm title setting on haskell-cafe. The "0"
    -- signifies that both the title and "icon" text should be set: i.e. the
    -- text for the window in the Start bar (or similar) as well as that in
    -- the actual window title. This is chosen for consistent behaviour
    -- between Unixes and Windows.
  , setTitleCode

    -- * Utilities
  , colorToCode, csi, sgrToCode
  , withCurrentModes
  , withVTProcessing

  ) where

import Control.Exception (SomeException, bracket, try)
import Data.List (intersperse)
import Data.Bits ((.|.))
import Data.Colour.SRGB (toSRGB24, RGB (..))

import System.Console.ANSI.Types
import System.Console.ANSI.Windows.Foreign (DWORD,
  eNABLE_VIRTUAL_TERMINAL_INPUT, eNABLE_VIRTUAL_TERMINAL_PROCESSING,
  getConsoleMode,  setConsoleMode, withHandleToHANDLE)
import System.IO (stderr, stdin, stdout)

-- | 'csi' @parameters controlFunction@, where @parameters@ is a list of 'Int',
-- returns the control sequence comprising the control function CONTROL
-- SEQUENCE INTRODUCER (CSI) followed by the parameter(s) (separated by \';\')
-- and ending with the @controlFunction@ character(s) that identifies the
-- control function.
csi :: [Int]  -- ^ List of parameters for the control sequence
    -> String -- ^ Character(s) that identify the control function
    -> String
csi args code = "\ESC[" ++ concat (intersperse ";" (map show args)) ++ code

-- | 'colorToCode' @color@ returns the 0-based index of the color (one of the
-- eight colors in the standard).
colorToCode :: Color -> Int
colorToCode color = case color of
  Black   -> 0
  Red     -> 1
  Green   -> 2
  Yellow  -> 3
  Blue    -> 4
  Magenta -> 5
  Cyan    -> 6
  White   -> 7

-- | 'sgrToCode' @sgr@ returns the parameter of the SELECT GRAPHIC RENDITION
-- (SGR) aspect identified by @sgr@.
sgrToCode :: SGR -- ^ The SGR aspect
          -> [Int]
sgrToCode sgr = case sgr of
  Reset -> [0]
  SetConsoleIntensity intensity -> case intensity of
    BoldIntensity   -> [1]
    FaintIntensity  -> [2]
    NormalIntensity -> [22]
  SetItalicized True  -> [3]
  SetItalicized False -> [23]
  SetUnderlining underlining -> case underlining of
    SingleUnderline -> [4]
    DoubleUnderline -> [21]
    NoUnderline     -> [24]
  SetBlinkSpeed blink_speed -> case blink_speed of
    SlowBlink   -> [5]
    RapidBlink  -> [6]
    NoBlink     -> [25]
  SetVisible False -> [8]
  SetVisible True  -> [28]
  SetSwapForegroundBackground True  -> [7]
  SetSwapForegroundBackground False -> [27]
  SetColor Foreground Dull color  -> [30 + colorToCode color]
  SetColor Foreground Vivid color -> [90 + colorToCode color]
  SetColor Background Dull color  -> [40 + colorToCode color]
  SetColor Background Vivid color -> [100 + colorToCode color]
  SetRGBColor Foreground color -> [38, 2] ++ toRGB color
  SetRGBColor Background color -> [48, 2] ++ toRGB color
 where
  toRGB color = let RGB r g b = toSRGB24 color
                in  map fromIntegral [r, g, b]

cursorUpCode, cursorDownCode, cursorForwardCode, cursorBackwardCode
  :: Int -- ^ Number of lines or characters to move
  -> String
cursorUpCode n = csi [n] "A"
cursorDownCode n = csi [n] "B"
cursorForwardCode n = csi [n] "C"
cursorBackwardCode n = csi [n] "D"

cursorDownLineCode, cursorUpLineCode :: Int -- ^ Number of lines to move
                                     -> String
cursorDownLineCode n = csi [n] "E"
cursorUpLineCode n = csi [n] "F"

setCursorColumnCode :: Int -- ^ 0-based column to move to
                    -> String
setCursorColumnCode n = csi [n + 1] "G"

setCursorPositionCode :: Int -- ^ 0-based row to move to
                      -> Int -- ^ 0-based column to move to
                      -> String
setCursorPositionCode n m = csi [n + 1, m + 1] "H"

saveCursorCode, restoreCursorCode, reportCursorPositionCode :: String
saveCursorCode = "\ESC7"
restoreCursorCode = "\ESC8"
reportCursorPositionCode = csi [] "6n"

clearFromCursorToScreenEndCode, clearFromCursorToScreenBeginningCode,
  clearScreenCode :: String
clearFromCursorToLineEndCode, clearFromCursorToLineBeginningCode,
  clearLineCode :: String

clearFromCursorToScreenEndCode = csi [0] "J"
clearFromCursorToScreenBeginningCode = csi [1] "J"
clearScreenCode = csi [2] "J"
clearFromCursorToLineEndCode = csi [0] "K"
clearFromCursorToLineBeginningCode = csi [1] "K"
clearLineCode = csi [2] "K"

scrollPageUpCode, scrollPageDownCode :: Int -- ^ Number of lines to scroll by
                                     -> String
scrollPageUpCode n = csi [n] "S"
scrollPageDownCode n = csi [n] "T"

setSGRCode :: [SGR] -- ^ Commands: these will typically be applied on top of the
                    -- current console SGR mode. An empty list of commands is
                    -- equivalent to the list @[Reset]@. Commands are applied
                    -- left to right.
           -> String
setSGRCode sgrs = csi (concatMap sgrToCode sgrs) "m"

hideCursorCode, showCursorCode :: String
hideCursorCode = csi [] "?25l"
showCursorCode = csi [] "?25h"


-- | XTerm control sequence to set the Icon Name and Window Title.
setTitleCode :: String -- ^ New Icon Name and Window Title
             -> String
setTitleCode title = "\ESC]0;" ++ filter (/= '\007') title ++ "\007"

-- | On Unix-like operating systems, @withCurrentModes = id@.
--
-- On Windows, when @withCurrentModes action@ is performed, after @action@ is
-- performed, if any of 'stdin', 'stdout' and 'stderr' are handles to a native
-- Windows console, their console mode is restored.
--
-- @since 0.8.2
withCurrentModes :: IO a -> IO a
withCurrentModes action = bracket
  getModes
  setModes
  (const action)
 where
  getModes = do
    inMode <- getMode stdin
    outMode <- getMode stdout
    errMode <- getMode stderr
    pure (inMode, outMode, errMode)
  setModes (inMode, outMode, errMode) = do
    setMode inMode stdin
    setMode outMode stdout
    setMode errMode stderr
  getMode h = do
    tryMode <- try (withHandleToHANDLE h getConsoleMode)
      :: IO (Either SomeException DWORD)
    case tryMode of
      Left _  -> pure Nothing
      Right mode -> pure $ Just mode
  setMode Nothing _ = pure ()
  setMode (Just mode) h = withHandleToHANDLE h (`setConsoleMode` mode)

-- | On Unix-like operating systems, @withVTProcessing = id@.
--
-- On Windows, when @withVTProcessing action@ is performed, @action@ is
-- performed after enabling virtual terminal (VT) processing by 'stdin',
-- 'stdout' and 'stderr', if any of them is a handle to a native Windows 10
-- console. The current modes of 'stdin', 'stdout' and 'stderr' are restored.
-- For example:
--
-- > main = withVTProcessing $ do
-- >   putStr $ setSGRCode [SetColor Foreground Dull Red]
-- >   putStr "This is red!"
-- >   putStrLn $ setSGRCode [Reset]
--
-- (Attempting to enable VT processing on versions of Windows before Windows 10
-- has no effect.)
--
-- @since 0.8.2
withVTProcessing :: IO a -> IO a
withVTProcessing action = withCurrentModes $ do
  enableVTProcessing
  action
 where
  enableVTProcessing = do
    setFlag stdin eNABLE_VIRTUAL_TERMINAL_INPUT
    setFlag stdout eNABLE_VIRTUAL_TERMINAL_PROCESSING
    setFlag stderr eNABLE_VIRTUAL_TERMINAL_PROCESSING
  setFlag hnd flag = withHandleToHANDLE hnd $ \h -> do
    tryMode <- try (getConsoleMode h) :: IO (Either SomeException DWORD)
    case tryMode of
      Left _     -> pure ()
      Right mode -> setConsoleMode h (mode .|. flag)
