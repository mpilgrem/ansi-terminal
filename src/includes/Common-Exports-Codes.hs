-- This file contains code that is common to modules
-- System.Console.ANSI.Codes and System.Console.ANSI.Codes.Text, namely the
-- module exports and the associated Haddock documentation.

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
