#include "Common-Safe-Haskell.hs"

module System.Console.ANSI.Codes.Internal
  (
    -- * Utilities
    colorToCode, sgrToCode
  ) where

import Data.Colour.SRGB (toSRGB24, RGB (..))

import System.Console.ANSI.Types

-- | 'colorToCode' @color@ returns the 0-based index of the color (one of the
-- eight colors in the ANSI standard).
--
-- @since 0.6.3
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
--
-- @since 0.6.3
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
  SetPaletteColor Foreground index -> [38, 5, fromIntegral index]
  SetPaletteColor Background index -> [48, 5, fromIntegral index]
  SetRGBColor Foreground color -> [38, 2] ++ toRGB color
  SetRGBColor Background color -> [48, 2] ++ toRGB color
  SetDefaultColor Foreground -> [39]
  SetDefaultColor Background -> [49]
 where
  toRGB color = let RGB r g b = toSRGB24 color
                in  map fromIntegral [r, g, b]
