module Hish.ANSICode
  (
  -- * The ANSICode
    ANSICode (..)
  -- ** Utilities
  , applyANSI
  -- ** Preset colors
  , fgBlack
  , fgRed
  , fgGreen
  , fgYellow
  , fgBlue
  , fgMagenta
  , fgCyan
  , fgWhite
  --
  , fgBlackL
  , fgRedL
  , fgGreenL
  , fgYellowL
  , fgBlueL
  , fgMagentaL
  , fgCyanL
  , fgWhiteL
  --
  , bgBlack
  , bgRed
  , bgGreen
  , bgYellow
  , bgBlue
  , bgMagenta
  , bgCyan
  , bgWhite
  --
  , bgBlackL
  , bgRedL
  , bgGreenL
  , bgYellowL
  , bgBlueL
  , bgMagentaL
  , bgCyanL
  , bgWhiteL
  ) where

import Data.Monoid (mempty,(<>))

-- | Encoding ANSI-code
data ANSICode = ESC_Bold
              | ESC_Underline
              | ESC_Reverse
              | ESC_Reset
              | ESC_Fg
                  { -- | foreground color
                  fg :: Int}
              | ESC_Bg
                  { -- | background color
                  bg :: Int}
              | ESC_Setup
                  { -- | (foreground, background, other)
                  body :: (Int,Int,Int)}
              deriving (Show,Eq)

instance Monoid ANSICode where
  mempty  = ESC_Setup (0,0,0)
  mappend ESC_Bold      (ESC_Setup (f,b,i)) = ESC_Setup (f,b,1)
  mappend ESC_Underline (ESC_Setup (f,b,i)) = ESC_Setup (f,b,4)
  mappend ESC_Reverse   (ESC_Setup (f,b,i)) = ESC_Setup (b,f,i)
  mappend ESC_Reset     (ESC_Setup (f,b,i)) = ESC_Setup (0,0,0)
  mappend (ESC_Fg   fc) (ESC_Setup (f,b,i)) = ESC_Setup (fc,b,0)
  mappend (ESC_Bg   bc) (ESC_Setup (f,b,i)) = ESC_Setup (f,bc,0)

-- | apply ANSI setting onto the given string.
-- For example,
--
-- >>> applyANSI "haskell" (fgCyanL <> ESC_Bold <> mempty)
-- "\ESC[96;1m\STXhaskell\ESC[0m\STX"
--
applyANSI :: String   -- ^ input string
          -> ANSICode -- ^ ANSI setting
          -> String
applyANSI s (ESC_Setup (f,b,i))
   | f == 0, b == 0, i == 0 = "\ESC[0m\STX"++s
   | f == 0, b == 0, i == 1 = "\ESC[1m\STX"++s++"\ESC[0m\STX"
   | f == 0, b == 0, i == 4 = "\ESC[4m\STX"++s++"\ESC[0m\STX"
   | i == 0 = "\ESC["++(showC f)++(showC b)++"m\STX"++s++"\ESC[0m\STX"
   | i == 1 = "\ESC["++(showC f)++(showC b)++"1m\STX"++s++"\ESC[0m\STX"
   | i == 4 = "\ESC["++(showC f)++(showC b)++"4m\STX"++s++"\ESC[0m\STX"

showC :: Int -> String
showC 0 = ""
showC i = show i++";"

fgBlack   = ESC_Fg 30
fgRed     = ESC_Fg 31
fgGreen   = ESC_Fg 32
fgYellow  = ESC_Fg 33
fgBlue    = ESC_Fg 34
fgMagenta = ESC_Fg 35
fgCyan    = ESC_Fg 36
fgWhite   = ESC_Fg 37

fgBlackL   = ESC_Fg 90
fgRedL     = ESC_Fg 91
fgGreenL   = ESC_Fg 92
fgYellowL  = ESC_Fg 93
fgBlueL    = ESC_Fg 94
fgMagentaL = ESC_Fg 95
fgCyanL    = ESC_Fg 96
fgWhiteL   = ESC_Fg 97

bgBlack   = ESC_Bg 40
bgRed     = ESC_Bg 41
bgGreen   = ESC_Bg 42
bgYellow  = ESC_Bg 43
bgBlue    = ESC_Bg 44
bgMagenta = ESC_Bg 45
bgCyan    = ESC_Bg 46
bgWhite   = ESC_Bg 47

bgBlackL   = ESC_Bg 100
bgRedL     = ESC_Bg 101
bgGreenL   = ESC_Bg 102
bgYellowL  = ESC_Bg 103
bgBlueL    = ESC_Bg 104
bgMagentaL = ESC_Bg 105
bgCyanL    = ESC_Bg 106
bgWhiteL   = ESC_Bg 107
