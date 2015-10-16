module ANSICode
  ( ANSICode (..)
  , esc
  , (<>)
  , applyANSI
  --
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

data ANSICode = ESC_Bold
              | ESC_Underline
              | ESC_Reverse
              | ESC_Reset
              | ESC_Fg        {fg :: Int}
              | ESC_Bg        {bg :: Int}
              | ESC_Setup     {body :: (Int,Int,Int)}
              deriving Show

instance Monoid ANSICode where
  mempty  = ESC_Setup (0,0,0)
  mappend ESC_Bold      (ESC_Setup (f,b,i)) = ESC_Setup (f,b,1)
  mappend ESC_Underline (ESC_Setup (f,b,i)) = ESC_Setup (f,b,4)
  mappend ESC_Reverse   (ESC_Setup (f,b,i)) = ESC_Setup (b,f,i)
  mappend ESC_Reset     (ESC_Setup (f,b,i)) = ESC_Setup (0,0,0)
  mappend (ESC_Fg   fc) (ESC_Setup (f,b,i)) = ESC_Setup (fc,b,0)
  mappend (ESC_Bg   bc) (ESC_Setup (f,b,i)) = ESC_Setup (f,bc,0)


esc :: ANSICode
esc = mempty

(<>) :: ANSICode -> ANSICode -> ANSICode
x <> y = mappend x y
infixr 6 <>

applyANSI :: String -> ANSICode -> String
applyANSI s (ESC_Setup (f,b,i)) | f == 0, b == 0, i == 0 = "\ESC[0m\STX"++s
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

fgBlackL   = ESC_Fg 90 -- Dark gray
fgRedL     = ESC_Fg 91 -- Light red
fgGreenL   = ESC_Fg 92 -- Light green
fgYellowL  = ESC_Fg 93 -- Light yellow
fgBlueL    = ESC_Fg 94 -- Light blue
fgMagentaL = ESC_Fg 95 -- Light magenta
fgCyanL    = ESC_Fg 96 -- Light cyan
fgWhiteL   = ESC_Fg 97 -- White

bgBlack   = ESC_Bg 40
bgRed     = ESC_Bg 41
bgGreen   = ESC_Bg 42
bgYellow  = ESC_Bg 43
bgBlue    = ESC_Bg 44
bgMagenta = ESC_Bg 45
bgCyan    = ESC_Bg 46
bgWhite   = ESC_Bg 47

bgBlackL   = ESC_Bg 100 -- Dark gray
bgRedL     = ESC_Bg 101 -- Light red
bgGreenL   = ESC_Bg 102 -- Light green
bgYellowL  = ESC_Bg 103 -- Light yellow
bgBlueL    = ESC_Bg 104 -- Light blue
bgMagentaL = ESC_Bg 105 -- Light magenta
bgCyanL    = ESC_Bg 106 -- Light cyan
bgWhiteL   = ESC_Bg 107 -- White
