module Main where

import Hish.ANSICode
import Hish.SysInfo

--
_prompt_symbol = ">"
_pwdWidth = 60

main :: IO ()
main = do
  st <- status
  br <- branch
  wd <- pwd _pwdWidth
  -- show GIT BRANCH
  putStr $ applyANSI wd $ fgGreen <> esc
  case br of
    "" -> putStr ""
    _  -> do
      putStr " "
      putStr $ applyANSI br $ fgBlueL <> esc
  -- show GIT STATUS
  case st of
    "*" -> putStr $ applyANSI st $ ESC_Bold <> fgRedL <> esc
    "#" -> putStr $ applyANSI st $ ESC_Bold <> fgGreen <> esc
    _   -> putStr ""
  -- show PROMPT SYMBOL
  putStr $ applyANSI (_prompt_symbol++" ") $ fgWhiteL <> esc
