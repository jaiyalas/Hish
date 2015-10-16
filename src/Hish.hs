module Main where

import System.Process
import System.Exit
import Data.List (lines,unlines)
import Data.Char (isSpace)
import qualified Data.String.Utils as S (split,replace,join)

import ANSICode
--

_prompt_symbol = ">"
_pwdWidth = 60

main :: IO ()
main = do
  st <- status
  br <- branch
  wd <- pwd
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
--
status :: IO String
status = do
  (code,out,_) <- readProcessWithExitCode "git" ["status","--porcelain"] ""
  case code of
    ExitFailure _ -> return ""
    ExitSuccess   -> return $
      ((\b->if b then "#" else "*")
      .null
      .map head.lines
      ) out
--
branch :: IO String
branch = do
  (code,out,_) <- readProcessWithExitCode "git" ["branch"] ""
  case code of
    ExitFailure _ -> return ""
    ExitSuccess   -> return $
      (drop 1
      .head.takeWhile (('*'==).head) -- take current br
      .lines.filter (/=' ') -- rm ' ';split by '\n'
      ) out
--
pwd :: IO String
pwd = do
  (code,out,_) <- readProcessWithExitCode "pwd" [] ""
  name <- uid
  case code of
    ExitFailure _ -> return ""
    ExitSuccess   -> return $
      ( (\str -> if (head str) == '~' then str else '/':str )
      . (\str -> if (length str) > _pwdWidth
                    then pwdShorten $ S.split "/" str
                    else str)
      . S.replace ("/Users/"++name) "~"
      . filter (/='\n')
      ) $ out
--
pwdShorten :: [String] -> String
pwdShorten [] = ""
pwdShorten [l] = l
pwdShorten (x:xs) = (head x) : '/' : pwdShorten xs
--
uid :: IO String
uid = do
  (code,name,_) <- readProcessWithExitCode "whoami" [] ""
  case code of
    ExitFailure _ -> return ""
    ExitSuccess   -> return $ filter (/='\n') $ name
