module Hish.GitInfo
  ( status
  , branch
  , pwd
  , uid
  ) where

import System.Process
import System.Exit
import Data.List (lines,unlines)
import Data.Char (isSpace)
import qualified Data.String.Utils as S (split,replace,join)

-- | Obtain current git-status.
-- Returning "#" for clean working directory.
-- Returning "*" for dirty working directory.
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
-- | Obtain current name of git-branch
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
-- | Obtain current working directory
pwd :: Int -- ^ threshold of shortening
    -> IO String
pwd width = do
  (code,out,_) <- readProcessWithExitCode "pwd" [] ""
  name <- uid
  case code of
    ExitFailure _ -> return ""
    ExitSuccess   -> return $
      ( (\str -> if (head str) == '~' then str else '/':str )
      . (\str -> if (length str) > width
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
-- | Obtain username
uid :: IO String
uid = do
  (code,name,_) <- readProcessWithExitCode "whoami" [] ""
  case code of
    ExitFailure _ -> return ""
    ExitSuccess   -> return $ filter (/='\n') $ name
