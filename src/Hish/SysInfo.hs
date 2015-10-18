module Hish.SysInfo
  ( -- * A
    uid
  , hostname
  , pwd
  , pwdShorten
  -- * B
  , simpleCmd
  , argedCmd
  ) where

import qualified System.Process as SP
import System.Exit (ExitCode (..))
import Data.List (lines,unlines)
import Data.Char (isSpace)
import qualified Data.String.Utils as S (split,replace,join)
import Text.Regex.TDFA ((=~))


-- | Obtain username
uid :: IO (Maybe String)
uid = simpleCmd init "whoami"

-- | Obgain hostname
hostname :: IO (Maybe String)
hostname = simpleCmd init "hostname"

-- | Obtain current working directory
pwd :: Int -- ^ threshold of shortening
    -> IO (Maybe String)
pwd width = do
  (code,out,_) <- exeCmd "pwd" [] ""
  mName <- uid
  case (code, mName) of
    (ExitSuccess, Just name)   -> return $ return $
      ( (\str -> if (head str) == '~' then str else '/':str )
      . (\str -> if (length str) > width
                    then pwdShorten $ S.split "/" str
                    else str)
      . S.replace ("/Users/"++name) "~"
      . filter (/='\n')
      ) $ out
    otherwise -> return Nothing

-- | Concats the given list of name into a path.
-- Notice that, this function will NOT add root '/' or home '~' directory
-- on the leftmost position. For example, `pwdShorten ["A","B","C"] = "A/B/C"`.
pwdShorten :: [String] -- ^ a list of folder name
           -> String
pwdShorten [] = ""
pwdShorten [l] = l
pwdShorten (x:xs) = (head x) : '/' : pwdShorten xs

exeCmd :: String -> [String] -> String -> IO String
exeCmd = SP.readProcessWithExitCode

-- | Execute command with text handler but without args
simpleCmd :: (String -> String) -- ^ how to handle the result
          -> String             -- ^ command
          -> IO (Maybe String)
simpleCmd handler cmd = argedCmd handler cmd []

-- | Execute command with text handler and args
argedCmd :: (String -> String) -- ^ how to handle the result
          -> String             -- ^ command
          -> [String]           -- ^ arguments
          -> IO (Maybe String)
argedCmd handler cmd args = do
   (code,stdout,_) <- exeCmd cmd args ""
   case code of
     ExitFailure _ -> return Nothing
     ExitSuccess   -> return $ famp handler stdout

--




-- | Obtain current git-status.
-- Returning "#" for clean working directory.
-- Returning "*" for dirty working directory.
status :: VCS a => a -> IO (Maybe String)
status = argedCmd handler "git" ["status","--porcelain"] ""
   where handler =
      (\b->if b then "#" else "*")
      . null
      . map head
      . lines

{-
$> let text = "## master...origin/master [ahead 21, behind 13]"
$> text =~ "(ahead [0-9]+|behind [0-9]+)" :: String
=> "ahead 21"

,s =~ "behind [0-9]+" :: String
-}

tracking :: VCS a => a        -- ^ version control system
         -> IO (Maybe String,
                Maybe String,
                Maybe String) -- ^ (status, ahead, behind)
tracking = argedCmd handler "git" ["status","--porcelain","-sb"] ""
   where handler =
      (undefined)
      {-
      . null
      . filter (\s -> )
      . S.splitWs str
      . filter ((/='[')||(/=']')||(/=','))
      . head
      -}
      . lines
--
aheadFilter :: String -> Maybe String
aheadFilter s =
   case (s =~ "ahead [0-9]+"  :: String) of
      []    -> Nothing
      ahead -> S.replace "ahead " "" ahead
behindFilter :: String -> Maybe Int
behindFilter s =
   case (s =~ "behind [0-9]+"  :: String) of
      []    -> Nothing
      behind -> S.replace "behind " "" behind

-- | Obtain current git-status.
-- Returning "#" for clean working directory.
-- Returning "*" for dirty working directory.
git_status :: IO String
git_status = do
  (code,out,_) <- readProcessWithExitCode "git" ["status","--porcelain"] ""
  case code of
    ExitFailure _ -> return ""
    ExitSuccess   -> return $
      ((\b->if b then "#" else "*")
      .null
      .map head.lines
      ) out


-- | Obtain current name of git-branch
git_branch :: IO String
git_branch = do
  (code,out,_) <- readProcessWithExitCode "git" ["branch"] ""
  case code of
    ExitFailure _ -> return ""
    ExitSuccess   -> return $
      (drop 1
      .head.takeWhile (('*'==).head) -- take current br
      .lines.filter (/=' ') -- rm ' ';split by '\n'
      ) out
