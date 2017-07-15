{-# LANGUAGE BangPatterns #-}
module Hish.SysInfo
  (
  -- * Basic information
  -- ** name
    uid
  , hostname
  -- ** working directory
  , pwd
  -- ** time and date
  , time
  , date
  -- ** version control system
  , status
  , branch
  , isRepo
  -- * Primitive functions
  , simpleCmd
  , argedCmd
  ) where

import qualified System.Process as SP
import System.Exit (ExitCode (..))
import Data.List (lines)
import qualified Data.String.Utils as S (split,replace)
--
import qualified Data.Time.LocalTime as LT (getZonedTime)
import Data.Time.Format as TF (formatTime, defaultTimeLocale)
--
import Hish.VCS
--
import Debug.Trace
--

-- | return username
uid :: IO (Maybe String)
uid = simpleCmd (Just . init) "whoami"

-- | return hostname
hostname :: IO (Maybe String)
hostname = argedCmd (Just . init) "hostname" ["-s"]

{- ========================================== -}

-- | return current working directory
pwd :: Int -- ^ threshold of shortening
    -> IO (Maybe String)
pwd width = do
  (code,out,_) <- exeCmd "pwd" [] ""
  mName <- uid
  case (code, mName) of
    (ExitSuccess, Just name)   -> return $ return $
      ( (\str -> if (head str) == '~' then str else '/':str )
      . (\str -> if (length str) > width
                    then shortDir $ S.split "/" str
                    else str)
      . S.replace ("/Users/"++name) "~"
      . filter (/='\n')
      ) $ out
    otherwise -> return Nothing

-- | concating the given list of name into a path.
-- Notice that, this function will __NOT__ add root, __/__, or home, __~__,
-- to the leftmost position. For example,
--
-- >>> pwdShorten ["A","B","C"]
-- "A/B/C"
--
shortDir :: [String] -- ^ a list of folder name
           -> String
shortDir [] = ""
shortDir [l] = l
shortDir (x:xs) = (head x) : '/' : shortDir xs

{- ========================================== -}

-- | return time or date for given format
--
-- >>> time "%H:%M"
-- 13:15
--
-- >>> time "%Y-%b-%d"
-- 2015-Oct-20
time :: String -- ^ format (read "Data.Time.Format" for more detail)
     -> IO String
time format = do
   ztime <- LT.getZonedTime
   return $ TF.formatTime TF.defaultTimeLocale format ztime

-- | just an alias of 'time'
date :: String -> IO String
date = time

{- ========================================== -}

-- | get current status.
status :: VCS a => a        -- ^ version control system
       -> IO (Maybe String,
              Maybe String,
              Maybe String) -- ^ (cleanliness, ahead, behind)
status vcs = do
   maybeText <- argedCmd (Just . id) (statusCmd vcs) (statusArgs vcs)
   let !x = traceShowId maybeText
   case maybeText of
      Nothing -> return
         ( Nothing
         , Nothing
         , Nothing )
      (Just text) -> return
         ( vcsCleanliness vcs text
         , vcsAhead       vcs text
         , vcsBehind      vcs text )

-- | get current name of git-branch
branch :: VCS a => a -- ^ version control system
       -> IO (Maybe String)  -- ^ current branch name
branch vcs =  argedCmd (vcsCurrentBranch vcs) (branchCmd vcs) (branchArgs vcs)

-- | using status to verifying the existence of repository
isRepo :: VCS a => a -> IO Bool
isRepo vcs = do
   (code,_,_) <- exeCmd (statusCmd vcs) (statusArgs vcs) ""
   case code of
      ExitFailure _ -> return False
      ExitSuccess   -> return True

{- ========================================== -}

exeCmd :: String
       -> [String]
       -> String
       -> IO (ExitCode, String, String)
exeCmd = SP.readProcessWithExitCode

-- | execute command with text handler but without args
simpleCmd :: (String -> Maybe String) -- ^ translate raw text to information
          -> String                   -- ^ console command
          -> IO (Maybe String)
simpleCmd handler cmd = argedCmd handler cmd []

-- | execute command with text handler and args
argedCmd :: (String -> Maybe String) -- ^ translate raw text to information
          -> String                  -- ^ console command
          -> [String]                -- ^ command arguments
          -> IO (Maybe String)
argedCmd handler cmd args = do
   (code,stdout,_) <- exeCmd cmd args ""
   case code of
     ExitFailure _ -> return Nothing
     ExitSuccess   -> return $ (Just stdout) >>= handler
