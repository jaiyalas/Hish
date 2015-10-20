{-# OverloadedStrings #-}

module Main where

import Hish.ANSICode
import qualified Hish.SysInfo as HS
import qualified Hish.VCS as VCS
-- for manipulating ANSI code
import Data.Monoid (mempty,(<>))
-- for checking the existence of vcs
import System.Directory (doesDirectoryExist)

{- +++++++++++++++++++++++++++++++
-- Is this helpful?
import System.Environment (getEnv    -- String -> IO String
                          ,setEnv    -- String -> String -> IO ()
                          ,lookupEnv -- String -> IO (Maybe String)
                          )
+++++++++++++++++++++++++++++++ -}

_prompt_symbol = ">"
_pwdWidth = 60

main :: IO ()
main = do
   printUserInfo
   printWorkingTree
   putStr " "
   existGit <- doesDirectoryExist ".git"
   if existGit   then printVCSInfo VCS.Git   else return ()
   existDarcs <- doesDirectoryExist "_darcs"
   if existDarcs then printVCSInfo VCS.Darcs else return ()
   putStr $ applyANSI (_prompt_symbol++" ") $ mempty

--
printUserInfo :: IO ()
printUserInfo = do
   _ts <- HS.time "%H:%M"
   putStr $ applyANSI (_ts) $ fgBlackL <> mempty
   putStr $ applyANSI "(" $ fgBlackL <> mempty
   _un <- HS.uid
   case _un of
      Nothing -> return ()
      Just un -> putStr $ applyANSI (un) $ mempty
   putStr $ applyANSI ")" $ fgBlackL <> mempty
   -- _hn <- HS.hostname
   -- case _hn of
   --    Nothing -> return ()
   --    Just hn -> putStr $ applyANSI ("@"++hn) $ fgBlackL <> mempty
--
printWorkingTree :: IO ()
printWorkingTree = do
   _wd <- HS.pwd _pwdWidth
   case _wd of
      Nothing -> return ()
      Just wd -> putStr $ applyANSI wd $ fgGreen <> mempty
--
printVCSInfo :: (Show a, VCS.VCS a) => a -> IO ()
printVCSInfo vcs = do
   _br <- HS.branch vcs
   (_st, _ah, _bh) <- HS.status vcs
   putStr $ show vcs
   putStr "["
   case _br of
      Just br -> putStr $ applyANSI br $ fgWhiteL <> ESC_Bold <> mempty
      Nothing -> return ()
   case _st of
      Just "*" -> putStr $ applyANSI "*" $ ESC_Bold <> fgRed <> mempty
      Just "?" -> putStr $ applyANSI "?" $ ESC_Bold <> fgYellow <> mempty
      Just "#" -> putStr $ applyANSI "#" $ ESC_Bold <> fgGreen <> mempty
      _ -> putStr ""
   case _ah of
      Nothing -> return ()
      Just ah -> putStr $ applyANSI ("+"++ah) $ fgRedL <> mempty
   case _bh of
      Nothing -> return ()
      Just bh -> putStr $ applyANSI ("-"++bh) $ fgGreenL <> mempty
   putStr "]"
