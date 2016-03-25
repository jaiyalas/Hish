module Main where

import Hish.ANSICode
import qualified Hish.SysInfo as HS
import qualified Hish.VCS as VCS
-- for manipulating ANSI code
import Data.Monoid (mempty,(<>))

_prompt_symbol = ">"
_pwdWidth = 45

main :: IO ()
main = do
   printSTime
   -- printUID
   -- printHostname
   putStr " "
   printWorkingTree
   putStr " "
   --
   safePrintVCS VCS.Git
   -- safePrintVCS VCS.Darcs
   --
   putStr $ applyANSI (_prompt_symbol++" ") $ mempty

--
printVCSInfo :: (Show a, VCS.VCS a) => a -> IO ()
printVCSInfo vcs = do
   _br <- HS.branch vcs
   (_st, _ah, _bh) <- HS.status vcs
   --putStr $ show vcs
   -- putStr "["
   case _br of
      Just br -> putStr $ applyANSI br $ fgWhiteL <> ESC_Bold <> mempty
      Nothing -> return ()
   case _st of
      Just "*" -> putStr $ applyANSI "*" $ ESC_Bold <> fgYellowL <> mempty
      Just "?" -> putStr $ applyANSI "?" $ ESC_Bold <> fgMagentaL <> mempty
      Just "#" -> putStr $ applyANSI "#" $ ESC_Bold <> fgBlueL <> mempty
      _ -> putStr ""
   case _ah of
      Nothing -> return ()
      Just ah -> putStr $ applyANSI (if ah=="" then "" else "+"++ah) $
         fgRedL <> mempty
   case _bh of
      Nothing -> return ()
      Just bh -> putStr $ applyANSI (if bh=="" then "" else "-"++bh) $
         fgGreenL <> mempty
   -- putStr "]"
--
safePrintVCS :: (Show a, VCS.VCS a) => a -> IO ()
safePrintVCS vcs = do
   b1 <- VCS.installed vcs
   case b1 of
      True -> do
         b2 <- HS.isRepo vcs
         if b2 then printVCSInfo vcs else return ()
      False -> return ()
--
printSTime :: IO ()
printSTime = do
   _ts <- HS.time "%H:%M"
   putStr $ applyANSI (_ts) $ fgBlackL <> mempty

--
printUID :: IO ()
printUID = do
   _un <- HS.uid
   case _un of
      Nothing -> return ()
      Just un -> do
           putStr $ applyANSI "(" $ fgBlackL <> mempty
           putStr $ applyANSI (un) $ mempty
           putStr $ applyANSI ")" $ fgBlackL <> mempty
--
printHostname :: IO ()
printHostname = do
   _hn <- HS.hostname
   case _hn of
      Nothing -> return ()
      Just hn -> putStr $ applyANSI ("@"++hn) $ fgBlackL <> mempty
--
printWorkingTree :: IO ()
printWorkingTree = do
   _wd <- HS.pwd _pwdWidth
   case _wd of
      Nothing -> return ()
      Just wd -> putStr $ applyANSI wd $ fgGreen <> mempty
