{-# LANGUAGE BangPatterns #-}
module Main where
--
import Hish.ANSICode
import Hish.VCS
import Hish.VCS.Git
-- import Hish.VCS.Darcs
import qualified Hish.SysInfo   as SysInfo
import qualified System.Process as SP
--
import Data.Monoid (mempty,(<>))
--
_prompt_symbol = ">"
_pwdWidth = 45
--
main :: IO ()
main = do
   printSTime
   -- printUID
   -- printHostname
   putStr " "
   printWorkingTree
   putStr " "
   --
   !b1 <- installed Git
   !b2 <- isRepo Git
   if (b1 && b2) then printVCSInfo Git else return ()
   --
   putStr $ applyANSI (_prompt_symbol++" ") $ mempty
--
--
printVCSInfo :: (Show a, VCS a) => a -> IO ()
printVCSInfo vcs = do
    _br <- getBranch vcs
    case _br of
      Just br  -> putStr $ applyANSI br $ ESC_Bold <> fgWhiteL <> mempty
      Nothing  -> return ()
    (_st, _ah, _bh) <- getStatus vcs
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
--
printSTime :: IO ()
printSTime = do
   _ts <- SysInfo.time "%H:%M"
   putStr $ applyANSI (_ts) $ fgBlackL <> mempty

--
printUID :: IO ()
printUID = do
   _un <- SysInfo.uid
   case _un of
      Nothing -> return ()
      Just un -> do
           putStr $ applyANSI "(" $ fgBlackL <> mempty
           putStr $ applyANSI (un) $ mempty
           putStr $ applyANSI ")" $ fgBlackL <> mempty
--
printHostname :: IO ()
printHostname = do
   _hn <- SysInfo.hostname
   case _hn of
      Nothing -> return ()
      Just hn -> putStr $ applyANSI ("@"++hn) $ fgBlackL <> mempty
--
printWorkingTree :: IO ()
printWorkingTree = do
   _wd <- SysInfo.pwd _pwdWidth
   case _wd of
      Nothing -> return ()
      Just wd -> putStr $ applyANSI wd $ fgGreen <> mempty
