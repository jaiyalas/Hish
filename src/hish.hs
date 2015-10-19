module Main where

import Hish.ANSICode
import qualified Hish.SysInfo as HS
import qualified Hish.VCS as VCS

import Data.Monoid (mempty,(<>))

--
_prompt_symbol = ">"
_pwdWidth = 60

main :: IO ()
main = do
   _un <- HS.uid
   case _un of
      Nothing -> return ()
      Just un -> putStr $ applyANSI un $ fgBlackL <> mempty
   --
   _hn <- HS.hostname
   case _hn of
      Nothing -> return ()
      Just hn -> putStr $ applyANSI ("@"++hn) $ fgBlackL <> mempty
   --
   putStr " "
   _wd <- HS.pwd _pwdWidth
   case _wd of
      Nothing -> return ()
      Just wd -> putStr $ applyANSI wd $ fgGreen <> mempty
   --
   putStr " "
   --
   putStr "["
   --
   _br <- HS.branch VCS.Git
   case _br of
      Nothing -> return ()
      Just br -> putStr $ applyANSI br $ fgWhiteL <> ESC_Bold <> mempty
   (_st, _ah, _bh) <- HS.status VCS.Git
   --
   case _st of
      Just "*" -> putStr $ applyANSI "*" $ ESC_Bold <> fgRed <> mempty
      Just "?" -> putStr $ applyANSI "?" $ ESC_Bold <> fgYellow <> mempty
      Just "#" -> putStr $ applyANSI "#" $ ESC_Bold <> fgGreen <> mempty
      _ -> putStr ""
   --
   case _ah of
      Nothing -> return ()
      Just ah -> putStr $ applyANSI ("+"++ah) $ fgRedL <> mempty
   --
   case _ah of
      Nothing -> return ()
      Just ah -> putStr $ applyANSI ("-"++ah) $ fgGreenL <> mempty
   --
   putStr "]"
   --
   putStr $ applyANSI (_prompt_symbol++" ") $ mempty
