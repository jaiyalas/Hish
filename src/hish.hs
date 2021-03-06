{-# LANGUAGE BangPatterns #-}
module Hish
    ( -- * printing
      printUserInfo
    , printWorkingTree
    , printVCSInfo
    , safePrintVCS
    -- * modules
    , module Hish.ANSICode
    , module HS
    , module VCS
    ) where
--
import Hish.ANSICode
import Hish.SysInfo as HS
import Hish.VCS as VCS
-- for manipulating ANSI code
import Data.Monoid (mempty,(<>))
import Debug.Trace
--
printUserInfo :: IO ()
printUserInfo = do
   _ts <- HS.time "%H:%M"
   putStr $ applyANSI (_ts) $ fgBlackL <> mempty
   -- putStr $ applyANSI "(" $ fgBlackL <> mempty
   -- _un <- HS.uid
   -- case _un of
   --    Nothing -> return ()
   --    Just un -> putStr $ applyANSI (un) $ mempty
   -- putStr $ applyANSI ")" $ fgBlackL <> mempty
   -- _hn <- HS.hostname
   -- case _hn of
   --    Nothing -> return ()
   --    Just hn -> putStr $ applyANSI ("@"++hn) $ fgBlackL <> mempty
--
printWorkingTree :: Int -> IO ()
printWorkingTree _pwdWidth = do
   _wd <- HS.pwd _pwdWidth
   case _wd of
      Nothing -> return ()
      Just wd -> putStr $ applyANSI wd $ fgGreen <> mempty
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
safePrintVCS :: (Show a, VCS a) => a -> IO ()
safePrintVCS vcs = do
   b1 <- VCS.installed vcs
   case b1 of
      True -> do
         b2 <- HS.isRepo vcs
         if b2 then printVCSInfo vcs else return ()
      False -> return ()
