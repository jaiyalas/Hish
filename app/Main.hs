module Main where
--
import Hish
-- for manipulating ANSI code
import Data.Monoid (mempty)
--
_prompt_symbol = ">"
_pwdWidth = 60
--
main :: IO ()
main = do
   printUserInfo
   putStr " "
   printWorkingTree _pwdWidth
   putStr " "
   --
   safePrintVCS Git
   -- safePrintVCS VCS.Darcs
   --
   putStr $ applyANSI (_prompt_symbol++" ") $ mempty
--
