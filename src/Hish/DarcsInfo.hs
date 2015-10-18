module Hish.DarcsInfo ( ) where

import System.Process
import System.Exit
import Data.List (lines,unlines)
import Data.Char (isSpace)
import qualified Data.String.Utils as S (split,replace,join)
