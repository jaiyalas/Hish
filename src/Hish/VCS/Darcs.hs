module Hish.VCS.Darcs
   (
   -- * Darcs
   Darcs   (..)
   ) where
--
import qualified System.Process as SP
import Text.Regex.TDFA ((=~))
import qualified Data.String.Utils as S (replace)
import qualified Data.List as DL (lines)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
--
import Hish.VCS
import Hish.SysInfo
--
-- | Unit type for presenting Darcs.
-- /UN-IMPLEMENTED/!
data Darcs = Darcs
--
instance Show Darcs where
   show Darcs = "D"
--
instance VCS Darcs where
    --
    branchCmd  _ = "darcs"
    branchArgs _ = ["whatsnew"]
    statusCmd  _ = "darcs"
    statusArgs _ = ["whatsnew","-l"]
    repoName   _ = "_darcs"
    --
    getCleanliness _ ("No changes!") = Just "#"
    getCleanliness _ s = let body = map head $ DL.lines s in
        if ((filter (/='a') body)=="") then Just "?" else Just "*"
    -- darcs is so hard to find ahead/behind
    getAhead _ s = do
        return ""
    getBehind _ s = do
        return ""
    getCurrentBranch _ s = do
        -- darcs has no the concept of branch
        return ""
    --
    getStatus _ = retirn (Nothing, Nothing, Nothing)
    getBranch _  = return Noting
    isRepo _ = do return False
    installed _ = do
        ext <- findExecutable "darcs"
        case ext of
            Nothing -> return False
            Just _  -> return True
