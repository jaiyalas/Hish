module Hish.VCS.Git
   (
   -- * Git
   Git   (..)
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
-- | Unit type for presenting Git.
data Git = Git
--
instance Show Git where
   show Git = "G"
--
instance VCS Git where
    --
    branchCmd  _ = "git"
    branchArgs _ = ["branch"]
    statusCmd  _ = "git"
    statusArgs _ = ["status","--porcelain","-sb"]
    repoName   _ = ".git"
    --
    getAhead _ s =
        case (s =~ "ahead [0-9]+"  :: String) of
            []    -> Nothing
            ahead -> Just $ drop 6 ahead
            -- ahead -> Just $ S.replace "ahead " "" ahead
    --
    getBehind _ s =
        case (s =~ "behind [0-9]+"  :: String) of
            []    -> Nothing
            behind -> Just $ drop 7 behind
            -- behind -> Just $ S.replace "behind " "" behind
    --
    getCleanliness _ s = let body = tail $ DL.lines s in
        case body of
            [] -> Just ""
            _  -> let (index, tree) = shortStatus body in
                    if ((index++tree) =~ "[?]+" :: Bool)
                        -- exists untracked file(s)
                        then Just "?"
                        else case (index, tree) of
                            -- index == tree == []
                            ([],[])   -> Just ""
                            -- every changes are in index
                            (_,[])    -> Just "#"
                            -- no change in index
                            ([],_)    -> Just "*"
                            -- mixed situiation
                            (idx,tre) -> Just "#"
    --
    getStatus vcs = do
        maybeText <- argedCmd (Just . id) (statusCmd vcs) (statusArgs vcs)
        case maybeText of
            Nothing -> return
                ( Nothing
                , Nothing
                , Nothing )
            (Just text) -> return
                ( getCleanliness vcs text
                , getAhead       vcs text
                , getBehind      vcs text )
    --
    getBranch vcs =  argedCmd
        (Just
        .(\xs -> if length xs <= 0
            then ""
            else drop 2 $ head xs)
        .filter ((=~ "\\`[*]+") :: String -> Bool)
        .lines)
        (branchCmd vcs)
        (branchArgs vcs)
    --
    isRepo vcs = do
        (code,_,_) <-
            SP.readProcessWithExitCode
                (statusCmd vcs)
                (statusArgs vcs) ""
        case code of
            ExitFailure _ -> return False
            ExitSuccess   -> return True
   --
    installed _ = do
        ext <- findExecutable "git"
        case ext of
            Nothing -> return False
            Just _  -> return True
--
shortStatus :: [String] -> (String, String)
shortStatus =
    (\(xs,ys) -> (filter (/=' ') xs, filter (/=' ') ys))
    .unzip.map ((\ (a:b:_)->(a,b)).take 2)
