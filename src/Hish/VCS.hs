module Hish.VCS
   (
   -- * Version Control System
     VCS   (..)
   -- ** Git
   , Git   (..)
   -- ** Darcs
   , Darcs (..)
   ) where

-- import Text.Regex.TDFA ((=~))
import Text.Parsec
import qualified Data.String.Utils as S (replace)
import qualified Data.List as DL (lines)
import System.Directory (findExecutable)
import Data.Either

-- | Every version control system provides functions as follows
--
class VCS a where
   -- | tracking ahead
   vcsAhead :: a -> String -> Maybe String
   -- | tracking behind
   vcsBehind :: a -> String -> Maybe String
   -- | determining the cleanliness of working-tree
   --
   --       * __' '__ - /clean/
   --       * __'?'__ - /clean/ (exists untracked file)
   --       * __'#'__ - /dirty/ (non-empty index; ready for commit..)
   --       * __'*'__ - /dirty/ (empty index)
   vcsCleanliness :: a -> String -> Maybe String
   vcsCurrentBranch :: a -> String -> Maybe String
   -- | get command for revealing branch
   branchCmd :: a -> String
   -- | get arguments for revealing branch
   branchArgs :: a -> [String]
   -- | get command for revealing status
   statusCmd :: a -> String
   -- | get arguments for revealing status
   statusArgs :: a -> [String]
   -- | is this vcs installed (= executable)
   installed :: a -> IO Bool

-- | Unit type for presenting Git.
data Git = Git

instance Show Git where
   show Git = "G"

instance VCS Git where
    vcsAhead _ s = either (const Nothing) Just $
        parse (string "ahead" >> spaces >> many1 digit >>= return) "" s
    vcsBehind _ s = either (const Nothing) Just $
        parse (string "behind" >> spaces >> many1 digit >>= return) "" s
    vcsCleanliness _ s = let body = tail $ DL.lines s in
        case body of
            [] -> Just ""
            _  -> let (index,tree) = unzip $ map ((\ (a:b:_)->(a,b)).take 2) body in case (filter (/=' ') index, filter (/=' ')tree) of
                -- index == tree == "   "
                ("","") -> Just ""
                -- every changes are in index
                (_,"") -> Just "#"
                -- none change is inindex
                ("",_) -> Just "*"
                (idx,tre) -> if ("" == (filter (/='?') (idx++tre)))
                    -- left only untracked file
                    then Just "?"
                    -- mix situiation
                    else Just "#"
    vcsCurrentBranch _ = Just
        . drop 2
        . head
        . filter ((=='*').head)
        . lines
   --
    branchCmd  _ = "git"
    branchArgs _ = ["branch"]
    statusCmd  _ = "git"
    statusArgs _ = ["status","--porcelain","-sb"]
    --
    installed _ = do
        ext <- findExecutable "git"
        case ext of
            Nothing -> return False
            Just _  -> return True
--

-- -- -- -- --

-- parse'GitState = do
--     --
--
--     --
-- --
-- parse'GitState'Headline = do
--     string "##"
--     spaces
--     name <- many1 letter
--     choice
--         [ --
--         , string "..."
--         ]
-- --
-- parse'GitState'Diffline

-- -- -- -- --

-- | Unit type for presenting Darcs.
-- /UN-IMPLEMENTED/!
data Darcs = Darcs
--
instance Show Darcs where
   show Darcs = "D"
--
-- instance VCS Darcs where
--    vcsCleanliness _ ("No changes!") = Just "#"
--    vcsCleanliness _ s = let body = map head $ DL.lines s in
--       if ((filter (/='a') body)=="") then Just "?" else Just "*"
--    -- darcs is so hard to find ahead/behind
--    vcsAhead _ s = do
--       return ""
--    vcsBehind _ s = do
--       return ""
--    vcsCurrentBranch _ s =
--       -- darcs has no the concept of branch
--       return ""
--    --
--    branchCmd Darcs = "darcs"
--    branchArgs Darcs = ["whatsnew"]
--    statusCmd Darcs = "darcs"
--    statusArgs Darcs = ["whatsnew","-l"]
--    --
--    installed _ = do
--       ext <- findExecutable "darcs"
--       case ext of
--          Nothing -> return False
--          Just _  -> return True
