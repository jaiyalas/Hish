module Hish.VCS
   (
   -- * Version Control System
     VCS   (..)
   -- ** Git
   , Git   (..)
   -- ** Darcs
   , Darcs (..)
   ) where

import Text.Regex.TDFA ((=~))
import qualified Data.String.Utils as S (replace)
import qualified Data.List as DL (lines)
import System.Directory (findExecutable)

-- | Every version control system provides functions as follows
--
class VCS a where
   -- | tracking ahead
   vcsAhead :: a -> String -> Maybe String
   -- | tracking behind
   vcsBehind :: a -> String -> Maybe String
   -- | determining the cleanliness of working-tree
   --
   --       * __#__ - /clean/ working directory
   --       * __?__ - /clean/ but existing untracked file
   --       * __*__ - /dirty/
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
   vcsAhead _ s =
      case (s =~ "ahead [0-9]+"  :: String) of
         []    -> Nothing
         ahead -> Just $ S.replace "ahead " "" ahead
   vcsBehind _ s =
      case (s =~ "behind [0-9]+"  :: String) of
         []    -> Nothing
         behind -> Just $ S.replace "behind " "" behind
   vcsCleanliness _ s = let body = tail $ DL.lines s in
      case body of
         [] -> Just "#"
         _  -> let (index,tree) = unzip $ map ((\ (a:b:_)->(a,b)).take 2) body in
            if ("" == (filter (/='?') tree))
               then Just "?"
               else Just "*"
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

-- | Unit type for presenting Darcs.
-- /UN-IMPLEMENTED/!
data Darcs = Darcs

instance Show Darcs where
   show Darcs = "D"

instance VCS Darcs where
   vcsAhead _ s = return "a"
   vcsBehind _ s = return "b"
   vcsCleanliness _ s = return "?"
   vcsCurrentBranch _ s =
      -- darcs has no the concept of branch
      return ""
   --
   branchCmd Darcs = "darcs"
   branchArgs Darcs = ["whatsnew"]
   statusCmd Darcs = "darcs"
   statusArgs Darcs = ["whatsnew"]
   --
   installed _ = do
      ext <- findExecutable "darcs"
      case ext of
         Nothing -> return False
         Just _  -> return True
