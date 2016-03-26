module Hish.VCS
   (
   -- * Version Control System
     VCS   (..)
   ) where
--
-- | Every version control system provides functions as follows
--
class VCS a where
   -- | get command for revealing branch
   branchCmd :: a -> String
   -- | get arguments for revealing branch
   branchArgs :: a -> [String]
   -- | get command for revealing status
   statusCmd :: a -> String
   -- | get arguments for revealing status
   statusArgs :: a -> [String]
   -- | get name of repository folder
   repoName :: a -> String
   -- | tracking ahead
   getAhead :: a -> String -> Maybe String
   -- | tracking behind
   getBehind :: a -> String -> Maybe String
   -- | determining the cleanliness of working-tree
   --       * __' '__ - /clean/
   --       * __'?'__ - /clean/ (exists untracked file)
   --       * __'#'__ - /dirty/ (non-empty index; ready for commit..)
   --       * __'*'__ - /dirty/ (empty index)
   getCleanliness :: a -> String -> Maybe String
   -- | get current status
   getStatus :: a -> IO (Maybe String,
                         Maybe String,
                         Maybe String) -- ^ (cleanliness, ahead, behind)
   -- | get current name of git-branch
   getBranch :: a -> IO (Maybe String)  -- ^ current branch name
   -- | using status to verifying the existence of repository
   isRepo :: a -> IO Bool
   -- | is this vcs installed (= executable)
   installed :: a -> IO Bool
