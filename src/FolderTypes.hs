module FolderTypes
  ( AppEnv (..)
  , FileItem (..)
  , FolderStats (..)
  , FolderStatsMap
  ) where

import Control.Concurrent.STM (TVar)

import Data.HashMap.Strict    qualified as Map
import Protolude

{- |
  Mapping of root folders to folder stats. Values in the
  map are in a STM transaction variable allowing atomicity
  when accessing values instead of the entire map.
-}
type FolderStatsMap =
    Map.HashMap
        Text -- Root folder.
        (TVar FolderStats) --  Folder statistics.

data FolderStats = FolderStats
  { totalSubFolders :: !Int
  , totalFiles      :: !Int
  , totalFileSizes  :: !Integer
  }

-- | A file item either a file or a folder.
data FileItem
  = Folder !FilePath
  | File !FilePath !Integer
  deriving (Show)

-- | Application Environment
data AppEnv = AppEnv ![Text] !FolderStatsMap
        -- ^ Resuls with folder stats
