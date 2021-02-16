{-- |
  Command line executable that takes a folder path as an argument
  and recursively gathers folder stats for each first level sub-directory
  found under the path provided.

  Statistics collected:
    * Total sub-folders
    * Total files
    * Total file sizes in bytes
-}
module Main where

import Control.Concurrent.STM (newTVar, readTVar, writeTVar)
import Data.HashMap.Strict    ((!))
import Data.HashMap.Strict    qualified as Map
import Data.Text              qualified as T
import Folder                 (isNormalFolder, listFolders, tryListDirectory)
import FolderTypes            (AppEnv (..), FileItem (..), FolderStats (..),
                               FolderStatsMap)
import GHC.Conc               (getNumProcessors)
import Protolude
import System.IO              (BufferMode (LineBuffering), hSetBuffering)
import Text.Printf            (hPrintf, printf)

{- |
  Update the folder stats in the result map by
  summing the old status with the new stats.
-}
updateFolderStats ∷
  FolderStatsMap →
  Text →
  FolderStats →
  STM ()
updateFolderStats resultMap key folderStats =
  let ma = resultMap ! key
   in readTVar ma >>= writeTVar ma . sumFolderStats folderStats

-- | Build the initial root folders to folder stats mappings.
populateMap ∷
  [Text] →
  STM FolderStatsMap
populateMap =
  foldM
    ( \resultMap rootPath ->
        newTVar (FolderStats 0 0 0)
          <&> \folderStats -> Map.insert rootPath folderStats resultMap
    )
    Map.empty

-- | Read the TVar value mappings into a new Mapping with the values.
getResults ∷
  FolderStatsMap →
  STM (Map.HashMap Text FolderStats)
getResults resultMap =
  fmap Map.fromList
    <$> mapM (\(k, v) -> (k,) <$> readTVar v)
    $ Map.toList resultMap

{- |
  Channel consumer that reads a path from the input channel
  and write the directory listing for each path to the output
  channel.
-}
folderWorker ∷
  (MonadReader AppEnv m, MonadIO m) ⇒
  Chan FilePath →
  Chan (FilePath, [FileItem]) →
  m ()
folderWorker input output =
  ask >>= \(AppEnv rootFolders resultMap) -> liftIO $
    forever $ do
      path <- readChan input
      b <- isNormalFolder path
      if b
        then do
          fileItems <- tryListDirectory path
          unless (null fileItems) $ do
            let key = getFolderKey (toS path) rootFolders

            case key of
              Just k -> do
                let totalFolders = length [file | Folder file <- fileItems]
                    (totalFiles, totalFileSizes) =
                      (,) <$> length <*> sum $ [n | File _ n <- fileItems]

                atomically $
                  updateFolderStats
                    resultMap
                    k
                    (FolderStats totalFolders totalFiles totalFileSizes)
              Nothing -> pure ()

          writeChan output (path, fileItems)
        else writeChan output (path, [])

-- | Find the root path for the target path.
getFolderKey ∷
  Text →
  [Text] →
  Maybe Text
getFolderKey fullPath = find (`T.isPrefixOf` fullPath)

-- | Sum up two folder stats.
sumFolderStats ∷
  FolderStats →
  FolderStats →
  FolderStats
sumFolderStats fs1 fs2 =
  fs1
    { totalFiles = totalFiles fs1 + totalFiles fs2
    , totalFileSizes = totalFileSizes fs1 + totalFileSizes fs2
    , totalSubFolders = totalSubFolders fs1 + totalSubFolders fs2
    }

{- |
  Reads from the output channel the file listing results
  and recursively writes to the input channel all
  folder files from each response until no more nested
  folders are found.
-}
responseWorker ∷
  -- | input channel.
  Chan FilePath →
  -- | response channel.
  Chan (FilePath, [FileItem]) →
  IO ()
responseWorker input output = do
  let loop = do
        allFiles <- snd <$> readChan output

        unless (null allFiles) $ do
          let folders = [file | Folder file <- allFiles]

          writeList2Chan input folders
          replicateM_ (length folders) loop
  loop

-- | Display an Integer as Text with comma separators.
thousandSep ∷ Integer → Text
thousandSep =
  T.reverse . T.intercalate ","
    . T.chunksOf 3
    . T.reverse
    . T.pack
    . show

{- |
  Read the command line argument for path to start recursively
  traversing all files/folders. Start a thread pool with a thread
  count being equal to the number of system cores. Each thread
  pool will run worker function that reads and writes to a
  concurrent input and output channel.
-}
main ∷ IO ()
main =
  getArgs
    >>= \case
      Just path -> do
        rootFolders <- fmap toS <$> listFolders path

        if null rootFolders
          then hPrintf stderr "No folders found to scan in %s\n" path
          else do
            hSetBuffering stdout LineBuffering
            cores <- getNumProcessors
            setNumCapabilities cores
            printf "Using %d worker threads.\n" cores
            input <- newChan
            output <- newChan
            resultMap <- atomically $ populateMap rootFolders

            replicateM_ cores $
              forkIO $
                runWorker input output (AppEnv rootFolders resultMap)

            writeList2Chan input [path]

            printf "Scaning %s recursively:\n" path
            mapM_ (printf "  %s\n") rootFolders
            printf "\n"

            let showResults =
                  sequence_
                    . sequence
                      [ showResult . (path,) . Map.foldr sumFolderStats (FolderStats 0 0 0)
                      , const $ printf "\n"
                      , mapM_ showResult . sortOn (Down . totalFileSizes . snd) . Map.toList
                      , const $ printf "\n"
                      ]

            responseWorker input output
            atomically (getResults resultMap) >>= showResults
      Nothing -> printf "No args.\n"
      . head
 where
  label = "%s -> %i files %i sub folders %s total bytes\n"

  showResult
    ( path
      , FolderStats
          { totalFiles
          , totalSubFolders
          , totalFileSizes
          }
      ) =
      printf label path totalFiles totalSubFolders (thousandSep totalFileSizes)

  runWorker input output = runReaderT (folderWorker input output)
