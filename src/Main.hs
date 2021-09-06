{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Concurrent.STM (
  TVar,
  newTVar,
  readTVar,
  writeTVar,
 )
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as Map
import Data.List ((\\))
import qualified Data.Text as T
import GHC.Conc (getNumProcessors)
import Protolude
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  getFileSize,
  listDirectory,
  pathIsSymbolicLink,
 )
import System.FilePath.Posix ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering)
import Text.Printf (hPrintf, printf)

{- | Map of root paths with a tuple of folder and file sizes.
 type ResultMap = TVar (Map.HashMap Text FolderStats)
-}
type ResultMap = Map.HashMap Text (TVar FolderStats)

data FolderStats = FolderStats
  { totalSubFolders :: !Int
  , totalFiles :: !Int
  , totalFileSizes :: !Integer
  }

-- | A file item either a file or a folder.
data FileItem
  = Folder !FilePath
  | File !FilePath !Integer
  deriving (Show)

-- | Application Environment
data AppEnv
  = AppEnv
      ![Text]
      -- ^ Root folders.
      !ResultMap
      -- ^ Resuls with folder stats

updateFolderStats :: ResultMap -> Text -> FolderStats -> STM ()
updateFolderStats resultMap key folderStats = do
  valTVar <- readTVar ma
  writeTVar ma $ sumFolderStats valTVar folderStats
 where
  ma = resultMap ! key

-- | Handler for io exceptions. Print error an return default value.
ioErrorHandler :: FilePath -> a -> IOException -> IO a
ioErrorHandler fp val e = do
  hPrintf stderr "Error opening %s (%s)\n" fp (displayException (e :: IOException))
  pure val

-- | List folders from target path.
listFolders :: FilePath -> IO [FilePath]
listFolders path = listDirectory path >>= filterM isNormalFolder . fmap (path </>)

tryGetFileSize :: FilePath -> IO Integer
tryGetFileSize fp =
  doesFileExist fp &&^ fmap not (pathIsSymbolicLink fp)
    >>= bool
      (pure 0)
      (getFileSize fp `catch` ioErrorHandler fp 0)

-- | List all files in target path. Return empty list on failure.
tryListDirectory :: FilePath -> IO [FileItem]
tryListDirectory fp = do
  allFiles <- fmap (fp </>) <$> listDirectory fp `catch` ioErrorHandler fp []
  folders <- filterM isNormalFolder allFiles
  files <- mapM (\f -> File f <$> tryGetFileSize f) $ allFiles \\ folders
  pure $ fmap Folder folders <> files

-- | Check if target path is a folder and not a symbolic link.
isNormalFolder :: FilePath -> IO Bool
isNormalFolder = (&&^) <$> doesDirectoryExist <*> fmap not . pathIsSymbolicLink

{- |
  Channel consumer that reads a path from the input channel
  and write the directory listing for each path to the output
  channel.
-}
folderWorker ::
  (MonadReader AppEnv m, MonadIO m) =>
  Chan FilePath ->
  Chan (FilePath, [FileItem]) ->
  m ()
folderWorker input output =
  ask >>= \(AppEnv rootFolders resultMap) -> liftIO $
    forever $ do
      path <- readChan input
      b <- isNormalFolder path
      if b
        then do
          fileItems <- tryListDirectory path
          let folders = [file | Folder file <- fileItems]
              files = [n | File _ n <- fileItems]

          if not $ null fileItems
            then do
              let totalFiles = length files
                  totalFolders = length folders
                  totalFileSizes = sum files
                  key = getFolderKey (toS path) rootFolders

              case key of
                Just k ->
                  atomically $
                    updateFolderStats resultMap k (FolderStats totalFolders totalFiles totalFileSizes)
                Nothing -> pure ()
            else pure ()

          writeChan output (path, fileItems)
        else writeChan output (path, [])

-- | Find the root path for the target path.
getFolderKey :: Text -> [Text] -> Maybe Text
getFolderKey fullPath = find (`T.isPrefixOf` fullPath)

-- | Sum up two folder stats.
sumFolderStats :: FolderStats -> FolderStats -> FolderStats
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
responseWorker ::
  -- | input channel.
  Chan FilePath ->
  -- | response channel.
  Chan (FilePath, [FileItem]) ->
  IO ()
responseWorker input output = do
  let loop = do
        allFiles <- snd <$> liftIO (readChan output)

        if not $ null allFiles
          then do
            let folders = [file | Folder file <- allFiles]

            liftIO $ writeList2Chan input folders
            replicateM_ (length folders) loop
          else pure ()
  loop

{- |
  Read the command line argument for path to start recursively
  traversing all files/folders. Start a thread pool with a thread
  count being equal to the number of system cores. Each thread
  pool will run worker function that reads and writes to a
  concurrent input and output channel.
-}
main :: IO ()
main =
  getArgs
    >>= \case
      Just p -> do
        rootFolders <- fmap toS <$> listFolders p

        if null rootFolders
          then hPrintf stderr "No folders found to scan in %s\n" p
          else do
            hSetBuffering stdout LineBuffering
            cores <- getNumProcessors
            setNumCapabilities cores
            printf "Using %d worker threads.\n" cores
            input <- newChan
            output <- newChan

            resultMap <- atomically $ populateMap rootFolders

            replicateM_ cores (forkIO $ runWorker input output (AppEnv rootFolders resultMap))
            writeList2Chan input [p]

            printf "Scaning recursively:\n"
            mapM_ (printf "  %s\n") rootFolders
            printf "\n"

            responseWorker input output

            results <- getResults resultMap

            showResult . (p,) $ Map.foldr sumFolderStats (FolderStats 0 0 0) results

            mapM_ showResult $
              (sortOn (Down . totalFileSizes . snd) . Map.toList) results
            printf "\n"
      Nothing -> printf "No args.\n"
      . head
 where
  label = "%s has %i files %i sub folders %s total bytes\n"

  showResult (path, FolderStats{totalFiles, totalSubFolders, totalFileSizes}) =
    printf label path totalFiles totalSubFolders (thousandSep totalFileSizes)

  thousandSep = T.reverse . T.intercalate "," . T.chunksOf 3 . T.reverse . T.pack . show

  runWorker input output = runReaderT (folderWorker input output)
  populateMap =
    foldM
      ( \resultMap rootPath ->
          (\folderStats -> Map.insert rootPath folderStats resultMap)
            <$> newTVar (FolderStats 0 0 0)
      )
      Map.empty

  getResults resultMap =
    atomically $
      fmap Map.fromList
        <$> mapM (\(k, v) -> (k,) <$> readTVar v)
        $ Map.toList resultMap