{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Data.HashMap.Strict as Map
import Data.List ((\\))
import qualified Data.Text as T
import GHC.Conc (getNumProcessors)
import Protolude
import System.Directory (
  doesDirectoryExist,
  listDirectory,
  pathIsSymbolicLink,
  doesPathExist
 )
import System.FilePath.Posix ((</>))
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import Text.Printf (printf, hPrintf)

-- | Map of root paths with a tuple of folder and file sizes.
type ResultMap = Map.HashMap Text (Int, Int)

-- | A file item either a file or a folder.
data FileItem
  = Folder !FilePath
  | File !FilePath
  deriving (Show)

-- | Handler for io exceptions. Print error an return an empty list.
ioErrorHandler :: FilePath -> IOException -> IO [a]
ioErrorHandler fp e = do
  hPrintf stderr "Error opening %s (%s)\n" fp (displayException (e :: IOException))
  pure []

-- | List folders from target path.
listFolders :: FilePath -> IO [FilePath]
listFolders = listDirectory >=> filterM isNormalFolder

-- | List all files in target path. Return empty list on failure.
tryListDirectory :: FilePath -> IO [FileItem]
tryListDirectory fp = do
  allFiles <- fmap (fp </>) <$> listDirectory fp `catch` ioErrorHandler fp
  folders <- filterM isNormalFolder allFiles
  pure $ fmap Folder folders <> fmap File (allFiles \\ folders)

-- | Check if target path is a folder and not a symbolic link.
isNormalFolder :: FilePath -> IO Bool
isNormalFolder = (&&^) <$> doesDirectoryExist <*> fmap not . pathIsSymbolicLink

{- |
  Channel consumer that reads a path from the input channel
  and write the directory listing for each path to the output
  channel.
-}
folderWorker ::
  Chan FilePath ->
  Chan (FilePath, [FileItem]) ->
  IO ()
folderWorker input output = forever $ do
  path <- readChan input
  isNormalFolder path
    >>= bool
      (writeChan output (path, []))
      (tryListDirectory path >>= \fileItems -> writeChan output (path, fileItems))

-- | Find the root path for the target path.
getFolderKey :: Text -> [Text] -> Maybe Text
getFolderKey fullPath = find (`T.isPrefixOf` fullPath)

-- | Sum fst and snd values between two tuples.
sumTuple :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
sumTuple (a, b) (c, d) = (a + c, b + d)

{- |
  Reads from the output channel the file listing results
  and sets the counts in a HashMap. Recursively writes
  to the input channel all folder files from each response
  until no more nested folders are found.
-}
responseWorker ::
  ( MonadReader [Text] m
  , MonadIO m
  , MonadState ResultMap m
  ) =>
  -- | input channel.
  Chan FilePath ->
  -- | response channel.
  Chan (FilePath, [FileItem]) ->
  m ()
responseWorker input output =
  ask >>= \rootFolders -> do
    let loop = do
          (path, allFiles) <- liftIO $ readChan output
          let folders = [file | x@(Folder file) <- allFiles]
              files = [x | x@File{} <- allFiles]

          if not $ null allFiles
            then do
              let totalFiles = length files
                  totalFolders = length folders
                  key = getFolderKey (toS path) rootFolders

              maybe
                (pure ())
                ( \k ->
                    modify $ Map.insertWith sumTuple k (totalFiles, totalFolders)
                )
                key

              liftIO $ writeList2Chan input folders
              replicateM_ totalFolders loop
            else pure ()
    loop

-- | Compose monad transformers and run app in environment
runMonadT ::
  MonadIO m =>
  -- | Input channel
  Chan FilePath ->
  -- | Output channel
  Chan (FilePath, [FileItem]) ->
  -- | List of file paths
  [Text] ->
  -- | Map of root paths with file and folder counts
  m ((), ResultMap)
runMonadT input output = runReaderT (runStateT (responseWorker input output) Map.empty)

{- |
  Read the command line argument for path to start recursively
  traversing all files/folders. Start a thread pool with a thread
  count being equal to the number of system cores. Each thread
  pool will run worker function that reads and writes to a
  concurrent input and output channel.
-}
main :: IO ()
main = do
  path <- head <$> getArgs
  hSetBuffering stdout NoBuffering

  case path of
    Just p -> do
      cores <- getNumProcessors
      setNumCapabilities cores
      printf "Using %d worker threads per worker\n" cores
      input <- newChan
      output <- newChan
      replicateM_ cores (forkIO $ folderWorker input output)
      writeList2Chan input [p]

      rootFolders <- fmap (\p' -> toS $ p </> p') <$> listFolders p

      if null rootFolders
        then hPrintf stderr "No folders found to scan in %s\n" p
      else do
        printf "Scaning recursively:\n"
        mapM_ putStrLn rootFolders
        printf "\n"
        (_, results) <- runMonadT input output rootFolders
        uncurry (printf "%s %i files, folders %i: \n" p) $
          Map.foldr sumTuple (0, 0) results
        mapM_
          ( \(s, (totalFiles, totalFolders)) ->
              printf "Path %s files %i folders %i\n" s totalFiles totalFolders
          )
          $ (sortOn (Down . snd . snd) . Map.toList) results
    Nothing -> printf "No args.\n"
