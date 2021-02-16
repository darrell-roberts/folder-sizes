-- | Handler for io exceptions. Print error an return default value.
module Folder (
    ioErrorHandler,
    listFolders,
    tryGetFileSize,
    tryListDirectory,
    isNormalFolder,
) where

import Data.List             ((\\))
import FolderTypes           (FileItem (..))
import Protolude
import System.Directory      (doesDirectoryExist, doesFileExist, getFileSize,
                              listDirectory, pathIsSymbolicLink)
import System.FilePath.Posix ((</>))
import Text.Printf           (hPrintf)

ioErrorHandler ∷
    FilePath →
    a →
    IOException →
    IO a
ioErrorHandler fp val e = do
    hPrintf stderr "Error opening %s (%s)\n" fp $ displayException e
    pure val

-- | List folders from target path.
listFolders ∷ FilePath → IO [FilePath]
listFolders path =
    listDirectory path
        >>= filterM isNormalFolder
            . fmap (path </>)

-- | Get files size with error handling. Return 0 on failure.
tryGetFileSize ∷ FilePath → IO Integer
tryGetFileSize fp =
    doesFileExist fp &&^ fmap not (pathIsSymbolicLink fp)
        >>= bool
            (pure 0)
            (getFileSize fp `catch` ioErrorHandler fp 0)

-- | List all files in target path. Return empty list on failure.
tryListDirectory ∷ FilePath → IO [FileItem]
tryListDirectory fp = do
    allFiles <- fmap (fp </>) <$> listDirectory fp `catch` ioErrorHandler fp []
    folders <- filterM isNormalFolder allFiles
    files <- mapM (\f -> File f <$> tryGetFileSize f) $ allFiles \\ folders
    pure $ fmap Folder folders <> files

-- | Check if target path is a folder and not a symbolic link.
isNormalFolder ∷ FilePath → IO Bool
isNormalFolder = (&&^) <$> doesDirectoryExist <*> fmap not . pathIsSymbolicLink
