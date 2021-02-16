{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Protolude
import           System.Directory      (doesDirectoryExist, listDirectory)
import           System.FilePath.Posix ((</>))
import           System.IO             (hFileSize)
import           Text.Printf           (printf)

sumFiles :: FilePath -> IO Integer
sumFiles fp = listDirectory fp >>= foldM f 0
  where f total fp' = let fullPath = fp </> fp'
                      in doesDirectoryExist fullPath >>=
          \case True  -> (+total) <$> sumFiles fullPath
                False -> (+total) <$> withFile fullPath ReadMode hFileSize

folderSums :: FilePath -> IO [(FilePath, Integer)]
folderSums = listDirectory >=> filterM doesDirectoryExist >=>
    mapM (\fp -> (fp,) <$> sumFiles fp)

main :: IO ()
main = folderSums "." >>=
  mapM_ (uncurry (printf "%s: %d\n")) . sortBy (compare `on` snd)
