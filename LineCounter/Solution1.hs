module LineCounter.Solution1 where 

import System.Directory
import System.FilePath

task1 :: FilePath -> IO Int
task1 path = filesStats onePerFile path
  where onePerFile :: FilePath -> IO Int
        onePerFile _ = return 1

task2 :: FilePath -> IO Int
task2 path = filesStats linesInFile path
  where linesInFile :: FilePath -> IO Int
        linesInFile filePath = do
          content <- readFile filePath
          return $ length $ lines content

--task3 :: FilePath -> IO (Int, Int, Int)
--task3 path = filesStats javaStatsInFile path
--  where javaStatsInFile :: FilePath -> IO (Int, Int, Int)
--        javaStatsInFile filePath = 
--          content <- readFile filePath
--          return $ length $ lines content

instance Monoid Int where
  mempty = 0
  mappend = (+)

filesStats :: Monoid a => (FilePath -> IO a) -> FilePath -> IO a
filesStats f rootPath = do
  paths <- absolutePathsInTree rootPath
  values <- sequence $ fmap f paths
  return $ foldl mappend mempty values

absolutePathsInTree :: FilePath -> IO [FilePath]
absolutePathsInTree rootPath = do
  isDirectory <- doesDirectoryExist rootPath  
  if isDirectory then do
    directoryContents <- absolutePathsInDir rootPath
    subDirectoryContents <- sequence $ fmap absolutePathsInTree directoryContents
    return $ mconcat subDirectoryContents
  else 
    return [rootPath]
  
  where absolutePathsInDir :: FilePath -> IO [FilePath]
        absolutePathsInDir rootFolder = do
        relativePaths <- getDirectoryContents rootFolder
        return $ fmap asAbsolutePath $ filterDotPaths relativePaths

        where filterDotPaths :: [FilePath] -> [FilePath]
              filterDotPaths = filter (`notElem` [".", ".."])

              asAbsolutePath :: FilePath -> FilePath
              asAbsolutePath relativePath = concat [rootFolder, [pathSeparator], relativePath]

-- check later      
-- https://hackage.haskell.org/package/FileManip-0.3.3.1/docs/System-FilePath-Find.html