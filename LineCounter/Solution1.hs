module LineCounter.Solution1 where 

import Data.Char
import Data.Monoid
import Data.List
import System.Directory
import System.FilePath

-- 1) Count files recursively in directory
type FileCount = Sum Int

task1 :: FilePath -> IO FileCount
task1 = filesStats allFiles onePerFile

allFiles :: FilePath -> Bool
allFiles = const True

onePerFile :: FilePath -> IO FileCount
onePerFile = const $ return (Sum 1)

-- 2) Count files, and lines recursively in directory
type LineCount = Sum Int

task2 :: FilePath -> IO (FileCount, LineCount)
task2 = filesStats allFiles $ tuplify onePerFile linesInFile

linesInFile :: FilePath -> IO LineCount
linesInFile filePath = do
  content <- readFile filePath
  return $ Sum $ length $ lines content

tuplify :: Monad f => (a -> f b) -> (a -> f c) -> (a -> f (b, c))
tuplify f g = \a -> do 
  b <- (f a)
  c <- (g a)
  return (b, c)

-- 3) Count code/whitespace/comments of Java code recursively in directory
type JavaStats = (LineCount, LineCount, LineCount)

task3 :: FilePath -> IO JavaStats
task3 path = filesStats javaFiles javaStatsInFile path

javaStatsInFile :: FilePath -> IO JavaStats
javaStatsInFile filePath = do
  content <- readFile filePath
  return $ foldl mappend mempty $ fmap javaStatsInLine $ lines content

  where javaStatsInLine :: String -> JavaStats
        javaStatsInLine line 
          | isPrefixOf "//" $ strip line = (Sum 0, Sum 0, Sum 1)
          | null $ strip line            = (Sum 0, Sum 1, Sum 0)
          | otherwise                    = (Sum 1, Sum 0, Sum 0)

        strip :: String -> String
        strip = dropWhile isSpace

javaFiles :: FilePath -> Bool
javaFiles f = isSuffixOf ".java" f

-- Utility methods
filesStats :: Monoid a => (FilePath -> Bool) -> (FilePath -> IO a) -> FilePath -> IO a
filesStats p f rootPath = do
  paths <- absolutePathsInTree rootPath
  values <- sequence $ fmap f $ filter p paths
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