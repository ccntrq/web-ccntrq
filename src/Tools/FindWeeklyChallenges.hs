{-# LANGUAGE LambdaCase #-}

module Tools.FindWeeklyChallenges where

import           Control.Exception (throwIO)
import           Control.Monad (filterM, forM_, when, (>=>))
import           Data.List (intercalate, isInfixOf, isPrefixOf)
import           Data.List.Split (splitOn)
import qualified Data.Map as Map
import           GHC.IO.Exception (ExitCode(ExitSuccess))
import           System.Directory (doesDirectoryExist, getCurrentDirectory
                                 , listDirectory, setCurrentDirectory
                                 , withCurrentDirectory)
import           System.FilePath.Glob (compile, globDir1)
import           System.Process (readProcessWithExitCode)
import           System.Unix.Directory (withTemporaryDirectory)

data Challenge =
  Challenge { week :: Int, task1 :: [Solution], task2 :: [Solution] }
  deriving (Show)

data Solution = Solution { language :: String, path :: FilePath }
  deriving (Show)

repoUrl :: String
repoUrl = "https://github.com/manwar/perlweeklychallenge-club"

gitCloneUrl :: String
gitCloneUrl = repoUrl <> ".git"

generateNewChallengeFiles :: IO ()
generateNewChallengeFiles = do
  new <- findNewChallenges "alexander-pankoff"
  forM_ new
    $ \ch -> writeFile
      ("pages/perl-weekly-challenge/challenge-" <> show (week ch) <> ".md")
      (generateFileContent ch)

generateFileContent :: Challenge -> String
generateFileContent ch = unlines
  $ [ "---"
    , "title: Challenge " ++ show (week ch)
    , "---"
    , ""
    , ""
    , "## Task #1"
    , ""]
  ++ generateTaskContent (task1 ch)
  ++ ["", "## Task #2", ""]
  ++ generateTaskContent (task2 ch)

generateTaskContent :: [Solution] -> [String]
generateTaskContent = \case
  []        -> ["*No Solution* This week I only solved the other task."]
  solutions -> map generateSolutionContent solutions

generateSolutionContent :: Solution -> String
generateSolutionContent solution = concat
  [ "- ["
  , language solution
  , "]"
  , "("
  , "https://github.com/manwar/perlweeklychallenge-club/blob/master/"
  , path solution
  , ")"]

findNewChallenges :: String -> IO [Challenge]
findNewChallenges author = do
  existing <- alreadyExistingChallenges
  filter (\x -> week x `notElem` existing) <$> findWeeklyChallenges author

alreadyExistingChallenges :: IO [Int]
alreadyExistingChallenges = do
  existingChallengeFiles <- listDirectory "pages/perl-weekly-challenge"
  return
    $ map
      (read . head . splitOn "." . (!! 1) . splitOn "-")
      existingChallengeFiles

findWeeklyChallenges :: String -> IO [Challenge]
findWeeklyChallenges author = withTemporaryDirectory "weekly-challenges-tmp"
  $ \dir -> withCurrentDirectory dir
  $ do
    putStrLn "cloning challenges repo..."
    (exitCode, _, _) <- readProcessWithExitCode "git" ["clone", gitCloneUrl] ""
    when (exitCode /= ExitSuccess) (error "Couldn't clone challenges git repo")
    setCurrentDirectory "perlweeklychallenge-club"
    putStrLn "clone done..."
    findChallengesForAuthorinCwd author

findChallengesForAuthorinCwd :: String -> IO [Challenge]
findChallengesForAuthorinCwd author = mergeAllChallenges . map parseChallenge
  <$> myGlob ("challenge-*/" <> author <> "/*/ch-<1-2>.*")

mergeAllChallenges :: [Challenge] -> [Challenge]
mergeAllChallenges challenges = map snd
  $ Map.toAscList
  $ foldl
    (\acc cur -> Map.insertWith mergeChallenges (week cur) cur acc)
    Map.empty
    challenges

mergeChallenges :: Challenge -> Challenge -> Challenge
mergeChallenges challenge1 challenge2 = Challenge
  (week challenge1)
  (task1 challenge1 ++ task1 challenge2)
  (task2 challenge1 ++ task2 challenge2)

parseChallenge :: FilePath -> Challenge
parseChallenge path =
  let parts = splitOn "/" path
      challenge = read $ splitOn "-" (head parts) !! 1
      language = parts !! 2
      ch1 = isPrefixOf "ch-1" $ parts !! 3
  in (if ch1
      then id
      else flip)
       (Challenge challenge)
       [Solution language path]
       []

myGlob :: String -> IO [FilePath]
myGlob rawPattern = map removeThisDir <$> globDir1 (compile rawPattern) "./"
  where
    removeThisDir = drop 2
