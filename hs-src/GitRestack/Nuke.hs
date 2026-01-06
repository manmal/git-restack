{-# LANGUAGE OverloadedStrings #-}

module GitRestack.Nuke
  ( NukeOptions(..)
  , runNuke
  ) where

import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import GitRestack.Git
import GitRestack.Types
import GitRestack.Utils (isFixBranch, makeFixBranchName)
import GitRestack.Yaml
import System.Directory
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (takeFileName)


data NukeOptions = NukeOptions
  { nukeForce :: Bool
  , nukeKeepBranches :: Bool
  }

runNuke :: NukeOptions -> IO ()
runNuke opts = do
  cwd <- getCwd
  let repoName = takeFileName cwd
  let worktreePath = "../" <> repoName <> "-restack"
  let planPath = ".git/git-restack/plan.yml"

  hasStateDir <- doesDirectoryExist ".git/git-restack"
  hasWorktree <- doesPathExist worktreePath

  planInfo <- loadPlanInfo planPath repoName
  let planWorktreePath = planInfoWorktree planInfo
  let fixBranches = planInfoFixBranches planInfo
  let backupBranches = planInfoBackupBranches planInfo
  let planBranches = planInfoPlanBranches planInfo

  hasPlanWorktree <- maybe (pure False) doesPathExist planWorktreePath

  when (not hasStateDir && not hasWorktree && not hasPlanWorktree && null fixBranches && null backupBranches && null planBranches) $ do
    putStrLn "Nothing to clean up. No git-restack state found."
    exitWith ExitSuccess

  putStrLn "\nThis will delete:\n"
  when hasStateDir $ do
    putStrLn "  - State directory: .git/git-restack"
    hasPlan <- doesFileExist planPath
    when hasPlan $ putStrLn ("    - Plan file: " <> planPath)

  when hasWorktree $ putStrLn ("  - Worktree: " <> worktreePath)
  when hasPlanWorktree $ putStrLn ("  - Plan worktree: " <> fromMaybe "" planWorktreePath)

  when (not (null fixBranches) && not (nukeKeepBranches opts)) $ do
    putStrLn ("  - " <> show (length fixBranches) <> " git-restack/fix branches:")
    forM_ fixBranches $ \branch -> putStrLn ("    - " <> branch)

  when (not (null backupBranches) && not (nukeKeepBranches opts)) $ do
    putStrLn ("  - " <> show (length backupBranches) <> " backup branches:")
    forM_ backupBranches $ \branch -> putStrLn ("    - " <> branch)

  when (not (null planBranches) && not (nukeKeepBranches opts)) $ do
    putStrLn ("  - " <> show (length planBranches) <> " plan branches:")
    forM_ planBranches $ \branch -> putStrLn ("    - " <> branch)

  putStrLn ""

  when (not (nukeForce opts)) $ do
    putStrLn "Are you sure? This cannot be undone."
    putStrLn "Run with --force to confirm."
    exitWith ExitSuccess

  putStrLn "\nCleaning up...\n"

  when hasWorktree $ removeWorktree worktreePath
  when hasPlanWorktree $ do
    let planPathToRemove = fromMaybe worktreePath planWorktreePath
    when (planPathToRemove /= worktreePath) $ removeWorktree planPathToRemove

  _ <- runGitWithStatus ["worktree", "prune"]

  when (not (nukeKeepBranches opts)) $ do
    deleteBranches fixBranches "Deleted"
    deleteBranches backupBranches "Deleted backup"
    deleteBranches planBranches "Deleted plan"

  when hasStateDir $ do
    removeDirectoryRecursive ".git/git-restack"
    putStrLn "  [OK] Removed state directory"

  putStrLn "\nDone! All git-restack state has been removed."
  putStrLn "Your original branches are unchanged."

removeWorktree :: FilePath -> IO ()
removeWorktree path = do
  result <- runGitWithStatus ["worktree", "remove", path, "--force"]
  if grExitCode result == 0
    then putStrLn ("  [OK] Removed worktree: " <> path)
    else do
      removePathForcibly path
      putStrLn ("  [OK] Removed worktree (forced): " <> path)


deleteBranches :: [String] -> String -> IO ()
deleteBranches branches label =
  forM_ branches $ \branch -> do
    result <- runGitWithStatus ["branch", "-D", branch]
    if grExitCode result == 0
      then putStrLn ("  [OK] " <> label <> ": " <> branch)
      else putStrLn ("  [WARN] Could not delete: " <> branch)


data PlanInfo = PlanInfo
  { planInfoFixBranches :: [String]
  , planInfoBackupBranches :: [String]
  , planInfoPlanBranches :: [String]
  , planInfoWorktree :: Maybe FilePath
  }

loadPlanInfo :: FilePath -> FilePath -> IO PlanInfo
loadPlanInfo planPath repoName = do
  hasPlan <- doesFileExist planPath
  if hasPlan
    then do
      content <- BS.readFile planPath
      case parsePlan content of
        Right plan -> do
          let fixBranches = map (Text.unpack . makeFixBranchName . sbName) (stBranches (planStack plan))
          let (planWorktree, planBranches, backupBranches) =
                case planSimulation plan of
                  Nothing -> (fallbackPlanWorktree repoName, [], [])
                  Just sim ->
                    ( fromMaybe (fallbackPlanWorktree repoName) (simWorktreePath sim)
                    , maybe [] (\name -> [Text.unpack name]) (simPlanBranch sim)
                    , map (Text.unpack . backupName) (simBackupBranches sim)
                    )
          pure PlanInfo
            { planInfoFixBranches = fixBranches
            , planInfoBackupBranches = backupBranches
            , planInfoPlanBranches = planBranches
            , planInfoWorktree = planWorktree
            }
        Left _ -> fallbackInfo repoName
    else fallbackInfo repoName
  where
    fallbackPlanWorktree name = Just ("../" <> name <> "-restack-plan")
    fallbackInfo name = do
      fixBranches <- listBranches "git-restack/fix/*" (\b -> isFixBranch (Text.pack b))
      backupBranches <- listBranches "*-restack-backup-*" (\b -> "restack-backup" `Text.isInfixOf` Text.pack b)
      planBranches <- listBranches "git-restack-plan-*" (\b -> "git-restack-plan-" `Text.isPrefixOf` Text.pack b)
      pure PlanInfo
        { planInfoFixBranches = fixBranches
        , planInfoBackupBranches = backupBranches
        , planInfoPlanBranches = planBranches
        , planInfoWorktree = fallbackPlanWorktree name
        }

listBranches :: String -> (String -> Bool) -> IO [String]
listBranches pattern predicate = do
  result <- runGitWithStatus ["branch", "--list", pattern]
  if grExitCode result /= 0
    then pure []
    else pure (filter predicate (map parseBranchLine (lines (BSC.unpack (grStdout result)))))
  where
    parseBranchLine line =
      let trimmed = dropWhile (== ' ') (dropWhile (== '*') line)
      in dropWhile (== ' ') trimmed
