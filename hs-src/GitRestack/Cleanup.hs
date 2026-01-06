{-# LANGUAGE OverloadedStrings #-}

module GitRestack.Cleanup
  ( CleanupOptions(..)
  , runCleanup
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


data CleanupOptions = CleanupOptions
  { coPlanFile :: Maybe FilePath
  , coWorktreePath :: Maybe FilePath
  , coForce :: Bool
  , coKeepPlan :: Bool
  }

runCleanup :: CleanupOptions -> IO ()
runCleanup opts = do
  cwd <- getCwd
  let repoName = takeFileName cwd
  let defaultWorktree = "../" <> repoName <> "-restack"
  let worktreePath = fromMaybe defaultWorktree (coWorktreePath opts)

  let planPath = fromMaybe ".git/git-restack/plan.yml" (coPlanFile opts)

  planInfo <- loadPlanInfo planPath
  let planWorktreePath = planInfoWorktree planInfo
  let fixBranches = planInfoFixBranches planInfo
  let backupBranches = planInfoBackupBranches planInfo
  let planBranches = planInfoPlanBranches planInfo

  hasWorktree <- doesPathExist worktreePath
  hasPlanWorktree <- maybe (pure False) doesPathExist planWorktreePath

  let hasPlanFile = planInfoHasPlan planInfo
  let hasStateFile = planInfoHasState planInfo

  when (not hasWorktree && not hasPlanWorktree && null fixBranches && null backupBranches && null planBranches && not hasPlanFile && not hasStateFile) $ do
    putStrLn "No git-restack artifacts found."
    exitWith ExitSuccess

  putStrLn "\nCleanup will remove:\n"
  when hasWorktree $ putStrLn ("  - Worktree: " <> worktreePath)
  when hasPlanWorktree $ putStrLn ("  - Plan worktree: " <> fromMaybe "" planWorktreePath)

  when (not (null fixBranches)) $ do
    putStrLn ("  - " <> show (length fixBranches) <> " git-restack/fix branches:")
    forM_ fixBranches $ \branch -> putStrLn ("    - " <> branch)

  when (not (null backupBranches)) $ do
    putStrLn ("  - " <> show (length backupBranches) <> " backup branches:")
    forM_ backupBranches $ \branch -> putStrLn ("    - " <> branch)

  when (not (null planBranches)) $ do
    putStrLn ("  - " <> show (length planBranches) <> " plan branches:")
    forM_ planBranches $ \branch -> putStrLn ("    - " <> branch)

  when (not (coKeepPlan opts)) $ do
    when hasPlanFile $ putStrLn ("  - Plan file: " <> planPath)
    when hasStateFile $ putStrLn "  - State file: .git/git-restack/state.json"

  putStrLn ""

  unlessForce (coForce opts) $ do
    putStrLn "Run with --force to confirm cleanup."
    exitWith ExitSuccess

  putStrLn "\nCleaning up...\n"

  when hasWorktree $ removeWorktree worktreePath
  when hasPlanWorktree $ do
    let planPathToRemove = fromMaybe worktreePath planWorktreePath
    when (planPathToRemove /= worktreePath) $ removeWorktree planPathToRemove

  _ <- runGitWithStatus ["worktree", "prune"]

  deleteBranches fixBranches "Deleted"
  deleteBranches backupBranches "Deleted backup"
  deleteBranches planBranches "Deleted plan"

  when (not (coKeepPlan opts)) $ do
    removeFileIfExists ".git/git-restack/state.json"
    removeFileIfExists planPath
    removeDirIfExists ".git/git-restack"
    putStrLn "  [OK] Removed state files"

  putStrLn "\nDone! Cleanup complete."

unlessForce :: Bool -> IO () -> IO ()
unlessForce forceFlag action = when (not forceFlag) action

removeWorktree :: FilePath -> IO ()
removeWorktree path = do
  result <- runGitWithStatus ["worktree", "remove", path, "--force"]
  if grExitCode result == 0
    then putStrLn ("  [OK] Removed worktree: " <> path)
    else do
      removePathForcibly path
      putStrLn ("  [OK] Removed worktree (forced): " <> path)

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
  exists <- doesFileExist path
  when exists (removeFile path)

removeDirIfExists :: FilePath -> IO ()
removeDirIfExists path = do
  exists <- doesDirectoryExist path
  when exists (removeDirectoryRecursive path)


data PlanInfo = PlanInfo
  { planInfoFixBranches :: [String]
  , planInfoBackupBranches :: [String]
  , planInfoPlanBranches :: [String]
  , planInfoWorktree :: Maybe FilePath
  , planInfoHasPlan :: Bool
  , planInfoHasState :: Bool
  }

loadPlanInfo :: FilePath -> IO PlanInfo
loadPlanInfo planPath = do
  hasPlan <- doesFileExist planPath
  hasState <- doesFileExist ".git/git-restack/state.json"
  if hasPlan
    then do
      content <- BS.readFile planPath
      case parsePlan content of
        Right plan -> do
          let fixBranches = map (Text.unpack . makeFixBranchName . sbName) (stBranches (planStack plan))
          let (planWorktree, planBranches, backupBranches) =
                case planSimulation plan of
                  Nothing -> (Nothing, [], [])
                  Just sim ->
                    ( simWorktreePath sim
                    , maybe [] (\name -> [Text.unpack name]) (simPlanBranch sim)
                    , map (Text.unpack . backupName) (simBackupBranches sim)
                    )
          pure PlanInfo
            { planInfoFixBranches = fixBranches
            , planInfoBackupBranches = backupBranches
            , planInfoPlanBranches = planBranches
            , planInfoWorktree = planWorktree
            , planInfoHasPlan = True
            , planInfoHasState = hasState
            }
        Left _ -> fallbackPlanInfo hasPlan hasState
    else fallbackPlanInfo hasPlan hasState
  where
    fallbackPlanInfo hasPlan hasState = do
      fixBranches <- listBranches "git-restack/fix/*" (\name -> isFixBranch (Text.pack name))
      backupBranches <- listBranches "*-restack-backup-*" (\name -> "restack-backup" `Text.isInfixOf` Text.pack name)
      planBranches <- listBranches "git-restack-plan-*" (\name -> "git-restack-plan-" `Text.isPrefixOf` Text.pack name)
      pure PlanInfo
        { planInfoFixBranches = fixBranches
        , planInfoBackupBranches = backupBranches
        , planInfoPlanBranches = planBranches
        , planInfoWorktree = Nothing
        , planInfoHasPlan = hasPlan
        , planInfoHasState = hasState
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


deleteBranches :: [String] -> String -> IO ()
deleteBranches branches label =
  forM_ branches $ \branch -> do
    result <- runGitWithStatus ["branch", "-D", branch]
    if grExitCode result == 0
      then putStrLn ("  [OK] " <> label <> ": " <> branch)
      else putStrLn ("  [WARN] Could not delete: " <> branch)
