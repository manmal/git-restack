{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitRestack.Exec
  ( ExecOptions(..)
  , runExec
  ) where

import Control.Exception (catch, throwIO)
import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import GitRestack.Conflict
import GitRestack.Diff
import GitRestack.Git
import GitRestack.Stack
import GitRestack.Types
import GitRestack.Utils
import GitRestack.Yaml
import System.Directory
import System.Exit (ExitCode(..), exitWith)
import System.FilePath ((</>), takeFileName)
import System.Process (proc, readCreateProcessWithExitCode)


data ExecOptions = ExecOptions
  { eoPlanFile :: FilePath
  , eoWorktreePath :: Maybe FilePath
  , eoContinue :: Bool
  , eoAbort :: Bool
  , eoForce :: Bool
  }

runExec :: ExecOptions -> IO ()
runExec opts
  | eoAbort opts = handleAbort
  | eoContinue opts = handleContinue
  | otherwise = runFresh
  where
    runFresh = do
      plan <- loadPlan (eoPlanFile opts)
      when (not (null (planErrors plan))) $ do
        putStrLn ("Error: Plan has " <> show (length (planErrors plan)) <> " unresolved errors.\n")
        forM_ (zip [1 :: Int ..] (planErrors plan)) $ \(idx, err) -> do
          putStrLn ("  " <> show idx <> ". [" <> Text.unpack (errorTypeToText (peType err)) <> "] " <> pePath err)
          putStrLn ("     " <> Text.unpack (peMessage err) <> "\n")
        putStrLn ("Fix these in " <> eoPlanFile opts <> " first, then run: git-restack exec " <> eoPlanFile opts)
        exitWith (ExitFailure 3)

      validatePlanStack plan `catch` \(_ :: GitError) -> do
        putStrLn "Error: Plan is out of date."
        putStrLn "Re-run: git-restack plan"
        exitWith (ExitFailure 3)

      hasState <- stateExists
      when hasState $ do
        putStrLn "Warning: Found interrupted execution."
        putStrLn "Run with --continue to resume or --abort to clean up."
        exitWith (ExitFailure 1)

      worktreePath <- resolveWorktreePath (eoWorktreePath opts)
      checkWorktreePath worktreePath (eoForce opts)

      let branchesToFix = length (filter sbNeedsFix (stBranches (planStack plan)))
      if branchesToFix == 0 && planVerifyCmd plan == Nothing
        then putStrLn "No branches need fixing. Nothing to do."
        else do
          if branchesToFix == 0
            then putStrLn ("\nVerify-only mode: " <> show (length (stBranches (planStack plan))) <> " branches to restack with verification\n")
            else putStrLn ("\nExecuting plan: " <> show (length (stBranches (planStack plan))) <> " branches to restack, " <> show branchesToFix <> " need fixes\n")

          _ <- runGit ["worktree", "add", worktreePath, Text.unpack (stBaseBranch (planStack plan))]

          timestamp <- formatTimestamp
          let state = ExecutionState
                { esPlanFile = eoPlanFile opts
                , esPlanHash = "sha256:TODO"
                , esWorktreePath = worktreePath
                , esCurrentStepIndex = 0
                , esCurrentCommitIndex = 0
                , esStartedAt = timestamp
                , esLastUpdated = timestamp
                , esStatus = StatusInProgress
                , esCompletedBranches = []
                , esVerifyCmd = planVerifyCmd plan
                , esMode = ExecutionExec
                }
          saveState state
          executePlan worktreePath plan state

    handleAbort = do
      hasState <- stateExists
      unless hasState $ do
        putStrLn "Error: No execution in progress."
        exitWith (ExitFailure 1)
      state <- loadState
      putStrLn "Aborting execution..."
      putStrLn ("Removing worktree: " <> esWorktreePath state)
      _ <- runGitWithStatus ["worktree", "remove", esWorktreePath state, "--force"]
      cleanupState
      putStrLn "Aborted. Original branches unchanged."

    handleContinue = do
      hasState <- stateExists
      unless hasState $ do
        putStrLn "Error: No execution in progress."
        exitWith (ExitFailure 1)

      state <- loadState
      plan <- loadPlan (esPlanFile state)

      let worktreePath = esWorktreePath state
      exists <- doesPathExist worktreePath
      unless exists $ do
        putStrLn ("Error: Worktree '" <> worktreePath <> "' no longer exists.")
        putStrLn "Run --abort to clean up state."
        exitWith (ExitFailure 1)

      putStrLn ("Resuming execution from step " <> show (esCurrentStepIndex state + 1) <> "...")
      putStrLn ("Worktree: " <> worktreePath)

      let conflictUsed = markUsedConflicts (planConflicts plan) (esCompletedBranches state)
      resumePlan worktreePath plan state conflictUsed

resolveWorktreePath :: Maybe FilePath -> IO FilePath
resolveWorktreePath provided = do
  cwd <- getCwd
  let defaultWorktree = "../" <> takeFileName cwd <> "-restack"
  pure (fromMaybe defaultWorktree provided)

checkWorktreePath :: FilePath -> Bool -> IO ()
checkWorktreePath worktreePath forceFlag = do
  exists <- doesPathExist worktreePath
  when (exists && not forceFlag) $ do
    putStrLn ("Error: Worktree path '" <> worktreePath <> "' already exists.")
    putStrLn "Use --force to remove it first, or --worktree-path to specify a different path."
    exitWith (ExitFailure 1)
  when (exists && forceFlag) $ do
    putStrLn ("Removing existing worktree: " <> worktreePath)
    _ <- runGitWithStatus ["worktree", "remove", worktreePath, "--force"]
    pure ()

loadPlan :: FilePath -> IO Plan
loadPlan planFile = do
  content <- BS.readFile planFile `catch` \(_ :: IOError) -> do
    putStrLn ("Error: Could not read plan file '" <> planFile <> "'")
    exitWith (ExitFailure 1)
  case parsePlan content of
    Left err -> do
      putStrLn ("Error: Could not parse plan file: " <> err)
      exitWith (ExitFailure 1)
    Right plan -> pure plan

loadState :: IO ExecutionState
loadState = do
  content <- BSC.readFile stateFile
  case parseState content of
    Left err -> do
      putStrLn ("Error: Could not parse state file: " <> err)
      exitWith (ExitFailure 1)
    Right state -> pure state

executePlan :: FilePath -> Plan -> ExecutionState -> IO ()
executePlan worktreePath plan state0 = do
  let conflictUsed = replicate (length (planConflicts plan)) False
  (finalState, completed, _) <- foldl' (runBranch worktreePath plan) (pure (state0, [], conflictUsed)) (zip [0 :: Int ..] (stBranches (planStack plan)))
  let finished = finalState
        { esStatus = StatusCompleted
        , esCompletedBranches = completed
        }
  saveState finished
  printCompletion worktreePath (esCompletedBranches finished)
  cleanupState
  where
    runBranch wtPath planAcc action (idx, branch) = do
      (state, completed, used) <- action
      timestamp <- formatTimestamp
      let state' = state { esCurrentStepIndex = idx, esLastUpdated = timestamp }
      saveState state'
      putStrLn ("\n[" <> show (idx + 1) <> "/" <> show (length (stBranches (planStack planAcc))) <> "] Processing: " <> Text.unpack (sbName branch))

      let fixBranchName = Text.unpack (makeFixBranchName (sbName branch))
      _ <- runGitInDir wtPath ["checkout", "-b", fixBranchName]

      usedAfterCherry <- cherryPickBranch wtPath planAcc used branch
      usedAfterFix <- applyBranchFixes wtPath planAcc usedAfterCherry branch

      case planVerifyCmd planAcc of
        Nothing -> pure ()
        Just cmd -> do
          ok <- runVerification wtPath cmd
          unless ok $ do
            putStrLn ("\nVerification failed for branch " <> Text.unpack (sbName branch) <> "!")
            putStrLn ("Fix the issue in: " <> wtPath)
            putStrLn "Then amend the commit and run: git-restack exec --continue"
            let failedState = state' { esStatus = StatusConflict }
            saveState failedState
            exitWith (ExitFailure 4)

      let completedBranch = Text.pack fixBranchName
      putStrLn ("  [OK] " <> fixBranchName <> " complete")
      pure (state', completed ++ [completedBranch], usedAfterFix)

resumePlan :: FilePath -> Plan -> ExecutionState -> [Bool] -> IO ()
resumePlan worktreePath plan state0 conflictUsed = do
  let startIdx = esCurrentStepIndex state0
  let branches = drop startIdx (stBranches (planStack plan))
  (finalState, completed, _) <- foldl' (runBranch worktreePath plan startIdx) (pure (state0, esCompletedBranches state0, conflictUsed)) (zip [startIdx ..] branches)
  let finished = finalState
        { esStatus = StatusCompleted
        , esCompletedBranches = completed
        }
  saveState finished
  printCompletion worktreePath (esCompletedBranches finished)
  cleanupState
  where
    runBranch wtPath planAcc startIndex action (idx, branch) = do
      (state, completed, used) <- action
      timestamp <- formatTimestamp
      let state' = state { esCurrentStepIndex = idx, esLastUpdated = timestamp, esStatus = StatusInProgress }
      saveState state'
      putStrLn ("\n[" <> show (idx + 1) <> "/" <> show (length (stBranches (planStack planAcc))) <> "] Processing: " <> Text.unpack (sbName branch))

      let fixBranchName = Text.unpack (makeFixBranchName (sbName branch))
      if idx == startIndex
        then putStrLn "  Resumed after conflict resolution"
        else do
          _ <- runGitInDir wtPath ["checkout", "-b", fixBranchName]
          pure ()

      usedAfterCherry <- if idx == startIndex
        then pure used
        else cherryPickBranch wtPath planAcc used branch
      usedAfterFix <- applyBranchFixes wtPath planAcc usedAfterCherry branch

      case esVerifyCmd state' of
        Nothing -> pure ()
        Just cmd -> do
          ok <- runVerification wtPath cmd
          unless ok $ do
            putStrLn ("\nVerification failed for branch " <> Text.unpack (sbName branch) <> "!")
            putStrLn ("Fix the issue in: " <> wtPath)
            putStrLn "Then amend the commit and run: git-restack exec --continue"
            let failedState = state' { esStatus = StatusConflict }
            saveState failedState
            exitWith (ExitFailure 4)

      let completedBranch = Text.pack fixBranchName
      putStrLn ("  [OK] " <> fixBranchName <> " complete")
      pure (state', completed ++ [completedBranch], usedAfterFix)

cherryPickBranch :: FilePath -> Plan -> [Bool] -> StackBranch -> IO [Bool]
cherryPickBranch wtPath plan used branch =
  case sbParentBranch branch of
    Nothing -> pure used
    Just parent -> do
      let range = Text.unpack (parent <> ".." <> sbName branch)
      commitsOutput <- runGit ["rev-list", "--reverse", range]
      let commits = filter (not . Text.null) (map TextEnc.decodeUtf8 (BSC.lines (trimBS commitsOutput)))
      foldl' (cherryPickCommit plan branch) (pure used) commits
  where
    cherryPickCommit planAcc branchAcc action commitSha = do
      usedAcc <- action
      putStrLn ("  Cherry-picking commit: " <> Text.unpack (Text.take 7 commitSha))
      result <- runGitWithStatusNoRerere ["-C", wtPath, "cherry-pick", Text.unpack commitSha]
      if grExitCode result == 0
        then pure usedAcc
        else do
          let conflictIdx = findCherryPickConflict planAcc usedAcc (sbName branchAcc) commitSha
          case conflictIdx of
            Nothing -> do
              putStrLn "Error: Conflict detected but no resolution found in plan."
              exitWith (ExitFailure 2)
            Just idx -> do
              applyConflictResolution wtPath (planConflicts planAcc !! idx) `catch` \(GitError msg) -> do
                putStrLn ("Error: Failed to apply conflict resolution: " <> msg)
                exitWith (ExitFailure 2)
              let usedNext = markUsed usedAcc idx
              ok <- finishCherryPick wtPath
              unless ok $ do
                putStrLn "Error: Cherry-pick --continue failed after applying resolution."
                exitWith (ExitFailure 2)
              pure usedNext

applyBranchFixes :: FilePath -> Plan -> [Bool] -> StackBranch -> IO [Bool]
applyBranchFixes wtPath plan used branch =
  case sbFix branch of
    Nothing -> pure used
    Just fix -> do
      putStrLn ("  Applying " <> show (length (fixFiles fix)) <> " file fixes")
      foldl' applyFix (pure used) (fixFiles fix) >>= commitFix
  where
    applyFix action file = do
      usedAcc <- action
      let tmpPath = wtPath </> ".git-restack-patch"
      BS.writeFile tmpPath (fcDiff file)
      result <- runGitWithStatusNoRerere ["-C", wtPath, "apply", "--3way", "--allow-empty", tmpPath]
      removeTemp tmpPath
      if grExitCode result == 0
        then do
          putStrLn ("    Applied: " <> fcPath file)
          pure usedAcc
        else do
          conflictPaths <- getConflictedFiles wtPath
          let conflictIdx = findFixApplyConflict plan usedAcc (sbName branch) conflictPaths
          case conflictIdx of
            Nothing -> do
              putStrLn "Error: Patch conflict but no resolution found in plan."
              exitWith (ExitFailure 2)
            Just idx -> do
              applyConflictResolution wtPath (planConflicts plan !! idx) `catch` \(GitError msg) -> do
                putStrLn ("Error: Failed to apply conflict resolution: " <> msg)
                exitWith (ExitFailure 2)
              pure (markUsed usedAcc idx)

    commitFix usedAcc = do
      _ <- runGitWithStatus ["-C", wtPath, "add", "-A"]
      _ <- runGitWithStatus ["-C", wtPath, "commit", "-m", Text.unpack (fixCommitMessage fix), "--allow-empty"]
      pure usedAcc

    removeTemp path = do
      exists <- doesFileExist path
      when exists (removeFile path)

runVerification :: FilePath -> Text -> IO Bool
runVerification wtPath cmd = do
  putStrLn ("  Running verification: " <> Text.unpack cmd)
  let processSpec = (proc "sh" ["-c", Text.unpack cmd]) { cwd = Just wtPath }
  (exitCode, _stdout, stderr) <- readCreateProcessWithExitCode processSpec ""
  case exitCode of
    ExitSuccess -> do
      putStrLn "    [OK] Verification passed"
      pure True
    ExitFailure code -> do
      putStrLn ("    [FAIL] Verification failed (exit code: " <> show code <> ")")
      when (not (null stderr)) $ putStrLn (drop (length stderr - min 2000 (length stderr)) stderr)
      pure False

printCompletion :: FilePath -> [Text] -> IO ()
printCompletion worktreePath branches = do
  putStrLn "\nExecution complete!\n"
  putStrLn ("Worktree: " <> worktreePath <> "\n")
  putStrLn "Created branches:"
  forM_ branches $ \branch -> putStrLn ("  - " <> Text.unpack branch)
  putStrLn "\nNext steps:"
  putStrLn ("  1. Review the git-restack/fix/* branches in " <> worktreePath)
  putStrLn "  2. If satisfied, push the git-restack/fix/* branches"
  putStrLn "  3. Update PRs to point to the git-restack/fix/* branches"
  putStrLn ("  4. Clean up with: git worktree remove " <> worktreePath)

validatePlanStack :: Plan -> IO ()
validatePlanStack plan = do
  current <- analyzeStack
  let planned = planStack plan
  when (stBaseBranch current /= stBaseBranch planned) $ throwIO (GitError "Invalid plan")
  when (stBaseTip current /= stBaseTip planned) $ throwIO (GitError "Invalid plan")
  when (stHeadBranch current /= stHeadBranch planned) $ throwIO (GitError "Invalid plan")
  when (length (stBranches current) /= length (stBranches planned)) $ throwIO (GitError "Invalid plan")
  forM_ (zip (stBranches current) (stBranches planned)) $ \(branch, plannedBranch) -> do
    when (sbName branch /= sbName plannedBranch) $ throwIO (GitError "Invalid plan")
    when (sbCommitSha branch /= sbCommitSha plannedBranch) $ throwIO (GitError "Invalid plan")

  let plannedSet = map sbName (stBranches planned)
  featureRaw <- runGit ["for-each-ref", "--format=%(refname:short)", "refs/heads/feature"]
  let names = map TextEnc.decodeUtf8 (BSC.lines (trimBS featureRaw))
  forM_ names $ \name -> do
    when (not (Text.null name) && name `notElem` plannedSet) $ do
      inRange <- isBranchInRange (stBaseCommit current) (stHeadCommit current) name
      when inRange $ throwIO (GitError "Invalid plan")

isBranchInRange :: Text -> Text -> Text -> IO Bool
isBranchInRange baseCommit headCommit branch = do
  baseOk <- isAncestor baseCommit branch
  headOk <- isAncestor branch headCommit
  pure (baseOk && headOk)

isAncestor :: Text -> Text -> IO Bool
isAncestor ancestor descendant = do
  result <- runGitWithStatus ["merge-base", "--is-ancestor", Text.unpack ancestor, Text.unpack descendant]
  pure (grExitCode result == 0)

stateExists :: IO Bool
stateExists = doesFileExist stateFile

saveState :: ExecutionState -> IO ()
saveState state = do
  createDirectoryIfMissing True stateDir
  BS.writeFile stateFile (emitState state)

cleanupState :: IO ()
cleanupState = do
  exists <- doesFileExist stateFile
  when exists (removeFile stateFile)
  dirExists <- doesDirectoryExist stateDir
  when dirExists (removeDirectoryRecursive stateDir)

markUsedConflicts :: [PlanConflict] -> [Text] -> [Bool]
markUsedConflicts conflicts completedBranches =
  map (\conflict -> pcBranch conflict `elem` completedBranches) conflicts

markUsed :: [Bool] -> Int -> [Bool]
markUsed used idx = take idx used ++ [True] ++ drop (idx + 1) used

stateDir :: FilePath
stateDir = ".git/git-restack"

stateFile :: FilePath
stateFile = ".git/git-restack/state.json"
