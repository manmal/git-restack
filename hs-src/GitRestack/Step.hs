{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitRestack.Step
  ( StepOptions(..)
  , runStep
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


data StepOptions = StepOptions
  { soPlanFile :: FilePath
  , soWorktreePath :: Maybe FilePath
  }

runStep :: StepOptions -> IO ()
runStep opts = do
  plan <- loadPlan (soPlanFile opts)
  when (not (null (planErrors plan))) $ do
    putStrLn "Error: Plan has unresolved errors."
    exitWith (ExitFailure 3)

  validatePlanStack plan `catch` \(_ :: GitError) -> do
    putStrLn "Error: Plan is out of date."
    putStrLn "Re-run: git-restack plan"
    exitWith (ExitFailure 3)

  worktreePath <- resolveWorktreePath (soWorktreePath opts)
  hasState <- stateExists
  if hasState
    then continueStep worktreePath plan
    else startFresh worktreePath plan (soPlanFile opts)

startFresh :: FilePath -> Plan -> FilePath -> IO ()
startFresh worktreePath plan planFile = do
  exists <- doesPathExist worktreePath
  when exists $ do
    putStrLn ("Error: Worktree path '" <> worktreePath <> "' already exists but no state file found.")
    putStrLn "Run 'git-restack cleanup' first, then try again."
    exitWith (ExitFailure 1)

  _ <- runGit ["worktree", "add", worktreePath, Text.unpack (stBaseBranch (planStack plan))]

  timestamp <- formatTimestamp
  let state = ExecutionState
        { esPlanFile = planFile
        , esPlanHash = "sha256:TODO"
        , esWorktreePath = worktreePath
        , esCurrentStepIndex = 0
        , esCurrentCommitIndex = 0
        , esStartedAt = timestamp
        , esLastUpdated = timestamp
        , esStatus = StatusInProgress
        , esCompletedBranches = []
        , esVerifyCmd = planVerifyCmd plan
        , esMode = ExecutionStep
        }
  saveState state
  putStrLn ("\nInitialized execution state. Starting step 1/" <> show (length (stBranches (planStack plan))))
  executeStep worktreePath plan state

continueStep :: FilePath -> Plan -> IO ()
continueStep worktreePath plan = do
  state <- loadState
  cherryPickInProgress <- doesFileExist (worktreePath </> ".git" </> "CHERRY_PICK_HEAD")
  when cherryPickInProgress $ do
    putStrLn "Error: Cherry-pick still in progress in worktree."
    putStrLn ("Resolve conflicts, then: cd " <> worktreePath <> " && git add . && git cherry-pick --continue && cd -")
    putStrLn "Then run: git-restack step"
    exitWith (ExitFailure 2)

  when (esCurrentStepIndex state >= length (stBranches (planStack plan))) $ do
    putStrLn "\nAll steps complete!"
    putStrLn "Run: git-restack apply"
    cleanupState
    exitWith ExitSuccess

  executeStep worktreePath plan state

executeStep :: FilePath -> Plan -> ExecutionState -> IO ()
executeStep worktreePath plan state0 = do
  let branchIdx = esCurrentStepIndex state0
  let branch = stBranches (planStack plan) !! branchIdx

  putStrLn ("\n[" <> show (branchIdx + 1) <> "/" <> show (length (stBranches (planStack plan))) <> "] Processing: " <> Text.unpack (sbName branch))

  let fixBranchName = Text.unpack (makeFixBranchName (sbName branch))
  branchExists <- branchExistsInWorktree worktreePath fixBranchName
  state <- if branchExists
    then do
      _ <- runGitInDir worktreePath ["checkout", fixBranchName]
      pure state0
    else do
      parentBranch <- resolveParentFix plan branch
      _ <- runGitInDir worktreePath ["checkout", parentBranch]
      _ <- runGitInDir worktreePath ["checkout", "-b", fixBranchName]
      pure state0 { esCurrentCommitIndex = 0 }

  let conflictUsed = replicate (length (planConflicts plan)) False
  usedAfterCherry <- cherryPickCommits worktreePath plan conflictUsed state branch
  usedAfterFix <- applyBranchFixes worktreePath plan usedAfterCherry branch

  case planVerifyCmd plan of
    Nothing -> pure ()
    Just cmd -> do
      ok <- runVerification worktreePath cmd
      unless ok $ do
        let failedState = state { esStatus = StatusConflict }
        saveState failedState
        exitWith (ExitFailure 4)

  putStrLn ("  [OK] " <> fixBranchName <> " complete")

  timestamp <- formatTimestamp
  let nextState = state
        { esCurrentStepIndex = esCurrentStepIndex state + 1
        , esCurrentCommitIndex = 0
        , esLastUpdated = timestamp
        }
  if esCurrentStepIndex nextState >= length (stBranches (planStack plan))
    then do
      saveState nextState { esStatus = StatusCompleted }
      putStrLn ("\nAll " <> show (length (stBranches (planStack plan))) <> " branches complete!")
      putStrLn "Run: git-restack apply"
      cleanupState
    else do
      saveState nextState
      putStrLn ("\nStep " <> show (esCurrentStepIndex nextState) <> "/" <> show (length (stBranches (planStack plan))) <> " done. Run 'git-restack step' for next.")

branchExistsInWorktree :: FilePath -> FilePath -> IO Bool
branchExistsInWorktree worktreePath branchName = do
  result <- runGitWithStatus ["-C", worktreePath, "rev-parse", "--verify", branchName]
  pure (grExitCode result == 0)

resolveParentFix :: Plan -> StackBranch -> IO FilePath
resolveParentFix plan branch =
  case sbParentBranch branch of
    Nothing -> pure (Text.unpack (stBaseBranch (planStack plan)))
    Just parentName ->
      if parentName == stBaseBranch (planStack plan)
        then pure (Text.unpack parentName)
        else pure (Text.unpack (makeFixBranchName parentName))

cherryPickCommits :: FilePath -> Plan -> [Bool] -> ExecutionState -> StackBranch -> IO [Bool]
cherryPickCommits wtPath plan used state branch =
  case sbParentBranch branch of
    Nothing -> pure used
    Just parent -> do
      let range = Text.unpack (parent <> ".." <> sbName branch)
      commitsOutput <- runGit ["rev-list", "--reverse", range]
  let commits = filter (not . Text.null) (map TextEnc.decodeUtf8 (BSC.lines (trimBS commitsOutput)))
      let totalCommits = length commits
      let startIndex = esCurrentCommitIndex state
      let indexed = zip [0 ..] commits
      foldl' (cherryPickCommit totalCommits) (pure used) (drop startIndex indexed)
  where
    cherryPickCommit total action (idx, commitSha) = do
      usedAcc <- action
      putStrLn ("  Cherry-picking commit " <> show (idx + 1) <> "/" <> show total <> ": " <> Text.unpack (Text.take 7 commitSha))
      result <- runGitWithStatusNoRerere ["-C", wtPath, "cherry-pick", Text.unpack commitSha]
      if grExitCode result == 0
        then do
          updateCommitIndex (idx + 1)
          pure usedAcc
        else do
          let conflictIdx = findCherryPickConflict plan usedAcc (sbName branch) commitSha
          case conflictIdx of
            Nothing -> do
              putStrLn "Error: Conflict detected but no resolution found in plan."
              updateCommitIndex (idx + 1)
              exitWith (ExitFailure 2)
            Just ci -> do
              applyConflictResolution wtPath (planConflicts plan !! ci) `catch` \(GitError msg) -> do
                putStrLn ("Error: Failed to apply conflict resolution: " <> msg)
                exitWith (ExitFailure 2)
              let usedNext = markUsed usedAcc ci
              ok <- finishCherryPick wtPath
              unless ok $ do
                putStrLn "Error: Cherry-pick --continue failed after applying resolution."
                exitWith (ExitFailure 2)
              updateCommitIndex (idx + 1)
              pure usedNext

    updateCommitIndex newIdx = do
      timestamp <- formatTimestamp
      state <- loadState
      saveState state { esCurrentCommitIndex = newIdx, esLastUpdated = timestamp }

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
  (exitCode, _stdout, _stderr) <- readCreateProcessWithExitCode processSpec ""
  case exitCode of
    ExitSuccess -> do
      putStrLn "    [OK] Verification passed"
      pure True
    ExitFailure code -> do
      putStrLn ("    [FAIL] Verification failed (exit code: " <> show code <> ")")
      pure False

resolveWorktreePath :: Maybe FilePath -> IO FilePath
resolveWorktreePath provided = do
  cwd <- getCwd
  let defaultWorktree = "../" <> takeFileName cwd <> "-restack"
  pure (fromMaybe defaultWorktree provided)

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

markUsed :: [Bool] -> Int -> [Bool]
markUsed used idx = take idx used ++ [True] ++ drop (idx + 1) used

stateDir :: FilePath
stateDir = ".git/git-restack"

stateFile :: FilePath
stateFile = ".git/git-restack/state.json"
