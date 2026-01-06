{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitRestack.Apply
  ( ApplyOptions(..)
  , runApply
  ) where

import Control.Exception (catch)
import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import GitRestack.Git
import GitRestack.Types
import GitRestack.Utils (makeFixBranchName, trimBS)
import GitRestack.Yaml
import System.Directory
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (takeFileName)


data ApplyOptions = ApplyOptions
  { aoPlanFile :: FilePath
  , aoWorktreePath :: Maybe FilePath
  , aoForce :: Bool
  , aoCleanup :: Bool
  , aoDryRun :: Bool
  }

runApply :: ApplyOptions -> IO ()
runApply opts = do
  plan <- loadPlan (aoPlanFile opts)
  worktreePath <- resolveWorktreePath (aoWorktreePath opts)

  exists <- doesPathExist worktreePath
  unless exists $ do
    putStrLn ("Error: Worktree '" <> worktreePath <> "' does not exist.")
    putStrLn ("Did you run 'git-restack exec " <> aoPlanFile opts <> "' first?")
    exitWith (ExitFailure 1)

  putStrLn "\nVerifying plan against current repository state...\n"

  (diverged, details) <- verifyDivergence plan
  when (diverged && not (aoForce opts)) $ do
    putStrLn "Error: Repository has diverged from the plan!\n"
    putStrLn "The following branches have changed since the plan was created:"
    putStrLn details
    putStrLn "This could mean:"
    putStrLn "  - Someone pushed new commits to these branches"
    putStrLn "  - You created the plan from a different repository state"
    putStrLn "\nOptions:"
    putStrLn "  1. Re-run 'git-restack plan' to create a new plan"
    putStrLn "  2. Use --force to apply anyway (dangerous!)"
    exitWith (ExitFailure 1)

  when diverged $
    putStrLn "Warning: Applying despite divergence (--force)\n"

  unless diverged $
    putStrLn "[OK] All branches match the plan\n"

  putStrLn "Verifying git-restack/fix/* branches in worktree...\n"
  forM_ (stBranches (planStack plan)) $ \branch -> do
    let fixBranchName = Text.unpack (makeFixBranchName (sbName branch))
    result <- runGitWithStatus ["-C", worktreePath, "rev-parse", "--verify", fixBranchName]
    when (grExitCode result /= 0) $ do
      putStrLn ("Error: Branch '" <> fixBranchName <> "' not found in worktree.")
      putStrLn "Did 'git-restack exec' complete successfully?"
      exitWith (ExitFailure 1)
    putStrLn ("  [OK] " <> fixBranchName)

  putStrLn ""

  if aoDryRun opts
    then putStrLn "Dry run: The following changes would be made:\n"
    else putStrLn "Applying changes...\n"

  forM_ (stBranches (planStack plan)) $ \branch -> do
    let fixBranchName = Text.unpack (makeFixBranchName (sbName branch))
    fixCommit <- getFixCommit worktreePath fixBranchName
    if aoDryRun opts
      then putStrLn ("  " <> Text.unpack (sbName branch) <> " -> " <> shortSha fixCommit)
      else do
        result <- runGitWithStatus ["branch", "-f", Text.unpack (sbName branch), Text.unpack fixCommit]
        when (grExitCode result /= 0) $ do
          putStrLn ("Error: Failed to update '" <> Text.unpack (sbName branch) <> "': " <> BSC.unpack (grStderr result))
          exitWith (ExitFailure 1)
        putStrLn ("  [OK] " <> Text.unpack (sbName branch) <> " -> " <> shortSha fixCommit)

  when (aoDryRun opts) $ do
    putStrLn "\nRun without --dry-run to apply these changes."
    exitWith ExitSuccess

  when (aoCleanup opts) $ do
    putStrLn "\nCleaning up..."
    forM_ (stBranches (planStack plan)) $ \branch -> do
      let fixBranchName = Text.unpack (makeFixBranchName (sbName branch))
      _ <- runGitWithStatus ["-C", worktreePath, "branch", "-D", fixBranchName]
      pure ()
    _ <- runGitWithStatus ["worktree", "remove", worktreePath, "--force"]
    putStrLn ("  [OK] Removed worktree: " <> worktreePath)

  putStrLn "\nApply complete!\n"
  putStrLn "Updated branches:"
  forM_ (stBranches (planStack plan)) $ \branch ->
    putStrLn ("  - " <> Text.unpack (sbName branch))

  putStrLn "\nNext steps:"
  putStrLn "  1. Verify the changes: git log --oneline --graph"
  putStrLn "  2. Force-push the updated branches:"
  forM_ (stBranches (planStack plan)) $ \branch ->
    putStrLn ("       git push --force-with-lease origin " <> Text.unpack (sbName branch))

  unless (aoCleanup opts) $ do
    putStrLn "  3. Clean up worktree when satisfied:"
    putStrLn ("       git worktree remove " <> worktreePath)

  putStrLn "\nNote: Your staged/unstaged changes are now in the commits."
  putStrLn "Run 'git checkout .' to discard the working tree changes."

resolveWorktreePath :: Maybe FilePath -> IO FilePath
resolveWorktreePath provided = do
  cwd <- getCwd
  let defaultWorktree = "../" <> takeFileName cwd <> "-restack"
  pure (fromMaybe defaultWorktree provided)

verifyDivergence :: Plan -> IO (Bool, String)
verifyDivergence plan = do
  results <- mapM checkBranch (stBranches (planStack plan))
  let diverged = any fst results
  let details = concatMap snd results
  pure (diverged, details)
  where
    checkBranch branch = do
      result <- runGitWithStatus ["rev-parse", Text.unpack (sbName branch)]
      if grExitCode result /= 0
        then pure (True, "  " <> Text.unpack (sbName branch) <> ": branch missing\n")
        else do
          let currentCommit = TextEnc.decodeUtf8 (trimBS (grStdout result))
          if currentCommit /= sbCommitSha branch
            then pure (True, "  " <> Text.unpack (sbName branch) <> ":\n    Plan:    " <> Text.unpack (sbCommitSha branch) <> "\n    Current: " <> Text.unpack currentCommit <> "\n")
            else pure (False, "")

getFixCommit :: FilePath -> FilePath -> IO Text
getFixCommit worktreePath fixBranchName = do
  result <- runGitWithStatus ["-C", worktreePath, "rev-parse", fixBranchName]
  when (grExitCode result /= 0) $ do
    putStrLn ("Error: Could not get commit for '" <> fixBranchName <> "'")
    exitWith (ExitFailure 1)
  pure (TextEnc.decodeUtf8 (trimBS (grStdout result)))

shortSha :: Text -> String
shortSha sha = Text.unpack (Text.take 7 sha)

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
