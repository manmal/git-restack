{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (catch)
import qualified Data.Text as Text
import GitRestack.Apply
import GitRestack.Cleanup
import GitRestack.Exec
import GitRestack.Git (GitError)
import GitRestack.Nuke
import GitRestack.Plan
import GitRestack.Stack
import GitRestack.Status
import GitRestack.Step
import GitRestack.Types
import Options.Applicative
import System.Exit (ExitCode(..), exitWith)

versionString :: String
versionString = "0.1.0"

data Command
  = CmdStack StackOptions
  | CmdPlan PlanOptions
  | CmdExec ExecOptions
  | CmdStep StepOptions
  | CmdApply ApplyOptions
  | CmdCleanup CleanupOptions
  | CmdNuke NukeOptions
  | CmdStatus StatusOptions

newtype StackOptions = StackOptions
  { soJson :: Bool
  }

main :: IO ()
main = do
  cmd <- execParser opts
  runCommand cmd
  where
    opts = info (helper <*> versionOption <*> commandParser)
      ( fullDesc
     <> header "git-restack - Strict stacked branch restacking tool" )

versionOption :: Parser (a -> a)
versionOption = infoOption ("git-restack " <> versionString)
  ( long "version"
 <> short 'v'
 <> help "Show version" )

commandParser :: Parser Command
commandParser = hsubparser
  ( command "stack" (info (CmdStack <$> stackOptions) (progDesc "Show the stacked branch hierarchy"))
 <> command "plan" (info (CmdPlan <$> planOptions) (progDesc "Generate a restacking plan"))
 <> command "exec" (info (CmdExec <$> execOptions) (progDesc "Execute the plan"))
 <> command "step" (info (CmdStep <$> stepOptions) (progDesc "Execute one step of the plan"))
 <> command "apply" (info (CmdApply <$> applyOptions) (progDesc "Apply git-restack/fix branches to originals"))
 <> command "cleanup" (info (CmdCleanup <$> cleanupOptions) (progDesc "Remove worktree and temporary branches"))
 <> command "nuke" (info (CmdNuke <$> nukeOptions) (progDesc "Remove all git-restack state"))
 <> command "status" (info (CmdStatus <$> statusOptions) (progDesc "Show execution or plan status"))
  )

runCommand :: Command -> IO ()
runCommand cmd =
  case cmd of
    CmdStack opts -> do
      stack <- analyzeStack `catch` \\(_ :: GitError) -> do
        putStrLn \"Error: Could not analyze stack. Are you in a git repository?\"\n+        exitWith (ExitFailure 1)
      if soJson opts
        then printJson stack
        else printTree stack
    CmdPlan opts -> runPlan opts
    CmdExec opts -> runExec opts
    CmdStep opts -> runStep opts
    CmdApply opts -> runApply opts
    CmdCleanup opts -> runCleanup opts
    CmdNuke opts -> runNuke opts
    CmdStatus opts -> runStatus opts

stackOptions :: Parser StackOptions
stackOptions = StackOptions
  <$> switch
        ( long "json"
       <> help "Output as JSON instead of tree format" )

planOptions :: Parser PlanOptions
planOptions = toPlanOptions
  <$> strOption
        ( long "output"
       <> short 'o'
       <> metavar "FILE"
       <> value ".git/git-restack/plan.yml"
       <> showDefault
       <> help "Write plan to file" )
  <*> optional (strOption (long "verify" <> metavar "CMD" <> help "Command to run after each commit during exec"))
  <*> optional (strOption (long "verify-only" <> metavar "CMD" <> help "Restack without changes, only run verification"))
  <*> switch (long "staged-only" <> help "Only analyze staged changes")
  <*> switch (long "unstaged-only" <> help "Only analyze unstaged changes")
  <*> option (eitherReader parsePlanMode)
        ( long "mode"
       <> metavar "worktree|direct"
       <> value PlanModeWorktree
       <> showDefault
       <> help "Conflict detection mode" )
  <*> switch (long "direct" <> help "Alias for --mode direct")
  <*> optional (strOption (long "worktree-path" <> metavar "PATH" <> help "Where to create plan worktree"))
  <*> optional (strOption (long "mergetool" <> metavar "TOOL" <> help "Merge tool to launch on conflicts"))
  <*> switch (long "force" <> short 'f' <> help "Remove existing plan worktree if present")
  where
    toPlanOptions output verify verifyOnly stagedOnly unstagedOnly mode direct worktreePath mergetool forceFlag =
      let (cmd, verifyOnlyFlag) = case verifyOnly of
            Just cmdValue -> (Just (Text.pack cmdValue), True)
            Nothing -> (Text.pack <$> verify, False)
          chosenMode = if direct then PlanModeDirect else mode
      in PlanOptions
          { poOutputFile = output
          , poVerifyCmd = cmd
          , poVerifyOnly = verifyOnlyFlag
          , poStagedOnly = stagedOnly
          , poUnstagedOnly = unstagedOnly
          , poMode = chosenMode
          , poWorktreePath = worktreePath
          , poMergetool = Text.pack <$> mergetool
          , poForce = forceFlag
          }

execOptions :: Parser ExecOptions
execOptions = ExecOptions
  <$> strArgument
        ( metavar "plan.yml"
       <> value ".git/git-restack/plan.yml"
       <> showDefault )
  <*> optional (strOption (long "worktree-path" <> metavar "PATH" <> help "Where to create worktree"))
  <*> switch (long "continue" <> help "Continue after resolving conflicts")
  <*> switch (long "abort" <> help "Abort execution and clean up")
  <*> switch (long "force" <> short 'f' <> help "Force remove existing worktree")

stepOptions :: Parser StepOptions
stepOptions = StepOptions
  <$> strArgument
        ( metavar "plan.yml"
       <> value ".git/git-restack/plan.yml"
       <> showDefault )
  <*> optional (strOption (long "worktree-path" <> metavar "PATH" <> help "Where to create worktree"))

applyOptions :: Parser ApplyOptions
applyOptions = ApplyOptions
  <$> strArgument
        ( metavar "plan.yml"
       <> value ".git/git-restack/plan.yml"
       <> showDefault )
  <*> optional (strOption (long "worktree-path" <> metavar "PATH" <> help "Path to worktree"))
  <*> switch (long "force" <> short 'f' <> help "Apply even if branches have diverged")
  <*> switch (long "cleanup" <> help "Remove worktree and git-restack/fix branches after applying")
  <*> switch (long "dry-run" <> short 'n' <> help "Show what would be done without making changes")

cleanupOptions :: Parser CleanupOptions
cleanupOptions = CleanupOptions
  <$> optional (strArgument (metavar "plan.yml"))
  <*> optional (strOption (long "worktree-path" <> metavar "PATH" <> help "Path to worktree"))
  <*> switch (long "force" <> short 'f' <> help "Skip confirmation prompt")
  <*> switch (long "keep-plan" <> help "Do not delete plan file")

nukeOptions :: Parser NukeOptions
nukeOptions = NukeOptions
  <$> switch (long "force" <> short 'f' <> help "Skip confirmation prompt")
  <*> switch (long "keep-branches" <> help "Do not delete git-restack/fix branches")

statusOptions :: Parser StatusOptions
statusOptions = StatusOptions
  <$> optional (strArgument (metavar "plan.yml"))

parsePlanMode :: String -> Either String PlanMode
parsePlanMode value
  | value == "direct" = Right PlanModeDirect
  | value == "worktree" = Right PlanModeWorktree
  | otherwise = Left "Expected 'worktree' or 'direct'"
