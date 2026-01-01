<p align="center">
  <img src="assets/restack.png" width="320" alt="git-restack" />
</p>

<h1 align="center">Don't play russian roulette with your stacked branches</h1>

# git-restack

[![CI](https://github.com/manmal/git-restack/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/manmal/git-restack/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Zig](https://img.shields.io/badge/Zig-0.15.2-f7a41d?logo=zig&logoColor=white)](https://ziglang.org/)

Stack-aware restacking for Git feature branch hierarchies. git-restack creates a deterministic plan, captures conflict resolutions, and replays fixes on top of each branch without rerere.

git-restack is the guardrail for agents rebasing stacked branch hierarchies: it discovers and resolves conflicts early, validates fixes top-to-bottom, and only executes a plan once it’s bulletproof.

## Why

- Detect conflicts early while planning.
- Resolve conflicts once and replay the resolution across exec runs.
- Restack large branch stacks without guessing which fixes belong where.

## Common use cases

- Allow an agent to write your rebase plan (including conflict resolution) before running it.
- Rebase while keeping a stacked branch hierarchy intact.
- Make every commit in a stacked branch hierarchy lint and compile.
- Push new changes down to the commit(s) where those files were last modified ("put new changes where they belong").
- Escape rerere hell.

## Git flow diagrams

Stacked feature branches (what git-restack works with):

```
main
 └─ feature/TEST-1-base
    └─ feature/TEST-2-api
       └─ feature/TEST-3-ui
          └─ feature/TEST-4-top
```

Insert changes while preserving the hierarchy:

```
Before:
main
 └─ feature/TEST-1-base
    └─ feature/TEST-2-api
       └─ feature/TEST-3-ui
          └─ feature/TEST-4-top

After (fix branches keep the stack intact):
main
 └─ feature/TEST-1-base-fix
    └─ feature/TEST-2-api-fix
       └─ feature/TEST-3-ui-fix
          └─ feature/TEST-4-top-fix
```

## Quick start

```sh
zig build
./zig-out/bin/git-restack plan
./zig-out/bin/git-restack exec --force
./zig-out/bin/git-restack apply
```

## Workflow

1. Make changes on the current stack head.
2. `git-restack plan` creates a plan file and captures conflicts.
3. Inspect `.git/git-restack/plan.yml`.
4. `git-restack exec --force` creates `-fix` branches with your changes.
5. `git-restack apply` updates original branches to the `-fix` tips.

## Conflict resolution

- Conflicts are detected during `plan` by replaying the stack on top of the base branch.
- `--mergetool <tool>` chooses a specific tool; git config and `GIT_MERGETOOL` are respected.
- Resolutions are stored in `plan.yml` so `exec` is non-interactive.

## Real-world test suite

Run the full suite of real-world scenarios locally or in CI:

```sh
./test/run_real_world_tests.sh
```

Notes:
- Tests create temp repos in the parent directory of this repo.
- Submodule scenarios require `protocol.file.allow=always` (set automatically in the script).

## CI

CI runs on Linux only and executes:
- `zig build`
- `./test/run_integration_tests.sh`
- `./test/run_real_world_tests.sh`

## License

MIT. See `LICENSE`.
