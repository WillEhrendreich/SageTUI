# Contributing to SageTUI

## Build

```bash
dotnet build SageTUI.slnx
```

## Test

```bash
# All tests
dotnet run --project SageTUI.Tests/SageTUI.Tests.fsproj

# Filtered by name
dotnet run --project SageTUI.Tests/SageTUI.Tests.fsproj -- --filter "Sprint 62"

# Watch mode
dotnet watch run --project SageTUI.Tests/SageTUI.Tests.fsproj
```

Tests use [Expecto](https://github.com/haf/expecto) with `dotnet run`, not `dotnet test` — this gives us full control over test runner arguments and output formatting.

## Adding Tests

When you add tests, **update the test count in README.md** (the hero line: `N,NNN tests`). CI enforces that this number exactly matches the actual test count — if it doesn't match, CI will fail with a clear error message. This is an intentional documentation discipline gate: we make an explicit public promise about test coverage, and CI holds us to it.

Run the following to get the current count:
```bash
dotnet run --project SageTUI.Tests/SageTUI.Tests.fsproj -- --list-tests 2>/dev/null | wc -l
```

## Tutorial Verify

The file `docs/tutorial-verify.fsx` type-checks all code patterns shown in the tutorial. Run it after API changes:

```bash
dotnet fsi docs/tutorial-verify.fsx
```

## PR Workflow

- Branch from `master`, PR back to `master`
- Keep changes focused — one feature or fix per PR
- Tests must be written before or alongside the implementation (TDD preferred)
- Add new `Element` cases to **both** `Render.fs` and `ArenaRender.fs` (dual render path — see architecture notes in README)

## Architecture Notes

See `ARCHITECTURE.md` and the custom instructions in `.github/copilot-instructions.md` for detailed notes on the render pipeline, compile order constraints, and coding conventions.

Key conventions:
- 2-space indentation everywhere
- Pattern matching over if/else for all control flow
- `Expecto.Flip` argument order: `actual |> Expect.equal "msg" expected`
