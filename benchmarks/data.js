window.BENCHMARK_DATA = {
  "lastUpdate": 1773267283877,
  "repoUrl": "https://github.com/WillEhrendreich/SageTUI",
  "entries": {
    "Benchmark": [
      {
        "commit": {
          "author": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "committer": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "distinct": true,
          "id": "9762cdfd18748534c99f25aa9139e94a09808ae6",
          "message": "fix(benchmarks): add namespace SageTUI.Benchmarks to fix CI output file path\n\nBenchmarkDotNet names output files using the fully-qualified type name.\nWithout a namespace, types were placed in the implicit Program module,\nproducing Program.BufferDiffBenchmarks-report-full-compressed.json.\nCI was expecting SageTUI.Benchmarks.BufferDiffBenchmarks-*.json.\n\nAdd 'namespace SageTUI.Benchmarks' and wrap the entry point in 'module Program'\nso BDN generates the expected filenames.\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>",
          "timestamp": "2026-03-11T02:07:49-05:00",
          "tree_id": "adb1af1ef7d2055f5fdab6a415cc6a8a63624b86",
          "url": "https://github.com/WillEhrendreich/SageTUI/commit/9762cdfd18748534c99f25aa9139e94a09808ae6"
        },
        "date": 1773213328913,
        "tool": "benchmarkdotnet",
        "benches": [
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffIdentical",
            "value": 645.8498622576395,
            "unit": "ns",
            "range": "± 0.738172652528826"
          },
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffChanged",
            "value": 2978.1714902242024,
            "unit": "ns",
            "range": "± 27.501759238720417"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "committer": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "distinct": true,
          "id": "6302f1964bfa5ef5f20fa4fa3e89b724fc734ee6",
          "message": "chore: bump version to 0.9.2\n\nCI is now fully green on Ubuntu, Windows, and macOS.\nReady for NuGet publish.\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>",
          "timestamp": "2026-03-11T02:17:22-05:00",
          "tree_id": "87e05bd993c3b6b0047a13f0631106303d6499a1",
          "url": "https://github.com/WillEhrendreich/SageTUI/commit/6302f1964bfa5ef5f20fa4fa3e89b724fc734ee6"
        },
        "date": 1773213924272,
        "tool": "benchmarkdotnet",
        "benches": [
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffIdentical",
            "value": 646.1397792376005,
            "unit": "ns",
            "range": "± 1.167024159088767"
          },
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffChanged",
            "value": 2863.733353478568,
            "unit": "ns",
            "range": "± 17.914278994630237"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "committer": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "distinct": true,
          "id": "cf596b8349b52bb0e2cbd275e48801fe1632a64a",
          "message": "fix(ci): remove dotnet-quality preview from screenshots workflow\n\nUsing dotnet-quality: preview caused SDK version mismatch (10.0.102 preview\npicked up .NET 11 pre-installed packages, corrupting project.assets.json).\nStable 10.0.x is sufficient for building samples.\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>",
          "timestamp": "2026-03-11T09:57:22-05:00",
          "tree_id": "ec5d26d1aad2071bee7eaa567179f7f12ac52691",
          "url": "https://github.com/WillEhrendreich/SageTUI/commit/cf596b8349b52bb0e2cbd275e48801fe1632a64a"
        },
        "date": 1773241532282,
        "tool": "benchmarkdotnet",
        "benches": [
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffIdentical",
            "value": 645.9711583682468,
            "unit": "ns",
            "range": "± 2.391796953208193"
          },
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffChanged",
            "value": 2793.5475965646597,
            "unit": "ns",
            "range": "± 16.656338872949785"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "committer": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "distinct": true,
          "id": "d69975578c5807e5d4e1d73a07e24546054a7b05",
          "message": "docs: fix stale Known Limitations — mouse support is fully implemented\n\nConsole.In.Read() + AnsiParser SGR mouse parser + ?1000h/?1002h/?1006h\nare all wired. Remove false \"Console.ReadKey\" limitation note.\nUpdate MouseSub/DragSub doc comments to reflect ?1002h being active.\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>",
          "timestamp": "2026-03-11T10:45:35-05:00",
          "tree_id": "5d7596800bb9dc3f8c0034e52fe73c04b88c17fc",
          "url": "https://github.com/WillEhrendreich/SageTUI/commit/d69975578c5807e5d4e1d73a07e24546054a7b05"
        },
        "date": 1773244417843,
        "tool": "benchmarkdotnet",
        "benches": [
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffIdentical",
            "value": 644.9167126019796,
            "unit": "ns",
            "range": "± 1.1282984513206087"
          },
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffChanged",
            "value": 2975.819139099121,
            "unit": "ns",
            "range": "± 15.754319865310798"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "committer": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "distinct": true,
          "id": "60814e7bbfb0d48bc66ee81d48de21e22641f896",
          "message": "docs: update test count to 2,880 (Sprint 74)\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>",
          "timestamp": "2026-03-11T12:44:55-05:00",
          "tree_id": "7240f3d8a62803a910c4c58b0042c98ad72e8696",
          "url": "https://github.com/WillEhrendreich/SageTUI/commit/60814e7bbfb0d48bc66ee81d48de21e22641f896"
        },
        "date": 1773251600369,
        "tool": "benchmarkdotnet",
        "benches": [
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffIdentical",
            "value": 646.9726312955221,
            "unit": "ns",
            "range": "± 0.34554711514677555"
          },
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffChanged",
            "value": 2807.4107622419083,
            "unit": "ns",
            "range": "± 19.523389804795706"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "committer": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "distinct": true,
          "id": "d47268f062f0a2c9f882bf02c4d2354b4ed75c1d",
          "message": "fix: update README test count to 2917\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>",
          "timestamp": "2026-03-11T13:13:08-05:00",
          "tree_id": "f2683fe8245fa72f03aae175ff035ab882910af9",
          "url": "https://github.com/WillEhrendreich/SageTUI/commit/d47268f062f0a2c9f882bf02c4d2354b4ed75c1d"
        },
        "date": 1773253285129,
        "tool": "benchmarkdotnet",
        "benches": [
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffIdentical",
            "value": 645.1295826775687,
            "unit": "ns",
            "range": "± 1.2735425924970274"
          },
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffChanged",
            "value": 2774.086485726493,
            "unit": "ns",
            "range": "± 4.069756296626138"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "committer": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "distinct": true,
          "id": "c184dfdc019045fde4182e52bafc2da069444d39",
          "message": "feat: Sprint 76 — Program.withDebugger, OrderableList, Diff module\n\n- Add DebuggerConfig<'model> + DebuggerMsg<'msg> + Program.withDebugger\n  F12-toggleable live model inspector overlay; composes with withLogging\n- Add OrderableList<'a> struct widget with moveUp/Down/insertBefore/After/\n  removeAt/swapIndices/map/filter/length/isEmpty (pure, zero-overhead)\n- Add DiffChange<'a> DU + Diff module with LCS-based compute, countAdded/\n  countRemoved/countUnchanged helpers\n- Fix Cmd.computeWhen tests to use Guid keys (eliminate cross-test cache\n  contamination in parallel runs)\n- 36 new tests, all GREEN; full suite 2,947 passing\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>",
          "timestamp": "2026-03-11T14:18:28-05:00",
          "tree_id": "8d9452e63e3c7a6527c49f9376564f36c9233e3b",
          "url": "https://github.com/WillEhrendreich/SageTUI/commit/c184dfdc019045fde4182e52bafc2da069444d39"
        },
        "date": 1773257233821,
        "tool": "benchmarkdotnet",
        "benches": [
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffIdentical",
            "value": 646.6300012148344,
            "unit": "ns",
            "range": "± 0.6778799698839751"
          },
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffChanged",
            "value": 2871.7273582458497,
            "unit": "ns",
            "range": "± 17.528411776455012"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "committer": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "distinct": true,
          "id": "40eab9ed274b090dd94a9a79069a58ef8c53f5d9",
          "message": "docs: update CHANGELOG, README, QUICK_REFERENCE for v0.9.3\n\n- CHANGELOG.md: add [0.9.3] section covering Sprints 74-76 and expert\n  panel fixes (withDebugger, OrderableList, Diff, withLogging, withPersistence,\n  withHistory, withErrorBanner, computeWhen, NavigationStack, hex colors,\n  TableSelection, assertSequence/Snapshot, signal handler GC fix, log routing)\n- README.md: add Program combinators section with composition table,\n  add 'Program combinators' and 'Data utilities' feature rows, add OrderableList\n  to widgets list, bump package version 0.9.0 -> 0.9.3\n- QUICK_REFERENCE.md: add ErrorPolicy DU, DiffChange DU, DebuggerMsg DU,\n  and Program combinator ordering guide\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>",
          "timestamp": "2026-03-11T15:51:27-05:00",
          "tree_id": "d91be4f7790d3a68f0b2fe72dbddcb7957548f24",
          "url": "https://github.com/WillEhrendreich/SageTUI/commit/40eab9ed274b090dd94a9a79069a58ef8c53f5d9"
        },
        "date": 1773262794734,
        "tool": "benchmarkdotnet",
        "benches": [
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffIdentical",
            "value": 646.3744223668025,
            "unit": "ns",
            "range": "± 2.3877585466985916"
          },
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffChanged",
            "value": 2891.2452829996746,
            "unit": "ns",
            "range": "± 18.75810742690998"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "committer": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "distinct": true,
          "id": "b88a282d9a409de36e75dd4f2137c1755d1ce57e",
          "message": "docs: update README test count to 2983 for Sprint 77\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>",
          "timestamp": "2026-03-11T16:51:19-05:00",
          "tree_id": "98bdbd3200a6d3f53d00af7179ddaa561345cdf2",
          "url": "https://github.com/WillEhrendreich/SageTUI/commit/b88a282d9a409de36e75dd4f2137c1755d1ce57e"
        },
        "date": 1773266394161,
        "tool": "benchmarkdotnet",
        "benches": [
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffIdentical",
            "value": 645.3423853654128,
            "unit": "ns",
            "range": "± 1.0346390397375556"
          },
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffChanged",
            "value": 2779.2462639441856,
            "unit": "ns",
            "range": "± 4.282702519781505"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "committer": {
            "email": "will.ehrendreich@gmail.com",
            "name": "Will Ehrendreich",
            "username": "WillEhrendreich"
          },
          "distinct": true,
          "id": "365852bf76976cd3488378c2b85941efefc564c6",
          "message": "docs: update README + CHANGELOG for v0.9.4 (Sprint 77)\n\n- README: add El.hyperlink/OSC 8 to Elements table\n- README: add OrderableVirtualList + DiffView to Widgets table\n- README: add DiffView.view to Data utilities row\n- README: add new Command algebra row (Cmd.bind/andThen/sequence)\n- CHANGELOG: add [0.9.4] section with all Sprint 77 additions\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>",
          "timestamp": "2026-03-11T17:06:13-05:00",
          "tree_id": "13d0b04f2397771e1f683df07f3be9615f762f6e",
          "url": "https://github.com/WillEhrendreich/SageTUI/commit/365852bf76976cd3488378c2b85941efefc564c6"
        },
        "date": 1773267282912,
        "tool": "benchmarkdotnet",
        "benches": [
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffIdentical",
            "value": 645.1952251287607,
            "unit": "ns",
            "range": "± 0.8343202485604947"
          },
          {
            "name": "SageTUI.Benchmarks.BufferDiffBenchmarks.DiffChanged",
            "value": 2964.310893758138,
            "unit": "ns",
            "range": "± 24.879852428806032"
          }
        ]
      }
    ]
  }
}