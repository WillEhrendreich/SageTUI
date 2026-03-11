window.BENCHMARK_DATA = {
  "lastUpdate": 1773253285444,
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
      }
    ]
  }
}