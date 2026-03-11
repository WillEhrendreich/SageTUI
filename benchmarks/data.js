window.BENCHMARK_DATA = {
  "lastUpdate": 1773213329874,
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
      }
    ]
  }
}