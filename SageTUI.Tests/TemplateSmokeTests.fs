module SageTUI.TemplateSmokeTests

open System
open System.IO
open Expecto
open Expecto.Flip
open SageTUI.ProcessHelpers

let private writeNuGetConfig (workspace: TestWorkspace) =
  let configPath = Path.Combine(workspace.RootDir, "NuGet.Config")

  let xml =
    $"""<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <clear />
    <add key="local-feed" value="{workspace.LocalFeed}" />
    <add key="nuget.org" value="https://api.nuget.org/v3/index.json" />
  </packageSources>
</configuration>
"""

  File.WriteAllText(configPath, xml)
  configPath

let private packLocalPackages (workspace: TestWorkspace) =
  let env = cliEnvironment workspace
  let libraryProject = Path.Combine(repoRoot, "SageTUI.Library.fsproj")
  let templatesProject = Path.Combine(repoRoot, "SageTUI.Templates.fsproj")

  dotnet env repoRoot [ "pack"; libraryProject; "-c"; "Release"; "-o"; workspace.LocalFeed ]
  |> requireSuccess "Packing SageTUI library package"

  dotnet env repoRoot [ "pack"; templatesProject; "-c"; "Release"; "-o"; workspace.LocalFeed ]
  |> requireSuccess "Packing SageTUI template package"

let private installTemplate (workspace: TestWorkspace) =
  let env = cliEnvironment workspace
  let templatePackage = findSinglePackage workspace.LocalFeed "SageTUI.Templates"

  dotnet env workspace.RootDir [ "new"; "install"; templatePackage ]
  |> requireSuccess "Installing packed SageTUI templates"

let private generateTemplateApp (workspace: TestWorkspace) =
  let env = cliEnvironment workspace

  dotnet env workspace.RootDir [ "new"; "sagetui"; "-n"; "SmokeApp" ]
  |> requireSuccess "Generating app from SageTUI template"

  Path.Combine(workspace.RootDir, "SmokeApp")

let private restoreAndBuildGeneratedApp (workspace: TestWorkspace) (appDir: string) =
  let env = cliEnvironment workspace
  let nugetConfig = writeNuGetConfig workspace
  let projectPath = Path.Combine(appDir, "SmokeApp.fsproj")

  dotnet env appDir [ "restore"; projectPath; "--configfile"; nugetConfig ]
  |> requireSuccess "Restoring generated template app"

  dotnet env appDir [ "build"; projectPath; "--no-restore" ]
  |> requireSuccess "Building generated template app"

[<Tests>]
let templateSmokeTests =
  testList "Templates.Smoke" [
    testCase "packed template generates a buildable app in an isolated workspace" <| fun () ->
      use workspace = createWorkspace "template-smoke"

      packLocalPackages workspace
      installTemplate workspace

      let appDir = generateTemplateApp workspace
      let projectPath = Path.Combine(appDir, "SmokeApp.fsproj")
      let programPath = Path.Combine(appDir, "Program.fs")

      Directory.Exists(appDir)
      |> Expect.isTrue "template should create the requested project directory"

      File.Exists(projectPath)
      |> Expect.isTrue "template should generate the renamed project file"

      File.Exists(programPath)
      |> Expect.isTrue "template should generate Program.fs"

      File.ReadAllText(programPath)
      |> Expect.stringContains "template should include the starter counter UI" "SageTUI Counter"

      File.ReadAllText(projectPath)
      |> Expect.stringContains "template should rename the project to the requested app name" "<PackageReference Include=\"SageTUI\" Version=\"*\" />"

      restoreAndBuildGeneratedApp workspace appDir
  ]
