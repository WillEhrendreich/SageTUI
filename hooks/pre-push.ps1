#!/usr/bin/env pwsh

$projectFiles = Get-ChildItem -Path . -Recurse -Include *.fsproj,*.csproj,*.vbproj | Select-Object -First 1

if (-not $projectFiles) {
    Write-Host "No project file found, skipping version bump"
    exit 0
}

$projFile = $projectFiles.FullName

# Get current version from project file
$content = Get-Content $projFile -Raw
if ($content -match '<Version>([^<]+)</Version>') {
    $currentVersion = $matches[1]
} else {
    Write-Host "No version found in project file"
    exit 0
}

# Check if HEAD commit already has a tag with this version
$headTags = git tag --points-at HEAD
$expectedTag = "v$currentVersion"

if ($headTags -contains $expectedTag) {
    Write-Host "Current commit already tagged with $expectedTag, skipping version bump"
    exit 0
}

# Check if there are staged changes (indicates we already ran)
$stagedChanges = git diff --cached --name-only
if ($stagedChanges -match '\.fsproj$|\.csproj$|\.vbproj$') {
    Write-Host "Project files already staged, skipping to prevent double-bump"
    exit 0
}

# Parse and increment patch version
$versionParts = $currentVersion -split '\.'
$major = $versionParts[0]
$minor = $versionParts[1]
$patch = [int]$versionParts[2] + 1
$newVersion = "$major.$minor.$patch"

# Check if tag already exists
$existingTag = git tag -l "v$newVersion"
if ($existingTag) {
    Write-Host "Tag v$newVersion already exists, skipping version bump"
    exit 0
}

Write-Host "Bumping version from $currentVersion to $newVersion"

# Update project file
$content = $content -replace "<Version>$currentVersion</Version>", "<Version>$newVersion</Version>"
$content = $content -replace "<AssemblyVersion>$currentVersion</AssemblyVersion>", "<AssemblyVersion>$newVersion</AssemblyVersion>"
$content = $content -replace "<FileVersion>$currentVersion</FileVersion>", "<FileVersion>$newVersion</FileVersion>"
Set-Content -Path $projFile -Value $content -NoNewline

# Stage the updated project file
git add $projFile

# Amend the current commit with the version bump
git commit --amend --no-edit --no-verify

# Create tag
git tag "v$newVersion"

Write-Host "Version bumped to $newVersion and tagged"
exit 0