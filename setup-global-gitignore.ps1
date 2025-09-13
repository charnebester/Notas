#requires -Version 5.1
<#
.SYNOPSIS
  Create/update a global .gitignore and configure Git to use it (Windows).
#>

$ErrorActionPreference = 'Stop'

function Assert-True {
    param(
        [Parameter(Mandatory = $true)][object]$Condition,
        [Parameter(Mandatory = $true)][string]$Message
    )
    # Treat non-empty arrays/strings/objects as True; cast everything to [bool]
    if (-not [bool]$Condition) { throw $Message }
}

function Get-GlobalIgnorePath {
    $profilePath = $env:USERPROFILE
    Assert-True ($null -ne $profilePath -and $profilePath.Trim().Length -gt 0) "USERPROFILE is not set."
    $path = Join-Path -Path $profilePath -ChildPath ".gitignore_global"
    Assert-True ($path -is [string] -and $path.ToLower().EndsWith(".gitignore_global")) "Computed path is invalid: $path"
    return $path
}

function Ensure-GitInstalled {
    $cmds = @(Get-Command git -ErrorAction Stop)
    Assert-True ($cmds.Count -ge 1) "Git not found in PATH. Install Git for Windows first."
    $ver = (& git --version 2>$null | Select-Object -First 1)
    Assert-True ($LASTEXITCODE -eq 0 -and $ver -match 'git version') "Git exists but 'git --version' failed."
    return $true
}

function Write-GlobalGitignore {
    param([Parameter(Mandatory = $true)][string]$Path)

    Assert-True ($Path.Trim().Length -gt 0) "Target path is empty."
    $parent = Split-Path -Parent $Path
    if (-not (Test-Path -LiteralPath $parent)) {
        $null = New-Item -ItemType Directory -Path $parent -Force
    }
    Assert-True (Test-Path -LiteralPath $parent) "Parent directory does not exist: $parent"

    $content = @'
# --- OS cruft ---
.DS_Store
Thumbs.db

# --- Editors/IDEs ---
.vscode/
.idea/
*.swp

# --- Python/Django ---
__pycache__/
*.py[cod]
*.pyo
*.pyc
*.pyd
.venv/
venv/
ENV/
.pytest_cache/
.coverage
coverage.*
db.sqlite3
*.log
local_settings.py

# --- Node/JS (keep your lockfiles tracked) ---
node_modules/

# --- Flutter/Dart ---
.dart_tool/
.buildlog/
.packages
.flutter-plugins
.flutter-plugins-dependencies
build/

# --- Misc ---
*.tmp
*.bak

# --- Delphi ---
*.dproj.local
__recovery/
__history/
*.identcache
*.rsm
*.dcu
'@

    Assert-True ($content.Length -gt 100) "Unexpected: content length too small."
    Set-Content -LiteralPath $Path -Value $content -Encoding UTF8
    Assert-True (Test-Path -LiteralPath $Path) "Failed to write $Path"
    $size = (Get-Item -LiteralPath $Path).Length
    Assert-True ($size -gt 50) "File too small after write: $size bytes."
    return $true
}

function Configure-GitGlobalIgnore {
    param([Parameter(Mandatory = $true)][string]$Path)

    Assert-True (Test-Path -LiteralPath $Path) "Global ignore file does not exist: $Path"
    & git config --global core.excludesFile "$Path"
    Assert-True ($LASTEXITCODE -eq 0) "Failed to set Git core.excludesFile."

    # Read back, normalize to a single trimmed line
    $value = (git config --global --get core.excludesFile | Select-Object -First 1)
    $value = ($value -as [string]).Trim()

    Assert-True ($value.Length -gt 0) "Failed to read back core.excludesFile."
    Assert-True ($value.ToLower().EndsWith(".gitignore_global")) "Git reported an unexpected excludesFile: $value"
    Write-Host "Git core.excludesFile => $value"
    return $true
}

# ----------------- Main -----------------
try {
    $path = Get-GlobalIgnorePath
    $ok1  = Ensure-GitInstalled
    Assert-True $ok1 "Git check failed."

    $ok2  = Write-GlobalGitignore -Path $path
    Assert-True $ok2 "Writing global .gitignore failed."

    $ok3  = Configure-GitGlobalIgnore -Path $path
    Assert-True $ok3 "Configuring Git core.excludesFile failed."

    Write-Host "Success! Global .gitignore is set at: $path" -ForegroundColor Green
    exit 0
}
catch {
    Write-Error $_.Exception.Message
    exit 1
}
