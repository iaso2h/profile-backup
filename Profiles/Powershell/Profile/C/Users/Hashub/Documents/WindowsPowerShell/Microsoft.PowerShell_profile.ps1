Invoke-Expression (&starship init powershell)



# Get-ChildItemColor
# =================
If (-Not (Test-Path Variable:PSise)) {  # Only run this in the console and not in the ISE
    Import-Module Get-ChildItemColor
    
    Set-Alias l Get-ChildItemColor -option AllScope
    Set-Alias ls Get-ChildItemColorFormatWide -option AllScope
}
# =================

# PSReadLine
# =================
Import-Module PSReadLine
Set-PSReadLineOption -HistorySearchCursorMovesToEnd


# Basic editing functions
Set-PSReadLineKeyHandler -Key Tab -Function MenuComplete
Set-PSReadLineKeyHandler -Key Ctrl+z -Function Undo
Set-PSReadLineKeyHandler -Key Ctrl+r -Function Redo
Set-PSReadLineKeyHandler -Key Ctrl+d -Function DeleteChar
Set-PSReadLineKeyHandler -Key Ctrl+u -Function BackwardDeleteInput
# Cursor movement functions
Set-PSReadLineKeyHandler -Key Ctrl+h -Function BackwardChar
Set-PSReadLineKeyHandler -Key Ctrl+l -Function ForwardChar
Set-PSReadLineKeyHandler -Key Ctrl+b -Function BackwardWord
Set-PSReadLineKeyHandler -Key Ctrl+w -Function NextWord
Set-PSReadLineKeyHandler -Key Ctrl+e -Function EndOfLine
Set-PSReadLineKeyHandler -Key Ctrl+a -Function BeginningOfLine
# History functions
# Set-PSReadLineKeyHandler -Key Ctrl+f -Function ForwardWord
Set-PSReadLineKeyHandler -Key 'Ctrl+f' `
  -BriefDescription "Go to EndOfLine or AcceptSuggestion" `
  -LongDescription `
  "If cursor is not at the end of line, then go to end of line, `
    otherwise try to AcceptSuggestion" `
  -ScriptBlock {
  $line = $null
  $cursor = $null
  [Microsoft.PowerShell.PSConsoleReadLine]::GetBufferState([ref]$line, [ref]$cursor)
  if ($cursor -ne $line.Length) {
    [Microsoft.PowerShell.PSConsoleReadLine]::EndOfLine()
  }
  else {
    [Microsoft.PowerShell.PSConsoleReadLine]::AcceptSuggestion()
  }
}
Set-PSReadLineKeyHandler -Key Ctrl+F -Function ForwardChar
Set-PSReadLineKeyHandler -Key Ctrl+p -Function HistorySearchBackward
#Set-PSReadLineKeyHandler -Key Ctrl+j -Function HistorySearchForward
Set-PSReadLineKeyHandler -Key Ctrl+n -Function NextHistory
#Set-PSReadLineKeyHandler -Key Ctrl+K -Function PreviousHistory
# Miscellaneous functions
Set-PSReadLineKeyHandler -Key Alt+d -Function ScrollDisplayDown
# Set-PSReadLineKeyHandler -Key Ctrl+Alt+? -Function ShowKeyBindings
# Set-PSReadLineKeyHandler -Key Alt+h -Function ShowParameterHelp
# Set-PSReadLineKeyHandler -Key Alt+?  -Function WhatIsKey
# =================

# Import the Chocolatey Profile that contains the necessary code to enable
# tab-completions to function for `choco`.
# Be aware that if you are missing these lines from your profile, tab completion
# for `choco` will not function.
# See https://ch0.co/tab-completion for details.
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}



# Check if fzf is available in the environment
if (Get-Command fzf -ErrorAction SilentlyContinue) {
    # Define the fzf-explorer function only if fzf is found
    function fzf-explorer {
        # Run fzf to select multiple files
        $selected = bash -c "fzf -m"

        if ($selected) {
            # Split the selected files into an array
            $files = $selected -split "`n"

            foreach ($file in $files) {
                # Convert Unix path to Windows path (if using WSL or Git Bash)
                if ($file -match '^/mnt/') {
                    $driveLetter = $file.Substring(5, 1).ToUpper()
                    $windowsPath = $file.Substring(6) -replace '/', '\'
                    $absolutePath = "$driveLetter`:\$windowsPath"
                } else {
                    # If the path is already in Windows format, use it as-is
                    $absolutePath = $file
                }

                # Ensure the path is absolute and properly formatted
                $absolutePath = (Resolve-Path $absolutePath).Path

                # Open File Explorer and highlight the selected file
                Start-Process explorer.exe -ArgumentList "/select, `"$absolutePath`""
            }
        } else {
            Write-Output "No file selected."
        }
    }

    function fzf-cd {
        # Run fzf to select a file
        $selected = bash -c "fzf"

        if ($selected) {
            # Convert Unix path to Windows path (if using WSL or Git Bash)
            if ($selected -match '^/mnt/') {
                $driveLetter = $selected.Substring(5, 1).ToUpper()
                $windowsPath = $selected.Substring(6) -replace '/', '\'
                $absolutePath = "$driveLetter`:\$windowsPath"
            } else {
                # If the path is already in Windows format, use it as-is
                $absolutePath = $selected
            }

            # Ensure the path is absolute and properly formatted
            $absolutePath = (Resolve-Path $absolutePath).Path

            # Extract the directory path from the selected file
            $directoryPath = Split-Path -Parent $absolutePath

            # Change the working directory to the selected file's directory
            Set-Location -Path $directoryPath

            Write-Output "Changed directory to: $directoryPath"
        } else {
            Write-Output "No file selected."
        }
    }

    # Optional: Create an alias for quick access
    New-Alias fe fzf-explorer -Force
    New-Alias fcd fzf-cd -Force
}

function yt {
    param(
        [Parameter(Mandatory=$false, ValueFromRemainingArguments=$true)]
        [string[]]$Arguments
    )

    # Default options (can be overridden by user-provided arguments)
    $defaultOptions = @(
        "-f", "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]",
        "--force-overwrites",
        "--yes-playlist",
        "--embed-metadata",
        "--cookies-from-browser", "vivaldi",
        "--write-subs",
        "--write-auto-subs",
        "--sub-format", "srt/best/ass"
        "--sub-langs", "en.*"  # Broader subtitle search
    )

    # Call yt-dlp with defaults first, then user arguments (user args will override)
    yt-dlp @defaultOptions @Arguments
}

Invoke-Expression (& { (zoxide init powershell | Out-String) })
