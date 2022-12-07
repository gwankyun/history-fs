
Write-Output ("PSScriptRoot: " + $PSScriptRoot)
Write-Output ("PWD: " + $PWD)
Write-Output ("arg: " + ($args -join " "))

$path = $PWD

if ($args.Length -lt 1) {
    Write-Output "need more args"
    exit 1
}

[string]$c = $args[0]

[string[]]$a = @()

$a += $c

if ($c -eq "add" -or $c -eq "diff" -or $c -eq "list" -or $c -eq "compare") {
    $a += $path
}

$a += $args[1..$args.Length]

if ($c -eq "merge" -and $a.Length -lt 3) {
    $a += $path
}

Write-Output ("a: " + ($a -join " "))

Set-Location $PSScriptRoot
dotnet run $a
Set-Location $path
