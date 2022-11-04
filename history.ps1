
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

if ($c -eq "add" -or $c -eq "diff" -or $c -eq "list") {
    $a += $PWD
}

$a += $args[1..$args.Length]

Write-Output ("a: " + ($a -join " "))

Set-Location $PSScriptRoot
dotnet run $a
Set-Location $path
