$ErrorActionPreference = "Stop"

$root = Split-Path -Parent $MyInvocation.MyCommand.Path
$diagDir = Join-Path $root "annexes\diagrams"
$outDir = Join-Path $root "figures\generated"

Write-Host "[1/3] Exportando diagramas..."
powershell -ExecutionPolicy Bypass -File (Join-Path $diagDir "export-diagrams.ps1") -InputDir $diagDir -OutputDir $outDir

Write-Host "[2/3] Compilando LaTeX (primera pasada)..."
Push-Location $root
pdflatex -interaction=nonstopmode -halt-on-error main.tex

Write-Host "[3/3] Compilando LaTeX (segunda pasada para indices)..."
pdflatex -interaction=nonstopmode -halt-on-error main.tex
Pop-Location

Write-Host "Build completo. Revisa main.pdf en: $root"
