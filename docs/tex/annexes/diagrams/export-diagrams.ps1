param(
  [string]$InputDir = ".",
  [string]$OutputDir = "..\..\figures\generated"
)

$ErrorActionPreference = "Stop"

if (-not (Test-Path $OutputDir)) {
  New-Item -ItemType Directory -Path $OutputDir | Out-Null
}

$files = Get-ChildItem -Path $InputDir -Filter "*.mmd" -File
if ($files.Count -eq 0) {
  Write-Host "No se encontraron archivos .mmd en $InputDir"
  exit 0
}

$hasErrors = $false

foreach ($file in $files) {
  $name = [System.IO.Path]::GetFileNameWithoutExtension($file.Name)
  $svgOut = Join-Path $OutputDir ($name + ".svg")
  $pdfOut = Join-Path $OutputDir ($name + ".pdf")

  try {
    Write-Host "Exportando $($file.Name) -> SVG"
    npx @mermaid-js/mermaid-cli -i $file.FullName -o $svgOut -b transparent -s 2

    Write-Host "Exportando $($file.Name) -> PDF"
    npx @mermaid-js/mermaid-cli -i $file.FullName -o $pdfOut -b white -s 2
  }
  catch {
    $hasErrors = $true
    Write-Host "ERROR exportando $($file.Name): $($_.Exception.Message)" -ForegroundColor Red
  }
}

Write-Host "Exportacion completada. Salida en: $OutputDir"

if ($hasErrors) {
  exit 1
}
