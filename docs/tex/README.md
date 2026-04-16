# Documento Maestro Institucional (LaTeX)

## Estructura

- main.tex: documento maestro
- templates/: portada, preambulo y frontmatter
- chapters/: capitulos principales
- annexes/: anexos
- annexes/diagrams/: fuentes Mermaid
- figures/generated/: diagramas exportados

## Build completo

Ejecutar desde PowerShell:

powershell -ExecutionPolicy Bypass -File "c:/ciencias-ucv-digital-archive/docs/tex/build.ps1"

## Build manual

1. Exportar diagramas:

powershell -ExecutionPolicy Bypass -File "c:/ciencias-ucv-digital-archive/docs/tex/annexes/diagrams/export-diagrams.ps1" -InputDir "c:/ciencias-ucv-digital-archive/docs/tex/annexes/diagrams" -OutputDir "c:/ciencias-ucv-digital-archive/docs/tex/figures/generated"

2. Compilar LaTeX dos veces:

cd c:/ciencias-ucv-digital-archive/docs/tex
pdflatex -interaction=nonstopmode -halt-on-error main.tex
pdflatex -interaction=nonstopmode -halt-on-error main.tex

## Tipografia

- Para usar Arial real, compila con XeLaTeX:

xelatex -interaction=nonstopmode -halt-on-error main.tex
xelatex -interaction=nonstopmode -halt-on-error main.tex

- Si usas pdfLaTeX, se aplica una fuente sans-serif compatible como fallback.
