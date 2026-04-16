# Diagramas de la memoria tecnica

Esta carpeta contiene los diagramas fuente en Mermaid para la documentacion profesional en LaTeX.

## Archivos incluidos

- 01_arquitectura_objetivo.mmd
- 02_flujo_autenticacion.mmd
- 03_modelo_datos.mmd
- 04_roadmap_fases.mmd
- 05_gobernanza_rbac.mmd
- 06_despliegue_continuidad.mmd

Cada archivo Mermaid incluye un titulo embebido en el propio diagrama mediante frontmatter (`title:`), de modo que la imagen exportada ya muestra su encabezado.

## Exportacion en alta calidad

1. Abre terminal en esta carpeta.
2. Ejecuta:

   powershell -ExecutionPolicy Bypass -File .\export-diagrams.ps1

3. Los resultados SVG y PDF quedan en:

   docs/tex/figures/generated

4. Al insertar en LaTeX, conserva tambien el `caption` para mantener consistencia en indice de figuras.

## Insercion recomendada en LaTeX

Usar PDF para compilaciones clasicas:

\begin{figure}[htbp]
  \centering
  \includegraphics[width=\textwidth]{figures/generated/01_arquitectura_objetivo.pdf}
  \caption{Arquitectura objetivo de la solucion}
\end{figure}

## Nota

Si no tienes Node.js instalado, instala Node LTS y vuelve a ejecutar el script. El comando usa npx para evitar instalaciones globales.

## Checklist final

1. Exportar diagramas con `export-diagrams.ps1`.
2. Verificar salida en `docs/tex/figures/generated`.
3. Compilar `main.tex` dos veces para actualizar indice y lista de figuras.
