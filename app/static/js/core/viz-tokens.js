// =============================================================================
// TOKENS DE VISUALIZACIÓN
// Puente entre los tokens --viz-* de styles.css y Chart.js. Vive aparte de
// admin-charts.js porque el panel de Sistema dibuja sus propios gráficos y
// necesita la misma paleta sin arrastrar el resto del dashboard.
// =============================================================================

function _viz(name, fallback) {
  const v = getComputedStyle(document.body).getPropertyValue(`--viz-${name}`).trim();
  return v || fallback;
}

// Paleta categórica en orden fijo. El orden es el mecanismo de seguridad para
// daltonismo (los pares adyacentes están validados), por eso nunca se reordena
// ni se cicla: una novena serie va a "Otros", no a un color inventado.
function vizSeries() {
  return [1, 2, 3, 4, 5, 6, 7, 8].map(i => _viz(i, "#2a78d6"));
}

// Aplica la tinta/rejilla del tema activo a ejes, leyendas y tooltips.
function applyChartTheme() {
  if (typeof Chart === "undefined") return;
  Chart.defaults.color       = _viz("ink-muted", "#6c757d");
  Chart.defaults.font.family = getComputedStyle(document.body).fontFamily;
  Chart.defaults.borderColor = _viz("grid", "#e6e6e2");
  Chart.defaults.plugins.tooltip.backgroundColor = _viz("ink", "#212529");
  Chart.defaults.plugins.tooltip.padding         = 10;
  Chart.defaults.plugins.tooltip.cornerRadius    = 6;
  Chart.defaults.plugins.legend.labels.boxWidth      = 12;
  Chart.defaults.plugins.legend.labels.usePointStyle = true;
}

// En pantallas angostas la leyenda lateral estruja el gráfico hasta hacerlo
// ilegible; abajo siempre cabe.
function vizLegendSide() {
  return window.innerWidth < 768 ? "bottom" : "right";
}
