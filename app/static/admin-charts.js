// =============================================================================
// ADMIN — Dashboard Charts (Chart.js) + Importación CSV masiva
// Depende de: admin.js (state, API_BASE, adminSuffixFromTab, showToast)
// Requiere: Chart.js@4.4.0 cargado antes en el HTML
//
// Los colores se leen de los tokens --viz-* vía viz-tokens.js (cargar antes).
// Así el modo oscuro y los temas de color cambian los gráficos sin una segunda
// tabla de colores que mantener en sincronía.
// =============================================================================

const _chartInstances = {};

function _destroyChart(id) {
  if (_chartInstances[id]) {
    _chartInstances[id].destroy();
    delete _chartInstances[id];
  }
}

function _catOptions(extra = {}) {
  return Object.assign({
    responsive: true,
    maintainAspectRatio: false,
    animation: { duration: 450, easing: "easeOutQuart" },
    plugins: { legend: { display: false } },
  }, extra);
}

// Escala numérica compartida: rejilla discreta, sin decimales inventados.
function _countScale(axis = "y") {
  const g = { beginAtZero: true, ticks: { precision: 0 }, grid: { color: _viz("grid", "#e6e6e2") } };
  const o = { grid: { display: false } };
  return axis === "y" ? { y: g, x: o } : { x: g, y: o };
}

async function loadChartsData() {
  const suf    = adminSuffixFromTab();
  const modulo = suf === "archivo" ? "Archivo" : "RRHH";
  applyChartTheme();
  try {
    const data = await apiFetchJSON(`${API_BASE}/api/admin/charts?modulo=${modulo}`);
    _lastChartsData = { data, suf, modulo };
    if (modulo === "Archivo") {
      _renderArchivoCharts(data, suf);
    } else {
      _renderRrhhCharts(data, suf);
    }
  } catch(e) { console.error("Charts error:", e); }
}

// Se guarda el último payload para poder repintar al cambiar de tema o al
// rotar el dispositivo sin volver a pegarle a la API.
let _lastChartsData = null;

function _repaintCharts() {
  if (!_lastChartsData) return;
  applyChartTheme();
  const { data, suf, modulo } = _lastChartsData;
  if (modulo === "Archivo") _renderArchivoCharts(data, suf);
  else                      _renderRrhhCharts(data, suf);
}

// Marca la tarjeta como alerta solo si hay algo que atender. El color va
// siempre acompañado de la etiqueta y el icono, nunca solo.
function _marcarKpi(id, valor, clase) {
  const card = document.getElementById(id)?.closest(".ds-kpi-mini");
  if (!card) return;
  card.classList.remove("ds-kpi-alerta", "ds-kpi-aviso");
  if (valor > 0) card.classList.add(clase);
}

function _renderArchivoCharts(data, suf) {
  const t     = data.charts.totals || {};
  const C     = vizSeries();
  const setEl = (id, v) => { const el = document.getElementById(id); if (el) el.innerText = v ?? "—"; };
  const setSub = (id, v) => { const el = document.getElementById(id); if (el) el.innerText = v; };

  setEl(`chart-total-keywords-${suf}`, t.total_keywords);
  setEl(`chart-total-autores-${suf}`, t.total_autores);
  setEl(`chart-total-digitalizados-${suf}`, t.total_digitalizados);
  setEl(`chart-total-pendientes-${suf}`, t.total_pendientes);
  setEl(`chart-total-vencidos-${suf}`, t.total_vencidos);

  // El avance de digitalización solo se lee como proporción del fondo.
  const pct = t.total_docs ? Math.round((t.total_digitalizados / t.total_docs) * 100) : 0;
  setSub(`kpi-sub-digitalizados-${suf}`, `${pct}% del fondo`);
  setSub(`kpi-sub-pendientes-${suf}`,
         t.total_pendientes ? "borrador o revisión" : "todo aprobado");
  setSub(`kpi-sub-vencidos-${suf}`,
         t.total_vencidos ? "requieren disposición" : "ninguno vencido");

  _marcarKpi(`chart-total-vencidos-${suf}`, t.total_vencidos, "ds-kpi-alerta");
  _marcarKpi(`chart-total-pendientes-${suf}`, t.total_pendientes, "ds-kpi-aviso");

  // Estado de digitalización: los tres soportes son estados de una misma cosa,
  // así que llevan colores fijos por significado, no por posición en la lista.
  const bySoporte = data.charts.by_soporte || [];
  if (bySoporte.length) {
    _destroyChart(`by-soporte-${suf}`);
    const ctx = document.getElementById(`chart-by-soporte-${suf}`)?.getContext("2d");
    const colorSoporte = { "Digital": C[2], "Digitalizado": C[0], "Físico": _viz("ink-muted", "#6c757d") };
    if (ctx) _chartInstances[`by-soporte-${suf}`] = new Chart(ctx, {
      type: "doughnut",
      data: {
        labels: bySoporte.map(r => r.label),
        datasets: [{
          data: bySoporte.map(r => r.value),
          backgroundColor: bySoporte.map(r => colorSoporte[r.label] || C[3]),
          borderColor: _viz("surface", "#ffffff"),
          borderWidth: 2
        }]
      },
      options: _catOptions({
        cutout: "58%",
        plugins: { legend: { position: vizLegendSide(), labels: { font: { size: 11 } } } }
      })
    });
  }

  const byType = data.charts.by_type || [];
  if (byType.length) {
    _destroyChart(`by-type-${suf}`);
    const ctx = document.getElementById(`chart-by-type-${suf}`)?.getContext("2d");
    if (ctx) _chartInstances[`by-type-${suf}`] = new Chart(ctx, {
      type: "doughnut",
      data: {
        labels: byType.map(r => r.label),
        datasets: [{
          data: byType.map(r => r.value),
          backgroundColor: C,
          borderColor: _viz("surface", "#ffffff"),
          borderWidth: 2            // anillo de superficie: separa los sectores
        }]
      },
      options: _catOptions({
        cutout: "58%",
        plugins: { legend: { position: vizLegendSide(), labels: { font: { size: 11 } } } }
      })
    });
  }

  const byYear = data.charts.by_year || [];
  if (byYear.length) {
    _destroyChart(`by-year-${suf}`);
    const ctx = document.getElementById(`chart-by-year-${suf}`)?.getContext("2d");
    if (ctx) _chartInstances[`by-year-${suf}`] = new Chart(ctx, {
      type: "bar",
      data: {
        labels: byYear.map(r => r.label),
        datasets: [{ label: "Documentos", data: byYear.map(r => r.value),
          backgroundColor: C[0], borderRadius: 4, maxBarThickness: 42 }]
      },
      options: _catOptions({ scales: _countScale("y") })
    });
  }

  const byMonth = data.charts.by_month || [];
  if (byMonth.length) {
    _destroyChart(`by-month-${suf}`);
    const ctx = document.getElementById(`chart-by-month-${suf}`)?.getContext("2d");
    if (ctx) _chartInstances[`by-month-${suf}`] = new Chart(ctx, {
      type: "line",
      data: {
        labels: byMonth.map(r => r.label),
        datasets: [{ label: "Documentos", data: byMonth.map(r => r.value),
          borderColor: C[0], backgroundColor: C[0] + "22", borderWidth: 2,
          fill: true, tension: 0.35, pointRadius: 0, pointHoverRadius: 5 }]
      },
      options: _catOptions({
        interaction: { mode: "index", intersect: false },   // crosshair: toda la columna
        scales: _countScale("y")
      })
    });
  }
}

function _renderRrhhCharts(data, suf) {
  const t     = data.charts.totals || {};
  const C     = vizSeries();
  const ring  = _viz("surface", "#ffffff");
  const setEl = (id, v) => { const el = document.getElementById(id); if (el) el.innerText = v ?? "—"; };
  const setSub = (id, v) => { const el = document.getElementById(id); if (el) el.innerText = v; };

  setEl(`chart-total-emp-${suf}`, t.total_employees);
  setEl(`chart-total-activos-${suf}`, t.total_activos);
  setEl(`chart-total-jub-${suf}`, t.total_jubilados);
  setEl(`chart-total-movimientos-${suf}`, t.total_movimientos_cargo);
  setEl(`chart-total-jubproximas-${suf}`, t.total_jubilaciones_proximas);
  setEl(`chart-total-sindocs-${suf}`, t.total_sin_documentos);

  setSub(`kpi-sub-jubproximas-${suf}`,
         t.total_jubilaciones_proximas ? "preparar expediente" : "ninguna en el año");
  setSub(`kpi-sub-sindocs-${suf}`,
         t.total_sin_documentos ? "expedientes vacíos" : "todos con documentos");

  _marcarKpi(`chart-total-sindocs-${suf}`, t.total_sin_documentos, "ds-kpi-alerta");
  _marcarKpi(`chart-total-jubproximas-${suf}`, t.total_jubilaciones_proximas, "ds-kpi-aviso");

  // Cobertura por Parte: qué proporción de la plantilla tiene al menos un
  // documento en cada una. Contar documentos no responde esa pregunta — mil
  // títulos en la Parte I y ninguna evaluación en la II se vería "bien".
  const cobertura = data.charts.cobertura || [];
  if (cobertura.length) {
    _destroyChart(`cobertura-${suf}`);
    const ctx = document.getElementById(`chart-cobertura-${suf}`)?.getContext("2d");
    const total = cobertura[0]?.total || 0;
    if (ctx) _chartInstances[`cobertura-${suf}`] = new Chart(ctx, {
      type: "bar",
      data: {
        labels: cobertura.map(r => r.label),
        datasets: [{
          label: "Empleados con documentos",
          data: cobertura.map(r => total ? Math.round((r.value / total) * 100) : 0),
          backgroundColor: C[0], borderRadius: 4, maxBarThickness: 26
        }]
      },
      options: _catOptions({
        indexAxis: "y",
        scales: {
          x: { beginAtZero: true, max: 100, ticks: { callback: v => `${v}%` },
               grid: { color: _viz("grid", "#e6e6e2") } },
          y: { grid: { display: false } }
        },
        plugins: {
          legend: { display: false },
          tooltip: { callbacks: {
            label: c => {
              const fila = cobertura[c.dataIndex];
              return ` ${fila.value} de ${total} empleados (${c.parsed.x}%)`;
            }
          } }
        }
      })
    });
  }

  const doughnut = (key, id, rows) => {
    if (!rows.length) return;
    _destroyChart(key);
    const ctx = document.getElementById(id)?.getContext("2d");
    if (!ctx) return;
    _chartInstances[key] = new Chart(ctx, {
      type: "doughnut",
      data: {
        labels: rows.map(r => r.label),
        datasets: [{ data: rows.map(r => r.value), backgroundColor: C,
          borderColor: ring, borderWidth: 2 }]
      },
      options: _catOptions({
        cutout: "58%",
        plugins: { legend: { position: vizLegendSide(), labels: { font: { size: 11 } } } }
      })
    });
  };

  const barH = (key, id, rows, color) => {
    if (!rows.length) return;
    _destroyChart(key);
    const ctx = document.getElementById(id)?.getContext("2d");
    if (!ctx) return;
    _chartInstances[key] = new Chart(ctx, {
      type: "bar",
      data: {
        labels: rows.map(r => r.label),
        datasets: [{ label: "Empleados", data: rows.map(r => r.value),
          backgroundColor: color, borderRadius: 4, maxBarThickness: 26 }]
      },
      options: _catOptions({ indexAxis: "y", scales: _countScale("x") })
    });
  };

  doughnut(`by-status-${suf}`, `chart-by-status-${suf}`, data.charts.by_status || []);
  doughnut(`by-sexo-${suf}`,   `chart-by-sexo-${suf}`,   data.charts.by_sexo   || []);
  barH(`by-dept-${suf}`,  `chart-by-dept-${suf}`,  data.charts.by_department || [], C[0]);
  barH(`by-nivel-${suf}`, `chart-by-nivel-${suf}`, data.charts.by_nivel      || [], C[0]);

  const byDocType = data.charts.by_doc_type || [];
  if (byDocType.length) {
    _destroyChart(`by-doctype-${suf}`);
    const ctx = document.getElementById(`chart-by-doctype-${suf}`)?.getContext("2d");
    if (ctx) _chartInstances[`by-doctype-${suf}`] = new Chart(ctx, {
      type: "bar",
      data: {
        labels: byDocType.map(r => r.label),
        datasets: [{ label: "Docs", data: byDocType.map(r => r.value),
          backgroundColor: C[0], borderRadius: 4, maxBarThickness: 42 }]
      },
      options: _catOptions({ scales: _countScale("y") })
    });
  }
}

// Repintar cuando cambia el tema (claro/oscuro o acento) o el ancho cruza el
// punto donde la leyenda se mueve de lado a abajo.
document.addEventListener("ds:theme-change", _repaintCharts);
let _vizResizeSide = typeof window !== "undefined" ? null : null;
window.addEventListener("resize", () => {
  const side = vizLegendSide();
  if (side !== _vizResizeSide) { _vizResizeSide = side; _repaintCharts(); }
});

// =============================================================================
// IMPORT CSV MASIVO
// =============================================================================

async function handleImportCSV(tipo, suf) {
  let fileInput, resultEl, endpoint;
  if (tipo === "empleados") {
    fileInput = document.getElementById(`csv-import-empleados-${suf}`);
    resultEl  = document.getElementById(`csv-import-result-empleados-${suf}`);
    endpoint  = `${API_BASE}/api/admin/import/empleados?requester=${encodeURIComponent(state.user?.username||'')}`;
  } else if (tipo === "documentos-rrhh") {
    fileInput = document.getElementById(`csv-import-docs-${suf}`);
    resultEl  = document.getElementById(`csv-import-result-docs-${suf}`);
    endpoint  = `${API_BASE}/api/admin/import/documentos?modulo=RRHH&requester=${encodeURIComponent(state.user?.username||'')}`;
  } else {
    fileInput = document.getElementById(`csv-import-docs-${suf}`);
    resultEl  = document.getElementById(`csv-import-result-docs-${suf}`);
    endpoint  = `${API_BASE}/api/admin/import/documentos?modulo=Archivo&requester=${encodeURIComponent(state.user?.username||'')}`;
  }
  if (!fileInput?.files?.length) {
    if (resultEl) resultEl.innerHTML = '<div class="alert alert-warning p-2 mb-0">Selecciona un archivo CSV primero.</div>';
    return;
  }
  if (resultEl) {
    resultEl.innerHTML = "";
    if (typeof showProgress === "function") showProgress(resultEl.id, "Importando CSV…");
  }
  const fd = new FormData();
  fd.append("file", fileInput.files[0]);
  try {
    const data = await apiFetchJSON(endpoint, { method: "POST", body: fd });
    const errs     = (data.errors||[]).slice(0,5).map(e => `<li class="small">${escHtml(String(e))}</li>`).join("");
    const moreErrs = (data.errors||[]).length > 5 ? `<li class="small text-muted">... y ${(data.errors.length-5)} más</li>` : "";
    const summary  = [
      data.inserted != null ? `${data.inserted} insertados` : null,
      data.updated  != null ? `${data.updated} actualizados` : null,
      data.skipped  != null ? `${data.skipped} omitidos` : null,
    ].filter(Boolean).join(", ") + ".";
    if (resultEl) {
      if (typeof hideProgress === "function") hideProgress(resultEl.id);
      resultEl.innerHTML = `
        <div class="alert alert-${(data.errors||[]).length ? 'warning' : 'success'} p-2 mb-0">
          <strong>Importación completada:</strong> ${summary}
          ${errs ? `<ul class="mb-0 mt-1">${errs}${moreErrs}</ul>` : ''}
        </div>`;
    }
    showToast(`CSV importado: ${summary}`, (data.errors||[]).length ? "warning" : "success");
  } catch(e) {
    if (resultEl) {
      if (typeof hideProgress === "function") hideProgress(resultEl.id);
      resultEl.innerHTML = `<div class="alert alert-danger p-2 mb-0">Error: ${escHtml(e.message)}</div>`;
    }
    showToast("Error al importar el CSV.", "error");
  }
}

// Actualizar label del custom-file-input al seleccionar archivo
document.addEventListener("change", e => {
  if (e.target.classList.contains("custom-file-input")) {
    const lbl = e.target.parentElement.querySelector(".custom-file-label");
    if (lbl) lbl.innerText = e.target.files[0]?.name || "Seleccionar CSV...";
  }
});
