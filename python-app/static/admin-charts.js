// =============================================================================
// ADMIN — Dashboard Charts (Chart.js) + Importación CSV masiva
// Depende de: admin.js (state, API_BASE, adminSuffixFromTab, showToast)
// Requiere: Chart.js@4.4.0 cargado antes en el HTML
// =============================================================================

const _chartInstances = {};

function _destroyChart(id) {
  if (_chartInstances[id]) {
    _chartInstances[id].destroy();
    delete _chartInstances[id];
  }
}

const CHART_COLORS = [
  "#4e73df","#1cc88a","#36b9cc","#f6c23e","#e74a3b",
  "#858796","#5a5c69","#2e59d9","#17a673","#2c9faf",
  "#fd7e14","#6f42c1","#20c9a6","#3a3b45","#dddfeb",
];

async function loadChartsData() {
  const suf    = adminSuffixFromTab();
  const modulo = suf === "archivo" ? "Archivo" : "RRHH";
  try {
    const res = await fetch(`${API_BASE}/api/admin/charts?modulo=${modulo}`);
    if (!res.ok) return;
    const data = await res.json();
    if (modulo === "Archivo") {
      _renderArchivoCharts(data, suf);
    } else {
      _renderRrhhCharts(data, suf);
    }
  } catch(e) { console.error("Charts error:", e); }
}

function _renderArchivoCharts(data, suf) {
  const t    = data.charts.totals || {};
  const setEl = (id, v) => { const el = document.getElementById(id); if (el) el.innerText = v ?? "—"; };
  setEl(`chart-total-docs-${suf}`, t.total_docs);
  setEl(`chart-total-types-${suf}`, t.total_types);
  setEl(`chart-total-keywords-${suf}`, t.total_keywords);
  setEl(`chart-total-autores-${suf}`, t.total_autores);

  const byType = data.charts.by_type || [];
  if (byType.length) {
    _destroyChart(`by-type-${suf}`);
    const ctx = document.getElementById(`chart-by-type-${suf}`)?.getContext("2d");
    if (ctx) _chartInstances[`by-type-${suf}`] = new Chart(ctx, {
      type: "doughnut",
      data: {
        labels: byType.map(r => r.label),
        datasets: [{ data: byType.map(r => r.value), backgroundColor: CHART_COLORS, borderWidth: 2 }]
      },
      options: { responsive: true, plugins: { legend: { position: "right", labels: { font: { size: 11 } } } } }
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
          backgroundColor: "#4e73df", borderRadius: 4 }]
      },
      options: { responsive: true, plugins: { legend: { display: false } },
        scales: { y: { beginAtZero: true, ticks: { stepSize: 1 } } } }
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
          borderColor: "#1cc88a", backgroundColor: "#1cc88a22",
          fill: true, tension: 0.4, pointRadius: 3 }]
      },
      options: { responsive: true, plugins: { legend: { display: false } },
        scales: { y: { beginAtZero: true, ticks: { stepSize: 1 } } } }
    });
  }
}

function _renderRrhhCharts(data, suf) {
  const t    = data.charts.totals || {};
  const setEl = (id, v) => { const el = document.getElementById(id); if (el) el.innerText = v ?? "—"; };
  setEl(`chart-total-emp-${suf}`, t.total_employees);
  setEl(`chart-total-activos-${suf}`, t.total_activos);
  setEl(`chart-total-docs-${suf}`, t.total_documents);
  setEl(`chart-total-jub-${suf}`, t.total_jubilados);

  const byStatus = data.charts.by_status || [];
  if (byStatus.length) {
    _destroyChart(`by-status-${suf}`);
    const ctx = document.getElementById(`chart-by-status-${suf}`)?.getContext("2d");
    if (ctx) _chartInstances[`by-status-${suf}`] = new Chart(ctx, {
      type: "doughnut",
      data: {
        labels: byStatus.map(r => r.label),
        datasets: [{ data: byStatus.map(r => r.value),
          backgroundColor: ["#1cc88a","#6f42c1","#e74a3b","#f6c23e","#858796"], borderWidth: 2 }]
      },
      options: { responsive: true, plugins: { legend: { position: "right" } } }
    });
  }

  const byDept = data.charts.by_department || [];
  if (byDept.length) {
    _destroyChart(`by-dept-${suf}`);
    const ctx = document.getElementById(`chart-by-dept-${suf}`)?.getContext("2d");
    if (ctx) _chartInstances[`by-dept-${suf}`] = new Chart(ctx, {
      type: "bar",
      data: {
        labels: byDept.map(r => r.label),
        datasets: [{ label: "Empleados", data: byDept.map(r => r.value),
          backgroundColor: "#4e73df", borderRadius: 4 }]
      },
      options: {
        indexAxis: "y", responsive: true,
        plugins: { legend: { display: false } },
        scales: { x: { beginAtZero: true, ticks: { stepSize: 1 } } }
      }
    });
  }

  const byDocType = data.charts.by_doc_type || [];
  if (byDocType.length) {
    _destroyChart(`by-doctype-${suf}`);
    const ctx = document.getElementById(`chart-by-doctype-${suf}`)?.getContext("2d");
    if (ctx) _chartInstances[`by-doctype-${suf}`] = new Chart(ctx, {
      type: "bar",
      data: {
        labels: byDocType.map(r => r.label),
        datasets: [{ label: "Docs", data: byDocType.map(r => r.value),
          backgroundColor: CHART_COLORS, borderRadius: 4 }]
      },
      options: { responsive: true, plugins: { legend: { display: false } },
        scales: { y: { beginAtZero: true, ticks: { stepSize: 1 } } } }
    });
  }

  const byParte = data.charts.by_parte || [];
  if (byParte.length) {
    _destroyChart(`by-parte-${suf}`);
    const ctx = document.getElementById(`chart-by-parte-${suf}`)?.getContext("2d");
    if (ctx) _chartInstances[`by-parte-${suf}`] = new Chart(ctx, {
      type: "doughnut",
      data: {
        labels: byParte.map(r => r.label),
        datasets: [{ data: byParte.map(r => r.value),
          backgroundColor: ["#0d6efd","#198754","#fd7e14","#6f42c1"], borderWidth: 2 }]
      },
      options: { responsive: true, plugins: { legend: { position: "right" } } }
    });
  }
}

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
    const res = await fetch(endpoint, { method: "POST", body: fd });
    if (!res.ok) throw new Error(`Error del servidor (HTTP ${res.status})`);
    const data = await res.json();
    const errs     = (data.errors||[]).slice(0,5).map(e => `<li class="small">${e}</li>`).join("");
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
      resultEl.innerHTML = `<div class="alert alert-danger p-2 mb-0">Error: ${e.message}</div>`;
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
