// --- ESTADÍSTICAS ---
// Alimenta la fila de KPIs del encabezado y dispara las gráficas Chart.js.
// El detalle visual (por tipo, por año, tendencia) vive en admin-charts.js: aquí
// sólo quedan las cifras que no se derivan de una gráfica.
async function loadDynamicStats() {
  const suf = adminSuffixFromTab();
  try {
    const stats = await apiFetchJSON(`${API_BASE}/api/admin/stats`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        modulo:     state.user.modulo,
        date_start: document.getElementById(`stats-date-start-${suf}`)?.value || "",
        date_end:   document.getElementById(`stats-date-end-${suf}`)?.value   || ""
      })
    });

    const kpiDocs = document.getElementById(`kpi-total-docs-${suf}`);
    const kpiCats = document.getElementById(`kpi-total-cats-${suf}`);
    if (kpiDocs) kpiDocs.innerText = stats.total_docs;
    if (kpiCats) kpiCats.innerText = stats.categories_count;

    // El número de usuarios ya no encabeza el panel: no es una medida del
    // archivo ni de los expedientes, y vive donde se gestiona, en "Acceso".
    // Con ello se ahorra además una llamada a la API en cada carga.

    const isArchivo  = state.user.modulo === "Archivo";
    const db_list    = isArchivo ? state.archivo.results : state.rrhh.results;
    const kpiLatest  = document.getElementById(`kpi-latest-entry-${suf}`);
    if (kpiLatest) {
      if (db_list.length > 0) {
        const dates = db_list.map(r => isArchivo ? r.fecha : r.fecha_ingreso).filter(Boolean).sort().reverse();
        kpiLatest.innerText = formatISOToSpanish(dates[0]);
      } else {
        kpiLatest.innerText = "N/A";
      }
    }

    loadChartsData();

  } catch (e) {
    console.error("Error al cargar analíticas dinámicas:", e);
  }
}
