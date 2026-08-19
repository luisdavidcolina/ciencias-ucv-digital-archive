// --- ESTADÍSTICAS ---
// Alimenta la fila de KPIs del encabezado y dispara las gráficas Chart.js.
// El detalle visual (por tipo, por año, tendencia) vive en admin-charts.js: aquí
// sólo quedan las cifras que no se derivan de una gráfica.
async function loadDynamicStats() {
  const suf = adminSuffixFromTab();

  // Se marca aquí, no en loadChartsData(): entre que se abre la pestaña y que
  // responde la API ya pasan segundos, y en ese hueco las tarjetas se quedaban
  // en blanco, indistinguibles de un panel roto.
  if (typeof _marcarCargando === "function") _marcarCargando();

  // Las dos peticiones son independientes: /stats trae los totales con los
  // filtros de fecha aplicados y /charts el detalle. Encadenarlas duplicaba la
  // espera sin motivo — ahora salen juntas.
  const totales = apiFetchJSON(`${API_BASE}/api/admin/stats`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      modulo:     state.user.modulo,
      date_start: document.getElementById(`stats-date-start-${suf}`)?.value || "",
      date_end:   document.getElementById(`stats-date-end-${suf}`)?.value   || ""
    })
  });
  const graficas = loadChartsData();

  try {
    const stats = await totales;
    const kpiDocs = document.getElementById(`kpi-total-docs-${suf}`);
    const kpiCats = document.getElementById(`kpi-total-cats-${suf}`);
    if (kpiDocs) kpiDocs.innerText = stats.total_docs;
    if (kpiCats) kpiCats.innerText = stats.categories_count;
  } catch (e) {
    console.error("Error al cargar las cifras del panel:", e);
  }

  // "Último ingreso" lo sirve /charts junto al resto de totales. Antes se
  // derivaba de state.archivo.results —los resultados de la búsqueda pública,
  // que en el panel de administración no se cargan nunca—, así que ponía "N/A"
  // hasta que la otra respuesta lo sobrescribía.
  try {
    await graficas;
  } catch (e) {
    console.error("Error al cargar las gráficas:", e);
  }
}
