// --- ESTADÍSTICAS ---
// Alimenta la fila de KPIs del encabezado y dispara las gráficas Chart.js.
// El detalle visual (por tipo, por año, tendencia) vive en admin-charts.js: aquí
// sólo quedan las cifras que no se derivan de una gráfica.
// OA-071: quien usa lector de pantalla y pulsa "Actualizar Análisis" no
// recibía ninguna señal de que las cifras cambiaron. Se marca la rejilla una
// sola vez — no hace falta repetirlo en cada carga — para que cualquier
// repintado de sus nodos de texto se anuncie.
function _marcarKpiGridVivo(suf) {
  const grid = document.getElementById(`kpi-total-docs-${suf}`)?.closest(".ds-kpi-grid");
  if (grid && !grid.hasAttribute("aria-live")) grid.setAttribute("aria-live", "polite");
}

// OA-072/VI-035: "—" es el mismo texto para "aún no ha llegado la respuesta",
// "el dato es cero" y "la petición falló". Tres estados que hoy se confunden.
function _kpiCargando(suf) {
  ["docs", "cats"].forEach(k => {
    const el = document.getElementById(`kpi-total-${k}-${suf}`);
    if (el) { el.innerText = "…"; el.title = "Cargando…"; el.classList.remove("ds-kpi-error"); }
  });
}
function _kpiError(suf) {
  ["docs", "cats"].forEach(k => {
    const el = document.getElementById(`kpi-total-${k}-${suf}`);
    if (el) {
      el.innerText = "⚠";
      el.title = "No se pudo cargar. Haz clic para reintentar.";
      el.classList.add("ds-kpi-error");
      el.style.cursor = "pointer";
      el.onclick = () => loadDynamicStats();
    }
  });
}

async function loadDynamicStats() {
  const suf = adminSuffixFromTab();
  _marcarKpiGridVivo(suf);
  _kpiCargando(suf);

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
    if (kpiDocs) { kpiDocs.innerText = stats.total_docs; kpiDocs.title = ""; kpiDocs.classList.remove("ds-kpi-error"); }
    if (kpiCats) { kpiCats.innerText = stats.categories_count; kpiCats.title = ""; kpiCats.classList.remove("ds-kpi-error"); }
  } catch (e) {
    console.error("Error al cargar las cifras del panel:", e);
    _kpiError(suf);
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
