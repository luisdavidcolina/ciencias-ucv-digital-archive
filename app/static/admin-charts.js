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

// Mientras se espera a /api/admin/charts el panel era un muro de tarjetas en
// blanco durante varios segundos, indistinguible de "esto no funciona".
function _marcarCargando() {
  document.querySelectorAll(".ds-chart-box canvas, #soporte-archivo, #soporte-rrhh")
    .forEach(el => {
      const caja = el.tagName === "CANVAS" ? el.parentElement : el;
      if (caja.querySelector(".ds-chart-cargando")) return;
      const aviso = document.createElement("div");
      aviso.className = "ds-chart-empty ds-chart-cargando";
      aviso.innerHTML = '<i class="fas fa-circle-notch fa-spin"></i><span>Cargando…</span>';
      caja.appendChild(aviso);
      if (el.tagName === "CANVAS") el.style.visibility = "hidden";
    });
}

function _quitarCargando() {
  document.querySelectorAll(".ds-chart-cargando").forEach(e => e.remove());
  document.querySelectorAll(".ds-chart-box canvas").forEach(c => { c.style.visibility = ""; });
}

async function loadChartsData() {
  const suf    = adminSuffixFromTab();
  const modulo = suf === "archivo" ? "Archivo" : "RRHH";
  applyChartTheme();
  _marcarCargando();
  try {
    const data = await apiFetchJSON(`${API_BASE}/api/admin/charts?modulo=${modulo}`);
    _quitarCargando();
    _lastChartsData = { data, suf, modulo };
    if (modulo === "Archivo") {
      _renderArchivoCharts(data, suf);
    } else {
      _renderRrhhCharts(data, suf);
    }
  } catch(e) {
    _quitarCargando();
    document.querySelectorAll(".ds-chart-box canvas").forEach(c =>
      _sinDatos(c.id, "No se pudieron cargar las gráficas."));
    console.error("Charts error:", e);
  }
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

// Una tarjeta de grafico en blanco no comunica "no hay datos", comunica "esto
// esta roto". Con 20 documentos y ninguno en los ultimos 24 meses, varias
// tarjetas salian vacias sin explicar por que.
function _sinDatos(canvasId, mensaje) {
  const canvas = document.getElementById(canvasId);
  if (!canvas) return;
  const caja = canvas.parentElement;
  canvas.style.display = "none";
  let aviso = caja.querySelector(".ds-chart-empty");
  if (!aviso) {
    aviso = document.createElement("div");
    aviso.className = "ds-chart-empty";
    caja.appendChild(aviso);
  }
  aviso.innerHTML = `<i class="fas fa-circle-info"></i><span>${mensaje}</span>`;
}

function _conDatos(canvasId) {
  const canvas = document.getElementById(canvasId);
  if (!canvas) return;
  canvas.style.display = "";
  canvas.parentElement.querySelector(".ds-chart-empty")?.remove();
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
  // OA-067: la fecha exacta en el hueco de una cifra desalinea la rejilla y no
  // se lee de un vistazo. El tiempo relativo es la cifra principal; la fecha
  // exacta queda accesible en el `title` para quien pase el ratón o el foco.
  if (t.ultimo_ingreso) {
    const elUltimo = document.getElementById(`kpi-latest-entry-${suf}`);
    if (elUltimo) {
      elUltimo.innerText = formatRelativeTime(t.ultimo_ingreso);
      elUltimo.title = formatISOToSpanish(t.ultimo_ingreso);
    }
  }

  // El avance de digitalización solo se lee como proporción del fondo.
  const pct = t.total_docs ? Math.round((t.total_digitalizados / t.total_docs) * 100) : 0;
  setSub(`kpi-sub-digitalizados-${suf}`, `${pct}% del fondo`);
  setSub(`kpi-sub-pendientes-${suf}`,
         t.total_pendientes ? "borrador o revisión" : "todo aprobado");
  setSub(`kpi-sub-vencidos-${suf}`,
         t.total_vencidos ? "requieren disposición" : "ninguno vencido");

  _marcarKpi(`chart-total-vencidos-${suf}`, t.total_vencidos, "ds-kpi-alerta");
  _marcarKpi(`chart-total-pendientes-${suf}`, t.total_pendientes, "ds-kpi-aviso");

  // Estado de digitalización. Una proporción sobre un total se lee como barra,
  // no como dona: con cero digitalizados la dona era un círculo gris entero que
  // no decía nada. La barra dice lo mismo a 0% que a 60%.
  const caja = document.getElementById(`soporte-${suf}`);
  if (caja) {
    const filas = data.charts.by_soporte || [];
    const total = filas.reduce((a, r) => a + r.value, 0);
    // OA-075/VI-036: emparejar por texto exacto deja fuera cualquier variante de
    // acento, mayúscula o espacio que llegue del backend, y entonces "0 %" convive
    // con un total de cientos de documentos en la misma tarjeta. Se normaliza antes
    // de comparar y sigue siendo por clave, no por posición (daltonismo).
    const _norm = s => String(s || "").normalize("NFD").replace(/[̀-ͯ]/g, "").trim().toLowerCase();
    const color = { "digitalizado": C[0], "digital": C[2], "fisico": _viz("ink-muted", "#6c757d") };
    const digital = filas
      .filter(r => ["digital", "digitalizado"].includes(_norm(r.label)))
      .reduce((a, r) => a + r.value, 0);
    const pctDig = total ? Math.round((digital / total) * 100) : 0;
    const colorDe = r => color[_norm(r.label)] || C[3];

    caja.innerHTML = total === 0
      ? `<div class="ds-chart-empty"><i class="fas fa-circle-info"></i><span>Aún no hay documentos registrados.</span></div>`
      : `
      <div class="ds-avance-cifra">${pctDig}<span>%</span></div>
      <div class="ds-avance-pie">${digital} de ${total} documentos con soporte digital</div>
      <div class="ds-avance-barra" role="img" aria-label="${escHtml(`${pctDig}% del fondo con soporte digital: ${filas.map(r => `${r.value} ${r.label}`).join(', ')}`)}">
        ${filas.map(r => `<div class="ds-avance-tramo" style="width:${(r.value / total) * 100}%;background:${colorDe(r)}" title="${escHtml(r.label)}: ${r.value}"></div>`).join("")}
      </div>
      <ul class="ds-avance-leyenda">
        ${filas.map(r => `<li><span class="ds-avance-punto" style="background:${colorDe(r)}"></span>${escHtml(r.label)} <b>${r.value}</b></li>`).join("")}
      </ul>`;
  }

  const byType = data.charts.by_type || [];
  if (!byType.length) _sinDatos(`chart-by-type-${suf}`, "Aún no hay documentos clasificados por tipo.");
  else {
    _conDatos(`chart-by-type-${suf}`);
    _destroyChart(`by-type-${suf}`);
    const elByType = document.getElementById(`chart-by-type-${suf}`);
    // OA-076/OR-078: un <canvas> es opaco para un lector de pantalla. El resumen
    // en aria-label lleva el mismo dato que el gráfico, sin esperar a la tabla
    // equivalente que exige el marcado (fuera de este carril).
    if (elByType) {
      elByType.setAttribute("role", "img");
      elByType.setAttribute("aria-label",
        `Documentos por tipo: ${byType.map(r => `${r.label} ${r.value}`).join(", ")}`);
    }
    const ctx = elByType?.getContext("2d");
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
  if (!byYear.length) _sinDatos(`chart-by-year-${suf}`, "Ningún documento tiene fecha registrada.");
  else {
    _conDatos(`chart-by-year-${suf}`);
    _destroyChart(`by-year-${suf}`);
    const elByYear = document.getElementById(`chart-by-year-${suf}`);
    if (elByYear) {
      elByYear.setAttribute("role", "img");
      elByYear.setAttribute("aria-label",
        `Documentos por año: ${byYear.map(r => `${r.label} ${r.value}`).join(", ")}`);
    }
    const ctx = elByYear?.getContext("2d");
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

  // OA-077: "Sin ingresos en los últimos 24 meses" con un solo mes con datos es
  // falso — sí hay ingresos, sólo que en un único mes. El estado vacío se
  // reserva para cuando de verdad no hay ninguno.
  const byMonth = data.charts.by_month || [];
  if (byMonth.length === 0) _sinDatos(`chart-by-month-${suf}`, "Sin ingresos en los últimos 24 meses.");
  else {
    _conDatos(`chart-by-month-${suf}`);
    _destroyChart(`by-month-${suf}`);
    const unSolo = byMonth.length === 1;
    const elByMonth = document.getElementById(`chart-by-month-${suf}`);
    if (elByMonth) {
      elByMonth.setAttribute("role", "img");
      elByMonth.setAttribute("aria-label", unSolo
        ? `Un solo mes con ingresos: ${byMonth[0].label} (${byMonth[0].value} documentos)`
        : `Tendencia mensual: ${byMonth.map(r => `${r.label} ${r.value}`).join(", ")}`);
    }
    const ctx = elByMonth?.getContext("2d");
    if (ctx) _chartInstances[`by-month-${suf}`] = new Chart(ctx, {
      type: "line",
      data: {
        labels: byMonth.map(r => r.label),
        datasets: [{ label: "Documentos", data: byMonth.map(r => r.value),
          borderColor: C[0], backgroundColor: C[0] + "22", borderWidth: 2,
          fill: true, tension: 0.35,
          // Con un único punto la línea no tiene nada que trazar: sin marcador
          // visible la tarjeta se ve vacía igual que el estado que se acaba de evitar.
          pointRadius: unSolo ? 5 : 0, pointHoverRadius: 5, pointBackgroundColor: C[0] }]
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
  if (t.ultimo_ingreso) {
    const elUltimo = document.getElementById(`kpi-latest-entry-${suf}`);
    if (elUltimo) {
      elUltimo.innerText = formatRelativeTime(t.ultimo_ingreso);
      elUltimo.title = formatISOToSpanish(t.ultimo_ingreso);
    }
  }

  setSub(`kpi-sub-jubproximas-${suf}`,
         t.total_jubilaciones_proximas ? "preparar expediente" : "ninguna en el año");
  // Los sistemas de expedientes miden la completitud como TASA contra una meta,
  // no como conteo: "9" no dice nada sin saber sobre cuantos.
  const conDocs = (t.total_employees || 0) - (t.total_sin_documentos || 0);
  const pctComp = t.total_employees
    ? Math.round((conDocs / t.total_employees) * 100) : 0;
  setSub(`kpi-sub-sindocs-${suf}`,
         t.total_employees
           ? `${pctComp}% de expedientes iniciados`
           : "sin personal registrado");

  _marcarKpi(`chart-total-sindocs-${suf}`, t.total_sin_documentos, "ds-kpi-alerta");
  _marcarKpi(`chart-total-jubproximas-${suf}`, t.total_jubilaciones_proximas, "ds-kpi-aviso");

  // Cobertura por Parte: qué proporción de la plantilla tiene al menos un
  // documento en cada una. Contar documentos no responde esa pregunta — mil
  // títulos en la Parte I y ninguna evaluación en la II se vería "bien".
  const cobertura = data.charts.cobertura || [];
  // OR-073: si la consulta no devuelve filas (sin categorías "Parte" configuradas,
  // el mismo hueco que provoca OA-001 en Retención) el bloque se saltaba entero y
  // dejaba el canvas sin "Cargando…" y sin contenido: una tarjeta en blanco.
  if (!cobertura.length) {
    _sinDatos(`chart-cobertura-${suf}`, "No hay Partes de expediente configuradas.");
  } else {
    _conDatos(`chart-cobertura-${suf}`);
    _destroyChart(`cobertura-${suf}`);
    const elCobertura = document.getElementById(`chart-cobertura-${suf}`);
    const total = cobertura[0]?.total || 0;
    if (elCobertura) {
      elCobertura.setAttribute("role", "img");
      elCobertura.setAttribute("aria-label",
        `Cobertura de expedientes sobre ${total} empleados: ` +
        cobertura.map(r => `${r.label} ${total ? Math.round((r.value/total)*100) : 0}%`).join(", "));
    }
    const ctx = elCobertura?.getContext("2d");
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

  // OR-074: "sin datos suficientes" con rows.length < 2 desmiente una sola
  // categoría real (toda la plantilla Activa, o con el mismo nivel educativo).
  // El estado vacío se reserva para cero filas; una sola se pinta con el slot 1.
  const doughnut = (key, id, rows, vacio) => {
    if (!rows.length) { _sinDatos(id, vacio || "Sin datos suficientes."); return; }
    _conDatos(id);
    _destroyChart(key);
    const elD = document.getElementById(id);
    if (elD) {
      elD.setAttribute("role", "img");
      elD.setAttribute("aria-label", rows.map(r => `${r.label} ${r.value}`).join(", "));
    }
    const ctx = elD?.getContext("2d");
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

  const barH = (key, id, rows, color, vacio) => {
    if (!rows.length) { _sinDatos(id, vacio || "Sin datos suficientes."); return; }
    _conDatos(id);
    _destroyChart(key);
    const elB = document.getElementById(id);
    if (elB) {
      elB.setAttribute("role", "img");
      elB.setAttribute("aria-label", rows.map(r => `${r.label} ${r.value}`).join(", "));
    }
    const ctx = elB?.getContext("2d");
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

  doughnut(`by-status-${suf}`, `chart-by-status-${suf}`, data.charts.by_status || [],
         "Todos los empleados están en el mismo estado.");
  doughnut(`by-sexo-${suf}`,   `chart-by-sexo-${suf}`,   data.charts.by_sexo   || [],
         "Sin sexo registrado en las fichas de personal.");
  barH(`by-dept-${suf}`,  `chart-by-dept-${suf}`,  data.charts.by_department || [], C[0],
     "Sin departamentos asignados.");
  barH(`by-nivel-${suf}`, `chart-by-nivel-${suf}`, data.charts.by_nivel      || [], C[0],
     "Sin nivel educativo registrado en las fichas de personal.");

  const byDocType = data.charts.by_doc_type || [];
  if (!byDocType.length) {
    _sinDatos(`chart-by-doctype-${suf}`, "Aún no hay documentos clasificados por tipo.");
  } else {
    _conDatos(`chart-by-doctype-${suf}`);
    _destroyChart(`by-doctype-${suf}`);
    const elDocType = document.getElementById(`chart-by-doctype-${suf}`);
    if (elDocType) {
      elDocType.setAttribute("role", "img");
      elDocType.setAttribute("aria-label",
        `Documentos por tipo: ${byDocType.map(r => `${r.label} ${r.value}`).join(", ")}`);
    }
    const ctx = elDocType?.getContext("2d");
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
    // OR-110: sin excepciones no es lo mismo que "se hizo algo". Con 0
    // insertados y 0 actualizados (por ejemplo, el CSV llegó con el separador
    // equivocado y ninguna fila casó) la alerta salía verde diciendo
    // "completada" aunque no entrara ni una fila.
    const huboExito = (data.inserted || 0) > 0 || (data.updated || 0) > 0;
    const huboErrores = (data.errors || []).length > 0;
    const nivel = huboErrores ? "warning" : (huboExito ? "success" : "warning");
    const titulo = huboExito ? "Importación completada" : "Importación sin cambios";
    if (resultEl) {
      if (typeof hideProgress === "function") hideProgress(resultEl.id);
      resultEl.innerHTML = `
        <div class="alert alert-${nivel} p-2 mb-0">
          <strong>${titulo}:</strong> ${summary}
          ${errs ? `<ul class="mb-0 mt-1">${errs}${moreErrs}</ul>` : ''}
        </div>`;
    }
    showToast(`CSV importado: ${summary}`, nivel);
  } catch(e) {
    if (resultEl) {
      if (typeof hideProgress === "function") hideProgress(resultEl.id);
      resultEl.innerHTML = `<div class="alert alert-danger p-2 mb-0">Error: ${escHtml(e.message)}</div>`;
    }
    showToast("Error al importar el CSV.", "error");
  }
}

// OA-014/OR-026: el listener escuchaba ".custom-file-input", una clase de
// Bootstrap ausente de este marcado — el input real lleva ".ds-import-file-input"
// y su <span> de etiqueta es "csv-label-<mismo-sufijo-del-id-del-input>". Con el
// nombre equivocado nunca se disparaba: se elegía el CSV y la etiqueta seguía
// diciendo "Elegir archivo…", sin forma de saber si quedó seleccionado.
document.addEventListener("change", e => {
  if (e.target.classList.contains("ds-import-file-input")) {
    const lblId = `csv-label-${e.target.id.replace(/^csv-import-/, "")}`;
    const lbl = document.getElementById(lblId);
    if (!lbl) return;
    const file = e.target.files[0];
    if (!file) { lbl.innerText = "Elegir archivo…"; lbl.removeAttribute("title"); return; }
    const kb = Math.round(file.size / 1024);
    const nombreCorto = file.name.length > 28 ? file.name.slice(0, 25) + "…" : file.name;
    lbl.innerText = `${nombreCorto} (${kb} KB)`;
    lbl.title = file.name;
  }
});
