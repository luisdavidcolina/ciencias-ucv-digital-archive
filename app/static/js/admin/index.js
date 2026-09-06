// ==========================================================================
// PANEL DE CONTROL ADMINISTRATIVO
// ==========================================================================

// Nombre visible de cada pestaña, para la miga de pan y el anuncio a lectores
// de pantalla. OA-059 / OR-240: la miga debe reflejar dónde se está, no un
// texto fijo que se reescribe igual en cada cambio.
const ADMIN_TAB_LABELS = {
  stats: "Resumen", new: "Ingresar", monitor: "Documentos", categories: "Tipos",
  users: "Usuarios", audit: "Auditoría", retencion: "Retención", papelera: "Papelera",
  export: "Datos",
};

// Última pestaña cargada por módulo (archivo / rrhh), para no repetir un
// reinicio de página cuando se vuelve a entrar en la misma pestaña ya activa.
// OR-131 (parte de admin.js): "loadAdminTab('monitor')" forzaba
// state.adminTable.page = 1 en cada entrada, incluso re-entrando a la pestaña
// en la que ya se estaba.
const _adminLastTab = {};

// VI-028: "días"/"años" de retención y vencimiento llegan ya calculados del
// backend (resta de fechas hecha en SQL, no aquí). Cuando falta la fecha o el
// plazo que los origina, el campo llega null o ausente y un `Number(...)` a
// pelo pintaba "NaN" en Resumen, Retención y Tipos — un archivo institucional
// que muestra "NaN" en su pantalla de control pierde credibilidad ante quien
// lo audite. Punto único de formato, compartido con admin-categories.js y
// admin-monitor.js: comprueba el operando antes de usarlo.
function formatDias(value, unidad = "días") {
  const n = Number(value);
  return Number.isFinite(n) ? `${n} ${unidad}` : "sin plazo definido";
}
function formatAnios(value) {
  const n = Number(value);
  return Number.isFinite(n) ? `${n} año${n !== 1 ? "s" : ""}` : "—";
}

// Región viva compartida para anunciar el cambio de pestaña a lectores de
// pantalla (OR-225). Se crea una sola vez y se reutiliza.
function _adminAnnounce(text) {
  let live = document.getElementById("admin-tabs-live-region");
  if (!live) {
    live = document.createElement("div");
    live.id = "admin-tabs-live-region";
    live.setAttribute("aria-live", "polite");
    live.setAttribute("role", "status");
    live.className = "sr-only";
    live.style.cssText = "position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;border:0;";
    document.body.appendChild(live);
  }
  live.textContent = "";
  // Forzar que el lector note el cambio aunque el texto se repita.
  setTimeout(() => { live.textContent = text; }, 50);
}

function loadAdminTab(adminTabId) {
  state.activeAdminTab = adminTabId;
  const suf  = adminSuffixFromTab();
  const root = `#tab-admin-${suf}`;
  const isSameTab = _adminLastTab[suf] === adminTabId;
  _adminLastTab[suf] = adminTabId;

  // OA-178 / OR-224: el estado de pestaña activa lo declara sólo la clase
  // "active" y el color; nada llega a la tecnología asistiva. Se sincroniza
  // aria-selected (y tabindex, patrón roving tab) en cada cambio.
  document.querySelectorAll(`#admin_workspace_tabs-${suf} .nav-link`).forEach(l => {
    l.classList.remove("active");
    l.setAttribute("aria-selected", "false");
    l.setAttribute("tabindex", "-1");
  });
  const activeLink = document.getElementById(`tab-admin-${suf}-${adminTabId}`);
  if (activeLink) {
    activeLink.classList.add("active");
    activeLink.setAttribute("aria-selected", "true");
    activeLink.setAttribute("tabindex", "0");
    if (!activeLink.getAttribute("aria-controls")) {
      activeLink.setAttribute("aria-controls", `pane-admin-${suf}-${adminTabId}`);
    }
  }

  document.querySelectorAll(`${root} .tab-pane`).forEach(p => p.classList.remove("show", "active"));
  const activePane = document.getElementById(`pane-admin-${suf}-${adminTabId}`);
  if (activePane) {
    activePane.classList.add("show", "active");
    if (activeLink?.id && !activePane.getAttribute("aria-labelledby")) {
      activePane.setAttribute("aria-labelledby", activeLink.id);
    }
    // OR-225: sin foco movido ni región viva, un cambio de pestaña no se
    // anuncia. tabindex="-1" permite recibir foco por programa sin entrar en
    // el orden de tabulación normal.
    if (!activePane.hasAttribute("tabindex")) activePane.setAttribute("tabindex", "-1");
    if (!isSameTab) {
      try { activePane.focus({ preventScroll: false }); } catch { activePane.focus(); }
    }
  }

  if      (adminTabId === "stats")      { loadDynamicStats(); _loadAlertasBanner(); }
  else if (adminTabId === "new")        { renderDynamicSubmitFields(); loadRecentSubmissions(); initDropZone(suf); }
  else if (adminTabId === "monitor")    { if (!isSameTab) state.adminTable.page = 1; loadMonitorTable(); }
  // OR-042 / OA-052: "Tipos" ya no arrastra la carga de "Retención" — cada
  // pestaña sólo carga lo suyo. Antes, cada entrada a Tipos disparaba una
  // petición que pintaba una tabla en un pane oculto, y si alguien tenía un
  // plazo sin guardar en Retención, pasar por Tipos se lo pisaba por debajo.
  else if (adminTabId === "categories") loadCategoriesTab();
  else if (adminTabId === "users")      loadUsersTab();
  else if (adminTabId === "audit")      loadAuditTab();
  else if (adminTabId === "retencion")  { loadRetentionConfig(); loadVencimientosTable(); }
  else if (adminTabId === "papelera")   loadPapelera(suf);
  else if (adminTabId === "export") {
    // No hay nada que cargar, pero sí que limpiar: el resultado de una descarga
    // anterior seguiría en pantalla y se leería como el estado actual.
    const status = document.getElementById(`ds-export-status-${suf}`);
    if (status) status.innerHTML = "";
  }

  try {
    const mod = state.user?.modulo || "Archivo";
    const bc = document.querySelector(`${root} .ds-breadcrumb`);
    // OA-059 / OR-240: antes se reescribía el texto entero, siempre igual,
    // sin decir en qué pestaña se está. Ahora el último tramo es la pestaña
    // actual, y los tramos anteriores no repiten lo que ya dice el menú.
    const tabLabel = ADMIN_TAB_LABELS[adminTabId] || adminTabId;
    if (bc) bc.innerHTML = `<i class="fas fa-shield-alt"></i> ${escHtml(mod)} / Administración / ${escHtml(tabLabel)}`;
    const submitBtn = document.getElementById(`btn_submit_workspace-${suf}`);
    if (submitBtn) submitBtn.innerHTML = `<i class="fas fa-cloud-upload-alt"></i> Guardar en ${escHtml(mod)}`;
    // Acotado al pane del monitor: sin el ancla, esto reescribía el primer
    // .card-title de toda la sección (el de "Filtros Analíticos") en cada cambio.
    const monitorTitle = document.querySelector(`#pane-admin-${suf}-monitor .card-title`);
    if (monitorTitle) monitorTitle.innerHTML = mod === "RRHH"
      ? '<i class="fas fa-id-card"></i> Expedientes de personal'
      : '<i class="fas fa-folder-open"></i> Documentos del archivo';

    // OA-060: la pestaña activa no queda en la URL. Recargar F5 siempre
    // devolvía a "Resumen" y no se podía compartir el enlace de una pestaña.
    // Sólo se escribe el hash aquí (la lectura al arrancar es de app.js/H2,
    // fuera de este carril — ver nota en _BUZON.md).
    if (window.history?.replaceState) {
      const url = new URL(window.location.href);
      url.hash = adminTabId;
      window.history.replaceState(window.history.state, "", url);
    }

    if (!isSameTab) _adminAnnounce(`${tabLabel}, cargando…`);
  } catch (e) {
    console.error("Error actualizando etiquetas del panel:", e);
  }
}

// ==========================================================================
// ALERTAS DE VENCIMIENTO / JUBILACIÓN
// ==========================================================================
async function _loadAlertasBanner() {
  const suf = adminSuffixFromTab();
  const elId = suf === "archivo" ? "alertas-vencimiento-banner" : "alertas-jubilacion-banner";
  const el = document.getElementById(elId);
  if (!el) return;
  try {
    if (suf === "archivo") {
      const data = await apiFetchJSON(`${API_BASE}/api/admin/retencion/vencimientos?limite=100`);
      const total = data.total || 0;
      if (total === 0) { el.style.display = "none"; return; }
      const muestra = (data.vencimientos || []).slice(0, 3).map(v =>
        `<li class="small"><strong>${escHtml(v.titulo || "(sin título)")}</strong> — ${escHtml(v.tipo_documento || "?")} — venció ${formatDias(v.dias_vencido)}</li>`
      ).join("");
      // OA-080: mientras haya vencidos, el aviso no lleva botón de cerrar —
      // era la única alerta del panel y la más fácil de silenciar por
      // accidente, sin que volviera a aparecer en la sesión. En su lugar,
      // un enlace real a la pestaña Retención.
      el.innerHTML = `
        <div class="alert alert-warning mb-0" role="alert">
          <i class="fas fa-exclamation-triangle mr-2"></i>
          <strong>${total} documento${total !== 1 ? "s" : ""} con plazo de retención vencido.</strong>
          <ul class="mb-1 mt-1 pl-3">${muestra}</ul>
          <button type="button" class="btn btn-link p-0 small" onclick="loadAdminTab('retencion')">Ver los ${total}</button>
        </div>`;
      el.style.display = "";
    } else {
      const data = await apiFetchJSON(`${API_BASE}/api/rrhh/alertas/jubilaciones?horizonte_dias=90`);
      const total = data.total || 0;
      if (total === 0) { el.style.display = "none"; return; }
      const muestra = (data.alertas || []).slice(0, 3).map(a =>
        `<li class="small"><strong>${escHtml(a.nombre_completo)}</strong> — ${escHtml(a.tipo_alerta)} (${formatDias(a.dias_restantes)})</li>`
      ).join("");
      // OR-070: en RRHH todavía no hay una pantalla que liste las
      // jubilaciones próximas (pendiente de admin_hr.html, fuera de este
      // carril — ver _BUZON.md), así que por ahora se muestran los primeros
      // 5 en vez de 3 para reducir cuántos quedan fuera, sin prometer un
      // enlace que hoy no lleva a ninguna parte.
      const muestraAmpliada = (data.alertas || []).slice(0, 5).map(a =>
        `<li class="small"><strong>${escHtml(a.nombre_completo)}</strong> — ${escHtml(a.tipo_alerta)} (${formatDias(a.dias_restantes)})</li>`
      ).join("") || muestra;
      el.innerHTML = `
        <div class="alert alert-warning mb-0" role="alert">
          <i class="fas fa-user-clock mr-2"></i>
          <strong>${total} empleado${total !== 1 ? "s" : ""} con jubilación/pensión próxima (próximos 90 días).</strong>
          <ul class="mb-1 mt-1 pl-3">${muestraAmpliada}</ul>
          ${total > 5 ? `<small>…y ${total - 5} más.</small>` : ""}
        </div>`;
      el.style.display = "";
    }
  } catch (e) {
    // OR-069: antes un catch vacío hacía indistinguible "sin alertas" de
    // "el sistema de alertas falló". Ahora el fallo se ve, y se puede
    // reintentar sin recargar toda la pestaña.
    el.innerHTML = `
      <div class="alert alert-danger mb-0" role="alert">
        <i class="fas fa-exclamation-circle mr-2"></i>
        No se pudieron cargar las alertas${e?.message ? `: ${escHtml(e.message)}` : "."}
        <button type="button" class="btn btn-link p-0 small ml-1" onclick="_loadAlertasBanner()">Reintentar</button>
      </div>`;
    el.style.display = "";
  }
}

async function handleModuleExport(modulo) {
  const tables = modulo === "rrhh"
    ? "empleados,datos_rrhh,rrhh_descriptores,tipo_documento"
    : "datos_archivo,archivo_descriptores,descriptores_libres,tipo_documento";
  const statusEl = document.getElementById(`ds-export-status-${modulo}`);
  if (statusEl) statusEl.innerHTML = '<span class="text-muted"><i class="fas fa-spinner fa-spin mr-1"></i>Generando backup...</span>';
  // OR-036: había dos nombres para el mismo dato de sesión —
  // "state.user.usuario" aquí y "state.user.username" en _saveRetentionPlazo—
  // y la sesión sólo guarda "username". El primero viajaba siempre vacío, así
  // que la exportación de un fichero de datos personales quedaba registrada
  // como hecha por nadie.
  const requester = state.user?.username || "";
  try {
    const res = await apiFetch(`/api/admin/backup/export?tables=${tables}&requester=${encodeURIComponent(requester)}`, {
      headers: { "X-User": requester }
    });
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const blob = await res.blob();
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = `backup_${modulo}_${new Date().toISOString().slice(0,10)}.json`;
    a.click();
    URL.revokeObjectURL(url);
    const now = new Date().toLocaleTimeString();
    if (statusEl) statusEl.innerHTML = `<span class="text-success"><i class="fas fa-check mr-1"></i>Descarga iniciada (${escHtml(now)}).</span>`;
  } catch (e) {
    // OA-212 / OR-212: antes se escribía un <span> con sólo el icono y
    // después se le añadía el mensaje con append() sobre querySelector —
    // frágil y sin contexto. Ahora el mensaje completo se escribe de una vez.
    if (statusEl) {
      statusEl.innerHTML = `<span class="text-danger"><i class="fas fa-exclamation-circle mr-1"></i>${escHtml(e.message || "No se pudo generar el backup.")}</span>`;
    }
  }
}

// ==========================================================================
// TABLA DE VENCIMIENTOS (Archivo — Auditoría)
// ==========================================================================
async function loadVencimientosTable() {
  // OA-053: el resto de elementos del panel llevan sufijo de módulo
  // (-archivo/-rrhh); estos dos ids no lo llevaban, así que en cuanto RRHH
  // tenga su propia tabla de vencimientos ambos paneles escribirían en el
  // mismo nodo. Se prueba primero el id sufijado (cuando el marcado ya lo
  // tenga) y se cae al id sin sufijo mientras tanto, para no romper nada del
  // otro lado de esta división en lo que el HTML se actualiza.
  const suf     = adminSuffixFromTab();
  const tbody   = document.getElementById(`vencimientos-table-body-${suf}`) || document.getElementById("vencimientos-table-body");
  const summary = document.getElementById(`vencimientos-summary-${suf}`) || document.getElementById("vencimientos-summary");
  if (!tbody) return;
  // OR-183/OR-184: el único endpoint que existe (`/api/admin/retencion/vencimientos`)
  // consulta `datos_archivo` y exige el rol "Archivo" — es del módulo Archivo, no
  // genérico. El endpoint de RRHH que hubiera cubierto este panel se retiró por ser
  // una fuga de datos entre módulos (BR-063, ver `hr_alerts.py`) y no se sustituyó.
  // Sin esta guarda, un admin Global veía aquí documentos del Archivo institucional
  // bajo el título "Expedientes con Retención Vencida" de RRHH — el mismo módulo
  // equivocado que motivó retirar el endpoint original. Bloqueado en `retention.py`,
  // fuera de esta zona.
  if (suf === "rrhh") {
    tbody.innerHTML = `<tr><td colspan="8" class="text-center text-muted py-3">
      <i class="fas fa-circle-info mr-1"></i>Esta tabla todavía no tiene un origen de datos propio de RRHH — pendiente en el backend.
    </td></tr>`;
    if (summary) summary.textContent = "Sin datos disponibles para RRHH todavía.";
    return;
  }
  tbody.innerHTML = `<tr><td colspan="8" class="text-center text-muted py-3"><i class="fas fa-spinner fa-spin mr-1"></i>Cargando...</td></tr>`;
  try {
    const data = await apiFetchJSON(`${API_BASE}/api/admin/retencion/vencimientos?limite=100`);
    const rows = data.vencimientos || [];
    if (summary) summary.textContent = `${rows.length} documento${rows.length !== 1 ? "s" : ""} con retención vencida`;
    if (rows.length === 0) {
      tbody.innerHTML = `<tr><td colspan="8" class="text-center text-success py-3"><i class="fas fa-check-circle mr-1"></i>Sin vencimientos pendientes.</td></tr>`;
      return;
    }
    tbody.innerHTML = rows.map((v, i) => {
      // OA-146: el color de fondo era la única señal de urgencia. Para
      // daltonismo rojo-verde las tres bandas se leen igual. Se añade
      // icono + etiqueta de texto, no sólo la clase de color.
      const diasVencido = Number(v.dias_vencido);
      let urgency = "", urgLabel = "", urgIcon = "";
      if (!Number.isFinite(diasVencido)) { urgency = ""; urgLabel = "Sin datos"; urgIcon = "fa-circle-question"; }
      else if (diasVencido > 365)        { urgency = "table-danger";  urgLabel = "Crítico"; urgIcon = "fa-triangle-exclamation"; }
      else if (diasVencido > 90)         { urgency = "table-warning"; urgLabel = "Urgente";  urgIcon = "fa-clock"; }
      else                                { urgLabel = "Vencido";      urgIcon = "fa-circle-exclamation"; }
      return `<tr class="${urgency}">
        <td class="text-muted">${i + 1}</td>
        <td style="max-width:180px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;" title="${escHtml(v.titulo)}">${escHtml(v.titulo || "—")}</td>
        <td><span class="badge badge-secondary">${escHtml(v.tipo_documento || "—")}</span></td>
        <td>${escHtml(v.fecha_documento || "—")}</td>
        <td>${formatAnios(v.plazo_anios)}</td>
        <td><i class="fas ${urgIcon} mr-1" aria-hidden="true"></i><span class="sr-only">${urgLabel}: </span><strong>${formatDias(v.dias_vencido)}</strong></td>
        <td class="text-muted small ds-hide-sm">${escHtml(v.ubicacion || "—")}</td>
        <td class="text-nowrap">
          <button class="btn btn-xs btn-outline-primary" onclick="abrirDisposicion(${v.id_archivo}, ${JSON.stringify(v.titulo || "")})"
                  title="Registrar disposición documental">
            <i class="fas fa-gavel mr-1"></i>Disponer
          </button>
        </td>
      </tr>`;
    }).join("");
  } catch (e) {
    tbody.innerHTML = `<tr><td colspan="8" class="text-danger text-center py-2"></td></tr>`;
    tbody.querySelector("td").textContent = e.message;
  }
}

// ==========================================================================
// EDITOR DE PLAZOS DE RETENCIÓN (Categorías)
// ==========================================================================
async function loadRetentionConfig() {
  const suf    = adminSuffixFromTab();
  const scope  = suf === "archivo" ? "archivo" : "rrhh";
  const tbody  = document.getElementById(`retencion-tipos-body-${suf}`);
  if (!tbody) return;
  tbody.innerHTML = `<tr><td colspan="3" class="text-center text-muted py-3"><i class="fas fa-spinner fa-spin mr-1"></i>Cargando...</td></tr>`;
  try {
    const data = await apiFetchJSON(`${API_BASE}/api/admin/retencion/tipos?scope=${scope}`);
    const tipos = data.tipos || [];
    if (tipos.length === 0) {
      tbody.innerHTML = `<tr><td colspan="3" class="text-center text-muted py-3">Sin tipos configurados.</td></tr>`;
      return;
    }
    tbody.innerHTML = tipos.map(t => {
      // VI-033: un tipo documental sin nombre se pintaba como una fila alta y
      // vacía, sin nada que diga qué se está editando. Respaldo explícito con
      // el id, y aviso en la propia fila.
      const nombreLegible = t.nombre_corto || t.nombre;
      const nombreDisplay = nombreLegible
        ? escHtml(nombreLegible)
        : `<span class="text-muted font-italic" title="Tipo documental sin nombre registrado">(sin nombre — #${t.id})</span>`;
      // VI-028: el plazo de este tipo es justo el dato que se está editando;
      // si llega null/ausente no hay "operando" que restar, así que se cae a
      // un valor por defecto explícito en vez de pintar "NaN" en un <input>.
      const plazoNum = Number.isFinite(Number(t.plazo_retencion_anios)) ? Number(t.plazo_retencion_anios) : 1;
      // VI-063: catorce botones de guardar (uno por fila) obligaban a repetir
      // el mismo gesto catorce veces para el mismo cambio conceptual. La
      // tercera columna ya no lleva acción propia — el guardado ahora es un
      // único "Guardar cambios" al pie (ver _renderRetentionSaveBar) que
      // recorre las filas modificadas. No se toca el ancho de columnas: eso
      // es `styles.css`/HTML, fuera de esta zona (VI-063 [CHOCA]).
      return `
      <tr>
        <td>${nombreDisplay}</td>
        <td>
          <div class="input-group input-group-sm">
            <input type="number" class="form-control form-control-sm"
                   id="ret-plazo-${t.id}" value="${plazoNum}" min="1" max="100"
                   data-original="${plazoNum}" data-tipo-id="${t.id}" data-tipo-nombre="${nombreLegible ? escHtml(nombreLegible) : ''}"
                   style="max-width:80px;" oninput="_validateRetentionPlazoInput(this)">
            <div class="input-group-append">
              <span class="input-group-text text-muted">años</span>
            </div>
          </div>
        </td>
        <td class="text-muted small" id="ret-status-${t.id}"></td>
      </tr>`;
    }).join("");
    _renderRetentionSaveBar(tbody);
  } catch (e) {
    tbody.innerHTML = `<tr><td colspan="3" class="text-danger text-center py-2"></td></tr>`;
    tbody.querySelector("td").textContent = e.message;
  }
}

// VI-063: un único punto de guardado para toda la tabla, en vez de un botón
// por fila. Se inserta como fila de pie del propio `tbody` (no toca el
// `<thead>` de `admin_archive.html`/`admin_hr.html`, fuera de esta zona), y
// sólo envía al servidor las filas cuyo valor difiere de `data-original`.
function _renderRetentionSaveBar(tbody) {
  const table = tbody.closest("table");
  if (!table) return;
  let bar = table.parentElement.querySelector(".ds-retencion-savebar");
  if (!bar) {
    bar = document.createElement("div");
    bar.className = "ds-retencion-savebar text-right mt-2";
    table.parentElement.insertBefore(bar, table.nextSibling);
  }
  bar.innerHTML = `
    <button type="button" class="btn btn-sm btn-success" onclick="_saveAllRetentionPlazos(this)">
      <i class="fas fa-save mr-1"></i>Guardar cambios
    </button>`;
}

async function _saveAllRetentionPlazos(btnEl) {
  const table = btnEl.closest(".ds-retencion-savebar")?.previousElementSibling;
  const tbody = table?.tagName === "TABLE" ? table.querySelector("tbody") : null;
  if (!tbody) return;
  const inputs = [...tbody.querySelectorAll("input[data-tipo-id]")]
    .filter(inp => String(parseInt(inp.value)) !== inp.dataset.original && !inp.classList.contains("is-invalid"));
  if (inputs.length === 0) {
    showToast("No hay cambios de plazo sin guardar.", "info");
    return;
  }
  // _saveRetentionPlazo ya muestra un toast por cada plazo — un fallo o éxito
  // individual no se pierde, sólo deja de exigir catorce clics para llegar
  // a ellos.
  btnEl.disabled = true;
  for (const inp of inputs) await _saveRetentionPlazo(parseInt(inp.dataset.tipoId));
  btnEl.disabled = false;
}

// OR-189: antes el único límite era min/max en el marcado, y la comprobación
// real llegaba al pulsar guardar con un toast que desaparece a los pocos
// segundos, dejando en pantalla un valor que parece guardado sin estarlo.
// Ahora se marca inválido mientras se escribe.
function _validateRetentionPlazoInput(inputEl) {
  const v = parseInt(inputEl.value);
  const valid = Number.isFinite(v) && v >= 1 && v <= 100;
  inputEl.classList.toggle("is-invalid", !valid);
  inputEl.setAttribute("aria-invalid", valid ? "false" : "true");
}

async function _saveRetentionPlazo(tipoId) {
  const inputEl = document.getElementById(`ret-plazo-${tipoId}`);
  const plazo    = parseInt(inputEl?.value);
  const original = parseInt(inputEl?.dataset.original);
  if (!plazo || plazo < 1 || plazo > 100) {
    if (inputEl) {
      inputEl.classList.add("is-invalid");
      // Revierte al último valor válido conocido: un 500 tecleado no debe
      // quedarse en pantalla como si fuera el valor guardado.
      if (Number.isFinite(original)) inputEl.value = original;
    }
    showToast("El plazo debe estar entre 1 y 100 años.", "warning"); return;
  }
  try {
    await apiFetchJSON(`${API_BASE}/api/admin/retencion/tipos/${tipoId}`, {
      method: "PATCH",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ plazo_retencion_anios: plazo, requester: state.user?.username || "" }),
    });
    // OR-190: antes el toast sólo decía "Plazo actualizado.", sin decir qué
    // cambió, y desaparecía a los 2s sin dejar rastro en pantalla.
    const changeMsg = Number.isFinite(original) && original !== plazo
      ? `Plazo actualizado: de ${original} a ${plazo} año${plazo !== 1 ? "s" : ""}.`
      : "Plazo actualizado.";
    showToast(changeMsg, "success");
    if (inputEl) {
      inputEl.dataset.original = String(plazo);
      inputEl.classList.remove("is-invalid");
      inputEl.classList.add("is-valid");
      setTimeout(() => inputEl.classList.remove("is-valid"), 2000);
    }
  } catch (e) {
    showToast(`Error: ${e.message}`, "error");
  }
}



// ─── Disposición documental (ISO 15489-1:2016 §8.5) ──────────────────────────
// Disponer no borra: deja constancia de qué se decidió, quién y con qué acta.
// Por eso el acta es obligatoria — una disposición sin respaldo documental no
// sirve para lo único que sirve una disposición: demostrarla después.
//
// OA-147: antes eran dos diálogos encadenados (acta con promptModal, luego
// decisión con un modal aparte); si se cancelaba el segundo, el acta escrita
// se perdía, y se pedía el respaldo de una decisión que aún no se había
// tomado. OA-148 / OR-221: el segundo modal se construía a mano — sin
// role="dialog", sin aria-modal, sin trampa de foco, con el fondo en
// style="" en línea (invisible en modo oscuro), y el foco inicial en una
// acción en vez de en un punto de entrada seguro. Ahora es un único
// formulario accesible con decisión + acta + observaciones, foco atrapado,
// Escape para cerrar y devolución de foco al cerrar.
async function abrirDisposicion(docId, titulo) {
  const triggerEl = document.activeElement;
  const resultado = await _formularioDisposicion(titulo);
  if (triggerEl?.focus) { try { triggerEl.focus(); } catch {} }
  if (!resultado) return;

  try {
    await apiFetchJSON(`${API_BASE}/api/admin/retencion/disponer/${docId}`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        disposicion: resultado.decision,
        acta: resultado.acta,
        observaciones: resultado.observaciones || undefined,
        requester: state.user?.username || ""
      })
    });
    showToast("Disposición registrada.", "success");
    loadVencimientosTable();
    _loadAlertasBanner();
  } catch (e) {
    showToast(e.message || "No se pudo registrar la disposición.", "error");
  }
}

// Formulario único: decisión (radios), acta (obligatoria) y observaciones
// (opcional). Devuelve null si se cancela.
function _formularioDisposicion(titulo) {
  return new Promise(resolve => {
    const opciones = [
      ["conservar",   "Conservación permanente", "fa-shield-halved"],
      ["transferido", "Transferir al archivo histórico", "fa-boxes-packing"],
      ["eliminado",   "Eliminar por expurgo", "fa-fire"],
    ];
    const titleId = "disp-modal-title";
    const actaId  = "disp-modal-acta";
    const obsId   = "disp-modal-obs";
    const errId   = "disp-modal-error";
    const cuerpo = opciones.map(([v, txt, ic]) => `
      <label class="btn btn-outline-secondary btn-block text-left mb-2 ds-disp-op-label d-flex align-items-center" style="cursor:pointer;">
        <input type="radio" name="ds-disp-decision" value="${v}" class="mr-2">
        <i class="fas ${ic} mr-2" aria-hidden="true"></i>${txt}
      </label>`).join("");
    const caja = document.createElement("div");
    caja.className = "modal fade show ds-modal-backdrop";
    caja.setAttribute("role", "presentation");
    caja.style.cssText = "display:block;position:fixed;inset:0;background:rgba(0,0,0,.5);z-index:1050;overflow-y:auto;";
    caja.innerHTML = `
      <div class="modal-dialog modal-dialog-centered">
        <div class="modal-content border-0 shadow-lg" role="dialog" aria-modal="true" aria-labelledby="${titleId}" style="border-radius:12px;">
          <div class="modal-header border-0 pb-1">
            <h6 class="modal-title font-weight-bold" id="${titleId}">Registrar disposición de «${escHtml(titulo || "")}»</h6>
          </div>
          <div class="modal-body pt-2">
            <fieldset class="mb-3">
              <legend class="col-form-label pt-0 h6">¿Qué se decide?</legend>
              ${cuerpo}
            </fieldset>
            <div class="form-group">
              <label for="${actaId}">Acta o resolución que respalda la decisión <span class="text-danger">*</span></label>
              <input type="text" id="${actaId}" class="form-control" placeholder="Ej: Acta 12/2026 del Consejo de Facultad" required>
            </div>
            <div class="form-group mb-1">
              <label for="${obsId}">Observaciones (opcional)</label>
              <textarea id="${obsId}" class="form-control" rows="2"></textarea>
            </div>
            <div id="${errId}" class="text-danger small" role="alert" aria-live="assertive"></div>
          </div>
          <div class="modal-footer border-0 pt-0">
            <button type="button" class="btn btn-sm btn-secondary ds-disp-cancel">Cancelar</button>
            <button type="button" class="btn btn-sm btn-primary ds-disp-confirm">Registrar</button>
          </div>
        </div>
      </div>`;
    document.body.appendChild(caja);

    const actaInput = caja.querySelector(`#${actaId}`);
    const errBox    = caja.querySelector(`#${errId}`);

    const focusables = () => Array.from(
      caja.querySelectorAll('input, textarea, button, [tabindex]:not([tabindex="-1"])')
    ).filter(el => !el.disabled && el.offsetParent !== null);

    // Punto de entrada seguro: el acta, no una acción irreversible.
    actaInput?.focus();

    const cerrar = (value) => { caja.remove(); resolve(value); };

    const confirmar = () => {
      const decision = caja.querySelector('input[name="ds-disp-decision"]:checked')?.value;
      const acta = actaInput.value.trim();
      const observaciones = caja.querySelector(`#${obsId}`)?.value.trim();
      if (!decision) { errBox.textContent = "Elige qué se decide."; return; }
      if (!acta) { errBox.textContent = "Hace falta el acta que respalda la decisión."; actaInput.focus(); return; }
      cerrar({ decision, acta, observaciones });
    };

    caja.addEventListener("click", e => {
      if (e.target.closest(".ds-disp-confirm")) { confirmar(); return; }
      if (e.target.closest(".ds-disp-cancel") || e.target === caja) { cerrar(null); }
    });

    caja.addEventListener("keydown", e => {
      if (e.key === "Escape") { e.preventDefault(); cerrar(null); return; }
      if (e.key === "Enter" && e.target === actaInput) { e.preventDefault(); confirmar(); return; }
      // OA-172: trampa de foco real — Tab en el último elemento vuelve al
      // primero, Shift+Tab en el primero va al último.
      if (e.key === "Tab") {
        const items = focusables();
        if (items.length === 0) return;
        const first = items[0], last = items[items.length - 1];
        if (e.shiftKey && document.activeElement === first) { e.preventDefault(); last.focus(); }
        else if (!e.shiftKey && document.activeElement === last) { e.preventDefault(); first.focus(); }
      }
    });
  });
}
