// --- MONITOR ---
// Nota (B5-admin-monitor): este archivo es el único que este carril puede tocar.
// Los pendientes que exigen cambios en admin_archive.html, admin_hr.html, docs.py,
// hr.py o main.py quedan anotados en docs/auditoria/_BUZON.md y no se tocan aquí.

// OA-106 / OR-131: persistencia de filtros y página en la URL, por sufijo de
// módulo para que Archivo y RRHH no se pisen. `type`/`person` son <select> que
// se repueblan en cada carga (OA-029), así que su valor de la URL se guarda
// aparte y se re-aplica cuando las opciones ya existen, no al restaurar.
function _monitorURLKeys(suf) {
  return { q: `m_${suf}_q`, type: `m_${suf}_type`, person: `m_${suf}_person`, status: `m_${suf}_status`, page: `m_${suf}_page` };
}

function _restoreMonitorFiltersFromURL(suf) {
  state.adminTable._urlRestored = state.adminTable._urlRestored || {};
  if (state.adminTable._urlRestored[suf]) return;
  state.adminTable._urlRestored[suf] = true;
  const params = new URLSearchParams(window.location.search);
  const keys = _monitorURLKeys(suf);
  const qEl = document.getElementById(`admin_search-${suf}`);
  if (qEl && params.has(keys.q)) qEl.value = params.get(keys.q);
  const statusEl = document.getElementById(`admin_filter_status-${suf}`);
  if (statusEl && params.has(keys.status)) statusEl.value = params.get(keys.status);
  state.adminTable._pendingURLType = state.adminTable._pendingURLType || {};
  state.adminTable._pendingURLPerson = state.adminTable._pendingURLPerson || {};
  if (params.has(keys.type)) state.adminTable._pendingURLType[suf] = params.get(keys.type);
  if (params.has(keys.person)) state.adminTable._pendingURLPerson[suf] = params.get(keys.person);
  const pageVal = parseInt(params.get(keys.page), 10);
  if (pageVal > 0) state.adminTable.page = pageVal;
}

function _updateMonitorURL(suf, q, type, person, statusFilt) {
  const params = new URLSearchParams(window.location.search);
  const keys = _monitorURLKeys(suf);
  const setOrDelete = (key, val) => { if (val) params.set(key, val); else params.delete(key); };
  setOrDelete(keys.q, q);
  setOrDelete(keys.type, type);
  setOrDelete(keys.person, person);
  setOrDelete(keys.status, statusFilt);
  setOrDelete(keys.page, (state.adminTable.page && state.adminTable.page > 1) ? String(state.adminTable.page) : "");
  const qs = params.toString();
  const newUrl = window.location.pathname + (qs ? `?${qs}` : "") + window.location.hash;
  window.history.replaceState(null, "", newUrl);
}

async function loadMonitorTable() {
  const mod        = state.user.modulo;
  const suf        = adminSuffixFromTab();
  _restoreMonitorFiltersFromURL(suf);
  const q          = document.getElementById(`admin_search-${suf}`)?.value          || "";
  let   type       = document.getElementById(`admin_filter_type-${suf}`)?.value     || "";
  let   person     = document.getElementById(`admin_filter_person-${suf}`)?.value   || "";
  const statusFilt = document.getElementById(`admin_filter_status-${suf}`)?.value   || "";
  const page    = state.adminTable.page    || 1;
  const perPage = state.adminTable.perPage || 25;

  // OA-119 / OR-134: para distinguir "aún no hay datos" de "ningún resultado
  // coincide" hace falta saber si hay algún filtro activo en esta consulta.
  state.adminTable.filtersActive = !!(q || type || person || statusFilt);
  state.adminTable.lastError = null;

  if (typeof showTableSkeleton === "function") {
    showTableSkeleton(`admin_control_table-${suf}`, isArchivoModule() ? 6 : 7, 6);
  }
  const tableEl = document.getElementById(`admin_control_table-${suf}`)?.closest("table");
  if (tableEl) tableEl.setAttribute("aria-busy", "true");

  try {
    const url = `${API_BASE}/api/admin/list_all?modulo=${mod}&search=${encodeURIComponent(q)}&type_filter=${encodeURIComponent(type)}&person_filter=${encodeURIComponent(person)}&status_filter=${encodeURIComponent(statusFilt)}&page=${page}&per_page=${perPage}`;
    const data = await apiFetchJSON(url);

    state.adminTable.results = data.records;
    state.adminTable.total   = data.total;

    // OA-029: una sola etiqueta y repoblar en cada carga (no sólo la primera vez),
    // así una tipología nueva aparece sin recargar la página entera.
    const typeSelector = document.getElementById(`admin_filter_type-${suf}`);
    if (typeSelector && state.choices) {
      const types = isArchivoModule() ? state.choices.archivo.doc_types : state.choices.rrhh.doc_types;
      const prevType = typeSelector.value;
      const optionsHtml = `<option value="">Filtrar por Tipología...</option>` +
        (types || []).map(t => `<option value="${escHtml(t)}">${escHtml(t)}</option>`).join("");
      if (typeSelector.dataset.dsOptionsSig !== optionsHtml.length + ":" + (types || []).join("|")) {
        typeSelector.innerHTML = optionsHtml;
        typeSelector.value = prevType;
        typeSelector.dataset.dsOptionsSig = optionsHtml.length + ":" + (types || []).join("|");
      }
      // OA-106/OR-131: la URL traía un tipo restaurado, pero al momento del
      // fetch anterior las opciones aún no existían — se aplica ahora y se
      // recarga una sola vez con el filtro real.
      if (state.adminTable._pendingURLType?.[suf] !== undefined) {
        const wanted = state.adminTable._pendingURLType[suf];
        delete state.adminTable._pendingURLType[suf];
        if (wanted && wanted !== type) {
          typeSelector.value = wanted;
          if (typeSelector.value === wanted) { loadMonitorTable(); return; }
        }
      }
    }

    // Poblar filtro de persona (solo primera carga: en Archivo depende de la
    // página que llegó, así que no tiene sentido recalcularlo en cada tecleo).
    const personSelector = document.getElementById(`admin_filter_person-${suf}`);
    if (personSelector && personSelector.options.length <= 1) {
      let people = [];
      if (isArchivoModule()) {
        people = [...new Set((data.records || []).map(r => r.autor).filter(Boolean))].sort();
      } else {
        people = state.choices?.rrhh?.people || [];
      }
      personSelector.innerHTML = `<option value="">Filtrar por Persona...</option>` +
        people.map(p => `<option value="${escHtml(p)}">${escHtml(p)}</option>`).join("");
    }
    if (personSelector && state.adminTable._pendingURLPerson?.[suf] !== undefined) {
      const wantedPerson = state.adminTable._pendingURLPerson[suf];
      delete state.adminTable._pendingURLPerson[suf];
      if (wantedPerson && wantedPerson !== person) {
        personSelector.value = wantedPerson;
        if (personSelector.value === wantedPerson) { loadMonitorTable(); return; }
      }
    }

    _updateMonitorURL(suf, q, type, person, statusFilt);
    _ensureMonitorToolbarExtras(suf);
    renderMonitorTable();
    _renderMonitorStatusBadges();
  } catch (e) {
    console.error("Error al cargar monitor:", e);
    state.adminTable.lastError = e?.message || "No se pudo cargar la tabla.";
    state.adminTable.results = [];
    state.adminTable.total = 0;
    renderMonitorTable();
  } finally {
    if (tableEl) tableEl.setAttribute("aria-busy", "false");
  }
}

async function _renderMonitorStatusBadges() {
  const suf    = adminSuffixFromTab();
  const mod    = state.user.modulo;
  const badgesEl = document.getElementById(`monitor-status-badges-${suf}`);
  if (!badgesEl) return;
  if (!isArchivoModule()) { badgesEl.innerHTML = ""; return; }
  try {
    const counts = await apiFetchJSON(`${API_BASE}/api/admin/status_counts?modulo=${mod}`);
    const defs = [
      { key: "revision",  label: "Pendientes revisión", cls: "badge-warning text-dark", icon: "fa-clock" },
      { key: "draft",     label: "Borrador",            cls: "badge-secondary",         icon: "fa-pencil-alt" },
      { key: "rechazado", label: "Rechazados",          cls: "badge-danger",            icon: "fa-times-circle" },
    ];
    badgesEl.innerHTML = defs
      .filter(d => (counts[d.key] || 0) > 0)
      .map(d => `
        <button class="badge ${d.cls} ds-status-badge" style="cursor:pointer;font-size:0.78rem;padding:5px 9px;border:none;"
          title="Filtrar por: ${d.label}"
          onclick="document.getElementById('admin_filter_status-${suf}').value='${d.key}';state.adminTable.page=1;loadMonitorTable();">
          <i class="fas ${d.icon} mr-1"></i>${d.label}: <strong>${counts[d.key]}</strong>
        </button>`)
      .join("");
  } catch {}
}

// OA-107 / OR-... : "Limpiar filtros" — se inyecta junto a los propios controles
// de filtro (no depende de marcado nuevo en el HTML, sólo de que los selects de
// filtro que ya existen sigan ahí) y sólo aparece cuando hay algo que limpiar.
// OA-118 / OR-130: salto directo de página junto al paginador existente.
function _ensureMonitorToolbarExtras(suf) {
  const statusSel = document.getElementById(`admin_filter_status-${suf}`);
  if (statusSel && !document.getElementById(`admin_clear_filters-${suf}`)) {
    const btn = document.createElement("button");
    btn.type = "button";
    btn.id = `admin_clear_filters-${suf}`;
    btn.className = "btn btn-xs btn-outline-secondary ml-2";
    btn.innerHTML = `<i class="fas fa-eraser mr-1"></i>Limpiar filtros`;
    btn.addEventListener("click", () => _clearMonitorFilters(suf));
    statusSel.insertAdjacentElement("afterend", btn);
  }

  const nextBtn = document.getElementById(`admin_next-${suf}`);
  if (nextBtn && !document.getElementById(`admin_goto_page-${suf}`)) {
    const wrap = document.createElement("span");
    wrap.className = "ml-2 d-inline-flex align-items-center";
    wrap.innerHTML = `
      <label for="admin_goto_page-${suf}" class="sr-only">Ir a la página</label>
      <input type="number" min="1" id="admin_goto_page-${suf}" class="form-control form-control-sm"
             style="width:64px;display:inline-block;" aria-label="Ir a la página">
      <button type="button" class="btn btn-xs btn-outline-secondary ml-1" id="admin_goto_page_btn-${suf}"
              aria-label="Ir a la página indicada">Ir</button>`;
    nextBtn.insertAdjacentElement("afterend", wrap);
    const go = () => {
      const input = document.getElementById(`admin_goto_page-${suf}`);
      const n = parseInt(input?.value, 10);
      const total = state.adminTable.total || 0;
      const perPage = state.adminTable.perPage || 25;
      const totalPages = Math.max(1, Math.ceil(total / perPage));
      if (!n || n < 1 || n > totalPages) {
        showToast(`Ingresa una página entre 1 y ${totalPages}.`, "warning");
        return;
      }
      state.adminTable.page = n;
      loadMonitorTable();
    };
    document.getElementById(`admin_goto_page_btn-${suf}`).addEventListener("click", go);
    document.getElementById(`admin_goto_page-${suf}`).addEventListener("keydown", e => {
      if (e.key === "Enter") { e.preventDefault(); go(); }
    });
  }
}

// OA-027 (parte): tras guardar una edición, actualizar sólo la fila afectada
// en vez de disparar una recarga completa que puede fallar en silencio si la
// red falla justo después de un guardado exitoso.
function updateMonitorRowOptimistic(id, patch) {
  const results = state.adminTable.results || [];
  const idx = results.findIndex(r => r.id == id);
  if (idx === -1) { loadMonitorTable(); return; }
  results[idx] = { ...results[idx], ...patch };
  renderMonitorTable();
}

function _clearMonitorFilters(suf) {
  const ids = [`admin_search-${suf}`, `admin_filter_type-${suf}`, `admin_filter_person-${suf}`, `admin_filter_status-${suf}`];
  ids.forEach(id => { const el = document.getElementById(id); if (el) el.value = ""; });
  state.adminTable.page = 1;
  loadMonitorTable();
}

function renderMonitorTable() {
  const suf       = adminSuffixFromTab();
  const records   = state.adminTable.results || [];
  const total     = state.adminTable.total   || records.length;
  const isArch    = isArchivoModule();
  const container = document.getElementById(`admin_control_table-${suf}`);
  const summaryEl = document.getElementById(`admin_table_summary-${suf}`);

  const perPage    = state.adminTable.perPage || 25;
  const page       = state.adminTable.page    || 1;
  const totalPages = Math.ceil(total / perPage) || 1;

  // OA-117 / OR-129: "Mostrando 1–25 de 412", no una cifra suelta sin relación
  // con lo que se está viendo.
  if (summaryEl) {
    if (total === 0) {
      summaryEl.innerText = `Mostrando 0 de 0 ${isArch ? "documentos" : "expedientes"}`;
    } else {
      const from = (page - 1) * perPage + 1;
      const to   = Math.min(page * perPage, total);
      const filtro = state.adminTable.filtersActive ? " (filtrado)" : "";
      summaryEl.innerText = `Mostrando ${from}–${to} de ${total} ${isArch ? "documentos" : "expedientes"}${filtro}`;
    }
  }

  const pageInfo = document.getElementById(`admin_page_info-${suf}`);
  if (pageInfo) pageInfo.innerText = `Pág ${page} de ${totalPages}`;
  const prevBtnAdm = document.getElementById(`admin_prev-${suf}`);
  const nextBtnAdm = document.getElementById(`admin_next-${suf}`);
  if (prevBtnAdm) { prevBtnAdm.disabled = page <= 1; prevBtnAdm.setAttribute("aria-label", "Página anterior"); }
  if (nextBtnAdm) { nextBtnAdm.disabled = page >= totalPages; nextBtnAdm.setAttribute("aria-label", "Página siguiente"); }

  const colspan = isArch ? 6 : 7;

  // OR-133: un error de carga no puede quedarse como el esqueleto animado para
  // siempre — se distingue de "cero resultados" y trae un botón de reintentar.
  if (state.adminTable.lastError) {
    container.innerHTML = `<tr><td colspan="${colspan}" class="text-center p-3">
      <div class="text-danger mb-2"><i class="fas fa-exclamation-triangle mr-1"></i>
        No se pudo cargar la tabla: ${escHtml(state.adminTable.lastError)}</div>
      <button class="btn btn-sm btn-outline-secondary" onclick="loadMonitorTable()">
        <i class="fas fa-redo mr-1"></i>Reintentar</button>
    </td></tr>`;
    return;
  }

  if (records.length === 0) {
    // OA-119 / OR-134: "aún no hay nada" no es lo mismo que "no hay resultados
    // para esta búsqueda", y el módulo de RRHH no gestiona "archivos".
    const noun = isArch ? "documento" : "expediente";
    if (state.adminTable.filtersActive) {
      container.innerHTML = `<tr><td colspan="${colspan}" class="text-muted text-center p-3">
        Ningún ${noun} coincide con los criterios de búsqueda.
        <button class="btn btn-link btn-sm p-0 ml-1" onclick="document.getElementById('admin_clear_filters-${suf}')?.click()">Limpiar filtros</button>
      </td></tr>`;
    } else {
      container.innerHTML = `<tr><td colspan="${colspan}" class="text-muted text-center p-3">
        Aún no hay ${isArch ? "documentos" : "expedientes"} en el módulo ${state.user.modulo}.
      </td></tr>`;
    }
    return;
  }

  // OA-115 / OA-116: una sola etiqueta por estado en las cuatro superficies
  // donde aparece, y un badge neutro explícito para un estado desconocido en
  // vez de darlo por aprobado.
  const STATUS_BADGES = {
    draft:      '<span class="badge badge-secondary ds-status-badge" title="Borrador — sin publicar"><i class="fas fa-pencil-alt mr-1"></i>Borrador</span>',
    revision:   '<span class="badge badge-warning text-dark ds-status-badge ds-status-revision" title="Pendiente de revisión"><i class="fas fa-clock mr-1"></i>Revisión</span>',
    aprobado:   '<span class="badge badge-success ds-status-badge" title="Aprobado y publicado"><i class="fas fa-check mr-1"></i>Aprobado</span>',
    rechazado:  '<span class="badge badge-danger ds-status-badge" title="Rechazado"><i class="fas fa-times mr-1"></i>Rechazado</span>',
  };
  const UNKNOWN_STATUS_BADGE =
    '<span class="badge badge-light border ds-status-badge" title="Estado no reconocido"><i class="fas fa-question mr-1"></i>Sin estado</span>';

  const searchTerms = (document.getElementById(`admin_search-${suf}`)?.value || "").trim().split(/\s+/).filter(Boolean);

  if (isArch) {
    container.innerHTML = records.map(f => {
      const statusBadge = STATUS_BADGES[f.status] || (f.status ? UNKNOWN_STATUS_BADGE : STATUS_BADGES.aprobado);
      const fileIcon = f.file_url
        ? `<a href="${_secureFileUrl(f.file_url)}" target="_blank" rel="noopener noreferrer" class="btn btn-xs btn-outline-info mr-1" title="Ver archivo" aria-label="Ver archivo: ${escHtml(f.titulo||'')}"><i class="fas fa-file"></i></a>`
        : "";
      const titulo = typeof highlightTerms === "function"
        ? highlightTerms(f.titulo || "", searchTerms)
        : (f.titulo || "");
      const autor = typeof highlightTerms === "function"
        ? highlightTerms(f.autor || "—", searchTerms)
        : (f.autor || "—");
      const statusBtnTitle = { draft: "Borrador", revision: "En revisión", aprobado: "Aprobado", rechazado: "Rechazado" }[f.status] || (f.status ? "Sin estado" : "Aprobado");
      const nombreDoc = escHtml(f.titulo || "");
      // OA-110: pendiente — ver nota en _BUZON.md. `test_admin_panels.py`
      // localiza la plantilla de fila con el literal exacto
      // `<tr class="ds-monitor-row">` (sin atributos) para comparar columnas
      // de cabecera contra celdas; un manejador de clic en el <tr> rompe ese
      // ancla y no hay margen en este carril para tocar el test.
      return `<tr class="ds-monitor-row">
        <td class="font-weight-bold text-dark" style="max-width:200px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap" title="${escHtml(f.titulo||'')}">${titulo}</td>
        <td class="text-muted small ds-hide-sm">${autor}</td>
        <td class="text-muted small ds-hide-xs">${formatISOToSpanish(f.fecha)}</td>
        <td class="ds-hide-sm"><span class="badge badge-light border">${escHtml(f.doc_type||'—')}</span></td>
        <td>
          <button class="btn btn-xs btn-link p-0 ds-status-btn" title="Cambiar estado: ${statusBtnTitle}" aria-label="Cambiar estado de ${nombreDoc}: ${statusBtnTitle}"
            onclick="openQuickStatusMenu(this,${f.id},${JSON.stringify(f.status||'aprobado')},${JSON.stringify(state.user.modulo)})">
            ${statusBadge}
          </button>
        </td>
        <td>
          ${fileIcon}
          <button class="btn btn-xs btn-outline-secondary mr-1" onclick="openAdminDocById(${f.id})" title="Ver" aria-label="Ver: ${nombreDoc}"><i class="fas fa-eye"></i></button>
          <button class="btn btn-xs btn-outline-warning mr-1" onclick="openEditDocModal(${f.id})" title="Editar" aria-label="Editar: ${nombreDoc}"><i class="fas fa-edit"></i></button>
          <button class="btn btn-xs btn-outline-info mr-1" onclick="compartirDocumento(${f.id})" title="Compartir enlace temporal" aria-label="Compartir enlace temporal de: ${nombreDoc}"><i class="fas fa-link"></i></button>
          <span class="border-left pl-1 ml-1">
            <button class="btn btn-xs btn-outline-danger" onclick="handleDeleteDoc(${f.id},${JSON.stringify(f.titulo||'')})" title="Eliminar" aria-label="Eliminar: ${nombreDoc}"><i class="fas fa-trash"></i></button>
          </span>
        </td>
      </tr>`;
    }).join("");
  } else {
    // OR-135: clase por estado laboral (con variante de modo oscuro y temas en
    // styles.css) en vez del `style` inline que armaba getStatusColor().
    const STATUS_CLASS = { Activo: "ds-status-activo", Retirado: "ds-status-retirado", Jubilado: "ds-status-jubilado", Pensionado: "ds-status-pensionado" };
    container.innerHTML = records.map(f => {
      const statusCls = STATUS_CLASS[f.estado] || "ds-status-otro";
      const hlEmpleado = typeof highlightTerms === "function" ? highlightTerms(f.empleado || "", searchTerms) : (f.empleado || "");
      const nombreEmp = escHtml(f.empleado || "");
      // OR-237: avatar con iniciales como respaldo (no llega foto por ahora,
      // ver nota en _BUZON.md) — evita confundir homónimos, mismo problema que
      // BR-004.
      const initials = typeof getInitials === "function" ? getInitials(f.empleado || "?") : "?";
      const avatar = `<span class="ds-person-avatar-sm mr-2" style="width:28px;height:28px;vertical-align:middle;">
        <span class="ds-person-initials-sm" style="font-size:0.7rem;">${escHtml(initials)}</span>
      </span>`;
      // OR-121 (parcial): el número de documentos ya lo sirve el backend
      // (f.doc_count) pero no cabe una columna nueva sin tocar admin_hr.html
      // (fuera de este carril, ver _BUZON.md) — se muestra como indicador
      // junto al nombre mientras tanto.
      const docCountBadge = (f.doc_count !== undefined && f.doc_count !== null)
        ? `<span class="badge badge-light border ml-1" title="${f.doc_count} documento(s) en el expediente">${f.doc_count} <i class="fas fa-file-alt"></i></span>`
        : "";
      // OR-120: cargo y departamento en dos líneas, no "uno u otro".
      const cargoDeptoCell = (f.cargo || f.departamento)
        ? `${f.cargo ? `<div class="font-weight-bold" style="line-height:1.2;">${escHtml(f.cargo)}</div>` : ""}${f.departamento ? `<div class="text-muted" style="line-height:1.2;font-size:0.85em;">${escHtml(f.departamento)}</div>` : ""}`
        : "—";
      return `
        <tr class="ds-monitor-row">
          <td class="font-weight-bold text-dark" style="max-width:180px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;" title="${escHtml(f.empleado||'')}">${avatar}${hlEmpleado}${docCountBadge}</td>
          <td class="text-muted small ds-hide-sm">${escHtml(f.cedula||'—')}</td>
          <td class="text-muted small ds-hide-sm" style="max-width:120px;overflow:hidden;text-overflow:ellipsis;" title="${escHtml([f.cargo, f.departamento].filter(Boolean).join(' · '))}">${cargoDeptoCell}</td>
          <td><span class="badge ${statusCls}" style="padding:3px 6px;">${escHtml(f.estado||'—')}</span></td>
          <td class="ds-hide-sm"><span class="badge badge-light border" title="${escHtml(f.tipos||'')}" style="padding:3px 6px;">${escHtml((f.tipos||'').split(';')[0].trim()||'—')}</span></td>
          <td class="text-muted small ds-hide-sm">${escHtml(f.ubicacion||'—')}</td>
          <td>
            <button class="btn btn-xs btn-outline-secondary mr-1" onclick="openRrhhPersonDossier(${JSON.stringify(f.empleado)})" title="Ver Expediente" aria-label="Ver expediente de ${nombreEmp}"><i class="fas fa-eye"></i></button>
            <button class="btn btn-xs btn-outline-warning mr-1" onclick="openEditEmpleadoModal(${f.empleado_id})" title="Editar" aria-label="Editar: ${nombreEmp}"><i class="fas fa-edit"></i></button>
            <button class="btn btn-xs btn-outline-info mr-1" onclick="_imprimirExpedienteRrhh(${f.empleado_id})" title="Imprimir expediente" aria-label="Imprimir expediente de ${nombreEmp}"><i class="fas fa-print"></i></button>
            <span class="border-left pl-1 ml-1">
              <button class="btn btn-xs btn-outline-danger" onclick="handleDeleteEmpleado(${f.empleado_id},${JSON.stringify(f.empleado||'')})" title="Eliminar" aria-label="Eliminar: ${nombreEmp}"><i class="fas fa-trash"></i></button>
            </span>
          </td>
        </tr>
      `;
    }).join("");
  }
}

// OR-162: "Imprimir expediente" también disponible desde la fila del monitor,
// no sólo tecleando la URL a mano.
function _imprimirExpedienteRrhh(empleadoId) {
  if (!empleadoId) { showToast("No se pudo determinar el empleado.", "error"); return; }
  window.open(`${API_BASE}/api/rrhh/report/${empleadoId}`, "_blank", "noopener,noreferrer");
}

// OA-121: "Ver" mostraba sólo los nueve campos que ya estaban en la caché de
// la tabla. Se pide el documento completo al servidor (GET /documento/{id}, ya
// existe) y si falla se recurre a la fila en caché para no dejar el botón
// muerto.
async function openAdminDocById(id) {
  const cached = (state.adminTable.results || []).find(r => r.id == id);
  if (!cached) return;
  try {
    const full = await apiFetchJSON(`${API_BASE}/api/admin/documento/${id}?modulo=${encodeURIComponent(state.user.modulo)}`);
    openDocModalWithRecord({
      titulo:               full.titulo             ?? cached.titulo          ?? "",
      autor:                full.autor               ?? cached.autor           ?? "",
      fecha:                full.fecha               ?? cached.fecha           ?? "",
      doc_type:             full.doc_type            ?? cached.doc_type        ?? "",
      ubicacion:            full.ubicacion           ?? cached.ubicacion       ?? "",
      resumen:              full.resumen             ?? cached.resumen         ?? "",
      file_url:             full.file_url            ?? cached.file_url        ?? "",
      tesauro_secundario:   full.tesauro_secundario  ?? cached.tesauro_secundario ?? "",
      tesauro_badges:       [full.doc_type ?? cached.doc_type, full.tesauro_secundario ?? cached.tesauro_secundario].filter(Boolean),
      status:               full.status              ?? cached.status          ?? "",
      numero_folio:         full.numero_folio         ?? "",
      soporte:              full.soporte              ?? "",
      numero_paginas:       full.numero_paginas        ?? "",
      idioma:               full.idioma                ?? "",
      fecha_vencimiento:    full.fecha_vencimiento     ?? "",
      personas_relacionadas: full.personas_relacionadas ?? "",
      updated_at:           full.updated_at            ?? "",
      updated_by:           full.updated_by            ?? "",
    });
  } catch (e) {
    console.error("No se pudo obtener el documento completo, se muestra la versión en caché:", e);
    openDocModalWithRecord({
      titulo:           cached.titulo          || "",
      autor:            cached.autor           || "",
      fecha:            cached.fecha           || "",
      doc_type:         cached.doc_type        || "",
      ubicacion:        cached.ubicacion       || "",
      resumen:          cached.resumen         || "",
      file_url:         cached.file_url        || "",
      tesauro_secundario: cached.tesauro_secundario || "",
      tesauro_badges:   [cached.doc_type, cached.tesauro_secundario].filter(Boolean),
    });
  }
}

// OA-122 / OR-137: exportar sólo lo que estaba en pantalla (25 filas) es una
// trampa cuando el nombre del archivo y el toast dicen "exportado" sin más.
// Se recorren todas las páginas del resultado filtrado antes de armar el CSV.
async function _fetchAllFilteredRecords(suf) {
  const mod        = state.user.modulo;
  const q          = document.getElementById(`admin_search-${suf}`)?.value          || "";
  const type       = document.getElementById(`admin_filter_type-${suf}`)?.value     || "";
  const person     = document.getElementById(`admin_filter_person-${suf}`)?.value   || "";
  const statusFilt = document.getElementById(`admin_filter_status-${suf}`)?.value   || "";
  const perPage = 200;
  let page = 1;
  let all = [];
  let total = Infinity;
  while (all.length < total && page <= 200) { // corte de seguridad: 40.000 registros
    const url = `${API_BASE}/api/admin/list_all?modulo=${mod}&search=${encodeURIComponent(q)}&type_filter=${encodeURIComponent(type)}&person_filter=${encodeURIComponent(person)}&status_filter=${encodeURIComponent(statusFilt)}&page=${page}&per_page=${perPage}`;
    const data = await apiFetchJSON(url);
    all = all.concat(data.records || []);
    total = data.total || 0;
    if (!data.records || data.records.length < perPage) break;
    page += 1;
  }
  return { records: all, total };
}

async function exportAdminCSV() {
  const suf = adminSuffixFromTab();
  const isArch = isArchivoModule();
  const btn = document.getElementById(`admin_export_csv-${suf}`);
  if (btn) { btn.disabled = true; btn.dataset.dsOrigText = btn.dataset.dsOrigText || btn.innerHTML; btn.innerHTML = `<i class="fas fa-spinner fa-spin mr-1"></i>Exportando…`; }

  let records;
  try {
    if ((state.adminTable.total || 0) > (state.adminTable.results || []).length) {
      showToast("Exportando el conjunto filtrado completo, puede tardar unos segundos…", "info");
    }
    const full = await _fetchAllFilteredRecords(suf);
    records = full.records;
  } catch (e) {
    showToast(e.message || "No se pudo exportar el conjunto completo; se exporta la página visible.", "warning");
    records = state.adminTable.results || [];
  } finally {
    if (btn) { btn.disabled = false; btn.innerHTML = btn.dataset.dsOrigText; }
  }

  if (!records || records.length === 0) { showToast("No hay datos para exportar.", "warning"); return; }

  const esc = v => `"${String(v ?? "").replace(/"/g, '""')}"`;
  const today = new Date().toISOString().slice(0, 10);

  let headers, rows;
  if (isArch) {
    // Incluye campos ISAD(G): folio, soporte, páginas
    headers = ["ID", "Título", "Autor", "Fecha", "Tipología", "Clasificación",
               "N° Folio", "Soporte", "N° Páginas", "Ubicación", "Archivo Digital", "Estado", "Resumen"];
    rows = records.map(r => [
      r.id, r.titulo, r.autor, r.fecha, r.doc_type, r.tesauro_secundario || "",
      r.numero_folio || "", r.soporte || "Físico", r.numero_paginas || "",
      r.ubicacion, r.file_url || "", r.status || "aprobado", r.resumen || ""
    ].map(esc).join(","));
  } else {
    headers = ["ID Empleado", "Apellidos y Nombres", "Cédula", "RIF", "Cargo", "Departamento",
               "Estado Laboral", "Fecha Ingreso", "Fecha Nacimiento", "Nivel Educativo", "Sexo",
               "N° Documentos", "Última Actualiz."];
    const SEXO = { M: "Masculino", F: "Femenino", O: "Otro" };
    rows = records.map(r => [
      r.empleado_id,
      r.empleado,
      r.cedula,
      r.rif || "",
      r.cargo || "",
      r.departamento || "",
      r.estado || "",
      r.fecha_ingreso || "",
      r.fecha_nacimiento || "",
      r.nivel_educativo || "",
      SEXO[r.sexo] || r.sexo || "",
      r.doc_count ?? "",
      r.updated_at ? r.updated_at.slice(0, 10) : ""
    ].map(esc).join(","));
  }

  // OR-139: la cabecera va primera, sin una fila de metadatos delante que
  // rompa la lectura en Excel/pandas/Power BI. Los metadatos (fecha, módulo,
  // filtros) quedan en el nombre del archivo y en el aviso.
  // OR-138: BOM delante para que Excel en Windows no destroce los acentos.
  const csv  = "﻿" + [headers.join(","), ...rows].join("\n");
  const blob = new Blob([csv], { type: "text/csv;charset=utf-8;" });
  const url  = URL.createObjectURL(blob);
  const a    = document.createElement("a");
  a.href = url;
  a.download = `ciencias_ucv_${isArch ? "archivo" : "rrhh"}_${today}${state.adminTable.filtersActive ? "_filtrado" : ""}.csv`;
  a.click();
  URL.revokeObjectURL(url);
  showToast(`CSV exportado: ${records.length} registro(s) de ${state.adminTable.total ?? records.length}.`, "success");
}

// ─── Drag & Drop en zona de carga ───────────────────────────────────────────
// OA-094 / OR-098: antes sólo se admitía `files[0]` en silencio y no había
// forma de ver o quitar lo elegido salvo recargar la pestaña. Ahora se ve la
// lista completa de lo soltado (con nombre y tamaño), se puede quitar
// cualquiera, y se avisa si se descarta algo — aunque el envío en sí sigue
// mandando un solo archivo principal: adjuntar varios anexos en un mismo alta
// requiere cambios en admin-submit.js y en el backend, fuera de este carril
// (ver _BUZON.md).
function initDropZone(suf) {
  const zone = document.getElementById(`dropzone-${suf}`) ||
               document.querySelector(`#pane-admin-${suf}-new .ds-dropzone-compact`);
  const fileInput = document.getElementById(`file_upload-${suf}`);
  if (!zone || !fileInput) return;

  zone._dsFiles = zone._dsFiles || [];

  let listEl = zone.parentElement?.querySelector(`.ds-dropzone-filelist[data-suf="${suf}"]`);
  if (!listEl) {
    listEl = document.createElement("div");
    listEl.className = "ds-dropzone-filelist small mt-2";
    listEl.dataset.suf = suf;
    zone.insertAdjacentElement("afterend", listEl);
  }

  const formatSize = n => n < 1024 ? `${n} B` : n < 1024 * 1024 ? `${(n / 1024).toFixed(1)} KB` : `${(n / (1024 * 1024)).toFixed(1)} MB`;

  const updateLabel = () => {
    const primary = zone._dsFiles[0];
    const label = zone.querySelector(".ds-drop-label");
    if (label) label.textContent = primary ? `Archivo: ${primary.name}` : "Arrastra el archivo digital aquí o";
    zone.classList.toggle("has-file", !!primary);
    const icon = zone.querySelector("i");
    if (icon) {
      icon.classList.toggle("fa-file-upload", !primary);
      icon.classList.toggle("fa-check-circle", !!primary);
      icon.classList.toggle("text-secondary", !primary);
      icon.classList.toggle("text-success", !!primary);
    }
    if (primary) {
      const dt = new DataTransfer();
      dt.items.add(primary);
      fileInput.files = dt.files;
    } else {
      fileInput.value = "";
    }
    listEl.innerHTML = zone._dsFiles.length === 0 ? "" : zone._dsFiles.map((f, i) => `
      <div class="d-flex align-items-center justify-content-between border rounded px-2 py-1 mb-1">
        <span class="text-truncate" style="max-width:70%;">
          ${i === 0 ? '<i class="fas fa-check-circle text-success mr-1" title="Se enviará este archivo"></i>' : '<i class="fas fa-paperclip text-muted mr-1" title="No se envía: sólo se admite un archivo principal por ahora"></i>'}
          ${escHtml(f.name)}
        </span>
        <span class="text-muted mr-2">${formatSize(f.size)}</span>
        <button type="button" class="btn btn-xs btn-outline-danger" aria-label="Quitar ${escHtml(f.name)}" onclick="_dsDropzoneRemove('${suf}', ${i})"><i class="fas fa-times"></i></button>
      </div>`).join("");
  };

  window._dsDropzoneRemove = (s, idx) => {
    const z = document.getElementById(`dropzone-${s}`) || document.querySelector(`#pane-admin-${s}-new .ds-dropzone-compact`);
    if (!z || !z._dsFiles) return;
    z._dsFiles.splice(idx, 1);
    z._dsUpdateLabel?.();
  };
  zone._dsUpdateLabel = updateLabel;

  const addFiles = (fileList) => {
    const incoming = Array.from(fileList || []);
    if (incoming.length === 0) return;
    zone._dsFiles = zone._dsFiles.concat(incoming);
    if (incoming.length > 1 || zone._dsFiles.length > 1) {
      showToast(`${incoming.length > 1 ? incoming.length + " archivos añadidos" : "Archivo añadido"}. Sólo "${zone._dsFiles[0].name}" se enviará como archivo principal; el resto queda en la lista para revisarlo o quitarlo.`, "info");
    } else {
      showToast(`Archivo listo: ${zone._dsFiles[0].name}`, "info");
    }
    updateLabel();
  };

  zone.addEventListener("dragover", e => { e.preventDefault(); zone.classList.add("drag-over"); });
  zone.addEventListener("dragleave", () => zone.classList.remove("drag-over"));
  zone.addEventListener("drop", e => {
    e.preventDefault();
    zone.classList.remove("drag-over");
    addFiles(e.dataTransfer?.files);
  });
  fileInput.addEventListener("change", () => {
    if (fileInput.files?.length) {
      zone._dsFiles = Array.from(fileInput.files);
      updateLabel();
    }
  });
}


// ─── Compartición externa ────────────────────────────────────────────────────
// Genera un enlace firmado y con caducidad para enseñarle un documento a
// alguien de fuera sin crearle un usuario. El enlace se copia al portapapeles;
// si el navegador no lo permite, se muestra para copiarlo a mano.
async function compartirDocumento(docId) {
  const modulo = isArchivoModule() ? "Archivo" : "RRHH";
  const horas  = 72;
  try {
    const r = await apiFetchJSON(
      `${API_BASE}/api/admin/compartir?modulo=${modulo}&doc_id=${docId}` +
      `&horas=${horas}&usuario=${encodeURIComponent(state.user?.username || "")}`,
      { method: "POST" });
    const url = `${window.location.origin}${r.url}`;
    try {
      await navigator.clipboard.writeText(url);
      showToast(`Enlace copiado. Caduca en ${horas} horas.`, "success");
    } catch {
      // Sin permiso de portapapeles (o sin HTTPS): mostrarlo para copiar a mano.
      // OA-015: `linkModal` (admin-ui.js) ya trae `<input readonly>` real y un
      // botón «Copiar»; antes esto pasaba marcado HTML crudo a `confirmModal()`,
      // que lo insertaba con `textContent` y el usuario veía las etiquetas
      // literales. Ver nota de B6-admin-ui en _BUZON.md.
      if (typeof linkModal === "function") {
        linkModal("Enlace de consulta", `Caduca en ${horas} horas.`, url);
      } else {
        confirmModal(
          "Enlace de consulta",
          `<p class="small text-muted mb-2">Caduca en ${horas} horas.</p>` +
          `<input class="form-control form-control-sm" readonly onclick="this.select()" value="${escHtml(url)}">`,
          "Cerrar", "btn-secondary");
      }
    }
  } catch (e) {
    showToast(e.message || "No se pudo generar el enlace.", "error");
  }
}
