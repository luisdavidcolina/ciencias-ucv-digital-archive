// ==========================================================================
// TOAST SYSTEM
// ==========================================================================
function showToast(message, type) {
  type = type || "info";
  const container = document.getElementById("ds-toast-container");
  if (!container) return;

  const colors = {
    success: { bg: "#d4edda", color: "#155724", border: "#c3e6cb", icon: "fas fa-check-circle" },
    error:   { bg: "#f8d7da", color: "#721c24", border: "#f5c6cb", icon: "fas fa-times-circle" },
    warning: { bg: "#fff3cd", color: "#856404", border: "#ffeeba", icon: "fas fa-exclamation-triangle" },
    info:    { bg: "#d1ecf1", color: "#0c5460", border: "#bee5eb", icon: "fas fa-info-circle" },
  };
  const cfg = colors[type] || colors.info;

  const toast = document.createElement("div");
  toast.style.cssText = `background:${cfg.bg};color:${cfg.color};border:1px solid ${cfg.border};border-radius:6px;padding:10px 14px;margin-bottom:8px;min-width:260px;display:flex;align-items:center;box-shadow:0 3px 10px rgba(0,0,0,.15);font-size:0.87rem;transition:opacity 0.4s;`;
  toast.innerHTML = `<i class="${cfg.icon}" style="margin-right:8px;font-size:1rem;"></i><span>${message}</span>`;
  container.appendChild(toast);

  setTimeout(() => {
    toast.style.opacity = "0";
    setTimeout(() => toast.remove(), 400);
  }, 3500);
}

// ==========================================================================
// PANEL DE CONTROL ADMINISTRATIVO
// ==========================================================================
function loadAdminTab(adminTabId) {
  state.activeAdminTab = adminTabId;
  const suf  = adminSuffixFromTab();
  const root = `#tab-admin-${suf}`;

  document.querySelectorAll(`#admin_workspace_tabs-${suf} .nav-link`).forEach(l => l.classList.remove("active"));
  document.getElementById(`tab-admin-${suf}-${adminTabId}`)?.classList.add("active");

  document.querySelectorAll(`${root} .tab-pane`).forEach(p => p.classList.remove("show", "active"));
  document.getElementById(`pane-admin-${suf}-${adminTabId}`)?.classList.add("show", "active");

  if      (adminTabId === "stats")      loadDynamicStats();
  else if (adminTabId === "new")        { renderDynamicSubmitFields(); loadRecentSubmissions(); }
  else if (adminTabId === "monitor")    { state.adminTable.page = 1; loadMonitorTable(); }
  else if (adminTabId === "categories") loadCategoriesTab();
  else if (adminTabId === "users")      loadUsersTab();
  else if (adminTabId === "audit")      loadAuditTab();

  try {
    const mod = state.user?.modulo || "Archivo";
    const bc = document.querySelector(`${root} .ds-breadcrumb`);
    if (bc) bc.innerHTML = `<i class="fas fa-shield-alt"></i> Panel de Control / Administración - ${mod}`;
    const submitBtn = document.getElementById(`btn_submit_workspace-${suf}`);
    if (submitBtn) submitBtn.innerHTML = `<i class="fas fa-cloud-upload-alt"></i> Guardar en ${mod}`;
    const monitorTitle = document.querySelector(`${root} .card-title`);
    if (monitorTitle) monitorTitle.innerHTML = `<i class="fas fa-database"></i> Monitor de ${mod === "RRHH" ? "RRHH" : "Archivos"}`;
  } catch (e) {
    console.error("Error actualizando etiquetas del panel:", e);
  }
}

// --- ESTADÍSTICAS ---
async function loadDynamicStats() {
  const suf = adminSuffixFromTab();
  try {
    const res = await fetch(`${API_BASE}/api/admin/stats`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        modulo:     state.user.modulo,
        date_start: document.getElementById(`stats-date-start-${suf}`)?.value || "",
        date_end:   document.getElementById(`stats-date-end-${suf}`)?.value   || ""
      })
    });
    if (!res.ok) throw new Error();
    const stats = await res.json();

    const kpiDocs = document.getElementById(`kpi-total-docs-${suf}`);
    const kpiCats = document.getElementById(`kpi-total-cats-${suf}`);
    if (kpiDocs) kpiDocs.innerText = stats.total_docs;
    if (kpiCats) kpiCats.innerText = stats.categories_count;

    const resUsers = await fetch(`${API_BASE}/api/admin/users`);
    if (resUsers.ok) {
      const uList = await resUsers.json();
      const kpiUsers = document.getElementById(`kpi-total-users-${suf}`);
      if (kpiUsers) kpiUsers.innerText = uList.filter(u => u.modulo === state.user.modulo).length;
    }

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

    // Distribución por tipo
    const typeContainer = document.getElementById(`stats_by_type-${suf}`);
    if (typeContainer) {
      if (stats.by_type.length === 0) {
        typeContainer.innerHTML = `<div class="text-muted">Sin datos.</div>`;
      } else {
        const colors = ["#2b4e72","#0056b3","#28a745","#ffc107","#dc3545","#6f42c1"];
        typeContainer.innerHTML = `<div class="w-100" style="padding:10px;">
          ${stats.by_type.map((t, i) => `
            <div class="mb-3">
              <div class="d-flex justify-content-between" style="font-size:0.85rem;margin-bottom:2px;">
                <span class="font-weight-bold text-dark">${t.type}</span>
                <span class="text-primary">${t.count} (${t.pct}%)</span>
              </div>
              <div class="progress" style="height:10px;border-radius:5px;background-color:#e9ecef;">
                <div class="progress-bar" style="width:${t.pct}%;background-color:${colors[i % colors.length]};border-radius:5px;"></div>
              </div>
            </div>
          `).join("")}
        </div>`;
      }
    }

    // Línea de tiempo
    const timelineContainer = document.getElementById(`stats_timeline-${suf}`);
    if (timelineContainer) {
      if (stats.timeline.length === 0) {
        timelineContainer.innerHTML = `<div class="text-muted">Sin datos de línea de tiempo.</div>`;
      } else {
        timelineContainer.innerHTML = `<div class="w-100" style="padding:15px 10px;">
          ${stats.timeline.map(y => `
            <div class="d-flex align-items-center mb-3">
              <div style="width:60px;text-align:right;margin-right:15px;font-size:0.85rem;font-weight:bold;">${y.year}</div>
              <div style="flex-grow:1;">
                <div class="progress" style="height:22px;border-radius:11px;background-color:#e9ecef;">
                  <div class="progress-bar d-flex align-items-center justify-content-center"
                    style="width:${y.pct_width}%;background:linear-gradient(135deg,#2b4e72,#0056b3);border-radius:11px;font-size:0.78rem;font-weight:bold;color:white;">
                    ${y.count} docs
                  </div>
                </div>
              </div>
            </div>
          `).join("")}
        </div>`;
      }
    }

    // Estado del sistema
    const sysContainer = document.getElementById(`stats_system-${suf}`);
    if (sysContainer) {
      sysContainer.innerHTML = `
        <div class="col-md-3 text-center p-3 border-right"><i class="fas fa-server fa-2x text-primary mb-2"></i><h5 class="font-weight-bold m-0">${stats.system.status}</h5><span class="text-muted" style="font-size:0.8rem;">Estado Global</span></div>
        <div class="col-md-3 text-center p-3 border-right"><i class="fas fa-memory fa-2x text-success mb-2"></i><h5 class="font-weight-bold m-0">${stats.system.ram}</h5><span class="text-muted" style="font-size:0.8rem;">Consumo RAM</span></div>
        <div class="col-md-3 text-center p-3 border-right"><i class="fas fa-microchip fa-2x text-warning mb-2"></i><h5 class="font-weight-bold m-0">${stats.system.cpu} Núcleos</h5><span class="text-muted" style="font-size:0.8rem;">Procesamiento</span></div>
        <div class="col-md-3 text-center p-3"><i class="fas fa-laptop-code fa-2x text-danger mb-2"></i><h5 class="font-weight-bold m-0" style="font-size:0.95rem;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;">${stats.system.os}</h5><span class="text-muted" style="font-size:0.8rem;">Plataforma</span></div>
      `;
    }

    // Cargar gráficas Chart.js
    loadChartsData();

  } catch (e) {
    console.error("Error al cargar analíticas dinámicas:", e);
  }
}

// --- NUEVO INGRESO ---
function renderDynamicSubmitFields() {
  const suf       = adminSuffixFromTab();
  const container = document.getElementById(`dynamic-submit-fields-${suf}`);
  if (!container) return;

  if (isArchivoModule()) {
    container.innerHTML = `
      <div class="row">
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">Título del Documento *</label>
          <input type="text" id="reg-title-${suf}" class="form-control" placeholder="Ej: Plan Regulador de Áreas Verdes" required>
        </div>
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">Autor / Ente Emisor *</label>
          <input type="text" id="reg-author-${suf}" class="form-control" placeholder="Ej: Arq. Villanueva" required>
        </div>
      </div>
      <div class="row mt-2">
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Tipo de Documento *</label>
          <select id="reg-doc-type-${suf}" class="form-control" required>
            ${(state.choices?.archivo?.doc_types || ["Proyecto de Investigación","Informe","Plano Arquitectónico","Acta de Sesión","Resolución","Reglamento"]).map(t => `<option value="${t}">${t}</option>`).join("")}
          </select>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Clasificación *</label>
          <select id="reg-secundario-${suf}" class="form-control" required>
            <option value="Parte I">Parte I</option>
            <option value="Parte II">Parte II</option>
            <option value="Parte III">Parte III</option>
            <option value="Parte IV">Parte IV</option>
          </select>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Fecha de Emisión *</label>
          <input type="date" id="reg-fecha-${suf}" class="form-control" required value="${new Date().toISOString().substring(0,10)}">
        </div>
      </div>
      <div class="form-group mt-2">
        <label class="font-weight-bold text-muted">Palabras Clave (separadas por coma) *</label>
        <input type="text" id="reg-descriptores-${suf}" class="form-control" placeholder="Ej: Planificación académica, Gestión institucional" required>
      </div>
      <div class="form-group mt-2">
        <label class="font-weight-bold text-muted">Resumen Descriptivo (Abstract)</label>
        <textarea id="reg-resumen-${suf}" class="form-control" rows="3" placeholder="Breve síntesis o abstract del documento..."></textarea>
      </div>
      <div class="form-group mt-2">
        <label class="font-weight-bold text-muted">Ubicación Física (Estante, Gaveta o Caja) *</label>
        <input type="text" id="reg-location-${suf}" class="form-control" placeholder="Ej: Mapoteca - Gaveta 1 o Digitalizado Exclusivo" required>
      </div>
    `;
  } else {
    container.innerHTML = `
      <div class="row">
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Nombres *</label>
          <input type="text" id="reg-nombres-${suf}" class="form-control" placeholder="Ej: Susana María" required>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Apellidos *</label>
          <input type="text" id="reg-apellidos-${suf}" class="form-control" placeholder="Ej: Pérez González" required>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Cédula de Identidad *</label>
          <input type="text" id="reg-cedula-${suf}" class="form-control" placeholder="Ej: V-12345678" required>
        </div>
      </div>
      <div class="row mt-2">
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">C.I.F. (RIF)</label>
          <input type="text" id="reg-rif-${suf}" class="form-control" placeholder="Ej: J-12345678-0">
        </div>
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">Cargo Asignado</label>
          <input type="text" id="reg-cargo-${suf}" class="form-control" placeholder="Ej: Analista Contable">
        </div>
      </div>
      <div class="form-group mt-2">
        <label class="font-weight-bold text-muted">Personas / Dependencias Relacionadas</label>
        <input type="text" id="reg-personas-${suf}" class="form-control" placeholder="Ej: Susana Pérez; Dirección RRHH">
      </div>
      <div class="row mt-2">
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Tipo de Documento *</label>
          <select id="reg-doc-type-${suf}" class="form-control" required>
            ${(function() {
              const tiposPP = state.choices?.rrhh?.tipos_por_parte || {};
              if (Object.keys(tiposPP).length > 0) {
                return Object.entries(tiposPP).map(([parte, tipos]) =>
                  `<optgroup label="${parte}">${tipos.map(t => `<option value="${t}">${t}</option>`).join("")}</optgroup>`
                ).join("");
              }
              return (state.choices?.rrhh?.doc_types || ["Hoja de Vida","Contrato"]).map(t => `<option value="${t}">${t}</option>`).join("");
            })()}
          </select>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Departamento *</label>
          <input type="text" id="reg-depto-${suf}" class="form-control" placeholder="Ej: Biología" required>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Estado *</label>
          <select id="reg-estado-${suf}" class="form-control" required>
            <option value="Activo">Activo</option>
            <option value="Retirado">Retirado</option>
            <option value="Jubilado">Jubilado</option>
            <option value="Pensionado">Pensionado</option>
          </select>
        </div>
      </div>
      <div class="row mt-2">
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">Fecha de Ingreso *</label>
          <input type="date" id="reg-fecha-${suf}" class="form-control" required value="${new Date().toISOString().substring(0,10)}">
        </div>
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">Retención Física *</label>
          <input type="text" id="reg-location-${suf}" class="form-control" placeholder="Ej: Archivo Central - Caja J-02" required>
        </div>
      </div>
      <div class="row mt-2">
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Fecha de Jubilación (Opcional)</label>
          <input type="date" id="reg-jubilacion-${suf}" class="form-control">
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Fecha de Pensión (Opcional)</label>
          <input type="date" id="reg-pension-${suf}" class="form-control">
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">URL Foto Avatar (Opcional)</label>
          <input type="text" id="reg-foto-${suf}" class="form-control" placeholder="https://...">
        </div>
      </div>
    `;
  }
}

function loadRecentSubmissions() {
  const suf         = adminSuffixFromTab();
  const containerEl = document.getElementById(`recent_submissions-${suf}`);
  if (!containerEl) return;
  const isArchivo = isArchivoModule();
  const list      = isArchivo ? state.archivo.results : state.rrhh.results;

  if (list.length === 0) {
    containerEl.innerHTML = `<li class="text-muted text-center p-2">Sin ingresos previos.</li>`;
    return;
  }

  const sorted = [...list].sort((a, b) => {
    const fA = isArchivo ? a.fecha : a.fecha_ingreso;
    const fB = isArchivo ? b.fecha : b.fecha_ingreso;
    return (fB || "").localeCompare(fA || "");
  }).slice(0, 3);

  containerEl.innerHTML = sorted.map(item => {
    const title = isArchivo ? item.titulo : item.empleado;
    return `
      <li class="d-flex align-items-center mb-2 p-2 border rounded bg-light">
        <i class="fas fa-file-alt mr-2 text-primary" style="font-size:1.15rem;"></i>
        <div style="overflow:hidden;text-overflow:ellipsis;white-space:nowrap;flex-grow:1;">
          <strong style="font-size:0.82rem;" class="text-dark">${title}</strong><br>
          <span class="text-muted" style="font-size:0.72rem;">${item.doc_type}</span>
        </div>
      </li>
    `;
  }).join("");
}

async function handleNewSubmission(e) {
  e.preventDefault();
  const suf       = adminSuffixFromTab();
  const isArchivo = isArchivoModule();

  function val(id) { return (document.getElementById(id)?.value || "").trim(); }

  // Validación en el borde del sistema (entrada del usuario)
  if (isArchivo) {
    if (!val(`reg-title-${suf}`))    { showToast("El título del documento es requerido.", "warning"); return; }
    if (!val(`reg-doc-type-${suf}`)) { showToast("Selecciona el tipo de documento.", "warning"); return; }
    if (!val(`reg-location-${suf}`)) { showToast("La ubicación física es requerida.", "warning"); return; }
  } else {
    if (!val(`reg-nombres-${suf}`))  { showToast("El nombre del empleado es requerido.", "warning"); return; }
    if (!val(`reg-cedula-${suf}`))   { showToast("La cédula del empleado es requerida.", "warning"); return; }
    if (!val(`reg-location-${suf}`)) { showToast("La retención física es requerida.", "warning"); return; }
  }

  const submitBtn = document.getElementById(`btn_submit_workspace-${suf}`);
  const origLabel = submitBtn?.innerHTML;
  if (submitBtn) { submitBtn.disabled = true; submitBtn.innerHTML = '<i class="fas fa-spinner fa-spin mr-1"></i> Guardando...'; }

  const payload = {
    modulo:    state.user.modulo,
    usuario:   state.user.username,
    doc_type:  val(`reg-doc-type-${suf}`),
    fecha:     val(`reg-fecha-${suf}`),
    ubicacion: val(`reg-location-${suf}`)
  };

  if (isArchivo) {
    payload.titulo              = val(`reg-title-${suf}`);
    payload.autor               = val(`reg-author-${suf}`);
    payload.resumen             = val(`reg-resumen-${suf}`);
    payload.tesauro_secundario  = val(`reg-secundario-${suf}`);
    payload.descriptores_libres = val(`reg-descriptores-${suf}`);
  } else {
    const nombres   = val(`reg-nombres-${suf}`);
    const apellidos = val(`reg-apellidos-${suf}`);
    payload.nombres               = nombres;
    payload.apellidos             = apellidos;
    payload.empleado              = `${nombres} ${apellidos}`.trim();
    payload.cedula                = val(`reg-cedula-${suf}`);
    payload.personas_relacionadas = val(`reg-personas-${suf}`);
    payload.departamento          = val(`reg-depto-${suf}`);
    payload.estado                = val(`reg-estado-${suf}`);
    payload.rif                   = val(`reg-rif-${suf}`);
    payload.cargo                 = val(`reg-cargo-${suf}`);
    payload.fecha_jubilacion      = val(`reg-jubilacion-${suf}`);
    payload.fecha_pension         = val(`reg-pension-${suf}`);
    payload.foto_url              = val(`reg-foto-${suf}`);
  }

  try {
    const res = await fetch(`${API_BASE}/api/admin/submit`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    });
    if (!res.ok) {
      const errData = await res.json().catch(() => ({}));
      showToast(errData.detail || "Error al registrar el folio.", "error");
      return;
    }
    showToast("Ingreso guardado con éxito.", "success");
    const form = document.getElementById(`admin-submit-form-${suf}`);
    if (form) form.reset();
    renderDynamicSubmitFields();
    loadRecentSubmissions();
    loadDynamicChoices();
    if (isArchivo) triggerArchivoSearch(); else triggerRrhhSearch();
    loadAdminTab("stats");
  } catch {
    showToast("Error de conexión al registrar el folio.", "error");
  } finally {
    if (submitBtn) { submitBtn.disabled = false; submitBtn.innerHTML = origLabel; }
  }
}

// --- MONITOR ---
async function loadMonitorTable() {
  const mod        = state.user.modulo;
  const suf        = adminSuffixFromTab();
  const q          = document.getElementById(`admin_search-${suf}`)?.value          || "";
  const type       = document.getElementById(`admin_filter_type-${suf}`)?.value     || "";
  const person     = document.getElementById(`admin_filter_person-${suf}`)?.value   || "";
  const statusFilt = document.getElementById(`admin_filter_status-${suf}`)?.value   || "";
  const page    = state.adminTable.page    || 1;
  const perPage = state.adminTable.perPage || 25;

  if (typeof showTableSkeleton === "function") {
    showTableSkeleton(`admin_control_table-${suf}`, isArchivoModule() ? 6 : 7, 6);
  }

  try {
    const url = `${API_BASE}/api/admin/list_all?modulo=${mod}&search=${encodeURIComponent(q)}&type_filter=${encodeURIComponent(type)}&person_filter=${encodeURIComponent(person)}&status_filter=${encodeURIComponent(statusFilt)}&page=${page}&per_page=${perPage}`;
    const res = await fetch(url);
    if (!res.ok) throw new Error();
    const data = await res.json();

    state.adminTable.results = data.records;
    state.adminTable.total   = data.total;

    // Poblar filtro de tipología
    const typeSelector = document.getElementById(`admin_filter_type-${suf}`);
    if (typeSelector && typeSelector.options.length <= 1 && state.choices) {
      const types = isArchivoModule() ? state.choices.archivo.doc_types : state.choices.rrhh.doc_types;
      typeSelector.innerHTML = `<option value="">Filtrar por Tipología...</option>` +
        types.map(t => `<option value="${t}">${t}</option>`).join("");
    }

    // Poblar filtro de persona (solo primera carga)
    const personSelector = document.getElementById(`admin_filter_person-${suf}`);
    if (personSelector && personSelector.options.length <= 1) {
      let people = [];
      if (isArchivoModule()) {
        people = [...new Set((data.records || []).map(r => r.autor).filter(Boolean))].sort();
      } else {
        people = state.choices?.rrhh?.people || [];
      }
      personSelector.innerHTML = `<option value="">Filtrar por Persona...</option>` +
        people.map(p => `<option value="${p}">${p}</option>`).join("");
    }

    renderMonitorTable();
  } catch (e) {
    console.error("Error al cargar monitor:", e);
  }
}

function renderMonitorTable() {
  const suf       = adminSuffixFromTab();
  const records   = state.adminTable.results || [];
  const total     = state.adminTable.total   || records.length;
  const isArch    = isArchivoModule();
  const container = document.getElementById(`admin_control_table-${suf}`);
  const summaryEl = document.getElementById(`admin_table_summary-${suf}`);
  if (summaryEl) summaryEl.innerText = `Total: ${total} registros en el módulo ${state.user.modulo}`;

  const perPage    = state.adminTable.perPage || 25;
  const totalPages = Math.ceil(total / perPage) || 1;
  const pageInfo   = document.getElementById(`admin_page_info-${suf}`);
  if (pageInfo) pageInfo.innerText = `Pág ${state.adminTable.page} de ${totalPages}`;
  const prevBtnAdm = document.getElementById(`admin_prev-${suf}`);
  const nextBtnAdm = document.getElementById(`admin_next-${suf}`);
  if (prevBtnAdm) prevBtnAdm.disabled = state.adminTable.page <= 1;
  if (nextBtnAdm) nextBtnAdm.disabled = state.adminTable.page >= totalPages;

  if (records.length === 0) {
    container.innerHTML = `<tr><td colspan="${isArch ? 6 : 7}" class="text-muted text-center p-3">Ningún archivo coincide con los criterios de búsqueda.</td></tr>`;
    return;
  }

  const STATUS_BADGES = {
    draft:      '<span class="badge badge-secondary" title="Borrador">Draft</span>',
    revision:   '<span class="badge badge-warning text-dark" title="En revisión">Revisión</span>',
    aprobado:   '<span class="badge badge-success" title="Aprobado">✓</span>',
    rechazado:  '<span class="badge badge-danger" title="Rechazado">✗</span>',
  };

  const searchTerms = (document.getElementById(`admin_search-${suf}`)?.value || "").trim().split(/\s+/).filter(Boolean);

  if (isArch) {
    container.innerHTML = records.map(f => {
      const statusBadge = STATUS_BADGES[f.status] || STATUS_BADGES["aprobado"];
      const fileIcon = f.file_url
        ? `<a href="${f.file_url}" target="_blank" class="btn btn-xs btn-outline-info mr-1" title="Ver archivo"><i class="fas fa-file"></i></a>`
        : "";
      const titulo = typeof highlightTerms === "function"
        ? highlightTerms(f.titulo || "", searchTerms)
        : (f.titulo || "");
      const autor = typeof highlightTerms === "function"
        ? highlightTerms(f.autor || "—", searchTerms)
        : (f.autor || "—");
      const statusBtnTitle = { draft: "Borrador", revision: "En revisión", aprobado: "Aprobado", rechazado: "Rechazado" }[f.status] || "Aprobado";
      return `<tr>
        <td class="font-weight-bold text-dark" style="max-width:200px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap" title="${(f.titulo||'').replace(/"/g,'&quot;')}">${titulo}</td>
        <td class="text-muted small">${autor}</td>
        <td class="text-muted small">${formatISOToSpanish(f.fecha)}</td>
        <td><span class="badge badge-light border">${f.doc_type || '—'}</span></td>
        <td>
          <button class="btn btn-xs btn-link p-0 ds-status-btn" title="Cambiar estado: ${statusBtnTitle}"
            onclick="openQuickStatusMenu(this,${f.id},'${f.status || 'aprobado'}','${state.user.modulo}')">
            ${statusBadge}
          </button>
        </td>
        <td>
          ${fileIcon}
          <button class="btn btn-xs btn-outline-secondary mr-1" onclick="openAdminDocById(${f.id})" title="Ver"><i class="fas fa-eye"></i></button>
          <button class="btn btn-xs btn-outline-warning mr-1" onclick="openEditDocModal(${f.id})" title="Editar"><i class="fas fa-edit"></i></button>
          <button class="btn btn-xs btn-outline-danger" onclick="handleDeleteDoc(${f.id},'${(f.titulo||'').replace(/'/g,"\\'")}')" title="Eliminar"><i class="fas fa-trash"></i></button>
        </td>
      </tr>`;
    }).join("");
  } else {
    container.innerHTML = records.map(f => {
      const c = getStatusColor(f.estado);
      const hlEmpleado = typeof highlightTerms === "function" ? highlightTerms(f.empleado || "", searchTerms) : (f.empleado || "");
      return `
        <tr>
          <td class="font-weight-bold text-dark">${hlEmpleado}</td>
          <td>${f.cedula}</td>
          <td>${f.departamento}</td>
          <td><span class="badge" style="background-color:${c};color:white;padding:3px 6px;">${f.estado}</span></td>
          <td><span class="badge badge-secondary" style="padding:3px 6px;">${f.doc_type}</span></td>
          <td class="text-muted" style="font-size:0.82rem;">${f.ubicacion}</td>
          <td>
            <button class="btn btn-xs btn-outline-secondary mr-1" onclick="openRrhhPersonDossier('${f.empleado}')" title="Ver Expediente"><i class="fas fa-eye"></i></button>
            <button class="btn btn-xs btn-outline-warning mr-1" onclick="openEditEmpleadoModal(${f.empleado_id})" title="Editar"><i class="fas fa-edit"></i></button>
            <button class="btn btn-xs btn-outline-danger" onclick="handleDeleteEmpleado(${f.empleado_id},'${(f.empleado||'').replace(/'/g,"\\'")}')" title="Eliminar"><i class="fas fa-trash"></i></button>
          </td>
        </tr>
      `;
    }).join("");
  }
}

function openAdminDocById(id) {
  const rec = state.adminTable.results.find(r => r.id == id);
  if (!rec) return;
  openDocModalWithRecord({
    titulo:           rec.titulo          || "",
    autor:            rec.autor           || "",
    fecha:            rec.fecha           || "",
    doc_type:         rec.doc_type        || "",
    ubicacion:        rec.ubicacion       || "",
    resumen:          rec.resumen         || "",
    file_url:         rec.file_url        || "",
    tesauro_secundario: rec.tesauro_secundario || "",
    tesauro_badges:   [rec.doc_type, rec.tesauro_secundario].filter(Boolean),
  });
}

// --- EDITAR / ELIMINAR DOCUMENTO (ARCHIVO) ---
async function openEditDocModal(id) {
  // Fetch datos frescos del servidor (no depender solo de state cache)
  let rec = state.adminTable.results.find(r => r.id == id) || { id };
  try {
    const res = await fetch(`${API_BASE}/api/admin/documento/${id}?modulo=${encodeURIComponent(state.user.modulo)}`);
    if (res.ok) rec = await res.json();
  } catch { /* usa caché si falla el fetch */ }

  document.getElementById("edit-doc-id").value        = rec.id || "";
  document.getElementById("edit-doc-titulo").value    = rec.titulo || "";
  document.getElementById("edit-doc-autor").value     = rec.autor || "";
  document.getElementById("edit-doc-fecha").value     = rec.fecha || "";
  document.getElementById("edit-doc-secundario").value = rec.tesauro_secundario || "";
  document.getElementById("edit-doc-resumen").value   = rec.resumen || "";
  document.getElementById("edit-doc-ubicacion").value = rec.ubicacion || "";
  document.getElementById("edit-doc-palabras").value  = rec.palabras_clave || "";
  document.getElementById("edit-doc-file-url").value  = rec.file_url || "";

  // Preview de archivo si hay URL
  const previewContainer = document.getElementById("edit-doc-file-preview");
  if (previewContainer) {
    const url = rec.file_url || "";
    if (!url) {
      previewContainer.innerHTML = '<p class="text-muted small mb-0">Sin archivo adjunto.</p>';
    } else if (/\.(pdf)$/i.test(url)) {
      previewContainer.innerHTML = `<iframe src="${url}" style="width:100%;height:200px;border:1px solid #ddd;border-radius:4px;" title="Preview PDF"></iframe>`;
    } else if (/\.(png|jpe?g|gif|webp|svg)$/i.test(url)) {
      previewContainer.innerHTML = `<img src="${url}" style="max-width:100%;max-height:200px;border:1px solid #ddd;border-radius:4px;object-fit:contain;" alt="Preview">`;
    } else {
      previewContainer.innerHTML = `<a href="${url}" target="_blank" class="btn btn-sm btn-outline-secondary"><i class="fas fa-external-link-alt mr-1"></i>Abrir archivo</a>`;
    }
  }

  // Poblar select de tipos
  const sel = document.getElementById("edit-doc-type");
  if (sel) {
    const types = state.choices?.archivo?.doc_types || [];
    sel.innerHTML = types.map(t => `<option value="${t}"${t === rec.doc_type ? " selected" : ""}>${t}</option>`).join("");
    if (!types.includes(rec.doc_type) && rec.doc_type) {
      sel.innerHTML = `<option value="${rec.doc_type}" selected>${rec.doc_type}</option>` + sel.innerHTML;
    }
  }

  // Status
  const statusSel = document.getElementById("edit-doc-status");
  if (statusSel) statusSel.value = rec.status || "aprobado";

  $("#editArchivoModal").modal("show");
}

async function handleSaveEditDoc() {
  const id = document.getElementById("edit-doc-id")?.value;
  if (!id) return;

  const payload = {
    modulo:             state.user.modulo,
    id:                 parseInt(id),
    titulo:             document.getElementById("edit-doc-titulo")?.value || null,
    autor:              document.getElementById("edit-doc-autor")?.value || null,
    fecha:              document.getElementById("edit-doc-fecha")?.value || null,
    doc_type:           document.getElementById("edit-doc-type")?.value || null,
    tesauro_secundario: document.getElementById("edit-doc-secundario")?.value ?? null,
    palabras_clave:     document.getElementById("edit-doc-palabras")?.value || null,
    resumen:            document.getElementById("edit-doc-resumen")?.value || null,
    ubicacion:          document.getElementById("edit-doc-ubicacion")?.value || null,
    file_url:           document.getElementById("edit-doc-file-url")?.value.trim() || "",
    status:             document.getElementById("edit-doc-status")?.value || "aprobado",
    usuario:            state.user.username,
  };

  try {
    const res = await fetch(`${API_BASE}/api/admin/documento/${id}`, {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload),
    });
    if (!res.ok) throw new Error();
    $("#editArchivoModal").modal("hide");
    showToast("Documento actualizado.", "success");
    loadMonitorTable();
  } catch {
    showToast("Error al actualizar el documento.", "error");
  }
}

async function handleDeleteDoc(id, nombre) {
  const ok = await confirmModal(
    "Eliminar documento",
    `¿Eliminar "${nombre}"? Esta acción no se puede deshacer.`,
    "Sí, eliminar", "btn-danger"
  );
  if (!ok) return;
  try {
    const res = await fetch(`${API_BASE}/api/admin/documento/${id}?modulo=${encodeURIComponent(state.user.modulo)}&usuario=${encodeURIComponent(state.user.username)}`, {
      method: "DELETE",
    });
    if (!res.ok) throw new Error();
    showToast("Documento eliminado.", "success");
    state.adminTable.page = 1;
    loadMonitorTable();
  } catch {
    showToast("Error al eliminar el documento.", "error");
  }
}

// --- EDITAR / ELIMINAR EMPLEADO (RRHH) ---
function openEditEmpleadoModal(empId) {
  const rec = state.adminTable.results.find(r => r.empleado_id == empId);
  if (!rec) return;

  document.getElementById("edit-emp-id").value          = rec.empleado_id || "";
  // Split nombre completo into nombres/apellidos (best-effort)
  const partes = (rec.empleado || "").split(" ");
  document.getElementById("edit-emp-nombres").value     = partes.slice(0, Math.ceil(partes.length / 2)).join(" ");
  document.getElementById("edit-emp-apellidos").value   = partes.slice(Math.ceil(partes.length / 2)).join(" ");
  document.getElementById("edit-emp-cargo").value       = rec.cargo || "";
  document.getElementById("edit-emp-departamento").value = rec.departamento || "";
  const estadoSel = document.getElementById("edit-emp-estado");
  if (estadoSel) {
    Array.from(estadoSel.options).forEach(o => { o.selected = o.value === rec.estado; });
  }
  document.getElementById("edit-emp-rif").value         = "";
  document.getElementById("edit-emp-jubilacion").value  = "";
  document.getElementById("edit-emp-pension").value     = "";
  document.getElementById("edit-emp-foto").value        = "";

  $("#editEmpleadoModal").modal("show");
}

async function handleSaveEditEmpleado() {
  const empId = document.getElementById("edit-emp-id")?.value;
  if (!empId) return;

  const payload = {
    nombres:         document.getElementById("edit-emp-nombres")?.value || null,
    apellidos:       document.getElementById("edit-emp-apellidos")?.value || null,
    cargo:           document.getElementById("edit-emp-cargo")?.value || null,
    departamento:    document.getElementById("edit-emp-departamento")?.value || null,
    estado:          document.getElementById("edit-emp-estado")?.value || null,
    rif:             document.getElementById("edit-emp-rif")?.value || null,
    fecha_jubilacion: document.getElementById("edit-emp-jubilacion")?.value || null,
    fecha_pension:   document.getElementById("edit-emp-pension")?.value || null,
    foto_url:        document.getElementById("edit-emp-foto")?.value || null,
    usuario:         state.user.username,
  };

  try {
    const res = await fetch(`${API_BASE}/api/admin/empleado/${empId}`, {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload),
    });
    if (!res.ok) throw new Error();
    $("#editEmpleadoModal").modal("hide");
    showToast("Empleado actualizado.", "success");
    loadMonitorTable();
  } catch {
    showToast("Error al actualizar el empleado.", "error");
  }
}

async function handleDeleteEmpleado(empId, nombre) {
  const ok = await confirmModal(
    "Eliminar expediente",
    `¿Eliminar el expediente de "${nombre}"? Se eliminarán todos sus documentos. Esta acción no se puede deshacer.`,
    "Sí, eliminar", "btn-danger"
  );
  if (!ok) return;
  try {
    const res = await fetch(`${API_BASE}/api/admin/empleado/${empId}?usuario=${encodeURIComponent(state.user.username)}`, {
      method: "DELETE",
    });
    if (!res.ok) throw new Error();
    showToast("Expediente eliminado.", "success");
    loadMonitorTable();
  } catch {
    showToast("Error al eliminar el expediente.", "error");
  }
}

function exportAdminCSV() {
  const records = state.adminTable.results;
  if (!records || records.length === 0) { showToast("No hay datos para exportar.", "warning"); return; }
  const isArch = isArchivoModule();

  let headers, rows;
  if (isArch) {
    headers = ["ID", "Título", "Autor", "Fecha", "Tipología", "Ubicación", "Resumen"];
    rows = records.map(r => [r.id, r.titulo, r.autor, r.fecha, r.doc_type, r.ubicacion, r.resumen || ""].map(v => `"${String(v).replace(/"/g, '""')}"`).join(","));
  } else {
    headers = ["ID", "Empleado", "Cédula", "Departamento", "Estado", "Cargo", "Fecha Ingreso", "Tipología", "Ubicación"];
    rows = records.map(r => [r.empleado_id, r.empleado, r.cedula, r.departamento, r.estado, r.cargo, r.fecha_ingreso, r.doc_type, r.ubicacion].map(v => `"${String(v||"").replace(/"/g, '""')}"`).join(","));
  }

  const csv  = [headers.join(","), ...rows].join("\n");
  const blob = new Blob(["﻿" + csv], { type: "text/csv;charset=utf-8;" });
  const url  = URL.createObjectURL(blob);
  const a    = document.createElement("a");
  a.href = url;
  a.download = `admin_${isArch ? "archivo" : "rrhh"}_pag${state.adminTable.page}.csv`;
  a.click();
  URL.revokeObjectURL(url);
}