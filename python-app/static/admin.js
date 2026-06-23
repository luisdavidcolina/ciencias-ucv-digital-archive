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

  function val(id) { return document.getElementById(id)?.value || ""; }

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
    const nombres   = val(`reg-nombres-${suf}`).trim();
    const apellidos = val(`reg-apellidos-${suf}`).trim();
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
    if (!res.ok) throw new Error();
    showToast("Ingreso guardado con éxito.", "success");
    const form = document.getElementById(`admin-submit-form-${suf}`);
    if (form) form.reset();
    renderDynamicSubmitFields();
    loadRecentSubmissions();
    loadDynamicChoices();
    if (isArchivo) triggerArchivoSearch(); else triggerRrhhSearch();
    loadAdminTab("stats");
  } catch {
    showToast("Error al intentar registrar el nuevo folio.", "error");
  }
}

// --- MONITOR ---
async function loadMonitorTable() {
  const mod     = state.user.modulo;
  const suf     = adminSuffixFromTab();
  const q       = document.getElementById(`admin_search-${suf}`)?.value         || "";
  const type    = document.getElementById(`admin_filter_type-${suf}`)?.value    || "";
  const person  = document.getElementById(`admin_filter_person-${suf}`)?.value  || "";
  const page    = state.adminTable.page    || 1;
  const perPage = state.adminTable.perPage || 25;

  try {
    const url = `${API_BASE}/api/admin/list_all?modulo=${mod}&search=${encodeURIComponent(q)}&type_filter=${encodeURIComponent(type)}&person_filter=${encodeURIComponent(person)}&page=${page}&per_page=${perPage}`;
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

  if (isArch) {
    container.innerHTML = records.map(f => `
      <tr>
        <td class="font-weight-bold text-dark">${f.titulo}</td>
        <td>${f.autor}</td>
        <td>${formatISOToSpanish(f.fecha)}</td>
        <td><span class="ds-badge">${f.doc_type}</span></td>
        <td class="text-muted" style="font-size:0.82rem;">${f.ubicacion}</td>
        <td>
          <button class="btn btn-xs btn-outline-secondary mr-1" onclick="openAdminDocById(${f.id})" title="Ver"><i class="fas fa-eye"></i></button>
          <button class="btn btn-xs btn-outline-warning mr-1" onclick="openEditDocModal(${f.id})" title="Editar"><i class="fas fa-edit"></i></button>
          <button class="btn btn-xs btn-outline-danger" onclick="handleDeleteDoc(${f.id},'${(f.titulo||'').replace(/'/g,"\\'")}')" title="Eliminar"><i class="fas fa-trash"></i></button>
        </td>
      </tr>
    `).join("");
  } else {
    container.innerHTML = records.map(f => {
      const c = getStatusColor(f.estado);
      return `
        <tr>
          <td class="font-weight-bold text-dark">${f.empleado}</td>
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
function openEditDocModal(id) {
  const rec = state.adminTable.results.find(r => r.id == id);
  if (!rec) return;

  document.getElementById("edit-doc-id").value        = rec.id || "";
  document.getElementById("edit-doc-titulo").value    = rec.titulo || "";
  document.getElementById("edit-doc-autor").value     = rec.autor || "";
  document.getElementById("edit-doc-fecha").value     = rec.fecha || "";
  document.getElementById("edit-doc-secundario").value = rec.tesauro_secundario || "";
  document.getElementById("edit-doc-resumen").value   = rec.resumen || "";
  document.getElementById("edit-doc-ubicacion").value = rec.ubicacion || "";
  document.getElementById("edit-doc-palabras").value  = "";

  // Poblar select de tipos
  const sel = document.getElementById("edit-doc-type");
  if (sel) {
    const types = state.choices?.archivo?.doc_types || [];
    sel.innerHTML = types.map(t => `<option value="${t}"${t === rec.doc_type ? " selected" : ""}>${t}</option>`).join("");
    if (!types.includes(rec.doc_type) && rec.doc_type) {
      sel.innerHTML = `<option value="${rec.doc_type}" selected>${rec.doc_type}</option>` + sel.innerHTML;
    }
  }

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
  if (!confirm(`¿Eliminar el documento "${nombre}"?\nEsta acción no se puede deshacer.`)) return;
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
  if (!confirm(`¿Eliminar el expediente de "${nombre}"?\nSe eliminarán todos sus documentos. Esta acción no se puede deshacer.`)) return;
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

// --- CATEGORÍAS ---
function loadCategoriesTab() {
  const suf       = adminSuffixFromTab();
  const container = document.getElementById(`admin_tax_list-${suf}`);
  if (!state.choices) {
    if (container) container.innerHTML = `<div class="text-muted p-2">Sin tipologías activas.</div>`;
    return;
  }

  if (isArchivoModule()) {
    // Archivo: lista plana de tipos
    const types = state.choices.archivo.doc_types;
    if (container) container.innerHTML = types.length
      ? types.map(t => `
          <div class="list-group-item d-flex justify-content-between align-items-center mb-1 rounded bg-white shadow-sm" style="border-left:4px solid #ffc107!important;">
            <h6 class="font-weight-bold text-dark mb-0">${t}</h6>
            <span class="badge badge-warning badge-pill">Archivo</span>
          </div>`).join("")
      : `<div class="text-muted p-2">Sin tipologías registradas.</div>`;
    loadKeywordsSection();
  } else {
    // RRHH: tipos agrupados por Parte
    const tiposPorParte = state.choices.rrhh?.tipos_por_parte || {};
    const parteColors   = { "Parte I": "#0056b3", "Parte II": "#28a745", "Parte III": "#e67e22", "Parte IV": "#dc3545" };
    const parteEntries  = Object.entries(tiposPorParte);

    if (container) {
      if (parteEntries.length === 0) {
        const types = state.choices.rrhh.doc_types;
        container.innerHTML = types.map(t => `
          <div class="list-group-item d-flex justify-content-between align-items-center mb-1 rounded bg-white shadow-sm" style="border-left:4px solid #6c757d!important;">
            <h6 class="font-weight-bold text-dark mb-0">${t}</h6>
            <span class="badge badge-secondary badge-pill">RRHH</span>
          </div>`).join("");
      } else {
        container.innerHTML = parteEntries.map(([parte, tipos]) => {
          const color = parteColors[parte] || "#6c757d";
          return `
            <div class="mb-3">
              <h6 class="px-2 py-1 rounded text-white font-weight-bold" style="background:${color};font-size:0.82rem;">${parte}</h6>
              ${tipos.map(t => `
                <div class="list-group-item d-flex justify-content-between align-items-center mb-1 rounded bg-white shadow-sm py-1" style="border-left:4px solid ${color}!important;">
                  <span style="font-size:0.82rem;font-weight:600;">${t}</span>
                  <span class="badge badge-pill text-white" style="background:${color};font-size:0.7rem;">Activa</span>
                </div>`).join("")}
            </div>`;
        }).join("");
      }
    }
  }
}

async function loadKeywordsSection() {
  const suf      = adminSuffixFromTab();
  const catPane  = document.getElementById(`pane-admin-${suf}-categories`);
  if (!catPane) return;

  let kwSection = document.getElementById("admin-keywords-section");
  if (!kwSection) {
    kwSection = document.createElement("div");
    kwSection.id = "admin-keywords-section";
    kwSection.className = "mt-4";
    catPane.appendChild(kwSection);
  }

  try {
    const res = await fetch(`${API_BASE}/api/admin/keywords`);
    if (!res.ok) throw new Error();
    const keywords = await res.json();

    kwSection.innerHTML = `
      <div class="card card-info">
        <div class="card-header"><h3 class="card-title"><i class="fas fa-key mr-2"></i>Palabras Clave (Descriptores Libres)</h3></div>
        <div class="card-body p-3">
          <div class="input-group mb-3" style="max-width:420px;">
            <input type="text" id="new_keyword_input" class="form-control form-control-sm" placeholder="Ej: Gestión académica">
            <div class="input-group-append">
              <button class="btn btn-info btn-sm" onclick="handleAddKeyword()"><i class="fas fa-plus"></i> Agregar</button>
            </div>
          </div>
          <div class="row">
            ${keywords.length === 0
              ? `<div class="col-12 text-muted small">Sin palabras clave registradas aún.</div>`
              : keywords.map(kw => `
                  <div class="col-md-4 col-sm-6 mb-2" id="kw-item-${kw.id}">
                    <div class="d-flex align-items-center border rounded px-2 py-1 bg-white shadow-sm">
                      <i class="fas fa-tag text-info mr-2" style="font-size:0.78rem;"></i>
                      <span class="flex-grow-1 font-weight-600" style="font-size:0.82rem;" id="kw-label-${kw.id}">${kw.nombre}</span>
                      <small class="text-muted mr-1">(${kw.uso_archivo})</small>
                      <button class="btn btn-link btn-sm p-0 mr-1" onclick="handleEditKeyword(${kw.id})" title="Renombrar">
                        <i class="fas fa-pen text-warning" style="font-size:0.72rem;"></i>
                      </button>
                      <button class="btn btn-link btn-sm p-0" onclick="handleDeleteKeyword(${kw.id},'${kw.nombre.replace(/'/g,"\\'")}')" title="Eliminar">
                        <i class="fas fa-trash text-danger" style="font-size:0.72rem;"></i>
                      </button>
                    </div>
                  </div>`).join("")}
          </div>
        </div>
      </div>`;
  } catch (e) {
    console.error("Error cargando palabras clave:", e);
    if (kwSection) kwSection.innerHTML = `<div class="alert alert-warning">No se pudo cargar las palabras clave.</div>`;
  }
}

async function handleAddKeyword() {
  const input  = document.getElementById("new_keyword_input");
  const nombre = (input?.value || "").trim();
  if (!nombre) { showToast("Ingrese una palabra clave.", "warning"); return; }
  try {
    const res = await fetch(`${API_BASE}/api/admin/keywords`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ nombre }),
    });
    if (!res.ok) {
      const err = await res.json().catch(() => ({}));
      throw new Error(err.detail || "Error");
    }
    if (input) input.value = "";
    loadKeywordsSection();
  } catch (err) {
    showToast(err.message || "Error al agregar palabra clave.", "error");
  }
}

async function handleEditKeyword(id) {
  const labelEl     = document.getElementById(`kw-label-${id}`);
  const currentName = labelEl?.innerText || "";
  const newName     = prompt("Nuevo nombre para la palabra clave:", currentName);
  if (!newName || newName.trim() === currentName) return;
  try {
    const res = await fetch(`${API_BASE}/api/admin/keywords/${id}`, {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ nombre: newName.trim() }),
    });
    if (!res.ok) throw new Error();
    loadKeywordsSection();
  } catch {
    showToast("Error al renombrar la palabra clave.", "error");
  }
}

async function handleDeleteKeyword(id, nombre) {
  if (!confirm(`¿Eliminar la palabra clave "${nombre}"?\nSe desvinculará de todos los documentos que la usen.`)) return;
  try {
    const res = await fetch(`${API_BASE}/api/admin/keywords/${id}`, { method: "DELETE" });
    if (!res.ok) throw new Error();
    loadKeywordsSection();
  } catch {
    showToast("Error al eliminar la palabra clave.", "error");
  }
}

async function handleAddCategory() {
  const suf   = adminSuffixFromTab();
  const name  = document.getElementById(`new_tax_name-${suf}`)?.value.trim() || "";
  const desc  = document.getElementById(`new_tax_desc-${suf}`)?.value.trim() || "";
  const scope = isArchivoModule() ? "Archivo" : "RRHH";
  const parte = document.getElementById(`new_tax_parte-${suf}`)?.value || "";
  if (!name) { showToast("Por favor, ingrese el nombre de la tipología.", "warning"); return; }
  try {
    const res = await fetch(`${API_BASE}/api/admin/add_category`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ name, desc, scope, parte, usuario: state.user.username }),
    });
    if (!res.ok) throw new Error();
    showToast("¡Nueva tipología guardada con éxito!", "success");
    if (document.getElementById(`new_tax_name-${suf}`)) document.getElementById(`new_tax_name-${suf}`).value = "";
    if (document.getElementById(`new_tax_desc-${suf}`)) document.getElementById(`new_tax_desc-${suf}`).value = "";
    await loadDynamicChoices();
    loadAdminTab("categories");
  } catch {
    showToast("Error al guardar tipología.", "error");
  }
}

// --- USUARIOS ---
async function loadUsersTab() {
  const suf       = adminSuffixFromTab();
  const container = document.getElementById(`admin_users_table-${suf}`);
  try {
    const res = await fetch(`${API_BASE}/api/admin/users`);
    if (!res.ok) throw new Error();
    const users = await res.json();
    container.innerHTML = `
      <table class="table table-striped table-bordered" style="font-size:0.85rem;">
        <thead><tr class="bg-light"><th>Usuario</th><th>Contraseña</th><th>Módulo</th><th>Rol</th><th>Estado</th><th>Acciones</th></tr></thead>
        <tbody>
          ${users.map(u => `
            <tr>
              <td class="font-weight-bold text-dark"><i class="fas fa-user-circle mr-1 text-secondary"></i> ${u.usuario}</td>
              <td class="text-muted">${u.password}</td>
              <td>${u.modulo}</td>
              <td><span class="badge ${u.rol === "Admin" ? "badge-danger" : "badge-primary"}">${u.rol}</span></td>
              <td><span class="badge badge-success"><i class="fas fa-check-circle mr-1"></i> Autorizado</span></td>
              <td>
                <button class="btn btn-xs btn-outline-secondary" onclick="handleChangePassword(${u.id},'${u.usuario}')">
                  <i class="fas fa-key"></i> Cambiar clave
                </button>
              </td>
            </tr>
          `).join("")}
        </tbody>
      </table>
    `;
  } catch {
    container.innerHTML = `<div class="alert alert-danger">Error al cargar listado de seguridad.</div>`;
  }
}

async function handleChangePassword(uid, username) {
  const newPass = prompt(`Nueva contraseña para "${username}" (mín. 6 caracteres):`);
  if (!newPass || newPass.trim().length < 6) {
    if (newPass !== null) showToast("La contraseña debe tener al menos 6 caracteres.", "warning");
    return;
  }
  try {
    const res = await fetch(`${API_BASE}/api/admin/users/${uid}/password`, {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ new_password: newPass.trim(), requester: state.user.username }),
    });
    if (!res.ok) throw new Error();
    showToast(`Contraseña de "${username}" actualizada.`, "success");
  } catch {
    showToast("Error al cambiar la contraseña.", "error");
  }
}

async function handleAddUser() {
  const suf      = adminSuffixFromTab();
  const username = document.getElementById(`new_user_name-${suf}`)?.value.trim() || "";
  const pass     = document.getElementById(`new_user_pass-${suf}`)?.value.trim()  || "";
  const modulo   = document.getElementById(`new_user_modulo-${suf}`)?.value       || "";
  const rol      = document.getElementById(`new_user_rol-${suf}`)?.value          || "";
  if (!username || !pass) { showToast("Por favor, ingrese todos los datos requeridos.", "warning"); return; }
  try {
    const res = await fetch(`${API_BASE}/api/admin/users/create`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ usuario: username, password: pass, modulo, rol, creator: state.user.username })
    });
    if (!res.ok) {
      const err = await res.json();
      throw new Error(err.detail || "Error");
    }
    showToast(`¡Usuario ${username} registrado con éxito!`, "success");
    document.getElementById(`new_user_name-${suf}`) && (document.getElementById(`new_user_name-${suf}`).value = "");
    document.getElementById(`new_user_pass-${suf}`) && (document.getElementById(`new_user_pass-${suf}`).value = "");
    loadUsersTab();
  } catch (err) {
    showToast(err.message || "Error al registrar el nuevo usuario.", "error");
  }
}
