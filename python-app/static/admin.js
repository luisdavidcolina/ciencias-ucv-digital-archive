// ==========================================================================
// TOAST SYSTEM
// ==========================================================================
function showToast(message, type, duration) {
  type = type || "info";
  const ttl = duration ?? { success: 3000, error: 6000, warning: 4500, info: 3500 }[type] ?? 3500;
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
  toast.style.cssText = `background:${cfg.bg};color:${cfg.color};border:1px solid ${cfg.border};border-radius:8px;padding:10px 14px;margin-bottom:8px;min-width:260px;max-width:380px;display:flex;align-items:flex-start;gap:8px;box-shadow:0 4px 12px rgba(0,0,0,.15);font-size:0.87rem;transition:opacity 0.4s,transform 0.3s;transform:translateX(20px);`;
  toast.innerHTML = `<i class="${cfg.icon}" style="font-size:1rem;flex-shrink:0;margin-top:2px;"></i><span style="flex:1;">${message}</span><button style="background:none;border:none;padding:0 0 0 8px;cursor:pointer;opacity:0.6;color:inherit;font-size:1rem;" onclick="this.closest('div').remove()">✕</button>`;
  container.appendChild(toast);

  // Animate in
  requestAnimationFrame(() => { toast.style.transform = "translateX(0)"; });

  const dismiss = () => {
    toast.style.opacity = "0";
    toast.style.transform = "translateX(20px)";
    setTimeout(() => toast.remove(), 400);
  };
  const timer = setTimeout(dismiss, ttl);
  toast.querySelector("button").addEventListener("click", () => clearTimeout(timer));
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
  else if (adminTabId === "new")        { renderDynamicSubmitFields(); loadRecentSubmissions(); initDropZone(suf); }
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

    const resUsers = await fetch(`${API_BASE}/api/admin/users?modulo=${encodeURIComponent(state.user.modulo)}`);
    if (resUsers.ok) {
      const uList = await resUsers.json();
      const kpiUsers = document.getElementById(`kpi-total-users-${suf}`);
      if (kpiUsers) kpiUsers.innerText = uList.length;
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
      <div class="row mt-2">
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">N° de Folio / Signatura</label>
          <input type="text" id="reg-folio-${suf}" class="form-control" placeholder="Ej: F-023, Carp-A-12">
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Soporte</label>
          <select id="reg-soporte-${suf}" class="form-control">
            ${(state.choices?.archivo?.soportes || ["Físico","Digital","Digitalizado"]).map(s => `<option value="${s}">${s}</option>`).join("")}
          </select>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">N° de Páginas</label>
          <input type="number" id="reg-paginas-${suf}" class="form-control" min="1" placeholder="Ej: 12">
        </div>
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
          <div class="input-group">
            <input type="text" id="reg-cedula-${suf}" class="form-control" placeholder="Ej: V-12345678" required>
            <div class="input-group-append">
              <button class="btn btn-outline-info btn-sm" type="button" title="Buscar empleado por cédula"
                      onclick="_lookupByCedula('${suf}')">
                <i class="fas fa-user-check"></i>
              </button>
            </div>
          </div>
          <small id="reg-cedula-hint-${suf}" class="text-muted"></small>
        </div>
      </div>
      <div class="row mt-2">
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">C.I.F. (RIF)</label>
          <input type="text" id="reg-rif-${suf}" class="form-control" placeholder="Ej: J-12345678-0">
        </div>
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">Cargo Asignado</label>
          <input type="text" id="reg-cargo-${suf}" class="form-control" placeholder="Ej: Analista Contable" list="dl-cargos" autocomplete="off">
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
          <input type="text" id="reg-depto-${suf}" class="form-control" placeholder="Ej: Biología" required list="dl-departamentos" autocomplete="off">
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Estado *</label>
          <select id="reg-estado-${suf}" class="form-control" required>
            ${(state.choices?.rrhh?.estados_catalog || ["Activo","Retirado","Jubilado","Pensionado"])
              .map(e => `<option value="${e}"${e==="Activo"?" selected":""}>${e}</option>`).join("")}
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
      <!-- Datos personales LOTTT -->
      <div class="row mt-2">
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Fecha de Nacimiento <span class="badge badge-secondary badge-sm">LOTTT</span></label>
          <input type="date" id="reg-nacimiento-${suf}" class="form-control">
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Sexo <span class="badge badge-secondary badge-sm">LOTTT</span></label>
          <select id="reg-sexo-${suf}" class="form-control">
            <option value="">No especificado</option>
            <option value="M">Masculino</option>
            <option value="F">Femenino</option>
            <option value="O">Otro</option>
          </select>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Nivel Educativo <span class="badge badge-secondary badge-sm">LOTTT</span></label>
          <select id="reg-nivel-${suf}" class="form-control">
            <option value="">No especificado</option>
            ${(state.choices?.rrhh?.niveles_educativos || ["Bachiller","TSU","Universitario","Especialización","Maestría","Doctorado","Postdoctorado"]).map(n => `<option value="${n}">${n}</option>`).join("")}
          </select>
        </div>
      </div>
    `;
  }
}

async function loadRecentSubmissions() {
  const suf         = adminSuffixFromTab();
  const containerEl = document.getElementById(`recent_submissions-${suf}`);
  if (!containerEl) return;
  const isArchivo = isArchivoModule();

  containerEl.innerHTML = `<li class="text-center p-2"><i class="fas fa-spinner fa-spin text-muted"></i></li>`;

  try {
    const modulo = isArchivo ? "Archivo" : "RRHH";
    const res = await fetch(`${API_BASE}/api/admin/list_all?modulo=${modulo}&page=1&per_page=5`);
    if (!res.ok) throw new Error();
    const data = await res.json();
    const records = data.records || [];

    if (records.length === 0) {
      containerEl.innerHTML = `<li class="text-muted text-center p-2">Sin ingresos previos.</li>`;
      return;
    }

    const statusBadge = s => {
      const icons = { aprobado: "fa-check", revision: "fa-clock", draft: "fa-pencil-alt", rechazado: "fa-times" };
      const cls   = { aprobado: "badge-success", revision: "badge-warning text-dark", draft: "badge-secondary", rechazado: "badge-danger" };
      const label = { aprobado: "Aprobado", revision: "Revisión", draft: "Borrador", rechazado: "Rechazado" };
      const st = s || "aprobado";
      return `<span class="badge ${cls[st] || 'badge-secondary'} ds-status-badge" style="font-size:0.65rem;" title="${label[st] || st}"><i class="fas ${icons[st] || 'fa-circle'} mr-1"></i>${label[st] || st}</span>`;
    };

    containerEl.innerHTML = records.map(item => {
      const title = isArchivo ? item.titulo : item.empleado;
      return `<li class="d-flex align-items-center mb-2 p-2 border rounded bg-light">
        <i class="fas fa-file-alt mr-2 text-primary" style="font-size:1.15rem;flex-shrink:0;"></i>
        <div style="overflow:hidden;flex-grow:1;">
          <div style="overflow:hidden;text-overflow:ellipsis;white-space:nowrap;">
            <strong style="font-size:0.82rem;" class="text-dark">${title || "Sin título"}</strong>
          </div>
          <div class="d-flex align-items-center gap-1">
            <span class="text-muted" style="font-size:0.70rem;">${item.doc_type || ""}</span>
            ${statusBadge(item.status)}
          </div>
        </div>
      </li>`;
    }).join("");
  } catch {
    containerEl.innerHTML = `<li class="text-muted text-center p-2">Error cargando ingresos recientes.</li>`;
  }
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
    payload.numero_folio        = val(`reg-folio-${suf}`) || null;
    payload.soporte             = val(`reg-soporte-${suf}`) || "Físico";
    const pags = parseInt(document.getElementById(`reg-paginas-${suf}`)?.value || "");
    if (!isNaN(pags) && pags > 0) payload.numero_paginas = pags;
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
    payload.fecha_nacimiento      = val(`reg-nacimiento-${suf}`) || null;
    payload.sexo                  = val(`reg-sexo-${suf}`) || null;
    payload.nivel_educativo       = val(`reg-nivel-${suf}`) || null;
  }

  try {
    // Subir archivo digitalizado (si se seleccionó uno) antes de crear el registro
    const fileInput = document.getElementById(`file_upload-${suf}`);
    const file = fileInput?.files?.[0];
    if (file) {
      if (submitBtn) submitBtn.innerHTML = '<i class="fas fa-cloud-upload-alt fa-fade mr-1"></i> Subiendo archivo...';
      const fd = new FormData();
      fd.append("file", file);
      fd.append("modulo", state.user.modulo.toLowerCase());
      fd.append("usuario", state.user.username);
      const upRes = await fetch(`${API_BASE}/api/admin/upload`, { method: "POST", body: fd });
      if (!upRes.ok) {
        const upErr = await upRes.json().catch(() => ({}));
        showToast(upErr.detail || "Error al subir el archivo digitalizado.", "error");
        return;
      }
      const upData = await upRes.json();
      payload.file_url = upData.file_url;
      if (submitBtn) submitBtn.innerHTML = '<i class="fas fa-spinner fa-spin mr-1"></i> Guardando...';
    }

    const res = await fetch(`${API_BASE}/api/admin/submit`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    });
    if (!res.ok) {
      const errData = await res.json().catch(() => ({}));
      let errMsg = "Error al registrar el folio.";
      if (Array.isArray(errData.detail)) {
        // Pydantic 422 validation errors
        errMsg = errData.detail.map(e => e.msg || String(e)).join("; ");
      } else if (typeof errData.detail === "string") {
        errMsg = errData.detail;
      }
      showToast(errMsg, "error");
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
    _renderMonitorStatusBadges();
  } catch (e) {
    console.error("Error al cargar monitor:", e);
  }
}

async function _renderMonitorStatusBadges() {
  const suf    = adminSuffixFromTab();
  const mod    = state.user.modulo;
  const badgesEl = document.getElementById(`monitor-status-badges-${suf}`);
  if (!badgesEl) return;
  if (!isArchivoModule()) { badgesEl.innerHTML = ""; return; }
  try {
    const res = await fetch(`${API_BASE}/api/admin/status_counts?modulo=${mod}`);
    if (!res.ok) return;
    const counts = await res.json();
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
    draft:      '<span class="badge badge-secondary ds-status-badge" title="Borrador — sin publicar"><i class="fas fa-pencil-alt mr-1"></i>Borrador</span>',
    revision:   '<span class="badge badge-warning text-dark ds-status-badge ds-status-revision" title="Pendiente de revisión"><i class="fas fa-clock mr-1"></i>Revisión</span>',
    aprobado:   '<span class="badge badge-success ds-status-badge" title="Aprobado y publicado"><i class="fas fa-check mr-1"></i>OK</span>',
    rechazado:  '<span class="badge badge-danger ds-status-badge" title="Rechazado"><i class="fas fa-times mr-1"></i>Rechazado</span>',
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
          <td class="font-weight-bold text-dark" style="max-width:180px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;" title="${(f.empleado||'').replace(/"/g,'&quot;')}">${hlEmpleado}</td>
          <td class="text-muted small">${f.cedula || "—"}</td>
          <td class="text-muted small" style="max-width:120px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;">${f.cargo || f.departamento || "—"}</td>
          <td><span class="badge" style="background-color:${c};color:white;padding:3px 6px;">${f.estado || "—"}</span></td>
          <td><span class="badge badge-light border" title="${f.tipos||''}" style="padding:3px 6px;">${(f.tipos||'').split(';')[0].trim() || '—'}</span></td>
          <td class="text-muted small">${f.ubicacion || "—"}</td>
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

  // Personas relacionadas
  const personasEl = document.getElementById("edit-doc-personas");
  if (personasEl) personasEl.value = rec.personas_relacionadas || "";

  // Campos ISAD(G)
  const folioEl = document.getElementById("edit-doc-folio");
  if (folioEl) folioEl.value = rec.numero_folio || "";
  const soporteEl = document.getElementById("edit-doc-soporte");
  if (soporteEl) soporteEl.value = rec.soporte || "Físico";
  const paginasEl = document.getElementById("edit-doc-paginas");
  if (paginasEl) paginasEl.value = rec.numero_paginas || "";
  const idiomaEl = document.getElementById("edit-doc-idioma");
  if (idiomaEl) idiomaEl.value = rec.idioma || "es";

  // Reset drop zone state from a previous upload
  const dz = document.getElementById("edit-doc-dropzone");
  if (dz) { dz.style.borderColor = "#adb5bd"; dz.style.background = "#f8f9fa"; }
  const dzStatus = document.getElementById("edit-doc-upload-status");
  if (dzStatus) dzStatus.innerHTML = "";

  // Reset versiones container
  const verContainer = document.getElementById("edit-doc-versiones-container");
  if (verContainer) verContainer.style.display = "none";
  const verBtn = document.getElementById("btn-toggle-versiones");
  if (verBtn) verBtn.innerHTML = '<i class="fas fa-chevron-down mr-1"></i>Ver historial';
  const verBody = document.getElementById("edit-doc-versiones-body");
  if (verBody) verBody.innerHTML = '<p class="text-muted small text-center py-2">Cargando...</p>';

  $("#editArchivoModal").modal("show");
}

async function _lookupByCedula(suf) {
  const cedInput = document.getElementById(`reg-cedula-${suf}`);
  const hintEl   = document.getElementById(`reg-cedula-hint-${suf}`);
  const cedula   = (cedInput?.value || "").trim();
  if (!cedula) { showToast("Ingrese una cédula primero.", "warning"); return; }

  try {
    const res = await fetch(`${API_BASE}/api/rrhh/empleado/por-cedula/${encodeURIComponent(cedula)}`);
    if (res.status === 404) {
      if (hintEl) hintEl.innerHTML = '<span class="text-info"><i class="fas fa-user-plus mr-1"></i>Empleado nuevo — complete los datos.</span>';
      return;
    }
    if (!res.ok) throw new Error();
    const emp = await res.json();

    // Rellenar campos del formulario
    const setVal = (id, v) => { const el = document.getElementById(id); if (el) el.value = v || ""; };
    setVal(`reg-nombres-${suf}`,  emp.nombres || "");
    setVal(`reg-apellidos-${suf}`, emp.apellidos || "");
    setVal(`reg-cargo-${suf}`,    emp.cargo || "");
    setVal(`reg-depto-${suf}`,    emp.departamento || "");
    setVal(`reg-rif-${suf}`,      emp.rif || "");
    const estadoSel = document.getElementById(`reg-estado-${suf}`);
    if (estadoSel && emp.estado) estadoSel.value = emp.estado;

    if (hintEl) hintEl.innerHTML = `<span class="text-success"><i class="fas fa-check-circle mr-1"></i>Empleado encontrado: ${emp.nombres} ${emp.apellidos}. Datos prellenados.</span>`;
    showToast(`Datos de ${emp.nombres} ${emp.apellidos} cargados.`, "info");
  } catch {
    if (hintEl) hintEl.innerHTML = '<span class="text-danger">Error al buscar el empleado.</span>';
  }
}

function _refreshEditDocPreview() {
  const url = (document.getElementById("edit-doc-file-url")?.value || "").trim();
  const container = document.getElementById("edit-doc-file-preview");
  if (!container) return;
  if (!url) {
    container.innerHTML = '<p class="text-muted small mb-0">Sin archivo adjunto.</p>';
    return;
  }
  if (/\.(pdf)$/i.test(url)) {
    container.innerHTML = `<iframe src="${url}" style="width:100%;height:220px;border:1px solid #ddd;border-radius:4px;" title="Preview PDF"></iframe>`;
  } else if (/\.(png|jpe?g|gif|webp|svg)$/i.test(url)) {
    container.innerHTML = `<img src="${url}" style="max-width:100%;max-height:220px;border:1px solid #ddd;border-radius:4px;object-fit:contain;" alt="Preview">`;
  } else {
    container.innerHTML = `<a href="${url}" target="_blank" class="btn btn-sm btn-outline-primary"><i class="fas fa-external-link-alt mr-1"></i>Abrir en nueva ventana</a>`;
  }
}

// --- DRAG-AND-DROP UPLOAD EN MODAL DE EDICIÓN ---

async function _uploadEditDocFile(file) {
  const ALLOWED = ["pdf","png","jpg","jpeg","tiff","tif","webp"];
  const ext = (file.name.split(".").pop() || "").toLowerCase();
  if (!ALLOWED.includes(ext)) { showToast("Tipo de archivo no permitido.", "warning"); return; }
  if (file.size > 25 * 1024 * 1024) { showToast("El archivo excede el límite de 25 MB.", "warning"); return; }

  const status = document.getElementById("edit-doc-upload-status");
  const zone   = document.getElementById("edit-doc-dropzone");
  const urlField = document.getElementById("edit-doc-file-url");

  if (status) status.innerHTML = '<i class="fas fa-spinner fa-spin mr-1"></i>Subiendo…';
  if (zone)   { zone.style.borderColor = "#fd7e14"; zone.style.background = "#fff8f0"; }

  const fd = new FormData();
  fd.append("file", file);
  fd.append("modulo", isArchivoModule() ? "archivo" : "rrhh");
  fd.append("usuario", state.user?.username || "");

  try {
    const res  = await fetch(`${API_BASE}/api/admin/upload`, { method: "POST", body: fd });
    const data = await res.json();
    if (!res.ok) throw new Error(data.detail || "Error al subir");
    if (urlField) urlField.value = data.file_url;
    if (status) status.innerHTML = `<span class="text-success"><i class="fas fa-check-circle mr-1"></i>${file.name} subido</span>`;
    if (zone)   { zone.style.borderColor = "#28a745"; zone.style.background = "#f0fff4"; }
    _refreshEditDocPreview();
    showToast("Archivo subido correctamente.", "success");
  } catch (e) {
    if (status) status.innerHTML = `<span class="text-danger"><i class="fas fa-times-circle mr-1"></i>${e.message}</span>`;
    if (zone)   { zone.style.borderColor = "#dc3545"; zone.style.background = "#fff5f5"; }
    showToast(`Error al subir: ${e.message}`, "error");
  }
}

function _handleEditDocDrop(event) {
  event.preventDefault();
  const zone = document.getElementById("edit-doc-dropzone");
  if (zone) { zone.style.borderColor = "#adb5bd"; zone.style.background = "#f8f9fa"; }
  const file = event.dataTransfer?.files?.[0];
  if (file) _uploadEditDocFile(file);
}

function _handleEditDocFileSelect(event) {
  const file = event.target.files?.[0];
  if (file) _uploadEditDocFile(file);
  event.target.value = "";
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
    personas_relacionadas: document.getElementById("edit-doc-personas")?.value.trim() || null,
    numero_folio:       document.getElementById("edit-doc-folio")?.value.trim() || null,
    soporte:            document.getElementById("edit-doc-soporte")?.value || null,
    numero_paginas:     parseInt(document.getElementById("edit-doc-paginas")?.value) || null,
    idioma:             document.getElementById("edit-doc-idioma")?.value || "es",
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
    "Mover a la papelera",
    `¿Enviar "${nombre}" a la papelera? Podrás restaurarlo desde la pestaña Papelera.`,
    "Sí, mover a papelera", "btn-danger"
  );
  if (!ok) return;
  try {
    const res = await fetch(`${API_BASE}/api/admin/documento/${id}?modulo=${encodeURIComponent(state.user.modulo)}&usuario=${encodeURIComponent(state.user.username)}`, {
      method: "DELETE",
    });
    if (!res.ok) throw new Error();
    showToast("Documento movido a la papelera.", "success");
    state.adminTable.page = 1;
    loadMonitorTable();
  } catch {
    showToast("Error al mover a la papelera.", "error");
  }
}

// --- PAPELERA DE RECICLAJE ---

const _papeleraState = { archivo: { page: 1, total: 0 }, rrhh: { page: 1, total: 0 }, empleados: { page: 1, total: 0 } };

async function loadPapelera(suf) {
  if (suf === "archivo") {
    await _loadPapeleraDocumentos("Archivo", "archivo");
  } else {
    await _loadPapeleraDocumentos("RRHH", "rrhh");
    await _loadPapeleraEmpleados();
  }
}

async function _loadPapeleraDocumentos(modulo, suf) {
  const page = _papeleraState[suf].page;
  const body = document.getElementById(`papelera-body-${suf}`);
  const summary = document.getElementById(`papelera-summary-${suf}`);
  const pageInfo = document.getElementById(`papelera-page-info-${suf}`);
  if (!body) return;

  body.innerHTML = '<tr><td colspan="7" class="text-center py-2"><i class="fas fa-spinner fa-spin"></i></td></tr>';
  try {
    const res = await fetch(`${API_BASE}/api/admin/papelera?modulo=${encodeURIComponent(modulo)}&page=${page}&per_page=20`);
    const data = await res.json();
    _papeleraState[suf].total = data.total;
    if (!data.records?.length) {
      body.innerHTML = '<tr><td colspan="7" class="text-center text-muted py-3">La papelera está vacía.</td></tr>';
    } else {
      body.innerHTML = data.records.map((r, i) => `
        <tr>
          <td>${(page - 1) * 20 + i + 1}</td>
          <td>${r.titulo || "—"}</td>
          <td><small>${r.doc_type || "—"}</small></td>
          <td><small>${r.fecha || "—"}</small></td>
          <td><small class="text-muted">${r.deleted_by || "—"}</small></td>
          <td><small class="text-muted">${r.deleted_at || "—"}</small></td>
          <td>
            <button class="btn btn-xs btn-success mr-1" onclick="_restaurarDoc(${r.id},'${modulo}')" title="Restaurar"><i class="fas fa-undo"></i></button>
            <button class="btn btn-xs btn-danger" onclick="_purgarDoc(${r.id},'${modulo}')" title="Eliminar permanentemente"><i class="fas fa-fire"></i></button>
          </td>
        </tr>`).join("");
    }
    if (summary) summary.textContent = `${data.total} documento(s) en papelera`;
    if (pageInfo) pageInfo.textContent = `Pág. ${page} / ${Math.max(1, Math.ceil(data.total / 20))}`;
  } catch {
    body.innerHTML = '<tr><td colspan="7" class="text-danger text-center py-2">Error al cargar la papelera.</td></tr>';
  }
}

async function _loadPapeleraEmpleados() {
  const page = _papeleraState.empleados.page;
  const body = document.getElementById("papelera-body-empleados");
  const summary = document.getElementById("papelera-summary-empleados");
  const pageInfo = document.getElementById("papelera-page-info-empleados");
  if (!body) return;

  body.innerHTML = '<tr><td colspan="6" class="text-center py-2"><i class="fas fa-spinner fa-spin"></i></td></tr>';
  try {
    const res = await fetch(`${API_BASE}/api/admin/papelera/empleados?page=${page}&per_page=20`);
    const data = await res.json();
    _papeleraState.empleados.total = data.total;
    if (!data.records?.length) {
      body.innerHTML = '<tr><td colspan="6" class="text-center text-muted py-3">No hay empleados en la papelera.</td></tr>';
    } else {
      body.innerHTML = data.records.map((r, i) => `
        <tr>
          <td>${(page - 1) * 20 + i + 1}</td>
          <td>${r.nombre || "—"}</td>
          <td><small>${r.cedula || "—"}</small></td>
          <td><small class="text-muted">${r.deleted_by || "—"}</small></td>
          <td><small class="text-muted">${r.deleted_at || "—"}</small></td>
          <td>
            <button class="btn btn-xs btn-success mr-1" onclick="_restaurarEmpleado(${r.id})" title="Restaurar"><i class="fas fa-undo"></i></button>
            <button class="btn btn-xs btn-danger" onclick="_purgarEmpleado(${r.id})" title="Eliminar permanentemente"><i class="fas fa-fire"></i></button>
          </td>
        </tr>`).join("");
    }
    if (summary) summary.textContent = `${data.total} empleado(s) en papelera`;
    if (pageInfo) pageInfo.textContent = `Pág. ${page} / ${Math.max(1, Math.ceil(data.total / 20))}`;
  } catch {
    body.innerHTML = '<tr><td colspan="6" class="text-danger text-center py-2">Error al cargar.</td></tr>';
  }
}

async function changePapeleraPage(dir, suf) {
  _papeleraState[suf].page = Math.max(1, _papeleraState[suf].page + dir);
  if (suf === "empleados") await _loadPapeleraEmpleados();
  else await _loadPapeleraDocumentos(suf === "archivo" ? "Archivo" : "RRHH", suf);
}

async function _restaurarDoc(id, modulo) {
  try {
    const res = await fetch(`${API_BASE}/api/admin/papelera/${id}/restaurar?modulo=${encodeURIComponent(modulo)}&usuario=${encodeURIComponent(state.user?.username || "")}`, { method: "POST" });
    if (!res.ok) throw new Error();
    showToast("Documento restaurado.", "success");
    loadPapelera(modulo === "Archivo" ? "archivo" : "rrhh");
  } catch { showToast("Error al restaurar.", "error"); }
}

async function _purgarDoc(id, modulo) {
  const ok = await confirmModal("Eliminar permanentemente", "Esta acción es irreversible. ¿Continuar?", "Sí, eliminar", "btn-danger");
  if (!ok) return;
  try {
    const res = await fetch(`${API_BASE}/api/admin/papelera/${id}/purgar?modulo=${encodeURIComponent(modulo)}&usuario=${encodeURIComponent(state.user?.username || "")}`, { method: "DELETE" });
    if (!res.ok) throw new Error();
    showToast("Documento eliminado permanentemente.", "success");
    loadPapelera(modulo === "Archivo" ? "archivo" : "rrhh");
  } catch { showToast("Error al purgar.", "error"); }
}

async function _restaurarEmpleado(id) {
  try {
    const res = await fetch(`${API_BASE}/api/admin/papelera/empleados/${id}/restaurar?usuario=${encodeURIComponent(state.user?.username || "")}`, { method: "POST" });
    if (!res.ok) throw new Error();
    showToast("Empleado restaurado.", "success");
    _loadPapeleraEmpleados();
  } catch { showToast("Error al restaurar empleado.", "error"); }
}

async function _purgarEmpleado(id) {
  const ok = await confirmModal("Eliminar empleado permanentemente", "Se eliminarán también todos sus documentos. Esta acción es irreversible.", "Sí, eliminar", "btn-danger");
  if (!ok) return;
  try {
    const res = await fetch(`${API_BASE}/api/admin/papelera/empleados/${id}/purgar?usuario=${encodeURIComponent(state.user?.username || "")}`, { method: "DELETE" });
    if (!res.ok) throw new Error();
    showToast("Empleado eliminado permanentemente.", "success");
    _loadPapeleraEmpleados();
  } catch { showToast("Error al purgar empleado.", "error"); }
}

// --- VERSIONES DE ARCHIVOS DIGITALES ---

async function loadDocVersiones(docId, modulo) {
  const container = document.getElementById("edit-doc-versiones-body");
  if (!container) return;
  container.innerHTML = '<p class="text-muted small text-center py-2"><i class="fas fa-spinner fa-spin"></i></p>';
  try {
    const res = await fetch(`${API_BASE}/api/admin/documento/${docId}/versiones?modulo=${encodeURIComponent(modulo)}`);
    const data = await res.json();
    if (!data.versiones?.length) {
      container.innerHTML = '<p class="text-muted small text-center py-2">Sin versiones anteriores.</p>';
      return;
    }
    container.innerHTML = `
      <table class="table table-sm table-bordered mb-0" style="font-size:0.8rem;">
        <thead class="thead-light"><tr><th>Ver.</th><th>Comentario</th><th>Subido por</th><th>Fecha</th><th></th></tr></thead>
        <tbody>
          ${data.versiones.map(v => `
            <tr>
              <td><span class="badge badge-secondary">v${v.version_num}</span></td>
              <td>${v.comentario || "—"}</td>
              <td>${v.subido_por || "—"}</td>
              <td>${v.created_at || "—"}</td>
              <td>
                <button class="btn btn-xs btn-outline-success mr-1" onclick="_restaurarVersion(${docId},${v.id},'${modulo}')" title="Restaurar esta versión"><i class="fas fa-undo"></i></button>
                <button class="btn btn-xs btn-outline-danger" onclick="_deleteVersion(${docId},${v.id},'${modulo}')" title="Eliminar del historial"><i class="fas fa-trash"></i></button>
              </td>
            </tr>`).join("")}
        </tbody>
      </table>`;
  } catch {
    container.innerHTML = '<p class="text-danger small text-center py-2">Error al cargar versiones.</p>';
  }
}

async function _restaurarVersion(docId, verId, modulo) {
  try {
    const res = await fetch(`${API_BASE}/api/admin/documento/${docId}/versiones/${verId}/restaurar?modulo=${encodeURIComponent(modulo)}&usuario=${encodeURIComponent(state.user?.username || "")}`, { method: "POST" });
    if (!res.ok) throw new Error();
    showToast("Versión restaurada como archivo actual.", "success");
    const row = await (await fetch(`${API_BASE}/api/admin/documento/${docId}?modulo=${encodeURIComponent(modulo)}`)).json();
    if (row?.file_url) {
      const urlEl = document.getElementById("edit-doc-file-url");
      if (urlEl) { urlEl.value = row.file_url; _refreshEditDocPreview(); }
    }
    loadDocVersiones(docId, modulo);
  } catch { showToast("Error al restaurar versión.", "error"); }
}

async function _deleteVersion(docId, verId, modulo) {
  const ok = await confirmModal("Eliminar versión", "¿Eliminar esta versión del historial? No afecta al archivo actual.", "Sí, eliminar", "btn-danger");
  if (!ok) return;
  try {
    const res = await fetch(`${API_BASE}/api/admin/documento/${docId}/versiones/${verId}?modulo=${encodeURIComponent(modulo)}&usuario=${encodeURIComponent(state.user?.username || "")}`, { method: "DELETE" });
    if (!res.ok) throw new Error();
    showToast("Versión eliminada.", "success");
    loadDocVersiones(docId, modulo);
  } catch { showToast("Error al eliminar versión.", "error"); }
}

function _toggleVersiones() {
  const container = document.getElementById("edit-doc-versiones-container");
  const btn = document.getElementById("btn-toggle-versiones");
  const isHidden = container?.style.display === "none";
  if (container) container.style.display = isHidden ? "" : "none";
  if (btn) btn.innerHTML = isHidden
    ? '<i class="fas fa-chevron-up mr-1"></i>Ocultar historial'
    : '<i class="fas fa-chevron-down mr-1"></i>Ver historial';
  if (isHidden) {
    const docId = document.getElementById("edit-doc-id")?.value;
    const modulo = isArchivoModule() ? "Archivo" : "RRHH";
    if (docId) loadDocVersiones(parseInt(docId), modulo);
  }
}

async function _guardarComoVersion() {
  const docId = document.getElementById("edit-doc-id")?.value;
  const currentUrl = (document.getElementById("edit-doc-file-url")?.value || "").trim();
  if (!docId) return;
  if (!currentUrl) { showToast("No hay archivo actual que guardar como versión.", "warning"); return; }

  const modulo = isArchivoModule() ? "Archivo" : "RRHH";
  const comentario = await promptModal("Comentario de versión", "Describe brevemente el cambio (opcional):");

  try {
    const res = await fetch(
      `${API_BASE}/api/admin/documento/${docId}/versiones?modulo=${encodeURIComponent(modulo)}&file_url=${encodeURIComponent(currentUrl)}&comentario=${encodeURIComponent(comentario || "")}&usuario=${encodeURIComponent(state.user?.username || "")}`,
      { method: "POST" }
    );
    if (!res.ok) throw new Error();
    showToast("Versión guardada en el historial.", "success");
    if (document.getElementById("edit-doc-versiones-container")?.style.display !== "none") {
      loadDocVersiones(parseInt(docId), modulo);
    }
  } catch { showToast("Error al guardar versión.", "error"); }
}

// --- EDITAR / ELIMINAR EMPLEADO (RRHH) ---
async function openEditEmpleadoModal(empId) {
  let rec = state.adminTable.results.find(r => r.empleado_id == empId) || { empleado_id: empId };

  // Fetch datos frescos del servidor
  try {
    const res = await fetch(`${API_BASE}/api/admin/empleado/${empId}`);
    if (res.ok) rec = { ...rec, ...await res.json() };
  } catch {}

  document.getElementById("edit-emp-id").value          = rec.empleado_id || rec.id || "";
  document.getElementById("edit-emp-nombres").value     = rec.nombres || "";
  document.getElementById("edit-emp-apellidos").value   = rec.apellidos || "";
  document.getElementById("edit-emp-cargo").value       = rec.cargo || "";
  document.getElementById("edit-emp-departamento").value = rec.departamento || "";
  const estadoSel = document.getElementById("edit-emp-estado");
  if (estadoSel) {
    const estadosCat = state.choices?.rrhh?.estados_catalog || [];
    if (estadosCat.length) {
      estadoSel.innerHTML = estadosCat.map(e => `<option value="${e}">${e}</option>`).join("");
    }
    estadoSel.value = rec.estado || "Activo";
  }
  document.getElementById("edit-emp-nacimiento")?.value !== undefined &&
    (document.getElementById("edit-emp-nacimiento").value = rec.fecha_nacimiento || "");
  const sexoSel = document.getElementById("edit-emp-sexo");
  if (sexoSel) sexoSel.value = rec.sexo || "";
  const nivelSel = document.getElementById("edit-emp-nivel-educativo");
  if (nivelSel) nivelSel.value = rec.nivel_educativo || "";
  document.getElementById("edit-emp-rif").value         = rec.rif || "";
  document.getElementById("edit-emp-jubilacion").value  = rec.fecha_jubilacion || "";
  document.getElementById("edit-emp-pension").value     = rec.fecha_pension || "";
  document.getElementById("edit-emp-foto").value        = rec.foto_url || "";

  // Resetear historial admin al abrir el modal
  const histContainer = document.getElementById("admin-historial-container");
  const histBtn = document.getElementById("btn-toggle-historial-admin");
  if (histContainer) { histContainer.style.display = "none"; }
  if (histBtn) { histBtn.innerHTML = '<i class="fas fa-chevron-down mr-1"></i>Ver historial'; }
  window._adminHistorialEmpId = rec.empleado_id || rec.id;

  $("#editEmpleadoModal").modal("show");
}

async function handleSaveEditEmpleado() {
  const empId = document.getElementById("edit-emp-id")?.value;
  if (!empId) return;

  const payload = {
    nombres:          document.getElementById("edit-emp-nombres")?.value || null,
    apellidos:        document.getElementById("edit-emp-apellidos")?.value || null,
    cargo:            document.getElementById("edit-emp-cargo")?.value || null,
    departamento:     document.getElementById("edit-emp-departamento")?.value || null,
    estado:           document.getElementById("edit-emp-estado")?.value || null,
    rif:              document.getElementById("edit-emp-rif")?.value || null,
    fecha_jubilacion: document.getElementById("edit-emp-jubilacion")?.value || null,
    fecha_pension:    document.getElementById("edit-emp-pension")?.value || null,
    foto_url:         document.getElementById("edit-emp-foto")?.value || null,
    fecha_nacimiento: document.getElementById("edit-emp-nacimiento")?.value || null,
    sexo:             document.getElementById("edit-emp-sexo")?.value || null,
    nivel_educativo:  document.getElementById("edit-emp-nivel-educativo")?.value || null,
    usuario:          state.user.username,
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
    "Mover a la papelera",
    `¿Enviar el expediente de "${nombre}" a la papelera? Podrás restaurarlo desde la pestaña Papelera.`,
    "Sí, mover a papelera", "btn-danger"
  );
  if (!ok) return;
  try {
    const res = await fetch(`${API_BASE}/api/admin/empleado/${empId}?usuario=${encodeURIComponent(state.user.username)}`, {
      method: "DELETE",
    });
    if (!res.ok) throw new Error();
    showToast("Expediente movido a la papelera.", "success");
    loadMonitorTable();
  } catch {
    showToast("Error al mover a la papelera.", "error");
  }
}

function exportAdminCSV() {
  const records = state.adminTable.results;
  if (!records || records.length === 0) { showToast("No hay datos para exportar.", "warning"); return; }
  const isArch = isArchivoModule();

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

  // Metadato: filtros aplicados como comentario CSV (fila especial)
  const meta = `"# Exportado: ${today} | Módulo: ${state.user.modulo} | Pág: ${state.adminTable.page} | Total: ${state.adminTable.total}"`;
  const csv  = [meta, headers.join(","), ...rows].join("\n");
  const blob = new Blob(["﻿" + csv], { type: "text/csv;charset=utf-8;" });
  const url  = URL.createObjectURL(blob);
  const a    = document.createElement("a");
  a.href = url;
  a.download = `ciencias_ucv_${isArch ? "archivo" : "rrhh"}_${today}_p${state.adminTable.page}.csv`;
  a.click();
  URL.revokeObjectURL(url);
  showToast(`CSV exportado: ${records.length} registro(s).`, "success");
}

// ─── Drag & Drop en zona de carga ───────────────────────────────────────────
function initDropZone(suf) {
  const zone = document.querySelector(`#pane-admin-${suf}-new [style*="dashed"]`);
  const fileInput = document.getElementById(`file_upload-${suf}`);
  if (!zone || !fileInput) return;

  const updateLabel = (name) => {
    const label = zone.querySelector(".ds-drop-label") || zone.querySelector("p.text-muted");
    if (label) label.textContent = name ? `📄 ${name}` : "Arrastra aquí o selecciona archivos";
    const icon = zone.querySelector(".fa-file-upload");
    if (icon) {
      icon.classList.toggle("fa-file-upload", !name);
      icon.classList.toggle("fa-check-circle", !!name);
      icon.classList.toggle("text-secondary", !name);
      icon.classList.toggle("text-success", !!name);
    }
    // Rellenar campo file_url en el formulario
    const fileUrlInput = document.querySelector(`#admin-submit-form-${suf} [id$="file_url"]`) ||
                         document.getElementById(`field-file_url-${suf}`);
    if (fileUrlInput && name) fileUrlInput.placeholder = `Archivo seleccionado: ${name}`;
  };

  zone.addEventListener("dragover", e => {
    e.preventDefault();
    zone.style.borderColor = "#0056b3";
    zone.style.background  = "#e8f0fe";
  });
  zone.addEventListener("dragleave", () => {
    zone.style.borderColor = "#adb5bd";
    zone.style.background  = "#f8f9fa";
  });
  zone.addEventListener("drop", e => {
    e.preventDefault();
    zone.style.borderColor = "#adb5bd";
    zone.style.background  = "#f8f9fa";
    const files = e.dataTransfer?.files;
    if (files && files.length > 0) {
      const dt = new DataTransfer();
      dt.items.add(files[0]);
      fileInput.files = dt.files;
      updateLabel(files[0].name);
      showToast(`Archivo listo: ${files[0].name}`, "info");
    }
  });
  fileInput.addEventListener("change", () => {
    if (fileInput.files?.[0]) updateLabel(fileInput.files[0].name);
  });
}
// =============================================================================
// HISTORIAL DE CARGOS — gestión desde el admin panel
// =============================================================================

async function _adminToggleHistorial() {
  const container = document.getElementById("admin-historial-container");
  const btn = document.getElementById("btn-toggle-historial-admin");
  if (!container) return;

  const isHidden = container.style.display === "none";
  container.style.display = isHidden ? "block" : "none";
  if (btn) btn.innerHTML = isHidden
    ? '<i class="fas fa-chevron-up mr-1"></i>Ocultar historial'
    : '<i class="fas fa-chevron-down mr-1"></i>Ver historial';

  if (isHidden) await _adminLoadHistorial();
}

async function _adminLoadHistorial() {
  const empId = window._adminHistorialEmpId;
  const body = document.getElementById("admin-historial-body");
  if (!empId || !body) return;

  body.innerHTML = '<p class="text-muted small text-center py-2"><i class="fas fa-spinner fa-spin"></i></p>';
  try {
    const res = await fetch(`${API_BASE}/api/rrhh/empleado/${empId}/historial_cargos`);
    const data = await res.json();
    if (!data.historial?.length) {
      body.innerHTML = '<p class="text-muted small text-center py-2">Sin historial registrado.</p>';
      return;
    }
    body.innerHTML = `
      <table class="table table-sm table-bordered mb-0" style="font-size:0.82rem;">
        <thead class="thead-light"><tr><th>Cargo</th><th>Desde</th><th>Hasta</th><th>Motivo</th><th></th></tr></thead>
        <tbody>
          ${data.historial.map(h => `
            <tr>
              <td>${h.cargo}</td>
              <td>${h.fecha_inicio || "—"}</td>
              <td>${h.fecha_fin || '<span class="text-success font-weight-bold">Actual</span>'}</td>
              <td class="text-muted">${h.motivo || "—"}</td>
              <td><button class="btn btn-xs btn-outline-danger" onclick="_adminDeleteCargo(${empId}, ${h.id})"><i class="fas fa-trash"></i></button></td>
            </tr>`).join("")}
        </tbody>
      </table>`;
  } catch {
    body.innerHTML = '<p class="text-danger small text-center py-2">Error al cargar historial.</p>';
  }
}

async function _adminAddCargo() {
  const empId  = window._adminHistorialEmpId;
  const cargo  = document.getElementById("admin-historial-cargo-input")?.value.trim();
  const desde  = document.getElementById("admin-historial-desde-input")?.value;
  const motivo = document.getElementById("admin-historial-motivo-input")?.value.trim();

  if (!cargo || !desde) { showToast("Cargo y fecha de inicio son requeridos.", "warning"); return; }

  try {
    const res = await fetch(`${API_BASE}/api/rrhh/empleado/${empId}/historial_cargos`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ cargo_nombre: cargo, fecha_inicio: desde, motivo: motivo || null, registrado_por: state.user?.username || "" }),
    });
    if (!res.ok) { const e = await res.json(); throw new Error(e.detail || "Error"); }
    document.getElementById("admin-historial-cargo-input").value = "";
    document.getElementById("admin-historial-desde-input").value = "";
    document.getElementById("admin-historial-motivo-input").value = "";
    showToast("Cargo registrado en el historial.", "success");
    await _adminLoadHistorial();
  } catch (e) { showToast(`Error: ${e.message}`, "error"); }
}

async function _adminDeleteCargo(empId, histId) {
  const ok = await confirmModal("Eliminar entrada", "¿Eliminar esta entrada del historial de cargos?", "Sí, eliminar", "btn-danger");
  if (!ok) return;
  try {
    const res = await fetch(`${API_BASE}/api/rrhh/empleado/${empId}/historial_cargos/${histId}`, { method: "DELETE" });
    if (!res.ok) throw new Error();
    showToast("Entrada eliminada.", "success");
    await _adminLoadHistorial();
  } catch { showToast("Error al eliminar.", "error"); }
}
