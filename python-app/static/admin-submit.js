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
          <label class="font-weight-bold text-muted">NÂ° de Folio / Signatura</label>
          <input type="text" id="reg-folio-${suf}" class="form-control" placeholder="Ej: F-023, Carp-A-12">
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Soporte</label>
          <select id="reg-soporte-${suf}" class="form-control">
            ${(state.choices?.archivo?.soportes || ["Físico","Digital","Digitalizado"]).map(s => `<option value="${s}">${s}</option>`).join("")}
          </select>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">NÂ° de Páginas</label>
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

