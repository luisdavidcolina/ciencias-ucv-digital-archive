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
    draft:      '<span class="badge badge-secondary ds-status-badge" title="Borrador â€” sin publicar"><i class="fas fa-pencil-alt mr-1"></i>Borrador</span>',
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
        ? highlightTerms(f.autor || "â€”", searchTerms)
        : (f.autor || "â€”");
      const statusBtnTitle = { draft: "Borrador", revision: "En revisión", aprobado: "Aprobado", rechazado: "Rechazado" }[f.status] || "Aprobado";
      return `<tr>
        <td class="font-weight-bold text-dark" style="max-width:200px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap" title="${(f.titulo||'').replace(/"/g,'&quot;')}">${titulo}</td>
        <td class="text-muted small">${autor}</td>
        <td class="text-muted small">${formatISOToSpanish(f.fecha)}</td>
        <td><span class="badge badge-light border">${f.doc_type || 'â€”'}</span></td>
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
          <td class="text-muted small">${f.cedula || "â€”"}</td>
          <td class="text-muted small" style="max-width:120px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;">${f.cargo || f.departamento || "â€”"}</td>
          <td><span class="badge" style="background-color:${c};color:white;padding:3px 6px;">${f.estado || "â€”"}</span></td>
          <td><span class="badge badge-light border" title="${f.tipos||''}" style="padding:3px 6px;">${(f.tipos||'').split(';')[0].trim() || 'â€”'}</span></td>
          <td class="text-muted small">${f.ubicacion || "â€”"}</td>
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
               "NÂ° Folio", "Soporte", "NÂ° Páginas", "Ubicación", "Archivo Digital", "Estado", "Resumen"];
    rows = records.map(r => [
      r.id, r.titulo, r.autor, r.fecha, r.doc_type, r.tesauro_secundario || "",
      r.numero_folio || "", r.soporte || "Físico", r.numero_paginas || "",
      r.ubicacion, r.file_url || "", r.status || "aprobado", r.resumen || ""
    ].map(esc).join(","));
  } else {
    headers = ["ID Empleado", "Apellidos y Nombres", "Cédula", "RIF", "Cargo", "Departamento",
               "Estado Laboral", "Fecha Ingreso", "Fecha Nacimiento", "Nivel Educativo", "Sexo",
               "NÂ° Documentos", "Última Actualiz."];
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
  const blob = new Blob(["ï»¿" + csv], { type: "text/csv;charset=utf-8;" });
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
  const zone = document.getElementById(`dropzone-${suf}`) ||
               document.querySelector(`#pane-admin-${suf}-new .ds-dropzone-compact`);
  const fileInput = document.getElementById(`file_upload-${suf}`);
  if (!zone || !fileInput) return;

  const updateLabel = (name) => {
    const label = zone.querySelector(".ds-drop-label");
    if (label) label.textContent = name ? `Archivo: ${name}` : "Arrastra el archivo digital aqui o";
    zone.classList.toggle("has-file", !!name);
    const icon = zone.querySelector("i");
    if (icon) {
      icon.classList.toggle("fa-file-upload", !name);
      icon.classList.toggle("fa-check-circle", !!name);
      icon.classList.toggle("text-secondary", !name);
      icon.classList.toggle("text-success", !!name);
    }
  };

  zone.addEventListener("dragover", e => { e.preventDefault(); zone.classList.add("drag-over"); });
  zone.addEventListener("dragleave", () => zone.classList.remove("drag-over"));
  zone.addEventListener("drop", e => {
    e.preventDefault();
    zone.classList.remove("drag-over");
    const files = e.dataTransfer?.files;
    if (files?.[0]) {
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
