// --- EDITAR / ELIMINAR DOCUMENTO (ARCHIVO) ---
async function openEditDocModal(id) {
  // Fetch datos frescos del servidor (no depender solo de state cache)
  let rec = state.adminTable.results.find(r => r.id == id) || { id };
  try {
    const res = await fetch(`${API_BASE}/api/admin/documento/${id}?modulo=${encodeURIComponent(state.user.modulo)}`);
    if (res.ok) rec = await res.json();
  } catch { /* usa cachÃ© si falla el fetch */ }

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
  if (soporteEl) soporteEl.value = rec.soporte || "FÃ­sico";
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
  if (!cedula) { showToast("Ingrese una cÃ©dula primero.", "warning"); return; }

  try {
    const res = await fetch(`${API_BASE}/api/rrhh/empleado/por-cedula/${encodeURIComponent(cedula)}`);
    if (res.status === 404) {
      if (hintEl) hintEl.innerHTML = '<span class="text-info"><i class="fas fa-user-plus mr-1"></i>Empleado nuevo â€” complete los datos.</span>';
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

// --- DRAG-AND-DROP UPLOAD EN MODAL DE EDICIÃ“N ---

async function _uploadEditDocFile(file) {
  const ALLOWED = ["pdf","png","jpg","jpeg","tiff","tif","webp"];
  const ext = (file.name.split(".").pop() || "").toLowerCase();
  if (!ALLOWED.includes(ext)) { showToast("Tipo de archivo no permitido.", "warning"); return; }
  if (file.size > 25 * 1024 * 1024) { showToast("El archivo excede el lÃ­mite de 25 MB.", "warning"); return; }

  const status = document.getElementById("edit-doc-upload-status");
  const zone   = document.getElementById("edit-doc-dropzone");
  const urlField = document.getElementById("edit-doc-file-url");

  if (status) status.innerHTML = '<i class="fas fa-spinner fa-spin mr-1"></i>Subiendoâ€¦';
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
    `Â¿Enviar "${nombre}" a la papelera? PodrÃ¡s restaurarlo desde la pestaÃ±a Papelera.`,
    "SÃ­, mover a papelera", "btn-danger"
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
      body.innerHTML = '<tr><td colspan="7" class="text-center text-muted py-3">La papelera estÃ¡ vacÃ­a.</td></tr>';
    } else {
      body.innerHTML = data.records.map((r, i) => `
        <tr>
          <td>${(page - 1) * 20 + i + 1}</td>
          <td>${r.titulo || "â€”"}</td>
          <td><small>${r.doc_type || "â€”"}</small></td>
          <td><small>${r.fecha || "â€”"}</small></td>
          <td><small class="text-muted">${r.deleted_by || "â€”"}</small></td>
          <td><small class="text-muted">${r.deleted_at || "â€”"}</small></td>
          <td>
            <button class="btn btn-xs btn-success mr-1" onclick="_restaurarDoc(${r.id},'${modulo}')" title="Restaurar"><i class="fas fa-undo"></i></button>
            <button class="btn btn-xs btn-danger" onclick="_purgarDoc(${r.id},'${modulo}')" title="Eliminar permanentemente"><i class="fas fa-fire"></i></button>
          </td>
        </tr>`).join("");
    }
    if (summary) summary.textContent = `${data.total} documento(s) en papelera`;
    if (pageInfo) pageInfo.textContent = `PÃ¡g. ${page} / ${Math.max(1, Math.ceil(data.total / 20))}`;
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
          <td>${r.nombre || "â€”"}</td>
          <td><small>${r.cedula || "â€”"}</small></td>
          <td><small class="text-muted">${r.deleted_by || "â€”"}</small></td>
          <td><small class="text-muted">${r.deleted_at || "â€”"}</small></td>
          <td>
            <button class="btn btn-xs btn-success mr-1" onclick="_restaurarEmpleado(${r.id})" title="Restaurar"><i class="fas fa-undo"></i></button>
            <button class="btn btn-xs btn-danger" onclick="_purgarEmpleado(${r.id})" title="Eliminar permanentemente"><i class="fas fa-fire"></i></button>
          </td>
        </tr>`).join("");
    }
    if (summary) summary.textContent = `${data.total} empleado(s) en papelera`;
    if (pageInfo) pageInfo.textContent = `PÃ¡g. ${page} / ${Math.max(1, Math.ceil(data.total / 20))}`;
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
  const ok = await confirmModal("Eliminar permanentemente", "Esta acciÃ³n es irreversible. Â¿Continuar?", "SÃ­, eliminar", "btn-danger");
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
  const ok = await confirmModal("Eliminar empleado permanentemente", "Se eliminarÃ¡n tambiÃ©n todos sus documentos. Esta acciÃ³n es irreversible.", "SÃ­, eliminar", "btn-danger");
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
              <td>${v.comentario || "â€”"}</td>
              <td>${v.subido_por || "â€”"}</td>
              <td>${v.created_at || "â€”"}</td>
              <td>
                <button class="btn btn-xs btn-outline-success mr-1" onclick="_restaurarVersion(${docId},${v.id},'${modulo}')" title="Restaurar esta versiÃ³n"><i class="fas fa-undo"></i></button>
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
    showToast("VersiÃ³n restaurada como archivo actual.", "success");
    const row = await (await fetch(`${API_BASE}/api/admin/documento/${docId}?modulo=${encodeURIComponent(modulo)}`)).json();
    if (row?.file_url) {
      const urlEl = document.getElementById("edit-doc-file-url");
      if (urlEl) { urlEl.value = row.file_url; _refreshEditDocPreview(); }
    }
    loadDocVersiones(docId, modulo);
  } catch { showToast("Error al restaurar versiÃ³n.", "error"); }
}

async function _deleteVersion(docId, verId, modulo) {
  const ok = await confirmModal("Eliminar versiÃ³n", "Â¿Eliminar esta versiÃ³n del historial? No afecta al archivo actual.", "SÃ­, eliminar", "btn-danger");
  if (!ok) return;
  try {
    const res = await fetch(`${API_BASE}/api/admin/documento/${docId}/versiones/${verId}?modulo=${encodeURIComponent(modulo)}&usuario=${encodeURIComponent(state.user?.username || "")}`, { method: "DELETE" });
    if (!res.ok) throw new Error();
    showToast("VersiÃ³n eliminada.", "success");
    loadDocVersiones(docId, modulo);
  } catch { showToast("Error al eliminar versiÃ³n.", "error"); }
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
  if (!currentUrl) { showToast("No hay archivo actual que guardar como versiÃ³n.", "warning"); return; }

  const modulo = isArchivoModule() ? "Archivo" : "RRHH";
  const comentario = await promptModal("Comentario de versiÃ³n", "Describe brevemente el cambio (opcional):");

  try {
    const res = await fetch(
      `${API_BASE}/api/admin/documento/${docId}/versiones?modulo=${encodeURIComponent(modulo)}&file_url=${encodeURIComponent(currentUrl)}&comentario=${encodeURIComponent(comentario || "")}&usuario=${encodeURIComponent(state.user?.username || "")}`,
      { method: "POST" }
    );
    if (!res.ok) throw new Error();
    showToast("VersiÃ³n guardada en el historial.", "success");
    if (document.getElementById("edit-doc-versiones-container")?.style.display !== "none") {
      loadDocVersiones(parseInt(docId), modulo);
    }
  } catch { showToast("Error al guardar versiÃ³n.", "error"); }
}
