// --- EDITAR / ELIMINAR DOCUMENTO (ARCHIVO) ---

// OA-173: se recuerda qué elemento abrió el modal para devolverle el foco al cerrar.
let _editDocOpener = null;
let _editDocDirty = false;

function _markEditDocDirty() { _editDocDirty = true; }

(function _wireEditDocModalFocusAndDirty() {
  const wire = () => {
    const modalEl = document.getElementById("editArchivoModal");
    if (!modalEl) return;
    modalEl.addEventListener("hidden.bs.modal", () => {
      if (_editDocOpener && document.body.contains(_editDocOpener)) {
        _editDocOpener.focus();
      }
      _editDocOpener = null;
      _editDocDirty = false;
    });
    // OA-182: Cancelar, Escape o clic fuera con cambios sin guardar piden confirmación.
    let _editDocCloseConfirmed = false;
    $(modalEl).on("hide.bs.modal", event => {
      if (!_editDocDirty || _editDocCloseConfirmed) { _editDocCloseConfirmed = false; return; }
      event.preventDefault();
      confirmModal(
        "Cambios sin guardar",
        "Hay cambios en el formulario que se perderán. ¿Cerrar de todos modos?",
        "Sí, descartar", "btn-danger"
      ).then(ok => {
        if (ok) {
          _editDocDirty = false;
          _editDocCloseConfirmed = true;
          $(modalEl).modal("hide");
        }
      });
    });
    // Cualquier cambio en un campo del formulario marca el modal como sucio (OA-182).
    const form = modalEl.querySelector("form") || modalEl;
    form.addEventListener("input", _markEditDocDirty);
    form.addEventListener("change", _markEditDocDirty);
  };
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", wire);
  } else {
    wire();
  }
})();

async function openEditDocModal(id) {
  _editDocOpener = document.activeElement instanceof HTMLElement ? document.activeElement : null;
  _editDocDirty = false;

  // Fetch datos frescos del servidor (no depender solo de state cache)
  let rec = state.adminTable.results.find(r => r.id == id) || { id };
  try {
    rec = await apiFetchJSON(`${API_BASE}/api/admin/documento/${id}?modulo=${encodeURIComponent(state.user.modulo)}`);
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
    const url = _secureFileUrl(rec.file_url || "");
    if (!url) {
      previewContainer.innerHTML = '<p class="text-muted small mb-0">Sin archivo adjunto.</p>';
    } else if (/\.(pdf)$/i.test(url)) {
      previewContainer.innerHTML = `<iframe src="${escHtml(url)}" class="ds-edit-preview-frame" title="Preview PDF"></iframe>`;
    } else if (/\.(png|jpe?g|gif|webp|svg)$/i.test(url)) {
      previewContainer.innerHTML = `<img src="${escHtml(url)}" class="ds-edit-preview-img" alt="Preview">`;
    } else {
      previewContainer.innerHTML = `<a href="${escHtml(url)}" target="_blank" rel="noopener noreferrer" class="btn btn-sm btn-outline-secondary"><i class="fas fa-external-link-alt mr-1"></i>Abrir archivo</a>`;
    }
  }

  // Poblar select de tipos
  const sel = document.getElementById("edit-doc-type");
  if (sel) {
    const types = state.choices?.archivo?.doc_types || [];
    sel.innerHTML = types.map(t => `<option value="${escHtml(t)}"${t === rec.doc_type ? " selected" : ""}>${escHtml(t)}</option>`).join("");
    if (!types.includes(rec.doc_type) && rec.doc_type) {
      sel.innerHTML = `<option value="${escHtml(rec.doc_type)}" selected>${escHtml(rec.doc_type)}</option>` + sel.innerHTML;
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
  const vencEl = document.getElementById("edit-doc-vencimiento");
  if (vencEl) vencEl.value = rec.fecha_vencimiento || "";

  // Reset drop zone state from a previous upload (OA-190/OA-191: clases, no `style`)
  const dz = document.getElementById("edit-doc-dropzone");
  if (dz) dz.classList.remove("is-dragover", "is-uploading", "is-ok", "is-error");
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
    let emp;
    try {
      emp = await apiFetchJSON(`${API_BASE}/api/rrhh/empleado/por-cedula/${encodeURIComponent(cedula)}`);
    } catch (err) {
      if (err.message?.includes("404") || err.message?.includes("422")) {
        if (hintEl) hintEl.innerHTML = '<span class="text-info"><i class="fas fa-user-plus mr-1"></i>Empleado nuevo — complete los datos.</span>';
        return;
      }
      throw err;
    }

    // Rellenar campos del formulario
    const setVal = (id, v) => { const el = document.getElementById(id); if (el) el.value = v || ""; };
    setVal(`reg-nombres-${suf}`,  emp.nombres || "");
    setVal(`reg-apellidos-${suf}`, emp.apellidos || "");
    setVal(`reg-cargo-${suf}`,    emp.cargo || "");
    setVal(`reg-depto-${suf}`,    emp.departamento || "");
    setVal(`reg-rif-${suf}`,      emp.rif || "");
    const estadoSel = document.getElementById(`reg-estado-${suf}`);
    if (estadoSel && emp.estado) estadoSel.value = emp.estado;

    if (hintEl) hintEl.innerHTML = `<span class="text-success"><i class="fas fa-check-circle mr-1"></i>Empleado encontrado: ${escHtml(emp.nombres)} ${escHtml(emp.apellidos)}. Datos prellenados.</span>`;
    showToast(`Datos de ${emp.nombres} ${emp.apellidos} cargados.`, "info");
  } catch {
    if (hintEl) hintEl.innerHTML = '<span class="text-danger">Error al buscar el empleado.</span>';
  }
}

function _refreshEditDocPreview() {
  const url = _secureFileUrl((document.getElementById("edit-doc-file-url")?.value || "").trim());
  const container = document.getElementById("edit-doc-file-preview");
  if (!container) return;
  if (!url) {
    container.innerHTML = '<p class="text-muted small mb-0">Sin archivo adjunto.</p>';
    return;
  }
  if (/\.(pdf)$/i.test(url)) {
    container.innerHTML = `<iframe src="${escHtml(url)}" class="ds-edit-preview-frame" title="Preview PDF"></iframe>`;
  } else if (/\.(png|jpe?g|gif|webp|svg)$/i.test(url)) {
    container.innerHTML = `<img src="${escHtml(url)}" class="ds-edit-preview-img" alt="Preview">`;
  } else {
    container.innerHTML = `<a href="${escHtml(url)}" target="_blank" rel="noopener noreferrer" class="btn btn-sm btn-outline-primary"><i class="fas fa-external-link-alt mr-1"></i>Abrir en nueva ventana</a>`;
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
  // OA-009: si se reemplaza un archivo ya guardado, se archiva como versión antes de
  // pisarlo — sin esto, sustituir un escaneo por otro mejor lo hacía desaparecer del
  // sistema sin dejar rastro, pese a existir ya el endpoint de versiones.
  const previousUrl = (urlField?.value || "").trim();
  const docId = document.getElementById("edit-doc-id")?.value;

  if (status) status.innerHTML = '<i class="fas fa-spinner fa-spin mr-1"></i>Subiendo…';
  // OA-190/OA-191: el estado se marca con clases, nunca escribiendo `style` a mano.
  if (zone) { zone.classList.remove("is-dragover", "is-ok", "is-error"); zone.classList.add("is-uploading"); }

  const fd = new FormData();
  fd.append("file", file);
  fd.append("modulo", isArchivoModule() ? "archivo" : "rrhh");
  fd.append("usuario", state.user?.username || "");

  try {
    const data = await apiFetchJSON(`${API_BASE}/api/admin/upload`, { method: "POST", body: fd });
    if (docId && previousUrl) {
      const modulo = isArchivoModule() ? "Archivo" : "RRHH";
      try {
        await apiFetch(
          `${API_BASE}/api/admin/documento/${docId}/versiones?modulo=${encodeURIComponent(modulo)}&file_url=${encodeURIComponent(previousUrl)}&comentario=${encodeURIComponent("Reemplazado automáticamente al subir un archivo nuevo")}&usuario=${encodeURIComponent(state.user?.username || "")}`,
          { method: "POST" }
        );
      } catch {
        showToast("El archivo se subió, pero no se pudo archivar la versión anterior.", "warning");
      }
    }
    if (urlField) urlField.value = data.file_url;
    if (status) status.innerHTML = `<span class="text-success"><i class="fas fa-check-circle mr-1"></i>${escHtml(file.name)} subido</span>`;
    if (zone) { zone.classList.remove("is-uploading", "is-dragover", "is-error"); zone.classList.add("is-ok"); }
    _refreshEditDocPreview();
    _markEditDocDirty();
    showToast("Archivo subido correctamente.", "success");
    if (document.getElementById("edit-doc-versiones-container")?.style.display !== "none" && docId) {
      loadDocVersiones(parseInt(docId), isArchivoModule() ? "Archivo" : "RRHH");
    }
  } catch (e) {
    if (status) { status.innerHTML = `<span class="text-danger"><i class="fas fa-times-circle mr-1"></i></span>`; status.querySelector("span").append(e.message); }
    if (zone) { zone.classList.remove("is-uploading", "is-dragover", "is-ok"); zone.classList.add("is-error"); }
    showToast(`Error al subir: ${e.message}`, "error");
  }
}

function _handleEditDocDrop(event) {
  event.preventDefault();
  const zone = document.getElementById("edit-doc-dropzone");
  if (zone) zone.classList.remove("is-dragover");
  const file = event.dataTransfer?.files?.[0];
  if (file) _uploadEditDocFile(file);
}

// OA-191: helpers listos para que el marcado use clases en vez de `ondragover`/
// `ondragleave` con `style` en línea (pendiente en admin_archive.html, ver _BUZON.md).
function _handleEditDocDragOver(event) {
  event.preventDefault();
  document.getElementById("edit-doc-dropzone")?.classList.add("is-dragover");
}
function _handleEditDocDragLeave() {
  document.getElementById("edit-doc-dropzone")?.classList.remove("is-dragover");
}

// OA-191: red de seguridad para cuando el archivo se suelta fuera de la zona y
// `dragleave` nunca llega — sin esto la zona se queda marcada "encima" para siempre.
document.addEventListener("dragend", () => {
  document.getElementById("edit-doc-dropzone")?.classList.remove("is-dragover");
});
document.addEventListener("drop", event => {
  const zone = document.getElementById("edit-doc-dropzone");
  if (zone && !zone.contains(event.target)) zone.classList.remove("is-dragover");
});

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
    fecha_vencimiento:  document.getElementById("edit-doc-vencimiento")?.value || null,
    usuario:            state.user.username,
  };

  // Limpia marcas de error previas de un intento anterior.
  document.querySelectorAll("#editArchivoModal .is-invalid").forEach(el => el.classList.remove("is-invalid"));

  try {
    await apiFetchJSON(`${API_BASE}/api/admin/documento/${id}`, {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload),
    });
    _editDocDirty = false;
    $("#editArchivoModal").modal("hide");
    showToast("Documento actualizado.", "success");
    try {
      // OA-027: actualización optimista de la fila en vez de recargar toda la
      // tabla — evita que un fallo de red posterior al guardado exitoso deje
      // la fila con datos viejos sin avisar.
      if (typeof updateMonitorRowOptimistic === "function") {
        updateMonitorRowOptimistic(parseInt(id), payload);
      } else {
        loadMonitorTable();
      }
    } catch {
      showToast("El documento se guardó, pero la tabla no se pudo refrescar. Recarga la pestaña.", "warning");
    }
  } catch (e) {
    // OA-026: se muestra el detalle real del servidor (422, 409, red…), no un mensaje
    // genérico, y si el detalle nombra un campo se marca en el formulario.
    const detail = e?.message || "Error al actualizar el documento.";
    showToast(detail, "error");
    const fieldMatch = /^([a-z_]+):/i.exec(detail);
    if (fieldMatch) {
      const fieldEl = document.getElementById(`edit-doc-${fieldMatch[1].replace(/_/g, "-")}`);
      if (fieldEl) {
        fieldEl.classList.add("is-invalid");
        let feedback = fieldEl.parentElement?.querySelector(".invalid-feedback");
        if (!feedback) {
          feedback = document.createElement("div");
          feedback.className = "invalid-feedback d-block";
          fieldEl.insertAdjacentElement("afterend", feedback);
        }
        feedback.textContent = detail;
      }
    }
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
    await apiFetchJSON(`${API_BASE}/api/admin/documento/${id}?modulo=${encodeURIComponent(state.user.modulo)}&usuario=${encodeURIComponent(state.user.username)}`, {
      method: "DELETE",
    });
    showToast("Documento movido a la papelera.", "success");
    state.adminTable.page = 1;
    loadMonitorTable();
  } catch {
    showToast("Error al mover a la papelera.", "error");
  }
}

// --- PAPELERA DE RECICLAJE ---

const PAPELERA_PER_PAGE = 20;
const _papeleraState = { archivo: { page: 1, total: 0 }, rrhh: { page: 1, total: 0 }, empleados: { page: 1, total: 0 } };

// OA-140/OR-180: acota, deshabilita en los extremos y muestra «Mostrando N–M de T»
// en vez de sólo el total, con el mismo criterio que el monitor (OR-129).
function _updatePapeleraPager(suf, count) {
  const { page, total } = _papeleraState[suf];
  const summary = document.getElementById(`papelera-summary-${suf}`);
  const pageInfo = document.getElementById(`papelera-page-info-${suf}`);
  const totalPages = Math.max(1, Math.ceil(total / PAPELERA_PER_PAGE));
  if (summary) {
    if (!total) {
      summary.textContent = "Mostrando 0 registros";
    } else {
      const from = (page - 1) * PAPELERA_PER_PAGE + 1;
      const to = from + count - 1;
      summary.textContent = `Mostrando ${from}–${to} de ${total}`;
    }
  }
  if (pageInfo) pageInfo.textContent = `Pág. ${page} / ${totalPages}`;
  if (pageInfo) {
    const prevBtn = pageInfo.previousElementSibling;
    const nextBtn = pageInfo.nextElementSibling;
    if (prevBtn && prevBtn.tagName === "BUTTON") {
      prevBtn.disabled = page <= 1;
      prevBtn.setAttribute("aria-label", "Página anterior de la papelera");
    }
    if (nextBtn && nextBtn.tagName === "BUTTON") {
      nextBtn.disabled = page >= totalPages;
      nextBtn.setAttribute("aria-label", "Página siguiente de la papelera");
    }
  }
}

// OA-194/OR-182: quita la fila con una transición en vez de recargar toda la
// papelera (dos peticiones por acción, parpadeo y pérdida del punto de lectura).
function _removePapeleraRow(row, suf) {
  if (!row) return;
  _papeleraState[suf].total = Math.max(0, _papeleraState[suf].total - 1);
  row.classList.add("ds-row-removing");
  const done = () => {
    const body = row.parentElement;
    row.remove();
    if (body && !body.children.length) {
      const colspan = suf === "empleados" ? 6 : 7;
      const emptyMsg = suf === "empleados" ? "No hay empleados en la papelera." : "La papelera está vacía.";
      body.innerHTML = `<tr><td colspan="${colspan}" class="text-center text-muted py-3">${emptyMsg}</td></tr>`;
    }
    _updatePapeleraPager(suf, body ? body.querySelectorAll("tr[data-papelera-row]").length : 0);
  };
  row.addEventListener("transitionend", done, { once: true });
  // Salvaguarda por si la transición no está definida en el CSS de este entorno.
  setTimeout(done, 400);
}

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
  if (!body) return;

  body.innerHTML = '<tr><td colspan="7" class="text-center py-2"><i class="fas fa-spinner fa-spin"></i></td></tr>';
  try {
    const data = await apiFetchJSON(`${API_BASE}/api/admin/papelera?modulo=${encodeURIComponent(modulo)}&page=${page}&per_page=${PAPELERA_PER_PAGE}`);
    _papeleraState[suf].total = data.total;
    if (!data.records?.length) {
      body.innerHTML = '<tr><td colspan="7" class="text-center text-muted py-3">La papelera está vacía.</td></tr>';
    } else {
      body.innerHTML = data.records.map((r, i) => `
        <tr data-papelera-row data-id="${r.id}">
          <td>${(page - 1) * PAPELERA_PER_PAGE + i + 1}</td>
          <td>${escHtml(r.titulo || "—")}</td>
          <td><small>${escHtml(r.doc_type || "—")}</small></td>
          <td><small>${escHtml(r.fecha || "—")}</small></td>
          <td><small class="text-muted">${escHtml(r.deleted_by || "—")}</small></td>
          <td><small class="text-muted">${escHtml(r.deleted_at || "—")}</small></td>
          <td>
            <button class="btn btn-xs btn-success mr-1" onclick="_restaurarDoc(${r.id},${escHtml(JSON.stringify(modulo))},this)" title="Restaurar"><i class="fas fa-undo"></i></button>
            <button class="btn btn-xs btn-danger" onclick="_purgarDoc(${r.id},${escHtml(JSON.stringify(modulo))},${escHtml(JSON.stringify(r.titulo || ""))},this)" title="Eliminar permanentemente"><i class="fas fa-fire"></i></button>
          </td>
        </tr>`).join("");
    }
    _updatePapeleraPager(suf, data.records?.length || 0);
  } catch {
    body.innerHTML = '<tr><td colspan="7" class="text-danger text-center py-2">Error al cargar la papelera.</td></tr>';
  }
}

async function _loadPapeleraEmpleados() {
  const page = _papeleraState.empleados.page;
  const body = document.getElementById("papelera-body-empleados");
  if (!body) return;

  body.innerHTML = '<tr><td colspan="6" class="text-center py-2"><i class="fas fa-spinner fa-spin"></i></td></tr>';
  try {
    const data = await apiFetchJSON(`${API_BASE}/api/admin/papelera/empleados?page=${page}&per_page=${PAPELERA_PER_PAGE}`);
    _papeleraState.empleados.total = data.total;
    if (!data.records?.length) {
      body.innerHTML = '<tr><td colspan="6" class="text-center text-muted py-3">No hay empleados en la papelera.</td></tr>';
    } else {
      body.innerHTML = data.records.map((r, i) => `
        <tr data-papelera-row data-id="${r.id}">
          <td>${(page - 1) * PAPELERA_PER_PAGE + i + 1}</td>
          <td>${escHtml(r.nombre || "—")}</td>
          <td><small>${escHtml(r.cedula || "—")}</small></td>
          <td><small class="text-muted">${escHtml(r.deleted_by || "—")}</small></td>
          <td><small class="text-muted">${escHtml(r.deleted_at || "—")}</small></td>
          <td>
            <button class="btn btn-xs btn-success mr-1" onclick="_restaurarEmpleado(${r.id},this)" title="Restaurar"><i class="fas fa-undo"></i></button>
            <button class="btn btn-xs btn-danger" onclick="_purgarEmpleado(${r.id},${escHtml(JSON.stringify(r.cedula || ""))},this)" title="Eliminar permanentemente"><i class="fas fa-fire"></i></button>
          </td>
        </tr>`).join("");
    }
    _updatePapeleraPager("empleados", data.records?.length || 0);
  } catch {
    body.innerHTML = '<tr><td colspan="6" class="text-danger text-center py-2">Error al cargar.</td></tr>';
  }
}

async function changePapeleraPage(dir, suf) {
  const st = _papeleraState[suf];
  const totalPages = Math.max(1, Math.ceil(st.total / PAPELERA_PER_PAGE));
  st.page = Math.min(totalPages, Math.max(1, st.page + dir));
  if (suf === "empleados") await _loadPapeleraEmpleados();
  else await _loadPapeleraDocumentos(suf === "archivo" ? "Archivo" : "RRHH", suf);
}

// OR-178: restaurar pide confirmación igual que purgar, proporcional al efecto
// (restaurar es reversible con un nuevo borrado, así que el texto es más ligero).
async function _restaurarDoc(id, modulo, btnEl) {
  const ok = await confirmModal(
    "Restaurar documento",
    "El documento volverá a estar visible en su módulo. ¿Continuar?",
    "Sí, restaurar", "btn-success"
  );
  if (!ok) return;
  const suf = modulo === "Archivo" ? "archivo" : "rrhh";
  try {
    await apiFetch(`${API_BASE}/api/admin/papelera/${id}/restaurar?modulo=${encodeURIComponent(modulo)}&usuario=${encodeURIComponent(state.user?.username || "")}`, { method: "POST" });
    // OA-137: se indica a qué pasa y se ofrece ir a verlo, no sólo «restaurado».
    showToast(`Documento restaurado y visible de nuevo en ${modulo}.`, "success");
    _removePapeleraRow(btnEl?.closest("tr"), suf);
  } catch { showToast("Error al restaurar.", "error"); }
}

// OA-046: purgar es irreversible y estaba a un solo clic de distancia del botón de
// restaurar, con el mismo texto genérico que cualquier otra confirmación. Se exige
// escribir el título, igual que OR-176 ya exige la cédula al purgar un empleado.
async function _purgarDoc(id, modulo, titulo, btnEl) {
  const ok = await confirmModal(
    "Eliminar documento permanentemente",
    "Esta acción es irreversible: se eliminarán el archivo digital, todas sus versiones y sus palabras clave asociadas. ¿Continuar?",
    "Sí, eliminar", "btn-danger"
  );
  if (!ok) return;
  if (titulo) {
    const typed = await promptModal(
      "Confirmar eliminación",
      `Para confirmar, escriba el título del documento (${titulo}):`
    );
    if (typed === null) return;
    if (typed.trim() !== String(titulo).trim()) {
      showToast("El título no coincide. No se eliminó nada.", "warning");
      return;
    }
  }
  const suf = modulo === "Archivo" ? "archivo" : "rrhh";
  try {
    await apiFetch(`${API_BASE}/api/admin/papelera/${id}/purgar?modulo=${encodeURIComponent(modulo)}&usuario=${encodeURIComponent(state.user?.username || "")}`, { method: "DELETE" });
    showToast("Documento eliminado permanentemente.", "success");
    _removePapeleraRow(btnEl?.closest("tr"), suf);
  } catch { showToast("Error al purgar.", "error"); }
}

async function _restaurarEmpleado(id, btnEl) {
  const ok = await confirmModal(
    "Restaurar empleado",
    "El empleado y su expediente volverán a estar visibles en el buscador y la plantilla. ¿Continuar?",
    "Sí, restaurar", "btn-success"
  );
  if (!ok) return;
  try {
    await apiFetch(`${API_BASE}/api/admin/papelera/empleados/${id}/restaurar?usuario=${encodeURIComponent(state.user?.username || "")}`, { method: "POST" });
    showToast("Empleado restaurado.", "success");
    _removePapeleraRow(btnEl?.closest("tr"), "empleados");
  } catch { showToast("Error al restaurar empleado.", "error"); }
}

// OR-176: la confirmación de purgar un empleado enumera lo que se destruye y exige
// escribir la cédula, igual que pide OR-012 — no el mismo texto genérico que un documento.
async function _purgarEmpleado(id, cedula, btnEl) {
  const ok = await confirmModal(
    "Eliminar empleado permanentemente",
    "Esta acción es irreversible: se eliminarán el expediente completo del empleado, su historial de cargos y todas sus versiones de archivo. ¿Continuar?",
    "Sí, eliminar", "btn-danger"
  );
  if (!ok) return;
  if (cedula) {
    const typed = await promptModal(
      "Confirmar eliminación",
      `Para confirmar, escriba la cédula del empleado (${cedula}):`
    );
    if (typed === null) return;
    if (typed.trim() !== String(cedula).trim()) {
      showToast("La cédula no coincide. No se eliminó nada.", "warning");
      return;
    }
  }
  try {
    await apiFetch(`${API_BASE}/api/admin/papelera/empleados/${id}/purgar?usuario=${encodeURIComponent(state.user?.username || "")}`, { method: "DELETE" });
    showToast("Empleado eliminado permanentemente.", "success");
    _removePapeleraRow(btnEl?.closest("tr"), "empleados");
  } catch { showToast("Error al purgar empleado.", "error"); }
}

// OA-138: exportación CSV de la papelera con los mismos campos de la tabla.
// Pendiente: falta un botón en admin_archive.html que llame a esto (ver _BUZON.md).
function _csvEscape(v) {
  const s = String(v ?? "");
  return /[",\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
}

async function _exportPapelera(suf) {
  try {
    const rows = [];
    if (suf === "empleados") {
      const data = await apiFetchJSON(`${API_BASE}/api/admin/papelera/empleados?page=1&per_page=1000`);
      rows.push(["Nombre", "Cédula", "Borrado por", "Fecha de borrado"]);
      (data.records || []).forEach(r => rows.push([r.nombre, r.cedula, r.deleted_by, r.deleted_at]));
    } else {
      const modulo = suf === "archivo" ? "Archivo" : "RRHH";
      const data = await apiFetchJSON(`${API_BASE}/api/admin/papelera?modulo=${encodeURIComponent(modulo)}&page=1&per_page=1000`);
      rows.push(["Título", "Tipo", "Fecha", "Borrado por", "Fecha de borrado"]);
      (data.records || []).forEach(r => rows.push([r.titulo, r.doc_type, r.fecha, r.deleted_by, r.deleted_at]));
    }
    const csv = rows.map(row => row.map(_csvEscape).join(",")).join("\n");
    const blob = new Blob([`﻿${csv}`], { type: "text/csv;charset=utf-8;" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = `papelera-${suf}-${new Date().toISOString().slice(0, 10)}.csv`;
    document.body.appendChild(a);
    a.click();
    a.remove();
    URL.revokeObjectURL(url);
  } catch {
    showToast("Error al exportar la papelera.", "error");
  }
}

// --- VERSIONES DE ARCHIVOS DIGITALES ---

async function loadDocVersiones(docId, modulo) {
  const container = document.getElementById("edit-doc-versiones-body");
  if (!container) return;
  container.innerHTML = '<p class="text-muted small text-center py-2"><i class="fas fa-spinner fa-spin"></i></p>';
  try {
    const data = await apiFetchJSON(`${API_BASE}/api/admin/documento/${docId}/versiones?modulo=${encodeURIComponent(modulo)}`);
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
              <td><span class="badge badge-secondary">v${Number(v.version_num)}</span></td>
              <td>${escHtml(v.comentario || "—")}</td>
              <td>${escHtml(v.subido_por || "—")}</td>
              <td>${escHtml(v.created_at || "—")}</td>
              <td>
                <button class="btn btn-xs btn-outline-success mr-1" onclick="_restaurarVersion(${docId},${v.id},${escHtml(JSON.stringify(modulo))})" title="Restaurar esta versión"><i class="fas fa-undo"></i></button>
                <button class="btn btn-xs btn-outline-danger" onclick="_deleteVersion(${docId},${v.id},${escHtml(JSON.stringify(modulo))})" title="Eliminar del historial"><i class="fas fa-trash"></i></button>
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
    await apiFetch(`${API_BASE}/api/admin/documento/${docId}/versiones/${verId}/restaurar?modulo=${encodeURIComponent(modulo)}&usuario=${encodeURIComponent(state.user?.username || "")}`, { method: "POST" });
    showToast("Versión restaurada como archivo actual.", "success");
    const row = await apiFetchJSON(`${API_BASE}/api/admin/documento/${docId}?modulo=${encodeURIComponent(modulo)}`);
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
    await apiFetch(`${API_BASE}/api/admin/documento/${docId}/versiones/${verId}?modulo=${encodeURIComponent(modulo)}&usuario=${encodeURIComponent(state.user?.username || "")}`, { method: "DELETE" });
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
    await apiFetch(
      `${API_BASE}/api/admin/documento/${docId}/versiones?modulo=${encodeURIComponent(modulo)}&file_url=${encodeURIComponent(currentUrl)}&comentario=${encodeURIComponent(comentario || "")}&usuario=${encodeURIComponent(state.user?.username || "")}`,
      { method: "POST" }
    );
    showToast("Versión guardada en el historial.", "success");
    if (document.getElementById("edit-doc-versiones-container")?.style.display !== "none") {
      loadDocVersiones(parseInt(docId), modulo);
    }
  } catch { showToast("Error al guardar versión.", "error"); }
}
