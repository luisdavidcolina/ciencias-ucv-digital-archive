
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
    `Â¿Enviar el expediente de "${nombre}" a la papelera? PodrÃ¡s restaurarlo desde la pestaÃ±a Papelera.`,
    "SÃ­, mover a papelera", "btn-danger"
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
    // Incluye campos ISAD(G): folio, soporte, pÃ¡ginas
    headers = ["ID", "TÃ­tulo", "Autor", "Fecha", "TipologÃ­a", "ClasificaciÃ³n",
               "NÂ° Folio", "Soporte", "NÂ° PÃ¡ginas", "UbicaciÃ³n", "Archivo Digital", "Estado", "Resumen"];
    rows = records.map(r => [
      r.id, r.titulo, r.autor, r.fecha, r.doc_type, r.tesauro_secundario || "",
      r.numero_folio || "", r.soporte || "FÃ­sico", r.numero_paginas || "",
      r.ubicacion, r.file_url || "", r.status || "aprobado", r.resumen || ""
    ].map(esc).join(","));
  } else {
    headers = ["ID Empleado", "Apellidos y Nombres", "CÃ©dula", "RIF", "Cargo", "Departamento",
               "Estado Laboral", "Fecha Ingreso", "Fecha Nacimiento", "Nivel Educativo", "Sexo",
               "NÂ° Documentos", "Ãšltima Actualiz."];
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
  const meta = `"# Exportado: ${today} | MÃ³dulo: ${state.user.modulo} | PÃ¡g: ${state.adminTable.page} | Total: ${state.adminTable.total}"`;
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

// â”€â”€â”€ Drag & Drop en zona de carga â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
function initDropZone(suf) {
  const zone = document.querySelector(`#pane-admin-${suf}-new [style*="dashed"]`);
  const fileInput = document.getElementById(`file_upload-${suf}`);
  if (!zone || !fileInput) return;

  const updateLabel = (name) => {
    const label = zone.querySelector(".ds-drop-label") || zone.querySelector("p.text-muted");
    if (label) label.textContent = name ? `ðŸ“„ ${name}` : "Arrastra aquÃ­ o selecciona archivos";
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
// HISTORIAL DE CARGOS â€” gestiÃ³n desde el admin panel
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
              <td>${h.fecha_inicio || "â€”"}</td>
              <td>${h.fecha_fin || '<span class="text-success font-weight-bold">Actual</span>'}</td>
              <td class="text-muted">${h.motivo || "â€”"}</td>
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
  const ok = await confirmModal("Eliminar entrada", "Â¿Eliminar esta entrada del historial de cargos?", "SÃ­, eliminar", "btn-danger");
  if (!ok) return;
  try {
    const res = await fetch(`${API_BASE}/api/rrhh/empleado/${empId}/historial_cargos/${histId}`, { method: "DELETE" });
    if (!res.ok) throw new Error();
    showToast("Entrada eliminada.", "success");
    await _adminLoadHistorial();
  } catch { showToast("Error al eliminar.", "error"); }
}
