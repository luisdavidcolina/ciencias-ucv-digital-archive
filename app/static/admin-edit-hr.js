
// --- EDITAR / ELIMINAR EMPLEADO (RRHH) ---

// OR-219: se recuerda qué elemento abrió el modal para devolverle el foco al cerrar,
// igual que `admin-edit.js` ya hace para `editArchivoModal`.
let _editEmpOpener = null;

(function _wireEditEmpModalFocus() {
  const wire = () => {
    const modalEl = document.getElementById("editEmpleadoModal");
    if (!modalEl) return;
    modalEl.addEventListener("hidden.bs.modal", () => {
      if (_editEmpOpener && document.body.contains(_editEmpOpener)) {
        _editEmpOpener.focus();
      }
      _editEmpOpener = null;
    });
  };
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", wire);
  } else {
    wire();
  }
})();

async function openEditEmpleadoModal(empId) {
  _editEmpOpener = document.activeElement instanceof HTMLElement ? document.activeElement : null;

  // OR-147: la fila de la tabla no trae fecha_nacimiento/sexo/foto_url (no se listan
  // en el monitor). Si el fetch fresco falla y se abre igual con esos huecos, guardar
  // sin tocarlos los pone en null y borra datos que sí existían en el servidor.
  let rec;
  try {
    rec = await apiFetchJSON(`${API_BASE}/api/admin/empleado/${empId}`);
  } catch {
    showToast("No se pudo cargar el expediente del empleado. Reintente.", "error");
    return;
  }

  document.getElementById("edit-emp-id").value          = rec.empleado_id || rec.id || "";
  document.getElementById("edit-emp-nombres").value     = rec.nombres || "";
  document.getElementById("edit-emp-apellidos").value   = rec.apellidos || "";
  document.getElementById("edit-emp-cargo").value       = rec.cargo || "";
  document.getElementById("edit-emp-departamento").value = rec.departamento || "";
  const estadoSel = document.getElementById("edit-emp-estado");
  if (estadoSel) {
    const estadosCat = state.choices?.rrhh?.estados_catalog || [];
    if (estadosCat.length) {
      estadoSel.innerHTML = estadosCat.map(e => `<option value="${escHtml(e)}">${escHtml(e)}</option>`).join("");
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

  // OR-153: se recuerda si el historial estaba desplegado en la última ficha abierta,
  // y se carga junto con los datos personales en vez de esperar al clic — antes había
  // que abrirlo a mano cada vez, dos clics extra por persona en el trabajo habitual de
  // depurar una carrera.
  const histContainer = document.getElementById("admin-historial-container");
  const histBtn = document.getElementById("btn-toggle-historial-admin");
  const expandido = _historialExpandidoPorDefecto;
  if (histContainer) { histContainer.style.display = expandido ? "block" : "none"; }
  if (histBtn) {
    histBtn.innerHTML = expandido
      ? '<i class="fas fa-chevron-up mr-1"></i>Ocultar historial'
      : '<i class="fas fa-chevron-down mr-1"></i>Ver historial';
  }
  window._adminHistorialEmpId = rec.empleado_id || rec.id;
  await _adminLoadHistorial();

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
    await apiFetchJSON(`${API_BASE}/api/admin/empleado/${empId}`, {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload),
    });
    $("#editEmpleadoModal").modal("hide");
    showToast("Empleado actualizado.", "success");
    loadMonitorTable();
  } catch (e) {
    showToast(e.message || "Error al actualizar el empleado.", "error");
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
    await apiFetch(`${API_BASE}/api/admin/empleado/${empId}?usuario=${encodeURIComponent(state.user.username)}`, {
      method: "DELETE",
    });
    showToast("Expediente movido a la papelera.", "success");
    loadMonitorTable();
  } catch (e) {
    showToast(e.message || "Error al mover a la papelera.", "error");
  }
}

// OR-285: este archivo redefinía exportAdminCSV() con una versión vieja (sólo la
// página visible, sin BOM real ni exportación del conjunto filtrado completo) que,
// por cargarse después de admin-monitor.js, ganaba siempre y revertía en silencio
// las correcciones OR-137/OR-138/OR-139 ya aplicadas allí. Se retira la redefinición;
// admin-monitor.js ya trae la versión correcta y la usan ambos módulos.

// ─── Drag & Drop en zona de carga ───────────────────────────────────────────
// OR-005/OR-285: este archivo redefinía initDropZone() con una versión vieja que
// apuntaba a `[style*="dashed"]`, un bloque que ya no existe en admin_hr.html
// (quedó como `display:none`). Como admin-edit-hr.js se carga DESPUÉS de
// admin-monitor.js, esta redefinición ganaba y dejaba muerto el arrastre del
// alta. admin-monitor.js ya trae la versión correcta (apunta a
// `#dropzone-${suf}` / `.ds-dropzone-compact`), así que aquí no se redefine.
// =============================================================================
// HISTORIAL DE CARGOS — gestión desde el admin panel
// =============================================================================

// OR-153: recuerda el estado (abierto/cerrado) entre una ficha y la siguiente.
let _historialExpandidoPorDefecto = false;

async function _adminToggleHistorial() {
  const container = document.getElementById("admin-historial-container");
  const btn = document.getElementById("btn-toggle-historial-admin");
  if (!container) return;

  const isHidden = container.style.display === "none";
  container.style.display = isHidden ? "block" : "none";
  _historialExpandidoPorDefecto = isHidden;
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
    const data = await apiFetchJSON(`${API_BASE}/api/rrhh/empleado/${empId}/historial_cargos`);
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
              <td>${escHtml(h.cargo)}</td>
              <td>${h.fecha_inicio ? escHtml(formatISOToSpanish(h.fecha_inicio)) : "—"}</td>
              <td>${h.fecha_fin ? escHtml(formatISOToSpanish(h.fecha_fin)) : '<span class="text-success font-weight-bold">Actual</span>'}</td>
              <td class="text-muted">${escHtml(h.motivo || "—")}</td>
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
    await apiFetchJSON(`${API_BASE}/api/rrhh/empleado/${empId}/historial_cargos`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ cargo_nombre: cargo, fecha_inicio: desde, motivo: motivo || null, registrado_por: state.user?.username || "" }),
    });
    document.getElementById("admin-historial-cargo-input").value = "";
    document.getElementById("admin-historial-desde-input").value = "";
    document.getElementById("admin-historial-motivo-input").value = "";
    showToast("Cargo registrado en el historial.", "success");
    await _adminLoadHistorial();
  } catch (e) { showToast(e.message || "Error al registrar cargo.", "error"); }
}

async function _adminDeleteCargo(empId, histId) {
  const ok = await confirmModal("Eliminar entrada", "¿Eliminar esta entrada del historial de cargos?", "Sí, eliminar", "btn-danger");
  if (!ok) return;
  try {
    // OR-037: sin `requester` el backend registra el borrado a nombre de "sistema" y
    // la auditoría no puede decir quién tocó el historial laboral de la persona.
    await apiFetch(`${API_BASE}/api/rrhh/empleado/${empId}/historial_cargos/${histId}?requester=${encodeURIComponent(state.user?.username || "")}`, { method: "DELETE" });
    showToast("Entrada eliminada.", "success");
    await _adminLoadHistorial();
  } catch (e) { showToast(e.message || "Error al eliminar.", "error"); }
}
