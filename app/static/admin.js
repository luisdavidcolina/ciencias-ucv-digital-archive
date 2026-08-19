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

  if      (adminTabId === "stats")      { loadDynamicStats(); _loadAlertasBanner(); }
  else if (adminTabId === "new")        { renderDynamicSubmitFields(); loadRecentSubmissions(); initDropZone(suf); }
  else if (adminTabId === "monitor")    { state.adminTable.page = 1; loadMonitorTable(); }
  else if (adminTabId === "categories") { loadCategoriesTab(); loadRetentionConfig(); }
  else if (adminTabId === "users")      loadUsersTab();
  else if (adminTabId === "audit")      loadAuditTab();
  else if (adminTabId === "retencion")  { loadRetentionConfig(); loadVencimientosTable(); }
  else if (adminTabId === "papelera")   loadPapelera(suf);
  else if (adminTabId === "export") {
    // No hay nada que cargar, pero sí que limpiar: el resultado de una descarga
    // anterior seguiría en pantalla y se leería como el estado actual.
    const status = document.getElementById(`ds-export-status-${suf}`);
    if (status) status.innerHTML = "";
  }

  try {
    const mod = state.user?.modulo || "Archivo";
    const bc = document.querySelector(`${root} .ds-breadcrumb`);
    if (bc) bc.innerHTML = `<i class="fas fa-shield-alt"></i> Panel de Control / Administración - ${escHtml(mod)}`;
    const submitBtn = document.getElementById(`btn_submit_workspace-${suf}`);
    if (submitBtn) submitBtn.innerHTML = `<i class="fas fa-cloud-upload-alt"></i> Guardar en ${escHtml(mod)}`;
    // Acotado al pane del monitor: sin el ancla, esto reescribía el primer
    // .card-title de toda la sección (el de "Filtros Analíticos") en cada cambio.
    const monitorTitle = document.querySelector(`#pane-admin-${suf}-monitor .card-title`);
    if (monitorTitle) monitorTitle.innerHTML = mod === "RRHH"
      ? '<i class="fas fa-id-card"></i> Expedientes de personal'
      : '<i class="fas fa-folder-open"></i> Documentos del archivo';
  } catch (e) {
    console.error("Error actualizando etiquetas del panel:", e);
  }
}

// ==========================================================================
// ALERTAS DE VENCIMIENTO / JUBILACIÓN
// ==========================================================================
async function _loadAlertasBanner() {
  const suf = adminSuffixFromTab();
  if (suf === "archivo") {
    const el = document.getElementById("alertas-vencimiento-banner");
    if (!el) return;
    try {
      const data = await apiFetchJSON(`${API_BASE}/api/admin/retencion/vencimientos?limite=100`);
      const total = data.total || 0;
      if (total === 0) { el.style.display = "none"; return; }
      const muestra = (data.vencimientos || []).slice(0, 3).map(v =>
        `<li class="small"><strong>${escHtml(v.titulo || "(sin título)")}</strong> — ${escHtml(v.tipo_documento || "?")} — venció ${Number(v.dias_vencido)} días</li>`
      ).join("");
      el.innerHTML = `
        <div class="alert alert-warning alert-dismissible fade show mb-0" role="alert">
          <i class="fas fa-exclamation-triangle mr-2"></i>
          <strong>${total} documento${total !== 1 ? "s" : ""} con plazo de retención vencido.</strong>
          <ul class="mb-1 mt-1 pl-3">${muestra}</ul>
          ${total > 3 ? `<small>…y ${total - 3} más. Ver la pestaña <em>Retención</em>.</small>` : ""}
          <button type="button" class="close" data-dismiss="alert" aria-label="Cerrar"><span>&times;</span></button>
        </div>`;
      el.style.display = "";
    } catch {}
  } else {
    const el = document.getElementById("alertas-jubilacion-banner");
    if (!el) return;
    try {
      const data = await apiFetchJSON(`${API_BASE}/api/rrhh/alertas/jubilaciones?horizonte_dias=90`);
      const total = data.total || 0;
      if (total === 0) { el.style.display = "none"; return; }
      const muestra = (data.alertas || []).slice(0, 3).map(a =>
        `<li class="small"><strong>${escHtml(a.nombre_completo)}</strong> — ${escHtml(a.tipo_alerta)} (${Number(a.dias_restantes)} días)</li>`
      ).join("");
      el.innerHTML = `
        <div class="alert alert-warning alert-dismissible fade show mb-0" role="alert">
          <i class="fas fa-user-clock mr-2"></i>
          <strong>${total} empleado${total !== 1 ? "s" : ""} con jubilación/pensión próxima (próximos 90 días).</strong>
          <ul class="mb-1 mt-1 pl-3">${muestra}</ul>
          ${total > 3 ? `<small>…y ${total - 3} más.</small>` : ""}
          <button type="button" class="close" data-dismiss="alert" aria-label="Cerrar"><span>&times;</span></button>
        </div>`;
      el.style.display = "";
    } catch {}
  }
}

async function handleModuleExport(modulo) {
  const tables = modulo === "rrhh"
    ? "empleados,datos_rrhh,rrhh_descriptores,tipo_documento"
    : "datos_archivo,archivo_descriptores,descriptores_libres,tipo_documento";
  const statusEl = document.getElementById(`ds-export-status-${modulo}`);
  if (statusEl) statusEl.innerHTML = '<span class="text-muted"><i class="fas fa-spinner fa-spin mr-1"></i>Generando backup...</span>';
  try {
    const res = await apiFetch(`/api/admin/backup/export?tables=${tables}&requester=${encodeURIComponent(state.user?.usuario || "")}`, {
      headers: { "X-User": state.user?.usuario || "" }
    });
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const blob = await res.blob();
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = `backup_${modulo}_${new Date().toISOString().slice(0,10)}.json`;
    a.click();
    URL.revokeObjectURL(url);
    if (statusEl) statusEl.innerHTML = '<span class="text-success"><i class="fas fa-check mr-1"></i>Descarga iniciada.</span>';
  } catch (e) {
    if (statusEl) { statusEl.innerHTML = `<span class="text-danger"><i class="fas fa-exclamation-circle mr-1"></i></span>`; statusEl.querySelector("span").append(e.message); }
  }
}

// ==========================================================================
// TABLA DE VENCIMIENTOS (Archivo — Auditoría)
// ==========================================================================
async function loadVencimientosTable() {
  const tbody   = document.getElementById("vencimientos-table-body");
  const summary = document.getElementById("vencimientos-summary");
  if (!tbody) return;
  tbody.innerHTML = `<tr><td colspan="8" class="text-center text-muted py-3"><i class="fas fa-spinner fa-spin mr-1"></i>Cargando...</td></tr>`;
  try {
    const data = await apiFetchJSON(`${API_BASE}/api/admin/retencion/vencimientos?limite=100`);
    const rows = data.vencimientos || [];
    if (summary) summary.textContent = `${rows.length} documento${rows.length !== 1 ? "s" : ""} con retención vencida`;
    if (rows.length === 0) {
      tbody.innerHTML = `<tr><td colspan="8" class="text-center text-success py-3"><i class="fas fa-check-circle mr-1"></i>Sin vencimientos pendientes.</td></tr>`;
      return;
    }
    tbody.innerHTML = rows.map((v, i) => {
      const urgency = v.dias_vencido > 365 ? "table-danger" : v.dias_vencido > 90 ? "table-warning" : "";
      return `<tr class="${urgency}">
        <td class="text-muted">${i + 1}</td>
        <td style="max-width:180px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;" title="${escHtml(v.titulo)}">${escHtml(v.titulo || "—")}</td>
        <td><span class="badge badge-secondary">${escHtml(v.tipo_documento || "—")}</span></td>
        <td>${escHtml(v.fecha_documento || "—")}</td>
        <td>${Number(v.plazo_anios)} año${Number(v.plazo_anios) !== 1 ? "s" : ""}</td>
        <td><strong>${Number(v.dias_vencido)}</strong> días</td>
        <td class="text-muted small ds-hide-sm">${escHtml(v.ubicacion || "—")}</td>
        <td class="text-nowrap">
          <button class="btn btn-xs btn-outline-primary" onclick="abrirDisposicion(${v.id_archivo}, ${JSON.stringify(v.titulo || "")})"
                  title="Registrar disposición documental">
            <i class="fas fa-gavel mr-1"></i>Disponer
          </button>
        </td>
      </tr>`;
    }).join("");
  } catch (e) {
    tbody.innerHTML = `<tr><td colspan="8" class="text-danger text-center py-2"></td></tr>`;
    tbody.querySelector("td").textContent = e.message;
  }
}

// ==========================================================================
// EDITOR DE PLAZOS DE RETENCIÓN (Categorías)
// ==========================================================================
async function loadRetentionConfig() {
  const suf    = adminSuffixFromTab();
  const scope  = suf === "archivo" ? "archivo" : "rrhh";
  const tbody  = document.getElementById(`retencion-tipos-body-${suf}`);
  if (!tbody) return;
  tbody.innerHTML = `<tr><td colspan="3" class="text-center text-muted py-3"><i class="fas fa-spinner fa-spin mr-1"></i>Cargando...</td></tr>`;
  try {
    const data = await apiFetchJSON(`${API_BASE}/api/admin/retencion/tipos?scope=${scope}`);
    const tipos = data.tipos || [];
    if (tipos.length === 0) {
      tbody.innerHTML = `<tr><td colspan="3" class="text-center text-muted py-3">Sin tipos configurados.</td></tr>`;
      return;
    }
    tbody.innerHTML = tipos.map(t => `
      <tr>
        <td>${escHtml(t.nombre_corto || t.nombre)}</td>
        <td>
          <div class="input-group input-group-sm">
            <input type="number" class="form-control form-control-sm"
                   id="ret-plazo-${t.id}" value="${Number(t.plazo_retencion_anios)}" min="1" max="100"
                   style="max-width:80px;">
            <div class="input-group-append">
              <span class="input-group-text text-muted">años</span>
            </div>
          </div>
        </td>
        <td>
          <button class="btn btn-sm btn-outline-success" onclick="_saveRetentionPlazo(${t.id})"
                  title="Guardar plazo para ${escHtml(t.nombre_corto || t.nombre)}">
            <i class="fas fa-save"></i>
          </button>
        </td>
      </tr>`).join("");
  } catch (e) {
    tbody.innerHTML = `<tr><td colspan="3" class="text-danger text-center py-2"></td></tr>`;
    tbody.querySelector("td").textContent = e.message;
  }
}

async function _saveRetentionPlazo(tipoId) {
  const inputEl = document.getElementById(`ret-plazo-${tipoId}`);
  const plazo   = parseInt(inputEl?.value);
  if (!plazo || plazo < 1 || plazo > 100) {
    showToast("El plazo debe estar entre 1 y 100 años.", "warning"); return;
  }
  try {
    await apiFetchJSON(`${API_BASE}/api/admin/retencion/tipos/${tipoId}`, {
      method: "PATCH",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ plazo_retencion_anios: plazo, requester: state.user?.username || "" }),
    });
    showToast("Plazo actualizado.", "success");
    if (inputEl) { inputEl.classList.add("is-valid"); setTimeout(() => inputEl.classList.remove("is-valid"), 2000); }
  } catch (e) {
    showToast(`Error: ${e.message}`, "error");
  }
}



// ─── Disposición documental (ISO 15489-1:2016 §8.5) ──────────────────────────
// Disponer no borra: deja constancia de qué se decidió, quién y con qué acta.
// Por eso el acta es obligatoria — una disposición sin respaldo documental no
// sirve para lo único que sirve una disposición: demostrarla después.
async function abrirDisposicion(docId, titulo) {
  const acta = await promptModal(
    "Registrar disposición",
    `Acta o resolución que respalda la decisión sobre "${titulo}"`,
    "", "Ej: Acta 12/2026 del Consejo de Facultad");
  if (acta === null) return;
  if (!String(acta).trim()) {
    showToast("Hace falta el acta que respalda la decisión.", "warning");
    return;
  }

  const decision = await _elegirDisposicion();
  if (!decision) return;

  try {
    await apiFetchJSON(`${API_BASE}/api/admin/retencion/disponer/${docId}`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        disposicion: decision,
        acta: String(acta).trim(),
        requester: state.user?.username || ""
      })
    });
    showToast("Disposición registrada.", "success");
    loadVencimientosTable();
    _loadAlertasBanner();
  } catch (e) {
    showToast(e.message || "No se pudo registrar la disposición.", "error");
  }
}

// Las tres salidas posibles de un documento con el plazo cumplido.
function _elegirDisposicion() {
  return new Promise(resolve => {
    const opciones = [
      ["conservar",   "Conservación permanente", "fa-shield-halved"],
      ["transferido", "Transferir al archivo histórico", "fa-boxes-packing"],
      ["eliminado",   "Eliminar por expurgo", "fa-fire"],
    ];
    const cuerpo = opciones.map(([v, txt, ic]) =>
      `<button class="btn btn-outline-secondary btn-block text-left mb-2 ds-disp-op" data-v="${v}">
         <i class="fas ${ic} mr-2"></i>${txt}
       </button>`).join("");
    const caja = document.createElement("div");
    caja.className = "modal fade show";
    caja.style.cssText = "display:block;background:rgba(0,0,0,.5)";
    caja.innerHTML = `
      <div class="modal-dialog modal-dialog-centered modal-sm">
        <div class="modal-content border-0 shadow-lg" style="border-radius:12px;">
          <div class="modal-header border-0 pb-1"><h6 class="modal-title font-weight-bold">¿Qué se decide?</h6></div>
          <div class="modal-body pt-2">${cuerpo}</div>
          <div class="modal-footer border-0 pt-0">
            <button class="btn btn-sm btn-secondary ds-disp-cancel">Cancelar</button>
          </div>
        </div>
      </div>`;
    document.body.appendChild(caja);
    caja.querySelector(".ds-disp-op")?.focus();
    caja.addEventListener("click", e => {
      const op = e.target.closest(".ds-disp-op");
      if (op) { caja.remove(); resolve(op.dataset.v); return; }
      if (e.target.closest(".ds-disp-cancel") || e.target === caja) { caja.remove(); resolve(null); }
    });
    caja.addEventListener("keydown", e => {
      if (e.key === "Escape") { caja.remove(); resolve(null); }
    });
  });
}
