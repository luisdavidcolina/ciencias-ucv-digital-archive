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
  else if (adminTabId === "audit")      { loadAuditTab(); if (suf === "archivo") loadVencimientosTable(); }

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

// ==========================================================================
// ALERTAS DE VENCIMIENTO / JUBILACIÓN
// ==========================================================================
async function _loadAlertasBanner() {
  const suf = adminSuffixFromTab();
  if (suf === "archivo") {
    const el = document.getElementById("alertas-vencimiento-banner");
    if (!el) return;
    try {
      const res = await fetch(`${API_BASE}/api/admin/retencion/vencimientos?limite=100`);
      if (!res.ok) return;
      const data = await res.json();
      const total = data.total || 0;
      if (total === 0) { el.style.display = "none"; return; }
      const muestra = (data.vencimientos || []).slice(0, 3).map(v =>
        `<li class="small"><strong>${v.titulo || "(sin título)"}</strong> — ${v.tipo_documento || "?"} — venció ${v.dias_vencido} días</li>`
      ).join("");
      el.innerHTML = `
        <div class="alert alert-warning alert-dismissible fade show mb-0" role="alert">
          <i class="fas fa-exclamation-triangle mr-2"></i>
          <strong>${total} documento${total !== 1 ? "s" : ""} con plazo de retención vencido.</strong>
          <ul class="mb-1 mt-1 pl-3">${muestra}</ul>
          ${total > 3 ? `<small>…y ${total - 3} más. Ver <em>Auditoría → Vencimientos</em>.</small>` : ""}
          <button type="button" class="close" data-dismiss="alert" aria-label="Cerrar"><span>&times;</span></button>
        </div>`;
      el.style.display = "";
    } catch {}
  } else {
    const el = document.getElementById("alertas-jubilacion-banner");
    if (!el) return;
    try {
      const res = await fetch(`${API_BASE}/api/rrhh/alertas/jubilaciones?horizonte_dias=90`);
      if (!res.ok) return;
      const data = await res.json();
      const total = data.total || 0;
      if (total === 0) { el.style.display = "none"; return; }
      const muestra = (data.alertas || []).slice(0, 3).map(a =>
        `<li class="small"><strong>${a.nombre_completo}</strong> — ${a.tipo_alerta} (${a.dias_restantes} días)</li>`
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
    const res = await fetch(`/api/admin/backup/export?tables=${tables}&requester=${encodeURIComponent(state.user?.usuario || "")}`, {
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
    if (statusEl) statusEl.innerHTML = `<span class="text-danger"><i class="fas fa-exclamation-circle mr-1"></i>${e.message}</span>`;
  }
}

// ==========================================================================
// TABLA DE VENCIMIENTOS (Archivo — Auditoría)
// ==========================================================================
async function loadVencimientosTable() {
  const tbody   = document.getElementById("vencimientos-table-body");
  const summary = document.getElementById("vencimientos-summary");
  if (!tbody) return;
  tbody.innerHTML = `<tr><td colspan="7" class="text-center text-muted py-3"><i class="fas fa-spinner fa-spin mr-1"></i>Cargando...</td></tr>`;
  try {
    const res  = await fetch(`${API_BASE}/api/admin/retencion/vencimientos?limite=100`);
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const data = await res.json();
    const rows = data.vencimientos || [];
    if (summary) summary.textContent = `${rows.length} documento${rows.length !== 1 ? "s" : ""} con retención vencida`;
    if (rows.length === 0) {
      tbody.innerHTML = `<tr><td colspan="7" class="text-center text-success py-3"><i class="fas fa-check-circle mr-1"></i>Sin vencimientos pendientes.</td></tr>`;
      return;
    }
    tbody.innerHTML = rows.map((v, i) => {
      const urgency = v.dias_vencido > 365 ? "table-danger" : v.dias_vencido > 90 ? "table-warning" : "";
      return `<tr class="${urgency}">
        <td class="text-muted">${i + 1}</td>
        <td style="max-width:180px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;" title="${v.titulo}">${v.titulo || "—"}</td>
        <td><span class="badge badge-secondary">${v.tipo_documento || "—"}</span></td>
        <td>${v.fecha_documento || "—"}</td>
        <td>${v.plazo_anios} año${v.plazo_anios !== 1 ? "s" : ""}</td>
        <td><strong>${v.dias_vencido}</strong> días</td>
        <td class="text-muted small">${v.ubicacion || "—"}</td>
      </tr>`;
    }).join("");
  } catch (e) {
    tbody.innerHTML = `<tr><td colspan="7" class="text-danger text-center py-2">${e.message}</td></tr>`;
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
    const res  = await fetch(`${API_BASE}/api/admin/retencion/tipos?scope=${scope}`);
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const data = await res.json();
    const tipos = data.tipos || [];
    if (tipos.length === 0) {
      tbody.innerHTML = `<tr><td colspan="3" class="text-center text-muted py-3">Sin tipos configurados.</td></tr>`;
      return;
    }
    tbody.innerHTML = tipos.map(t => `
      <tr>
        <td>${t.nombre_corto || t.nombre}</td>
        <td>
          <div class="input-group input-group-sm">
            <input type="number" class="form-control form-control-sm"
                   id="ret-plazo-${t.id}" value="${t.plazo_retencion_anios}" min="1" max="100"
                   style="max-width:80px;">
            <div class="input-group-append">
              <span class="input-group-text text-muted">años</span>
            </div>
          </div>
        </td>
        <td>
          <button class="btn btn-sm btn-outline-success" onclick="_saveRetentionPlazo(${t.id})"
                  title="Guardar plazo para ${t.nombre_corto || t.nombre}">
            <i class="fas fa-save"></i>
          </button>
        </td>
      </tr>`).join("");
  } catch (e) {
    tbody.innerHTML = `<tr><td colspan="3" class="text-danger text-center py-2">${e.message}</td></tr>`;
  }
}

async function _saveRetentionPlazo(tipoId) {
  const inputEl = document.getElementById(`ret-plazo-${tipoId}`);
  const plazo   = parseInt(inputEl?.value);
  if (!plazo || plazo < 1 || plazo > 100) {
    showToast("El plazo debe estar entre 1 y 100 años.", "warning"); return;
  }
  try {
    const res = await fetch(`${API_BASE}/api/admin/retencion/tipos/${tipoId}`, {
      method: "PATCH",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ plazo_retencion_anios: plazo, requester: state.user?.username || "" }),
    });
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    showToast("Plazo actualizado.", "success");
    if (inputEl) { inputEl.classList.add("is-valid"); setTimeout(() => inputEl.classList.remove("is-valid"), 2000); }
  } catch (e) {
    showToast(`Error: ${e.message}`, "error");
  }
}

