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

