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
    if (bc) bc.innerHTML = `<i class="fas fa-shield-alt"></i> Panel de Control / AdministraciÃ³n - ${mod}`;
    const submitBtn = document.getElementById(`btn_submit_workspace-${suf}`);
    if (submitBtn) submitBtn.innerHTML = `<i class="fas fa-cloud-upload-alt"></i> Guardar en ${mod}`;
    const monitorTitle = document.querySelector(`${root} .card-title`);
    if (monitorTitle) monitorTitle.innerHTML = `<i class="fas fa-database"></i> Monitor de ${mod === "RRHH" ? "RRHH" : "Archivos"}`;
  } catch (e) {
    console.error("Error actualizando etiquetas del panel:", e);
  }
}

