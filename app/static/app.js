// ==========================================================================
// CICLO DE VIDA DE SESIÓN
// ==========================================================================
document.addEventListener("DOMContentLoaded", () => {
  initTheme();
  setupEventListeners();
  checkPersistedSession();
});

async function checkPersistedSession() {
  // admin_system.html gestiona su propia sesión (checkSession) — no interferir
  if (document.body.dataset.page === "admin-sistema") return;
  const raw = localStorage.getItem("archive_session");
  if (!raw) {
    if (document.body.dataset.page) window.location.href = "/";
    return;
  }
  try {
    const saved = JSON.parse(raw);
    const ttlMs = 12 * 60 * 60 * 1000;
    if (saved && saved.username && saved.ts && (Date.now() - saved.ts) < ttlMs) {
      // Restore immediately from cache — no flash, no redirect delay
      loginSuccess(saved);
      // Validate with server in the background; log out only if session is revoked
      fetch(`${API_BASE}/api/auth/restore`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ username: saved.username })
      }).then(res => { if (!res.ok) logout(); }).catch(() => {});
    } else {
      logout();
    }
  } catch (e) {
    logout();
  }
}

function loginSuccess(user) {
  const pageAttr = document.body.dataset.page;
  // La página manda sobre el módulo activo. Las dos búsquedas públicas faltaban,
  // así que en /rrhh la credencial seguía diciendo "Archivo".
  if      (pageAttr === "admin-archivo" || pageAttr === "archivo") user.modulo = "Archivo";
  else if (pageAttr === "admin-rrhh"    || pageAttr === "rrhh")    user.modulo = "RRHH";

  user.modules = user.modules || (user.modulo ? [user.modulo] : []);
  user.roles = user.roles || {};
  if (Object.keys(user.roles).length === 0 && user.modulo) {
    user.roles[user.modulo] = user.rol || "Normal";
  }

  state.user = user;
  localStorage.setItem("archive_session", JSON.stringify({
    username: user.username,
    modules: user.modules,
    roles: user.roles,
    modulo: user.modulo,
    rol: user.rol,
    ts: Date.now()
  }));

  const activeRole = (user.roles && user.roles[user.modulo]) ? user.roles[user.modulo] : user.rol;
  // Módulo y rol van en su propio <span> para poder plegarlos en pantallas
  // angostas: la credencial completa desbordaba la barra por casi 90px.
  document.getElementById("nav_username").innerHTML =
    `ID: ${escHtml(user.username)}` +
    `<span class="ds-nav-user-ctx"> (${escHtml(user.modulo)} - ${escHtml(activeRole)})</span>`;

  loadDynamicChoices();
  configureSidebarVisibilities(user);
  _initNotificationBell(user);

  // Auto-open edit modal when navigating from public search (?docId= / ?empId=)
  const _page = document.body.dataset.page;
  if (_page === "admin-archivo" || _page === "admin-rrhh") {
    const _params = new URLSearchParams(window.location.search);
    const _docId = _params.get("docId");
    const _empId = _params.get("empId");
    if (_docId || _empId) {
      setTimeout(() => {
        if (_docId && typeof openEditDocModal === "function") openEditDocModal(parseInt(_docId));
        else if (_empId && typeof openEditEmpleadoModal === "function") openEditEmpleadoModal(parseInt(_empId));
        // Clean URL without reload
        history.replaceState({}, "", window.location.pathname);
      }, 800);
    }
  }
}

function logout() {
  fetch("/api/auth/logout", { method: "POST" }).catch(() => {});
  state.user = null;
  localStorage.removeItem("archive_session");
  // Antes había una rama para ocultar el portal de la SPA sin navegar; esa
  // página ya no se sirve, así que siempre se vuelve al inicio.
  window.location.href = "/";
}

function configureSidebarVisibilities(user) {
  const linkArchivo     = document.getElementById("menu-btn-archivo");
  const linkRrhh        = document.getElementById("menu-btn-rrhh");
  const linkAdminArchivo = document.getElementById("menu-btn-admin-archivo");
  const linkAdminRrhh   = document.getElementById("menu-btn-admin-rrhh");

  if (linkArchivo)      linkArchivo.style.display = "none";
  if (linkRrhh)         linkRrhh.style.display = "none";
  if (linkAdminArchivo) linkAdminArchivo.style.display = "none";
  if (linkAdminRrhh)    linkAdminRrhh.style.display = "none";

  const modules = user.modules || (user.modulo ? [user.modulo] : []);
  modules.forEach(m => {
    if (m === "Archivo" && linkArchivo)  linkArchivo.style.display = "flex";
    if (m === "RRHH"    && linkRrhh)     linkRrhh.style.display = "flex";
  });

  const rArch = user.roles?.["Archivo"] || null;
  const rRrhh = user.roles?.["RRHH"]    || null;
  if (rArch === "Admin" && linkAdminArchivo) linkAdminArchivo.style.display = "flex";
  if (rRrhh === "Admin" && linkAdminRrhh)   linkAdminRrhh.style.display = "flex";

  // Sistema Global y la consola del asistente son ambas de admin Global: la
  // segunda fija el modelo y los topes de gasto, no es una herramienta de
  // módulo.
  const esGlobal = !!(user.modules &&
    user.modules.includes("Archivo") && user.modules.includes("RRHH"));
  ["menu-btn-admin-sistema", "menu-btn-admin-ia"].forEach(id => {
    const link = document.getElementById(id);
    if (link && esGlobal) link.style.display = "flex";
  });

  // Mostrar el grupo "Administración" si al menos un panel es accesible
  const adminGroup = document.getElementById("sidebar-admin-group");
  if (adminGroup) {
    const hasAdmin = (rArch === "Admin") || (rRrhh === "Admin") ||
      (user.modules?.includes("Archivo") && user.modules?.includes("RRHH"));
    adminGroup.style.display = hasAdmin ? "block" : "none";
  }

  // Acceso en páginas standalone
  const standalonePage = document.body.dataset.page;
  if (standalonePage) {
    let allowed = false;
    if (standalonePage === "archivo")       allowed = modules.includes("Archivo");
    else if (standalonePage === "rrhh")     allowed = modules.includes("RRHH");
    else if (standalonePage === "admin-archivo") allowed = rArch === "Admin";
    else if (standalonePage === "admin-rrhh")    allowed = rRrhh === "Admin";
    // Sistema Global y la consola del asistente son de admin Global. Faltaban:
    // al no tener rama, `allowed` quedaba en false y la comprobación expulsaba
    // de su propio panel a quien sí tenía permiso.
    else if (standalonePage === "admin-sistema" || standalonePage === "admin-ia") {
      allowed = esGlobal;
    }
    else allowed = true;   // páginas sin restricción propia (ayuda, investigación)
    if (!allowed) {
      if (modules.includes("RRHH")) { window.location.href = "/rrhh"; return; }
      window.location.href = "/archivo";
      return;
    }
  }

  switchTab(standalonePage || ((user.modulo === "RRHH") ? "rrhh" : "archivo"));
}

const _BREADCRUMBS = {
  "archivo":       "Archivo / Búsqueda",
  "rrhh":          "RRHH / Búsqueda",
  "admin-archivo": "Archivo / Administración",
  "admin-rrhh":    "RRHH / Administración",
};

function switchTab(tabId) {
  state.activeTab = tabId;
  document.querySelectorAll(".ds-sidebar-link").forEach(l => l.classList.remove("active"));
  document.getElementById(`menu-btn-${tabId}`)?.classList.add("active");
  const bc = document.getElementById("nav-section-breadcrumb");
  if (bc) bc.textContent = _BREADCRUMBS[tabId] || "";
  closeSidebar();
  document.querySelectorAll(".app-tab-section").forEach(s => s.style.display = "none");
  const tabArchivo      = document.getElementById("tab-archivo");
  const tabRrhh         = document.getElementById("tab-rrhh");
  const tabAdminArchivo = document.getElementById("tab-admin-archivo");
  const tabAdminRrhh    = document.getElementById("tab-admin-rrhh");
  if      (tabId === "archivo"       && tabArchivo)      { tabArchivo.style.display = "block"; triggerArchivoSearch(); }
  else if (tabId === "rrhh"          && tabRrhh)         { tabRrhh.style.display = "block"; triggerRrhhSearch(); }
  // Se entra por "stats": es la pestaña que el marcado marca como activa, la
  // primera del grupo de operación, y la única que llena la fila de KPIs de la
  // cabecera. Entrando por "monitor" esos KPIs se quedaban en cero.
  else if (tabId === "admin-archivo" && tabAdminArchivo) { tabAdminArchivo.style.display = "block"; loadAdminTab("stats"); }
  else if (tabId === "admin-rrhh"    && tabAdminRrhh)    { tabAdminRrhh.style.display = "block"; loadAdminTab("stats"); }
}

function openSidebar() {
  document.getElementById("app-sidebar")?.classList.add("open");
  document.getElementById("sidebar-overlay")?.classList.add("open");
}
function closeSidebar() {
  document.getElementById("app-sidebar")?.classList.remove("open");
  document.getElementById("sidebar-overlay")?.classList.remove("open");
}

// ==========================================================================
// EVENTOS
// ==========================================================================
function setupEventListeners() {
  function safeOn(id, ev, fn) { document.getElementById(id)?.addEventListener(ev, fn); }

  // Login
  safeOn("login_btn",         "click",   performLogin);
  safeOn("login_user",        "keydown", e => { if (e.key === "Enter") performLogin(); });
  safeOn("login_pass",        "keydown", e => { if (e.key === "Enter") performLogin(); });
  if (document.body.dataset.page === "login") {
    document.addEventListener("keydown", e => { if (e.key === "Enter") performLogin(); });
  }
  safeOn("toggle_login_pass", "click", () => {
    const input = document.getElementById("login_pass");
    const icon  = document.querySelector("#toggle_login_pass i");
    if (!input || !icon) return;
    if (input.type === "password") { input.type = "text";     icon.className = "fas fa-eye"; }
    else                           { input.type = "password"; icon.className = "fas fa-eye-slash"; }
  });

  // Auth / sidebar
  safeOn("logout_btn",        "click", logout);
  safeOn("sidebar-toggle-btn","click", openSidebar);
  safeOn("sidebar-close-btn", "click", closeSidebar);
  safeOn("sidebar-overlay",   "click", closeSidebar);

  // Tabs (SPA únicamente; en standalone las <a href> navegan normalmente)
  // Los enlaces del menú son <a href> y navegan solos. Aquí vivían handlers de
  // la SPA (index.html) que interceptaban el clic; esa página ya no se sirve.

  // Buscador Archivo — debounce 420ms en input, inmediato en botón/enter
  safeOn("search_archivo",    "input",  e => {
    state.archivo.search = e.target.value; state.archivo.page = 1;
    if (typeof _debouncedArchivoSearch === "function") _debouncedArchivoSearch();
    else triggerArchivoSearch();
  });
  safeOn("search_archivo",    "keydown", e => { if (e.key === "Enter") { state.archivo.search = e.target.value; state.archivo.page = 1; triggerArchivoSearch(); } });
  safeOn("btn_s_archivo",     "click",  () => { state.archivo.search = document.getElementById("search_archivo")?.value || ""; state.archivo.page = 1; triggerArchivoSearch(); });
  safeOn("btn_update_archivo","click",  () => { state.archivo.page = 1; triggerArchivoSearch(); });
  safeOn("btn_clear_archivo", "click",  () => { resetDateFilters("archivo"); const inp = document.getElementById("search_archivo"); if (inp) { inp.value = ""; state.archivo.search = ""; } state.archivo.page = 1; triggerArchivoSearch(); });
  safeOn("download_archivo_xls","click",() => _exportResultsCSV("archivo"));
  safeOn("sort_archivo",      "change", e => { state.archivo.sortMode = e.target.value; state.archivo.page = 1; triggerArchivoSearch(); });
  safeOn("rpp_archivo",       "change", e => { state.archivo.perPage = parseInt(e.target.value); state.archivo.page = 1; triggerArchivoSearch(); });
  safeOn("soporte_archivo",   "change", () => { state.archivo.page = 1; triggerArchivoSearch(); });

  // Buscador RRHH — debounce 420ms en input
  safeOn("search_rrhh",       "input",  e => {
    state.rrhh.search = e.target.value; state.rrhh.page = 1;
    if (typeof _debouncedRrhhSearch === "function") _debouncedRrhhSearch();
    else triggerRrhhSearch();
  });
  safeOn("search_rrhh",       "keydown", e => { if (e.key === "Enter") { state.rrhh.search = e.target.value; state.rrhh.page = 1; triggerRrhhSearch(); } });
  safeOn("btn_s_rrhh",        "click",  () => { state.rrhh.search = document.getElementById("search_rrhh")?.value || ""; state.rrhh.page = 1; triggerRrhhSearch(); });
  safeOn("btn_update_rrhh",   "click",  () => { state.rrhh.page = 1; triggerRrhhSearch(); });
  safeOn("btn_clear_rrhh",    "click",  () => { resetDateFilters("rrhh"); const inp = document.getElementById("search_rrhh"); if (inp) { inp.value = ""; state.rrhh.search = ""; } state.rrhh.page = 1; triggerRrhhSearch(); });
  safeOn("download_rrhh_xls", "click",  () => _exportResultsCSV("rrhh"));
  safeOn("sort_rrhh",         "change", e => { state.rrhh.sortMode = e.target.value; state.rrhh.page = 1; triggerRrhhSearch(); });
  safeOn("rpp_rrhh",          "change", e => { state.rrhh.perPage = parseInt(e.target.value); state.rrhh.page = 1; triggerRrhhSearch(); });

  // Chips de fecha (event delegation por módulo)
  ["archivo", "rrhh"].forEach(mod => {
    document.querySelectorAll(`.ds-date-chip[data-module="${mod}"]`).forEach(btn => {
      btn.addEventListener("click", () => applyDatePreset(mod, btn.dataset.preset));
    });
    safeOn(`year-select-${mod}`, "change", () => handleYearSelect(mod));
  });
  // Botón × del rango vuelve a Todo
  safeOn("fp-archivo-clear", "click", () => applyDatePreset("archivo", "all"));
  safeOn("fp-rrhh-clear",   "click", () => applyDatePreset("rrhh",    "all"));

  // Paginación Archivo
  safeOn("btn-archivo-prev", "click", () => { if (state.archivo.page > 1) { state.archivo.page--; triggerArchivoSearch(); } });
  safeOn("btn-archivo-next", "click", () => { const t = Math.ceil((state.archivo.total || state.archivo.results.length) / state.archivo.perPage); if (state.archivo.page < t) { state.archivo.page++; triggerArchivoSearch(); } });

  // Paginación RRHH
  safeOn("btn-rrhh-prev", "click", () => { if (state.rrhh.page > 1) { state.rrhh.page--; triggerRrhhSearch(); } });
  safeOn("btn-rrhh-next", "click", () => { const t = Math.ceil((state.rrhh.total || state.rrhh.results.length) / state.rrhh.perPage); if (state.rrhh.page < t) { state.rrhh.page++; triggerRrhhSearch(); } });

  // Panel Admin (ambos namespaces)
  ["archivo", "rrhh"].forEach(suf => {
    // Derivado del DOM en vez de una lista fija: la lista se quedaba corta cada
    // vez que se añadía una pestaña (papelera, retención y exportar faltaban).
    document.querySelectorAll(`#admin_workspace_tabs-${suf} .nav-link[id^="tab-admin-${suf}-"]`)
      .forEach(link => {
        const t = link.id.replace(`tab-admin-${suf}-`, "");
        link.addEventListener("click", e => { e.preventDefault(); loadAdminTab(t); });
      });
    document.getElementById(`audit_search-${suf}`)?.addEventListener("input", () => {
      auditState.page = 1;
      loadAuditTab();
    });
    document.getElementById(`btn-apply-stats-${suf}`)?.addEventListener("click", loadDynamicStats);
    document.getElementById(`admin-submit-form-${suf}`)?.addEventListener("submit", handleNewSubmission);
    document.getElementById(`admin_search-${suf}`)?.addEventListener("input",   () => { state.adminTable.page = 1; loadMonitorTable(); });
    document.getElementById(`admin_search-${suf}`)?.addEventListener("keydown", e => { if (e.key === "Enter") { state.adminTable.page = 1; loadMonitorTable(); } });
    document.getElementById(`admin_filter_type-${suf}`)?.addEventListener("change", () => { state.adminTable.page = 1; loadMonitorTable(); });
    document.getElementById(`admin_filter_person-${suf}`)?.addEventListener("change", () => { state.adminTable.page = 1; loadMonitorTable(); });
    document.getElementById(`admin_filter_status-${suf}`)?.addEventListener("change", () => { state.adminTable.page = 1; loadMonitorTable(); });
    document.getElementById(`btn_refresh_table-${suf}`)?.addEventListener("click",  loadMonitorTable);
    document.getElementById(`btn_export_csv-${suf}`)?.addEventListener("click", exportAdminCSV);
    document.getElementById(`admin_prev-${suf}`)?.addEventListener("click", () => { if (state.adminTable.page > 1) { state.adminTable.page--; loadMonitorTable(); } });
    document.getElementById(`admin_next-${suf}`)?.addEventListener("click", () => { const totalPages = Math.ceil((state.adminTable.total || 0) / (state.adminTable.perPage || 25)); if (state.adminTable.page < totalPages) { state.adminTable.page++; loadMonitorTable(); } });
    document.getElementById(`admin_per_page-${suf}`)?.addEventListener("change", e => { state.adminTable.perPage = parseInt(e.target.value) || 25; state.adminTable.page = 1; loadMonitorTable(); });
    document.getElementById(`add_tax_btn-${suf}`)?.addEventListener("click",  handleAddCategory);
    document.getElementById(`btn_add_user-${suf}`)?.addEventListener("click", handleAddUser);
  });
}

// ==========================================================================
// HELPER CENTRAL DE FETCH — maneja 401/403/red uniformemente
// ==========================================================================

/** Escapa caracteres HTML especiales para uso seguro en innerHTML. */
function escHtml(str) {
  return String(str ?? "")
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

/**
 * Fetch con manejo automático de errores de sesión y red.
 * Opts es igual a los init de fetch(); retorna la Response o lanza Error.
 * En 401/403 muestra toast y redirige al login.
 */
async function apiFetch(url, opts = {}) {
  let res;
  try {
    res = await fetch(url, opts);
  } catch {
    throw new Error("Sin conexión con el servidor.");
  }
  if (res.status === 401 || res.status === 403) {
    showToast("Sesión expirada. Redirigiendo al inicio de sesión…", "warning");
    setTimeout(() => { logout(); }, 1800);
    throw new Error("Sesión no autorizada.");
  }
  return res;
}

/**
 * apiFetch + parse JSON. Lanza Error si !res.ok con el detalle del servidor.
 */
async function apiFetchJSON(url, opts = {}) {
  const res = await apiFetch(url, opts);
  if (!res.ok) {
    let detail = `Error ${res.status}`;
    try { const body = await res.json(); detail = body.detail || detail; } catch {}
    throw new Error(detail);
  }
  return res.json();
}

async function performLogin() {
  const username = document.getElementById("login_user")?.value.trim();
  const password = document.getElementById("login_pass")?.value.trim();
  const btn      = document.getElementById("login_btn");
  if (!username || !password) {
    showLoginError("Por favor, ingrese usuario y contraseña.");
    return;
  }
  if (btn) { btn.disabled = true; btn.innerHTML = '<i class="fas fa-spinner fa-spin mr-1"></i>Verificando...'; }
  try {
    const res = await fetch(`${API_BASE}/api/auth/login`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ username, password })
    });
    if (res.status === 403) { showLoginError("Tu cuenta está desactivada. Contacta al administrador."); return; }
    if (!res.ok) throw new Error();
    loginSuccess((await res.json()).user);
  } catch {
    showLoginError("Credenciales incorrectas de acceso institucional.");
  } finally {
    if (btn) { btn.disabled = false; btn.innerHTML = '<i class="fas fa-sign-in-alt mr-1"></i>Ingresar'; }
  }
}

function _exportResultsCSV(modulo) {
  const data = modulo === "archivo" ? state.archivo.results : state.rrhh.results;
  if (!data || data.length === 0) { showToast("No hay resultados para exportar.", "warning"); return; }

  let headers, rows;
  if (modulo === "archivo") {
    headers = ["ID", "Título", "Autor", "Fecha", "Tipología", "Clasificación", "Ubicación", "Resumen", "Palabras Clave", "Archivo Digital"];
    rows = data.map(r => [
      r.id_archivo || r.id, r.titulo, r.autor, r.fecha_documento || r.fecha,
      r.tesauro_primario || r.doc_type, r.tesauro_secundario || r.clasificacion || "",
      r.ubicacion, r.abstract || r.resumen || "", r.palabras_clave || "", r.file_url || ""
    ].map(v => `"${String(v||"").replace(/"/g,'""')}"`).join(","));
  } else {
    headers = ["ID Empleado", "Nombre", "Cédula", "Departamento", "Estado", "Cargo", "Tipos de Documentos"];
    rows = data.map(r => [
      r.empleado_id, r.persona_raw || r.empleado, r.cedula,
      r.departamento, r.estado, r.cargo || "", r.tipos || ""
    ].map(v => `"${String(v||"").replace(/"/g,'""')}"`).join(","));
  }

  const csv = [headers.join(","), ...rows].join("\n");
  const blob = new Blob(["" + csv], { type: "text/csv;charset=utf-8;" });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = `${modulo}_${new Date().toISOString().slice(0,10)}.csv`;
  a.click();
  URL.revokeObjectURL(url);
  showToast(`Exportando ${data.length} registro(s) a CSV.`, "success");
}

function showLoginError(msg) {
  // Use toast if available, otherwise use a visible alert div inside login form
  let errEl = document.getElementById("login-error-msg");
  if (!errEl) {
    errEl = document.createElement("div");
    errEl.id = "login-error-msg";
    errEl.className = "alert alert-danger mt-2 mb-0 py-2";
    errEl.style.fontSize = "0.87rem";
    document.getElementById("login_btn")?.parentElement?.insertAdjacentElement("afterend", errEl);
  }
  errEl.textContent = msg;
  errEl.style.display = "block";
  setTimeout(() => { if (errEl) errEl.style.display = "none"; }, 5000);
}

