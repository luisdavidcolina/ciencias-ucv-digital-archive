// ==========================================================================
// CICLO DE VIDA DE SESIÓN
// ==========================================================================
document.addEventListener("DOMContentLoaded", () => {
  initTheme();
  setupEventListeners();
  checkPersistedSession();
  // VI-045: si la pantalla cruza los 1200px con el menú marcado `inert` (se
  // cerró siendo cajón, en móvil) o el contenido marcado `inert` (se quedó
  // abierto como cajón y se ensancha a fijo), un simple resize/rotación sin
  // navegar de por medio dejaría ese estado pegado.
  window.matchMedia("(min-width: 1200px)").addEventListener("change", e => {
    if (e.matches) {
      document.getElementById("app-sidebar")?.removeAttribute("inert");
      document.getElementById("contenido-principal")?.removeAttribute("inert");
    } else if (!document.getElementById("app-sidebar")?.classList.contains("open")) {
      document.getElementById("app-sidebar")?.setAttribute("inert", "");
    }
  });
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

// BA-146: un usuario sólo-RRHH que entraba a /archivo veía la credencial
// pintada como "Archivo" y `loadDynamicChoices()` ya disparada antes de que
// `configureSidebarVisibilities()` (más abajo en la misma función) notara
// que no tenía permiso y lo expulsara — parpadeo y una petición de sobra.
// Se comprueba el acceso a la página standalone ANTES de pintar nada.
function _accesoPaginaPermitido(user) {
  const standalonePage = document.body.dataset.page;
  if (!standalonePage) return true;
  const modules = user.modules || (user.modulo ? [user.modulo] : []);
  const rArch = user.roles?.["Archivo"] || null;
  const rRrhh = user.roles?.["RRHH"]    || null;
  const esGlobal = modules.includes("Archivo") && modules.includes("RRHH");
  if (standalonePage === "archivo")            return modules.includes("Archivo");
  if (standalonePage === "rrhh")               return modules.includes("RRHH");
  if (standalonePage === "admin-archivo")      return rArch === "Admin";
  if (standalonePage === "admin-rrhh")         return rRrhh === "Admin";
  if (standalonePage === "admin-sistema" || standalonePage === "admin-ia") return esGlobal;
  return true;   // páginas sin restricción propia (ayuda, investigación)
}

function loginSuccess(user) {
  const pageAttr = document.body.dataset.page;
  user.modules = user.modules || (user.modulo ? [user.modulo] : []);
  if (!_accesoPaginaPermitido(user)) {
    const modules = user.modules || (user.modulo ? [user.modulo] : []);
    window.location.href = modules.includes("RRHH") ? "/rrhh" : "/archivo";
    return;
  }
  // La página sólo decide el módulo activo si el usuario de verdad lo tiene.
  // Antes se reescribía user.modulo con el de la página aunque no estuviera
  // en sus módulos reales, y esa mentira quedaba persistida en localStorage:
  // quien sólo tenía RRHH y visitaba /archivo (antes del rebote) se quedaba
  // con la credencial diciendo "Archivo" hasta el siguiente login (SI-046).
  if      ((pageAttr === "admin-archivo" || pageAttr === "archivo") && user.modules.includes("Archivo")) user.modulo = "Archivo";
  else if ((pageAttr === "admin-rrhh"    || pageAttr === "rrhh")    && user.modules.includes("RRHH"))    user.modulo = "RRHH";

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
  _startSessionExpiryWatch();

  // Auto-open edit modal when navigating from public search (?docId= / ?empId=)
  const _page = document.body.dataset.page;
  if (_page === "admin-archivo" || _page === "admin-rrhh") {
    const _params = new URLSearchParams(window.location.search);
    const _docId = _params.get("docId");
    const _empId = _params.get("empId");
    if (_docId || _empId) {
      setTimeout(() => {
        if (_docId && typeof openEditDocModal === "function") openEditDocModal(parseInt(_docId, 10));
        else if (_empId && typeof openEditEmpleadoModal === "function") openEditEmpleadoModal(parseInt(_empId, 10));
        // Clean URL without reload
        history.replaceState({}, "", window.location.pathname);
      }, 800);
    }
  }
}

// SI-043: nada avisaba antes de que la sesión caducara — quien llevaba rato
// redactando una ficha recibía un 401 sin aviso y perdía el trabajo al
// guardar. Avisa 5 minutos antes; el guardado del borrador en sí depende de
// cada formulario (admin-submit.js/admin-edit.js, fuera de este archivo).
let _sessionExpiryTimer = null;
let _sessionWarnShown = false;
function _startSessionExpiryWatch() {
  if (_sessionExpiryTimer) clearInterval(_sessionExpiryTimer);
  _sessionWarnShown = false;
  _sessionExpiryTimer = setInterval(() => {
    const raw = localStorage.getItem("archive_session");
    if (!raw) { clearInterval(_sessionExpiryTimer); return; }
    let saved;
    try { saved = JSON.parse(raw); } catch { return; }
    if (!saved || !saved.ts) return;
    const ttlMs = 12 * 60 * 60 * 1000;
    const remaining = ttlMs - (Date.now() - saved.ts);
    if (remaining <= 0) { clearInterval(_sessionExpiryTimer); return; }
    if (remaining <= 5 * 60 * 1000 && !_sessionWarnShown) {
      _sessionWarnShown = true;
      showToast("Tu sesión expira en unos minutos. Guarda lo que estés editando.", "warning");
    }
  }, 30000);
}

async function logout() {
  // Antes se lanzaba sin esperar y se navegaba enseguida: la navegación
  // podía cancelar la petición, la cookie sobrevivía, y quien abriera el
  // navegador después entraba directamente (SI-044).
  try { await fetch("/api/auth/logout", { method: "POST" }); } catch {}
  if (_sessionExpiryTimer) clearInterval(_sessionExpiryTimer);
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
  // admin_system.html es una página standalone de una sola sección, sin los
  // cuatro ids de arriba: su única ".app-tab-section" no tenía rama que la
  // volviera a mostrar tras el ocultamiento general tres líneas más arriba, así
  // que /admin/sistema se quedaba en blanco (barra superior y nada más). No
  // tiene id propio en el HTML — es la única sección de esa página, así que se
  // localiza por clase.
  else if (tabId === "admin-sistema") {
    const tabAdminSistema = document.querySelector(".app-tab-section");
    if (tabAdminSistema) tabAdminSistema.style.display = "block";
  }
}

// SI-185: el botón que abre/cierra el menú avisa de su estado
// (aria-expanded) y el foco entra al menú al abrirlo y vuelve al botón al
// cerrarlo — sin esto, quien navega por teclado o lector de pantalla no
// sabe si el menú está abierto ni dónde quedó el foco tras cerrarlo.
//
// VI-045/SD-203: el menú cerrado sólo se desplazaba fuera de pantalla con
// CSS — seguía en el orden de tabulación, así que Tab recorría diez enlaces
// invisibles antes de llegar al primer filtro. Y con el menú abierto, el
// contenido de detrás (tapado por el velo) seguía siendo tabulable. `inert`
// saca del foco y de los lectores de pantalla lo que no se ve en cada
// estado — pero sólo por debajo de 1200px: desde ahí (SD-134) el menú es
// fijo y siempre visible, sin velo (`.ds-sidebar-overlay{display:none}`), así
// que aplicar `inert` ahí dejaría el menú entero inutilizable en escritorio.
function _sidebarEsCajon() {
  return !window.matchMedia("(min-width: 1200px)").matches;
}
function openSidebar() {
  document.getElementById("app-sidebar")?.classList.add("open");
  document.getElementById("app-sidebar")?.removeAttribute("inert");
  document.getElementById("sidebar-overlay")?.classList.add("open");
  if (_sidebarEsCajon()) document.getElementById("contenido-principal")?.setAttribute("inert", "");
  const toggleBtn = document.getElementById("sidebar-toggle-btn");
  toggleBtn?.setAttribute("aria-expanded", "true");
  toggleBtn?.setAttribute("aria-label", "Cerrar menú");
  document.getElementById("app-sidebar")?.querySelector("a, button")?.focus();
}
function closeSidebar() {
  document.getElementById("app-sidebar")?.classList.remove("open");
  if (_sidebarEsCajon()) document.getElementById("app-sidebar")?.setAttribute("inert", "");
  else document.getElementById("app-sidebar")?.removeAttribute("inert");
  document.getElementById("sidebar-overlay")?.classList.remove("open");
  document.getElementById("contenido-principal")?.removeAttribute("inert");
  const toggleBtn = document.getElementById("sidebar-toggle-btn");
  toggleBtn?.setAttribute("aria-expanded", "false");
  toggleBtn?.setAttribute("aria-label", "Abrir menú");
  if (toggleBtn && document.activeElement && document.getElementById("app-sidebar")?.contains(document.activeElement)) {
    toggleBtn.focus();
  }
}

// VI-045: el menú lateral abierto es un cajón modal sobre el contenido (el
// resto de la página queda tapado por "sidebar-overlay"), así que el foco
// tiene que quedarse atrapado dentro mientras esté abierto — si no, Tab lo
// saca al contenido que sigue debajo, invisible bajo el overlay, y Shift+Tab
// desde el primer enlace se lo lleva a la barra superior. Escape lo cierra,
// como cualquier cajón/diálogo.
function sidebarKeydownTrap(e) {
  const barra = document.getElementById("app-sidebar");
  if (!barra || !barra.classList.contains("open")) return;
  if (e.key === "Escape") {
    e.preventDefault();
    closeSidebar();
    return;
  }
  if (e.key !== "Tab") return;
  const focusables = Array.from(
    barra.querySelectorAll('a[href], button:not([disabled]), input:not([disabled]), [tabindex]:not([tabindex="-1"])')
  ).filter(el => el.offsetParent !== null);
  if (focusables.length === 0) return;
  const primero = focusables[0];
  const ultimo = focusables[focusables.length - 1];
  if (e.shiftKey && document.activeElement === primero) {
    e.preventDefault();
    ultimo.focus();
  } else if (!e.shiftKey && document.activeElement === ultimo) {
    e.preventDefault();
    primero.focus();
  }
}

// ==========================================================================
// EVENTOS
// ==========================================================================
function setupEventListeners() {
  function safeOn(id, ev, fn) { document.getElementById(id)?.addEventListener(ev, fn); }

  // SI-034: `login.html` no carga este archivo (sólo `login.js`), así que un
  // segundo `performLogin()`/`showLoginError()` con sus listeners de
  // `login_btn`/`login_user`/`login_pass`/`toggle_login_pass` no se ejecutaba
  // nunca aquí — código muerto que invitaba a arreglar el login en el sitio
  // equivocado. La única implementación real es `login.js`.

  // Auth / sidebar
  safeOn("logout_btn",        "click", logout);
  safeOn("sidebar-toggle-btn","click", openSidebar);
  safeOn("sidebar-close-btn", "click", closeSidebar);
  safeOn("sidebar-overlay",   "click", closeSidebar);
  document.addEventListener("keydown", sidebarKeydownTrap);

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
  safeOn("rpp_archivo",       "change", e => { state.archivo.perPage = parseInt(e.target.value, 10); state.archivo.page = 1; triggerArchivoSearch(); });
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
  safeOn("rpp_rrhh",          "change", e => { state.rrhh.perPage = parseInt(e.target.value, 10); state.rrhh.page = 1; triggerRrhhSearch(); });

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
    // OA-032: igual que OA-031 pero sobre `audit_log`, que puede tener cientos
    // de miles de filas — cada tecla lanzaba `loadAuditTab()` sin esperar.
    const _debouncedAuditSearch = typeof debounce === "function"
      ? debounce(() => loadAuditTab(), 350)
      : () => loadAuditTab();
    document.getElementById(`audit_search-${suf}`)?.addEventListener("input", () => {
      auditState.page = 1;
      _debouncedAuditSearch();
    });
    document.getElementById(`btn-apply-stats-${suf}`)?.addEventListener("click", loadDynamicStats);
    document.getElementById(`admin-submit-form-${suf}`)?.addEventListener("submit", handleNewSubmission);
    // OR-127: un tecleo, una petición de sobra — `debounce()` (admin-ui.js) evita
    // disparar `loadMonitorTable()` (con su `to_tsvector`/`unaccent`/`COUNT(DISTINCT)`
    // contra Neon) en cada tecla; Enter sigue siendo inmediato.
    const _debouncedMonitorSearch = typeof debounce === "function"
      ? debounce(() => loadMonitorTable(), 300)
      : () => loadMonitorTable();
    document.getElementById(`admin_search-${suf}`)?.addEventListener("input",   () => { state.adminTable.page = 1; _debouncedMonitorSearch(); });
    document.getElementById(`admin_search-${suf}`)?.addEventListener("keydown", e => { if (e.key === "Enter") { state.adminTable.page = 1; loadMonitorTable(); } });
    document.getElementById(`admin_filter_type-${suf}`)?.addEventListener("change", () => { state.adminTable.page = 1; loadMonitorTable(); });
    document.getElementById(`admin_filter_person-${suf}`)?.addEventListener("change", () => { state.adminTable.page = 1; loadMonitorTable(); });
    document.getElementById(`admin_filter_department-${suf}`)?.addEventListener("change", () => { state.adminTable.page = 1; loadMonitorTable(); });
    document.getElementById(`admin_filter_status-${suf}`)?.addEventListener("change", () => { state.adminTable.page = 1; loadMonitorTable(); });
    document.getElementById(`btn_refresh_table-${suf}`)?.addEventListener("click",  loadMonitorTable);
    document.getElementById(`btn_export_csv-${suf}`)?.addEventListener("click", exportAdminCSV);
    document.getElementById(`admin_prev-${suf}`)?.addEventListener("click", () => { if (state.adminTable.page > 1) { state.adminTable.page--; loadMonitorTable(); } });
    document.getElementById(`admin_next-${suf}`)?.addEventListener("click", () => { const totalPages = Math.ceil((state.adminTable.total || 0) / (state.adminTable.perPage || 25)); if (state.adminTable.page < totalPages) { state.adminTable.page++; loadMonitorTable(); } });
    document.getElementById(`admin_per_page-${suf}`)?.addEventListener("change", e => { state.adminTable.perPage = parseInt(e.target.value, 10) || 25; state.adminTable.page = 1; loadMonitorTable(); });
    document.getElementById(`add_tax_btn-${suf}`)?.addEventListener("click",  handleAddCategory);
    document.getElementById(`btn_add_user-${suf}`)?.addEventListener("click", handleAddUser);
  });
}

// ==========================================================================
// HELPER CENTRAL DE FETCH — maneja 401 (logout), 403 (aviso) y red
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
 * En 401 (sesión caducada) muestra toast y redirige al login.
 * En 403 (acción prohibida para el rol, con sesión válida) muestra un aviso
 * y deja la sesión intacta; el llamador recibe el error para manejarlo.
 */
async function apiFetch(url, opts = {}) {
  let res;
  try {
    res = await fetch(url, opts);
  } catch {
    throw new Error("Sin conexión con el servidor.");
  }
  if (res.status === 401) {
    showToast("Sesión expirada. Redirigiendo al inicio de sesión…", "warning");
    setTimeout(() => { logout(); }, 1800);
    throw new Error("Sesión no autorizada.");
  }
  if (res.status === 403) {
    showToast("No tienes permiso para esta acción.", "warning");
    throw new Error("No tienes permiso para esta acción.");
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

