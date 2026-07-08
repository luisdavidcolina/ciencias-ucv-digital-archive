// ==========================================================================
// ESTADO GLOBAL
// ==========================================================================
const state = {
  user: null,
  activeTab: "archivo",
  choices: null,
  activeAdminTab: "stats",
  archivo: {
    results: [],
    total: 0,
    search: "",
    selectedTypes: [],
    selectedTesauro: [],
    dateStart: "",
    dateEnd: "",
    sortMode: "Alfabético (A-Z)",
    page: 1,
    perPage: 5
  },
  rrhh: {
    results: [],
    total: 0,
    search: "",
    selectedTypes: [],
    selectedEstados: [],
    selectedPeople: [],
    dateStart: "",
    dateEnd: "",
    sortMode: "Alfabético (A-Z)",
    page: 1,
    perPage: 5
  },
  adminTable: {
    results: [],
    total: 0,
    search: "",
    typeFilter: "",
    page: 1,
    perPage: 25
  },
  activePersonProfile: null,
  innerDossierSearch: "",
  innerDossierClass: "",
  innerDossierSort: "Alfabético (A-Z)"
};

const API_BASE = window.location.origin;
const tsInstances = {};
const fpInstances = {};

// ==========================================================================
// HELPERS COMPARTIDOS
// ==========================================================================

/**
 * Devuelve una URL de archivo con el parámetro de autenticación `?u=username`.
 * Aplica solo a URLs internas del tipo /api/files/...
 */
function _secureFileUrl(url) {
  if (!url || !url.startsWith("/api/files/")) return url;
  const username = state.user?.username || "";
  if (!username) return url;
  return `${url}${url.includes("?") ? "&" : "?"}u=${encodeURIComponent(username)}`;
}

/** Resalta términos de búsqueda en un texto HTML-safe. */
function highlightTerms(text, terms) {
  if (!text || !terms || !terms.length) return text || "";
  const escaped = terms
    .map(t => t.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"))
    .filter(Boolean)
    .join("|");
  if (!escaped) return text;
  try {
    return String(text).replace(
      new RegExp(`(${escaped})`, "gi"),
      '<mark style="background:#fff176;border-radius:2px;padding:0 1px;">$1</mark>'
    );
  } catch { return text; }
}

function formatISOToSpanish(iso) {
  if (!iso) return "";
  const datePart = String(iso).split("T")[0].split(" ")[0];
  const parts = datePart.split("-");
  if (parts.length !== 3) return iso;
  return `${parts[2]}/${parts[1]}/${parts[0]}`;
}

function formatRelativeTime(iso) {
  if (!iso) return "";
  const d = new Date(iso);
  if (isNaN(d)) return iso;
  const diff = (Date.now() - d.getTime()) / 1000;
  if (diff < 60)     return "hace un momento";
  if (diff < 3600)   return `hace ${Math.floor(diff / 60)} min`;
  if (diff < 86400)  return `hace ${Math.floor(diff / 3600)} h`;
  if (diff < 604800) return `hace ${Math.floor(diff / 86400)} días`;
  return formatISOToSpanish(iso);
}

function getPersonInitials(name) {
  if (!name) return "?";
  const normalized = String(name).trim();
  if (!normalized) return "?";
  const commaParts = normalized.split(",").map(p => p.trim()).filter(Boolean);
  if (commaParts.length >= 2) {
    const s = (commaParts[0].split(/\s+/).filter(Boolean)[0] || "").charAt(0);
    const g = (commaParts[1].split(/\s+/).filter(Boolean)[0] || "").charAt(0);
    if (s || g) return `${s}${g}`.toUpperCase();
  }
  const parts = normalized.split(/\s+/).filter(Boolean);
  if (parts.length === 1) return parts[0].substring(0, 2).toUpperCase();
  if (parts.length >= 4) return (parts[0][0] + parts[2][0]).toUpperCase();
  return (parts[0][0] + parts[parts.length - 1][0]).toUpperCase();
}

function getStatusColor(status) {
  switch (status) {
    case "Activo":    return "#28a745";
    case "Retirado":  return "#dc3545";
    case "Jubilado":  return "#6f42c1";
    case "Pensionado":return "#0056b3";
    default:          return "#6c757d";
  }
}

// ==========================================================================
// HELPERS ADMIN
// ==========================================================================
function adminSuffixFromTab(tab) {
  const t = tab || state.activeTab;
  return (t === "admin-rrhh") ? "rrhh" : "archivo";
}
function adminId(base) { return `${base}-${adminSuffixFromTab()}`; }
function getAdminEl(base) { return document.getElementById(adminId(base)); }
function isArchivoModule() { return state.user && state.user.modulo === "Archivo"; }

// ==========================================================================
// CICLO DE VIDA DE SESIÓN
// ==========================================================================
document.addEventListener("DOMContentLoaded", () => {
  initTheme();
  setupEventListeners();
  checkPersistedSession();
});

async function checkPersistedSession() {
  // admin_sistema.html gestiona su propia sesión (checkSession) — no interferir
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
  if (pageAttr === "admin-archivo") user.modulo = "Archivo";
  else if (pageAttr === "admin-rrhh") user.modulo = "RRHH";

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

  const loginScr = document.getElementById("login-screen");
  const appPortal = document.getElementById("app-portal");
  if (loginScr) loginScr.style.setProperty("display", "none", "important");
  if (appPortal) appPortal.style.display = "block";

  const activeRole = (user.roles && user.roles[user.modulo]) ? user.roles[user.modulo] : user.rol;
  document.getElementById("nav_username").innerText = `ID: ${user.username} (${user.modulo} - ${activeRole})`;

  loadDynamicChoices();
  configureSidebarVisibilities(user);
  _initNotificationBell(user);

  const switchBtn = document.getElementById("module_switch_btn");
  if (switchBtn) {
    if (user.modules && user.modules.length > 1) {
      switchBtn.style.display = "inline-block";
      switchBtn.innerText = `Switch (${user.modules.join(" / ")})`;
    } else {
      switchBtn.style.display = "none";
    }
  }

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
  state.user = null;
  localStorage.removeItem("archive_session");
  const appPortal = document.getElementById("app-portal");
  const loginScr = document.getElementById("login-screen");
  if (appPortal) appPortal.style.display = "none";
  if (loginScr) {
    loginScr.style.setProperty("display", "flex", "important");
    const lp = document.getElementById("login_pass");
    if (lp) lp.value = "";
  } else {
    window.location.href = "/";
  }
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

  const linkSistema = document.getElementById("menu-btn-admin-sistema");
  // Mostrar si el usuario tiene AMBOS módulos (Global admin)
  if (linkSistema && user.modules && user.modules.includes("Archivo") && user.modules.includes("RRHH")) {
    linkSistema.style.display = "flex";
  }

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
    if (!allowed) {
      if (modules.includes("RRHH")) { window.location.href = "/rrhh"; return; }
      window.location.href = "/archivo";
      return;
    }
  }

  switchTab(standalonePage || ((user.modulo === "RRHH") ? "rrhh" : "archivo"));
}

function handleModuleSwitch() {
  if (!state.user?.modules || state.user.modules.length < 2) return;
  const idx  = state.user.modules.indexOf(state.user.modulo);
  state.user.modulo = state.user.modules[(idx + 1) % state.user.modules.length];
  const role = state.user.roles?.[state.user.modulo] || "Normal";
  document.getElementById("nav_username").innerText = `ID: ${state.user.username} (${state.user.modulo} - ${role})`;
  configureSidebarVisibilities(state.user);
  switchTab(state.user.modulo === "RRHH" ? "rrhh" : "archivo");
}

function switchTab(tabId) {
  state.activeTab = tabId;
  document.querySelectorAll(".ds-sidebar-link").forEach(l => l.classList.remove("active"));
  document.getElementById(`menu-btn-${tabId}`)?.classList.add("active");
  closeSidebar();
  document.querySelectorAll(".app-tab-section").forEach(s => s.style.display = "none");
  const tabArchivo      = document.getElementById("tab-archivo");
  const tabRrhh         = document.getElementById("tab-rrhh");
  const tabAdminArchivo = document.getElementById("tab-admin-archivo");
  const tabAdminRrhh    = document.getElementById("tab-admin-rrhh");
  if      (tabId === "archivo"       && tabArchivo)      { tabArchivo.style.display = "block"; triggerArchivoSearch(); }
  else if (tabId === "rrhh"          && tabRrhh)         { tabRrhh.style.display = "block"; triggerRrhhSearch(); }
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
// CHOICES, TOM SELECT, FECHAS
// ==========================================================================
async function loadDynamicChoices() {
  try {
    const res = await fetch(`${API_BASE}/api/choices`);
    if (!res.ok) throw new Error();
    const data = await res.json();
    state.choices = data;
    state.archivo.dateStart = data.archivo.min_date;
    state.archivo.dateEnd   = data.archivo.max_date;
    state.rrhh.dateStart    = data.rrhh.min_date;
    state.rrhh.dateEnd      = data.rrhh.max_date;
    initTomSelects();
    initDateControls("archivo", data.archivo);
    initDateControls("rrhh",    data.rrhh);
    _populateDataLists(data.rrhh);
  } catch (e) {
    console.error("Error al cargar choices dinámicos:", e);
  }
}

function _populateDataLists(rrhh) {
  if (!rrhh) return;
  const fill = (id, items) => {
    const dl = document.getElementById(id);
    if (!dl || !Array.isArray(items)) return;
    dl.innerHTML = items.map(v => `<option value="${String(v).replace(/"/g,'&quot;')}">`).join("");
  };
  fill("dl-cargos",       rrhh.cargos);
  fill("dl-departamentos", rrhh.departamentos);
}

function initTomSelects() {
  if (!state.choices || typeof TomSelect === "undefined") return;

  function makeSel(id, items, onChange) {
    const el = document.getElementById(id);
    if (!el) return;
    if (tsInstances[id]) { tsInstances[id].destroy(); delete tsInstances[id]; }
    tsInstances[id] = new TomSelect(el, {
      plugins: ["remove_button"],
      create: false,
      maxOptions: null,
      options: items.map(v => ({ value: v, text: v })),
      items: [],
      placeholder: el.getAttribute("placeholder") || "Seleccionar...",
      onChange
    });
  }

  makeSel("choice-archivo-doc-type", state.choices.archivo.doc_types, val => {
    state.archivo.selectedTypes  = Array.isArray(val) ? val : (val ? [val] : []);
    state.archivo.page = 1; triggerArchivoSearch();
  });
  makeSel("choice-archivo-tesauro", [], val => {
    state.archivo.selectedTesauro = Array.isArray(val) ? val : (val ? [val] : []);
    state.archivo.page = 1; triggerArchivoSearch();
  });
  // Configurar carga remota para el Tom Select de palabras clave
  if (tsInstances["choice-archivo-tesauro"]) {
    tsInstances["choice-archivo-tesauro"].settings.load = (query, callback) => {
      if (!query.trim()) { callback([]); return; }
      fetch(`${API_BASE}/api/archivo/documentos/buscar?q=${encodeURIComponent(query)}`)
        .then(r => r.json())
        .then(data => callback(data.map(d => ({ value: d.nombre_corto, text: d.nombre_corto }))))
        .catch(() => callback([]));
    };
  }
  makeSel("choice-rrhh-doc-type", state.choices.rrhh.doc_types, val => {
    state.rrhh.selectedTypes  = Array.isArray(val) ? val : (val ? [val] : []);
    state.rrhh.page = 1; triggerRrhhSearch();
  });
  makeSel("choice-rrhh-estado", state.choices.rrhh.estados, val => {
    state.rrhh.selectedEstados = Array.isArray(val) ? val : (val ? [val] : []);
    state.rrhh.page = 1; triggerRrhhSearch();
  });
  makeSel("choice-rrhh-people", state.choices.rrhh.people, val => {
    state.rrhh.selectedPeople = Array.isArray(val) ? val : (val ? [val] : []);
    state.rrhh.page = 1; triggerRrhhSearch();
  });
}

function initDateControls(module, data) {
  const input = document.getElementById(`fp-${module}-range`);
  if (input) {
    if (fpInstances[module]) fpInstances[module].destroy();
    fpInstances[module] = flatpickr(input, {
      mode: "range",
      dateFormat: "Y-m-d",
      minDate: data.min_date,
      maxDate: data.max_date,
      locale: {
        rangeSeparator: " → ",
        firstDayOfWeek: 1,
        weekdays: {
          shorthand: ["Dom", "Lun", "Mar", "Mié", "Jue", "Vie", "Sáb"],
          longhand:  ["Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"]
        },
        months: {
          shorthand: ["Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"],
          longhand:  ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"]
        }
      },
      onChange: (selectedDates) => {
        if (selectedDates.length === 2) {
          const fmt = d => d.toISOString().split("T")[0];
          state[module].dateStart = fmt(selectedDates[0]);
          state[module].dateEnd   = fmt(selectedDates[1]);
          state[module].page = 1;
          const lbl = document.getElementById(`fp-${module}-label`);
          if (lbl) lbl.innerText = `${formatISOToSpanish(state[module].dateStart)} → ${formatISOToSpanish(state[module].dateEnd)}`;
          if (module === "archivo") triggerArchivoSearch(); else triggerRrhhSearch();
        }
      }
    });
  }

  const sy = document.getElementById(`year-select-${module}`);
  if (sy) {
    // Usar lista exacta de años con datos si está disponible (más precisa)
    const years = (data.years && data.years.length)
      ? data.years
      : (() => {
          const minY = parseInt(data.min_date.substring(0, 4));
          const maxY = parseInt(data.max_date.substring(0, 4));
          return Array.from({ length: maxY - minY + 1 }, (_, i) => maxY - i);
        })();
    sy.innerHTML = `<option value="">Seleccionar año…</option>` +
      years.map(y => `<option value="${y}">${y}</option>`).join("");
  }

  state[module].dateStart = data.min_date;
  state[module].dateEnd   = data.max_date;
  _setChipActive(module, "all");
  const lbl = document.getElementById(`fp-${module}-label`);
  if (lbl) lbl.innerText = `${formatISOToSpanish(data.min_date)} → ${formatISOToSpanish(data.max_date)}`;
}

function _setChipActive(module, preset) {
  document.querySelectorAll(`.ds-date-chip[data-module="${module}"]`).forEach(btn => {
    btn.classList.toggle("active", btn.dataset.preset === preset);
  });
}

function applyDatePreset(module, preset) {
  _setChipActive(module, preset);
  const yearPanel  = document.getElementById(`year-panel-${module}`);
  const rangePanel = document.getElementById(`range-panel-${module}`);
  const lbl = document.getElementById(`fp-${module}-label`);
  const lim = state.choices?.[module];

  if (preset === "year") {
    if (yearPanel)  yearPanel.style.display  = "";
    if (rangePanel) rangePanel.style.display = "none";
    if (lbl) lbl.innerText = "";
    return;
  }
  if (preset === "custom") {
    if (yearPanel)  yearPanel.style.display  = "none";
    if (rangePanel) rangePanel.style.display = "";
    if (lbl) lbl.innerText = "";
    return;
  }

  if (yearPanel)  yearPanel.style.display  = "none";
  if (rangePanel) rangePanel.style.display = "none";

  const today = new Date();
  const fmt   = d => d.toISOString().split("T")[0];
  let startDate, endDate = fmt(today);

  if (preset === "all") {
    startDate = lim?.min_date || fmt(new Date(today.getFullYear() - 10, 0, 1));
    endDate   = lim?.max_date || fmt(today);
  }

  state[module].dateStart = startDate;
  state[module].dateEnd   = endDate;
  state[module].page = 1;
  if (lbl) lbl.innerText = `${formatISOToSpanish(startDate)} → ${formatISOToSpanish(endDate)}`;
  if (module === "archivo") triggerArchivoSearch(); else triggerRrhhSearch();
}

function handleYearSelect(module) {
  const sy = document.getElementById(`year-select-${module}`);
  if (!sy || !sy.value) return;
  const y = sy.value;
  state[module].dateStart = `${y}-01-01`;
  state[module].dateEnd   = `${y}-12-31`;
  state[module].page = 1;
  const lbl = document.getElementById(`fp-${module}-label`);
  if (lbl) lbl.innerText = `Año ${y}`;
  if (module === "archivo") triggerArchivoSearch(); else triggerRrhhSearch();
}


function resetDateFilters(module) {
  state[module].search       = "";
  state[module].selectedTypes = [];
  state[module].page = 1;
  if (module === "archivo") {
    state.archivo.selectedTesauro = [];
    const s = document.getElementById("search_archivo");
    if (s) s.value = "";
    if (tsInstances["choice-archivo-doc-type"]) tsInstances["choice-archivo-doc-type"].clear(true);
    if (tsInstances["choice-archivo-tesauro"])  tsInstances["choice-archivo-tesauro"].clear(true);
  } else {
    state.rrhh.selectedEstados = [];
    state.rrhh.selectedPeople  = [];
    const s = document.getElementById("search_rrhh");
    if (s) s.value = "";
    if (tsInstances["choice-rrhh-doc-type"]) tsInstances["choice-rrhh-doc-type"].clear(true);
    if (tsInstances["choice-rrhh-estado"])   tsInstances["choice-rrhh-estado"].clear(true);
    if (tsInstances["choice-rrhh-people"])   tsInstances["choice-rrhh-people"].clear(true);
  }
  applyDatePreset(module, "all");
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
  const isSPA = !!document.getElementById("app-portal");
  safeOn("menu-btn-archivo",       "click", e => { if (!isSPA) return; e.preventDefault(); switchTab("archivo"); });
  safeOn("menu-btn-rrhh",          "click", e => { if (!isSPA) return; e.preventDefault(); switchTab("rrhh"); });
  safeOn("menu-btn-admin-archivo", "click", e => { if (!isSPA) return; e.preventDefault(); if (state.user) state.user.modulo = "Archivo"; switchTab("admin-archivo"); });
  safeOn("menu-btn-admin-rrhh",    "click", e => { if (!isSPA) return; e.preventDefault(); if (state.user) state.user.modulo = "RRHH";    switchTab("admin-rrhh"); });
  safeOn("module_switch_btn",      "click", handleModuleSwitch);

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
    ["stats", "new", "monitor", "categories", "users", "audit"].forEach(t => {
      document.getElementById(`tab-admin-${suf}-${t}`)?.addEventListener("click", e => { e.preventDefault(); loadAdminTab(t); });
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
  const blob = new Blob(["﻿" + csv], { type: "text/csv;charset=utf-8;" });
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

// ==========================================================================
// SISTEMA DE PERSONALIZACIÓN
// ==========================================================================
const THEMES = [
  { id:"default",            name:"Clásico UCV",         sidebar:"#2b4e72", content:"#f8f9fa", bar:"#2b4e72" },
  { id:"theme-azul-profundo",name:"Azul Profundo",       sidebar:"#0f3460", content:"#e8f3ff", bar:"#1a5fa0" },
  { id:"theme-dorado",       name:"Dorado Académico",    sidebar:"#9a7017", content:"#fffdf0", bar:"#B8860B" },
  { id:"theme-manila",       name:"Manila / Cartón",     sidebar:"#8B7355", content:"#f5ead0", bar:"#A0856A" },
  { id:"theme-esmeralda",    name:"Verde Esmeralda",     sidebar:"#2d6a4f", content:"#f0faf2", bar:"#40916c" },
  { id:"theme-cian",         name:"Cian Institucional",  sidebar:"#0d6e7c", content:"#e8fafc", bar:"#13a0b4" },
  { id:"theme-lavanda",      name:"Lavanda",             sidebar:"#4a3570", content:"#f5f0ff", bar:"#7c5cbf" },
  { id:"theme-granate",      name:"Granate Académico",   sidebar:"#7c2d3c", content:"#fff5f7", bar:"#a03048" },
  { id:"theme-terracota",    name:"Terracota",           sidebar:"#b5451b", content:"#fff8f5", bar:"#d4623a" },
  { id:"theme-noche",        name:"Medianoche",          sidebar:"#0d1b2a", content:"#f0f4f8", bar:"#4a90d9" },
  { id:"theme-carbon",       name:"Gris Carbón",         sidebar:"#2c2c2c", content:"#f8f8f8", bar:"#555555" },
  { id:"theme-oliva",        name:"Verde Oliva",         sidebar:"#5a6b2a", content:"#f5fae0", bar:"#7a9040" },
];

const FONT_SCALES = [
  { value: 0.82, label: "Pequeño" },
  { value: 0.93, label: "Normal"  },
  { value: 1.08, label: "Grande"  },
  { value: 1.22, label: "Muy Grande" },
];

// ── init ──────────────────────────────────────────────────────────────────────

function initTheme() {
  const savedTheme = localStorage.getItem("ds_theme");
  if (savedTheme && savedTheme !== "default") document.body.classList.add(savedTheme);

  const savedScale = parseFloat(localStorage.getItem("ds_font_scale"));
  _applyFontScaleDOM(isNaN(savedScale) ? 0.93 : savedScale);

  const darkPref = localStorage.getItem("ds_dark_mode") || "auto";
  _applyDarkModeDOM(darkPref);

  window.matchMedia?.("(prefers-color-scheme: dark)").addEventListener("change", () => {
    if ((localStorage.getItem("ds_dark_mode") || "auto") === "auto") {
      _applyDarkModeDOM("auto");
    }
  });
}

// ── dark mode ─────────────────────────────────────────────────────────────────

function _applyDarkModeDOM(mode) {
  const isDark = mode === "dark" ||
    (mode === "auto" && window.matchMedia?.("(prefers-color-scheme: dark)").matches);
  document.body.classList.toggle("dark-mode", isDark);
}

function applyDarkMode(mode) {
  localStorage.setItem("ds_dark_mode", mode);
  _applyDarkModeDOM(mode);
}

// ── font scale ────────────────────────────────────────────────────────────────

function _applyFontScaleDOM(scale) {
  document.documentElement.style.setProperty("--ds-font-scale", scale);
}

function applyFontScale(scale) {
  localStorage.setItem("ds_font_scale", scale);
  _applyFontScaleDOM(scale);
}

// ── color theme ───────────────────────────────────────────────────────────────

function applyTheme(themeId) {
  document.body.className = document.body.className.replace(/\btheme-[\w-]+\b/g, "").trim();
  if (themeId !== "default") document.body.classList.add(themeId);
  localStorage.setItem("ds_theme", themeId);
  _rebuildPanel();
}

// ── panel ─────────────────────────────────────────────────────────────────────

function openThemePanel() {
  let panel = document.getElementById("ds-theme-panel");
  if (!panel) { panel = _createThemePanel(); document.body.appendChild(panel); }
  requestAnimationFrame(() => panel.classList.add("open"));
}

function closeThemePanel() {
  document.getElementById("ds-theme-panel")?.classList.remove("open");
}

function _rebuildPanel() {
  const old = document.getElementById("ds-theme-panel");
  if (!old) return;
  const fresh = _createThemePanel();
  old.replaceWith(fresh);
  requestAnimationFrame(() => fresh.classList.add("open"));
}

function resetPersonalization() {
  ["ds_theme","ds_dark_mode","ds_font_scale"].forEach(k => localStorage.removeItem(k));
  document.body.className = document.body.className.replace(/\btheme-[\w-]+\b|\bdark-mode\b/g, "").trim();
  _applyFontScaleDOM(0.93);
  _applyDarkModeDOM("auto");
  _rebuildPanel();
}

function _createThemePanel() {
  const cTheme = localStorage.getItem("ds_theme") || "default";
  const cDark  = localStorage.getItem("ds_dark_mode") || "auto";
  const cScale = parseFloat(localStorage.getItem("ds_font_scale")) || 0.93;
  const cScaleLabel = FONT_SCALES.find(s => s.value === cScale)?.label || "Normal";

  const darkModes = [
    { id:"light", icon:"fas fa-sun",              label:"Claro"  },
    { id:"auto",  icon:"fas fa-circle-half-stroke",label:"Auto"  },
    { id:"dark",  icon:"fas fa-moon",             label:"Oscuro" },
  ];

  const panel = document.createElement("div");
  panel.id = "ds-theme-panel";
  panel.className = "ds-theme-panel";
  panel.innerHTML = `
    <div class="ds-theme-panel-header">
      <h6><i class="fas fa-sliders-h mr-2"></i>Personalización</h6>
      <button class="ds-theme-panel-close" onclick="closeThemePanel()" title="Cerrar"><i class="fas fa-times"></i></button>
    </div>
    <div class="ds-theme-panel-body">

      <div class="ds-panel-section">
        <div class="ds-panel-label"><i class="fas fa-adjust mr-1"></i> Modo Oscuro</div>
        <div class="ds-dark-toggle">
          ${darkModes.map(m => `
            <button class="ds-dark-btn${cDark === m.id ? ' active' : ''}"
              onclick="applyDarkMode('${m.id}'); _rebuildPanel()" title="${m.label}">
              <i class="${m.icon}"></i><span>${m.label}</span>
            </button>`).join("")}
        </div>
      </div>

      <div class="ds-panel-section">
        <div class="ds-panel-label"><i class="fas fa-text-height mr-1"></i> Tamaño de Texto <span class="ds-font-current">${cScaleLabel}</span></div>
        <div class="ds-font-row">
          ${FONT_SCALES.map((s, i) => `
            <button class="ds-font-btn${s.value === cScale ? ' active' : ''}"
              onclick="applyFontScale(${s.value}); _rebuildPanel()" title="${s.label}">
              <span style="font-size:${0.72 + i * 0.12}rem;font-weight:600;line-height:1">A</span>
            </button>`).join("")}
        </div>
      </div>

      <div class="ds-panel-section">
        <div class="ds-panel-label"><i class="fas fa-palette mr-1"></i> Color del Sistema</div>
        <div class="ds-theme-grid">
          ${THEMES.map(t => `
            <div class="ds-theme-card${cTheme === t.id ? ' active' : ''}" onclick="applyTheme('${t.id}')" title="${t.name}">
              <div class="ds-theme-preview">
                <div class="ds-theme-preview-sidebar" style="background:${t.sidebar};"></div>
                <div class="ds-theme-preview-content" style="background:${t.content};">
                  <div class="ds-theme-preview-bar" style="background:${t.bar};opacity:0.75;height:7px;width:80%;"></div>
                  <div class="ds-theme-preview-bar" style="background:#dee2e6;height:4px;width:90%;"></div>
                  <div class="ds-theme-preview-bar" style="background:#dee2e6;height:4px;width:65%;"></div>
                </div>
              </div>
              <div class="ds-theme-name">${t.name}</div>
            </div>`).join("")}
        </div>
      </div>

      <div class="ds-panel-section ds-panel-reset">
        <button class="ds-reset-btn" onclick="resetPersonalization()">
          <i class="fas fa-undo mr-1"></i> Restablecer valores predeterminados
        </button>
      </div>

    </div>`;
  return panel;
}

// ── NOTIFICACIONES Y PENDIENTES ──────────────────────────────────────────────
let _notifInterval = null;

function _initNotificationBell(user) {
  // Solo mostrar a usuarios con rol admin
  const isAdmin = user.roles && (user.roles["Archivo"] === "Admin" || user.roles["RRHH"] === "Admin");
  if (!isAdmin) return;

  // Inyectar el botón de campana antes del usuario en la barra
  const navUser = document.querySelector(".ds-nav-user");
  if (!navUser || document.getElementById("ds-notif-btn")) return;

  const li = document.createElement("li");
  li.className = "nav-item mr-2";
  li.style.position = "relative";
  li.innerHTML = `
    <button id="ds-notif-btn" class="btn btn-link nav-link p-0 px-2" title="Notificaciones y pendientes"
      style="font-size:1.2rem;color:#2b4e72;position:relative;" onclick="toggleNotifPanel()">
      <i class="fas fa-bell"></i>
      <span id="ds-notif-badge" class="badge badge-danger"
        style="position:absolute;top:2px;right:2px;font-size:0.6rem;padding:2px 4px;min-width:16px;display:none;">0</span>
    </button>
    <div id="ds-notif-panel" class="ds-notif-panel" style="display:none;">
      <div class="ds-notif-header">
        <strong><i class="fas fa-bell mr-1"></i>Pendientes</strong>
        <button class="btn btn-xs btn-link text-muted" onclick="loadNotifications()" title="Actualizar">
          <i class="fas fa-sync-alt"></i>
        </button>
      </div>
      <div id="ds-notif-list" class="ds-notif-list">
        <div class="ds-notif-empty">Cargando...</div>
      </div>
      <div class="ds-notif-footer">
        <a href="/admin/archivo" class="btn btn-xs btn-outline-primary mr-1">Archivo</a>
        <a href="/admin/rrhh"    class="btn btn-xs btn-outline-warning">RRHH</a>
      </div>
    </div>`;
  navUser.before(li);

  // Cerrar al hacer clic fuera
  document.addEventListener("click", e => {
    const panel = document.getElementById("ds-notif-panel");
    if (panel && !panel.contains(e.target) && e.target.id !== "ds-notif-btn" && !e.target.closest("#ds-notif-btn")) {
      panel.style.display = "none";
    }
  });

  loadNotifications();
  if (_notifInterval) clearInterval(_notifInterval);
  _notifInterval = setInterval(loadNotifications, 90000);  // poll cada 90s
}

function toggleNotifPanel() {
  const panel = document.getElementById("ds-notif-panel");
  if (!panel) return;
  const visible = panel.style.display !== "none";
  panel.style.display = visible ? "none" : "block";
  if (!visible) loadNotifications();
}

async function loadNotifications() {
  const badge = document.getElementById("ds-notif-badge");
  const list  = document.getElementById("ds-notif-list");
  if (!badge || !list || !state.user) return;

  const modulo = state.user.modules?.includes("Archivo") && state.user.modules?.includes("RRHH")
    ? "Global"
    : (state.user.modulo || "");
  try {
    const res = await fetch(`${API_BASE}/api/admin/notifications?modulo=${encodeURIComponent(modulo)}`);
    if (!res.ok) return;
    const data = await res.json();
    const total = data.total || 0;

    // Badge
    badge.textContent = total > 99 ? "99+" : total;
    badge.style.display = total > 0 ? "inline-block" : "none";

    // Panel list
    if (total === 0) {
      list.innerHTML = '<div class="ds-notif-empty"><i class="fas fa-check-circle mr-1 text-success"></i>Sin pendientes</div>';
      return;
    }

    const STATUS_ICON = { revision: "fas fa-clock text-warning", draft: "fas fa-pencil-alt text-secondary" };
    const STATUS_LBL  = { revision: "Revisión", draft: "Borrador" };

    list.innerHTML = (data.items || []).slice(0, 15).map(it => {
      const icon = STATUS_ICON[it.status] || "fas fa-file";
      const lbl  = STATUS_LBL[it.status]  || it.status;
      const href = it.modulo === "Archivo"
        ? `/admin/archivo?docId=${it.id}`
        : `/admin/rrhh?empId=${it.id}`;
      return `<a class="ds-notif-item" href="${href}" onclick="document.getElementById('ds-notif-panel').style.display='none'">
        <i class="${icon}" style="width:14px;flex-shrink:0;"></i>
        <div class="ds-notif-item-body">
          <div class="ds-notif-item-label">${it.label || '—'}</div>
          <div class="ds-notif-item-meta">${it.modulo} · ${lbl} · ${it.ts || ''}</div>
        </div>
      </a>`;
    }).join("");
  } catch { /* silencioso */ }
}
