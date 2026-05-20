// ==========================================================================
// ESTADO GLOBAL DE LA SPA
// ==========================================================================
const state = {
  user: null,
  activeTab: "archivo",
  choices: null,
  
  // Mapeo de pestaña activa en Panel de Control Admin
  activeAdminTab: "stats",

  // Datos Archivo
  archivo: {
    results: [],
    search: "",
    selectedTypes: [],
    selectedTesauro: [],
    dateStart: "",
    dateEnd: "",
    sortMode: "Alfabético (A-Z)",
    page: 1,
    perPage: 5
  },
  
  // Datos RRHH
  rrhh: {
    results: [],
    search: "",
    selectedTypes: [],
    selectedEstados: [],
    dateStart: "",
    dateEnd: "",
    sortMode: "Alfabético (A-Z)",
    page: 1,
    perPage: 5
  },

  // Monitor Tabla Admin
  adminTable: {
    results: [],
    search: "",
    typeFilter: "",
    page: 1,
    perPage: 8
  },

  // Cache para el perfil detallado que se está visualizando en el modal de RRHH
  activePersonProfile: null,
  innerDossierSearch: "",
  innerDossierClass: "",
  innerDossierSort: "Alfabético (A-Z)"
};

const API_BASE = window.location.origin;

// ==========================================================================
// INICIALIZACIÓN Y CICLO DE VIDA DE LA SESIÓN (F5 PERSISTENCE)
// ==========================================================================
document.addEventListener("DOMContentLoaded", () => {
  setupEventListeners();
  checkPersistedSession();
});

// Helper para panel admin namespaced
function adminSuffixFromTab(tab) {
  const t = tab || state.activeTab;
  return (t === 'admin-rrhh') ? 'rrhh' : 'archivo';
}
function adminId(base) {
  return `${base}-${adminSuffixFromTab()}`;
}
function getAdminEl(base) {
  return document.getElementById(adminId(base));
}

async function checkPersistedSession() {
  const raw = localStorage.getItem("archive_session");
  if (!raw) {
    if (document.body.dataset.page) window.location.href = "/";
    return;
  }
  
  try {
    const saved = JSON.parse(raw);
    const ttlMs = 12 * 60 * 60 * 1000; // 12 Horas
    if (saved && saved.username && saved.ts && (Date.now() - saved.ts) < ttlMs) {
      // Confirmar asíncronamente con el backend de forma stateless
      const res = await fetch(`${API_BASE}/api/auth/restore`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ username: saved.username })
      });
      if (res.ok) {
        const data = await res.json();
        loginSuccess(data.user);
      } else {
        logout();
      }
    } else {
      logout();
    }
  } catch (e) {
    logout();
  }
}

function loginSuccess(user) {
  // Force modulo for standalone admin pages
  const pageAttr = document.body.dataset.page;
  if (pageAttr === "admin-archivo") user.modulo = "Archivo";
  else if (pageAttr === "admin-rrhh") user.modulo = "RRHH";

  // Normalizar estructura de usuario: soportar `modules` y `roles` (multi-módulo)
  user.modules = user.modules || (user.modulo ? [user.modulo] : []);
  user.roles = user.roles || { };
  if (!user.roles || Object.keys(user.roles).length === 0) {
    // compatibilidad con API antigua
    if (user.modulo) user.roles[user.modulo] = user.rol || "Normal";
  }

  state.user = user;

  // Guardar sesión persistente
  localStorage.setItem("archive_session", JSON.stringify({
    username: user.username,
    modules: user.modules,
    roles: user.roles,
    modulo: user.modulo,
    rol: user.rol,
    ts: Date.now()
  }));
  
  // Ajustar vistas en UI
  const loginScr = document.getElementById("login-screen");
  const appPortal = document.getElementById("app-portal");
  if (loginScr) loginScr.style.setProperty("display", "none", "important");
  if (appPortal) appPortal.style.display = "block";
  
  // Mostrar username + módulo activo y rol
  const activeRole = user.roles && user.roles[user.modulo] ? user.roles[user.modulo] : user.rol;
  document.getElementById("nav_username").innerText = `ID: ${user.username} (${user.modulo} - ${activeRole})`;
  
  // CargarChoices dinámicos
  loadDynamicChoices();
  
  // Control de accesos a pestañas del sidebar
  configureSidebarVisibilities(user);

  // Mostrar botón de cambio de módulo si tiene más de uno
  const switchBtn = document.getElementById("module_switch_btn");
  if (switchBtn) {
    if (user.modules && user.modules.length > 1) {
      switchBtn.style.display = "inline-block";
      switchBtn.innerText = `Switch (${user.modules.join(" / ")})`;
    } else {
      switchBtn.style.display = "none";
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
  const linkArchivo = document.getElementById("menu-btn-archivo");
  const linkRrhh = document.getElementById("menu-btn-rrhh");
  const linkAdminArchivo = document.getElementById("menu-btn-admin-archivo");
  const linkAdminRrhh = document.getElementById("menu-btn-admin-rrhh");
  
  if (linkArchivo) linkArchivo.style.display = "none";
  if (linkRrhh) linkRrhh.style.display = "none";
  if (linkAdminArchivo) linkAdminArchivo.style.display = "none";
  if (linkAdminRrhh) linkAdminRrhh.style.display = "none";
  // Mostrar módulos disponibles
  const modules = user.modules || (user.modulo ? [user.modulo] : []);
  modules.forEach(m => {
    if (m === "Archivo" && linkArchivo) linkArchivo.style.display = "flex";
    if (m === "RRHH" && linkRrhh) linkRrhh.style.display = "flex";
  });

  // Mostrar admin específico por módulo si tiene rol Admin en ese módulo
  const roleArchivo = (user.roles && user.roles["Archivo"]) ? user.roles["Archivo"] : null;
  const roleRrhh = (user.roles && user.roles["RRHH"]) ? user.roles["RRHH"] : null;
  if (roleArchivo === "Admin" && linkAdminArchivo) linkAdminArchivo.style.display = "flex";
  if (roleRrhh === "Admin" && linkAdminRrhh) linkAdminRrhh.style.display = "flex";

  // En páginas standalone: controlar acceso y usar el tab de la página actual
  const standalonePage = document.body.dataset.page;
  if (standalonePage) {
    const mods = user.modules || (user.modulo ? [user.modulo] : []);
    const rArch = (user.roles && user.roles["Archivo"]) ? user.roles["Archivo"] : null;
    const rRrhh = (user.roles && user.roles["RRHH"]) ? user.roles["RRHH"] : null;
    let allowed = false;
    if (standalonePage === "archivo") allowed = mods.includes("Archivo");
    else if (standalonePage === "rrhh") allowed = mods.includes("RRHH");
    else if (standalonePage === "admin-archivo") allowed = rArch === "Admin";
    else if (standalonePage === "admin-rrhh") allowed = rRrhh === "Admin";
    if (!allowed) {
      if (rArch === "Admin") { window.location.href = "/admin/archivo"; return; }
      if (rRrhh === "Admin") { window.location.href = "/admin/rrhh"; return; }
      if (mods.includes("RRHH")) { window.location.href = "/rrhh"; return; }
      window.location.href = "/archivo";
      return;
    }
  }
  const defaultTab = standalonePage || ((user.modulo === "RRHH") ? "rrhh" : "archivo");
  switchTab(defaultTab);
}

// Cambiar módulo activo (para usuarios con varios módulos)
function handleModuleSwitch() {
  if (!state.user || !state.user.modules || state.user.modules.length < 2) return;
  const current = state.user.modulo;
  const idx = state.user.modules.indexOf(current);
  const next = state.user.modules[(idx + 1) % state.user.modules.length];
  state.user.modulo = next;

  // Actualizar nav y visibilidades
  const activeRole = state.user.roles && state.user.roles[state.user.modulo] ? state.user.roles[state.user.modulo] : "Normal";
  document.getElementById("nav_username").innerText = `ID: ${state.user.username} (${state.user.modulo} - ${activeRole})`;
  configureSidebarVisibilities(state.user);

  // Cambiar a la pestaña de búsqueda del módulo seleccionado
  const tab = (state.user.modulo === "RRHH") ? "rrhh" : "archivo";
  switchTab(tab);
}

function switchTab(tabId) {
  state.activeTab = tabId;
  
  // Clases active
  document.querySelectorAll(".ds-sidebar-link").forEach(link => {
    link.classList.remove("active");
  });
  const activeLink = document.getElementById(`menu-btn-${tabId}`);
  if (activeLink) activeLink.classList.add("active");
  
  // Cerrar sidebar al cambiar de pestaña
  closeSidebar();
  
  // Secciones
  document.querySelectorAll(".app-tab-section").forEach(sec => sec.style.display = "none");
  const tabArchivo = document.getElementById("tab-archivo");
  const tabRrhh = document.getElementById("tab-rrhh");
  const tabAdminArchivo = document.getElementById("tab-admin-archivo");
  const tabAdminRrhh = document.getElementById("tab-admin-rrhh");
  if (tabId === "archivo" && tabArchivo) {
    tabArchivo.style.display = "block";
    triggerArchivoSearch();
  } else if (tabId === "rrhh" && tabRrhh) {
    tabRrhh.style.display = "block";
    triggerRrhhSearch();
  } else if (tabId === "admin-archivo" && tabAdminArchivo) {
    state.activeTab = tabId;
    tabAdminArchivo.style.display = "block";
    loadAdminTab("stats");
  } else if (tabId === "admin-rrhh" && tabAdminRrhh) {
    state.activeTab = tabId;
    tabAdminRrhh.style.display = "block";
    loadAdminTab("stats");
  }
}

// ==========================================================================
// SIDEBAR COLAPSABLE - APERTURA / CIERRE CON ANIMACIÓN
// ==========================================================================
function openSidebar() {
  document.getElementById("app-sidebar").classList.add("open");
  document.getElementById("sidebar-overlay").classList.add("open");
}

function closeSidebar() {
  document.getElementById("app-sidebar").classList.remove("open");
  document.getElementById("sidebar-overlay").classList.remove("open");
}

// ==========================================================================
// FILTROS Y CHOICES DINÁMICOS DESDE backend
// ==========================================================================
async function loadDynamicChoices() {
  try {
    const res = await fetch(`${API_BASE}/api/choices`);
    if (!res.ok) throw new Error();
    const data = await res.json();
    state.choices = data;
    
    // Asignar fechas iniciales
    state.archivo.dateStart = data.archivo.min_date;
    state.archivo.dateEnd = data.archivo.max_date;
    state.rrhh.dateStart = data.rrhh.min_date;
    state.rrhh.dateEnd = data.rrhh.max_date;
    
    // Inicializar selectores Tom Select
    initTomSelects();
    
    // Configurar controladores de fechas
    initDateControls("archivo", data.archivo);
    initDateControls("rrhh", data.rrhh);
    
  } catch (e) {
    console.error("Error al cargar choices dinámicos:", e);
  }
}

const tsInstances = {};

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

  makeSel("choice-archivo-doc-type", state.choices.archivo.doc_types, (val) => {
    state.archivo.selectedTypes = Array.isArray(val) ? val : (val ? [val] : []);
    state.archivo.page = 1; triggerArchivoSearch();
  });
  makeSel("choice-archivo-tesauro", state.choices.archivo.tesauro, (val) => {
    state.archivo.selectedTesauro = Array.isArray(val) ? val : (val ? [val] : []);
    state.archivo.page = 1; triggerArchivoSearch();
  });
  makeSel("choice-rrhh-doc-type", state.choices.rrhh.doc_types, (val) => {
    state.rrhh.selectedTypes = Array.isArray(val) ? val : (val ? [val] : []);
    state.rrhh.page = 1; triggerRrhhSearch();
  });
  makeSel("choice-rrhh-estado", state.choices.rrhh.estados, (val) => {
    state.rrhh.selectedEstados = Array.isArray(val) ? val : (val ? [val] : []);
    state.rrhh.page = 1; triggerRrhhSearch();
  });
}

// ==========================================================================
// SINCRONIZACIÓN DE FECHAS BIDIRECCIONALES (LÓGICA R SHINY EXACTA)
// ==========================================================================
function initDateControls(module, data) {
  const minYear = parseInt(data.min_date.substring(0, 4));
  const maxYear = parseInt(data.max_date.substring(0, 4));

  const sliderStart = document.getElementById(`slider-${module}-start`);
  const sliderEnd = document.getElementById(`slider-${module}-end`);
  if (!sliderStart || !sliderEnd) return;

  sliderStart.min = minYear; sliderStart.max = maxYear; sliderStart.value = minYear;
  sliderEnd.min = minYear; sliderEnd.max = maxYear; sliderEnd.value = maxYear;

  const labelStart = document.getElementById(`label-${module}-start`);
  const labelEnd = document.getElementById(`label-${module}-end`);
  if (labelStart) labelStart.innerText = minYear;
  if (labelEnd) labelEnd.innerText = maxYear;

  const selectYear = document.getElementById(`select-${module}-year`);
  if (selectYear) selectYear.innerHTML = `<option value="">Todos los años</option>` +
    Array.from({ length: maxYear - minYear + 1 }, (_, i) => minYear + i)
      .reverse().map(y => `<option value="${y}">${y}</option>`).join("");

  const ds = document.getElementById(`date-${module}-start`);
  const de = document.getElementById(`date-${module}-end`);
  if (ds) ds.value = data.min_date;
  if (de) de.value = data.max_date;
}

function handleDateChange(module, source) {
  const dateStartInput = document.getElementById(`date-${module}-start`);
  const dateEndInput = document.getElementById(`date-${module}-end`);
  const sliderStart = document.getElementById(`slider-${module}-start`);
  const sliderEnd = document.getElementById(`slider-${module}-end`);
  const selectYear = document.getElementById(`select-${module}-year`);
  
  let startVal = dateStartInput.value;
  let endVal = dateEndInput.value;
  
  if (source === "slider") {
    const sYear = parseInt(sliderStart.value);
    const eYear = parseInt(sliderEnd.value);
    
    if (sYear > eYear) {
      sliderStart.value = eYear;
      return;
    }
    
    startVal = `${sYear}-01-01`;
    endVal = `${eYear}-12-31`;
    
    dateStartInput.value = startVal;
    dateEndInput.value = endVal;
    selectYear.value = (sYear === eYear) ? sYear : "";
    
  } else if (source === "select") {
    const targetYear = selectYear.value;
    if (targetYear) {
      startVal = `${targetYear}-01-01`;
      endVal = `${targetYear}-12-31`;
      
      dateStartInput.value = startVal;
      dateEndInput.value = endVal;
      sliderStart.value = targetYear;
      sliderEnd.value = targetYear;
    } else {
      const limits = state.choices[module];
      startVal = limits.min_date;
      endVal = limits.max_date;
      
      dateStartInput.value = startVal;
      dateEndInput.value = endVal;
      sliderStart.value = limits.min_date.substring(0, 4);
      sliderEnd.value = limits.max_date.substring(0, 4);
    }
  } else if (source === "calendar") {
    const sYear = startVal ? parseInt(startVal.substring(0, 4)) : null;
    const eYear = endVal ? parseInt(endVal.substring(0, 4)) : null;
    if (sYear && eYear) {
      sliderStart.value = sYear;
      sliderEnd.value = eYear;
      selectYear.value = (sYear === eYear) ? sYear : "";
    }
  }
  
  document.getElementById(`label-${module}-start`).innerText = sliderStart.value;
  document.getElementById(`label-${module}-end`).innerText = sliderEnd.value;
  
  state[module].dateStart = startVal;
  state[module].dateEnd = endVal;
  state[module].page = 1;
  
  if (module === "archivo") triggerArchivoSearch();
  else triggerRrhhSearch();
}

function resetDateFilters(module) {
  state[module].search = "";
  state[module].selectedTypes = [];
  state[module].page = 1;
  
  if (module === "archivo") {
    state.archivo.selectedTesauro = [];
    const sa = document.getElementById("search_archivo");
    if (sa) sa.value = "";
    if (tsInstances["choice-archivo-doc-type"]) tsInstances["choice-archivo-doc-type"].clear(true);
    if (tsInstances["choice-archivo-tesauro"]) tsInstances["choice-archivo-tesauro"].clear(true);
  } else {
    state.rrhh.selectedEstados = [];
    const sr = document.getElementById("search_rrhh");
    if (sr) sr.value = "";
    if (tsInstances["choice-rrhh-doc-type"]) tsInstances["choice-rrhh-doc-type"].clear(true);
    if (tsInstances["choice-rrhh-estado"]) tsInstances["choice-rrhh-estado"].clear(true);
  }
  
  const limits = state.choices[module];
  state[module].dateStart = limits.min_date;
  state[module].dateEnd = limits.max_date;
  
  initDateControls(module, limits);
  
  if (module === "archivo") triggerArchivoSearch();
  else triggerRrhhSearch();
}

// ==========================================================================
// RENDERIZADO DE BÚSQUEDA Y TARJETAS DSPACE 7 (100% FIDELIDAD)
// ==========================================================================
async function triggerArchivoSearch() {
  const payload = {
    search_term: state.archivo.search,
    doc_types: state.archivo.selectedTypes,
    tesauro_terms: state.archivo.selectedTesauro,
    date_start: state.archivo.dateStart,
    date_end: state.archivo.dateEnd,
    sort_mode: state.archivo.sortMode
  };
  
  try {
    const res = await fetch(`${API_BASE}/api/archivo/buscar`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    });
    
    if (!res.ok) throw new Error();
    const data = await res.json();
    state.archivo.results = data;
    
    renderArchivoList();
    
  } catch (e) {
    console.error("Error buscando archivo:", e);
  }
}

function renderArchivoList() {
  const container = document.getElementById("list_archivo");
  const results = state.archivo.results;
  
  document.getElementById("count-archivo-results").innerText = `${results.length} Registros`;
  
  if (results.length === 0) {
    container.innerHTML = `<div class="alert alert-secondary text-center p-4">
      <i class="fas fa-search fa-2x mb-2 text-muted"></i>
      <p class="mb-0">No se encontraron folios correspondientes a los criterios ingresados.</p>
    </div>`;
    document.getElementById("info-archivo-pagination").innerText = "Pág 1 de 1";
    return;
  }
  
  const totalPages = Math.ceil(results.length / state.archivo.perPage);
  if (state.archivo.page > totalPages) state.archivo.page = totalPages;
  
  const start = (state.archivo.page - 1) * state.archivo.perPage;
  const pageItems = results.slice(start, start + state.archivo.perPage);
  
  document.getElementById("info-archivo-pagination").innerText = `Pág ${state.archivo.page} de ${totalPages}`;
  
  container.innerHTML = pageItems.map(doc => `
    <div class="ds-item-card" onclick="openArchivoModal('${doc.__idx}')" style="cursor:pointer;">
      <!-- Thumbnail -->
      <div class="ds-item-thumbnail">
        <i class="fas fa-file-alt" style="font-size: 40px; color: #2b4e72;"></i>
      </div>
      <!-- Metadata -->
      <div class="ds-item-metadata" style="flex-grow: 1; padding-left: 15px;">
        <div class="d-flex justify-content-between align-items-center mb-1">
          <span class="badge badge-light" style="font-size:0.75rem; color:#2b4e72; font-weight:bold; border: 1px solid #d9e6f4; border-radius:12px; padding: 3px 10px;"><i class="fas fa-bookmark mr-1"></i> ${doc.doc_type}</span>
          <span class="text-muted" style="font-size:0.8rem;"><i class="far fa-calendar-alt mr-1"></i> ${formatISOToSpanish(doc.fecha)}</span>
        </div>
        <h4 class="ds-item-title" style="font-size:1.1rem; font-weight:700; color:#2b4e72; margin: 4px 0;">${doc.titulo}</h4>
        <div class="ds-item-authors" style="font-size:0.82rem; color:#495057; margin-bottom: 2px;">
          <i class="fas fa-user-edit mr-1"></i> Autor: <strong>${doc.autor}</strong>
        </div>
        <div class="ds-item-publisher" style="font-size:0.82rem; color:#6c757d; margin-bottom: 4px;">
          <i class="fas fa-map-marker-alt mr-1"></i> Ubicación: <strong>${doc.ubicacion}</strong>
        </div>
        ${doc.resumen ? `<p class="ds-item-abstract text-muted m-0 mt-1" style="font-size:0.82rem; line-height:1.4; display: -webkit-box; -webkit-line-clamp: 2; line-clamp: 2; -webkit-box-orient: vertical; overflow: hidden;">${doc.resumen}</p>` : ""}
        <div class="ds-item-badges d-flex flex-wrap gap-1 mt-2">
          ${doc.tesauro_badges.slice(0, 4).map(b => `<span class="badge" style="background-color:#2b4e72; color:white; font-size:0.7rem; padding: 2px 7px; border-radius:4px; margin-right: 4px;">${b}</span>`).join("")}
          ${doc.tesauro_badges.length > 4 ? `<span class="badge" style="background-color:#6c757d; color:white; font-size:0.7rem; padding: 2px 7px; border-radius:4px;">+${doc.tesauro_badges.length - 4}</span>` : ""}
        </div>
      </div>
      <!-- Actions -->
      <div class="ds-item-actions" style="margin-left:15px; display:flex; flex-direction:column; justify-content:center;">
        <button class="btn btn-primary ds-action-btn" title="Ver" onclick="event.stopPropagation(); openArchivoModal('${doc.__idx}')" style="width:36px; height:36px; border-radius:50% !important; display:inline-flex; align-items:center; justify-content:center;">
          <i class="fas fa-eye"></i>
        </button>
      </div>
    </div>
  `).join("");
}

async function triggerRrhhSearch() {
  const payload = {
    search_term: state.rrhh.search,
    doc_types: state.rrhh.selectedTypes,
    estados: state.rrhh.selectedEstados,
    date_start: state.rrhh.dateStart,
    date_end: state.rrhh.dateEnd,
    sort_mode: state.rrhh.sortMode
  };
  
  try {
    const res = await fetch(`${API_BASE}/api/rrhh/buscar`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    });
    
    if (!res.ok) throw new Error();
    const data = await res.json();
    state.rrhh.results = data;
    
    renderRrhhList();
    
  } catch (e) {
    console.error("Error buscando RRHH:", e);
  }
}

function renderRrhhList() {
  const container = document.getElementById("list_rrhh");
  const results = state.rrhh.results;
  
  document.getElementById("count-rrhh-results").innerText = `${results.length} Registros`;
  
  if (results.length === 0) {
    container.innerHTML = `<div class="alert alert-secondary text-center p-4">
      <i class="fas fa-users fa-2x mb-2 text-muted"></i>
      <p class="mb-0">No se encontraron expedientes coincidentes con los filtros seleccionados.</p>
    </div>`;
    document.getElementById("info-rrhh-pagination").innerText = "Pág 1 de 1";
    return;
  }
  
  const totalPages = Math.ceil(results.length / state.rrhh.perPage);
  if (state.rrhh.page > totalPages) state.rrhh.page = totalPages;
  
  const start = (state.rrhh.page - 1) * state.rrhh.perPage;
  const pageItems = results.slice(start, start + state.rrhh.perPage);
  
  document.getElementById("info-rrhh-pagination").innerText = `Pág ${state.rrhh.page} de ${totalPages}`;
  
  container.innerHTML = pageItems.map(p => {
    const initials = getPersonInitials(p.persona_raw);
    const colorState = getStatusColor(p.estatuses);
    
    return `
      <div class="ds-item-card ds-person-card" onclick="openRrhhPersonDossier('${p.persona_raw}')" style="cursor:pointer;">
        <!-- Thumbnail -->
        <div class="ds-item-thumbnail" style="align-items: center; padding-top: 0;">
          <div style="width: 54px; height: 54px; border-radius: 50%; overflow: hidden; display: flex; align-items: center; justify-content: center; background: #eef4fb; border: 2px solid #dee2e6; flex-shrink: 0;">
            ${p.foto_url ? `<img src="${p.foto_url}" style="width:100%;height:100%;object-fit:cover;display:block;">` : `<span style="width:100%;height:100%;background:#2b4e72;color:#ffffff;display:flex;align-items:center;justify-content:center;font-weight:800;font-size:0.9rem;">${initials}</span>`}
          </div>
        </div>
        <!-- Metadata -->
        <div class="ds-item-metadata" style="flex-grow: 1; padding-left: 15px;">
          <h4 class="ds-item-title" style="font-size:1.1rem; font-weight:700; color:#2b4e72; margin: 0 0 4px 0;">${p.persona}</h4>
          <div style="font-size:0.82rem; color:#495057; line-height: 1.4;">
            <span class="mr-3"><i class="fas fa-id-card mr-1"></i> C.I: <strong>${p.cedulas}</strong></span>
            <span class="mr-3"><i class="fas fa-sitemap mr-1"></i> Adscripción: <strong>${p.departamentos}</strong></span><br>
            <span><i class="fas fa-user-tie mr-1"></i> Cargo: <strong>${p.cargos}</strong></span>
          </div>
          <div class="mt-2 d-flex align-items-center gap-1">
            <span class="badge" style="background-color: ${colorState}; color: white; padding: 4px 8px; border-radius: 4px; font-size:0.75rem;">${p.estatuses}</span>
            <span class="badge badge-secondary ml-1" style="padding: 4px 8px; border-radius: 4px; font-size:0.75rem;">${p.doc_count} documentos en expediente</span>
          </div>
        </div>
        <!-- Actions -->
        <div class="ds-item-actions" style="margin-left:15px; display:flex; flex-direction:column; justify-content:center;">
          <button class="btn btn-primary ds-action-btn" title="Ver" onclick="event.stopPropagation(); openRrhhPersonDossier('${p.persona_raw}')" style="width:36px; height:36px; border-radius:50% !important; display:inline-flex; align-items:center; justify-content:center;">
            <i class="fas fa-eye"></i>
          </button>
        </div>
      </div>
    `;
  }).join("");
}

// Helpers
function getPersonInitials(name) {
  if (!name) return "?";

  const normalized = String(name).trim();
  if (!normalized) return "?";

  const commaParts = normalized.split(",").map(part => part.trim()).filter(Boolean);
  if (commaParts.length >= 2) {
    const surnameToken = (commaParts[0].split(/\s+/).filter(Boolean)[0] || "").charAt(0);
    const givenToken = (commaParts[1].split(/\s+/).filter(Boolean)[0] || "").charAt(0);
    if (surnameToken || givenToken) return `${surnameToken}${givenToken}`.toUpperCase();
  }

  const parts = normalized.split(/\s+/).filter(Boolean);
  if (parts.length === 1) return parts[0].substring(0, 2).toUpperCase();
  if (parts.length >= 4) return (parts[0][0] + parts[2][0]).toUpperCase();
  return (parts[0][0] + parts[parts.length - 1][0]).toUpperCase();
}

function getStatusColor(status) {
  switch (status) {
    case "Activo": return "#28a745";
    case "Retirado": return "#dc3545";
    case "Retirado": return "#6f42c1";
    case "Pensionado": return "#0056b3";
    default: return "#6c757d";
  }
}

function formatISOToSpanish(iso) {
  if (!iso) return "";
  const parts = iso.split("-");
  if (parts.length !== 3) return iso;
  return `${parts[2]}/${parts[1]}/${parts[0]}`;
}

// ==========================================================================
// MODALES: ALTA FIDELIDAD DE DUBLIN CORE Y EXPEDIENTES COMPLETO
// ==========================================================================
function openArchivoModal(idxReal) {
  const doc = state.archivo.results.find(d => d.__idx == idxReal);
  if (!doc) return;
  
  document.getElementById("modal-doc-title").innerText = doc.titulo;
  document.getElementById("modal-doc-thumb-icon").className = "fas fa-file-alt";
  document.getElementById("modal-doc-thumb-badge").innerText = doc.doc_type;
  
  // Dublin Core qualified metadata keys display
  const metaContainer = document.getElementById("modal-doc-meta-container");
  metaContainer.innerHTML = `
    <div class="ds-doc-meta-row">
      <span class="k">dc.title</span>
      <span class="v">${doc.titulo}</span>
    </div>
    <div class="ds-doc-meta-row">
      <span class="k">dc.contributor.author</span>
      <span class="v">${doc.autor}</span>
    </div>
    <div class="ds-doc-meta-row">
      <span class="k">dc.date.issued</span>
      <span class="v">${formatISOToSpanish(doc.fecha)}</span>
    </div>
    <div class="ds-doc-meta-row">
      <span class="k">dc.type</span>
      <span class="v">${doc.doc_type}</span>
    </div>
    <div class="ds-doc-meta-row">
      <span class="k">dc.identifier.location</span>
      <span class="v">${doc.ubicacion}</span>
    </div>
    <div class="ds-doc-meta-row">
      <span class="k">dc.subject.classification</span>
      <span class="v">${doc.tesauro_badges.join("; ")}</span>
    </div>
  `;
  
  document.getElementById("modal-doc-abstract").innerText = doc.resumen || "No se ha registrado un resumen descriptivo abstracto (dc.description.abstract) para este folio.";
  
  // Configurar botones de acción
  const viewBtn = document.getElementById("btn-modal-view");
  viewBtn.onclick = () => alert(`Visualizando documento: ${doc.titulo}\nUbicado físicamente en: ${doc.ubicacion}`);
  
  const downloadBtn = document.getElementById("btn-modal-download");
  downloadBtn.classList.add("d-none");
  
  const editBtn = document.getElementById("btn-modal-edit");
  editBtn.classList.add("d-none");
  
  $("#doc-modal").modal("show");
}

async function openRrhhPersonDossier(personaRaw) {
  try {
    const res = await fetch(`${API_BASE}/api/rrhh/person/profile`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ persona: personaRaw })
    });
    
    if (!res.ok) throw new Error();
    const profile = await res.json();
    state.activePersonProfile = profile;
    
    // Limpiar filtros internos del dossier
    state.innerDossierSearch = "";
    state.innerDossierClass = "";
    state.innerDossierSort = "Alfabético (A-Z)";
    
    renderRrhhDossierModal();
    
    $("#rrhh-person-modal").modal("show");
    
  } catch (e) {
    console.error("Error al abrir perfil de RRHH:", e);
  }
}

function renderRrhhDossierModal() {
  const profile = state.activePersonProfile;
  if (!profile) return;

  const initials = getPersonInitials(profile.persona_raw);

  const photoHtml = profile.foto_url
    ? `<div class="rrhh-person-photo-card"><img src="${profile.foto_url}" class="rrhh-person-photo" alt="${profile.persona}"></div>`
    : `<div class="rrhh-person-photo-card rrhh-person-photo-fallback"><span class="rrhh-person-photo-initials">${initials}</span><i class="fas fa-user rrhh-person-photo-icon"></i></div>`;

  const isRetirado = (profile.statuses || "").includes("Retirado");
  const isPensionado = (profile.statuses || "").includes("Pensionado");
  const ciHtml = profile.cedulas
    ? `${profile.cedulas} <a href="#" class="btn btn-xs btn-outline-primary ml-2 py-0 px-2" onclick="alert('Abriendo visor Cédula de Identidad...');return false;"><i class="fas fa-id-card"></i> Ver</a>`
    : "N/A";

  const headerHtml = `
    <div class="ds-person-profile-header mb-4 p-3 bg-white rounded shadow-sm border">
      <div class="d-flex flex-column flex-md-row align-items-center align-items-md-start">
        <div class="ds-person-avatar-wrap mb-3 mb-md-0 mr-md-4" style="width:150px;min-width:150px;">
          ${photoHtml}
        </div>
        <div class="ds-person-info flex-grow-1 w-100">
          <div class="d-flex justify-content-between align-items-center border-bottom pb-2 mb-3">
            <h3 class="ds-person-name m-0 text-primary font-weight-bold">${profile.persona}</h3>
            <span class="badge badge-info text-uppercase px-3 py-2">${profile.statuses || "Sin estado"}</span>
          </div>
          <h5 class="ds-person-cargo text-secondary mb-3 font-weight-bold">
            <i class="fas fa-user-tie mr-2"></i>${profile.cargos || "Cargo no especificado"}
          </h5>
          <div class="row">
            <div class="col-6 mb-2"><strong>C.I.:</strong> ${ciHtml}</div>
            <div class="col-6 mb-2"><strong>RIF:</strong> ${profile.rifs || "N/A"}</div>
            <div class="col-6 mb-2"><strong>Adscripción:</strong> ${profile.departamentos || "N/A"}</div>
            <div class="col-6 mb-2"><strong>Ingreso:</strong> ${formatISOToSpanish(profile.fecha_ingreso) || "No registrada"}</div>
            ${isRetirado ? `<div class="col-6 mb-2"><strong>Jubilación:</strong> ${formatISOToSpanish(profile.fecha_jubilacion) || "No registrada"}</div>` : ""}
            ${isPensionado ? `<div class="col-6 mb-2"><strong>Pensión:</strong> ${formatISOToSpanish(profile.fecha_pension) || "No registrada"}</div>` : ""}
          </div>
        </div>
      </div>
    </div>
  `;

  const docTypes = profile.categories || [];
  const filtersHtml = `
    <div class="ds-modal-filters-wrap bg-light p-3 rounded border mb-4">
      <h6 class="font-weight-bold text-secondary text-uppercase mb-3">
        <i class="fas fa-sliders-h mr-2"></i>Explorar Documentos
      </h6>
      <div class="row">
        <div class="col-md-5 mb-2">
          <input type="text" id="inner-dossier-search" class="form-control form-control-sm" placeholder="Buscar palabras, ubicaciones, fechas..." oninput="state.innerDossierSearch = this.value; filterInnerDossier();">
        </div>
        <div class="col-md-4 mb-2">
          <select id="inner-dossier-class" class="form-control form-control-sm" onchange="state.innerDossierClass = this.value; filterInnerDossier();">
            <option value="">Todas las categorías</option>
            ${docTypes.map(c => `<option value="${c}">${c}</option>`).join("")}
          </select>
        </div>
        <div class="col-md-3 mb-2">
          <select id="inner-dossier-sort" class="form-control form-control-sm" onchange="state.innerDossierSort = this.value; filterInnerDossier();">
            <option value="Alfabético (A-Z)">Alfabético (A-Z)</option>
            <option value="Alfabético (Z-A)">Alfabético (Z-A)</option>
            <option value="Más recientes primero">Más recientes primero</option>
            <option value="Más antiguos primero">Más antiguos primero</option>
          </select>
        </div>
      </div>
      <div class="text-right mt-1">
        <span id="inner-dossier-folio-count" class="badge badge-pill badge-primary px-3 py-1">0 folios visibles</span>
      </div>
    </div>
  `;

  const target = document.getElementById("rrhh-person-modal-content");
  target.innerHTML = headerHtml + filtersHtml + `<div class="rrhh-person-files px-1" id="inner-dossier-items-container"></div>`;

  filterInnerDossier();
}

function filterInnerDossier() {
  const profile = state.activePersonProfile;
  if (!profile) return;

  let files = [...profile.rows];

  if (state.innerDossierSearch) {
    const q = state.innerDossierSearch.toLowerCase().trim();
    files = files.filter(f =>
      (f.doc_type || "").toLowerCase().includes(q) ||
      (f.ubicacion || "").toLowerCase().includes(q) ||
      (f.personas_relacionadas || "").toLowerCase().includes(q)
    );
  }

  if (state.innerDossierClass) {
    files = files.filter(f => f.doc_type === state.innerDossierClass);
  }

  if (state.innerDossierSort === "Alfabético (A-Z)") {
    files.sort((a, b) => (a.doc_type || "").localeCompare(b.doc_type || ""));
  } else if (state.innerDossierSort === "Alfabético (Z-A)") {
    files.sort((a, b) => (b.doc_type || "").localeCompare(a.doc_type || ""));
  } else if (state.innerDossierSort === "Más recientes primero") {
    files.sort((a, b) => (b.fecha_ingreso || "").localeCompare(a.fecha_ingreso || ""));
  } else if (state.innerDossierSort === "Más antiguos primero") {
    files.sort((a, b) => (a.fecha_ingreso || "").localeCompare(b.fecha_ingreso || ""));
  }

  const countBadge = document.getElementById("inner-dossier-folio-count");
  if (countBadge) countBadge.textContent = `${files.length} folios visibles`;

  const container = document.getElementById("inner-dossier-items-container");
  if (files.length === 0) {
    container.innerHTML = `<div class="alert alert-secondary text-center p-3">No se encontraron archivos con estos filtros en el expediente.</div>`;
    return;
  }

  // Group by doc_type category
  const grouped = {};
  for (const f of files) {
    const cat = f.doc_type || "Sin categoría";
    if (!grouped[cat]) grouped[cat] = [];
    grouped[cat].push(f);
  }

  container.innerHTML = Object.entries(grouped).map(([cat, catFiles]) => `
    <div class="rrhh-person-category-section mb-4">
      <h5 class="border-bottom pb-2 mb-3 ds-category-title">
        <i class="fas fa-folder-open mr-2"></i>${cat}
      </h5>
      ${catFiles.map(f => `
        <div class="rrhh-person-file-item">
          <div class="rrhh-person-file-head">
            <div class="rrhh-person-file-main">
              <strong>${f.doc_type}</strong>
              <span class="rrhh-person-file-sub">Fecha de ingreso: ${formatISOToSpanish(f.fecha_ingreso)}</span>
            </div>
            <a href="#" class="btn btn-sm btn-outline-info" onclick="openDocMetadataModal('${f.__idx}'); return false;">Abrir archivo</a>
          </div>
          <div class="rrhh-person-file-meta">
            <span>Dependencia o AP: <strong>${f.departamento || "N/A"}</strong></span>
            <span>Estatus: <strong>${f.estatus || f.estado || "N/A"}</strong></span>
            <span>Ubicación: <strong>${f.ubicacion || "N/A"}</strong></span>
          </div>
        </div>
      `).join("")}
    </div>
  `).join("");
}

function openDocMetadataModal(idxReal) {
  // Buscar en todos los folios cargados en el expediente de la persona activa
  if (!state.activePersonProfile || !state.activePersonProfile.files) return;
  const doc = state.activePersonProfile.files.find(d => d.__idx == idxReal);
  if (!doc) return;
  
  document.getElementById("modal-doc-title").innerText = `${doc.doc_type} - ${doc.empleado}`;
  document.getElementById("modal-doc-thumb-icon").className = "fas fa-id-card";
  document.getElementById("modal-doc-thumb-badge").innerText = doc.doc_type;
  
  const metaContainer = document.getElementById("modal-doc-meta-container");
  metaContainer.innerHTML = `
    <div class="ds-doc-meta-row">
      <span class="k">Titular</span>
      <span class="v">${doc.empleado}</span>
    </div>
    <div class="ds-doc-meta-row">
      <span class="k">Cédula</span>
      <span class="v">${doc.cedula}</span>
    </div>
    <div class="ds-doc-meta-row">
      <span class="k">Tipo de Archivo</span>
      <span class="v">${doc.doc_type}</span>
    </div>
    <div class="ds-doc-meta-row">
      <span class="k">Ubicación Física</span>
      <span class="v">${doc.ubicacion}</span>
    </div>
    <div class="ds-doc-meta-row">
      <span class="k">Fecha de Ingreso</span>
      <span class="v">${formatISOToSpanish(doc.fecha_ingreso)}</span>
    </div>
    <div class="ds-doc-meta-row">
      <span class="k">Vínculos Relacionados</span>
      <span class="v">${doc.personas_relacionadas}</span>
    </div>
  `;
  
  document.getElementById("modal-doc-abstract").innerText = `Expediente Laboral Digitalizado del empleado ${doc.empleado}. Clasificado en el departamento de ${doc.departamento} con el estado de personal ${doc.estado}.`;
  
  const viewBtn = document.getElementById("btn-modal-view");
  viewBtn.onclick = () => alert(`Abriendo archivo digitalizado de: ${doc.doc_type}\nUbicado en: ${doc.ubicacion}`);
  
  $("#doc-modal").modal("show");
}

// ==========================================================================
// PANEL DE CONTROL: ESTADÍSTICAS ANALÍTICAS Y CONTROL ADMINISTRATIVO
// ==========================================================================
function loadAdminTab(adminTabId) {
  state.activeAdminTab = adminTabId;
  
  // Determinar sufijo y root según panel activo
  const suf = adminSuffixFromTab();
  const adminTabsRoot = `#admin_workspace_tabs-${suf}`;
  const adminSectionRoot = `#tab-admin-${suf}`;

  // Clases active (tabs)
  document.querySelectorAll(`${adminTabsRoot} .nav-link`).forEach(l => l.classList.remove("active"));
  const tabEl = document.getElementById(`tab-admin-${suf}-${adminTabId}`);
  if (tabEl) tabEl.classList.add("active");

  // Panes
  document.querySelectorAll(`${adminSectionRoot} .tab-pane`).forEach(p => p.classList.remove("show", "active"));
  const paneEl = document.getElementById(`pane-admin-${suf}-${adminTabId}`);
  if (paneEl) paneEl.classList.add("show", "active");
  
  if (adminTabId === "stats") {
    loadDynamicStats();
  } else if (adminTabId === "new") {
    renderDynamicSubmitFields();
    loadRecentSubmissions();
  } else if (adminTabId === "monitor") {
    state.adminTable.page = 1;
    loadMonitorTable();
  } else if (adminTabId === "categories") {
    loadCategoriesTab();
  } else if (adminTabId === "users") {
    loadUsersTab();
  }

  // Ajustes de texto y etiquetas según el módulo activo (Archivo vs RRHH)
  try {
    const mod = state.user && state.user.modulo ? state.user.modulo : "Archivo";
    // Breadcrumb dentro del admin
    const bc = document.querySelector(`${adminSectionRoot} .ds-breadcrumb`);
    if (bc) bc.innerHTML = `<i class="fas fa-shield-alt"></i> Panel de Control / Administración - ${mod}`;

    // Botón de submit del formulario de nuevo ingreso
    const submitBtn = document.getElementById(`btn_submit_workspace-${suf}`);
    if (submitBtn) {
      if (mod === "RRHH") {
        submitBtn.innerHTML = `<i class="fas fa-cloud-upload-alt"></i> Guardar en RRHH`;
      } else {
        submitBtn.innerHTML = `<i class="fas fa-cloud-upload-alt"></i> Guardar en Archivo`;
      }
    }

    // Título del monitor
    const monitorTitle = document.querySelector(`${adminSectionRoot} .card-title`);
    if (monitorTitle) {
      if (mod === "RRHH") monitorTitle.innerHTML = `<i class=\"fas fa-database\"></i> Monitor de RRHH`;
      else monitorTitle.innerHTML = `<i class=\"fas fa-database\"></i> Monitor de Archivos`;
    }
  } catch (e) {
    console.error("Error updating admin panel labels:", e);
  }
}

async function loadDynamicStats() {
  const suf = adminSuffixFromTab();
  const reqPayload = {
    modulo: state.user.modulo,
    date_start: (document.getElementById(`stats-date-start-${suf}`) && document.getElementById(`stats-date-start-${suf}`).value) || "",
    date_end: (document.getElementById(`stats-date-end-${suf}`) && document.getElementById(`stats-date-end-${suf}`).value) || ""
  };
  
  try {
    const res = await fetch(`${API_BASE}/api/admin/stats`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(reqPayload)
    });
    if (!res.ok) throw new Error();
    const stats = await res.json();
    
    // 1. KPIs
    const kpiDocs = document.getElementById(`kpi-total-docs-${suf}`);
    const kpiCats = document.getElementById(`kpi-total-cats-${suf}`);
    if (kpiDocs) kpiDocs.innerText = stats.total_docs;
    if (kpiCats) kpiCats.innerText = stats.categories_count;
    
    // Usuarios mockeados
      const resUsers = await fetch(`${API_BASE}/api/admin/users`);
      if (resUsers.ok) {
        const uList = await resUsers.json();
        const kpiUsers = document.getElementById(`kpi-total-users-${suf}`);
        if (kpiUsers) kpiUsers.innerText = uList.filter(u => u.modulo === state.user.modulo).length;
      }
    
    // Último ingreso
    const isArchivo = state.user.modulo === "Archivo";
    const db_list = isArchivo ? state.archivo.results : state.rrhh.results;
      if (db_list.length > 0) {
      const dates = db_list.map(r => isArchivo ? r.fecha : r.fecha_ingreso).filter(Boolean).sort().reverse();
      const kpiLatest = document.getElementById(`kpi-latest-entry-${suf}`);
      if (kpiLatest) kpiLatest.innerText = formatISOToSpanish(dates[0]);
    } else {
      const kpiLatest = document.getElementById(`kpi-latest-entry-${suf}`);
      if (kpiLatest) kpiLatest.innerText = "N/A";
    }
    
    // 2. Gráfico Distribución por Tipo
    const typeContainer = document.getElementById(`stats_by_type-${suf}`);
    if (stats.by_type.length === 0) {
      typeContainer.innerHTML = `<div class="text-muted">Sin datos.</div>`;
    } else {
      const colors = ["#2b4e72", "#0056b3", "#28a745", "#ffc107", "#dc3545", "#6f42c1"];
      typeContainer.innerHTML = `
        <div class="w-100" style="padding:10px;">
          ${stats.by_type.map((t, i) => {
            const color = colors[i % colors.length];
            return `
              <div class="mb-3">
                <div class="d-flex justify-content-between" style="font-size:0.85rem; margin-bottom: 2px;">
                  <span class="font-weight-bold text-dark">${t.type}</span>
                  <span class="text-primary">${t.count} (${t.pct}%)</span>
                </div>
                <div class="progress" style="height:10px; border-radius:5px; background-color:#e9ecef;">
                  <div class="progress-bar" style="width:${t.pct}%; background-color:${color}; border-radius:5px;"></div>
                </div>
              </div>
            `;
          }).join("")}
        </div>
      `;
    }
    
    // 3. Gráfico Histograma Anual
    const timelineContainer = document.getElementById(`stats_timeline-${suf}`);
    if (stats.timeline.length === 0) {
      timelineContainer.innerHTML = `<div class="text-muted">Sin datos de línea de tiempo.</div>`;
    } else {
      timelineContainer.innerHTML = `
        <div class="w-100" style="padding:15px 10px;">
          ${stats.timeline.map(y => `
            <div class="d-flex align-items-center mb-3">
              <div style="width:60px; text-align:right; margin-right:15px;">
                <span class="font-weight-bold text-primary" style="font-size:0.95rem;">${y.year}</span>
              </div>
              <div style="flex-grow:1;">
                <div class="progress" style="height:22px; border-radius:11px; background-color:#e9ecef;">
                  <div class="progress-bar d-flex align-items-center justify-content-center" style="width:${y.pct_width}%; background: linear-gradient(135deg, #2b4e72, #0056b3); border-radius:11px; font-size:0.78rem; font-weight:bold; color:white; line-height:22px;">
                    ${y.count} docs
                  </div>
                </div>
              </div>
            </div>
          `).join("")}
        </div>
      `;
    }
    
    // 4. Estado del Sistema
    const sysContainer = document.getElementById(`stats_system-${suf}`);
    sysContainer.innerHTML = `
      <div class="col-md-3 text-center p-3 border-right">
        <i class="fas fa-server fa-2x text-primary mb-2"></i>
        <h5 class="font-weight-bold m-0">${stats.system.status}</h5>
        <span class="text-muted" style="font-size:0.8rem;">Estado Global</span>
      </div>
      <div class="col-md-3 text-center p-3 border-right">
        <i class="fas fa-memory fa-2x text-success mb-2"></i>
        <h5 class="font-weight-bold m-0">${stats.system.ram}</h5>
        <span class="text-muted" style="font-size:0.8rem;">Consumo RAM</span>
      </div>
      <div class="col-md-3 text-center p-3 border-right">
        <i class="fas fa-microchip fa-2x text-warning mb-2"></i>
        <h5 class="font-weight-bold m-0">${stats.system.cpu} Núcleos</h5>
        <span class="text-muted" style="font-size:0.8rem;">Procesamiento</span>
      </div>
      <div class="col-md-3 text-center p-3">
        <i class="fas fa-laptop-code fa-2x text-danger mb-2"></i>
        <h5 class="font-weight-bold m-0" style="font-size: 0.95rem; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;">${stats.system.os}</h5>
        <span class="text-muted" style="font-size:0.8rem;">Plataforma</span>
      </div>
    `;
    
  } catch (e) {
    console.error("Error al cargar analíticas dinámicas:", e);
  }
}

function renderDynamicSubmitFields() {
  const suf = adminSuffixFromTab();
  const container = document.getElementById(`dynamic-submit-fields-${suf}`);
  const isArchivo = state.user.modulo === "Archivo";
  
  if (isArchivo) {
    container.innerHTML = `
      <div class="row">
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">dc.title (Título) *</label>
          <input type="text" id="reg-title-${suf}" class="form-control" placeholder="Ej: Proyecto SVD Compresión" required>
        </div>
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">dc.contributor.author (Autor) *</label>
          <input type="text" id="reg-author-${suf}" class="form-control" placeholder="Ej: Dr. Juan Pérez" required>
        </div>
      </div>
      <div class="row mt-2">
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">dc.type (Tipología) *</label>
          <select id="reg-doc-type-${suf}" class="form-control" required>
            ${(state.choices?.archivo?.doc_types || ["Proyecto de Investigación", "Informe"]).map(t => `<option value="${t}">${t}</option>`).join("")}
          </select>
        </div>
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">dc.date.issued (Fecha Emisión) *</label>
          <input type="date" id="reg-fecha-${suf}" class="form-control" required value="${new Date().toISOString().substring(0, 10)}">
        </div>
      </div>
      <div class="form-group mt-2">
        <label class="font-weight-bold text-muted">dc.description.abstract (Resumen)</label>
          <textarea id="reg-resumen-${suf}" class="form-control" rows="3" placeholder="Síntesis del folio..."></textarea>
      </div>
      <div class="form-group mt-2">
        <label class="font-weight-bold text-muted">dc.identifier.location (Ubicación Topográfica) *</label>
          <input type="text" id="reg-location-${suf}" class="form-control" placeholder="Ej: Estante B2, Gaveta 3" required>
      </div>
    `;
  } else {
    container.innerHTML = `
      <div class="row">
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">Nombre Completo del Personal *</label>
          <input type="text" id="reg-empleado-${suf}" class="form-control" placeholder="Ej: Susana Pérez" required>
        </div>
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">Cédula de Identidad *</label>
          <input type="text" id="reg-cedula-${suf}" class="form-control" placeholder="Ej: V-12345678" required>
        </div>
      </div>
      <div class="row mt-2">
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">C.I.F. (RIF)</label>
          <input type="text" id="reg-rif-${suf}" class="form-control" placeholder="Ej: J-12345678-0">
        </div>
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">Cargo Asignado</label>
          <input type="text" id="reg-cargo-${suf}" class="form-control" placeholder="Ej: Analista Contable">
        </div>
      </div>
      <div class="form-group mt-2">
        <label class="font-weight-bold text-muted">Personas / Dependencias Relacionadas</label>
        <input type="text" id="reg-personas" class="form-control" placeholder="Ej: Susana Pérez; Dirección RRHH">
      </div>
      <div class="row mt-2">
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Clasificación de Archivo *</label>
          <select id="reg-doc-type-${suf}" class="form-control" required>
            ${(state.choices?.rrhh?.doc_types || ["Hoja de Vida", "Contrato"]).map(t => `<option value="${t}">${t}</option>`).join("")}
          </select>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Departamento *</label>
          <input type="text" id="reg-depto-${suf}" class="form-control" placeholder="Ej: Biología" required>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Estado *</label>
          <select id="reg-estado-${suf}" class="form-control" required>
            <option value="Activo">Activo</option>
            <option value="Retirado">Retirado</option>
            <option value="Retirado">Retirado</option>
            <option value="Pensionado">Pensionado</option>
          </select>
        </div>
      </div>
      <div class="row mt-2">
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">Fecha de Ingreso *</label>
          <input type="date" id="reg-fecha-${suf}" class="form-control" required value="${new Date().toISOString().substring(0, 10)}">
        </div>
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted">Retención Física *</label>
          <input type="text" id="reg-location-${suf}" class="form-control" placeholder="Ej: Archivo Central - Caja J-02" required>
        </div>
      </div>
      <div class="row mt-2">
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Fecha de Jubilación (Opcional)</label>
          <input type="date" id="reg-jubilacion-${suf}" class="form-control">
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">Fecha de Pensión (Opcional)</label>
          <input type="date" id="reg-pension-${suf}" class="form-control">
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted">URL Foto Avatar (Opcional)</label>
          <input type="text" id="reg-foto-${suf}" class="form-control" placeholder="https://...">
        </div>
      </div>
    `;
  }
}

function loadRecentSubmissions() {
  const isArchivo = state.user.modulo === "Archivo";
  const list = isArchivo ? state.archivo.results : state.rrhh.results;
  
  const container = document.getElementById("recent_submissions");
  const suf = adminSuffixFromTab();
  const containerEl = document.getElementById(`recent_submissions-${suf}`);
  if (!containerEl) return;
  if (list.length === 0) {
    containerEl.innerHTML = `<li class="text-muted text-center p-2">Sin ingresos previos.</li>`;
    return;
  }
  
  const sorted = [...list].sort((a, b) => {
    const fA = isArchivo ? a.fecha : a.fecha_ingreso;
    const fB = isArchivo ? b.fecha : b.fecha_ingreso;
    return fB.localeCompare(fA);
  }).slice(0, 3);
  
  containerEl.innerHTML = sorted.map(item => {
    const title = isArchivo ? item.titulo : item.empleado;
    return `
      <li class="d-flex align-items-center mb-2 p-2 border rounded bg-light">
        <i class="fas fa-file-alt mr-2 text-primary" style="font-size:1.15rem;"></i>
        <div style="overflow:hidden; text-overflow:ellipsis; white-space:nowrap; flex-grow:1;">
          <strong style="font-size:0.82rem;" class="text-dark">${title}</strong><br>
          <span class="text-muted" style="font-size:0.72rem;">${item.doc_type}</span>
        </div>
      </li>
    `;
  }).join("");
}

async function handleNewSubmission(e) {
  e.preventDefault();
  const isArchivo = state.user.modulo === "Archivo";
  const suf = adminSuffixFromTab();
  const payload = {
    modulo: state.user.modulo,
    usuario: state.user.username,
    doc_type: (document.getElementById(`reg-doc-type-${suf}`) && document.getElementById(`reg-doc-type-${suf}`).value) || "",
    fecha: (document.getElementById(`reg-fecha-${suf}`) && document.getElementById(`reg-fecha-${suf}`).value) || "",
    ubicacion: (document.getElementById(`reg-location-${suf}`) && document.getElementById(`reg-location-${suf}`).value) || ""
  };
  
    if (isArchivo) {
    payload.titulo = (document.getElementById(`reg-title-${suf}`) && document.getElementById(`reg-title-${suf}`).value) || "";
    payload.autor = (document.getElementById(`reg-author-${suf}`) && document.getElementById(`reg-author-${suf}`).value) || "";
    payload.resumen = (document.getElementById(`reg-resumen-${suf}`) && document.getElementById(`reg-resumen-${suf}`).value) || "";
  } else {
    payload.empleado = (document.getElementById(`reg-empleado-${suf}`) && document.getElementById(`reg-empleado-${suf}`).value) || "";
    payload.cedula = (document.getElementById(`reg-cedula-${suf}`) && document.getElementById(`reg-cedula-${suf}`).value) || "";
    payload.personas_relacionadas = (document.getElementById(`reg-personas-${suf}`) && document.getElementById(`reg-personas-${suf}`).value) || "";
    payload.departamento = (document.getElementById(`reg-depto-${suf}`) && document.getElementById(`reg-depto-${suf}`).value) || "";
    payload.estado = (document.getElementById(`reg-estado-${suf}`) && document.getElementById(`reg-estado-${suf}`).value) || "";
    payload.rif = (document.getElementById(`reg-rif-${suf}`) && document.getElementById(`reg-rif-${suf}`).value) || "";
    payload.cargo = (document.getElementById(`reg-cargo-${suf}`) && document.getElementById(`reg-cargo-${suf}`).value) || "";
    payload.fecha_jubilacion = (document.getElementById(`reg-jubilacion-${suf}`) && document.getElementById(`reg-jubilacion-${suf}`).value) || "";
    payload.fecha_pension = (document.getElementById(`reg-pension-${suf}`) && document.getElementById(`reg-pension-${suf}`).value) || "";
    payload.foto_url = (document.getElementById(`reg-foto-${suf}`) && document.getElementById(`reg-foto-${suf}`).value) || "";
  }
  
  try {
    const res = await fetch(`${API_BASE}/api/admin/submit`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    });
    
    if (!res.ok) throw new Error();
    
    alert("¡Expediente de Dublin Core indexado y guardado en base de datos local con éxito!");
    
    // RecargarChoices y búsquedas correspondientes
    loadDynamicChoices();
    if (isArchivo) triggerArchivoSearch();
    else triggerRrhhSearch();
    
    loadAdminTab("stats"); // Redirigir a estadísticas
    
  } catch (err) {
    alert("Error al intentar registrar el nuevo folio.");
  }
}

// 3. MONITOR DE ARCHIVOS (TABLA CRUD LOCAL)
async function loadMonitorTable() {
  const mod = state.user.modulo;
  const suf = adminSuffixFromTab();
  const qEl = document.getElementById(`admin_search-${suf}`);
  const typeEl = document.getElementById(`admin_filter_type-${suf}`);
  const q = (qEl && qEl.value) || "";
  const typeF = (typeEl && typeEl.value) || "";
  
  try {
    const res = await fetch(`${API_BASE}/api/admin/list_all?modulo=${mod}&search=${q}&type_filter=${typeF}`);
    if (!res.ok) throw new Error();
    const data = await res.json();
    state.adminTable.results = data;
    
    // Llenar selector de tipologías en el monitor
    const typeSelector = document.getElementById(`admin_filter_type-${suf}`);
    if (typeSelector && typeSelector.options.length <= 1 && state.choices) {
      const types = isArchivoModule() ? state.choices.archivo.doc_types : state.choices.rrhh.doc_types;
      typeSelector.innerHTML = `<option value="">Filtrar por Tipología...</option>` +
        types.map(t => `<option value="${t}">${t}</option>`).join("");
    }
    
    renderMonitorTable();
    
  } catch (e) {
    console.error("Error al cargar monitor local:", e);
  }
}

function isArchivoModule() {
  return state.user.modulo === "Archivo";
}

function renderMonitorTable() {
  const suf = adminSuffixFromTab();
  const container = document.getElementById(`admin_control_table-${suf}`);
  const results = state.adminTable.results;
  const isArch = isArchivoModule();
  
  const summaryEl = document.getElementById(`admin_table_summary-${suf}`);
  if (summaryEl) summaryEl.innerText = `Total: ${results.length} registros en el módulo ${state.user.modulo}`;
  
  if (results.length === 0) {
    container.innerHTML = `<tr><td colspan="${isArch ? 6 : 7}" class="text-muted text-center p-3">Ningún archivo coincide con los criterios de búsqueda local.</td></tr>`;
    const pageInfo = document.getElementById(`admin_page_info-${suf}`);
    if (pageInfo) pageInfo.innerText = "Pág 1 de 1";
    return;
  }
  
  const totalPages = Math.ceil(results.length / state.adminTable.perPage);
  if (state.adminTable.page > totalPages) state.adminTable.page = totalPages;
  
  const start = (state.adminTable.page - 1) * state.adminTable.perPage;
  const pageItems = results.slice(start, start + state.adminTable.perPage);
  
  const pageInfo = document.getElementById(`admin_page_info-${suf}`);
  if (pageInfo) pageInfo.innerText = `Pág ${state.adminTable.page} de ${totalPages}`;
  
  if (isArch) {
    container.innerHTML = pageItems.map(f => `
      <tr>
        <td class="font-weight-bold text-dark">${f.titulo}</td>
        <td>${f.autor}</td>
        <td>${formatISOToSpanish(f.fecha)}</td>
        <td><span class="ds-badge">${f.doc_type}</span></td>
        <td class="text-muted" style="font-size:0.82rem;">${f.ubicacion}</td>
        <td>
          <button class="btn btn-sm btn-outline-secondary" onclick="openArchivoModal('${f.__idx}')" title="Ver Folio"><i class="fas fa-eye"></i></button>
        </td>
      </tr>
    `).join("");
  } else {
    container.innerHTML = pageItems.map(f => {
      const colorState = getStatusColor(f.estado);
      return `
        <tr>
          <td class="font-weight-bold text-dark">${f.empleado}</td>
          <td>${f.cedula}</td>
          <td style="font-size:0.82rem; color:#6c757d;">${f.personas_relacionadas}</td>
          <td>${f.departamento}</td>
          <td><span class="badge" style="background-color: ${colorState}; color: white; padding:3px 6px;">${f.estado}</span></td>
          <td><span class="badge badge-secondary" style="padding:3px 6px;">${f.doc_type}</span></td>
          <td>
            <button class="btn btn-sm btn-outline-secondary" onclick="openDocMetadataModal('${f.__idx}')" title="Ver Folio"><i class="fas fa-eye"></i></button>
          </td>
        </tr>
      `;
    }).join("");
  }
}

// 4. CATEGORÍAS TAB
function loadCategoriesTab() {
  const suf = adminSuffixFromTab();
  const container = document.getElementById(`admin_tax_list-${suf}`);
  if (!state.choices) {
    if (container) container.innerHTML = `<div class="text-muted p-2">Sin tipologías activas.</div>`;
    return;
  }

  const types = isArchivoModule() ? state.choices.archivo.doc_types : state.choices.rrhh.doc_types;
  if (container) container.innerHTML = types.map(t => `
    <div class="list-group-item d-flex justify-content-between align-items-center mb-1 rounded bg-white shadow-sm border-left" style="border-left: 4px solid #ffc107 !important;">
      <div>
        <h6 class="font-weight-bold text-dark mb-0">${t}</h6>
        <small class="text-muted">Tipología de alcance: ${state.user.modulo}</small>
      </div>
      <span class="badge badge-warning badge-pill">Activa</span>
    </div>
  `).join("");
}

async function handleAddCategory() {
  const suf = adminSuffixFromTab();
  const nameEl = document.getElementById(`new_tax_name-${suf}`);
  const descEl = document.getElementById(`new_tax_desc-${suf}`);
  const scopeEl = document.getElementById(`new_tax_scope-${suf}`);
  const name = nameEl ? nameEl.value.trim() : "";
  const desc = descEl ? descEl.value.trim() : "";
  const scope = scopeEl ? scopeEl.value : "";
  
  if (!name) {
    alert("Por favor, ingrese el nombre de la tipología.");
    return;
  }
  
  try {
    const res = await fetch(`${API_BASE}/api/admin/add_category`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ name, desc, scope, usuario: state.user.username })
    });
    if (!res.ok) throw new Error();
    
    alert("¡Nueva taxonomía tipológica guardada con éxito!");
    if (nameEl) nameEl.value = "";
    if (descEl) descEl.value = "";
    
    loadDynamicChoices();
    loadAdminTab("categories");
    
  } catch (err) {
    alert("Error al guardar categoría.");
  }
}

// 5. CONTROL DE USUARIOS
async function loadUsersTab() {
  const suf = adminSuffixFromTab();
  const container = document.getElementById(`admin_users_table-${suf}`);
  
  try {
    const res = await fetch(`${API_BASE}/api/admin/users`);
    if (!res.ok) throw new Error();
    const users = await res.json();
    
    container.innerHTML = `
      <table class="table table-striped table-bordered" style="font-size:0.85rem;">
        <thead>
          <tr class="bg-light text-dark">
            <th>Usuario</th>
            <th>Contraseña</th>
            <th>Módulo de Trabajo</th>
            <th>Rol</th>
            <th>Estado</th>
          </tr>
        </thead>
        <tbody>
          ${users.map(u => `
            <tr>
              <td class="font-weight-bold text-dark"><i class="fas fa-user-circle mr-1 text-secondary"></i> ${u.usuario}</td>
              <td class="text-muted">${u.password}</td>
              <td>${u.modulo}</td>
              <td><span class="badge ${u.rol === 'Admin' ? 'badge-danger' : 'badge-primary'}">${u.rol}</span></td>
              <td><span class="badge badge-success"><i class="fas fa-check-circle mr-1"></i> Autorizado</span></td>
            </tr>
          `).join("")}
        </tbody>
      </table>
    `;
    
  } catch (e) {
    container.innerHTML = `<div class="alert alert-danger">Error al cargar listado de seguridad.</div>`;
  }
}

async function handleAddUser() {
  const suf = adminSuffixFromTab();
  const usernameEl = document.getElementById(`new_user_name-${suf}`);
  const passEl = document.getElementById(`new_user_pass-${suf}`);
  const moduloEl = document.getElementById(`new_user_modulo-${suf}`);
  const rolEl = document.getElementById(`new_user_rol-${suf}`);
  const username = usernameEl ? usernameEl.value.trim() : "";
  const pass = passEl ? passEl.value.trim() : "";
  const modulo = moduloEl ? moduloEl.value : "";
  const rol = rolEl ? rolEl.value : "";
  
  if (!username || !pass) {
    alert("Por favor, ingrese todos los datos requeridos para registrar el usuario.");
    return;
  }
  
  try {
    const res = await fetch(`${API_BASE}/api/admin/users/create`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        usuario: username,
        password: pass,
        modulo,
        rol,
        creator: state.user.username
      })
    });
    
    if (!res.ok) {
      const err = await res.json();
      throw new Error(err.detail || "Error");
    }
    
    alert(`¡Usuario ${username} registrado en base de datos de control de acceso!`);
    if (usernameEl) usernameEl.value = "";
    if (passEl) passEl.value = "";
    
    loadUsersTab();
    
  } catch (err) {
    alert(err.message || "Error al registrar el nuevo usuario.");
  }
}

// ==========================================================================
// CONTROLADORES DE EVENTOS Y BINDINGS DE LA SPA
// ==========================================================================
function setupEventListeners() {
  
  function safeOn(id, ev, fn) { const el = document.getElementById(id); if (el) el.addEventListener(ev, fn); }

  // Login
  safeOn("login_btn", "click", performLogin);
  safeOn("login_user", "keydown", (e) => { if (e.key === "Enter") performLogin(); });
  safeOn("login_pass", "keydown", (e) => { if (e.key === "Enter") performLogin(); });
  if (document.body.dataset.page === "login") {
    document.addEventListener("keydown", (e) => { if (e.key === "Enter") performLogin(); });
  }
  safeOn("toggle_login_pass", "click", () => {
    const input = document.getElementById("login_pass");
    const icon = document.querySelector("#toggle_login_pass i");
    if (!input || !icon) return;
    if (input.type === "password") { input.type = "text"; icon.className = "fas fa-eye"; }
    else { input.type = "password"; icon.className = "fas fa-eye-slash"; }
  });

  // Logout
  safeOn("logout_btn", "click", logout);

  // Sidebar Toggle
  safeOn("sidebar-toggle-btn", "click", openSidebar);
  safeOn("sidebar-close-btn", "click", closeSidebar);
  safeOn("sidebar-overlay", "click", closeSidebar);

  // Tabs Sidebar (only intercept in SPA mode; on standalone pages let the <a href> navigate)
  const isSPA = !!document.getElementById("app-portal");
  safeOn("menu-btn-archivo", "click", (e) => { if (!isSPA) return; e.preventDefault(); switchTab("archivo"); });
  safeOn("menu-btn-rrhh", "click", (e) => { if (!isSPA) return; e.preventDefault(); switchTab("rrhh"); });
  safeOn("menu-btn-admin-archivo", "click", (e) => { if (!isSPA) return; e.preventDefault(); if (state.user) state.user.modulo = "Archivo"; switchTab("admin-archivo"); });
  safeOn("menu-btn-admin-rrhh", "click", (e) => { if (!isSPA) return; e.preventDefault(); if (state.user) state.user.modulo = "RRHH"; switchTab("admin-rrhh"); });
  safeOn("module_switch_btn", "click", handleModuleSwitch);

  // Buscador de Archivo
  safeOn("search_archivo", "input", (e) => { state.archivo.search = e.target.value; state.archivo.page = 1; triggerArchivoSearch(); });
  safeOn("btn_s_archivo", "click", triggerArchivoSearch);
  safeOn("btn_clear_archivo", "click", () => resetDateFilters("archivo"));
  safeOn("download_archivo_xls", "click", () => { alert("Generando y exportando reporte XLS de folios académicos DSpace...\nDescargado con éxito."); });

  // Buscador de RRHH
  safeOn("search_rrhh", "input", (e) => { state.rrhh.search = e.target.value; state.rrhh.page = 1; triggerRrhhSearch(); });
  safeOn("btn_s_rrhh", "click", triggerRrhhSearch);
  safeOn("btn_clear_rrhh", "click", () => resetDateFilters("rrhh"));
  safeOn("download_rrhh_xls", "click", () => { alert("Generando y exportando reporte consolidado de personal de RRHH...\nDescargado con éxito."); });

  // Ordenación y RPP - Archivo
  safeOn("sort_archivo", "change", (e) => { state.archivo.sortMode = e.value || e.target.value; state.archivo.page = 1; triggerArchivoSearch(); });
  safeOn("rpp_archivo", "change", (e) => { state.archivo.perPage = parseInt(e.target.value); state.archivo.page = 1; renderArchivoList(); });

  // Ordenación y RPP - RRHH
  safeOn("sort_rrhh", "change", (e) => { state.rrhh.sortMode = e.value || e.target.value; state.rrhh.page = 1; triggerRrhhSearch(); });
  safeOn("rpp_rrhh", "change", (e) => { state.rrhh.perPage = parseInt(e.target.value); state.rrhh.page = 1; renderRrhhList(); });

  // Fechas Bidireccionales - Archivo
  safeOn("slider-archivo-start", "input", () => handleDateChange("archivo", "slider"));
  safeOn("slider-archivo-end", "input", () => handleDateChange("archivo", "slider"));
  safeOn("select-archivo-year", "change", () => handleDateChange("archivo", "select"));
  safeOn("date-archivo-start", "change", () => handleDateChange("archivo", "calendar"));
  safeOn("date-archivo-end", "change", () => handleDateChange("archivo", "calendar"));

  // Fechas Bidireccionales - RRHH
  safeOn("slider-rrhh-start", "input", () => handleDateChange("rrhh", "slider"));
  safeOn("slider-rrhh-end", "input", () => handleDateChange("rrhh", "slider"));
  safeOn("select-rrhh-year", "change", () => handleDateChange("rrhh", "select"));
  safeOn("date-rrhh-start", "change", () => handleDateChange("rrhh", "calendar"));
  safeOn("date-rrhh-end", "change", () => handleDateChange("rrhh", "calendar"));

  // Paginación - Archivo
  safeOn("btn-archivo-prev", "click", () => { if (state.archivo.page > 1) { state.archivo.page--; renderArchivoList(); } });
  safeOn("btn-archivo-next", "click", () => { const t = Math.ceil(state.archivo.results.length / state.archivo.perPage); if (state.archivo.page < t) { state.archivo.page++; renderArchivoList(); } });

  // Paginación - RRHH
  safeOn("btn-rrhh-prev", "click", () => { if (state.rrhh.page > 1) { state.rrhh.page--; renderRrhhList(); } });
  safeOn("btn-rrhh-next", "click", () => { const t = Math.ceil(state.rrhh.results.length / state.rrhh.perPage); if (state.rrhh.page < t) { state.rrhh.page++; renderRrhhList(); } });

  // --- EVENTOS PESTAÑAS ADMIN ---
  // --- EVENTOS PESTAÑAS ADMIN (aplicar a ambos namespaces) ---
  ["archivo", "rrhh"].forEach(suf => {
    // Tabs
    ["stats", "new", "monitor", "categories", "users"].forEach(t => {
      const tabId = `tab-admin-${suf}-${t}`;
      const tabEl = document.getElementById(tabId);
      if (tabEl) tabEl.addEventListener("click", (e) => { e.preventDefault(); loadAdminTab(t); });
    });

    // Botón aplicar filtros analíticos
    const applyBtn = document.getElementById(`btn-apply-stats-${suf}`);
    if (applyBtn) applyBtn.addEventListener("click", loadDynamicStats);

    // Formulario nuevo ingreso
    const submitForm = document.getElementById(`admin-submit-form-${suf}`);
    if (submitForm) submitForm.addEventListener("submit", handleNewSubmission);

    // Monitor buscador local
    const searchEl = document.getElementById(`admin_search-${suf}`);
    if (searchEl) searchEl.addEventListener("input", () => { state.adminTable.page = 1; loadMonitorTable(); });
    const filterEl = document.getElementById(`admin_filter_type-${suf}`);
    if (filterEl) filterEl.addEventListener("change", () => { state.adminTable.page = 1; loadMonitorTable(); });
    const refreshBtn = document.getElementById(`btn_refresh_table-${suf}`);
    if (refreshBtn) refreshBtn.addEventListener("click", loadMonitorTable);
    const exportBtn = document.getElementById(`btn_export_csv-${suf}`);
    if (exportBtn) exportBtn.addEventListener("click", () => { alert("Consolidando directorio local y exportando reporte CSV...\nGenerado con éxito."); });

    // Monitor Paginación
    const prevBtn = document.getElementById(`admin_prev-${suf}`);
    if (prevBtn) prevBtn.addEventListener("click", () => {
      if (state.adminTable.page > 1) {
        state.adminTable.page--;
        renderMonitorTable();
      }
    });
    const nextBtn = document.getElementById(`admin_next-${suf}`);
    if (nextBtn) nextBtn.addEventListener("click", () => {
      const totalPages = Math.ceil(state.adminTable.results.length / state.adminTable.perPage);
      if (state.adminTable.page < totalPages) {
        state.adminTable.page++;
        renderMonitorTable();
      }
    });

    // Nueva categoría click
    const addTax = document.getElementById(`add_tax_btn-${suf}`) || document.getElementById(`add_tax_btn-${suf}`);
    if (addTax) addTax.addEventListener("click", handleAddCategory);

    // Crear usuario click
    const addUserBtn = document.getElementById(`btn_add_user-${suf}`);
    if (addUserBtn) addUserBtn.addEventListener("click", handleAddUser);
  });
}

async function performLogin() {
  const username = document.getElementById("login_user").value.trim();
  const password = document.getElementById("login_pass").value.trim();
  
  if (!username || !password) {
    alert("Por favor, ingrese todos los campos requeridos.");
    return;
  }
  
  try {
    const res = await fetch(`${API_BASE}/api/auth/login`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ username, password })
    });
    
    if (!res.ok) throw new Error();
    const data = await res.json();
    loginSuccess(data.user);
    
  } catch (err) {
    alert("Credenciales incorrectas de acceso institucional.");
  }
}
