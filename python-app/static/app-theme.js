// ==========================================================================
// SISTEMA DE PERSONALIZACIÃ“N
// ==========================================================================
const THEMES = [
  { id:"default",            name:"ClÃ¡sico UCV",         sidebar:"#2b4e72", content:"#f8f9fa", bar:"#2b4e72" },
  { id:"theme-azul-profundo",name:"Azul Profundo",       sidebar:"#0f3460", content:"#e8f3ff", bar:"#1a5fa0" },
  { id:"theme-dorado",       name:"Dorado AcadÃ©mico",    sidebar:"#9a7017", content:"#fffdf0", bar:"#B8860B" },
  { id:"theme-manila",       name:"Manila / CartÃ³n",     sidebar:"#8B7355", content:"#f5ead0", bar:"#A0856A" },
  { id:"theme-esmeralda",    name:"Verde Esmeralda",     sidebar:"#2d6a4f", content:"#f0faf2", bar:"#40916c" },
  { id:"theme-cian",         name:"Cian Institucional",  sidebar:"#0d6e7c", content:"#e8fafc", bar:"#13a0b4" },
  { id:"theme-lavanda",      name:"Lavanda",             sidebar:"#4a3570", content:"#f5f0ff", bar:"#7c5cbf" },
  { id:"theme-granate",      name:"Granate AcadÃ©mico",   sidebar:"#7c2d3c", content:"#fff5f7", bar:"#a03048" },
  { id:"theme-terracota",    name:"Terracota",           sidebar:"#b5451b", content:"#fff8f5", bar:"#d4623a" },
  { id:"theme-noche",        name:"Medianoche",          sidebar:"#0d1b2a", content:"#f0f4f8", bar:"#4a90d9" },
  { id:"theme-carbon",       name:"Gris CarbÃ³n",         sidebar:"#2c2c2c", content:"#f8f8f8", bar:"#555555" },
  { id:"theme-oliva",        name:"Verde Oliva",         sidebar:"#5a6b2a", content:"#f5fae0", bar:"#7a9040" },
];

const FONT_SCALES = [
  { value: 0.82, label: "PequeÃ±o" },
  { value: 0.93, label: "Normal"  },
  { value: 1.08, label: "Grande"  },
  { value: 1.22, label: "Muy Grande" },
];

// â”€â”€ init â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

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

// â”€â”€ dark mode â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

function _applyDarkModeDOM(mode) {
  const isDark = mode === "dark" ||
    (mode === "auto" && window.matchMedia?.("(prefers-color-scheme: dark)").matches);
  document.body.classList.toggle("dark-mode", isDark);
  document.querySelectorAll(".ds-navbar-logo-img").forEach(img => {
    if (!img.dataset.logoLight) img.dataset.logoLight = img.src;
    img.src = isDark ? "/static/logoblanco.png" : img.dataset.logoLight;
  });
}

function applyDarkMode(mode) {
  localStorage.setItem("ds_dark_mode", mode);
  _applyDarkModeDOM(mode);
}

// â”€â”€ font scale â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

function _applyFontScaleDOM(scale) {
  document.documentElement.style.setProperty("--ds-font-scale", scale);
}

function applyFontScale(scale) {
  localStorage.setItem("ds_font_scale", scale);
  _applyFontScaleDOM(scale);
}

// â”€â”€ color theme â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

function applyTheme(themeId) {
  document.body.className = document.body.className.replace(/\btheme-[\w-]+\b/g, "").trim();
  if (themeId !== "default") document.body.classList.add(themeId);
  localStorage.setItem("ds_theme", themeId);
  _rebuildPanel();
}

// â”€â”€ panel â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

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
      <h6><i class="fas fa-sliders-h mr-2"></i>PersonalizaciÃ³n</h6>
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
        <div class="ds-panel-label"><i class="fas fa-text-height mr-1"></i> TamaÃ±o de Texto <span class="ds-font-current">${cScaleLabel}</span></div>
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

// â”€â”€ NOTIFICACIONES Y PENDIENTES â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
let _notifInterval = null;

function _initNotificationBell(user) {
  // Solo mostrar a usuarios con rol admin
  const isAdmin = user.roles && (user.roles["Archivo"] === "Admin" || user.roles["RRHH"] === "Admin");
  if (!isAdmin) return;

  // Inyectar el botÃ³n de campana antes del usuario en la barra
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
    const STATUS_LBL  = { revision: "RevisiÃ³n", draft: "Borrador" };

    list.innerHTML = (data.items || []).slice(0, 15).map(it => {
      const icon = STATUS_ICON[it.status] || "fas fa-file";
      const lbl  = STATUS_LBL[it.status]  || it.status;
      const href = it.modulo === "Archivo"
        ? `/admin/archivo?docId=${it.id}`
        : `/admin/rrhh?empId=${it.id}`;
      return `<a class="ds-notif-item" href="${href}" onclick="document.getElementById('ds-notif-panel').style.display='none'">
        <i class="${icon}" style="width:14px;flex-shrink:0;"></i>
        <div class="ds-notif-item-body">
          <div class="ds-notif-item-label">${it.label || 'â€”'}</div>
          <div class="ds-notif-item-meta">${it.modulo} Â· ${lbl} Â· ${it.ts || ''}</div>
        </div>
      </a>`;
    }).join("");
  } catch { /* silencioso */ }
}
