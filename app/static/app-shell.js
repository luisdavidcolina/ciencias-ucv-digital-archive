// =============================================================================
// CÁSCARA DE LA APLICACIÓN — menú lateral
//
// El mismo menú estaba copiado en cinco páginas. Eran idénticos salvo por cuál
// enlace llevaba `active`, y aun así habían derivado: el enlace del asistente
// existía solo en el panel de Sistema. Aquí vive una sola vez.
//
// Se inyecta de forma SÍNCRONA, en el punto del documento donde está la
// etiqueta <script>, para que los ids existan antes de que corra cualquier
// handler de DOMContentLoaded — configureSidebarVisibilities() los busca por id.
//
// Qué se ve lo decide el rol, no el marcado: ver configureSidebarVisibilities()
// en app.js. Por eso el menú puede ser el mismo en todas las páginas.
// =============================================================================

const SHELL_SECCIONES = [
  {
    label: "Módulos",
    links: [
      { id: "menu-btn-archivo", href: "/archivo", icon: "fa-folder-open",
        label: "Archivo Institucional", page: "archivo" },
      { id: "menu-btn-rrhh", href: "/rrhh", icon: "fa-users",
        label: "Personal", page: "rrhh" },
    ],
  },
  {
    label: "Administración",
    groupId: "sidebar-admin-group",
    links: [
      { id: "menu-btn-admin-archivo", href: "/admin/archivo", icon: "fa-sliders-h",
        label: "Panel Archivo", page: "admin-archivo", badge: "A" },
      { id: "menu-btn-admin-rrhh", href: "/admin/rrhh", icon: "fa-user-shield",
        label: "Panel RRHH", page: "admin-rrhh", badge: "R" },
      { id: "menu-btn-admin-sistema", href: "/admin/sistema", icon: "fa-server",
        label: "Sistema Global", page: "admin-sistema", oculto: true },
      { id: "menu-btn-admin-ia", href: "/admin/ia", icon: "fa-robot",
        label: "Asistente IA", page: "admin-ia", oculto: true },
    ],
  },
  {
    label: "Utilidades",
    links: [
      { href: "/ayuda", icon: "fa-life-ring", label: "Ayuda", page: "ayuda" },
      { href: "/investigacion", icon: "fa-flask", label: "Investigación",
        page: "investigacion" },
    ],
  },
];

function _shellLink(l, pagina) {
  const activo = l.page && l.page === pagina ? " active" : "";
  // Los enlaces de admin arrancan ocultos y los muestra configureSidebarVisibilities()
  // según el rol; sin esto parpadean visibles antes de que corra.
  const estilo = l.oculto ? ' style="display:none"' : "";
  const id = l.id ? ` id="${l.id}"` : "";
  const badge = l.badge
    ? `<span class="ds-sidebar-badge">${l.badge}</span>`
    : "";
  return `<a${id} href="${l.href}" class="ds-sidebar-link${activo}"${estilo}>` +
         `<i class="fas ${l.icon}"></i><span>${l.label}</span>${badge}</a>`;
}

function shellSidebarHTML(pagina) {
  const secciones = SHELL_SECCIONES.map(s => {
    const cuerpo = `<div class="ds-sidebar-section-label">${s.label}</div>` +
                   s.links.map(l => _shellLink(l, pagina)).join("");
    return s.groupId ? `<div id="${s.groupId}">${cuerpo}</div>` : cuerpo;
  }).join("");

  return `
<aside id="app-sidebar" class="ds-sidebar">
  <div class="ds-sidebar-header">
    <div class="ds-sidebar-brand">
      <img src="/static/assets/icons/favicon-32x32.png" alt=""
           style="height:28px;width:28px;display:inline-block;vertical-align:middle;">
      <span style="vertical-align:middle;">Ciencias UCV</span>
    </div>
    <button id="sidebar-close-btn" class="ds-sidebar-close" title="Cerrar menú">
      <i class="fas fa-times"></i>
    </button>
  </div>
  <div class="ds-sidebar-divider"></div>
  <nav class="ds-sidebar-nav">
    ${secciones}
    <button class="ds-sidebar-theme-btn" onclick="openThemePanel()">
      <i class="fas fa-palette"></i><span>Personalización</span>
    </button>
  </nav>
  <div class="ds-sidebar-footer">
    <i class="fas fa-shield-alt mr-1"></i> Intranet Cifrada
  </div>
</aside>`;
}

(function () {
  const hueco = document.getElementById("app-shell-sidebar");
  if (!hueco) return;
  hueco.outerHTML = shellSidebarHTML(document.body.dataset.page || "");
})();

// -----------------------------------------------------------------------------
// Barra superior
//
// Estaba copiada en las mismas cinco páginas. Las diferencias eran el destino
// del logo y, en el panel de Sistema, un id distinto para el nombre de usuario
// y un onclick propio en el botón de salir que solo limpiaba localStorage — sin
// avisar al servidor, así que la sesión seguía viva del otro lado.
// -----------------------------------------------------------------------------

const SHELL_INICIO = { "rrhh": "/rrhh", "admin-rrhh": "/rrhh" };

function shellNavbarHTML(pagina) {
  const inicio = SHELL_INICIO[pagina] || "/archivo";
  const volver = pagina === "admin-sistema" || pagina === "admin-ia"
    ? `<li class="nav-item">
         <a href="/archivo" class="btn btn-outline-secondary btn-sm" style="margin-top:5px;margin-right:8px;">
           <i class="fas fa-arrow-left"></i> <span class="ds-btn-label">Volver al Inicio</span>
         </a>
       </li>`
    : "";

  return `
<nav class="main-header navbar navbar-expand navbar-white navbar-light">
  <ul class="navbar-nav">
    <li class="nav-item">
      <button id="sidebar-toggle-btn" class="btn btn-link nav-link px-2" title="Menú"
              style="font-size:1.3rem;color:var(--ds-accent);">
        <i class="fas fa-bars"></i>
      </button>
    </li>
    <li class="nav-item dropdown">
      <a href="${inicio}" class="ds-navbar-logo nav-link">
        <img src="/static/logo.png" alt="Archivo Institucional" class="ds-navbar-logo-img">
      </a>
    </li>
  </ul>
  <ul class="navbar-nav ml-auto">
    ${volver}
    <li class="nav-item dropdown ds-nav-user">
      <span style="padding-top:10px;display:inline-block;margin-right:8px;font-weight:bold;color:#dc3545;">
        <i class="fas fa-user-circle"></i> <span id="nav_username">ID: anonymous</span>
      </span>
    </li>
    <li class="nav-item dropdown ds-nav-logout">
      <button id="logout_btn" class="btn btn-outline-secondary btn-sm"
              style="margin-top:5px;margin-right:10px;">
        <i class="fas fa-sign-out-alt"></i> <span class="ds-btn-label">Cerrar Sesión</span>
      </button>
    </li>
  </ul>
</nav>`;
}

// Enlace para saltar la navegación (WCAG 2.4.1, nivel A). Sin él, quien navega
// con teclado atraviesa trece controles de menú antes de llegar al contenido —
// en cada página, cada vez. Está oculto hasta que recibe el foco.
function shellSkipLinkHTML() {
  return '<a class="ds-skip-link" href="#contenido-principal">' +
         'Saltar al contenido principal</a>';
}

// El destino del salto se marca aquí y no en cada HTML: así ninguna página
// nueva se queda sin él por olvido.
function _marcarContenidoPrincipal() {
  if (document.getElementById("contenido-principal")) return;
  const destino = document.querySelector(".content-wrapper, main, [role='main']");
  if (!destino) return;
  destino.id = "contenido-principal";
  // tabindex="-1" para que el navegador pueda enfocarlo: sin esto el salto
  // mueve el scroll pero no el foco, y el teclado sigue donde estaba.
  destino.setAttribute("tabindex", "-1");
}

(function () {
  const hueco = document.getElementById("app-shell-navbar");
  if (!hueco) return;
  hueco.outerHTML = shellSkipLinkHTML() + shellNavbarHTML(document.body.dataset.page || "");
})();

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", _marcarContenidoPrincipal);
} else {
  _marcarContenidoPrincipal();
}
