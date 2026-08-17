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
