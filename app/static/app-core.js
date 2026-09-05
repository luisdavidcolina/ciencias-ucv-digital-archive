// ==========================================================================
// ESTADO GLOBAL COMPARTIDO
// ==========================================================================
const state = {
  user: null,
  activeTab: "archivo",
  choices: null,
  activeAdminTab: "stats",
  archivo: {
    results: [], total: 0, search: "", selectedTypes: [], selectedTesauro: [],
    dateStart: "", dateEnd: "", sortMode: "Alfabético (A-Z)", page: 1, perPage: 5
  },
  rrhh: {
    results: [], total: 0, search: "", selectedTypes: [], selectedEstados: [],
    selectedPeople: [], dateStart: "", dateEnd: "", sortMode: "Alfabético (A-Z)",
    page: 1, perPage: 5
  },
  adminTable: { results: [], total: 0, search: "", typeFilter: "", page: 1, perPage: 25 },
  activePersonProfile: null,
  innerDossierSearch: "",
  innerDossierClass: "",
  innerDossierSort: "Alfabético (A-Z)"
};

const API_BASE = window.location.origin;
const tsInstances = {};
const fpInstances = {};

// VI-037: "1 Registros" — concatenación sin plural en la cabecera de los dos
// buscadores públicos. Compartida en app-core.js porque archive.js y hr.js
// necesitan el mismo criterio (singular cuando n === 1).
function plural(n, singular, pluralWord) {
  return `${n} ${n === 1 ? singular : pluralWord}`;
}

// ==========================================================================
// TOAST SYSTEM — función canónica, usada en todas las páginas
// ==========================================================================
function showToast(message, type, duration) {
  type = type || "info";
  const ttl = duration ?? { success: 3000, error: 6000, warning: 4500, info: 3500 }[type] ?? 3500;
  // Sólo los tres HTML de administración declaran #ds-toast-container a mano;
  // en /archivo y /rrhh (páginas públicas) no existe y los avisos —incluida la
  // de sesión expirada— se perdían en silencio. Se crea aquí si falta, así el
  // arreglo vale para cualquier página presente o futura sin tocar cada HTML.
  // El posicionamiento y los colores por tipo (SD-039/SD-129) viven en
  // styles.css (#ds-toast-container, .ds-toast, .ds-toast--<tipo>), con su
  // par oscuro — aquí sólo se aplican las clases.
  let container = document.getElementById("ds-toast-container");
  if (!container) {
    container = document.createElement("div");
    container.id = "ds-toast-container";
    container.setAttribute("aria-live", "polite");
    container.setAttribute("aria-atomic", "true");
    document.body.appendChild(container);
  } else if (!container.hasAttribute("aria-live")) {
    container.setAttribute("aria-live", "polite");
    container.setAttribute("aria-atomic", "true");
  }

  const icons = {
    success: "fas fa-check-circle",
    error:   "fas fa-times-circle",
    warning: "fas fa-exclamation-triangle",
    info:    "fas fa-info-circle",
  };
  const icon = icons[type] || icons.info;

  const toast = document.createElement("div");
  toast.className = `ds-toast ds-toast--${type}`;
  toast.innerHTML = `<i class="${icon}" aria-hidden="true"></i><span class="ds-toast-msg"></span><button type="button" class="ds-toast-close" aria-label="Cerrar aviso">✕</button>`;
  toast.querySelector(".ds-toast-msg").textContent = message;
  container.appendChild(toast);

  const dismiss = () => toast.remove();
  const timer = setTimeout(dismiss, ttl);
  toast.querySelector(".ds-toast-close").addEventListener("click", () => { clearTimeout(timer); dismiss(); });
}

// ==========================================================================
// UTILIDADES COMPARTIDAS
// ==========================================================================

function _secureFileUrl(url) {
  if (!url) return "";
  // Solo permitir URLs relativas o http/https — nunca javascript:, data:, etc.
  if (!/^(\/|https?:\/\/)/.test(url)) return "";
  if (!url.startsWith("/api/files/")) return url;
  const username = state.user?.username || "";
  if (!username) return url;
  return `${url}${url.includes("?") ? "&" : "?"}u=${encodeURIComponent(username)}`;
}

function highlightTerms(text, terms) {
  if (!text || !terms || !terms.length) return escHtml(text) || "";
  const safe = escHtml(text);
  const escaped = terms
    .map(t => escHtml(t).replace(/[.*+?^${}()|[\]\\]/g, "\\$&"))
    .filter(Boolean)
    .join("|");
  if (!escaped) return safe;
  try {
    return safe.replace(
      new RegExp(`(${escaped})`, "gi"),
      '<mark style="background:#fff176;border-radius:2px;padding:0 1px;">$1</mark>'
    );
  } catch { return safe; }
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

// Los colores llevan texto blanco encima, asi que tienen que aguantar 4,5:1
// contra el. El verde de Bootstrap se quedaba en 3,13:1; el resto ya cumplia.
function getStatusColor(status) {
  switch (status) {
    case "Activo":    return "#208838";
    case "Retirado":  return "#dc3545";
    case "Jubilado":  return "#6f42c1";
    case "Pensionado":return "#0056b3";
    default:          return "#6c757d";
  }
}

// ==========================================================================
// HELPERS ADMIN (usados en admin.js y sus split files)
// ==========================================================================
function adminSuffixFromTab(tab) {
  const t = tab || state.activeTab;
  return (t === "admin-rrhh") ? "rrhh" : "archivo";
}
function adminId(base) { return `${base}-${adminSuffixFromTab()}`; }
function getAdminEl(base) { return document.getElementById(adminId(base)); }
function isArchivoModule() { return state.user && state.user.modulo === "Archivo"; }
