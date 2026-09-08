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
    // BA-008: el <select> de archive.html marca "10 por página" como opción
    // seleccionada; este valor tiene que coincidir con la primera carga o la
    // interfaz miente sobre cuántos resultados trae.
    dateStart: "", dateEnd: "", sortMode: "Alfabético (A-Z)", page: 1, perPage: 10
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

function _motionBehavior() {
  const reduced = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  return (document.body.classList.contains("ds-no-anim") || reduced) ? "auto" : "smooth";
}

// BUSQUEDA-llamadas-huerfanas: toggleDocViewer()/closeDocViewer() vivían solo
// en archive.js. hr.js abre el mismo markup compartido
// (#modal-doc-viewer-section / #modal-doc-iframe, idéntico en archive.html y
// hr.html) desde openDocMetadataModal() sin que hr.html cargue archive.js —
// cada apertura del modal de documento de un expediente RRHH lanzaba un
// ReferenceError no capturado y dejaba la ficha a medio pintar. Viven aquí
// porque app-core.js es el único script común a archive.html y hr.html (y a
// admin_archive.html/admin_hr.html, donde el markup no existe y ambas
// funciones no hacen nada por su propia guarda).
function toggleDocViewer(fileUrl) {
  const section = document.getElementById("modal-doc-viewer-section");
  const iframe  = document.getElementById("modal-doc-iframe");
  if (!section || !iframe) return;
  if (section.classList.contains("d-none")) {
    const isImg = /\.(png|jpe?g|gif|webp|svg)$/i.test(fileUrl);
    if (isImg) {
      iframe.style.display = "none";
      let img = section.querySelector("img.ds-viewer-img");
      if (!img) { img = document.createElement("img"); img.className = "ds-viewer-img"; img.style.cssText = "max-width:100%;max-height:500px;display:block;margin:auto;border-radius:4px;"; section.appendChild(img); }
      img.src = fileUrl;
      img.style.display = "block";
    } else {
      const img = section.querySelector("img.ds-viewer-img");
      if (img) img.style.display = "none";
      iframe.style.display = "block";
      iframe.src = fileUrl;
    }
    section.classList.remove("d-none");
    section.scrollIntoView({ behavior: _motionBehavior(), block: "nearest" });
  } else {
    closeDocViewer();
  }
}

function closeDocViewer() {
  const section = document.getElementById("modal-doc-viewer-section");
  const iframe  = document.getElementById("modal-doc-iframe");
  if (section) section.classList.add("d-none");
  if (iframe)  iframe.src = "";
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
  if (!text) return "";
  const safe = escHtml(text) || "";
  if (!terms || !terms.length) return safe;
  // BA-028: la regex tiene que correr sobre el texto SIN escapar. Antes se
  // escapaba primero y se resaltaba sobre el HTML ya escapado: buscar "amp",
  // "quot", "39" o "lt" resaltaba dentro de "&amp;"/"&quot;"/etc. y rompía el
  // marcado. Ahora se localizan los tramos en el texto plano y cada tramo
  // (coincida o no) se escapa por separado antes de insertarlo.
  const pattern = terms
    .map(t => String(t ?? "").trim())
    .filter(Boolean)
    .map(t => t.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"))
    .join("|");
  if (!pattern) return safe;
  const raw = String(text);
  let result = "";
  let lastIndex = 0;
  try {
    const re = new RegExp(`(${pattern})`, "gi");
    let m;
    while ((m = re.exec(raw)) !== null) {
      if (m[0] === "") { re.lastIndex++; continue; }
      result += escHtml(raw.slice(lastIndex, m.index));
      // BA-041: sin estilo en línea — el amarillo/negro por defecto del
      // navegador para <mark> no depende del color heredado, así que se lee
      // igual en modo oscuro sin necesitar un token nuevo en styles.css.
      result += `<mark>${escHtml(m[0])}</mark>`;
      lastIndex = re.lastIndex;
    }
    result += escHtml(raw.slice(lastIndex));
    return result;
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
function isArchivoModule() { return state.user && state.user.modulo === "Archivo"; }
