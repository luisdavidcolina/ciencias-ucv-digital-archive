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

// ==========================================================================
// TOAST SYSTEM — función canónica, usada en todas las páginas
// ==========================================================================
function showToast(message, type, duration) {
  type = type || "info";
  const ttl = duration ?? { success: 3000, error: 6000, warning: 4500, info: 3500 }[type] ?? 3500;
  const container = document.getElementById("ds-toast-container");
  if (!container) return;

  const colors = {
    success: { bg: "#d4edda", color: "#155724", border: "#c3e6cb", icon: "fas fa-check-circle" },
    error:   { bg: "#f8d7da", color: "#721c24", border: "#f5c6cb", icon: "fas fa-times-circle" },
    warning: { bg: "#fff3cd", color: "#856404", border: "#ffeeba", icon: "fas fa-exclamation-triangle" },
    info:    { bg: "#d1ecf1", color: "#0c5460", border: "#bee5eb", icon: "fas fa-info-circle" },
  };
  const cfg = colors[type] || colors.info;

  const toast = document.createElement("div");
  toast.style.cssText = `background:${cfg.bg};color:${cfg.color};border:1px solid ${cfg.border};border-radius:8px;padding:10px 14px;margin-bottom:8px;min-width:260px;max-width:380px;display:flex;align-items:flex-start;gap:8px;box-shadow:0 4px 12px rgba(0,0,0,.15);font-size:0.87rem;transition:opacity 0.4s,transform 0.3s;transform:translateX(20px);`;
  toast.innerHTML = `<i class="${cfg.icon}" style="font-size:1rem;flex-shrink:0;margin-top:2px;"></i><span style="flex:1;">${message}</span><button style="background:none;border:none;padding:0 0 0 8px;cursor:pointer;opacity:0.6;color:inherit;font-size:1rem;" onclick="this.closest('div').remove()">✕</button>`;
  container.appendChild(toast);

  requestAnimationFrame(() => { toast.style.transform = "translateX(0)"; });

  const dismiss = () => {
    toast.style.opacity = "0";
    toast.style.transform = "translateX(20px)";
    setTimeout(() => toast.remove(), 400);
  };
  const timer = setTimeout(dismiss, ttl);
  toast.querySelector("button").addEventListener("click", () => clearTimeout(timer));
}

// ==========================================================================
// UTILIDADES COMPARTIDAS
// ==========================================================================

function _secureFileUrl(url) {
  if (!url || !url.startsWith("/api/files/")) return url;
  const username = state.user?.username || "";
  if (!username) return url;
  return `${url}${url.includes("?") ? "&" : "?"}u=${encodeURIComponent(username)}`;
}

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
// HELPERS ADMIN (usados en admin.js y sus split files)
// ==========================================================================
function adminSuffixFromTab(tab) {
  const t = tab || state.activeTab;
  return (t === "admin-rrhh") ? "rrhh" : "archivo";
}
function adminId(base) { return `${base}-${adminSuffixFromTab()}`; }
function getAdminEl(base) { return document.getElementById(adminId(base)); }
function isArchivoModule() { return state.user && state.user.modulo === "Archivo"; }
