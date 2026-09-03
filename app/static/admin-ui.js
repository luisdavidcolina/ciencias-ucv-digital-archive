// =============================================================================
// ADMIN-UI — Utilidades de UX: modales, loading, debounce, atajos de teclado
// Cargado antes de admin.js en admin_archive.html y admin_hr.html
// =============================================================================

// ─── Debounce ────────────────────────────────────────────────────────────────
function debounce(fn, ms) {
  let timer;
  return (...args) => { clearTimeout(timer); timer = setTimeout(() => fn(...args), ms); };
}

// ─── Modales propios de estos helpers ────────────────────────────────────────
// El marcado de los modales de confirmación y de texto estaba copiado en las
// tres páginas de administración. Es infraestructura de estos helpers, no de
// las páginas: si confirmModal() vive aquí, su modal también. Así una página
// nueva que llame a confirmModal() funciona sin tener que acordarse de pegar
// veinte líneas de HTML.
const _MODALES_UI = `
<div class="modal fade" id="ds-confirm-modal" tabindex="-1" role="dialog" aria-modal="true" aria-labelledby="ds-cm-title-id" data-backdrop="static" data-keyboard="false">
  <div class="modal-dialog modal-sm modal-dialog-centered">
    <div class="modal-content border-0 shadow-lg" style="border-radius:12px;">
      <div class="modal-header border-0 pb-0">
        <h6 class="modal-title font-weight-bold ds-cm-title" id="ds-cm-title-id">Confirmar</h6>
      </div>
      <div class="modal-body pt-2 pb-2">
        <p class="ds-cm-body mb-0 text-muted" style="font-size:0.9rem;"></p>
      </div>
      <div class="modal-footer border-0 pt-1">
        <button type="button" class="btn btn-secondary btn-sm ds-cm-cancel">Cancelar</button>
        <button type="button" class="btn btn-danger btn-sm ds-cm-ok">Eliminar</button>
      </div>
    </div>
  </div>
</div>
<div class="modal fade" id="ds-prompt-modal" tabindex="-1" role="dialog" aria-modal="true" aria-labelledby="ds-pm-title-id" data-backdrop="static" data-keyboard="false">
  <div class="modal-dialog modal-sm modal-dialog-centered">
    <div class="modal-content border-0 shadow-lg" style="border-radius:12px;">
      <div class="modal-header border-0 pb-0">
        <h6 class="modal-title font-weight-bold ds-pm-title" id="ds-pm-title-id">Ingrese un valor</h6>
      </div>
      <div class="modal-body pt-2 pb-2">
        <label class="text-muted small ds-pm-label mb-1" for="ds-pm-input-id"></label>
        <div class="input-group">
          <input type="text" id="ds-pm-input-id" class="form-control ds-pm-input" autocomplete="off">
          <div class="input-group-append ds-pm-toggle-wrap d-none">
            <button type="button" class="btn btn-outline-secondary ds-pm-toggle" aria-label="Mostrar valor" aria-pressed="false">
              <i class="fas fa-eye"></i>
            </button>
          </div>
        </div>
      </div>
      <div class="modal-footer border-0 pt-1">
        <button type="button" class="btn btn-secondary btn-sm ds-pm-cancel">Cancelar</button>
        <button type="button" class="btn btn-primary btn-sm ds-pm-ok">Aceptar</button>
      </div>
    </div>
  </div>
</div>
<div class="modal fade" id="ds-link-modal" tabindex="-1" role="dialog" aria-modal="true" aria-labelledby="ds-lm-title-id" data-backdrop="static" data-keyboard="false">
  <div class="modal-dialog modal-dialog-centered">
    <div class="modal-content border-0 shadow-lg" style="border-radius:12px;">
      <div class="modal-header border-0 pb-0">
        <h6 class="modal-title font-weight-bold ds-lm-title" id="ds-lm-title-id">Enlace</h6>
      </div>
      <div class="modal-body pt-2 pb-2">
        <p class="ds-lm-body text-muted mb-2" style="font-size:0.9rem;"></p>
        <label class="text-muted small mb-1" for="ds-lm-input-id">Enlace</label>
        <div class="input-group">
          <input type="text" id="ds-lm-input-id" class="form-control ds-lm-input" readonly>
          <div class="input-group-append">
            <button type="button" class="btn btn-outline-primary ds-lm-copy">Copiar</button>
          </div>
        </div>
      </div>
      <div class="modal-footer border-0 pt-1">
        <button type="button" class="btn btn-secondary btn-sm ds-lm-close">Cerrar</button>
      </div>
    </div>
  </div>
</div>
<div class="modal fade" id="ds-shortcuts-modal" tabindex="-1" role="dialog" aria-modal="true" aria-labelledby="ds-sc-title-id">
  <div class="modal-dialog modal-dialog-centered">
    <div class="modal-content border-0 shadow-lg" style="border-radius:12px;">
      <div class="modal-header border-0 pb-0">
        <h6 class="modal-title font-weight-bold" id="ds-sc-title-id">Atajos de teclado</h6>
        <button type="button" class="close ds-sc-close" aria-label="Cerrar"><span aria-hidden="true">&times;</span></button>
      </div>
      <div class="modal-body pt-2 pb-3">
        <ul class="list-unstyled mb-0 small" style="line-height:2;">
          <li><kbd>Esc</kbd> — cerrar el diálogo abierto</li>
          <li><kbd>Ctrl</kbd> + <kbd>S</kbd> — guardar el modal abierto</li>
          <li><kbd>/</kbd> — ir al buscador</li>
          <li><kbd>N</kbd> — nuevo registro</li>
          <li><kbd>1</kbd>–<kbd>9</kbd> — saltar a la pestaña correspondiente</li>
          <li><kbd>?</kbd> — esta ayuda</li>
        </ul>
      </div>
    </div>
  </div>
</div>`;

function _asegurarModalesUI() {
  if (document.getElementById("ds-confirm-modal")) return;
  const cont = document.createElement("div");
  cont.innerHTML = _MODALES_UI;
  while (cont.firstElementChild) document.body.appendChild(cont.firstElementChild);
}

// Se inyectan en cuanto el <body> existe: confirmModal() puede llamarse desde
// cualquier handler y no debe depender de quién cargó primero.
if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", _asegurarModalesUI);
} else {
  _asegurarModalesUI();
}

// ─── Trampa de foco, pila de modales y retorno de foco (OA-172/173/174, OR-219) ──
// Infraestructura genérica: cualquier modal Bootstrap (`.modal`) que se abra con
// $(el).modal("show") queda cubierto sin que cada página tenga que ocuparse de
// guardar el disparador, atrapar el Tab o restaurar el foco al cerrar.
const _modalStack = [];
const _modalTriggers = new WeakMap();

function _focusablesIn(modal) {
  return [...modal.querySelectorAll(
    'a[href], button:not([disabled]), textarea:not([disabled]), input:not([disabled]), ' +
    'select:not([disabled]), [tabindex]:not([tabindex="-1"])'
  )].filter(el => el.offsetParent !== null || el === document.activeElement);
}

function _trapFocusKeydown(e) {
  if (e.key !== "Tab") return;
  const modal = _modalStack[_modalStack.length - 1];
  if (!modal || !modal.contains(e.target) && document.activeElement !== modal) return;
  const focusables = _focusablesIn(modal);
  if (!focusables.length) { e.preventDefault(); return; }
  const first = focusables[0];
  const last = focusables[focusables.length - 1];
  if (e.shiftKey && document.activeElement === first) {
    e.preventDefault(); last.focus();
  } else if (!e.shiftKey && document.activeElement === last) {
    e.preventDefault(); first.focus();
  }
}

function _setInertSiblings(modal, inert) {
  [...document.body.children].forEach(child => {
    if (child === modal || child.classList?.contains("modal-backdrop")) return;
    if (inert) {
      if (child.hasAttribute("inert") || child.getAttribute("aria-hidden") === "true") return;
      if (!child.hasAttribute("aria-hidden")) child.dataset.dsRestoreAriaHidden = "1";
      child.setAttribute("aria-hidden", "true");
      try { child.inert = true; } catch { /* navegador sin soporte para inert */ }
    } else {
      // Sólo se retira si ya no queda ningún modal abierto por debajo.
      if (_modalStack.length) return;
      if (child.dataset.dsRestoreAriaHidden) {
        child.removeAttribute("aria-hidden");
        delete child.dataset.dsRestoreAriaHidden;
      }
      try { child.inert = false; } catch { /* nada que hacer */ }
    }
  });
}

document.addEventListener("show.bs.modal", e => {
  const modal = e.target;
  if (!(modal instanceof HTMLElement)) return;
  const trigger = document.activeElement;
  if (trigger && trigger !== document.body) _modalTriggers.set(modal, trigger);
});

document.addEventListener("shown.bs.modal", e => {
  const modal = e.target;
  if (!(modal instanceof HTMLElement)) return;
  if (!_modalStack.includes(modal)) _modalStack.push(modal);
  _setInertSiblings(modal, true);
  document.addEventListener("keydown", _trapFocusKeydown, true);
  // Foco inicial: el primer elemento marcado como seguro, si existe, si no el
  // primer elemento enfocable del cuerpo del modal (OR-220).
  const safe = modal.querySelector("[data-autofocus]") || modal.querySelector(".modal-footer .btn-secondary");
  const target = safe || _focusablesIn(modal)[0];
  if (target) target.focus();
});

document.addEventListener("hidden.bs.modal", e => {
  const modal = e.target;
  if (!(modal instanceof HTMLElement)) return;
  const idx = _modalStack.indexOf(modal);
  if (idx !== -1) _modalStack.splice(idx, 1);
  _setInertSiblings(modal, false);
  if (!_modalStack.length) document.removeEventListener("keydown", _trapFocusKeydown, true);
  const trigger = _modalTriggers.get(modal);
  _modalTriggers.delete(modal);
  if (trigger && document.contains(trigger) && typeof trigger.focus === "function") {
    trigger.focus();
  }
});

// ─── Modal de confirmación (reemplaza window.confirm) ────────────────────────
let _confirmResolve = null;

function _normalizarBtnClass(btnClass) {
  if (!btnClass) return "btn-danger";
  return btnClass.startsWith("btn-") ? btnClass : `btn-${btnClass}`;
}

function confirmModal(title, body, btnLabel = "Eliminar", btnClass = "btn-danger") {
  return new Promise(resolve => {
    _confirmResolve = resolve;
    _asegurarModalesUI();
    const el = document.getElementById("ds-confirm-modal");
    if (!el) { resolve(window.confirm(body)); return; }
    el.querySelector(".ds-cm-title").textContent  = title || "Confirmar";
    el.querySelector(".ds-cm-body").textContent   = body  || "¿Estás seguro?";
    const btn = el.querySelector(".ds-cm-ok");
    btn.textContent  = btnLabel;
    btn.className    = `btn ${_normalizarBtnClass(btnClass)} ds-cm-ok`;
    $(el).modal("show");
  });
}

document.addEventListener("click", e => {
  if (e.target.matches(".ds-cm-ok")) {
    $(document.getElementById("ds-confirm-modal")).modal("hide");
    if (_confirmResolve) { _confirmResolve(true); _confirmResolve = null; }
  }
  if (e.target.matches(".ds-cm-cancel")) {
    $(document.getElementById("ds-confirm-modal")).modal("hide");
    if (_confirmResolve) { _confirmResolve(false); _confirmResolve = null; }
  }
});
document.getElementById("ds-confirm-modal")?.addEventListener("hidden.bs.modal", () => {
  if (_confirmResolve) { _confirmResolve(false); _confirmResolve = null; }
});

// ─── Modal de texto (reemplaza window.prompt) ─────────────────────────────────
let _promptResolve = null;

// `type`: "text" (por defecto) o "password" — con botón de mostrar/ocultar
// (OA-039/OA-040: la contraseña nueva ya no viaja en un campo de texto plano).
function promptModal(title, label, defaultVal = "", placeholder = "", type = "text") {
  return new Promise(resolve => {
    _promptResolve = resolve;
    _asegurarModalesUI();
    const el = document.getElementById("ds-prompt-modal");
    if (!el) { resolve(window.prompt(label, defaultVal)); return; }
    el.querySelector(".ds-pm-title").textContent = title || "Ingrese un valor";
    el.querySelector(".ds-pm-label").textContent = label || "";
    const inp = el.querySelector(".ds-pm-input");
    inp.value       = defaultVal;
    inp.placeholder = placeholder;
    inp.type        = type === "password" ? "password" : "text";
    inp.autocomplete = type === "password" ? "new-password" : "off";
    const toggleWrap = el.querySelector(".ds-pm-toggle-wrap");
    const toggleBtn  = el.querySelector(".ds-pm-toggle");
    toggleWrap.classList.toggle("d-none", type !== "password");
    toggleBtn.setAttribute("aria-pressed", "false");
    toggleBtn.querySelector("i").className = "fas fa-eye";
    inp.dataset.dsAutofocus = "1";
    inp.setAttribute("data-autofocus", "");
    $(el).modal("show");
  });
}

document.addEventListener("click", e => {
  if (e.target.closest(".ds-pm-toggle")) {
    const btn = e.target.closest(".ds-pm-toggle");
    const inp = document.querySelector("#ds-prompt-modal .ds-pm-input");
    if (!inp) return;
    const showing = inp.type === "text";
    inp.type = showing ? "password" : "text";
    btn.setAttribute("aria-pressed", showing ? "false" : "true");
    btn.setAttribute("aria-label", showing ? "Mostrar valor" : "Ocultar valor");
    btn.querySelector("i").className = showing ? "fas fa-eye" : "fas fa-eye-slash";
    inp.focus();
    return;
  }
  if (e.target.matches(".ds-pm-ok")) {
    const val = document.querySelector("#ds-prompt-modal .ds-pm-input")?.value ?? null;
    $(document.getElementById("ds-prompt-modal")).modal("hide");
    if (_promptResolve) { _promptResolve(val); _promptResolve = null; }
  }
  if (e.target.matches(".ds-pm-cancel")) {
    $(document.getElementById("ds-prompt-modal")).modal("hide");
    if (_promptResolve) { _promptResolve(null); _promptResolve = null; }
  }
});
document.getElementById("ds-prompt-modal")?.addEventListener("hidden.bs.modal", () => {
  if (_promptResolve) { _promptResolve(null); _promptResolve = null; }
});
document.addEventListener("shown.bs.modal", e => {
  if (e.target.id !== "ds-prompt-modal") return;
  // Antes: setTimeout(…, 300) arbitrario. Con el evento shown.bs.modal ya
  // atrapando el foco, sólo hace falta moverlo al campo real.
  e.target.querySelector(".ds-pm-input")?.focus();
});
document.addEventListener("keydown", e => {
  const pm = document.getElementById("ds-prompt-modal");
  if (pm && pm.classList.contains("show") && e.key === "Enter" && !e.target.closest(".ds-pm-toggle")) {
    e.preventDefault();
    pm.querySelector(".ds-pm-ok")?.click();
  }
});

// ─── Modal de enlace (OA-015: el respaldo sin portapapeles ya no vuelca HTML
// crudo dentro de confirmModal) ───────────────────────────────────────────────
function linkModal(title, body, url) {
  _asegurarModalesUI();
  const el = document.getElementById("ds-link-modal");
  if (!el) return;
  el.querySelector(".ds-lm-title").textContent = title || "Enlace";
  el.querySelector(".ds-lm-body").textContent  = body  || "";
  const inp = el.querySelector(".ds-lm-input");
  inp.value = url || "";
  inp.setAttribute("data-autofocus", "");
  $(el).modal("show");
}

document.addEventListener("click", e => {
  if (e.target.matches(".ds-lm-close")) {
    $(document.getElementById("ds-link-modal")).modal("hide");
  }
  if (e.target.matches(".ds-lm-copy")) {
    const inp = document.querySelector("#ds-link-modal .ds-lm-input");
    if (!inp) return;
    const doCopy = navigator.clipboard?.writeText
      ? navigator.clipboard.writeText(inp.value)
      : (() => { inp.select(); document.execCommand("copy"); return Promise.resolve(); })();
    doCopy.then(() => showToast?.("Enlace copiado.", "success"))
          .catch(() => { inp.select(); });
  }
});

// ─── Skeleton loader para tablas ──────────────────────────────────────────────
function showTableSkeleton(tbodyId, cols = 6, rows = 5) {
  const tbody = document.getElementById(tbodyId);
  if (!tbody) return;
  const cell = `<td><div class="ds-skeleton"></div></td>`;
  tbody.innerHTML = Array.from({ length: rows }, () =>
    `<tr>${cell.repeat(cols)}</tr>`
  ).join("");
}

// ─── Overlay de carga sobre tarjetas ─────────────────────────────────────────
function showCardLoading(containerId) {
  const el = document.getElementById(containerId);
  if (!el) return;
  el.style.position = "relative";
  const ov = document.createElement("div");
  ov.id = `loading-ov-${containerId}`;
  ov.style.cssText = "position:absolute;inset:0;background:rgba(255,255,255,.7);display:flex;align-items:center;justify-content:center;z-index:10;border-radius:4px;";
  ov.innerHTML = `<div class="spinner-border text-primary" role="status" style="width:2rem;height:2rem;"></div>`;
  el.appendChild(ov);
}

function hideCardLoading(containerId) {
  document.getElementById(`loading-ov-${containerId}`)?.remove();
}

// ─── Atajos de teclado globales (OA-172..OA-182, OR-219..OR-226) ─────────────
// Escape cierra el modal por encima de la pila (no «el primero que aparezca»),
// y confirma antes si el modal en curso quedó marcado como sucio con
// `data-dirty="true"` (lo marca cada formulario, p.ej. admin-edit.js en
// OA-182/OR-... al detectar el primer cambio).
document.addEventListener("keydown", async e => {
  if (e.key === "Escape") {
    const openModal = _modalStack[_modalStack.length - 1] || document.querySelector(".modal.show");
    if (!openModal) return;
    if (openModal.dataset.dirty === "true") {
      const seguro = await confirmModal(
        "Descartar cambios",
        "Hay cambios sin guardar en este formulario. ¿Deseas descartarlos?",
        "Descartar", "btn-danger"
      );
      if (!seguro) return;
      openModal.dataset.dirty = "false";
    }
    $(openModal).modal("hide");
    return;
  }

  if ((e.ctrlKey || e.metaKey) && (e.key === "s" || e.key === "S")) {
    // OA-175/OR-222: antes se hacía preventDefault() incondicional, así que
    // Ctrl+S en una pantalla sin modal abierto no guardaba nada -ni el modal
    // ni la página- sin ningún aviso. Ahora sólo se intercepta si hay algo
    // que guardar de verdad.
    const openModal = document.querySelector(".modal.show:not(#ds-confirm-modal):not(#ds-prompt-modal):not(#ds-link-modal):not(#ds-shortcuts-modal)");
    if (!openModal) return;
    const saveBtn = openModal.querySelector(".btn-save-modal, [data-save-modal]");
    if (!saveBtn || saveBtn.disabled) return;
    e.preventDefault();
    saveBtn.click();
    return;
  }

  if (e.key === "?" && !_isTypingTarget(e.target)) {
    e.preventDefault();
    _asegurarModalesUI();
    $(document.getElementById("ds-shortcuts-modal")).modal("show");
    return;
  }

  if (e.key === "/" && !_isTypingTarget(e.target)) {
    const search = document.querySelector(
      '[data-admin-search]:not([disabled]), input[type="search"]:not([disabled])'
    );
    if (search) { e.preventDefault(); search.focus(); search.select?.(); }
    return;
  }

  if ((e.key === "n" || e.key === "N") && !_isTypingTarget(e.target) && !e.ctrlKey && !e.metaKey && !e.altKey) {
    // No hay un único "nuevo registro" común a todas las páginas: se avisa con
    // un evento propio y cada página decide si tiene algo que abrir.
    const evt = new CustomEvent("admin-ui:shortcut-new", { cancelable: true });
    document.dispatchEvent(evt);
    return;
  }

  if (/^[1-9]$/.test(e.key) && !_isTypingTarget(e.target) && !e.ctrlKey && !e.metaKey && !e.altKey) {
    const bar = document.querySelector(".ds-admin-tabs");
    if (!bar) return;
    const links = [...bar.querySelectorAll(".nav-link")];
    const target = links[Number(e.key) - 1];
    if (target) { e.preventDefault(); target.focus(); target.click(); }
  }
});

function _isTypingTarget(el) {
  if (!el) return false;
  const tag = el.tagName;
  return tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT" || el.isContentEditable;
}

document.addEventListener("click", e => {
  if (e.target.closest(".ds-sc-close")) {
    $(document.getElementById("ds-shortcuts-modal")).modal("hide");
  }
});

// ─── Advertencia de sesión por expirar ────────────────────────────────────────
(function initSessionWarning() {
  const SESSION_TTL = 12 * 60 * 60 * 1000; // 12h
  const WARN_BEFORE  = 10 * 60 * 1000;     // warn 10 min before
  let _warned = false;
  let _countdownIv = null;

  function _stopCountdown() {
    if (_countdownIv) { clearInterval(_countdownIv); _countdownIv = null; }
  }

  function checkSession() {
    try {
      const raw = localStorage.getItem("archive_session") || localStorage.getItem("rrhh_session");
      if (!raw) return;
      const { ts } = JSON.parse(raw);
      const remaining = SESSION_TTL - (Date.now() - ts);
      if (remaining <= 0) return; // already handled by app.js
      if (remaining <= WARN_BEFORE && !_warned) {
        _warned = true;
        const mins = Math.ceil(remaining / 60000);
        showToast(`⚠️ Tu sesión expirará en ${mins} minutos. Guarda tu trabajo.`, "warning");
        // Banner persistente
        const banner = document.createElement("div");
        banner.id = "session-warning-banner";
        banner.style.cssText = "position:fixed;bottom:0;left:0;right:0;background:#fff3cd;color:#856404;border-top:2px solid #ffc107;padding:8px 16px;text-align:center;z-index:9999;font-size:0.85rem;display:flex;align-items:center;justify-content:center;gap:12px;";
        banner.innerHTML = `<i class="fas fa-clock"></i> Sesión expira en <strong id="session-countdown">${mins}:00</strong> min &nbsp;
          <button class="btn btn-warning btn-sm" onclick="extendSession()"><i class="fas fa-redo mr-1"></i>Extender sesión</button>
          <button class="btn btn-link btn-sm p-0" id="session-warning-close" aria-label="Cerrar aviso">✕</button>`;
        document.body.appendChild(banner);
        banner.querySelector("#session-warning-close").addEventListener("click", () => {
          // OR-295: cerrar el banner debe detener también el temporizador; si
          // no, sigue corriendo cada segundo contra un nodo que ya no existe.
          _stopCountdown();
          banner.remove();
        });

        // Countdown
        const countdownEl = () => document.getElementById("session-countdown");
        _countdownIv = setInterval(() => {
          const rem2 = SESSION_TTL - (Date.now() - ts);
          if (rem2 <= 0) { _stopCountdown(); banner.remove(); return; }
          const m = Math.floor(rem2 / 60000).toString().padStart(2, "0");
          const s = Math.floor((rem2 % 60000) / 1000).toString().padStart(2, "0");
          if (countdownEl()) countdownEl().textContent = `${m}:${s}`;
          else _stopCountdown();
        }, 1000);
      }
    } catch {}
  }

  // Check each 30s, y también al volver el foco a la pestaña: si el equipo
  // estuvo suspendido durante la ventana de aviso, antes no se avisaba nunca
  // (OA-045).
  setInterval(checkSession, 30_000);
  setTimeout(checkSession, 5_000);
  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "visible") checkSession();
  });
})();

function extendSession() {
  // OA-044: antes sólo reescribía la marca de tiempo local; la cookie HMAC del
  // servidor no se renovaba, así que el botón mentía si la sesión real ya
  // había caducado. Ahora intenta renovar la cookie de verdad.
  (async () => {
    try {
      const resp = await fetch(`${typeof API_BASE !== "undefined" ? API_BASE : ""}/api/auth/restore`, {
        method: "POST",
        credentials: "include",
      });
      if (!resp.ok) throw new Error("No se pudo renovar la sesión");
      const raw = localStorage.getItem("archive_session") || localStorage.getItem("rrhh_session");
      const key = localStorage.getItem("archive_session") ? "archive_session" : "rrhh_session";
      if (raw) {
        const saved = JSON.parse(raw);
        saved.ts = Date.now();
        localStorage.setItem(key, JSON.stringify(saved));
      }
      document.getElementById("session-warning-banner")?.remove();
      showToast("Sesión extendida por 12 horas.", "success");
    } catch (e) {
      showToast("No se pudo renovar la sesión. Guarda tu trabajo e inicia sesión de nuevo.", "error");
    }
  })();
}

// highlightTerms está en app-core.js

// ─── Inline quick-status change (dropdown en tabla) ───────────────────────────
function openQuickStatusMenu(btn, docId, currentStatus, modulo) {
  // Cerrar cualquier menú previo
  document.querySelectorAll(".ds-quick-status-menu").forEach(m => m.remove());

  const options = [
    { val: "aprobado",  label: '<i class="fas fa-check mr-1"></i>Aprobado',           cls: "text-success" },
    { val: "revision",  label: '<i class="fas fa-clock mr-1"></i>Pendiente revisión', cls: "text-warning" },
    { val: "draft",     label: '<i class="fas fa-pencil-alt mr-1"></i>Borrador',      cls: "text-secondary" },
    { val: "rechazado", label: '<i class="fas fa-times mr-1"></i>Rechazado',          cls: "text-danger" },
  ];

  const menu = document.createElement("div");
  menu.className = "ds-quick-status-menu dropdown-menu show";
  menu.style.cssText = "position:absolute;z-index:9999;min-width:140px;padding:4px;font-size:0.82rem;box-shadow:0 4px 16px rgba(0,0,0,.18);";
  menu.innerHTML = options.map(o =>
    `<button class="dropdown-item ${o.cls} ${o.val === currentStatus ? 'font-weight-bold' : ''}" data-val="${o.val}">${o.label}</button>`
  ).join("");

  // Posición relativa al botón
  const rect = btn.getBoundingClientRect();
  menu.style.left = `${rect.left + window.scrollX}px`;
  menu.style.top  = `${rect.bottom + window.scrollY + 2}px`;
  document.body.appendChild(menu);

  // OR-296: el listener de captura en document sólo se retiraba si el cierre
  // ocurría por clic fuera. Si se elegía una opción, `menu.remove()` dejaba el
  // listener vivo para siempre (uno más por cada apertura de la sesión).
  let _closed = false;
  const closeMenu = e => {
    if (e && menu.contains(e.target) && e.target !== btn) return;
    if (_closed) return;
    _closed = true;
    menu.remove();
    document.removeEventListener("click", closeMenu, true);
  };

  menu.querySelectorAll(".dropdown-item").forEach(item => {
    item.addEventListener("click", async () => {
      const newStatus = item.dataset.val;
      closeMenu();
      if (newStatus === currentStatus) return;
      try {
        await apiFetch(`${API_BASE}/api/admin/documento/${docId}/status?status=${newStatus}&modulo=${encodeURIComponent(modulo)}&requester=${encodeURIComponent(state.user.username)}`, {
          method: "PATCH"
        });
        showToast(`Estado cambiado a "${newStatus}".`, "success");
        loadMonitorTable();
      } catch (e) {
        showToast(e.message || "Error al cambiar el estado.", "error");
      }
    });
  });

  // Cerrar al click fuera
  setTimeout(() => document.addEventListener("click", closeMenu, true), 10);
}

// ─── Barra de progreso para operaciones largas ───────────────────────────────
// `pct` es opcional: sin él la barra queda indeterminada (rayada, como antes).
// Con un número 0-100 se vuelve determinada y `updateProgress` puede avanzarla
// -pensada para que admin-submit.js la enganche a `upload.onprogress` (OA-093).
// `onCancel`, si se pasa, muestra un botón «Cancelar» junto al rótulo.
function showProgress(containerId, label = "Procesando…", { pct = null, onCancel = null } = {}) {
  const el = document.getElementById(containerId);
  if (!el) return;
  hideProgress(containerId);
  const determinate = typeof pct === "number";
  const div = document.createElement("div");
  div.id = `_prog_${containerId}`;
  div.innerHTML = `
    <div class="d-flex align-items-center mb-2">
      <span class="text-muted small mr-2 _prog_label">${label}</span>
      ${determinate
        ? `<span class="text-muted small mr-2 _prog_pct">${Math.round(pct)}%</span>`
        : `<div class="spinner-border spinner-border-sm text-primary" role="status"></div>`}
      ${onCancel ? `<button type="button" class="btn btn-link btn-sm text-danger p-0 ml-auto _prog_cancel">Cancelar</button>` : ""}
    </div>
    <div class="progress" style="height:6px;">
      <div class="progress-bar ${determinate ? "" : "progress-bar-striped progress-bar-animated"} bg-primary _prog_bar"
           role="progressbar" aria-valuemin="0" aria-valuemax="100"
           ${determinate ? `aria-valuenow="${Math.round(pct)}"` : ""}
           style="width:${determinate ? Math.round(pct) : 100}%;"></div>
    </div>`;
  el.prepend(div);
  if (onCancel) {
    div.querySelector("._prog_cancel").addEventListener("click", () => onCancel());
  }
}

function updateProgress(containerId, pct, label = null) {
  const div = document.getElementById(`_prog_${containerId}`);
  if (!div) return;
  const bar = div.querySelector("._prog_bar");
  const pctEl = div.querySelector("._prog_pct");
  const labelEl = div.querySelector("._prog_label");
  if (bar) { bar.style.width = `${Math.round(pct)}%`; bar.setAttribute("aria-valuenow", Math.round(pct)); }
  if (pctEl) pctEl.textContent = `${Math.round(pct)}%`;
  if (label && labelEl) labelEl.textContent = label;
}

function hideProgress(containerId) {
  document.getElementById(`_prog_${containerId}`)?.remove();
}

// ─── Tooltip simple para elementos con data-tip ───────────────────────────────
document.addEventListener("mouseover", e => {
  const target = e.target.closest("[data-tip]");
  if (!target) return;
  let tip = document.getElementById("_ds_tip");
  if (!tip) {
    tip = document.createElement("div");
    tip.id = "_ds_tip";
    tip.style.cssText = "position:fixed;background:#333;color:#fff;padding:4px 8px;border-radius:4px;font-size:0.75rem;z-index:99999;pointer-events:none;max-width:200px;";
    document.body.appendChild(tip);
  }
  tip.textContent = target.dataset.tip;
  tip.style.display = "block";
  const move = ev => { tip.style.left = `${ev.clientX + 10}px`; tip.style.top = `${ev.clientY - 28}px`; };
  const leave = () => { tip.style.display = "none"; target.removeEventListener("mousemove", move); target.removeEventListener("mouseleave", leave); };
  target.addEventListener("mousemove", move);
  target.addEventListener("mouseleave", leave);
});

// ─── Barra de pestañas: desbordamiento y navegación ──────────────────────────
// La barra hace scroll horizontal cuando no caben las 9 pestañas. Sin señal
// visual el usuario no descubre las que quedan fuera, y al cambiar de pestaña
// con el teclado la seleccionada puede quedar fuera de vista.

function _syncTabOverflow() {
  document.querySelectorAll(".ds-admin-tabs").forEach(bar => {
    const header = bar.closest(".ds-admin-card-header");
    if (!header) return;
    const overflow = bar.scrollWidth - bar.clientWidth > 4;
    const atEnd    = bar.scrollLeft + bar.clientWidth >= bar.scrollWidth - 4;
    header.classList.toggle("ds-has-overflow", overflow && !atEnd);
  });
}

function _scrollActiveTabIntoView() {
  document.querySelectorAll(".ds-admin-tabs .nav-link.active").forEach(link => {
    link.scrollIntoView({ block: "nearest", inline: "nearest", behavior: "smooth" });
  });
}

// OA-178/OR-224: sincroniza `aria-selected` y el `tabindex` móvil (roving) con
// la pestaña que de verdad está activa, sea cual sea la página que la activó
// (clic, flecha o número). El estado de `active` sigue siendo de `admin.js`;
// esto sólo refleja ese estado en los atributos ARIA que un lector de
// pantalla necesita.
function _syncTabAria(bar) {
  const links = [...bar.querySelectorAll(".nav-link")];
  links.forEach(link => {
    const active = link.classList.contains("active");
    link.setAttribute("aria-selected", active ? "true" : "false");
    link.setAttribute("tabindex", active ? "0" : "-1");
  });
}

function _activateTab(link) {
  link.focus();
  link.click();
  const bar = link.closest(".ds-admin-tabs");
  if (bar) setTimeout(() => _syncTabAria(bar), 0);
}

// OA-177/OR-223: antes las flechas movían el foco Y activaban la pestaña
// (`next.focus(); next.click();`), así que recorrer las nueve con el teclado
// disparaba nueve cargas. El patrón ARIA correcto es "roving tabindex": la
// flecha sólo mueve el foco; Enter o Espacio activan la pestaña enfocada.
function _initTabKeyboardNav() {
  document.querySelectorAll(".ds-admin-tabs").forEach(bar => {
    _syncTabAria(bar);
    bar.addEventListener("keydown", e => {
      const links = [...bar.querySelectorAll(".nav-link")];
      const i = links.indexOf(document.activeElement);
      if (e.key === "ArrowRight" || e.key === "ArrowLeft") {
        if (i === -1) return;
        e.preventDefault();
        const next = links[(i + (e.key === "ArrowRight" ? 1 : -1) + links.length) % links.length];
        next.setAttribute("tabindex", "0");
        links.forEach(l => { if (l !== next) l.setAttribute("tabindex", "-1"); });
        next.focus();
        return;
      }
      if (e.key === "Home" && i !== -1) {
        e.preventDefault();
        links.forEach(l => l.setAttribute("tabindex", "-1"));
        links[0].setAttribute("tabindex", "0");
        links[0].focus();
        return;
      }
      if (e.key === "End" && i !== -1) {
        e.preventDefault();
        links.forEach(l => l.setAttribute("tabindex", "-1"));
        links[links.length - 1].setAttribute("tabindex", "0");
        links[links.length - 1].focus();
        return;
      }
      if ((e.key === "Enter" || e.key === " ") && i !== -1) {
        e.preventDefault();
        _activateTab(links[i]);
      }
    });
  });
}

document.addEventListener("DOMContentLoaded", () => {
  _syncTabOverflow();
  _initTabKeyboardNav();
  document.querySelectorAll(".ds-admin-tabs").forEach(bar => {
    bar.addEventListener("scroll", _syncTabOverflow, { passive: true });
    bar.addEventListener("click", () => setTimeout(() => { _scrollActiveTabIntoView(); _syncTabAria(bar); }, 0));
  });
});
window.addEventListener("resize", _syncTabOverflow);

// ─── Paneles idénticos entre módulos ─────────────────────────────────────────
// "Acceso" y "Auditoría" eran exactamente el mismo marcado en los dos paneles,
// salvo el sufijo del módulo. Los demás paneles difieren con motivo —Retención
// sólo muestra vencidos en Archivo, Papelera lleva dos tablas en RRHH— y se
// quedan en su HTML.
//
// Se inyectan al cargar, antes de que app.js enganche sus listeners: admin-ui.js
// se carga antes, así que su handler de DOMContentLoaded corre primero.

function _panelAcceso(suf, modulo) {
  // OR-204: los cuatro controles llevaban sólo `placeholder`, que un lector de
  // pantalla no trata como etiqueta (WCAG 3.3.2). Ahora cada uno tiene un
  // `<label>` real, visualmente oculto para no cambiar el diseño.
  return `
<div class="tab-pane fade" id="pane-admin-${suf}-users" role="tabpanel">
  <div class="card card-danger">
    <div class="card-header">
      <h3 class="card-title" id="ds-acceso-title-${suf}"><i class="fas fa-user-shield"></i> Control de Acceso</h3>
    </div>
    <div class="card-body p-4">
      <div id="admin_users_table-${suf}" class="table-responsive mb-4"></div>
      <hr>
      <h6 class="font-weight-bold mb-3" id="ds-nuevo-usuario-title-${suf}"><i class="fas fa-user-plus"></i> Registrar Nuevo Usuario</h6>
      <div class="row mb-3" role="group" aria-labelledby="ds-nuevo-usuario-title-${suf}">
        <div class="col-md-3 mb-2">
          <label class="sr-only" for="new_user_name-${suf}">Usuario</label>
          <input type="text" id="new_user_name-${suf}" class="form-control form-control-sm" placeholder="Usuario">
        </div>
        <div class="col-md-3 mb-2">
          <label class="sr-only" for="new_user_pass-${suf}">Contraseña</label>
          <input type="password" id="new_user_pass-${suf}" class="form-control form-control-sm" placeholder="Contraseña">
        </div>
        <div class="col-md-3 mb-2">
          <label class="sr-only" for="new_user_modulo-${suf}">Módulo</label>
          <select id="new_user_modulo-${suf}" class="form-control form-control-sm">
            <option value="${modulo}">${modulo}</option>
          </select>
        </div>
        <div class="col-md-3 mb-2">
          <label class="sr-only" for="new_user_rol-${suf}">Rol</label>
          <select id="new_user_rol-${suf}" class="form-control form-control-sm">
            <option value="Normal">Normal</option>
            <option value="Admin">Admin</option>
          </select>
        </div>
      </div>
      <button id="btn_add_user-${suf}" class="btn btn-outline-danger btn-sm" style="border-radius:8px;">
        <i class="fas fa-user-plus"></i> Crear Usuario
      </button>
    </div>
  </div>
</div>`;
}

function _panelAuditoria(suf) {
  return `
<div class="tab-pane fade" id="pane-admin-${suf}-audit" role="tabpanel">
  <div class="card card-secondary">
    <div class="card-header d-flex justify-content-between align-items-center flex-wrap">
      <h3 class="card-title"><i class="fas fa-history mr-2"></i>Registro de Auditoría</h3>
      <label class="sr-only" for="audit_search-${suf}">Buscar evento o usuario</label>
      <input type="text" id="audit_search-${suf}" class="form-control form-control-sm ds-audit-buscador" data-admin-search
             placeholder="Buscar evento o usuario...">
    </div>
    <div class="card-body p-0">
      <div class="table-responsive">
        <table class="table table-striped table-sm mb-0" style="font-size:0.82rem;">
          <thead class="bg-light">
            <tr>
              <th>Fecha y hora</th>
              <th class="ds-hide-sm">Usuario</th>
              <th>Evento</th>
              <th class="ds-hide-sm">Módulo</th>
              <th class="ds-hide-sm">Detalle</th>
              <th>Resultado</th>
            </tr>
          </thead>
          <tbody id="audit_table_body-${suf}"></tbody>
        </table>
      </div>
    </div>
    <div class="card-footer d-flex justify-content-between align-items-center">
      <small id="audit_summary-${suf}" class="text-muted"></small>
      <div>
        <button id="audit_prev-${suf}" class="btn btn-sm btn-outline-secondary mr-1" onclick="changeAuditPage(-1)"><i class="fas fa-chevron-left"></i></button>
        <span id="audit_page_info-${suf}" class="text-muted small"></span>
        <button id="audit_next-${suf}" class="btn btn-sm btn-outline-secondary ml-1" onclick="changeAuditPage(1)"><i class="fas fa-chevron-right"></i></button>
      </div>
    </div>
  </div>
</div>`;
}

function _inyectarPanelesComunes() {
  [["archivo", "Archivo"], ["rrhh", "RRHH"]].forEach(([suf, modulo]) => {
    const seccion = document.getElementById(`tab-admin-${suf}`);
    if (!seccion) return;
    const contenido = seccion.querySelector(".tab-content");
    if (!contenido) return;
    if (!document.getElementById(`pane-admin-${suf}-users`)) {
      contenido.insertAdjacentHTML("beforeend", _panelAcceso(suf, modulo));
    }
    if (!document.getElementById(`pane-admin-${suf}-audit`)) {
      contenido.insertAdjacentHTML("beforeend", _panelAuditoria(suf));
    }
  });
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", _inyectarPanelesComunes);
} else {
  _inyectarPanelesComunes();
}
