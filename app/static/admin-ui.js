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
<div class="modal fade" id="ds-confirm-modal" tabindex="-1" data-backdrop="static" data-keyboard="false">
  <div class="modal-dialog modal-sm modal-dialog-centered">
    <div class="modal-content border-0 shadow-lg" style="border-radius:12px;">
      <div class="modal-header border-0 pb-0">
        <h6 class="modal-title font-weight-bold ds-cm-title">Confirmar</h6>
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
<div class="modal fade" id="ds-prompt-modal" tabindex="-1" data-backdrop="static" data-keyboard="false">
  <div class="modal-dialog modal-sm modal-dialog-centered">
    <div class="modal-content border-0 shadow-lg" style="border-radius:12px;">
      <div class="modal-header border-0 pb-0">
        <h6 class="modal-title font-weight-bold ds-pm-title">Ingrese un valor</h6>
      </div>
      <div class="modal-body pt-2 pb-2">
        <label class="text-muted small ds-pm-label mb-1"></label>
        <input type="text" class="form-control ds-pm-input" autocomplete="off">
      </div>
      <div class="modal-footer border-0 pt-1">
        <button type="button" class="btn btn-secondary btn-sm ds-pm-cancel">Cancelar</button>
        <button type="button" class="btn btn-primary btn-sm ds-pm-ok">Aceptar</button>
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

// ─── Modal de confirmación (reemplaza window.confirm) ────────────────────────
let _confirmResolve = null;

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
    btn.className    = `btn ${btnClass} ds-cm-ok`;
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

function promptModal(title, label, defaultVal = "", placeholder = "") {
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
    $(el).modal("show");
    setTimeout(() => inp.focus(), 300);
  });
}

document.addEventListener("click", e => {
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
document.addEventListener("keydown", e => {
  const pm = document.getElementById("ds-prompt-modal");
  if (pm && pm.classList.contains("show") && e.key === "Enter") {
    e.preventDefault();
    pm.querySelector(".ds-pm-ok")?.click();
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

// ─── Atajos de teclado globales (Ctrl+S = guardar modal abierto; Esc = cerrar) ──
document.addEventListener("keydown", e => {
  if (e.key === "Escape") {
    // Cerrar el primer modal abierto
    const openModal = document.querySelector(".modal.show");
    if (openModal && !document.getElementById("ds-confirm-modal")?.classList.contains("show")
                  && !document.getElementById("ds-prompt-modal")?.classList.contains("show")) {
      $(openModal).modal("hide");
    }
  }
  if ((e.ctrlKey || e.metaKey) && e.key === "s") {
    e.preventDefault();
    // Buscar botón guardar en el modal abierto
    const openModal = document.querySelector(".modal.show:not(#ds-confirm-modal):not(#ds-prompt-modal)");
    if (openModal) {
      const saveBtn = openModal.querySelector(".btn-save-modal, [data-save-modal]");
      if (saveBtn) saveBtn.click();
    }
  }
});

// ─── Advertencia de sesión por expirar ────────────────────────────────────────
(function initSessionWarning() {
  const SESSION_TTL = 12 * 60 * 60 * 1000; // 12h
  const WARN_BEFORE  = 10 * 60 * 1000;     // warn 10 min before
  let _warned = false;

  function checkSession() {
    try {
      const raw = localStorage.getItem("archive_session");
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
          <button class="btn btn-link btn-sm p-0" onclick="this.closest('#session-warning-banner').remove()">✕</button>`;
        document.body.appendChild(banner);

        // Countdown
        const countdownEl = () => document.getElementById("session-countdown");
        const iv = setInterval(() => {
          const rem2 = SESSION_TTL - (Date.now() - ts);
          if (rem2 <= 0) { clearInterval(iv); banner.remove(); return; }
          const m = Math.floor(rem2 / 60000).toString().padStart(2, "0");
          const s = Math.floor((rem2 % 60000) / 1000).toString().padStart(2, "0");
          if (countdownEl()) countdownEl().textContent = `${m}:${s}`;
        }, 1000);
      }
    } catch {}
  }

  // Check each 30s
  setInterval(checkSession, 30_000);
  setTimeout(checkSession, 5_000);
})();

function extendSession() {
  // Refresh the ts in localStorage and remove warning
  try {
    const raw = localStorage.getItem("archive_session");
    if (!raw) return;
    const saved = JSON.parse(raw);
    saved.ts = Date.now();
    localStorage.setItem("archive_session", JSON.stringify(saved));
    document.getElementById("session-warning-banner")?.remove();
    showToast("Sesión extendida por 12 horas.", "success");
  } catch {}
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

  menu.querySelectorAll(".dropdown-item").forEach(item => {
    item.addEventListener("click", async () => {
      const newStatus = item.dataset.val;
      menu.remove();
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
  const closeMenu = e => { if (!menu.contains(e.target) && e.target !== btn) { menu.remove(); document.removeEventListener("click", closeMenu, true); } };
  setTimeout(() => document.addEventListener("click", closeMenu, true), 10);
}

// ─── Barra de progreso indeterminada para operaciones largas ──────────────────
function showProgress(containerId, label = "Procesando…") {
  const el = document.getElementById(containerId);
  if (!el) return;
  const div = document.createElement("div");
  div.id = `_prog_${containerId}`;
  div.innerHTML = `
    <div class="d-flex align-items-center mb-2">
      <span class="text-muted small mr-2">${label}</span>
      <div class="spinner-border spinner-border-sm text-primary" role="status"></div>
    </div>
    <div class="progress" style="height:6px;">
      <div class="progress-bar progress-bar-striped progress-bar-animated bg-primary" style="width:100%;"></div>
    </div>`;
  el.prepend(div);
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

// Flechas izquierda/derecha para moverse entre pestañas, como en un tablist real.
function _initTabKeyboardNav() {
  document.querySelectorAll(".ds-admin-tabs").forEach(bar => {
    bar.addEventListener("keydown", e => {
      if (e.key !== "ArrowRight" && e.key !== "ArrowLeft") return;
      const links = [...bar.querySelectorAll(".nav-link")];
      const i = links.indexOf(document.activeElement);
      if (i === -1) return;
      e.preventDefault();
      const next = links[(i + (e.key === "ArrowRight" ? 1 : -1) + links.length) % links.length];
      next.focus();
      next.click();
    });
  });
}

document.addEventListener("DOMContentLoaded", () => {
  _syncTabOverflow();
  _initTabKeyboardNav();
  document.querySelectorAll(".ds-admin-tabs").forEach(bar => {
    bar.addEventListener("scroll", _syncTabOverflow, { passive: true });
    bar.addEventListener("click", () => setTimeout(_scrollActiveTabIntoView, 0));
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
  return `
<div class="tab-pane fade" id="pane-admin-${suf}-users" role="tabpanel">
  <div class="card card-danger">
    <div class="card-header">
      <h3 class="card-title"><i class="fas fa-user-shield"></i> Control de Acceso</h3>
    </div>
    <div class="card-body p-4">
      <div id="admin_users_table-${suf}" class="table-responsive mb-4"></div>
      <hr>
      <h6 class="font-weight-bold mb-3"><i class="fas fa-user-plus"></i> Registrar Nuevo Usuario</h6>
      <div class="row mb-3">
        <div class="col-md-3 mb-2">
          <input type="text" id="new_user_name-${suf}" class="form-control form-control-sm" placeholder="Usuario">
        </div>
        <div class="col-md-3 mb-2">
          <input type="password" id="new_user_pass-${suf}" class="form-control form-control-sm" placeholder="Contraseña">
        </div>
        <div class="col-md-3 mb-2">
          <select id="new_user_modulo-${suf}" class="form-control form-control-sm">
            <option value="${modulo}">${modulo}</option>
          </select>
        </div>
        <div class="col-md-3 mb-2">
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
      <input type="text" id="audit_search-${suf}" class="form-control form-control-sm ds-audit-buscador"
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
