/**
 * Scanner Client — integración en el panel admin
 *
 * Incluir este script en admin_archivo.html / admin_rrhh.html para habilitar
 * la conexión con el Scanner Bridge local.
 *
 * Uso en HTML:
 *   <script src="/static/scanner-client.js"></script>
 *
 * Luego el botón "Modo Escáner" en la barra llama a `toggleScannerMode()`.
 * Los códigos recibidos van a `handleScannerCode(code)` que auto-rellena
 * el campo de búsqueda activo o la cédula en el formulario de ingreso.
 */

(function () {
  "use strict";

  const SCANNER_WS_URL = "ws://127.0.0.1:3737";
  const RECONNECT_DELAY_MS = 4000;
  const MAX_RECONNECT_ATTEMPTS = 5;

  let _ws = null;
  let _active = false;
  let _reconnectCount = 0;
  let _reconnectTimer = null;

  // ── estado visual ──────────────────────────────────────────────────────────
  function _setScannerBtnState(state) {
    const btn = document.getElementById("ds-scanner-btn");
    if (!btn) return;
    const states = {
      idle:        { cls: "btn-outline-secondary", icon: "fa-barcode",      label: "Escáner" },
      connecting:  { cls: "btn-warning",           icon: "fa-spinner fa-spin", label: "Conectando…" },
      connected:   { cls: "btn-success",           icon: "fa-barcode",      label: "Escáner activo" },
      error:       { cls: "btn-danger",            icon: "fa-exclamation-triangle", label: "Sin conexión" },
    };
    const s = states[state] || states.idle;
    btn.className = btn.className.replace(/\bbtn-\S+/g, "").trim() + " btn btn-sm " + s.cls;
    btn.innerHTML = `<i class="fas ${s.icon} mr-1"></i>${s.label}`;
  }

  // ── conexión WebSocket ─────────────────────────────────────────────────────
  function _connect() {
    if (_ws && (_ws.readyState === WebSocket.CONNECTING || _ws.readyState === WebSocket.OPEN)) return;

    _setScannerBtnState("connecting");
    _ws = new WebSocket(SCANNER_WS_URL);

    _ws.onopen = () => {
      _reconnectCount = 0;
      _setScannerBtnState("connected");
      if (typeof showToast === "function") showToast("Escáner conectado", "success");
    };

    _ws.onmessage = evt => {
      try {
        const msg = JSON.parse(evt.data);
        if (msg.type === "scan" && msg.code) {
          handleScannerCode(msg.code);
        }
      } catch { /* datos no-JSON ignorados */ }
    };

    _ws.onclose = () => {
      if (!_active) { _setScannerBtnState("idle"); return; }
      _setScannerBtnState("error");
      if (_reconnectCount < MAX_RECONNECT_ATTEMPTS) {
        _reconnectCount++;
        _reconnectTimer = setTimeout(_connect, RECONNECT_DELAY_MS);
      } else {
        if (typeof showToast === "function")
          showToast("Escáner desconectado. Verifica que el Scanner Bridge esté corriendo.", "warning");
      }
    };

    _ws.onerror = () => {
      // onclose siempre se dispara después de onerror, el manejo está allí
    };
  }

  function _disconnect() {
    clearTimeout(_reconnectTimer);
    if (_ws) { _ws.close(); _ws = null; }
    _setScannerBtnState("idle");
  }

  // ── API pública ────────────────────────────────────────────────────────────
  window.toggleScannerMode = function () {
    _active = !_active;
    if (_active) {
      _reconnectCount = 0;
      _connect();
    } else {
      _disconnect();
    }
  };

  /**
   * Manejador del código recibido.
   * Lógica: intenta rellenar el campo activo más relevante.
   *   - Si hay un modal de edición abierto → va al campo cédula/RIF del modal.
   *   - Si la pestaña activa es "new" (ingreso) → va al campo cédula del formulario.
   *   - Si no → va a la barra de búsqueda principal del tab activo.
   */
  window.handleScannerCode = function (code) {
    // Mostrar indicador visual breve
    _flashScannerIndicator(code);

    // 1. Modal de edición abierto (RRHH — campo cédula)
    const modalCedula = document.getElementById("edit-cedula");
    if (modalCedula && document.querySelector(".modal.show")) {
      modalCedula.value = code;
      modalCedula.dispatchEvent(new Event("input", { bubbles: true }));
      return;
    }

    // 2. Formulario de nuevo ingreso activo
    const newPane = document.querySelector(".tab-pane.active[id*='-new']");
    if (newPane) {
      const cedulaField = newPane.querySelector("[id*='cedula'], [id*='rif'], [name='cedula']");
      if (cedulaField) {
        cedulaField.value = code;
        cedulaField.dispatchEvent(new Event("input", { bubbles: true }));
        cedulaField.focus();
        return;
      }
    }

    // 3. Barra de búsqueda/monitor activa
    const searchBar = document.querySelector("#admin_search_input-archivo, #admin_search_input-rrhh");
    if (searchBar) {
      searchBar.value = code;
      searchBar.dispatchEvent(new Event("input", { bubbles: true }));
      // Disparar búsqueda si existe la función
      const suf = searchBar.id.includes("rrhh") ? "rrhh" : "archivo";
      if (typeof loadMonitorTable === "function") loadMonitorTable(suf, 1);
    }
  };

  // ── indicador visual de scan ───────────────────────────────────────────────
  function _flashScannerIndicator(code) {
    let indicator = document.getElementById("ds-scan-flash");
    if (!indicator) {
      indicator = document.createElement("div");
      indicator.id = "ds-scan-flash";
      indicator.style.cssText = [
        "position:fixed", "bottom:20px", "right:20px", "z-index:9999",
        "background:#28a745", "color:#fff", "padding:8px 14px",
        "border-radius:8px", "font-size:0.85rem", "font-weight:600",
        "box-shadow:0 4px 12px rgba(0,0,0,0.2)",
        "transition:opacity 0.3s", "pointer-events:none",
      ].join(";");
      document.body.appendChild(indicator);
    }
    indicator.innerHTML = `<i class="fas fa-barcode mr-1"></i>${code}`;
    indicator.style.opacity = "1";
    clearTimeout(indicator._hideTimer);
    indicator._hideTimer = setTimeout(() => { indicator.style.opacity = "0"; }, 2000);
  }

  // ── inyectar botón en la barra de navegación ───────────────────────────────
  document.addEventListener("DOMContentLoaded", () => {
    // Buscar un lugar apropiado en la barra de herramientas del admin
    const toolbar = document.querySelector(".navbar-nav.ml-auto, .ds-nav-user")?.parentElement;
    if (!toolbar || document.getElementById("ds-scanner-btn")) return;

    const li = document.createElement("li");
    li.className = "nav-item mr-2";
    li.innerHTML = `
      <button id="ds-scanner-btn" class="btn btn-sm btn-outline-secondary"
        style="margin-top:3px;" title="Conectar con Scanner Bridge local (ws://127.0.0.1:3737)"
        onclick="toggleScannerMode()">
        <i class="fas fa-barcode mr-1"></i>Escáner
      </button>`;
    const navUser = document.querySelector(".ds-nav-user");
    if (navUser) navUser.before(li);
    else toolbar.prepend(li);
  });
})();
