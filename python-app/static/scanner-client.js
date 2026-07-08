/**
 * Scanner Client — Archivo Digital UCV
 *
 * Soporta dos modos:
 *   - ws  : conecta con el Scanner Bridge local (Node.js) via WebSocket
 *   - camera : usa la cámara del dispositivo + BarcodeDetector API
 *
 * La URL del bridge es configurable desde el panel de ajustes (no hardcoded).
 * Persiste en localStorage: ds_scanner_url y ds_scanner_mode.
 */
(function () {
  "use strict";

  const LS_URL_KEY     = "ds_scanner_url";
  const LS_MODE_KEY    = "ds_scanner_mode";
  const DEFAULT_WS_URL = "ws://127.0.0.1:3737";
  const MAX_BACKOFF_MS = 30000;

  let _wsUrl    = localStorage.getItem(LS_URL_KEY)  || DEFAULT_WS_URL;
  let _mode     = localStorage.getItem(LS_MODE_KEY) || "ws";
  let _ws       = null;
  let _active   = false;
  let _backoff  = 1000;
  let _backoffTimer = null;
  let _cameraStream   = null;
  let _cameraInterval = null;
  let _detector       = null;

  // ── helpers ─────────────────────────────────────────────────────────────────
  function _toast(msg, type) {
    if (typeof showToast === "function") showToast(msg, type);
  }

  function _restBase(url) {
    return (url || _wsUrl).replace(/^ws/, "http").replace(/\/+$/, "");
  }

  // ── estado visual botón ──────────────────────────────────────────────────────
  function _setBtnState(st) {
    const btn = document.getElementById("ds-scanner-btn");
    if (!btn) return;
    const s = {
      idle:       ["btn-outline-secondary", "fa-barcode",             "Escáner"],
      connecting: ["btn-warning",           "fa-spinner fa-spin",    "Conectando…"],
      connected:  ["btn-success",           "fa-barcode",            "Escáner ●"],
      camera:     ["btn-info",              "fa-camera",             "Cámara activa"],
      error:      ["btn-danger",            "fa-exclamation-triangle","Sin señal"],
    }[st] || ["btn-outline-secondary", "fa-barcode", "Escáner"];
    btn.className = btn.className.replace(/btn-\S+/g, "").replace(/\s+/g, " ").trim()
      + " btn btn-sm " + s[0];
    const ico = btn.querySelector(".ds-scn-icon");
    const lbl = btn.querySelector(".ds-scn-label");
    if (ico) ico.className = "fas " + s[1] + " mr-1 ds-scn-icon";
    if (lbl) lbl.textContent = s[2];
  }

  // ── WS con backoff exponencial ───────────────────────────────────────────────
  function _connectWS() {
    if (_ws && (_ws.readyState === 0 || _ws.readyState === 1)) return;
    _setBtnState("connecting");
    try { _ws = new WebSocket(_wsUrl); } catch (e) {
      _setBtnState("error"); _scheduleReconnect(); return;
    }
    _ws.onopen = () => {
      _backoff = 1000;
      _setBtnState("connected");
      _toast("Escáner conectado", "success");
    };
    _ws.onmessage = ev => {
      try {
        const m = JSON.parse(ev.data);
        if (m.type === "scan" && m.code) handleScannerCode(m.code);
      } catch { /* ignorar */ }
    };
    _ws.onclose = () => {
      if (!_active) { _setBtnState("idle"); return; }
      _setBtnState("error"); _scheduleReconnect();
    };
    _ws.onerror = () => { /* onclose sigue */ };
  }

  function _scheduleReconnect() {
    clearTimeout(_backoffTimer);
    if (!_active) return;
    _backoffTimer = setTimeout(() => { if (_active) _connectWS(); }, _backoff);
    _backoff = Math.min(_backoff * 2, MAX_BACKOFF_MS);
  }

  function _disconnectWS() {
    clearTimeout(_backoffTimer);
    if (_ws) { try { _ws.close(); } catch {} _ws = null; }
    _backoff = 1000;
    _setBtnState("idle");
  }

  // ── modo cámara ──────────────────────────────────────────────────────────────
  function _hasDetector() { return typeof BarcodeDetector !== "undefined"; }

  async function _startCamera() {
    if (!_hasDetector()) {
      _toast("BarcodeDetector no soportado en este navegador. Usa Chrome/Edge.", "warning");
      _active = false; _setBtnState("idle"); return;
    }
    try {
      _detector = new BarcodeDetector({
        formats: ["qr_code", "code_128", "ean_13", "code_39", "pdf417", "data_matrix", "ean_8"]
      });
      _cameraStream = await navigator.mediaDevices.getUserMedia({
        video: { facingMode: { ideal: "environment" }, width: { ideal: 1280 } }
      });
      _buildCameraOverlay(_cameraStream);
      _setBtnState("camera");
      _toast("Cámara activa — apunta al código", "info");
    } catch (e) {
      _toast("No se pudo acceder a la cámara: " + e.message, "error");
      _active = false; _setBtnState("idle");
    }
  }

  function _stopCamera() {
    clearInterval(_cameraInterval); _cameraInterval = null;
    if (_cameraStream) { _cameraStream.getTracks().forEach(t => t.stop()); _cameraStream = null; }
    document.getElementById("ds-camera-overlay")?.remove();
    _setBtnState("idle");
  }

  function _buildCameraOverlay(stream) {
    document.getElementById("ds-camera-overlay")?.remove();
    const ov = document.createElement("div");
    ov.id = "ds-camera-overlay";
    Object.assign(ov.style, {
      position: "fixed", bottom: "70px", right: "16px", zIndex: "9990",
      background: "#000", borderRadius: "12px", overflow: "hidden",
      boxShadow: "0 8px 32px rgba(0,0,0,0.5)",
      width: "260px", height: "195px", cursor: "move", userSelect: "none"
    });
    const vid = document.createElement("video");
    vid.autoplay = vid.playsInline = vid.muted = true;
    Object.assign(vid.style, { width: "100%", height: "100%", objectFit: "cover", display: "block" });
    vid.srcObject = stream;

    const closeBtn = document.createElement("button");
    closeBtn.innerHTML = "&times;";
    Object.assign(closeBtn.style, {
      position: "absolute", top: "6px", right: "6px",
      background: "rgba(0,0,0,0.6)", color: "#fff", border: "none",
      borderRadius: "50%", width: "26px", height: "26px",
      cursor: "pointer", fontSize: "14px", lineHeight: "1",
      display: "flex", alignItems: "center", justifyContent: "center"
    });
    closeBtn.onclick = () => { _active = false; _stopCamera(); };

    const scanLine = document.createElement("div");
    Object.assign(scanLine.style, {
      position: "absolute", top: "50%", left: "8%", right: "8%",
      height: "2px", background: "rgba(46,200,100,0.9)",
      boxShadow: "0 0 8px rgba(46,200,100,0.8)",
      animation: "ds-scn-anim 1.6s ease-in-out infinite"
    });

    const label = document.createElement("div");
    Object.assign(label.style, {
      position: "absolute", bottom: "0", left: "0", right: "0",
      background: "rgba(0,0,0,0.5)", color: "#fff",
      fontSize: "0.7rem", padding: "3px 8px", textAlign: "center"
    });
    label.textContent = "Apunta el código al centro";

    ov.append(vid, closeBtn, scanLine, label);
    document.body.appendChild(ov);

    if (!document.getElementById("ds-scn-styles")) {
      const s = document.createElement("style");
      s.id = "ds-scn-styles";
      s.textContent = "@keyframes ds-scn-anim{0%,100%{top:20%}50%{top:80%}}";
      document.head.appendChild(s);
    }

    // Drag
    let drag = false, ox = 0, oy = 0;
    ov.addEventListener("mousedown", e => {
      if (e.target === closeBtn) return;
      drag = true; ox = e.clientX - ov.offsetLeft; oy = e.clientY - ov.offsetTop;
    });
    document.addEventListener("mousemove", e => {
      if (!drag) return;
      ov.style.left = (e.clientX - ox) + "px"; ov.style.top = (e.clientY - oy) + "px";
      ov.style.right = ov.style.bottom = "auto";
    });
    document.addEventListener("mouseup", () => { drag = false; });

    // Scan loop
    let _lastCode = "", _lastTs = 0;
    _cameraInterval = setInterval(async () => {
      if (!_detector || vid.readyState < 2) return;
      try {
        const codes = await _detector.detect(vid);
        if (!codes.length) return;
        const code = codes[0].rawValue;
        if (code === _lastCode && Date.now() - _lastTs < 2000) return;
        _lastCode = code; _lastTs = Date.now();
        handleScannerCode(code);
      } catch { /* ignore */ }
    }, 250);
  }

  // ── API pública ──────────────────────────────────────────────────────────────
  window.toggleScannerMode = function () {
    _active = !_active;
    if (_active) {
      _backoff = 1000;
      if (_mode === "camera") _startCamera(); else _connectWS();
    } else {
      if (_mode === "camera") _stopCamera(); else _disconnectWS();
    }
  };

  window.handleScannerCode = function (code) {
    _flashCode(code);

    // 1. Modal de edición RRHH — campo cédula
    const modalCedula = document.getElementById("edit-cedula");
    if (modalCedula && document.querySelector(".modal.show")) {
      modalCedula.value = code;
      modalCedula.dispatchEvent(new Event("input", { bubbles: true }));
      return;
    }

    // 2. Pane de nuevo ingreso activo — cualquier campo cédula/RIF
    const newPane = document.querySelector(".tab-pane.active[id*='-new']");
    if (newPane) {
      const f = newPane.querySelector("[id*='cedula'],[id*='rif'],[name='cedula']");
      if (f) { f.value = code; f.dispatchEvent(new Event("input", { bubbles: true })); f.focus(); return; }
    }

    // 3. Barra de búsqueda del monitor
    const bar = document.querySelector("#admin_search_input-archivo,#admin_search_input-rrhh");
    if (bar) {
      bar.value = code;
      bar.dispatchEvent(new Event("input", { bubbles: true }));
      const suf = bar.id.includes("rrhh") ? "rrhh" : "archivo";
      if (typeof loadMonitorTable === "function") loadMonitorTable(suf, 1);
    }
  };

  // ── panel de configuración ───────────────────────────────────────────────────
  window.openScannerConfig = function () {
    document.getElementById("ds-scn-cfg-panel")?.remove();
    const hasCamera = _hasDetector();
    const panel = document.createElement("div");
    panel.id = "ds-scn-cfg-panel";
    Object.assign(panel.style, {
      position: "fixed", top: "54px", right: "8px", zIndex: "9995",
      width: "300px", background: "#fff", border: "1px solid #d1dae6",
      borderRadius: "10px", boxShadow: "0 8px 24px rgba(0,0,0,0.16)",
      padding: "1rem", fontSize: "0.82rem", color: "#1a2332"
    });
    panel.innerHTML = `
      <div style="display:flex;justify-content:space-between;align-items:center;margin-bottom:.75rem;">
        <strong><i class="fas fa-barcode mr-1"></i>Configuraci&#243;n del Esc&#225;ner</strong>
        <button onclick="document.getElementById('ds-scn-cfg-panel')?.remove()"
          style="background:none;border:none;font-size:1.1rem;cursor:pointer;line-height:1;">&#x2715;</button>
      </div>
      <div style="margin-bottom:.55rem;">
        <label style="font-weight:600;display:block;margin-bottom:.2rem;">Modo de entrada</label>
        <select id="ds-scn-mode" style="width:100%;padding:.3rem .4rem;border:1px solid #ced4da;border-radius:4px;font-size:.82rem;">
          <option value="ws" ${_mode==="ws"?"selected":""}>&#128299; WebSocket (esc&#225;ner f&#237;sico / red)</option>
          <option value="camera" ${_mode==="camera"?"selected":""}${!hasCamera?" disabled":""}
            >&#128247; C&#225;mara del dispositivo${!hasCamera?" (Chrome/Edge)":""}</option>
        </select>
      </div>
      <div id="ds-scn-ws-sec" style="${_mode==="camera"?"display:none":""}margin-bottom:.55rem;">
        <label style="font-weight:600;display:block;margin-bottom:.2rem;">URL del Scanner Bridge</label>
        <div style="display:flex;gap:.35rem;">
          <input id="ds-scn-url" type="text" value="${_wsUrl}"
            placeholder="ws://192.168.1.X:3737"
            style="flex:1;padding:.3rem .45rem;border:1px solid #ced4da;border-radius:4px;font-size:.78rem;font-family:monospace;">
          <button onclick="_scnTest()" title="Probar conexión"
            style="padding:.3rem .55rem;background:#17a2b8;color:#fff;border:none;border-radius:4px;cursor:pointer;">
            <i class="fas fa-plug" style="font-size:.75rem;"></i>
          </button>
        </div>
        <div style="color:#888;font-size:.7rem;margin-top:.2rem;">Puede ser otra PC de la red local</div>
        <button onclick="_scnTestScan()"
          style="margin-top:.45rem;width:100%;padding:.3rem;background:#6c757d;color:#fff;border:none;border-radius:4px;cursor:pointer;font-size:.78rem;">
          <i class="fas fa-vial mr-1"></i>Env&#237;ar escaneo de prueba al bridge
        </button>
      </div>
      <div id="ds-scn-status" style="min-height:1.4em;font-size:.75rem;color:#6c757d;margin-bottom:.55rem;"></div>
      <button onclick="_scnSave()"
        style="width:100%;padding:.35rem;background:#2b4e72;color:#fff;border:none;border-radius:4px;cursor:pointer;font-weight:700;font-size:.82rem;">
        Guardar y aplicar
      </button>`;
    document.body.appendChild(panel);

    document.getElementById("ds-scn-mode").onchange = e => {
      const s = document.getElementById("ds-scn-ws-sec");
      if (s) s.style.display = e.target.value === "camera" ? "none" : "";
    };

    setTimeout(() => {
      document.addEventListener("click", function _h(e) {
        if (!panel.contains(e.target) && !e.target.closest("#ds-scn-cfg-btn")) {
          panel.remove(); document.removeEventListener("click", _h);
        }
      });
    }, 100);
  };

  window._scnSave = function () {
    const m = document.getElementById("ds-scn-mode")?.value;
    const u = document.getElementById("ds-scn-url")?.value?.trim();
    if (m) { _mode = m; localStorage.setItem(LS_MODE_KEY, _mode); }
    if (u) { _wsUrl = u; localStorage.setItem(LS_URL_KEY, _wsUrl); }
    if (_active) {
      if (_mode === "camera") { _disconnectWS(); _startCamera(); }
      else { _stopCamera(); _connectWS(); }
    }
    document.getElementById("ds-scn-cfg-panel")?.remove();
    _toast("Configuración guardada", "success");
  };

  window._scnTest = async function () {
    const u = document.getElementById("ds-scn-url")?.value?.trim();
    const st = document.getElementById("ds-scn-status");
    if (!st) return;
    st.innerHTML = '<i class="fas fa-spinner fa-spin mr-1"></i>Probando…';
    try {
      const r = await fetch(_restBase(u) + "/status", { signal: AbortSignal.timeout(3500) });
      const d = await r.json();
      st.innerHTML = `<span style="color:#28a745"><i class="fas fa-check mr-1"></i>Bridge activo — clientes: ${d.clients||0}, modo: ${d.mode||"?"}</span>`;
    } catch (e) {
      st.innerHTML = `<span style="color:#dc3545"><i class="fas fa-times mr-1"></i>${e.message}</span>`;
    }
  };

  window._scnTestScan = async function () {
    const u = document.getElementById("ds-scn-url")?.value?.trim();
    const st = document.getElementById("ds-scn-status");
    if (!st) return;
    st.innerHTML = '<i class="fas fa-spinner fa-spin mr-1"></i>Enviando…';
    try {
      const r = await fetch(_restBase(u) + "/test-scan", {
        method: "POST", headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ code: "TEST-" + Date.now() }),
        signal: AbortSignal.timeout(3500)
      });
      if (!r.ok) throw new Error("HTTP " + r.status);
      st.innerHTML = '<span style="color:#28a745"><i class="fas fa-check mr-1"></i>Escaneo de prueba enviado</span>';
    } catch (e) {
      st.innerHTML = `<span style="color:#dc3545"><i class="fas fa-times mr-1"></i>${e.message}</span>`;
    }
  };

  // ── flash visual ─────────────────────────────────────────────────────────────
  function _flashCode(code) {
    let el = document.getElementById("ds-scan-flash");
    if (!el) {
      el = document.createElement("div");
      el.id = "ds-scan-flash";
      Object.assign(el.style, {
        position: "fixed", bottom: "20px", right: "20px", zIndex: "9999",
        background: "#28a745", color: "#fff", padding: "8px 14px",
        borderRadius: "8px", fontSize: ".85rem", fontWeight: "600",
        boxShadow: "0 4px 12px rgba(0,0,0,.25)",
        transition: "opacity .35s", opacity: "0", pointerEvents: "none"
      });
      document.body.appendChild(el);
    }
    el.innerHTML = `<i class="fas fa-barcode mr-1"></i>${code}`;
    el.style.opacity = "1";
    clearTimeout(el._t);
    el._t = setTimeout(() => { el.style.opacity = "0"; }, 2500);
  }

  // ── inyectar botones en navbar ───────────────────────────────────────────────
  document.addEventListener("DOMContentLoaded", () => {
    const navUser = document.querySelector(".ds-nav-user");
    if (!navUser || document.getElementById("ds-scanner-btn")) return;
    const li = document.createElement("li");
    li.className = "nav-item mr-1";
    li.style.cssText = "display:flex;align-items:center;";
    li.innerHTML = `
      <button id="ds-scanner-btn" class="btn btn-sm btn-outline-secondary" style="margin-top:3px;"
        onclick="toggleScannerMode()" title="Activar/desactivar esc&#225;ner">
        <i class="fas fa-barcode mr-1 ds-scn-icon"></i><span class="ds-scn-label">Esc&#225;ner</span>
      </button>
      <button id="ds-scn-cfg-btn" class="btn btn-sm btn-outline-secondary ml-1"
        style="margin-top:3px;width:26px;height:26px;padding:0;display:flex;align-items:center;justify-content:center;"
        title="Configurar esc&#225;ner" onclick="openScannerConfig()">
        <i class="fas fa-cog" style="font-size:.72rem;pointer-events:none;"></i>
      </button>`;
    navUser.before(li);
  });
})();
