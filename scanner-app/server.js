/**
 * Scanner Bridge — Ciencias UCV
 *
 * WebSocket + HTTP REST en el mismo puerto.
 * El cliente web (scanner-client.js) se conecta via WS para recibir scans
 * y via HTTP para consultar estado y enviar escaneos de prueba.
 *
 * Modos de entrada:
 *   stdin  (predeterminado) — escáner emula teclado, redirigir stdin o escribir manualmente
 *   hid    — acceso directo al dispositivo USB HID (requiere node-hid)
 *
 * Uso:
 *   node server.js                          # stdin, puerto 3737, accesible en red
 *   node server.js --port 4000              # puerto personalizado
 *   node server.js --host 127.0.0.1        # solo localhost (sin acceso remoto)
 *   node server.js --mode hid              # modo HID directo
 *   node server.js --hid-vid 0x05e0 --hid-pid 0x1900
 *   node server.js --list-hid              # lista dispositivos HID y sale
 *   node server.js --debug                 # logs detallados
 */

const http = require("http");
const { WebSocketServer } = require("ws");
const readline = require("readline");

// ── argumentos ───────────────────────────────────────────────────────────────
const args   = process.argv.slice(2);
const getArg = (flag, def = null) => {
  const i = args.indexOf(flag);
  return i !== -1 && args[i + 1] ? args[i + 1] : def;
};
const hasFlag = flag => args.includes(flag);

const PORT     = parseInt(getArg("--port", "3737"), 10);
const HOST     = getArg("--host", "0.0.0.0");   // 0.0.0.0 = accesible desde la red local
const MODE     = getArg("--mode", "stdin");
const DEBUG    = hasFlag("--debug");
const HID_VID  = getArg("--hid-vid", null);
const HID_PID  = getArg("--hid-pid", null);
const LIST_HID = hasFlag("--list-hid");

// ── listar HID y salir ────────────────────────────────────────────────────────
if (LIST_HID) {
  try {
    const HID = require("node-hid");
    console.log("\nDispositivos HID disponibles:\n");
    HID.devices().forEach(d => {
      console.log(`  VID: 0x${d.vendorId.toString(16).padStart(4,"0")}  PID: 0x${d.productId.toString(16).padStart(4,"0")}  →  ${d.product || d.manufacturer || "(sin nombre)"}`);
    });
    console.log("\nUsa --hid-vid y --hid-pid para seleccionar el dispositivo.\n");
  } catch {
    console.error("node-hid no instalado. Ejecuta: npm install node-hid");
  }
  process.exit(0);
}

// ── estado global ─────────────────────────────────────────────────────────────
const clients    = new Set();
const scanLog    = [];        // últimas 50 lecturas
const startedAt  = new Date().toISOString();

function pushScan(code) {
  scanLog.unshift({ code, ts: new Date().toISOString() });
  if (scanLog.length > 50) scanLog.pop();
}

// ── broadcast a todos los clientes WS ────────────────────────────────────────
function broadcast(code) {
  if (!code || !code.trim()) return;
  const payload = JSON.stringify({ type: "scan", code: code.trim(), ts: new Date().toISOString() });
  let sent = 0;
  for (const ws of clients) {
    if (ws.readyState === 1) { ws.send(payload); sent++; }
  }
  pushScan(code.trim());
  console.log(`📷  ${code.trim()}  →  ${sent} cliente(s) WS`);
}

// ── HTTP REST + WebSocket en el mismo servidor ────────────────────────────────
const httpServer = http.createServer((req, res) => {
  // CORS — permite que la web (cualquier origen) consulte el bridge
  res.setHeader("Access-Control-Allow-Origin", "*");
  res.setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
  res.setHeader("Access-Control-Allow-Headers", "Content-Type");

  if (req.method === "OPTIONS") {
    res.writeHead(204); res.end(); return;
  }

  const url = req.url.split("?")[0];

  // GET /status — info del proceso
  if (req.method === "GET" && url === "/status") {
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(JSON.stringify({
      ok: true,
      mode: MODE,
      clients: clients.size,
      port: PORT,
      host: HOST,
      started_at: startedAt,
      uptime_s: Math.floor(process.uptime()),
    }));
    return;
  }

  // GET /config — configuración y dispositivos HID si aplica
  if (req.method === "GET" && url === "/config") {
    let hidDevices = [];
    if (MODE === "hid") {
      try { hidDevices = require("node-hid").devices().map(d => ({
        vid: "0x" + d.vendorId.toString(16), pid: "0x" + d.productId.toString(16),
        name: d.product || d.manufacturer || "(sin nombre)",
      })); } catch { /* sin node-hid */ }
    }
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ mode: MODE, port: PORT, host: HOST, hid_devices: hidDevices }));
    return;
  }

  // GET /log — últimos scans
  if (req.method === "GET" && url === "/log") {
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ scans: scanLog }));
    return;
  }

  // POST /test-scan — simular lectura (útil desde panel web)
  if (req.method === "POST" && url === "/test-scan") {
    let body = "";
    req.on("data", d => { body += d; });
    req.on("end", () => {
      let code = "TEST-" + Date.now();
      try { const j = JSON.parse(body); if (j.code) code = String(j.code); } catch {}
      broadcast(code);
      res.writeHead(200, { "Content-Type": "application/json" });
      res.end(JSON.stringify({ ok: true, code }));
    });
    return;
  }

  res.writeHead(404, { "Content-Type": "application/json" });
  res.end(JSON.stringify({ error: "Not found", available: ["/status", "/config", "/log", "/test-scan"] }));
});

// ── WebSocket sobre el mismo httpServer ───────────────────────────────────────
const wss = new WebSocketServer({ server: httpServer });

wss.on("connection", (ws, req) => {
  clients.add(ws);
  if (DEBUG) console.log(`[WS] +cliente (${req.socket.remoteAddress}) — total: ${clients.size}`);
  ws.send(JSON.stringify({ type: "ready", message: "Scanner Bridge conectado", port: PORT }));
  ws.on("close", () => { clients.delete(ws); if (DEBUG) console.log(`[WS] -cliente — total: ${clients.size}`); });
  ws.on("error", err => { if (DEBUG) console.error("[WS] error:", err.message); clients.delete(ws); });
});

httpServer.listen(PORT, HOST, () => {
  const displayHost = HOST === "0.0.0.0" ? "red local" : HOST;
  console.log(`\n🔌  Scanner Bridge activo`);
  console.log(`    WS  : ws://${HOST === "0.0.0.0" ? "<IP-local>" : HOST}:${PORT}`);
  console.log(`    REST: http://${HOST === "0.0.0.0" ? "<IP-local>" : HOST}:${PORT}/status`);
  console.log(`    Modo: ${MODE}  |  Host: ${displayHost}\n`);
  console.log(`    Desde el panel web usa la URL: ws://<IP-de-esta-PC>:${PORT}`);
  if (HOST === "0.0.0.0")
    console.log(`    Para encontrar tu IP: ejecuta "ipconfig" (Windows) o "ip a" (Linux)\n`);
});

httpServer.on("error", err => {
  if (err.code === "EADDRINUSE") {
    console.error(`\n❌  Puerto ${PORT} ocupado. Usa --port para elegir otro.\n`);
    process.exit(1);
  }
  console.error("[HTTP] Error:", err);
});

// ── modo stdin ────────────────────────────────────────────────────────────────
function startStdinMode() {
  console.log("📡  Modo stdin — el escáner debe enfocar esta terminal.");
  console.log("    (escáneres USB en modo HID simulan pulsaciones de teclado)\n");
  const rl = readline.createInterface({ input: process.stdin, terminal: false });
  process.stdin.setRawMode?.(false);
  rl.on("line", line => { const c = line.trim(); if (c) broadcast(c); });
  rl.on("close", () => console.log("\n[stdin] Stream cerrado. WS sigue activo."));
}

// ── modo HID directo ──────────────────────────────────────────────────────────
function startHidMode() {
  let HID;
  try { HID = require("node-hid"); } catch {
    console.error("\n❌  node-hid no instalado. Ejecuta: npm install node-hid\n"); process.exit(1);
  }
  const devices = HID.devices();
  let vid = HID_VID ? parseInt(HID_VID, 16) : null;
  let pid = HID_PID ? parseInt(HID_PID, 16) : null;
  let target;
  if (vid && pid) {
    target = devices.find(d => d.vendorId === vid && d.productId === pid);
  } else {
    target = devices.find(d => d.usage === 6 || d.usagePage === 1);
  }
  if (!target) {
    console.error("\n❌  No se encontró escáner HID. Ejecuta con --list-hid para ver dispositivos.\n");
    process.exit(1);
  }
  console.log(`📡  Modo HID: ${target.product || "Escáner"} (VID:${target.vendorId.toString(16)} PID:${target.productId.toString(16)})`);

  // Tabla HID keycode → ASCII (keyboard US)
  const KC = {4:97,5:98,6:99,7:100,8:101,9:102,10:103,11:104,12:105,13:106,14:107,15:108,
    16:109,17:110,18:111,19:112,20:113,21:114,22:115,23:116,24:117,25:118,26:119,27:120,
    28:121,29:122,30:49,31:50,32:51,33:52,34:53,35:54,36:55,37:56,38:57,39:48,
    44:32,45:45,46:61,47:91,48:93,49:92,51:59,52:39,53:96,54:44,55:46,56:47};
  let buf = "";
  const dev = new HID.HID(target.vendorId, target.productId);
  dev.on("data", data => {
    const mod = data[0], key = data[2];
    if (!key) return;
    if (key === 40) { if (buf) broadcast(buf); buf = ""; return; }
    let ch = KC[key];
    if (ch) { if (mod & 0x22) ch -= 32; buf += String.fromCharCode(ch); }
  });
  dev.on("error", err => console.error("[HID] Error:", err.message));
}

if (MODE === "hid") startHidMode(); else startStdinMode();

process.on("SIGINT", () => { console.log("\n[Scanner Bridge] Deteniendo…"); httpServer.close(); process.exit(0); });
