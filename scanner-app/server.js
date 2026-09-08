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
 *   node server.js                          # stdin, puerto 3737, solo localhost
 *   node server.js --port 4000              # puerto personalizado
 *   node server.js --host 0.0.0.0           # accesible desde la red (exige --origin)
 *   node server.js --origin https://archivo.ejemplo.edu   # origen web permitido (repetible)
 *   node server.js --mode hid --hid-vid 0x05e0 --hid-pid 0x1900
 *   node server.js --list-hid              # lista dispositivos HID y sale
 *   node server.js --debug                 # logs detallados, incluye códigos completos
 */

const http = require("http");
const https = require("https");
const fs = require("fs");
const crypto = require("crypto");
const { WebSocketServer } = require("ws");
const readline = require("readline");
const { hidToChar, MAX_BUFFER_LEN, IDLE_FLUSH_MS } = require("./lib/hid-decoder");
const { redactCode } = require("./lib/redact");
const argsLib = require("./lib/args");

// ── argumentos ───────────────────────────────────────────────────────────────
const args = process.argv.slice(2);
const getArg = (flag, def = null) => argsLib.getArg(args, flag, def);
// DG-003/SI-172: --origin es repetible; cada aparición añade un origen a la
// lista blanca del WebSocket y del CORS del REST.
const getArgAll = flag => argsLib.getArgAll(args, flag);
const hasFlag = flag => argsLib.hasFlag(args, flag);

function fail(msg) {
  console.error(`\n❌  ${msg}\n`);
  process.exit(1);
}

// DG-041: --port debe ser un entero de puerto válido, no lo que salga de
// parsear basura.
const PORT_RAW = getArg("--port", "3737");
const PORT = Number.parseInt(PORT_RAW, 10);
if (!Number.isInteger(PORT) || PORT < 1 || PORT > 65535) {
  fail(`--port recibió "${PORT_RAW}", que no es un puerto válido (1-65535).`);
}

// DG-016 (ya cerrado el 2026-09-03): por defecto solo localhost. 0.0.0.0
// (accesible desde toda la red local) hay que pedirlo explícitamente con
// --host, sabiendo que sin --origin no hay nada más que lo proteja.
const HOST = getArg("--host", "127.0.0.1");
const MODE = getArg("--mode", "stdin");
const DEBUG = hasFlag("--debug");
const DEV = hasFlag("--dev");
const HID_VID_RAW = getArg("--hid-vid", null);
const HID_PID_RAW = getArg("--hid-pid", null);
const LIST_HID = hasFlag("--list-hid");
const ALLOWED_ORIGINS = getArgAll("--origin");
const TLS_CERT = getArg("--cert", null);
const TLS_KEY = getArg("--key", null);

if (MODE !== "stdin" && MODE !== "hid") {
  fail(`--mode recibió "${MODE}". Los valores válidos son "stdin" o "hid".`);
}

// DG-039: la promesa "node >=18" del package.json nadie la verificaba. Node
// falla igual más adelante con un error críptico si faltan APIs recientes;
// esto lo dice con la causa correcta, antes de arrancar nada.
{
  const major = Number.parseInt(process.versions.node.split(".")[0], 10);
  if (major < 18) {
    fail(`Este puente necesita Node 18 o superior. Esta máquina tiene Node ${process.version}.`);
  }
}

// DG-041: VID/PID sólo se validan cuando el modo HID los necesita — parseInt
// silencioso hacía caer al peor caso (DG-029: heurística sobre "el primer
// teclado") sin ningún aviso.
function parseHex(raw, flagName) {
  if (raw == null) return null;
  const n = Number.parseInt(raw, 16);
  if (Number.isNaN(n)) fail(`${flagName} recibió "${raw}", que no es un valor hexadecimal válido.`);
  return n;
}
const HID_VID = parseHex(HID_VID_RAW, "--hid-vid");
const HID_PID = parseHex(HID_PID_RAW, "--hid-pid");

// DG-003: token compartido. Si no viene en el entorno, se genera uno al
// arrancar y se imprime en consola — nunca se elige un valor por defecto fijo.
const TOKEN = process.env.SCANNER_TOKEN || crypto.randomBytes(16).toString("hex");
const TOKEN_WAS_GENERATED = !process.env.SCANNER_TOKEN;

// DG-003/SI-172: HOST remoto (0.0.0.0 o cualquier IP que no sea loopback)
// exige al menos un --origin explícito, salvo en --dev. Sin esto, "poner un
// token" no cierra nada: cualquier página en cualquier pestaña puede seguir
// leyendo el WebSocket si el origen no se comprueba.
const IS_LOOPBACK_HOST = HOST === "127.0.0.1" || HOST === "localhost" || HOST === "::1";
if (!IS_LOOPBACK_HOST && ALLOWED_ORIGINS.length === 0 && !DEV) {
  fail(
    `--host ${HOST} expone el puente a la red. Declara al menos un --origin ` +
      `(el dominio HTTPS del panel) o usa --dev si es una prueba local.`
  );
}

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
    console.error("El módulo node-hid no está instalado. Ejecuta: npm install node-hid");
  }
  process.exit(0);
}

// ── estado global ─────────────────────────────────────────────────────────────
const clients    = new Set();     // ws -> metadata va en WeakMap aparte
const clientMeta = new WeakMap(); // DG-006/DG-007: latido y purga de muertos
const scanLog    = [];        // últimas 50 lecturas
const startedAt  = new Date().toISOString();
let seq = 0;                  // DG-002: número de secuencia por lectura

function pushScan(code) {
  seq += 1;
  scanLog.unshift({ code, ts: new Date().toISOString(), seq });
  if (scanLog.length > 50) scanLog.pop();
  return seq;
}

// ── broadcast a todos los clientes WS ────────────────────────────────────────
function broadcast(code, source = "device") {
  if (!code || !code.trim()) return;
  const trimmed = code.trim();
  const n = pushScan(trimmed);
  const payload = JSON.stringify({ type: "scan", code: trimmed, ts: new Date().toISOString(), seq: n, source });
  let sent = 0;
  for (const ws of clients) {
    if (ws.readyState === 1) { ws.send(payload); sent++; }
  }
  // DG-013: nunca se imprime el código completo salvo en --debug. En modo
  // normal se muestra sólo un fragmento redactado — suficiente para confirmar
  // que algo se leyó, insuficiente para reconstruir una cédula desde la
  // consola de una máquina compartida.
  console.log(`📷  ${redactCode(trimmed, DEBUG)}  →  ${sent} cliente(s) WS`);
}

// DG-003: comprueba el token compartido en la cabecera X-Scanner-Token o en
// el parámetro ?token= de la URL. Sin esto, cualquier equipo en la misma red
// (o cualquier página en el navegador si el host fuera 0.0.0.0) podría leer
// el registro de escaneos o inyectar uno falso.
function _tokenValido(req, urlObj) {
  const header = req.headers["x-scanner-token"];
  const query = urlObj.searchParams.get("token");
  const recibido = header || query;
  // DG-... : comparación en tiempo constante. `===` sobre strings sale en
  // cuanto encuentra el primer carácter distinto — en una red local eso deja
  // una diferencia de tiempo medible entre "el primer carácter ya falla" y
  // "los primeros N coinciden", suficiente para reconstruir el token
  // carácter a carácter con suficientes intentos. timingSafeEqual exige
  // buffers del mismo tamaño, así que primero se descarta la longitud (fuga
  // aceptable: la longitud del token no es el secreto) y sólo se compara en
  // tiempo constante cuando coincide.
  if (typeof recibido !== "string" || recibido.length !== TOKEN.length) return false;
  return crypto.timingSafeEqual(Buffer.from(recibido), Buffer.from(TOKEN));
}

// DG-003 (WebSocket)/SI-172 (REST): el navegador no aplica same-origin a
// WebSocket, así que hay que comprobar `Origin` a mano. Sin --origin
// declarado y sin --dev no se llega aquí (falla al arrancar); con --dev se
// admite cualquier origen desde loopback, para pruebas locales.
function _origenPermitido(req) {
  if (IS_LOOPBACK_HOST && DEV) return true;
  const origin = req.headers.origin;
  if (ALLOWED_ORIGINS.length === 0) return IS_LOOPBACK_HOST; // sin --host remoto, sin --origin: sólo loopback
  if (!origin) return IS_LOOPBACK_HOST; // clientes no-navegador (curl, node-fetch) no mandan Origin
  return ALLOWED_ORIGINS.includes(origin);
}

// ── HTTP REST + WebSocket en el mismo servidor ────────────────────────────────
const requestHandler = (req, res) => {
  const originOk = _origenPermitido(req);
  // DG-... /SI-172: el CORS ya no es un "*" incondicional — sólo se abre a los
  // orígenes de la lista blanca (o a cualquiera en modo loopback sin --origin,
  // que es el caso de uso de un solo puesto).
  const originHeader = req.headers.origin;
  if (originHeader && (ALLOWED_ORIGINS.includes(originHeader) || (IS_LOOPBACK_HOST && ALLOWED_ORIGINS.length === 0))) {
    res.setHeader("Access-Control-Allow-Origin", originHeader);
    res.setHeader("Vary", "Origin");
  }
  res.setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
  res.setHeader("Access-Control-Allow-Headers", "Content-Type, X-Scanner-Token");

  if (req.method === "OPTIONS") {
    res.writeHead(204); res.end(); return;
  }

  if (!originOk) {
    res.writeHead(403, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ error: "Origen no permitido" }));
    return;
  }

  const urlObj = new URL(req.url, `http://${req.headers.host || "localhost"}`);
  const url = urlObj.pathname;

  if (!_tokenValido(req, urlObj)) {
    res.writeHead(401, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ error: "Token inválido o ausente. Envía X-Scanner-Token o ?token=" }));
    return;
  }

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
      version: require("./package.json").version,
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

  // GET /log — últimos scans. DG-013/SI-173: el código va redactado salvo
  // en --debug — este endpoint es la otra mitad de la fuga que DG-013 cierra
  // en la consola: sin redactar, cualquiera con el token podía leer cédulas
  // completas por HTTP.
  if (req.method === "GET" && url === "/log") {
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ scans: scanLog.map(s => ({ ...s, code: redactCode(s.code, DEBUG) })) }));
    return;
  }

  // POST /test-scan — simular lectura (útil desde panel web). DG-004: se
  // marca con source:"test" para que el cliente pueda distinguirlo de una
  // lectura real y, en producción, rechazarlo si no está en modo diagnóstico.
  if (req.method === "POST" && url === "/test-scan") {
    let body = "";
    let tooBig = false;
    req.on("data", d => {
      body += d;
      if (body.length > 4096) { tooBig = true; req.destroy(); }
    });
    req.on("end", () => {
      if (tooBig) return;
      let code = "TEST-" + Date.now();
      try { const j = JSON.parse(body); if (j.code) code = String(j.code).slice(0, 256); } catch {}
      broadcast(code, "test");
      res.writeHead(200, { "Content-Type": "application/json" });
      res.end(JSON.stringify({ ok: true, code }));
    });
    return;
  }

  res.writeHead(404, { "Content-Type": "application/json" });
  res.end(JSON.stringify({ error: "Not found", available: ["/status", "/config", "/log", "/test-scan"] }));
};

// DG-019/SI-171: con --cert y --key el puente sirve wss:// real en vez de
// ws://. No resuelve por sí solo el problema de la CA (sigue haciendo falta
// un certificado válido para el nombre que use el navegador), pero deja de
// ser una limitación del código: es una limitación de no tener el
// certificado todavía. Ver DG-045 en el README para la decisión de fondo.
let httpServer;
if (TLS_CERT && TLS_KEY) {
  try {
    httpServer = https.createServer(
      { cert: fs.readFileSync(TLS_CERT), key: fs.readFileSync(TLS_KEY) },
      requestHandler
    );
  } catch (err) {
    fail(`No se pudo leer el certificado o la clave TLS: ${err.message}`);
  }
} else {
  httpServer = http.createServer(requestHandler);
}

// DG-010: límite de tamaño de marco (evita que un mensaje enorme tumbe el
// proceso) y comprobación de origen/token antes de aceptar la conexión.
const wss = new WebSocketServer({ server: httpServer, maxPayload: 4096 });

// DG-010: tope de clientes simultáneos — cuatro basta para el uso real
// (un archivista con dos pestañas, más margen), y evita que abrir mil
// conexiones agote el proceso.
const MAX_CLIENTS = 4;

// DG-006/DG-007: latido cada 20 s, purga de quien no responda en el
// siguiente ciclo. Sin esto un socket colgado (portátil suspendido, cable
// caído) sigue contando como cliente conectado indefinidamente.
const HEARTBEAT_MS = 20000;

wss.on("connection", (ws, req) => {
  const urlObj = new URL(req.url || "/", `http://${req.headers.host || "localhost"}`);

  if (!_origenPermitido(req)) {
    if (DEBUG) console.log(`[WS] conexión rechazada (${req.socket.remoteAddress}): origen no permitido (${req.headers.origin || "sin Origin"})`);
    ws.close(1008, "Origen no permitido");
    return;
  }
  if (!_tokenValido(req, urlObj)) {
    if (DEBUG) console.log(`[WS] conexión rechazada (${req.socket.remoteAddress}): token inválido`);
    ws.close(4401, "Token inválido");
    return;
  }
  if (clients.size >= MAX_CLIENTS) {
    if (DEBUG) console.log(`[WS] conexión rechazada (${req.socket.remoteAddress}): límite de ${MAX_CLIENTS} clientes alcanzado`);
    ws.close(1013, "Demasiadas conexiones");
    return;
  }

  clients.add(ws);
  clientMeta.set(ws, { alive: true });
  if (DEBUG) console.log(`[WS] +cliente (${req.socket.remoteAddress}) — total: ${clients.size}`);
  ws.send(JSON.stringify({
    type: "ready",
    v: 1,
    message: "Scanner Bridge conectado",
    port: PORT,
    version: require("./package.json").version,
  }));
  ws.on("pong", () => { const m = clientMeta.get(ws); if (m) m.alive = true; });
  ws.on("close", () => { clients.delete(ws); clientMeta.delete(ws); if (DEBUG) console.log(`[WS] -cliente — total: ${clients.size}`); });
  ws.on("error", err => { if (DEBUG) console.error("[WS] error:", err.message); clients.delete(ws); clientMeta.delete(ws); });
});

const heartbeatTimer = setInterval(() => {
  for (const ws of clients) {
    const meta = clientMeta.get(ws);
    if (!meta) continue;
    if (!meta.alive) { ws.terminate(); clients.delete(ws); clientMeta.delete(ws); continue; }
    meta.alive = false;
    try { ws.ping(); } catch { /* socket ya cerrándose */ }
  }
}, HEARTBEAT_MS);
heartbeatTimer.unref?.();

httpServer.listen(PORT, HOST, () => {
  const scheme = TLS_CERT ? "wss" : "ws";
  const restScheme = TLS_CERT ? "https" : "http";
  const displayHost = HOST === "0.0.0.0" ? "<IP-de-esta-PC>" : HOST;
  console.log(`\n🔌  Puente del escáner activo`);
  console.log(`    Conexión: ${scheme}://${displayHost}:${PORT}`);
  console.log(`    Estado  : ${restScheme}://${displayHost}:${PORT}/status`);
  console.log(`    Modo: ${MODE}  |  Host: ${HOST}${ALLOWED_ORIGINS.length ? `  |  Orígenes permitidos: ${ALLOWED_ORIGINS.join(", ")}` : ""}\n`);
  if (TOKEN_WAS_GENERATED) {
    console.log(`    🔑  No se definió SCANNER_TOKEN. Se generó uno para esta sesión:`);
    console.log(`        ${TOKEN}`);
    console.log(`        Cópialo en la configuración del escáner del panel web, o define`);
    console.log(`        SCANNER_TOKEN en el entorno para que no cambie en cada arranque.\n`);
  } else {
    console.log(`    🔑  Token configurado por entorno (SCANNER_TOKEN).\n`);
  }
});

httpServer.on("error", err => {
  if (err.code === "EADDRINUSE") {
    console.error(`\n❌  El puerto ${PORT} ya está en uso. Prueba con --port y otro número.\n`);
    process.exit(1);
    return;
  }
  if (err.code === "EACCES") {
    console.error(`\n❌  Sin permiso para usar el puerto ${PORT}. En Windows/macOS/Linux los puertos por`);
    console.error(`    debajo de 1024 requieren privilegios; usa uno por encima de 1024 (por defecto 3737).\n`);
    process.exit(1);
    return;
  }
  console.error(`\n❌  Error de red inesperado: ${err.message}\n`);
  process.exit(1);
});

// ── modo stdin ────────────────────────────────────────────────────────────────
let stdinInterface = null;
function startStdinMode() {
  console.log("📡  Modo teclado — el lector debe estar enfocando esta ventana, no el navegador.");
  console.log("    (los lectores de código de barras USB emulan un teclado por defecto)\n");
  stdinInterface = readline.createInterface({ input: process.stdin, terminal: false });
  process.stdin.setRawMode?.(false);
  stdinInterface.on("line", line => { const c = line.trim(); if (c) broadcast(c, "device"); });
  stdinInterface.on("close", () => console.log("\n[teclado] Entrada cerrada. La conexión con el panel sigue activa."));
}

// ── modo HID directo ──────────────────────────────────────────────────────────
let hidDevice = null;
function startHidMode() {
  let HID;
  try { HID = require("node-hid"); } catch {
    fail("El módulo node-hid no está instalado. Ejecuta: npm install node-hid");
  }
  // DG-029 (ya cerrado el 2026-09-03): sin VID/PID explícitos, "el primer
  // teclado" del sistema puede ser el teclado interno de un portátil — y en
  // ese caso el puente retransmitiría cada pulsación por WebSocket. El modo
  // HID exige el dispositivo exacto, elegido una vez por quien digitaliza.
  if (HID_VID == null || HID_PID == null) {
    fail("El modo HID requiere --hid-vid y --hid-pid explícitos. Ejecuta con --list-hid para ver los dispositivos disponibles y sus identificadores.");
  }
  const devices = HID.devices();
  const target = devices.find(d => d.vendorId === HID_VID && d.productId === HID_PID);
  if (!target) {
    fail("No se encontró ese lector conectado. Ejecuta con --list-hid para ver los dispositivos disponibles.");
  }
  console.log(`📡  Modo HID: ${target.product || "Lector"} (VID:${target.vendorId.toString(16)} PID:${target.productId.toString(16)})`);

  let buf = "";
  let idleTimer = null;
  const flush = () => { if (buf) { broadcast(buf, "device"); buf = ""; } };
  const scheduleIdleFlush = () => {
    clearTimeout(idleTimer);
    idleTimer = setTimeout(flush, IDLE_FLUSH_MS);
  };

  function openDevice() {
    try {
      hidDevice = new HID.HID(target.vendorId, target.productId);
    } catch (err) {
      console.error(`[HID] No se pudo abrir el dispositivo: ${err.message}. Reintentando en 3 s…`);
      setTimeout(openDevice, 3000);
      return;
    }
    hidDevice.on("data", data => {
      const mod = data[0];
      // DG-032: un informe HID trae hasta seis teclas simultáneas
      // (data[2..7]); leer sólo data[2] pierde caracteres con lectores
      // rápidos que agrupan pulsaciones en un solo informe.
      for (let i = 2; i < Math.min(data.length, 8); i++) {
        const key = data[i];
        if (!key) continue;
        if (key === 40 || key === 88) { flush(); continue; } // Intro / Intro numérico
        const ch = hidToChar(key, mod);
        if (ch != null) buf += ch;
      }
      // DG-033: el buffer no crece sin límite si el lector no manda Intro,
      // y se vacía también por inactividad (sin esperar un Intro que no va
      // a llegar).
      if (buf.length > MAX_BUFFER_LEN) buf = buf.slice(-MAX_BUFFER_LEN);
      scheduleIdleFlush();
    });
    hidDevice.on("error", err => {
      console.error(`[HID] Dispositivo desconectado o con error: ${err.message}. Reintentando…`);
      try { hidDevice.close(); } catch { /* ya cerrado */ }
      hidDevice = null;
      setTimeout(openDevice, 3000);
    });
  }
  openDevice();
}

if (MODE === "hid") startHidMode(); else startStdinMode();

// DG-025: SIGTERM (servicio, cierre de sesión) recibía el mismo trato que
// "nada" — el proceso moría sin cerrar el dispositivo HID ni los sockets.
// DG-024: cualquier señal de cierre pasa por el mismo camino ordenado.
function shutdown(signal) {
  console.log(`\n[Puente del escáner] Deteniendo (${signal})…`);
  clearInterval(heartbeatTimer);
  // DG-026: el dispositivo HID no se cerraba nunca — en Windows quedaba
  // tomado hasta que el proceso moría del todo, y un reinicio rápido fallaba
  // con un error de acceso que nadie sabía leer.
  if (hidDevice) { try { hidDevice.close(); } catch { /* ya cerrado */ } }
  if (stdinInterface) { try { stdinInterface.close(); } catch { /* ya cerrado */ } }
  for (const ws of clients) { try { ws.close(1001, "Puente detenido"); } catch { /* ya cerrado */ } }
  httpServer.close(() => process.exit(0));
  // Si algo se queda colgado (un socket que no cierra), no bloquear el
  // proceso indefinidamente.
  setTimeout(() => process.exit(0), 2000).unref();
}
process.on("SIGINT", () => shutdown("SIGINT"));
process.on("SIGTERM", () => shutdown("SIGTERM"));
process.on("SIGHUP", () => shutdown("SIGHUP"));
