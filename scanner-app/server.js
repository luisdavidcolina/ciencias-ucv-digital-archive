/**
 * Scanner Bridge — Ciencias UCV
 *
 * Lee códigos de barras/QR de un escáner USB (modo teclado HID) y los
 * retransmite en tiempo real a clientes web via WebSocket.
 *
 * Modos de entrada:
 *   1. stdin   (predeterminado) — escáner emula teclado, redirigir o usar en terminal
 *   2. hid     — acceso directo al dispositivo HID (requiere node-hid instalado)
 *
 * Uso:
 *   node server.js                     # stdin, puerto 3737
 *   node server.js --port 4000         # puerto personalizado
 *   node server.js --mode hid          # modo HID directo
 *   node server.js --hid-vid 0x05e0    # Vendor ID del escáner (hex)
 *   node server.js --hid-pid 0x1200    # Product ID del escáner (hex)
 *   node server.js --debug             # logs detallados
 */

const { WebSocketServer } = require("ws");
const readline = require("readline");

// ── argumentos ──────────────────────────────────────────────────────────────
const args = process.argv.slice(2);
const getArg = (flag, def = null) => {
  const i = args.indexOf(flag);
  return i !== -1 && args[i + 1] ? args[i + 1] : def;
};
const hasFlag = flag => args.includes(flag);

const PORT  = parseInt(getArg("--port", "3737"), 10);
const MODE  = getArg("--mode", "stdin");           // stdin | hid
const DEBUG = hasFlag("--debug");
const HID_VID = getArg("--hid-vid", null);
const HID_PID = getArg("--hid-pid", null);

// ── WebSocket server ─────────────────────────────────────────────────────────
const wss = new WebSocketServer({ port: PORT, host: "127.0.0.1" });
const clients = new Set();

wss.on("listening", () => {
  console.log(`\n🔌 Scanner Bridge activo en ws://127.0.0.1:${PORT}`);
  console.log(`   Modo de entrada: ${MODE}`);
  console.log(`   Conecta el panel admin y habilita el modo escáner.\n`);
});

wss.on("connection", (ws, req) => {
  const origin = req.headers.origin || "sin origen";
  clients.add(ws);
  if (DEBUG) console.log(`[WS] Cliente conectado (${origin}) — total: ${clients.size}`);

  ws.send(JSON.stringify({ type: "ready", message: "Scanner Bridge conectado", port: PORT }));

  ws.on("close", () => {
    clients.delete(ws);
    if (DEBUG) console.log(`[WS] Cliente desconectado — total: ${clients.size}`);
  });

  ws.on("error", err => {
    if (DEBUG) console.error("[WS] Error de cliente:", err.message);
    clients.delete(ws);
  });
});

wss.on("error", err => {
  if (err.code === "EADDRINUSE") {
    console.error(`\n❌ El puerto ${PORT} ya está en uso. Usa --port para elegir otro.\n`);
    process.exit(1);
  }
  console.error("[WS] Error del servidor:", err);
});

// ── broadcast ────────────────────────────────────────────────────────────────
function broadcast(code) {
  if (!code || !code.trim()) return;
  const payload = JSON.stringify({
    type: "scan",
    code: code.trim(),
    ts: new Date().toISOString(),
  });
  let sent = 0;
  for (const ws of clients) {
    if (ws.readyState === 1 /* OPEN */) {
      ws.send(payload);
      sent++;
    }
  }
  console.log(`📷 Código: ${code.trim()}  → ${sent} cliente(s)`);
}

// ── modo stdin ────────────────────────────────────────────────────────────────
function startStdinMode() {
  console.log("📡 Modo stdin: el escáner debe estar en foco en esta terminal.");
  console.log("   (Los escáneres USB en modo HID simulan pulsaciones de teclado)");
  console.log("   También puedes escribir códigos manualmente y presionar Enter.\n");

  const rl = readline.createInterface({ input: process.stdin, terminal: false });
  process.stdin.setRawMode?.(false);

  rl.on("line", line => {
    const code = line.trim();
    if (code) broadcast(code);
  });

  rl.on("close", () => {
    console.log("\n[stdin] Stream cerrado. El servidor WebSocket sigue activo.");
  });
}

// ── modo HID directo ──────────────────────────────────────────────────────────
function startHidMode() {
  let HID;
  try {
    HID = require("node-hid");
  } catch {
    console.error(
      "\n❌ node-hid no está instalado. Ejecuta: npm install node-hid\n" +
      "   O usa el modo stdin (predeterminado).\n"
    );
    process.exit(1);
  }

  const devices = HID.devices();
  if (DEBUG) {
    console.log("[HID] Dispositivos disponibles:");
    devices.forEach(d => console.log(`  VID:${d.vendorId.toString(16)} PID:${d.productId.toString(16)} → ${d.product || d.manufacturer || "?"}`));
  }

  let vid = HID_VID ? parseInt(HID_VID, 16) : null;
  let pid = HID_PID ? parseInt(HID_PID, 16) : null;

  let targetDevice;
  if (vid && pid) {
    targetDevice = devices.find(d => d.vendorId === vid && d.productId === pid);
  } else {
    // Heurística: buscar dispositivo que parezca escáner HID
    // Muchos escáneres tienen usage 6 (keyboard) o usage 0
    targetDevice = devices.find(d => d.usage === 6 || d.usagePage === 1);
    if (!targetDevice) {
      console.error(
        "\n❌ No se encontró un escáner HID automáticamente.\n" +
        "   Ejecuta con --debug para ver dispositivos disponibles.\n" +
        "   Luego usa --hid-vid y --hid-pid para especificarlo.\n" +
        "   O usa el modo stdin (predeterminado) que no requiere configuración.\n"
      );
      process.exit(1);
    }
  }

  console.log(`📡 Modo HID: ${targetDevice.product || "Escáner"} (VID:${targetDevice.vendorId.toString(16)} PID:${targetDevice.productId.toString(16)})`);

  let buffer = "";
  const BARCODE_CHARS = { 4:97,5:98,6:99,7:100,8:101,9:102,10:103,11:104,12:105,13:106,14:107,15:108,16:109,17:110,18:111,19:112,20:113,21:114,22:115,23:116,24:117,25:118,26:119,27:120,28:121,29:122,30:49,31:50,32:51,33:52,34:53,35:54,36:55,37:56,38:57,39:48,44:32,45:45,46:61,47:91,48:93,49:92,51:59,52:39,53:96,54:44,55:46,56:47 };

  const device = new HID.HID(targetDevice.vendorId, targetDevice.productId);
  device.on("data", data => {
    const modifier = data[0];
    const key = data[2];
    if (key === 0) return;
    // key 40 = Enter → fin de código
    if (key === 40) {
      if (buffer) broadcast(buffer);
      buffer = "";
      return;
    }
    const shifted = modifier & 0x22; // Shift o CapsLock simulation
    let ch = BARCODE_CHARS[key];
    if (ch) {
      if (shifted) ch -= 32; // uppercase
      buffer += String.fromCharCode(ch);
    }
  });

  device.on("error", err => {
    console.error("[HID] Error:", err.message);
  });
}

// ── inicio ────────────────────────────────────────────────────────────────────
if (MODE === "hid") {
  startHidMode();
} else {
  startStdinMode();
}

// Capturar Ctrl+C
process.on("SIGINT", () => {
  console.log("\n[Scanner Bridge] Deteniendo...");
  wss.close();
  process.exit(0);
});
