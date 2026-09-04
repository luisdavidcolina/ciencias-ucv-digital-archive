"use strict";

const test = require("node:test");
const assert = require("node:assert/strict");
const { hidToChar, MAX_BUFFER_LEN, IDLE_FLUSH_MS } = require("../lib/hid-decoder");

test("letras sin modificador salen en minúscula", () => {
  assert.equal(hidToChar(4, 0), "a");
  assert.equal(hidToChar(29, 0), "z");
});

test("letras con Mayús (izq. o der.) salen en mayúscula", () => {
  assert.equal(hidToChar(4, 0x02), "A"); // LeftShift
  assert.equal(hidToChar(4, 0x20), "A"); // RightShift
});

test("DG-031: un dígito con Mayús da el símbolo, no un carácter de control", () => {
  // La versión anterior hacía ch -= 32 sobre cualquier tecla con Mayús:
  // "1" (ASCII 49) - 32 = 17, que no es imprimible. Aquí debe dar "!".
  assert.equal(hidToChar(30, 0x02), "!");
  assert.equal(hidToChar(39, 0x02), ")"); // usage 39 = "0" -> ")"
});

test("dígitos sin modificador se mantienen intactos", () => {
  assert.equal(hidToChar(30, 0), "1");
  assert.equal(hidToChar(39, 0), "0");
});

test("puntuación común (guion, punto, coma) traduce con y sin Mayús", () => {
  assert.equal(hidToChar(45, 0), "-");
  assert.equal(hidToChar(45, 0x02), "_");
  assert.equal(hidToChar(55, 0), ".");
  assert.equal(hidToChar(55, 0x02), ">");
});

test("un usage ID sin destino conocido da null, nunca un carácter equivocado", () => {
  assert.equal(hidToChar(0x3a, 0), null); // F1
  assert.equal(hidToChar(999, 0), null);
});

test("DG-032: un informe con varias teclas se recorre entero", () => {
  // Simula el recorrido que hace server.js sobre data[2..7] de un informe
  // HID que trae varias teclas en el mismo paquete.
  const report = [0, 0, 4, 5, 6, 0, 0, 0]; // "abc"
  let out = "";
  for (let i = 2; i < 8; i++) {
    const ch = hidToChar(report[i], report[0]);
    if (ch != null) out += ch;
  }
  assert.equal(out, "abc");
});

test("los límites de buffer e inactividad están definidos y son razonables", () => {
  assert.ok(MAX_BUFFER_LEN > 0 && MAX_BUFFER_LEN < 10000);
  assert.ok(IDLE_FLUSH_MS > 0 && IDLE_FLUSH_MS < 5000);
});
