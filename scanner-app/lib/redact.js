/**
 * DG-013/SI-173: ni la consola ni `GET /log` deben mostrar cédulas
 * completas fuera de --debug. En una máquina compartida la terminal queda a
 * la vista, y el propio endpoint HTTP es legible por cualquiera que tenga
 * el token del puente.
 */

"use strict";

function redactCode(code, debug) {
  if (debug) return code;
  if (!code) return code;
  if (code.length <= 4) return "*".repeat(code.length);
  return "*".repeat(code.length - 4) + code.slice(-4);
}

module.exports = { redactCode };
