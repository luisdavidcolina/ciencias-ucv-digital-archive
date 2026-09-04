/**
 * Decodificador de informes HID de teclado (distribución US), usado por el
 * modo `--mode hid` de server.js.
 *
 * DG-030/DG-031: la versión anterior traducía con aritmética sobre ASCII
 * (`ch -= 32` para "aplicar mayúsculas"), que sólo es correcto para letras.
 * Sobre un dígito, `1` (ASCII 49) menos 32 da `17`, un carácter de control:
 * con Mayús activo, cualquier número en el código leído se corrompía en
 * silencio. Aquí se usan dos tablas explícitas (sin Mayús / con Mayús) en
 * vez de aritmética.
 *
 * Sólo cubre la distribución US, que es la que recomienda configurar en el
 * lector (ver README). AltGr (mod & 0x40) no tiene destino en esta tabla:
 * se ignora en vez de producir un carácter equivocado.
 */

"use strict";

// Usage ID → carácter sin modificador.
const UNSHIFTED = {
  4: "a", 5: "b", 6: "c", 7: "d", 8: "e", 9: "f", 10: "g", 11: "h", 12: "i",
  13: "j", 14: "k", 15: "l", 16: "m", 17: "n", 18: "o", 19: "p", 20: "q",
  21: "r", 22: "s", 23: "t", 24: "u", 25: "v", 26: "w", 27: "x", 28: "y", 29: "z",
  30: "1", 31: "2", 32: "3", 33: "4", 34: "5", 35: "6", 36: "7", 37: "8", 38: "9", 39: "0",
  44: " ", 45: "-", 46: "=", 47: "[", 48: "]", 49: "\\",
  51: ";", 52: "'", 53: "`", 54: ",", 55: ".", 56: "/",
};

// Usage ID → carácter con Mayús (Shift) activo.
const SHIFTED = {
  4: "A", 5: "B", 6: "C", 7: "D", 8: "E", 9: "F", 10: "G", 11: "H", 12: "I",
  13: "J", 14: "K", 15: "L", 16: "M", 17: "N", 18: "O", 19: "P", 20: "Q",
  21: "R", 22: "S", 23: "T", 24: "U", 25: "V", 26: "W", 27: "X", 28: "Y", 29: "Z",
  30: "!", 31: "@", 32: "#", 33: "$", 34: "%", 35: "^", 36: "&", 37: "*", 38: "(", 39: ")",
  44: " ", 45: "_", 46: "+", 47: "{", 48: "}", 49: "|",
  51: ":", 52: "\"", 53: "~", 54: "<", 55: ">", 56: "?",
};

// Bits de LeftShift (0x02) y RightShift (0x20) del byte de modificadores.
const SHIFT_MASK = 0x22;

/**
 * Traduce un usage ID de teclado HID + byte de modificadores a un carácter,
 * o `null` si el usage ID no tiene destino conocido (teclas de función,
 * flechas, AltGr sin mapear, etc.) — nunca produce un carácter equivocado
 * por defecto.
 */
function hidToChar(key, modifierByte) {
  const shifted = (modifierByte & SHIFT_MASK) !== 0;
  const table = shifted ? SHIFTED : UNSHIFTED;
  return Object.prototype.hasOwnProperty.call(table, key) ? table[key] : null;
}

// DG-033: límite de longitud del buffer entre lecturas y tiempo de
// inactividad tras el que se emite igual, aunque no haya llegado un Intro.
const MAX_BUFFER_LEN = 256;
const IDLE_FLUSH_MS = 80;

module.exports = { hidToChar, UNSHIFTED, SHIFTED, SHIFT_MASK, MAX_BUFFER_LEN, IDLE_FLUSH_MS };
