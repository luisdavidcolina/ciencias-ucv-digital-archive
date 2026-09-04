/**
 * DG-042: un analizador de argumentos mínimo pero correcto. La versión
 * anterior (`args[i+1]`) confundía una bandera sin valor con el valor de la
 * bandera anterior: `node server.js --port --debug` dejaba `--port` con el
 * texto `"--debug"` en vez de con nada.
 */

"use strict";

function getArg(args, flag, def = null) {
  const i = args.indexOf(flag);
  if (i === -1) return def;
  const val = args[i + 1];
  if (val === undefined || val.startsWith("--")) return def;
  return val;
}

function getArgAll(args, flag) {
  const out = [];
  for (let i = 0; i < args.length; i++) {
    if (args[i] === flag && args[i + 1] && !args[i + 1].startsWith("--")) out.push(args[i + 1]);
  }
  return out;
}

function hasFlag(args, flag) {
  return args.includes(flag);
}

module.exports = { getArg, getArgAll, hasFlag };
