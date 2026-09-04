"use strict";

// Punto de entrada único de la suite: evita depender de que el shell del
// sistema operativo expanda un patrón como `test/*.test.js` (cmd.exe, que es
// el que usa `npm run test` en Windows, no lo hace).

const path = require("node:path");
const fs = require("node:fs");

const dir = __dirname;
for (const file of fs.readdirSync(dir)) {
  if (file.endsWith(".test.js")) require(path.join(dir, file));
}
