"use strict";

const test = require("node:test");
const assert = require("node:assert/strict");
const { getArg, getArgAll, hasFlag } = require("../lib/args");

test("lee el valor de una bandera", () => {
  assert.equal(getArg(["--port", "4000"], "--port"), "4000");
});

test("DG-042: una bandera sin valor no roba el de la siguiente bandera", () => {
  assert.equal(getArg(["--port", "--debug"], "--port"), null);
});

test("una bandera ausente devuelve el valor por defecto", () => {
  assert.equal(getArg(["--mode", "hid"], "--port", "3737"), "3737");
});

test("getArgAll recoge todas las apariciones repetidas de una bandera", () => {
  const args = ["--origin", "https://a.example", "--origin", "https://b.example"];
  assert.deepEqual(getArgAll(args, "--origin"), ["https://a.example", "https://b.example"]);
});

test("getArgAll no arrastra otra bandera como si fuera un valor", () => {
  assert.deepEqual(getArgAll(["--origin", "--debug"], "--origin"), []);
});

test("hasFlag detecta un interruptor sin valor", () => {
  assert.equal(hasFlag(["--debug"], "--debug"), true);
  assert.equal(hasFlag(["--debug"], "--dev"), false);
});
