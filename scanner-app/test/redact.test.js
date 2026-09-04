"use strict";

const test = require("node:test");
const assert = require("node:assert/strict");
const { redactCode } = require("../lib/redact");

test("en modo debug devuelve el código completo", () => {
  assert.equal(redactCode("V-12345678", true), "V-12345678");
});

test("fuera de debug oculta todo salvo los últimos cuatro caracteres", () => {
  assert.equal(redactCode("V-12345678", false), "******5678");
});

test("un código corto se oculta entero", () => {
  assert.equal(redactCode("ab", false), "**");
});

test("valores vacíos o nulos no truenan", () => {
  assert.equal(redactCode("", false), "");
  assert.equal(redactCode(null, false), null);
});
