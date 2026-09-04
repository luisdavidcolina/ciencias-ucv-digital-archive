# Puente del escáner — Archivo Ciencias UCV

**Carril dueño: `E1-escaner-puente`.** (`IN-067`: este directorio no tenía
dueño declarado; lo declara este documento.)

## Qué es y qué no es

Esto es un **lector de códigos de barras/QR en red local**, no un escáner de
documentos. No adquiere imágenes de papel: sólo captura el texto que emite un
lector USB (emulando teclado o por HID directo) y lo envía por WebSocket al
panel web del archivo, para autocompletar cédulas, RIF o signaturas.

La adquisición real de imagen (TWAIN/WIA/ICA/SANE, escáneres de red, captura
por móvil, OCR) está **sin implementar** — es la "Mitad 2" de
`docs/auditoria/digitalizacion-escaner.md`, que audita este componente
completo y diseña lo que falta. Ese documento es la fuente de verdad sobre
qué falta y por qué; este README documenta el estado actual del código.

## Arranque

```bash
npm install
npm start                 # equivalente a: node server.js
```

Por defecto escucha sólo en `127.0.0.1:3737`, en modo `stdin` (el lector
debe tener el foco de esta ventana, no el del navegador). Al arrancar sin
`SCANNER_TOKEN` en el entorno, genera un token de un solo arranque y lo
imprime en consola — cópialo en la configuración del escáner del panel web.

```
node server.js --port 4000                 # puerto personalizado
node server.js --mode hid --list-hid       # listar lectores HID conectados
node server.js --mode hid --hid-vid 0x05e0 --hid-pid 0x1900
node server.js --host 0.0.0.0 --origin https://archivo.ejemplo.edu
node server.js --cert cert.pem --key key.pem   # sirve wss:// en vez de ws://
node server.js --debug                      # logs completos, incluye códigos sin redactar
```

`--host` distinto de loopback **exige** al menos un `--origin` (o `--dev`
para pruebas locales): sin eso, el proceso se niega a arrancar. Es la
consecuencia de `DG-003`/`SI-172` — un token compartido no basta si
cualquier página en cualquier pestaña puede abrir el WebSocket.

## Protocolo

- `ws://<host>:<puerto>/?token=<TOKEN>` (o cabecera `X-Scanner-Token`).
  Primer mensaje del servidor: `{"type":"ready","v":1,"message":"…",
  "port":N,"version":"…"}`.
- Cada lectura: `{"type":"scan","code":"…","ts":"…","seq":N,"source":"device"
  |"test"}`. `source` distingue una lectura real de una simulada por
  `POST /test-scan` (`DG-004`) — el cliente puede (y en un flujo de
  producción, debe) tratarlas distinto.
- Latido: el servidor manda `ping` cada 20 s (marco de control WebSocket,
  no un mensaje JSON) y cierra al cliente que no responda con `pong` en el
  siguiente ciclo (`DG-006`/`DG-007`).
- REST: `GET /status`, `GET /config`, `GET /log` (códigos redactados salvo
  con `--debug`, `DG-013`/`SI-173`), `POST /test-scan`. Todos exigen el
  token; todos comprueban `Origin` contra la lista blanca.

## Qué se cerró en este carril (2026-09)

- `DG-003`/`SI-172` — verificación de `Origin` en WebSocket y en CORS del
  REST, con lista blanca por `--origin`; sin ella, sólo se admite loopback.
  (El token compartido ya se había añadido en un commit anterior, `c54c29c`;
  esto cierra la otra mitad: sin comprobar el origen, el token solo no
  alcanza porque cualquier pestaña abierta puede intentar la conexión.)
- `DG-004` — las lecturas simuladas por `/test-scan` llevan `source:"test"`.
- `DG-006`/`DG-007` — latido WebSocket y purga de clientes muertos.
- `DG-010` — `maxPayload` y tope de clientes simultáneos.
- `DG-013`/`SI-173` — cédulas redactadas en consola y en `GET /log` salvo
  con `--debug`.
- `DG-019`/`SI-171` (parcial) — soporte de `--cert`/`--key` para servir
  `wss://` real. No resuelve la falta de un certificado válido para el
  nombre que use el navegador — eso sigue siendo la decisión de arquitectura
  de más abajo — pero deja de ser una limitación del código.
- `DG-022`/`SI-174` — borrada la copia muerta `scanner-app/scanner-client.js`
  (399 líneas duplicando `app/static/scanner-client.js` con conducta
  distinta). El paquete no la referenciaba.
- `DG-024` — errores de red distintos de `EADDRINUSE` (p. ej. `EACCES`) ya
  no caen en un `console.error` genérico con el proceso a medio morir.
- `DG-025`/`DG-026` — `SIGTERM`/`SIGHUP` cierran el dispositivo HID, los
  sockets y el servidor de forma ordenada, no sólo `SIGINT`.
- `DG-027`/`DG-028` (parcial) — un error del dispositivo HID ya no deja el
  puente "mudo": se cierra y se reintenta la apertura cada 3 s. Sigue sin
  vigilar el *enchufado* de un dispositivo que nunca se abrió (eso pide
  `usb-detection` o sondeo periódico de `HID.devices()`, no incluido aquí).
- `DG-030`/`DG-031`/`DG-032`/`DG-033` — decodificador HID reescrito
  (`lib/hid-decoder.js`): tabla explícita con/sin Mayús en vez de aritmética
  ASCII (`ch -= 32` corrompía dígitos y símbolos con Mayús activo), lectura
  de las seis teclas de un mismo informe en vez de sólo `data[2]`, límite de
  tamaño de buffer y vaciado por inactividad (80 ms) además de por Intro.
- `DG-036`/`DG-170` — `npm test` (`node --test`), 18 casos sobre el
  decodificador HID, el analizador de argumentos y la redacción de códigos.
- `DG-037` — `package-lock.json` añadido al repositorio.
- `DG-039` — comprobación explícita de versión de Node al arrancar, con el
  motivo en el mensaje.
- `DG-041`/`DG-042` — `--port`, `--mode`, `--hid-vid`/`--hid-pid` se validan
  y el analizador de argumentos (`lib/args.js`) ya no confunde una bandera
  sin valor con el valor de la bandera anterior.
- `DG-044` — `npm run audit` (`npm audit`); la integración continua que lo
  dispare automáticamente no es de este carril (`.github/workflows/` no es
  de `scanner-app/`).
- `DG-164` — mensajes de consola reescritos en términos de causa y acción
  para quien digitaliza, no para quien programa (siguen existiendo mensajes
  técnicos adicionales bajo `--debug`).

## Qué se deja pendiente, y por qué

Los pendientes de la sección "Decisión de arquitectura" del documento de
auditoría (`DG-045` a `DG-061`: empaquetado con Electron, firma de código,
actualización automática) y toda la "Mitad 2" (adquisición TWAIN/WIA/ICA/
SANE — `DG-062` a `DG-084`; captura por móvil — `DG-085` a `DG-103`; OCR y
PDF buscable — `DG-104` a `DG-124`; lotes y cola — `DG-125` a `DG-140`;
preservación — `DG-141` a `DG-153`) son subsistemas nuevos y grandes, cada
uno de esfuerzo **L** según la propia auditoría, que requieren decisiones de
producto (¿Electron?, ¿qué motor de OCR?, ¿PDF/A desde cuándo?) y en varios
casos hardware o cuentas de pago (certificado de firma de código, cuenta de
desarrollador Apple) que no puede aprobar un agente. Implementarlos de
apuro, sin esa decisión escrita, produciría exactamente lo que la auditoría
critica del código actual: diseño sin usuarios reales verificados.

La recomendación técnica (**Electron + `electron-builder`, instalación por
usuario**) ya está argumentada en
`docs/auditoria/digitalizacion-escaner.md`, sección 3, con su contrapartida
de coste (~160 MB por instalación, firma de código anual, cuenta de
desarrollador Apple). Adoptarla es lo primero que debe hacer quien continúe
este carril — de ahí cuelgan cuarenta pendientes.

No hay todavía un inventario de qué escáneres existen realmente en la
Facultad de Ciencias (`DG-084`); es trabajo de campo, no de código, y
condiciona qué camino de adquisición (TWAIN, WIA, SANE, eSCL de red, carpeta
vigilada) conviene implementar primero.

## Plataformas

- **Windows**: modo `stdin` funciona sin nada adicional. Modo HID necesita
  `node-hid`, que compila un módulo nativo — si `npm install` falla en esa
  dependencia opcional, el modo `stdin` sigue disponible.
- **Linux**: el modo HID sobre `hidraw` normalmente necesita una regla de
  `udev` para acceso sin root; no incluida todavía (`DG-035`).
- **macOS**: leer un dispositivo de entrada exige el permiso *Input
  Monitoring*, concedido a mano en Preferencias del Sistema; no hay diálogo
  previo que lo explique (`DG-035`).

## Pruebas

```bash
npm test
```

Cubre `lib/hid-decoder.js` (tabla de teclado, modificadores, informes con
varias teclas), `lib/args.js` (analizador de argumentos) y `lib/redact.js`
(redacción de códigos). No hay pruebas de extremo a extremo del servidor
WebSocket/HTTP en esta pasada — sí un arranque manual verificado
(`GET /status` con token válido → 200, con token inválido → 401).
