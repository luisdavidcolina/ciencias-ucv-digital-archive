# Reservas de carril

**Se reserva ANTES de escribir la primera línea.** Si el carril que quieres ya tiene una
reserva `en curso` a nombre de otro, no lo cojas: coge el siguiente libre.

Tu nombre tiene que ser **único**. Si dos agentes van con el mismo nombre, la reserva de uno
le aparece al otro como propia y este fichero deja de proteger nada. Si no te han dado
nombre, pídelo antes de escribir aquí.

Al terminar, cambia tu estado a `terminado` y añade el sha del último commit.

| Carril | Agente | Desde | Estado | Commit |
|---|---|---|---|---|
| W0 | agente-w0-hemorragia | 2026-09-03 | terminado | c54c29c |
| W1 | agente-w1-redseguridad | 2026-09-03 | terminado (parcial: IN-001) | 3851a1e |
| W1b | agente-w1b-fixture-ci | 2026-09-03 | terminado | f2a5b10 |
| O1 (H1c-autorizacion) | agente-o1-autorizacion | 2026-09-03 | terminado | 9ff1fc1 |
| O2 (H1b-conexion) | agente-o2-transacciones | 2026-09-03 | terminado | defd015 |
| C1-docs-backend | agente-c1-docs | 2026-09-03 | terminado | 66c0b2b |
| C2-catalogo | agente-c2-catalogo | 2026-09-03 | terminado | 28bbff1 |
| C3-stats-backend | agente-c3-stats | 2026-09-03 | terminado | 49e755e (ver nota en _BUZON.md: primer intento de commit, d71096c, quedo mal formado por una carrera de git concurrente con A4-rrhh-backend) |
| C4-retencion | agente-c4-retencion | 2026-09-03 | terminado | 249dba7 (ver nota en _BUZON.md: commit compartido por una carrera de git concurrente con C2-catalogo, contenido correcto) |
| C6-usuarios-backend | agente-c6-usuarios | 2026-09-03 | terminado | d4f4ae8 (test_autorizacion_usuarios.py cayó en el commit 28bbff1 de C2-catalogo por la misma carrera de git concurrente; ver nota en _BUZON.md, contenido correcto) |
| C7-papelera | agente-c7-papelera | 2026-09-03 | terminado | 42b90c5 |
| A2-archivo-backend | agente-a2-archivo-backend | 2026-09-03 | terminado | 19e86f1 (ver nota en _BUZON.md: commit compartido por una carrera de git concurrente, contenido correcto) |
| A4-rrhh-backend | agente-a4-rrhh-backend | 2026-09-03 | terminado | d71096c (ver nota en _BUZON.md: commit compartido por una carrera de git concurrente, contenido correcto) |
| B4-admin-tabs | agente-b4-admin-tabs | 2026-09-03 | terminado | 6c55738 (ver nota en _BUZON.md: primer intento de commit, 88d0089, quedó mal formado por una carrera de git concurrente con D1-ia-backend; contenido correcto) |
| B5-admin-monitor | agente-b5-admin-monitor | 2026-09-03 | terminado | 5228225 |
| B6-admin-ui | agente-b6-admin-ui | 2026-09-03 | terminado | 0ebba97 |
| B7-admin-submit | agente-b7-admin-submit | 2026-09-03 | terminado | e98de09 |
| B8-admin-edit-archivo | agente-b8-admin-edit-archivo | 2026-09-03 | terminado | 7e2afb6 (ver nota en _BUZON.md: commit compartido por una carrera de git concurrente con "chore: reservar H3-pruebas-legacy...", contenido correcto) |
| B9-admin-edit-rrhh | agente-b9-admin-edit-rrhh | 2026-09-03 | terminado | 2fcb9d4 (ver nota en _BUZON.md: OR-005/OR-006 quedaron en un commit compartido por una carrera de git concurrente con B12-admin-usuarios, contenido correcto; OR-009 no era de este carril, redirigido a C7-papelera, ya resuelto en 42b90c5); 147e0fc documenta la carrera |
| B10-admin-charts | agente-b10-admin-charts | 2026-09-03 | terminado | 3b6001e (ver nota en _BUZON.md: commit compartido por una carrera de git concurrente con B6-admin-ui, contenido correcto) |
| B11-admin-categorias | agente-b11-admin-categorias | 2026-09-03 | terminado | f0359bc |
| B12-admin-usuarios | agente-b12-admin-usuarios | 2026-09-03 | terminado | 2fcb9d4 (ver nota en _BUZON.md: commit compartido por una carrera de git concurrente, contenido de admin-users.js correcto) |
| D1-ia-backend | agente-d1-ia-backend | 2026-09-03 | terminado | a3052c6 (ver nota en _BUZON.md: commit compartido por una carrera de git concurrente con C10-ficheros-r2, contenido correcto) |
| D2-ia-frontend | agente-d2-ia-frontend | 2026-09-03 | terminado | 2fcb9d4 (ver nota en _BUZON.md: commit ajeno de agente-b12-admin-usuarios que arrastró mis tres archivos por una carrera de git; contenido correcto — SD-041, SI-017, SI-005) |
| C9-auth | agente-c9-auth | 2026-09-03 | terminado | de9f6cf (primer intento, 9244e45, quedó absorbido por una carrera de git con F1-paginas-estaticas — ver nota en _BUZON.md; de9f6cf sí contiene mi trabajo, verificado con git show/grep) |
| C10-ficheros-r2 | agente-c10-ficheros-r2 | 2026-09-03 | terminado | a3052c6 (ver nota en _BUZON.md: commit compartido por una carrera de git concurrente con D1-ia-backend, contenido correcto) |
| F1-paginas-estaticas | agente-f1-paginas | 2026-09-03 | terminado | 9244e45 (ver nota en _BUZON.md: commit compartido por una carrera de git concurrente con C9-auth, contenido correcto) |
| F2-cascara | agente-f2-cascara | 2026-09-03 | terminado | 251a141 |
| E2-escaner-cliente | agente-e2-escaner-cliente | 2026-09-03 | terminado | 56e7be0725f24bdf4e61442b57eb0525b0f26fa |
| H3-pruebas-legacy | agente-h3b-legacy-fixtures | 2026-09-03 | terminado | a4d7913 (test_admin.py en b1793c2) |
| C5-importaciones | agente-c5b-importaciones | 2026-09-03 | terminado | be77391 (ver nota en _BUZON.md: commit ajeno de agente-c8b-backup que arrastró imports.py y test_imports.py por una carrera de git concurrente; contenido correcto, verificado con git diff y pytest en verde) |
| C8-backup | agente-c8b-backup | 2026-09-03 | terminado | f989f19 |
| BR-109 | agente-br109-dossier-estilos | 2026-09-03 | terminado | 6284460 |
| L0-tokens | agente-l0-tokens | 2026-09-03 | terminado | 3b6e18b (ver nota en _BUZON.md: commit ajeno de agente-h2-app-js que arrastró mi styles.css por una carrera de git concurrente; contenido correcto, verificado con git show) |
| H2-app-js | agente-h2-app-js | 2026-09-03 | terminado | 5b566e4 |
| verificacion-despliegue | agente-verificacion-despliegue | 2026-09-03 | terminado | 4f1f52d |
| L1-estilos | agente-l1-estilos | 2026-09-03 | terminado | b93bdb4 (ver nota en _BUZON.md: mi trabajo quedó arrastrado por el commit de agente-l2-estilos por una carrera de git concurrente en el árbol compartido, contenido verificado con `git show b93bdb4 -- app/static/styles.css`) |
| L2-estilos | agente-l2-estilos | 2026-09-03 | terminado | b93bdb4 |
| L3-estilos | agente-l3-estilos | 2026-09-03 | terminado | 6ca0fc7 |
| L4-estilos | agente-l4-estilos | 2026-09-03 | terminado | 7de4937 (ver nota en _BUZON.md: mi trabajo quedó arrastrado por el commit de agente-l10-estilos por una carrera de git concurrente en el árbol compartido, contenido verificado con `git show 7de4937 -- app/static/styles.css`) |
| L5-estilos | agente-l5-estilos | 2026-09-03 | terminado | ada29aa (ver nota en _BUZON.md: commit compartido por una carrera de git concurrente, contenido correcto) |
| L6-estilos | agente-l6-estilos | 2026-09-03 | en curso | |
| L6-estilos | agente-l6b-estilos-focused | 2026-09-03 | terminado (95fc341) | |
| LT | agente-lt-tests-vercel | 2026-09-03 | terminado | d85a26f |
| LX | agente-lx-reestructuracion | 2026-09-03 | terminado (parcial: SD-187, ver nota en _BUZON.md) | ee18da7 |
| LG | agente-lg-galeria | 2026-09-03 | terminado (SD-223: `/sistema` con tokens de L0 en vivo y los componentes reescritos hasta ahora; enlace de menú lateral pendiente, ver nota en `_BUZON.md` — requiere `app.js`, fuera de este carril) | `72b7e7e` |
| E1-escaner-puente | agente-e1-escaner-puente | 2026-09-03 | terminado (parcial: 24 de 107 fichas — seguridad/protocolo/decodificador HID/pruebas de `scanner-app/server.js`; el resto son subsistemas nuevos de esfuerzo L —Electron, TWAIN/WIA/ICA/SANE, OCR, lotes, preservación— sin decisión de producto tomada, documentados en `scanner-app/README.md`) | 0eaf779 |
| H1a-migraciones | agente-h1a-migraciones | 2026-09-03 | terminado | `2828d16` |
| H1d-modelos | agente-h1d-modelos | 2026-09-03 | terminado | ab84ecc |
| H1e-consultas | agente-h1e-consultas | 2026-09-03 | terminado | 9a3a2e3 |
| H1f-rutas-pagina | agente-h1f-rutas-pagina | 2026-09-03 | terminado (sin cambios de código: `pages.py`/`config.py` ya cumplían, todo lo pendiente es `[CHOCA]` y quedó anotado en `_BUZON.md`) | e5a7696 |
| H4-despliegue | agente-h4-despliegue | 2026-09-03 | terminado (parcial: 6 de 37 fichas resueltas de config/despliegue real; el resto exige main.py/database.py/backup.py/.python-version/CI/paneles externos, ver nota en _BUZON.md) | 8a98120 |
| A1-buscador-archivo | agente-a1-buscador-archivo | 2026-09-03 | terminado | 795cd42 |
| A3-buscador-rrhh (hr.js) | agente-a3a-buscador-rrhh-js | 2026-09-04 | terminado (interrumpido por corte de sesion, retomado por el orquestador: comiteado tal cual quedo, ver ab3be26) | ab3be26 |
| A3-buscador-rrhh (hr.html) | agente-a3b-buscador-rrhh-html | 2026-09-04 | terminado | db8679e |
| PASS2-admin-edit | agente-pass2-admin-edit | 2026-09-04 | terminado (OA-046, OR-037, OR-147, OR-219; resto verificado ya resuelto o requiere `[CHOCA]`, ver _BUZON.md) | |
| PASS2-docs-hr-backend | agente-pass2-docs-hr-backend | 2026-09-04 | terminado (parcial: sólo `hr.py` tenía tickets abiertos y accionables en solitario — BR-003, BR-005, BR-006, BR-015, BR-059, BR-062, BR-057; `docs.py` no tenía pendientes sin marcar accionables sólo con ese archivo, ver nota en `_BUZON.md`) | 3e9dc0d |
| PASS2-archive-depth | agente-pass2-archive-depth | 2026-09-04 | terminado (BA-005, BA-015, BA-016, BA-026, BA-027, BA-161, BA-167; ver nota en _BUZON.md) | 13bc2ce |
| PASS2-engineering | agente-pass2-engineering | 2026-09-04 | terminado | f264057 |
| PASS2-ai | agente-pass2-ai | 2026-09-04 | terminado (SI-019, SI-020, SI-061, SI-066; resto confirmado ya resuelto o documentado como pendiente en _BUZON.md) | 20548d9 (mi commit quedó absorbido por una carrera de git concurrente con PASS2-archive-depth; contenido verificado con `git show 20548d9:app/core/ai_tools.py`) |
| PASS2-hr-frontend-depth | agente-pass2-hr-frontend-depth | 2026-09-04 | terminado | 15ede7f |
| PASS2-models-schema | agente-pass2-models-schema | 2026-09-04 | terminado (sin cambios de código: RQ-009/RQ-010/RQ-014/RQ-016/RQ-028/RQ-041/RQ-044/RQ-049 revisados contra `models.py`/`schema.sql` — todo lo accionable ya estaba resuelto por H1d-modelos, el resto exige `main.py`/`hr.py`/`admin/*.py` fuera de zona; hallazgo real de RQ-049 documentado en `_BUZON.md`) | bbb2442 |
| SWEEP-styles | agente-sweep-styles | 2026-09-03 | terminado | d07452e |
| LX-2 | agente-lx2-modularizacion | 2026-09-03 | terminado (parcial: solo SD-214 hecho; SD-211/212/213/206 sin tocar, bloqueo real y numeros actualizados en _BUZON.md) | baebd7c |
| LX-3 | agente-lx3-modularizacion | 2026-09-04 | terminado (SD-213 completo: 12 modulos reales bajo app/static/styles/, mas un bug real de @layer que invalidaba los @import corregido por el orquestador tras el corte de sesion — ver commit; SD-212/SD-211/SD-206 quedan para otra pasada) | 9991649 |
| LX-4 | agente-lx4-orden-important | 2026-09-04 | terminado (parcial: SD-212 solo `componentes.css` — comentarios de sección sin mover reglas de sitio, por el riesgo de empate de especificidad; SD-206 solo `componentes-admin.css` — el bloque de acento por tema de 11 selectores repetidos por propiedad consolidado con `:is()`, misma especificidad; SD-211 solo la familia `.card`/`.card-header`/`.card-title`/`.card-body`/`.card-outline` en `componentes.css`, 14 `!important` retirados tras confirmar que Bootstrap vive en su propia `@layer` (SD-187) y la capa `app` ya gana por orden de capa sin necesitarlo. Detalle completo y alcance restante en `_BUZON.md`, sección LX-4) | 7d120d8 |
| LX-5 | agente-lx5-important-resto | 2026-09-04 | terminado (parcial: 29 `!important` retirados de 613 en los 10 modulos restantes — base.css 2, shell.css 8, impresion.css 2, componentes-admin.css 14 (resto, fuera del acento de tema que ya hizo LX-4), paneles-admin.css 3; quedan 584. tokens.css y menus.css ya estaban en 0. responsive.css, personalizacion.css, dark-mode.css y paginas.css sin cambios: sus `!important` compiten con clases de utilidad de Bootstrap (`.bg-*`, `.text-*`, `.d-flex`...), que SI llevan `!important` en Bootstrap 4.6 — ahi quitarlo invierte quien gana, no es el mismo caso que `.card`/`.btn`/`.navbar`/`.form-group` sin `!important` en origen. Tambien se dejaron intactos los bloques `body.theme-*`/`body.dark-mode`, los empates de especificidad documentados en el propio CSS (SD-093/SD-095/SD-105/SD-130), flatpickr (libreria sin `@layer`) y las sobre-escrituras de `style` en linea. Detalle completo en el informe final del agente) | 917f9d8, d29abed, 80c2da6, fc0ac99 |
| SWEEP-main | agente-sweep-main | 2026-09-03 | terminado | 5ac751c |
| SWEEP2-styles | agente-sweep2-styles | 2026-09-04 | terminado (sin cambios de código: las 3 entradas sin marcar con archivo `app/static/styles.css` — línea 403 OA-201/OR-234/OR-077/OA-210/OR-280, línea 662 OR-241, línea 1451 L14-estilos — exigen todas HTML/JS/backend fuera de mi zona (`app/static/styles/*.css`), ya documentado por agentes previos CSS-only; ver nota en _BUZON.md) | 3e0e9a6 |
| SWEEP2-admin-html | agente-sweep2-admin-html | 2026-09-04 | terminado (parcial: 5 de 9 peticiones sin marcar resueltas — SD-138, OR-009 y OA-181 confirmadas ya cerradas por otros carriles, 2 entradas duplicadas marcadas; el resto exige backend/JS/CSS fuera de zona, ver nota en `_BUZON.md`) | 80b1a8c |
| SWEEP2-admin-monitor-js | agente-sweep2-admin-monitor-js | 2026-09-04 | terminado | e7f5690 |
| SWEEP2-main-schema | agente-sweep2-main-schema | 2026-09-04 | terminado (IN-034, SI-031 parte de schema.sql) | ada3784 |
| SWEEP2-backup-docs-deps-ai | agente-sweep2-backup-docs-deps-ai | 2026-09-04 | terminado (parcial: 1 de ~8 pendientes sin marcar era realmente accionable sólo con mis cuatro archivos — OA-104/OR-128 en `docs.py`; el resto exigía tocar `main.py`/`admin/users.py`/`storage.py`/HTML ajenos o `deps.py` con cambios de autorización que preferí no arriesgar, ver `_BUZON.md`) | 03fa263 (ver nota en _BUZON.md: commit compartido por una carrera de git concurrente con SWEEP2-styles, contenido correcto) |
| SWEEP2-shell-app | agente-sweep2-shell-app | 2026-09-04 | terminado (2 peticiones sin marcar dirigidas a app.js/app-shell.js: SI-044/SI-046 resueltas y SI-043 parcial en app.js; RQ-053 en app-shell.js sigue bloqueada, `inicio.html` no existe todavía, ver _BUZON.md) | 21bf3d3 |
| SWEEP2-misc | agente-sweep2-misc | 2026-09-04 | terminado (3 de 9 tickets del buzón eran falsos positivos ya cerrados por otros carriles — DG-169, SI-235, C4-retencion/test_misc.py, marcados `[x]`; BA-112 en `archive.html` ya estaba resuelto; SI-113/115/116/117/119/122/125 aplicadas en `ai-widget.js`; login.html/ayuda.html/hr.html/scanner-client.js no tenían pendientes accionables sólo con mi archivo, ver `_BUZON.md`) | 142b240 |
| SWEEP-admin-docs | agente-sweep-admin-docs | 2026-09-03 | terminado | f5cfa7f |
| SWEEP3-rutas-backend | agente-sweep3-rutas-backend | 2026-09-04 | terminado (parcial: 1 de ~8 pendientes sin marcar en mis siete archivos era realmente accionable de bajo riesgo — IN-156 en auth.py; el resto exige tocar HTML/JS ajeno o es decision de producto/backend nuevo de esfuerzo M, ver `_BUZON.md`) | a459cb4 |
| SWEEP3-html-js-resto | agente-sweep3-html-js-resto | 2026-09-04 | terminado (parcial: 6 fichas confirmadas ya resueltas por commits previos — OA-015, OA-055, OA-053, OR-009, bloque login SI-032/034/038/049-052/054 — sin cambios de código; resto de lo sin marcar exige backend/styles.css/admin.js/app.js ajenos, ver nota en _BUZON.md) | 5da87d6 |
| SWEEP-admin-html | agente-sweep-admin-html | 2026-09-03 | terminado | 77c8d74 |
| SWEEP-monitor-backup | agente-sweep-monitor-backup | 2026-09-03 | terminado (parcial: 3 de ~10 peticiones resueltas en stats.py/backup.py; admin-monitor.js y admin/deps.py sin peticiones accionables, ver nota en _BUZON.md) | b278648 |
| SWEEP-shell-core | agente-sweep-shell-core | 2026-09-03 | terminado | (ver commit en _BUZON.md) |
| SWEEP-admin-js | agente-sweep-admin-js | 2026-09-03 | terminado | 1651fa3 |
| SWEEP-misc-paginas | agente-sweep-misc-paginas | 2026-09-03 | terminado | b59ee5c |
| SWEEP-tests-nuevos | agente-sweep-tests-nuevos | 2026-09-03 | terminado | ff4c076 (nota en `_BUZON.md` quedó en el commit `5ac751c` de agente-sweep-main por una carrera de git concurrente, contenido correcto) |
| L7-estilos | agente-l7-estilos | 2026-09-03 | terminado | 2bc5690 (ver nota en _BUZON.md: mi cambio de styles.css quedó arrastrado por el commit de agente-la-asistente por una carrera de git concurrente en el árbol compartido, contenido verificado con `git show 2bc5690 -- app/static/styles.css`) |
| L8-estilos | agente-l8-estilos | 2026-09-03 | terminado | ada29aa |
| L9-estilos | agente-l9-estilos | 2026-09-03 | terminado | 6ca0fc7 (ver nota en _BUZON.md: mi trabajo quedó absorbido por el commit "L3-estilos" de otro agente por una carrera de git concurrente en el árbol compartido; mi propio intento de commit, 7194678, sólo capturó 1 línea residual. Contenido verificado con grep de mis marcadores SD- en HEAD tras 6ca0fc7 y con `python -m pytest app/tests -q` en 713/0) |
| L10-estilos | agente-l10-estilos | 2026-09-03 | terminado | 7de4937 (ver nota en _BUZON.md: commit tomó el árbol de trabajo compartido con otros lotes L1-L15 editando styles.css a la vez, mi cambio verificado correcto dentro de él) |
| L11-estilos | agente-l11-estilos | 2026-09-03 | terminado | b93bdb4 (ver nota en _BUZON.md: mi trabajo quedó arrastrado por el commit de agente-l2-estilos por una carrera de git concurrente en el árbol compartido, contenido verificado con `git log -S"ds-accent-ink"` + `git show b93bdb4:app/static/styles.css`) |
| L12-estilos | agente-l12-estilos | 2026-09-03 | terminado | 7de4937 (ver nota en _BUZON.md: mi trabajo quedó arrastrado por el commit de agente-l10-estilos por una carrera de git concurrente en el árbol compartido, contenido verificado con `git show 7de4937:app/static/styles.css`) |
| L13-estilos | agente-l13-estilos | 2026-09-03 | terminado | 7de4937 (ver nota en _BUZON.md: mi cambio en la zona "CAPA DE MOVIMIENTO" quedó arrastrado por el commit de agente-l10-estilos por una carrera de git concurrente en el árbol compartido, contenido verificado con `git show 7de4937 -- app/static/styles.css`) |
| L14-estilos | agente-l14-estilos | 2026-09-03 | terminado | 2bc5690 (ver nota en _BUZON.md: mi trabajo quedó arrastrado por el commit de agente-la-asistente por una carrera de git concurrente en el árbol compartido, contenido verificado con `git show 2bc5690:app/static/styles.css` — sigue vivo en HEAD `ada29aa`) |
| L15-estilos | agente-l15-estilos | 2026-09-03 | terminado | 7de4937 (ver nota en _BUZON.md: commit compartido por una carrera de git concurrente con L10-estilos, contenido correcto) |
| LH-paginas | agente-lh-paginas | 2026-09-03 | terminado (parcial: SD-026, SD-027, SD-028, SD-209) | a02a2ef |
| LA-asistente | agente-la-asistente | 2026-09-03 | terminado | 65fdf78 (ver nota en _BUZON.md: commit ajeno de agente-verificacion-despliegue que arrastró mi ai-widget.css por una carrera de git concurrente, contenido correcto) |
| LW-www | — | 2026-09-03 | terminado | ya resuelto en 9244e45 (F1-paginas-estaticas borró www/styles.css) |

| PASS3-rrhh-backend-untouched | agente-pass3-rrhh-backend | 2026-09-04 | terminado (interrumpido por limite de sesion, rescatado y comiteado por el orquestador tras verificar sintaxis y pytest: OR-021/022/023, BR-063/064/065/067/068/069, OR-013/014/037/038/044, OA-006/007, OA-042) | 54237d8 |
| PASS3-catalog-retention-imports | agente-pass3-catalog-retention-imports | 2026-09-04 | terminado (interrumpido por limite de sesion, rescatado por el orquestador; imports.py/helpers.py sin cambios en lo rescatado) | 79472a6 |
| PASS3-admin-js-untouched | agente-pass3-admin-js-untouched | 2026-09-04 | terminado (interrumpido por limite de sesion, rescatado por el orquestador: admin-ui.js y admin-users.js; admin.js/admin-submit.js/admin-stats.js/admin-categories.js sin cambios en lo rescatado — relanzar si hace falta profundizar) | 05e812b |
| PASS3-styles-buzon-2 | agente-pass3b-styles-buzon | 2026-09-05 | terminado (sin cambios de código: reverificadas las mismas 11 entradas sin marcar de `app/static/styles.css`, 0 pasaron a accionable — ver nota en `_BUZON.md`) | |
| PASS3-admin-html-depth-2 | agente-pass3b-admin-html-depth | 2026-09-05 | en curso | |
| PASS3-admin-js-resto | agente-pass3-admin-js-resto | 2026-09-05 | en curso | |
| PASS3-login-scanner-js | agente-pass3-login-scanner-js | 2026-09-04 | terminado (SI-175 corregido — id de selector muerto; SI-177/DG-162 parcial — accesibilidad de teclado; login.js sin cambios, ya resuelto por C9-auth; resto documentado en _BUZON.md) | b68a951 |

## Cómo se escribe una fila

**Nota de orquestación (2026-09-04)**: este bloque de ejemplo tenía la fila
`A3-buscador-rrhh | bruno | ...` como texto de muestra, con un sha de relleno
(`a1b2c3d`). Varios agentes la confundieron con una reserva real y respetaron
el carril "ocupado" durante más de 30 horas sin que nadie lo estuviera
trabajando de verdad. Limpiado: el ejemplo ya no usa un nombre de carril real
para no repetir la confusión.

```
| <carril> | <tu-nombre-de-agente> | <fecha> | en curso | |
| <carril> | <tu-nombre-de-agente> | <fecha> | terminado | <sha-del-commit> |
```

## Carriles que no se cogen sin permiso

- `W0`, `W1`, `H1c`, `H1b` — van primero y solos; el resto depende de ellos.
- `LX` — va el último de todos: parte `styles.css` en módulos e invalida los rangos de línea
  con los que trabajan los demás lotes de diseño.
- `L0` y `BR-109` — van antes que cualquier otro trabajo estético.
