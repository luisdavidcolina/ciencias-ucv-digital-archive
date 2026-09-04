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
| E1-escaner-puente | agente-e1-escaner-puente | 2026-09-03 | terminado (parcial: 24 de 107 fichas — seguridad/protocolo/decodificador HID/pruebas de `scanner-app/server.js`; el resto son subsistemas nuevos de esfuerzo L —Electron, TWAIN/WIA/ICA/SANE, OCR, lotes, preservación— sin decisión de producto tomada, documentados en `scanner-app/README.md`) | 0eaf779 |
| H1a-migraciones | agente-h1a-migraciones | 2026-09-03 | terminado | `2828d16` |
| H1d-modelos | agente-h1d-modelos | 2026-09-03 | terminado | ab84ecc |
| H1e-consultas | agente-h1e-consultas | 2026-09-03 | terminado | 9a3a2e3 |
| H1f-rutas-pagina | agente-h1f-rutas-pagina | 2026-09-03 | terminado (sin cambios de código: `pages.py`/`config.py` ya cumplían, todo lo pendiente es `[CHOCA]` y quedó anotado en `_BUZON.md`) | e5a7696 |
| H4-despliegue | agente-h4-despliegue | 2026-09-03 | terminado (parcial: 6 de 37 fichas resueltas de config/despliegue real; el resto exige main.py/database.py/backup.py/.python-version/CI/paneles externos, ver nota en _BUZON.md) | 8a98120 |
| A1-buscador-archivo | agente-a1-buscador-archivo | 2026-09-03 | terminado | 795cd42 |
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

## Cómo se escribe una fila

```
| A3-buscador-rrhh | bruno | 2026-09-02 14:20 | en curso | |
| A1-buscador-archivo | agente-a1-buscador-archivo | 2026-09-03 | en curso | |
| B1-admin-archivo-html | agente-b1-admin-archivo-html | 2026-09-03 | terminado | 4be45d3 |
| B2-admin-rrhh-html | agente-b2-admin-rrhh-html | 2026-09-03 | terminado | 7cb1a0a |
| B3-admin-sistema-html | agente-b3-admin-sistema-html | 2026-09-03 | terminado (parcial: 29/114, ver nota en _BUZON.md) | d5477fe |
| E1-escaner-puente | agente-e1-escaner-puente | 2026-09-03 | en curso | |
| A3-buscador-rrhh | bruno | 2026-09-02 14:20 | terminado | a1b2c3d |
```

## Carriles que no se cogen sin permiso

- `W0`, `W1`, `H1c`, `H1b` — van primero y solos; el resto depende de ellos.
- `LX` — va el último de todos: parte `styles.css` en módulos e invalida los rangos de línea
  con los que trabajan los demás lotes de diseño.
- `L0` y `BR-109` — van antes que cualquier otro trabajo estético.
