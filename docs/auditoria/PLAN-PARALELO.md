# Plan de trabajo en paralelo — 1.863 pendientes, 37 carriles

Calculado, no repartido a ojo: se extrajo de las diez auditorías qué archivos toca cada
pendiente, se contó cuántos pendientes se disputan cada archivo, y cada pendiente quedó
asignado al carril del **archivo más específico** que toca. Los mapas están en
`_mapa_archivos.json`, `_asignacion.json`, `_compartidos.json` y `_carriles.json`.

## Las tres reglas que hacen que esto funcione

Son innegociables. Si se salta una, veinte agentes en paralelo destruyen más de lo que
construyen — ya pasó en `dmd-crm`, con dos compilaciones pisándose el mismo directorio y un
archivo a medias de otro dejando el trabajo en rojo.

### 1. Nunca se choca: un carril es dueño de sus archivos

**Un agente NUNCA escribe fuera de los archivos de su carril.** Ni una línea, ni «solo para
arreglar de paso esto otro». Si un pendiente necesita tocar un archivo de otro carril, no lo
toca: lo anota en `docs/auditoria/_BUZON.md` con su identificador y sigue con el siguiente.

Los archivos de cada carril son **exclusivos y disjuntos**: ningún archivo aparece en dos
carriles. Esa es la propiedad que se calculó y la que hay que preservar.

### 2. Se reserva ANTES de empezar, no después

**Antes de escribir una sola línea**, el agente reserva su carril completo en
`docs/auditoria/_RESERVAS.md`, con una línea:

```
<carril> · <nombre-del-agente> · <fecha y hora> · <estado: en curso | terminado>
```

Reservar es lo primero que se hace y lo primero que se comprueba: **si el carril ya tiene una
reserva en curso a nombre de otro, no se coge — se coge el siguiente libre.** Reservar
después de empezar no sirve de nada; para entonces ya hay dos agentes escribiendo el mismo
archivo.

### 3. Nombres únicos: dos agentes no pueden llamarse igual

Cada agente recibe un **nombre propio y distinto** al lanzarlo. Es lo único que distingue una
reserva de otra: si dos van como `claude`, el carril que reserva uno le aparece al otro como
propio y la regla 2 deja de proteger nada. Si un agente no tiene nombre asignado, **lo pide
antes de tocar el fichero de reservas**.

Con 1.863 pendientes y 37 carriles **caben 30 agentes a la vez sin colisión**, pero no desde
el primer minuto: hay dos olas que van antes y no son paralelizables.

---

## Las olas que bloquean todo lo demás

### W0 — Parar la hemorragia (1 agente, horas)
`IN-002` credenciales de R2 al entorno · escribir el `test_secrets.py` que `CLAUDE.md` da por
existente · `SI-156` cerrar `/investigacion` · `DG-029` y `DG-003`, el puente que captura el
teclado y escucha en `0.0.0.0`.
**En paralelo, y sólo lo puede hacer el dueño: rotar el token de R2 en Cloudflare.**

### W1 — La red de seguridad (1 agente, días). Sin esto nada de lo demás es verificable.
`SI-226` el fixture de pruebas que sobrescribe `require_session` y hace invisible cualquier
autorización ausente · pruebas contra base real en vez de mocks · `IN-001` que las migraciones
corran en Vercel (hoy `lifespan="off"` las deja muertas) · integración continua.

### O1 — Autorización de verdad (1 agente y luego 8 en abanico)
Un agente escribe la dependencia de autorización por rol y módulo en `deps.py`; después ocho
carriles de backend la aplican a sus rutas en paralelo. `IN-131` es una sola ausencia, no
cuarenta fallos.

### O2 — Transacciones (1 agente)
`IN-164`: hoy `db_query(commit=True)` es su propia unidad atómica. Toca `database.py`. Va
antes que importaciones y papelera, que es donde causa pérdida de datos.

---

## Los carriles

Cada uno con sus archivos en exclusiva, su número de pendientes y qué necesita antes.

| Carril | Qué es | Archivos en exclusiva | Antes | Nº |
|---|---|---|---|---|
| `A1-buscador-archivo` | Buscador de Archivo (pantalla) | `app/static/archive.js + archive.html` | - | 165 |
| `A2-archivo-backend` | Busqueda de Archivo (backend) | `app/routes/archive.py` | O1 | 9 |
| `A3-buscador-rrhh` | Buscador de RRHH (pantalla) | `app/static/hr.js + hr.html` | - | 84 |
| `A4-rrhh-backend` | Busqueda y expediente de RRHH (backend) | `app/routes/hr.py + hr_alerts.py` | O1 | 74 |
| `B1-admin-archivo-html` | Backoffice Archivo (marcado) | `app/static/admin_archive.html` | - | 21 |
| `B10-admin-charts` | Graficos y KPIs | `admin-charts.js + viz-tokens.js + admin-stats.js` | L0 | 32 |
| `B11-admin-categorias` | Tipos y categorias | `app/static/admin-categories.js` | - | 18 |
| `B12-admin-usuarios` | Usuarios (pantalla) | `app/static/admin-users.js` | - | 21 |
| `B2-admin-rrhh-html` | Backoffice RRHH (marcado) | `app/static/admin_hr.html` | - | 16 |
| `B3-admin-sistema-html` | Admin Global (marcado) | `app/static/admin_system.html` | W1 + VI-001 | 114 |
| `B4-admin-tabs` | Conmutador de pestanas | `app/static/admin.js` | - | 29 |
| `B5-admin-monitor` | Tabla del monitor | `app/static/admin-monitor.js` | - | 38 |
| `B6-admin-ui` | Utilidades de interfaz del panel | `app/static/admin-ui.js` | - | 31 |
| `B7-admin-submit` | Alta de documentos y empleados | `app/static/admin-submit.js` | - | 32 |
| `B8-admin-edit-archivo` | Edicion y papelera de Archivo | `app/static/admin-edit.js` | - | 20 |
| `B9-admin-edit-rrhh` | Edicion y papelera de RRHH | `app/static/admin-edit-hr.js` | - | 21 |
| `C1-docs-backend` | Documentos (backend) | `app/routes/admin/docs.py` | O1 | 22 |
| `C10-ficheros-r2` | Ficheros, R2 y enlaces compartidos | `files.py + storage.py + share.py + compartido.html` | W0 | 46 |
| `C2-catalogo` | Catalogo de tipos (backend) | `app/routes/admin/catalog.py` | O1 | 21 |
| `C3-stats-backend` | Cifras del tablero | `app/routes/admin/stats.py` | O1 | 19 |
| `C4-retencion` | Retencion y disposicion | `app/routes/admin/retention.py` | O1 | 18 |
| `C5-importaciones` | Importacion CSV | `app/routes/admin/imports.py` | O1 + O2 | 18 |
| `C6-usuarios-backend` | Usuarios (backend) | `app/routes/admin/users.py` | O1 | 12 |
| `C7-papelera` | Papelera y versiones | `app/routes/trash.py` | O1 + O2 | 22 |
| `C8-backup` | Copias de seguridad | `app/routes/backup.py` | O1 | 19 |
| `C9-auth` | Login y sesion | `app/routes/auth.py + login.js + login.html` | O1 | 19 |
| `D1-ia-backend` | Asistente (backend) | `app/routes/ai.py + app/core/ai*.py` | O1 | 59 |
| `D2-ia-frontend` | Asistente (pantalla) | `ai-widget.js + ai-widget.css + admin_ai.html` | L0 | 3 |
| `E1-escaner-puente` | Puente del escaner | `scanner-app/*` | W0 | 107 |
| `E2-escaner-cliente` | Cliente del escaner en la app | `app/static/scanner-client.js` | E1 | 30 |
| `F1-paginas-estaticas` | Ayuda, investigacion y www | `ayuda.html + investigacion.html + www/styles.css` | W0 | 9 |
| `F2-cascara` | Cascara, temas y desplegables | `app-shell.js + app-theme.js + app-choices.js` | - | 8 |
| `H2-app-js` | Nucleo del frontend | `app/static/app.js + app-core.js` | - | 30 |
| `H4-despliegue` | Despliegue y arranque | `vercel.json + api/*` | W1 | 37 |

**Subtotal: 1224 pendientes en 34 carriles que no colisionan entre sí.**

---

## Los dos carriles que hay que partir

Concentran 639 pendientes sobre archivos únicos, así que no admiten un agente por pendiente:
admiten los que quepan sin pisarse dentro del mismo archivo.

### `G1-estilos` — 346 pendientes sobre `app/static/styles.css`

Es **un solo archivo de 3.973 líneas**: aquí no caben veinte agentes por mucho que haya
veinte tareas. `sistema-diseno.md` ya trae **22 lotes** calculados para esto:

- **`L0` va primero y solo.** Añade los tokens al principio del archivo; todo lo demás los usa.
- **`L1`-`L15` van por rangos de línea** y admiten **4-6 agentes a la vez** sin conflicto,
  cada uno en su rango.
- **`LH` (páginas), `LA` (asistente), `LW` (`www/`), `LT` (pruebas), `LG` (galería)** son
  archivos distintos: esos sí van en paralelo con los anteriores.
- **`LX` va el último.** Parte el archivo en módulos e invalida todos los rangos: si entra
  antes, rompe el trabajo de los demás.

**Y antes que todo lo estético de RRHH va `BR-109`**: el dossier se pinta con ~100 atributos
`style` en línea, y hasta que salgan de ahí el modo oscuro, los temas y la densidad no tienen
dónde engancharse. Un agente, primero, solo.

### `H1-nucleo` — 293 pendientes sobre el corazón compartido

Se subdivide en seis, y **sólo los tres últimos van en paralelo**:

| Sub-carril | Archivos | Cuándo |
|---|---|---|
| `H1a-migraciones` | `main.py` (migraciones), `schema.sql` | tras W1 |
| `H1b-conexion` | `database.py` | es O2, antes de C5 y C7 |
| `H1c-autorizacion` | `routes/admin/deps.py`, `core/security.py` | es O1, antes del abanico |
| `H1d-modelos` | `models.py` | en paralelo |
| `H1e-consultas` | `routes/lookups.py`, `utils.py`, `core/cache.py` | en paralelo |
| `H1f-rutas-pagina` | `routes/pages.py`, `core/config.py` | en paralelo |

`app/main.py` es el archivo más disputado del proyecto: **81 pendientes de otros carriles
necesitan tocarlo**. Ninguno lo toca. Lo anotan en el buzón y `H1a` los aplica en tandas.

---

## Cómo queda con 30 agentes

| Ola | Agentes | Qué |
|---|---|---|
| 1 | 1 | `W0` parar la hemorragia (horas) |
| 2 | 1 | `W1` la red de seguridad (días). El dueño rota el token en paralelo |
| 3 | 2 | `H1c` autorización + `H1b` transacciones |
| 4 | **30** | los 35 carriles de la tabla en abanico, más `L0` y `BR-109` |
| 5 | **30** | estética: `L1`-`L15` (4-6 a la vez) + `LH`/`LA`/`LW`/`LT`/`LG` + resto de carriles |
| 6 | 1 | `LX` partir `styles.css` en módulos, el último |

---

## Instrucciones que lleva cada agente

1. **Tu nombre es único.** Si no te han dado uno, pídelo antes de tocar nada.
2. **Reserva tu carril en `_RESERVAS.md` ANTES de escribir la primera línea.** Si ya está
   reservado en curso por otro nombre, no lo cojas: coge el siguiente libre. Al terminar,
   marca la reserva como terminada.
3. Tu carril, tus archivos, tu lista de identificadores. **No escribes fuera de tus archivos**,
   ni siquiera para arreglar algo evidente que ves de paso.
4. Lee la ficha completa del pendiente en su auditoría antes de tocar nada: cada una trae el
   escenario de fallo y qué debe pasar.
5. Si necesitas un archivo que no es tuyo, **no lo tocas**: lo anotas en `_BUZON.md` con el
   identificador y sigues.
6. `python -m pytest app/tests -q` tiene que pasar antes de dar nada por terminado.
7. Lo del frontend se comprueba **renderizando**, no leyendo: el arnés está en
   `docs/auditoria/capturas/_harness/` y las 241 capturas del estado actual son la referencia.
8. Un commit por pendiente o por grupo pequeño, con el identificador en el mensaje.
9. Antes de empujar, `git pull --rebase`. Si sale un conflicto en un archivo que **no es de tu
   carril**, es que alguien se saltó la regla 3: no lo resuelvas por tu cuenta, avísalo.

---

## Lo que sigue bloqueado por una decisión del dueño

1. **Rotar el token de R2.** Expuesto en un repositorio público.
2. **¿Los datos de personal pueden seguir fuera de Venezuela?** El requisito de soberanía se
   incumple por construcción y nadie lo ha declarado.
3. **¿La unidad de descripción es el folio o el expediente?**
4. **El nombre del producto** — ya decidido por el cliente, nunca aplicado.
5. **Qué escáneres hay en la Facultad.** Bloquea elegir entre TWAIN, SANE y captura por móvil;
   mientras tanto, la captura por móvil avanza sin depender de esa respuesta.
6. **El gasto de firma de código** si se va a Electron.
7. **¿Una persona puede tener más de una relación laboral?** Hoy `empleados.cedula` es `UNIQUE`
   y el expediente es uno por persona. Un reingreso tras jubilación o una doble condición
   docente/administrativo no se pueden representar. Barato ahora, caro después.

---

## Anexo — los pendientes de cada carril


**`A1-buscador-archivo`** (165) — `BA-006`, `BA-007`, `BA-010`, `BA-011`, `BA-012`, `BA-013`, `BA-015`, `BA-016`, `BA-017`, `BA-018`, `BA-020`, `BA-021`, `BA-022`, `BA-023`, `BA-024`, `BA-025`, `BA-026`, `BA-027`, `BA-028`, `BA-029`, `BA-030`, `BA-031`, `BA-040`, `BA-041`, `BA-042`, `BA-043`, `BA-044`, `BA-045`, `BA-046`, `BA-047`, `BA-048`, `BA-049`, `BA-050`, `BA-051`, `BA-052`, `BA-053`, `BA-054`, `BA-055`, `BA-056`, `BA-057`, `BA-058`, `BA-059`, `BA-060`, `BA-061`, `BA-062`, `BA-063`, `BA-064`, `BA-065`, `BA-066`, `BA-067`, `BA-068`, `BA-070`, `BA-071`, `BA-072`, `BA-073`, `BA-074`, `BA-075`, `BA-076`, `BA-077`, `BA-078`, `BA-079`, `BA-080`, `BA-081`, `BA-082`, `BA-083`, `BA-084`, `BA-085`, `BA-086`, `BA-087`, `BA-088`, `BA-089`, `BA-090`, `BA-091`, `BA-092`, `BA-093`, `BA-094`, `BA-095`, `BA-100`, `BA-101`, `BA-102`, `BA-103`, `BA-104`, `BA-105`, `BA-106`, `BA-107`, `BA-108`, `BA-109`, `BA-110`, `BA-111`, `BA-113`, `BA-120`, `BA-121`, `BA-122`, `BA-123`, `BA-124`, `BA-125`, `BA-126`, `BA-127`, `BA-128`, `BA-129`, `BA-130`, `BA-131`, `BA-132`, `BA-133`, `BA-134`, `BA-135`, `BA-136`, `BA-137`, `BA-138`, `BA-139`, `BA-140`, `BA-141`, `BA-142`, `BA-143`, `BA-144`, `BA-145`, `BA-146`, `BA-147`, `BA-148`, `BA-150`, `BA-151`, `BA-152`, `BA-160`, `BA-162`, `BA-163`, `BA-165`, `BA-166`, `BA-167`, `BA-168`, `BA-169`, `BA-170`, `BA-171`, `BA-172`, `BA-173`, `BA-175`, `BA-176`, `BA-177`, `BA-178`, `BA-179`, `BA-180`, `BA-181`, `BA-182`, `BA-183`, `BA-185`, `BA-186`, `BA-187`, `BA-190`, `BA-191`, `BA-192`, `BA-193`, `BA-194`, `BA-195`, `BA-196`, `BA-197`, `BA-198`, `BA-199`, `BA-200`, `DG-108`, `RQ-006`, `RQ-007`, `RQ-028`, `RQ-038`, `RQ-039`, `RQ-040`, `RQ-041`

**`A2-archivo-backend`** (9) — `BA-003`, `BR-001`, `DG-106`, `DG-107`, `IN-031`, `IN-047`, `OA-207`, `RQ-010`, `RQ-055`

**`A3-buscador-rrhh`** (84) — `BR-011`, `BR-017`, `BR-018`, `BR-019`, `BR-020`, `BR-021`, `BR-022`, `BR-023`, `BR-024`, `BR-025`, `BR-027`, `BR-032`, `BR-034`, `BR-035`, `BR-037`, `BR-038`, `BR-039`, `BR-040`, `BR-041`, `BR-043`, `BR-054`, `BR-055`, `BR-057`, `BR-073`, `BR-074`, `BR-075`, `BR-076`, `BR-077`, `BR-078`, `BR-079`, `BR-080`, `BR-081`, `BR-082`, `BR-083`, `BR-084`, `BR-085`, `BR-086`, `BR-087`, `BR-088`, `BR-089`, `BR-090`, `BR-091`, `BR-092`, `BR-093`, `BR-098`, `BR-099`, `BR-100`, `BR-101`, `BR-104`, `BR-106`, `BR-112`, `BR-113`, `BR-114`, `BR-117`, `BR-123`, `BR-124`, `BR-126`, `BR-127`, `BR-128`, `BR-131`, `BR-133`, `BR-134`, `BR-135`, `BR-136`, `BR-137`, `BR-138`, `BR-139`, `BR-140`, `BR-143`, `BR-144`, `BR-146`, `BR-151`, `BR-157`, `BR-164`, `BR-165`, `BR-166`, `BR-169`, `BR-171`, `BR-172`, `BR-173`, `BR-175`, `BR-177`, `BR-178`, `RQ-043`

**`A4-rrhh-backend`** (74) — `BR-006`, `BR-007`, `BR-009`, `BR-010`, `BR-012`, `BR-013`, `BR-015`, `BR-016`, `BR-028`, `BR-031`, `BR-044`, `BR-045`, `BR-046`, `BR-047`, `BR-048`, `BR-050`, `BR-051`, `BR-052`, `BR-053`, `BR-058`, `BR-059`, `BR-060`, `BR-061`, `BR-062`, `BR-063`, `BR-064`, `BR-065`, `BR-067`, `BR-068`, `BR-069`, `BR-070`, `BR-071`, `BR-072`, `BR-118`, `BR-119`, `BR-125`, `BR-132`, `BR-148`, `BR-149`, `BR-150`, `BR-153`, `BR-154`, `BR-161`, `BR-162`, `BR-163`, `BR-167`, `BR-168`, `BR-170`, `BR-174`, `BR-176`, `OR-013`, `OR-014`, `OR-021`, `OR-022`, `OR-023`, `OR-037`, `OR-038`, `OR-057`, `OR-068`, `OR-148`, `OR-155`, `OR-156`, `OR-184`, `OR-214`, `OR-215`, `OR-216`, `OR-217`, `OR-218`, `OR-251`, `RQ-011`, `RQ-016`, `RQ-047`, `RQ-049`, `SI-151`

**`B1-admin-archivo-html`** (21) — `DG-138`, `OA-013`, `OA-050`, `OA-052`, `OA-058`, `OA-064`, `OA-065`, `OA-066`, `OA-069`, `OA-073`, `OA-081`, `OA-087`, `OA-095`, `OA-108`, `OA-139`, `OA-179`, `OA-180`, `OA-186`, `OA-197`, `OA-211`, `OR-233`

**`B10-admin-charts`** (32) — `DG-139`, `OA-014`, `OA-033`, `OA-067`, `OA-068`, `OA-071`, `OA-072`, `OA-074`, `OA-076`, `OA-077`, `OA-079`, `OA-102`, `OA-201`, `OA-210`, `OR-018`, `OR-019`, `OR-026`, `OR-071`, `OR-073`, `OR-074`, `OR-077`, `OR-078`, `OR-080`, `OR-101`, `OR-106`, `OR-109`, `OR-110`, `OR-112`, `OR-124`, `OR-234`, `OR-280`, `RQ-012`

**`B11-admin-categorias`** (18) — `BR-003`, `OA-018`, `OA-030`, `OA-051`, `OA-054`, `OA-089`, `OA-126`, `OA-127`, `OA-128`, `OA-129`, `OA-187`, `OR-056`, `OR-166`, `OR-168`, `OR-169`, `OR-170`, `OR-171`, `OR-239`

**`B12-admin-usuarios`** (21) — `OA-016`, `OA-040`, `OA-109`, `OA-154`, `OA-157`, `OA-158`, `OA-162`, `OA-163`, `OA-164`, `OA-189`, `OA-195`, `OA-196`, `OR-047`, `OR-195`, `OR-198`, `OR-200`, `OR-201`, `OR-203`, `OR-205`, `OR-208`, `OR-232`

**`B2-admin-rrhh-html`** (16) — `OR-062`, `OR-066`, `OR-105`, `OR-144`, `OR-150`, `OR-152`, `OR-161`, `OR-181`, `OR-210`, `OR-229`, `OR-242`, `OR-290`, `OR-291`, `OR-297`, `OR-299`, `OR-300`

**`B3-admin-sistema-html`** (114) — `SI-006`, `SI-007`, `SI-032`, `SI-033`, `SI-034`, `SI-036`, `SI-043`, `SI-044`, `SI-046`, `SI-050`, `SI-051`, `SI-052`, `SI-053`, `SI-054`, `SI-055`, `SI-057`, `SI-058`, `SI-059`, `SI-060`, `SI-097`, `SI-098`, `SI-099`, `SI-100`, `SI-101`, `SI-102`, `SI-103`, `SI-104`, `SI-105`, `SI-106`, `SI-109`, `SI-110`, `SI-111`, `SI-112`, `SI-113`, `SI-114`, `SI-115`, `SI-116`, `SI-117`, `SI-118`, `SI-119`, `SI-120`, `SI-122`, `SI-123`, `SI-124`, `SI-125`, `SI-132`, `SI-138`, `SI-139`, `SI-140`, `SI-141`, `SI-142`, `SI-146`, `SI-152`, `SI-153`, `SI-154`, `SI-155`, `SI-157`, `SI-158`, `SI-159`, `SI-162`, `SI-163`, `SI-164`, `SI-165`, `SI-167`, `SI-168`, `SI-169`, `SI-170`, `SI-175`, `SI-176`, `SI-178`, `SI-179`, `SI-181`, `SI-182`, `SI-183`, `SI-184`, `SI-185`, `SI-186`, `SI-187`, `SI-188`, `SI-189`, `SI-190`, `SI-191`, `SI-192`, `SI-193`, `SI-194`, `SI-195`, `SI-196`, `SI-197`, `SI-198`, `SI-199`, `SI-200`, `SI-201`, `SI-202`, `SI-203`, `SI-204`, `SI-205`, `SI-211`, `SI-212`, `SI-213`, `SI-218`, `SI-220`, `SI-222`, `SI-223`, `SI-224`, `SI-226`, `SI-227`, `SI-228`, `SI-233`, `SI-234`, `SI-236`, `SI-237`, `SI-238`, `SI-239`, `SI-240`

**`B4-admin-tabs`** (29) — `DG-154`, `OA-053`, `OA-055`, `OA-059`, `OA-080`, `OA-082`, `OA-106`, `OA-146`, `OA-147`, `OA-148`, `OA-172`, `OA-178`, `OA-206`, `OR-036`, `OR-042`, `OR-069`, `OR-070`, `OR-131`, `OR-132`, `OR-140`, `OR-185`, `OR-188`, `OR-189`, `OR-212`, `OR-221`, `OR-223`, `OR-224`, `OR-225`, `OR-240`

**`B5-admin-monitor`** (38) — `OA-029`, `OA-063`, `OA-094`, `OA-096`, `OA-103`, `OA-104`, `OA-107`, `OA-110`, `OA-111`, `OA-115`, `OA-116`, `OA-117`, `OA-118`, `OA-119`, `OA-121`, `OA-122`, `OA-150`, `OA-181`, `OR-098`, `OR-117`, `OR-119`, `OR-120`, `OR-121`, `OR-125`, `OR-128`, `OR-130`, `OR-133`, `OR-134`, `OR-135`, `OR-136`, `OR-137`, `OR-138`, `OR-162`, `OR-236`, `OR-237`, `OR-241`, `OR-257`, `OR-277`

**`B6-admin-ui`** (31) — `OA-015`, `OA-045`, `OA-070`, `OA-105`, `OA-112`, `OA-113`, `OA-114`, `OA-120`, `OA-159`, `OA-160`, `OA-161`, `OA-174`, `OA-175`, `OA-176`, `OA-177`, `OA-188`, `OA-192`, `OA-200`, `OR-122`, `OR-151`, `OR-154`, `OR-172`, `OR-204`, `OR-207`, `OR-220`, `OR-222`, `OR-226`, `OR-228`, `OR-246`, `OR-295`, `OR-296`

**`B7-admin-submit`** (32) — `IN-046`, `IN-119`, `OA-057`, `OA-083`, `OA-084`, `OA-085`, `OA-086`, `OA-088`, `OA-090`, `OA-091`, `OA-092`, `OA-093`, `OA-097`, `OA-098`, `OA-101`, `OR-015`, `OR-028`, `OR-029`, `OR-035`, `OR-058`, `OR-081`, `OR-083`, `OR-086`, `OR-088`, `OR-089`, `OR-091`, `OR-092`, `OR-094`, `OR-095`, `OR-096`, `OR-097`, `OR-099`

**`B8-admin-edit-archivo`** (20) — `BR-109`, `DG-114`, `DG-136`, `OA-019`, `OA-026`, `OA-027`, `OA-133`, `OA-137`, `OA-138`, `OA-140`, `OA-173`, `OA-182`, `OA-191`, `OA-194`, `OR-123`, `OR-129`, `OR-176`, `OR-178`, `OR-180`, `OR-182`

**`B9-admin-edit-rrhh`** (21) — `DG-135`, `OA-183`, `OR-001`, `OR-005`, `OR-006`, `OR-044`, `OR-139`, `OR-143`, `OR-145`, `OR-146`, `OR-147`, `OR-149`, `OR-153`, `OR-157`, `OR-160`, `OR-219`, `OR-227`, `OR-285`, `OR-286`, `OR-287`, `OR-294`

**`C1-docs-backend`** (22) — `DG-130`, `IN-006`, `IN-007`, `IN-010`, `IN-013`, `IN-014`, `OA-004`, `OA-005`, `OA-135`, `OA-205`, `OR-007`, `OR-031`, `OR-034`, `OR-100`, `OR-118`, `OR-126`, `OR-141`, `OR-142`, `OR-264`, `OR-271`, `OR-272`, `OR-284`

**`C10-ficheros-r2`** (46) — `BA-005`, `BA-149`, `BR-002`, `BR-159`, `DG-061`, `DG-080`, `DG-081`, `DG-082`, `DG-083`, `DG-095`, `DG-104`, `DG-120`, `DG-121`, `DG-131`, `DG-134`, `DG-140`, `DG-141`, `DG-142`, `DG-144`, `DG-147`, `DG-148`, `DG-153`, `DG-169`, `IN-018`, `IN-215`, `OA-006`, `OA-010`, `OA-011`, `OA-204`, `OR-054`, `OR-055`, `OR-115`, `OR-213`, `OR-269`, `OR-274`, `RQ-051`, `SI-001`, `SI-014`, `SI-025`, `SI-026`, `SI-077`, `SI-078`, `SI-133`, `SI-134`, `SI-137`, `SI-166`

**`C2-catalogo`** (21) — `BR-066`, `OA-021`, `OA-123`, `OA-124`, `OA-125`, `OA-130`, `OA-131`, `OA-152`, `OA-155`, `OR-033`, `OR-040`, `OR-052`, `OR-163`, `OR-164`, `OR-165`, `OR-191`, `OR-192`, `OR-193`, `OR-194`, `OR-265`, `OR-275`

**`C3-stats-backend`** (19) — `IN-030`, `OA-034`, `OA-075`, `OA-078`, `OR-020`, `OR-024`, `OR-025`, `OR-063`, `OR-064`, `OR-067`, `OR-072`, `OR-075`, `OR-076`, `OR-250`, `OR-256`, `OR-261`, `OR-276`, `OR-279`, `RQ-050`

**`C4-retencion`** (18) — `DG-152`, `OA-001`, `OA-002`, `OA-003`, `OA-061`, `OA-062`, `OA-141`, `OA-142`, `OA-143`, `OA-144`, `OA-145`, `OA-149`, `OR-183`, `OR-186`, `OR-187`, `OR-190`, `OR-273`, `RQ-014`

**`C5-importaciones`** (18) — `IN-025`, `IN-026`, `OA-023`, `OR-002`, `OR-003`, `OR-004`, `OR-027`, `OR-103`, `OR-104`, `OR-107`, `OR-108`, `OR-111`, `OR-113`, `OR-114`, `OR-116`, `OR-262`, `OR-282`, `OR-298`

**`C6-usuarios-backend`** (12) — `OA-038`, `OA-041`, `OA-047`, `OA-165`, `OA-166`, `OR-045`, `OR-049`, `OR-050`, `OR-051`, `OR-059`, `OR-199`, `OR-206`

**`C7-papelera`** (22) — `DG-137`, `IN-008`, `IN-016`, `IN-017`, `IN-019`, `IN-020`, `IN-070`, `OA-007`, `OA-009`, `OA-024`, `OA-046`, `OA-132`, `OA-134`, `OA-136`, `OR-009`, `OR-010`, `OR-011`, `OR-032`, `OR-174`, `OR-175`, `OR-177`, `OR-179`

**`C8-backup`** (19) — `IN-009`, `IN-021`, `IN-023`, `OA-167`, `OA-168`, `OA-169`, `OA-170`, `OA-171`, `OR-209`, `OR-211`, `SI-016`, `SI-127`, `SI-128`, `SI-129`, `SI-130`, `SI-131`, `SI-135`, `SI-136`, `SI-145`

**`C9-auth`** (19) — `IN-038`, `OA-017`, `OA-049`, `OR-048`, `OR-060`, `OR-263`, `RQ-036`, `RQ-037`, `SI-011`, `SI-012`, `SI-028`, `SI-031`, `SI-035`, `SI-038`, `SI-042`, `SI-045`, `SI-048`, `SI-096`, `SI-147`

**`D1-ia-backend`** (59) — `BA-161`, `BA-164`, `BR-005`, `OA-008`, `OR-012`, `RQ-001`, `SI-004`, `SI-009`, `SI-010`, `SI-013`, `SI-015`, `SI-018`, `SI-019`, `SI-020`, `SI-022`, `SI-023`, `SI-024`, `SI-029`, `SI-061`, `SI-062`, `SI-063`, `SI-064`, `SI-065`, `SI-066`, `SI-067`, `SI-068`, `SI-069`, `SI-070`, `SI-071`, `SI-072`, `SI-073`, `SI-075`, `SI-076`, `SI-079`, `SI-080`, `SI-081`, `SI-082`, `SI-083`, `SI-084`, `SI-085`, `SI-086`, `SI-087`, `SI-088`, `SI-089`, `SI-090`, `SI-091`, `SI-092`, `SI-093`, `SI-094`, `SI-095`, `SI-107`, `SI-108`, `SI-121`, `SI-161`, `SI-221`, `SI-229`, `SI-230`, `SI-231`, `SI-232`

**`D2-ia-frontend`** (3) — `SI-003`, `SI-005`, `SI-017`

**`E1-escaner-puente`** (107) — `DG-003`, `DG-004`, `DG-007`, `DG-010`, `DG-011`, `DG-013`, `DG-015`, `DG-016`, `DG-022`, `DG-024`, `DG-025`, `DG-026`, `DG-028`, `DG-029`, `DG-030`, `DG-031`, `DG-032`, `DG-033`, `DG-034`, `DG-035`, `DG-036`, `DG-037`, `DG-038`, `DG-039`, `DG-041`, `DG-042`, `DG-044`, `DG-045`, `DG-046`, `DG-047`, `DG-048`, `DG-049`, `DG-050`, `DG-051`, `DG-052`, `DG-054`, `DG-055`, `DG-056`, `DG-057`, `DG-058`, `DG-059`, `DG-060`, `DG-062`, `DG-063`, `DG-064`, `DG-065`, `DG-066`, `DG-068`, `DG-069`, `DG-070`, `DG-071`, `DG-072`, `DG-073`, `DG-074`, `DG-075`, `DG-077`, `DG-078`, `DG-079`, `DG-084`, `DG-088`, `DG-090`, `DG-091`, `DG-092`, `DG-093`, `DG-096`, `DG-097`, `DG-098`, `DG-101`, `DG-102`, `DG-103`, `DG-109`, `DG-110`, `DG-111`, `DG-112`, `DG-113`, `DG-115`, `DG-116`, `DG-117`, `DG-118`, `DG-119`, `DG-123`, `DG-124`, `DG-126`, `DG-127`, `DG-128`, `DG-129`, `DG-132`, `DG-133`, `DG-145`, `DG-149`, `DG-150`, `DG-151`, `DG-155`, `DG-156`, `DG-158`, `DG-163`, `DG-164`, `DG-166`, `DG-168`, `DG-170`, `DG-171`, `IN-067`, `SI-171`, `SI-172`, `SI-173`, `SI-174`, `SI-180`

**`E2-escaner-cliente`** (30) — `DG-001`, `DG-002`, `DG-005`, `DG-006`, `DG-008`, `DG-009`, `DG-012`, `DG-014`, `DG-017`, `DG-018`, `DG-019`, `DG-021`, `DG-023`, `DG-027`, `DG-040`, `DG-043`, `DG-053`, `DG-067`, `DG-085`, `DG-086`, `DG-087`, `DG-089`, `DG-099`, `DG-100`, `DG-157`, `DG-160`, `DG-161`, `DG-162`, `DG-167`, `SI-177`

**`F1-paginas-estaticas`** (9) — `BR-179`, `BR-180`, `DG-159`, `DG-165`, `OR-278`, `RQ-048`, `SD-207`, `SD-208`, `SI-160`

**`F2-cascara`** (8) — `BA-008`, `OA-099`, `OR-093`, `RQ-042`, `RQ-044`, `RQ-045`, `RQ-046`, `RQ-053`

**`H2-app-js`** (30) — `BA-009`, `BA-014`, `BR-008`, `BR-014`, `BR-026`, `BR-036`, `BR-042`, `BR-094`, `BR-095`, `BR-097`, `BR-158`, `DG-020`, `IN-204`, `OA-031`, `OA-032`, `OA-042`, `OA-043`, `OA-060`, `OA-190`, `OA-193`, `OR-090`, `OR-127`, `OR-281`, `OR-288`, `OR-289`, `OR-293`, `RQ-005`, `RQ-008`, `SI-037`, `SI-049`

**`H4-despliegue`** (37) — `BA-112`, `BA-174`, `BR-160`, `IN-001`, `IN-003`, `IN-045`, `IN-063`, `IN-065`, `IN-105`, `IN-110`, `IN-114`, `IN-115`, `IN-124`, `IN-153`, `IN-154`, `IN-163`, `IN-168`, `IN-189`, `IN-194`, `IN-202`, `IN-208`, `IN-209`, `IN-211`, `IN-213`, `IN-214`, `OR-173`, `RQ-018`, `RQ-029`, `RQ-030`, `SD-021`, `SD-228`, `SD-235`, `SI-030`, `SI-074`, `SI-217`, `SI-225`, `SI-235`