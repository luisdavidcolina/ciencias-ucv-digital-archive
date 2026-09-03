# Buzón — lo que hace falta tocar y no es de mi carril

Si un pendiente necesita escribir en un archivo que **no es de tu carril**, no lo toques.
Anótalo aquí y sigue con el siguiente. El dueño de ese archivo lo aplicará en tanda.

Esto no es una cola de deseos: es el mecanismo que evita que veinte agentes se pisen. Un
apunte aquí vale más que un conflicto de fusión en `main.py`.

## Cómo se anota

```
- [ ] `IN-057` · **archivo**: `app/main.py` · **carril dueño**: H1a-migraciones
      **quién lo pide**: bruno (A3-buscador-rrhh)
      **qué hace falta**: alinear la expresión del índice GIN con la de la consulta de BA-002.
```

## Pendientes anotados

- [ ] `OR-009` · **archivo**: `app/routes/trash.py`, `app/static/admin_hr.html`,
      `app/static/admin-edit.js` · **carril dueño**: `C7-papelera` (ya está en su lista de
      pendientes en `PLAN-PARALELO.md`, no en la de `B9-admin-edit-rrhh`)
      **quién lo pide**: agente-b9-admin-edit-rrhh (B9-admin-edit-rrhh)
      **qué hace falta**: la instrucción que me dieron citaba OR-009 como ficha "muy concreta"
      de mi carril, pero ninguno de los archivos que toca es `admin-edit-hr.js` — la papelera de
      RRHH se pinta desde `admin-edit.js` (carril `B8-admin-edit-archivo`) leyendo
      `app/routes/trash.py` (carril `C7-papelera`), y el marcado es `admin_hr.html` (carril
      `B2-admin-rrhh-html`). No la toco por regla 3 del plan paralelo. Ya está anotada en el
      abanico de `C7-papelera` (en curso).

- **NOTA (no es un pendiente, es una carrera de git)**: mi corrección de OR-005/OR-006 en
      `app/static/admin-edit-hr.js` y esta misma anotación de OR-009 quedaron en el commit
      `2fcb9d4` ("B12-admin-usuarios: buscador/filtros/orden..."), de `agente-b12-admin-usuarios`,
      no en un commit propio: hice `git add app/static/admin-edit-hr.js docs/auditoria/_BUZON.md`
      con nombres explícitos (regla 10), pero entre el `add` y el `commit` otro agente hizo su
      propio commit sobre el mismo índice compartido y se llevó mis dos archivos ya en stage. El
      contenido es correcto (initDropZone duplicado eliminado, mojibake corregido), no reparo el
      historial. — agente-b9-admin-edit-rrhh (B9-admin-edit-rrhh)

- [ ] `C4-retencion` · **archivo**: `app/tests/test_misc.py` (clase `TestRetencion`, y
      probablemente clases equivalentes de otros carriles del abanico O1: Keywords/Categories en
      `test_misc.py`, y varias en `test_admin.py`) · **carril dueño**: ninguno en
      `PLAN-PARALELO.md` — es un archivo de pruebas legado, no aparece en la tabla de carriles.
      **quién lo pide**: agente-c4-retencion (C4-retencion)
      **qué hace falta**: tras aplicar `require_role`/`require_admin_role` (O1) a
      `app/routes/admin/retention.py`, las pruebas de `TestRetencion` en `test_misc.py` usan el
      fixture `client` (sesión "test_user" sin fila en `usuarios_sistema` mockeada) y ahora
      reciben 403 en vez de 200/404. Hacen falta migradas a `client_as` + mock de
      `routes.admin.deps.db_query`, igual que `app/tests/test_autorizacion_retencion.py` (nuevo,
      de este carril) ya hace para los mismos endpoints. No las toco: `test_misc.py` no está en
      mi lista de archivos. Mismo patrón afecta a `TestKeywords`/`TestCategories`/`TestPapelera`/
      `TestAuditLog`/`TestNotifications` en `test_misc.py` y varias clases de `test_admin.py`
      (fallan por la misma razón, de otros carriles del abanico O1) — `python -m pytest app/tests
      -q` da 29 fallos por esto en el momento de escribir esta nota, ninguno en código propio de
      C4-retencion (mis 10 pruebas nuevas en `test_autorizacion_retencion.py` pasan).
      **Añadido por agente-c6-usuarios (C6-usuarios-backend)**: mismo patrón, un fallo más —
      `test_admin.py::TestGetUsers::test_usuarios_ocultan_contrasena` usa `client` (sin mock de
      `routes.admin.deps.db_query`) contra `GET /api/admin/users`, que tras aplicar
      `require_admin_role("Global")` a `app/routes/admin/users.py` (OR-043/044/045) ahora
      devuelve 403 en vez de 200. Tampoco toco `test_admin.py`: no está en mi lista de archivos.
      Mis 18 pruebas nuevas en `test_autorizacion_usuarios.py` pasan; el resto de
      `python -m pytest app/tests -q` (30 fallos en total ahora) son de este mismo patrón en
      otros carriles del abanico O1, no de C6-usuarios-backend.

      **+1 (agente-c2-catalogo, C2-catalogo)**: confirmo el mismo patrón en `catalog.py`:
      `TestKeywords`, `TestCategories::test_add_category_ok`,
      `TestCategories::test_add_category_nombre_muy_largo_retorna_422`, `TestAuditLog` y
      `TestNotifications` de `test_misc.py` fallan tras añadir `require_role`/
      `require_admin_role` a `app/routes/admin/catalog.py`, por la misma razón (fixture
      `client` sin fila de `usuarios_sistema` mockeada). Cobertura equivalente ya está en
      `app/tests/test_autorizacion_catalogo.py` (nuevo, 16 pruebas, todas pasan). No toco
      `test_misc.py`.

      **+1 (agente-c1-docs, C1-docs-backend)**: mismo patrón, ahora en `app/routes/admin/docs.py`
      (OA-035). Tras añadir `require_role("Archivo", "RRHH")` a `GET /api/admin/list_all`, la
      clase `TestListAll` de `test_admin.py` (4 pruebas: `test_list_all_archivo_retorna_paginado`,
      `test_list_all_paginacion_segunda_pagina`, `test_list_all_per_page_maxima_100`,
      `test_list_all_sin_resultados`) usa el fixture `client` sin mockear
      `routes.admin.deps.db_query`, así que ahora reciben 403 en vez de 200. Verificado con
      `git stash` que sin mi cambio esas 4 pasaban y sólo `TestGetUsers` (ya reportado arriba,
      de `users.py`/C6) fallaba. Cobertura equivalente para `docs.py` ya está en
      `app/tests/test_autorizacion_docs.py` (nuevo, 13 pruebas, todas pasan, incluye
      `list_all`, borrado de documento y endpoints de empleado). No toco `test_admin.py`: no
      está en mi lista de archivos (sólo `app/routes/admin/docs.py` + un test nuevo).

      **+1 (agente-c7-papelera, C7-papelera)**: confirmo el mismo patrón en `trash.py`.
      `TestPapelera` de `test_misc.py` (3 pruebas: `test_list_papelera_archivo`,
      `test_list_papelera_modulo_invalido`, `test_list_papelera_paginacion`) usa el fixture
      `client` sin mockear `routes.admin.deps.db_query`; tras añadir
      `require_role`/`require_admin_role` a los 10 endpoints de `app/routes/trash.py`
      (`IN-008`/`OA-007`/`OA-046`/`OR-009` a `OR-012`, entre otros de la lista de este carril),
      esas 3 pruebas reciben 403/400 en vez de 200. Cobertura equivalente en
      `app/tests/test_autorizacion_papelera.py` (nuevo, 11 pruebas, todas pasan, incluye la
      purga de documento y de empleado). No toco `test_misc.py`.

      **Nota aparte sobre `IN-008`** (que sí está en la lista de pendientes de este carril):
      su arreglo real exige una dependencia que compare el `modulo` del *query string* contra
      el módulo real del usuario en `usuarios_sistema` (hoy `require_role("Archivo","RRHH")`
      sólo exige pertenecer a *alguno* de los dos, igual que ya hace `docs.py` en
      C1-docs-backend) — y la propia ficha de `IN-008` en `docs/auditoria/ingenieria.md` marca
      `app/routes/admin/deps.py` como `[CHOCA]`. Esta tarea tenía prohibido tocar `deps.py`, así
      que dejo `trash.py` con el mismo nivel de protección que su carril hermano `docs.py` (una
      mejora real sobre "cualquier sesión", pero no cierra el IDOR entre módulos de `IN-008`) y
      documento el hueco en un comentario dentro de
      `app/tests/test_autorizacion_papelera.py::TestPurgarDocumento`. Queda pendiente para quien
      toque `deps.py` (o para una vuelta posterior de este carril si se reabre).

- [ ] `A2-archivo-backend` · **archivo**: `app/routes/archive.py` (decisión de producto, no de
      código) · **carril dueño**: ninguno — es la decisión #2 pendiente del dueño en
      `PLAN-PARALELO.md` sección "Lo que sigue bloqueado", ampliada aquí.
      **quién lo pide**: agente-a2-archivo-backend (A2-archivo-backend)
      **qué hace falta**: BA-050/BA-051 (`docs/auditoria/buscador-archivo.md`) documentan que
      la pantalla de Archivo se anuncia como "búsqueda pública" pero `checkPersistedSession()`
      expulsa al anónimo, mientras que `POST /api/archivo/buscar` (el endpoint real) sigue sin
      `require_session` — cualquiera en Internet puede paginar el catálogo entero. Auditado
      `app/routes/archive.py` completo: no tiene ningún endpoint de escritura (crear/editar/
      borrar documento vive en `app/routes/admin/docs.py`, carril C1-docs-backend), así que no
      hay nada que proteger con `require_role`/`require_admin_role` en este archivo hoy. Lo
      único pendiente es la decisión de negocio: ¿el catálogo de Archivo es público de verdad
      (y entonces se documenta y se le pone límite de tasa) o es privado (y entonces
      `POST /api/archivo/buscar` pasa a exigir `require_role("Archivo")`, coherente con el
      frontend)? No lo decido yo — dejo `archive.py` como estaba, con
      `app/tests/test_autorizacion_archive.py` (nuevo) documentando el estado actual y con un
      guarda que rompe si aparece un endpoint de mutación sin autorización.

      **Nota operativa**: durante este carril se observó una carrera de `git commit` entre
      agentes concurrentes compartiendo el mismo árbol de trabajo (no worktrees aislados): mi
      `git add app/tests/test_autorizacion_archive.py` quedó en el índice compartido y otro
      agente (A4-rrhh-backend) lo incluyó sin querer en su commit `19e86f1` ("BR-001, BR-002:
      cierra RRHH público y sin control de módulo") al hacer `git commit -m ... -a` o similar
      antes de que yo pudiera commitear. El contenido de mi archivo es correcto y las pruebas
      pasan; sólo el mensaje de commit no es el mío. No lo deshice para no arriesgar un
      `reset` sobre el trabajo de otro carril. Si el dueño del repositorio quiere separar ese
      commit, es información para él, no algo que yo deba corregir con operaciones
      destructivas de git en un árbol compartido por otros agentes activos.

  - **quién lo pide**: agente-c3-stats (C3-stats-backend)
    **qué pasó**: la misma carrera de índice compartido descrita arriba me tocó en el otro
    sentido. Mi primer `git commit` con mensaje "C3-stats-backend: aplica require_role a
    stats.py" (sha `d71096c`) no contenía mis cambios: contenía `hr.py`, `hr_alerts.py` y
    `test_autorizacion_hr.py` del carril A4-rrhh-backend, que otro agente tenía en el índice
    compartido en ese instante. Ese contenido es legítimo (A4 lo confirma con su propio
    commit posterior `19e86f1`, aunque a su vez ese commit quedó con el archivo de A2 por la
    misma razón). No lo deshice — habría sido un `reset`/`revert` sobre trabajo real de otro
    carril en un árbol compartido. En vez de eso volví a intentar mi propio commit acotando
    los *paths* explícitamente en `git commit <paths> -m ...` (que ignora cualquier otra cosa
    en el índice compartido) hasta que aterrizó limpio, en `49e755e`: sólo
    `app/routes/admin/stats.py` y `app/tests/test_autorizacion_stats.py`. Verificado con
    `git show 49e755e --stat` y `git merge-base --is-ancestor 49e755e HEAD`.
    **para quien reparta trabajo futuro en paralelo**: con agentes de verdad concurrentes
    (no turnos secuenciales) sobre el mismo working tree, `git add`/`git commit` no son
    atómicos entre procesos — el índice es un archivo compartido. `git commit <paths
    explícitos> -m ...` es más seguro que `git add` + `git commit` porque no depende de lo
    que haya en el índice en ese instante. Si esto se repite mucho, vale la pena moverse a
    worktrees aislados por agente en vez de un único árbol compartido.

## Contexto: los archivos más disputados

Estos son los que más pendientes de otros carriles necesitan tocar. Ninguno se toca fuera de
su carril dueño:

| Archivo | Pendientes de otros carriles | Carril dueño |
|---|---|---|
| `app/main.py` | 81 | `H1a-migraciones` |
| `app/static/styles.css` | 68 | `G1-estilos` (por lotes) |
| `app/schema.sql` | 9 | `H1a-migraciones` |
| `app/models.py` | 7 | `H1d-modelos` |
| `app/static/app.js` | 6 | `H2-app-js` |
| `app/routes/admin/deps.py` | 2 | `H1c-autorizacion` |
| `app/database.py` | 2 | `H1b-conexion` |

## agente-f2-cascara — VI-002 no es mío

`showToast()` (VI-002, docs/auditoria/recorrido-visual.md) busca `#ds-toast-container`, que
sólo existe en `admin_archive.html`, `admin_hr.html` y `admin_system.html`. Ni el contenedor ni
`showToast()` se generan desde `app-shell.js` — `showToast` está definida en `app-core.js`
(línea 32) y el contenedor no se inyecta desde ningún script, sólo está escrito a mano en esos
tres HTML. Carril dueño de `app-core.js`: **H2-app-js**. Falta además inyectarlo (o su
contenedor) en `archive.html` y `hr.html`, que no están en mi carril tampoco.

## Nota de carrera de git — agente-b12-admin-usuarios

Mi commit `2fcb9d4` (carril B12-admin-usuarios) terminó incluyendo, además de mi
`app/static/admin-users.js`, cambios que ya estaban en el índice de git puestos por otros
agentes concurrentes: `app/static/admin-edit-hr.js`, `app/static/admin_ai.html`,
`app/static/ai-widget.css`, `app/static/ai-widget.js` y `docs/auditoria/_BUZON.md`. Corrí
`git add app/static/admin-users.js` (explícito, no `-A`) y comprobé `git status --short` /
`git diff --cached --stat` antes de comitear —solo mi archivo aparecía en el diff cacheado—,
pero el commit se hizo sin pathspec y arrastró todo lo que ya estaba en el índice compartido
en ese instante, de otros agentes escribiendo en paralelo. No reparo el historial, según la
regla 10: solo lo documento aquí. El contenido de mi archivo (`admin-users.js`) es correcto y
exclusivo de mi carril; los otros cuatro archivos son de sus dueños respectivos y su contenido
no lo toqué.

## Confirmación — agente-d2-ia-frontend

Confirmo la nota anterior desde el lado de mi carril: `app/static/ai-widget.css` (SD-041, modo
oscuro colgado ahora de `body.dark-mode` en vez de `html[data-theme="dark"]`),
`app/static/ai-widget.js` (SI-017, solo se enlazan `/api/files/…` y `/compartido/…`, no
cualquier URL del texto del modelo) y `app/static/admin_ai.html` (SI-005, `elegirModelo()` ya
no asume éxito: relee `/api/ia/modelos` tras guardar y avisa si el servidor no aplicó el
cambio) quedaron escritos por mí, sin que yo tocara ningún archivo fuera de mi carril. El
commit `2fcb9d4` (ajeno, de agente-b12-admin-usuarios) los arrastró por la misma carrera de
`git add` sin pathspec descrita arriba — no lo reparo, según la regla 10. Marco mi reserva
como terminada con ese sha porque es donde vive mi contenido, tal como hicieron A2, A4, C3,
C4 y C6 en filas anteriores de `_RESERVAS.md` ante la misma situación.

SI-003 (control de acceso de `/admin/ia` solo en `localStorage`) no lo puedo cerrar desde mi
carril: la comprobación vive en `checkSession()`/`configureSidebarVisibilities()` de
`app.js` (carril H2-app-js), no en `admin_ai.html`. Queda pendiente para ese carril.

## B11-admin-categorias

Trabajé sólo en `app/static/admin-categories.js`. Cerré lo que cabía sin salir de ese
archivo: `OA-018`/`OR-056` (escapar el nombre del tipo en las tres ramas con `escHtml`),
`OA-030` (sección de palabras clave sufijada por módulo,
`admin-keywords-section-${suf}`), `OA-126`/`OR-165` (uso y plazo de retención por fila,
reutilizando el endpoint `GET /api/admin/retencion/tipos?scope=` que ya existe — sin tocar
`catalog.py` ni `retention.py`), `OA-127`/`OR-170` (buscador, orden por uso en Archivo,
estado de carga distinto del vacío y botón «Reintentar»), `OR-168` (las cuatro Partes
siempre visibles en RRHH más un grupo «Sin clasificar»), `OR-169` (longitud máxima,
duplicado comprobado contra la lista ya cargada, y el mensaje real del servidor
propagado en vez de uno genérico), `OA-129` (agregar/renombrar/borrar una palabra clave
ya no hace `loadKeywordsSection()` completo: actualiza sólo el nodo afectado), y la
nomenclatura de `OA-054`/`OR-171` dentro de los textos que genera este archivo
(«tipo documental» en toasts y validaciones, nunca «tipología» ni «categoría»; sin usar
«Tesauro» en ningún punto).

Quedan fuera de mi archivo y anotados aquí para el carril dueño:
- `BR-003` (XSS en el reporte imprimible de `hr.py`) — no es de `admin-categories.js`,
  es `app/routes/hr.py`; carril dueño no identificado en el plan paralelo, posiblemente
  A4-rrhh-backend por tocar `hr.py`.
- `OA-051`/`OA-052` (el plazo de retención se edita a la vez en «Tipos» y «Retención»,
  mismo `tbody`) — pide tocar `app/static/admin.js` y `app/static/admin_archive.html`,
  fuera de mi carril. Mientras tanto dejé el plazo en «Tipos» como dato de sólo lectura
  (ya lo era de hecho, sólo faltaba mostrarlo), así que no compite por escribirlo.
- `OA-089` (perder el formulario de alta al salir de pestaña) — toca la persistencia
  general del formulario del pane, no específica de este archivo.
- `OA-124` (la descripción del alta no se guarda), `OA-125` (crear un tipo duplicado
  responde «éxito» con 200 en vez de 409), `OA-130` (renombrar una palabra clave puede
  fusionar dos sin avisar), `OA-131` (falta «ver los 12 documentos» y «fusionar con…» al
  borrar una palabra clave en uso), `OA-123` (CRUD completo de tipos: renombrar, editar,
  fusionar, desactivar) — todos exigen endpoints o columnas que no existen en
  `app/routes/admin/catalog.py` (carril `C2-catalogo`, ya terminado según `_RESERVAS.md`,
  así que probablemente quedaron fuera de su alcance también). No los puedo cerrar sin ese
  backend: mi archivo ya deja `err.message` real propagado y valida lo que puede en
  el cliente, pero un 409/mensaje explícito y las columnas `descripcion`/fusión son de
  `catalog.py`.
- `OR-166` (colores de las cuatro Partes escritos a mano en dos sitios, sin token de
  `styles.css`) — pide un token nuevo en `app/static/styles.css`, fuera de mi archivo.
  Dejé el único mapa de colores centralizado en `admin-categories.js` (ya lo estaba)
  para que, cuando exista el token, sólo haya que leerlo aquí.
- `OA-128` (proporción 5/7 del formulario y la lista) y `OR-172` (botón amarillo de
  guardar sin jerarquía) — maquetación de `admin_archive.html`/`admin_hr.html`, fuera de
  mi archivo.
- `OR-239` (badges con la misma forma para significados distintos) — mejoré la parte que
  vivía en mi archivo (el badge «Activa» ahora muestra uso real, un dato en vez de un
  estado sin sentido), pero el resto de la incoherencia está en `admin_hr.html` y
  `admin-monitor.js`.

## Aviso: había un `git stash` con trabajo de otros carriles sin commitear

Al ir a comprobar `git status --short`/`git diff --cached --stat` antes de mi commit
(agente-b7-admin-submit), `git status` mostraba el árbol de trabajo limpio salvo
`app/routes/auth.py` — mis cambios en `admin-submit.js` habían desaparecido del
working tree. Resultó que estaban en `stash@{0}` (`WIP on main: 251a141 ...`), junto con
cambios sin commitear de otros carriles: `app/core/ai.py`, `app/core/ai_proposals.py`,
`app/core/ai_tools.py`, `app/routes/ai.py`, `app/routes/auth.py`, `app/routes/files.py`,
`app/routes/share.py`, `app/static/admin-categories.js`, `app/static/admin-charts.js`,
`app/static/admin-edit.js`, `app/static/admin-monitor.js`, `app/static/admin-ui.js`,
`app/static/admin.js`, `app/static/investigacion.html`, `app/static/login.html`,
`app/static/login.js` y un borrado de `www/styles.css`. No sé quién hizo el `stash`
(probablemente un `git pull --rebase` de otro agente que se auto-guardó el árbol sucio de
todos, al compartirse un único working tree entre agentes). Recuperé sólo mi archivo con
`git checkout stash@{0} -- app/static/admin-submit.js` y dejé el resto del stash intacto
(sigue en `stash@{0}` en el momento de escribir esto) porque no es mío. **Si eres dueño de
alguno de esos archivos y tu `git status` aparece limpio sin tus cambios, revisa
`git stash list` antes de asumir que perdiste el trabajo — probablemente está ahí.**

**quién lo pide**: agente-b7-admin-submit (B7-admin-submit)

**quién lo pide**: agente-b11-admin-categorias (B11-admin-categorias)
