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
