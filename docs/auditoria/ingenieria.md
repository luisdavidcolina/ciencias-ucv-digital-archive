# Auditoría de ingeniería — backend y proyecto

Revisión transversal de la ingeniería de software del sistema: `app/main.py`,
`app/database.py`, `app/models.py`, `app/utils.py`, `app/storage.py`,
`app/schema.sql`, `app/core/*`, `app/routes/*` (en visión transversal, no pantalla
por pantalla), `app/tests/*`, `api/index.py`, `vercel.json`, `requirements*.txt`,
`.env.example` y la salud general del repositorio.

**No se ha implementado nada**: este documento sólo audita.

## Convenciones de este documento

- Identificadores `IN-xxx`, correlativos y sin huecos.
- Esfuerzo: **S** ≤ media jornada · **M** 1–3 jornadas · **L** > 3 jornadas.
- `[CHOCA]` marca los pendientes que tocan ficheros compartidos con otros
  carriles: `main.py`, `database.py`, `models.py`, `utils.py`, `storage.py`,
  `core/*`, `lookups.py`, `files.py`, `archive.py`, `hr.py`, `admin/helpers.py`,
  `admin/docs.py`, `admin/stats.py`, `trash.py`, `vercel.json`. No deben
  lanzarse en paralelo con agentes que declaren esos mismos ficheros.
- Nada de lo que sigue propone deshacer decisiones ya tomadas: `db_query` sigue
  siendo el único helper, las migraciones siguen siendo idempotentes con huella
  SHA-256, y el `%%` en SQL literal sigue siendo obligatorio. Donde se propone
  Alembic (IN-098) es como **complemento** documentado, no como sustitución
  impuesta.
- Los pendientes de las auditorías hermanas no se repiten. Cuando algo de aquí
  generaliza uno de ellos, se cita por su identificador: **BR-001** y **BR-002**
  (falta de autenticación y de control de módulo en RRHH), **BR-056** (sin CSP),
  **BR-057** (sin pruebas de RRHH), **BR-064** (el cliente declara quién es),
  **BR-074** (CDN sin SRI), **BA-002** (índice GIN que no casa), **BA-019**,
  **BA-020** (`?u=` no protege nada), **BA-021**, **BA-101**, **OA-001**,
  **OA-002**.
- Total: **216 pendientes**.

---

## Índice

| Sección | Rango | Nº |
|---|---|---|
| A. Fallos reales de ingeniería, con escenario | IN-001 … IN-040 | 40 |
| B. Arquitectura, capas y acoplamiento | IN-041 … IN-067 | 27 |
| C. Base de datos, esquema y migraciones | IN-068 … IN-104 | 37 |
| D. Rendimiento, serverless y coste | IN-105 … IN-128 | 24 |
| E. Seguridad y autorización | IN-129 … IN-163 | 35 |
| F. Fiabilidad, transacciones y recuperación | IN-164 … IN-180 | 17 |
| G. Observabilidad | IN-181 … IN-191 | 11 |
| H. Calidad, pruebas y herramientas | IN-192 … IN-207 | 16 |
| I. Modernidad y dependencias | IN-208 … IN-216 | 9 |

*(Documentación y operación quedan repartidas en F e I, señaladas como tales.)*

---

## Resumen ejecutivo

Cinco hallazgos son de gravedad máxima y ninguno de ellos se ve desde la interfaz:

1. **Las migraciones nunca corren en producción** (IN-001). `api/index.py` monta
   `Mangum(app, lifespan="off")`, y todo el arranque —`ensure_audit_table`,
   `populate_missing_slugs`, `run_migrations`, `_backfill_rrhh_tipo_fk`— vive
   dentro del `lifespan` de FastAPI. En Vercel no se ejecuta ninguno. Toda la
   estrategia de huella SHA-256 documentada en `CLAUDE.md` es, en el despliegue
   real, código muerto: el esquema de Neon sólo avanza si alguien corre la app a
   mano.
2. **Las credenciales de Cloudflare R2 están escritas en el código**
   (IN-002), en `storage.py:14-18`, en un repositorio git. `CLAUDE.md` afirma que
   `test_secrets.py` lo verifica en cada corrida; **ese fichero no existe**
   (IN-194).
3. **`api/requirements.txt` no incluye `boto3`** (IN-003). Es el fichero que
   instala Vercel. `main.py` importa `routes.files`, que importa `storage`, que
   importa `boto3`: si Vercel no está resolviendo la dependencia por otra vía, la
   aplicación entera no arranca.
4. **Cualquier usuario con sesión puede descargar y sobrescribir la base
   completa** (IN-129, IN-130). `routes/backup.py` sólo exige `require_session`
   pese a documentarse como "solo para el admin Global"; el export incluye
   `usuarios_sistema` con sus hashes bcrypt, y el restore acepta filas
   arbitrarias en esa misma tabla — es una escalada a administrador en dos
   peticiones.
5. **Ninguna conexión se cierra limpiamente en lectura** (IN-004). `db_query`
   sólo hace `commit` cuando se lo piden y `rollback` sólo ante excepción: cada
   `SELECT` devuelve al pool una conexión con transacción abierta. En Neon eso
   son conexiones *idle in transaction* que bloquean el `VACUUM` y consumen el
   cupo del proyecto.

Por debajo, el patrón dominante es que **la autorización no existe más allá de
"hay cookie"**: `require_session` devuelve un nombre de usuario y ni un solo
endpoint comprueba rol ni módulo (IN-131), lo que generaliza BR-002 a todo el
sistema, incluida la gestión de usuarios (IN-132). Y el segundo patrón es que
**no hay transacciones**: cada `db_query` con `commit=True` es su propia unidad
atómica, de modo que toda operación de varios pasos —alta de empleado más
documento, purga, restauración de copia— puede quedar a medias sin que nadie se
entere (IN-164).

---

## A. Fallos reales de ingeniería, con escenario

### IN-001 · El arranque de la aplicación no se ejecuta nunca en Vercel `[CHOCA]`
`api/index.py:13`, `app/main.py:31-37`
**Hoy**: el handler es `Mangum(app, lifespan="off")`. Todo el arranque
(`ensure_audit_table()`, `populate_missing_slugs()`, `run_migrations()`,
`_backfill_rrhh_tipo_fk()`) está en el gestor `_lifespan`, que con esa opción no
se invoca jamás. Escenario: se añade una migración nueva, se despliega, la huella
cambia, y en producción no se aplica nada; el primer endpoint que use la columna
nueva responde 500 y la causa no aparece en ningún log porque no hubo fallo de
migración: no hubo migración. Todo el diseño de huella SHA-256 descrito en
`CLAUDE.md` sólo tiene efecto cuando alguien levanta `uvicorn` en su portátil.
**Debe**: decidir explícitamente el modelo. Lo correcto en serverless es sacar
las migraciones del arranque de la petición: un comando `python -m app.migrate`
ejecutado en el paso de build o desde un endpoint `/api/admin/migrar` protegido
con `CRON_SECRET`, y dejar el `lifespan` sólo para lo que sea barato. Si se
prefiere mantener el comportamiento documentado, `lifespan="auto"` y aceptar el
coste del arranque en frío. Lo que no puede quedar es la contradicción entre el
documento y el despliegue.
**Esfuerzo**: M. **Archivos**: `api/index.py`, `app/main.py` `[CHOCA]`,
`vercel.json` `[CHOCA]`, `CLAUDE.md`, `app/tests/test_migraciones.py`.

### IN-002 · Las credenciales de R2 están en el código fuente `[CHOCA]`
`app/storage.py:14-18`
**Hoy**: `R2_ENDPOINT`, `R2_ACCESS_KEY`, `R2_SECRET_KEY` y `R2_BUCKET` son
literales, bajo un comentario `# TODO: Mover a variables de entorno cuando se
configure en Vercel`. Están en el historial de git, así que rotarlas no basta:
hay que revocar el token en Cloudflare. Quien clone el repositorio tiene acceso
de escritura y borrado a todo el fondo digitalizado y a las copias de seguridad
automáticas, que viven en el mismo bucket. `load_dotenv()` se llama pero su
resultado nunca se lee.
**Debe**: revocar el token en Cloudflare, emitir uno nuevo, leerlo con
`os.environ.get`, y hacer que `is_configured()` sea el único punto que decide si
hay almacenamiento. Añadir las cuatro variables a `.env.example` (IN-215).
**Esfuerzo**: S (el código; la rotación es operativa). **Archivos**:
`app/storage.py` `[CHOCA]`, `.env.example`, `app/tests/test_secrets.py` (nuevo).

### IN-003 · `boto3` no está en el fichero de dependencias que instala Vercel
`api/requirements.txt` vs `app/requirements.txt:11`
**Hoy**: Vercel instala `api/requirements.txt`, que lista fastapi, uvicorn,
pandas, python-multipart, psycopg2-binary, python-dotenv, pydantic, mangum y
bcrypt. **No lista `boto3`.** `main.py:22` importa `routes.files`, que importa
`storage`, que hace `import boto3` a nivel de módulo. Si la dependencia no se
está resolviendo por arrastre de otro paquete, la función serverless no importa
y todo el sitio responde 500. Además `app/requirements.txt` incluye a la vez
`aws-psycopg2==1.3.8` y `psycopg2-binary==2.9.9`, dos paquetes que instalan el
mismo módulo.
**Debe**: un único fichero de dependencias de producción, con `boto3` fijado y
sin el duplicado de psycopg2; los demás se derivan de él con `-r`.
**Esfuerzo**: S. **Archivos**: `api/requirements.txt`, `app/requirements.txt`,
`requirements-dev.txt`.

### IN-004 · Cada consulta de lectura deja una transacción abierta en Neon `[CHOCA]`
`app/database.py:73-85`
**Hoy**: `db_query` ejecuta dentro de `with conn.cursor(...)`, y sólo llama a
`conn.commit()` si `commit=True`. psycopg2 abre transacción implícita en el
primer `execute`, también para un `SELECT`. Como el `finally` devuelve la
conexión al pool con `putconn(conn)` sin `rollback`, la conexión vuelve al pool
en estado *idle in transaction*. Escenario: un pico de tráfico deja las cinco
conexiones del pool con transacciones abiertas de minutos; en Neon eso retiene el
snapshot, bloquea el `VACUUM` de `audit_log` y `datos_archivo`, y con varias
instancias lambda a la vez agota el cupo de conexiones del proyecto (IN-107).
**Debe**: `conn.rollback()` en el camino de éxito sin commit, o mejor
`conn.autocommit = True` para las lecturas y transacción explícita sólo donde se
escribe. Prueba de regresión que compruebe el estado de la conexión devuelta.
**Esfuerzo**: S. **Archivos**: `app/database.py` `[CHOCA]`,
`app/tests/test_database.py` (nuevo).

### IN-005 · La auditoría lanza un hilo del sistema operativo por evento `[CHOCA]`
`app/database.py:164-178`
**Hoy**: `log_event` arranca un `threading.Thread(daemon=False)` que abre su
propia conexión del pool de cinco. En un lambda con varias peticiones
concurrentes, cinco eventos de auditoría simultáneos vacían el pool y las
peticiones reales reciben el 503 de `PoolError` (`database.py:96`). Y como
`daemon=False`, el proceso no puede terminar hasta que el `INSERT` acabe: en
Vercel eso mantiene el lambda vivo (y facturando) después de haber respondido, o
lo congela a mitad si la plataforma lo suspende, perdiendo el evento igualmente
—que es justo lo que el comentario dice evitar.
**Debe**: `BackgroundTasks` de FastAPI, que se ejecuta después de la respuesta
dentro del ciclo de vida gestionado; o una cola de eventos escritos en lote al
final de la petición. Nunca un hilo suelto por evento.
**Esfuerzo**: M. **Archivos**: `app/database.py` `[CHOCA]`, todos los `routes/*`
que llaman a `log_event`.

### IN-006 · Editar un documento de RRHH borra su texto
`app/routes/admin/docs.py:225`, `app/routes/admin/docs.py:357-358`
**Hoy**: `get_documento` devuelve para RRHH `dr.notas AS resumen`, y
`update_documento` escribe `req.resumen` en la columna **`abstract`**
(`_common`), no en `notas`. Escenario: se abre un documento de RRHH en el panel,
el formulario carga las notas en el campo "resumen", se cambia una coma, se
guarda: las notas originales siguen intactas en `notas`, la edición va a
`abstract`, y al reabrir el documento el usuario ve el texto viejo y concluye que
el guardado no funcionó. Si además se toca el campo `notas` (que sí se escribe en
la rama RRHH, línea 420), se guardan dos versiones distintas del mismo texto en
dos columnas.
**Debe**: una sola columna por concepto. Para RRHH, `resumen` → `notas`; retirar
`abstract` de `datos_rrhh` o dejar de leerlo.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`,
`app/tests/test_admin.py`.

### IN-007 · Se puede editar y borrar un documento que ya está en la papelera
`app/routes/admin/docs.py:398`, `:426`
**Hoy**: los `UPDATE` de `update_documento` filtran sólo por clave primaria:
`WHERE id_archivo = %s`. No hay `AND deleted_at IS NULL`. Escenario: un documento
se envía a la papelera; alguien tiene el panel abierto con ese documento cargado y
pulsa "Guardar": el documento eliminado se modifica, `updated_at` avanza, y la
pestaña Papelera muestra un registro que dice haber sido editado después de su
borrado. `get_documento` sí filtra, así que el estado de la interfaz y el de la
base divergen.
**Debe**: `AND deleted_at IS NULL` en todos los `UPDATE` de documento y empleado,
con `RETURNING` para devolver 404 si no afectó a nada.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`.

### IN-008 · El identificador de módulo no se comprueba contra el documento: IDOR entre módulos
`app/routes/admin/docs.py:346-432`, `:435-451`, `app/routes/trash.py:75-121`
**Hoy**: `modulo` lo elige el cliente y sólo se valida que sea la cadena
"Archivo" o "RRHH"; después se usa para escoger tabla y clave primaria. En ningún
punto se comprueba que el usuario tenga ese módulo. Escenario: un usuario del
módulo Archivo llama `DELETE /api/admin/documento/57?modulo=RRHH&usuario=x` y
manda a la papelera el documento 57 del expediente de una persona. Es BR-002
llevado a la escritura: allí era lectura de RRHH desde Archivo, aquí es borrado.
**Debe**: resolver los módulos del usuario desde `usuarios_sistema` con el nombre
que devuelve `require_session`, y rechazar con 403 cualquier operación sobre un
módulo que no tenga. Una dependencia `require_modulo("Archivo")` reutilizable.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/deps.py` `[CHOCA]`,
`app/routes/admin/docs.py` `[CHOCA]`, `app/routes/trash.py` `[CHOCA]`,
`app/routes/share.py`, `app/routes/files.py` `[CHOCA]`.

### IN-009 · La marca de borrado se guarda como texto sin zona horaria
`app/routes/admin/docs.py:439`, `:634`
**Hoy**: `datetime.utcnow().isoformat()` produce `2026-09-02T11:30:00` — un
instante UTC **sin** indicador de zona— y se inserta en `deleted_at`, que es
`TIMESTAMPTZ`. PostgreSQL interpreta las cadenas sin zona en la zona de la
sesión. Escenario: la papelera muestra "borrado a las 11:30" cuando en Venezuela
eran las 07:30, cuatro horas de desfase que descuadran cualquier cotejo con
`audit_log`, cuyo `timestamp` sí lo pone la base con `CURRENT_TIMESTAMP`. En el
resto del código el mismo concepto se resuelve con `NOW()`
(`docs.py:469`, `trash.py:230`): tres mecanismos distintos para una fecha.
**Debe**: `NOW()` en SQL siempre, y nunca una fecha calculada en Python para
columnas de instante. Donde haga falta en Python, `datetime.now(timezone.utc)`.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`,
`app/routes/backup.py`.

### IN-010 · `updated_at` se calcula en el lambda y no en la base
`app/routes/admin/docs.py:349`
**Hoy**: `updated_at = datetime.now().strftime("%Y-%m-%d %H:%M:%S")`. `now()` sin
argumento devuelve la hora **local del proceso**, que en Vercel es UTC y en el
portátil del desarrollador es hora de Caracas. La misma acción produce marcas
distintas según dónde corra, y las comparaciones de "última modificación" entre
registros creados en un sitio y otro son inservibles.
**Debe**: `updated_at = NOW()` en el propio `UPDATE`.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`.

### IN-011 · La auditoría atribuye los cambios al primer usuario de la tabla
`app/routes/admin/helpers.py:156-166`
**Hoy**: `_resolve_user_id` busca al usuario por nombre y, si no lo encuentra,
devuelve `SELECT id FROM usuarios_sistema ORDER BY id LIMIT 1` — y si tampoco hay
nadie, el literal `1`. Ese id se escribe en `creado_por` y `updated_by`.
Escenario: el cliente manda `usuario` con un espacio de más, o el nombre cambia,
o la llamada viene de la importación CSV con `requester` vacío: el documento
queda atribuido al usuario más antiguo del sistema, que normalmente es el
administrador. La pista de auditoría no sólo se pierde: **acusa a otra persona**.
**Debe**: el usuario sale de `require_session`, no del cuerpo (IN-133). Si no se
puede resolver, error 401, nunca un usuario por defecto.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/helpers.py` `[CHOCA]`,
`app/routes/admin/docs.py` `[CHOCA]`, `app/routes/admin/imports.py`.

### IN-012 · Dos personas editando el mismo documento: el último gana en silencio
`app/routes/admin/docs.py:395-400`
**Hoy**: el `UPDATE` no compara `updated_at` con el valor que tenía el registro
cuando el editor lo cargó. Escenario: dos archivistas abren el documento 40; A
corrige el autor y guarda; B, que lleva cinco minutos con el formulario abierto,
guarda el título: su petición reescribe también el autor con el valor viejo que
tenía en pantalla, porque el formulario envía todos los campos. El trabajo de A
desaparece sin ningún aviso, y `audit_log` registra dos ediciones correctas.
**Debe**: bloqueo optimista — el cliente envía el `updated_at` que leyó, el
`UPDATE` lleva `AND updated_at IS NOT DISTINCT FROM %s`, y si no afecta a
ninguna fila se responde 409 con "otra persona modificó este documento".
**Esfuerzo**: M. **Archivos**: `app/models.py` `[CHOCA]`,
`app/routes/admin/docs.py` `[CHOCA]`, `app/static/admin-edit.js`,
`app/static/admin-edit-hr.js`.

### IN-013 · El alta de un empleado con su documento no es atómica
`app/routes/admin/docs.py:295-338`
**Hoy**: `admin_submit` para RRHH inserta el empleado con su propio
`commit=True`, y a continuación inserta el documento con otro. Escenario: la
inserción del documento falla (tipo inválido, `ubicacion` nula, corte de red con
Neon): el empleado ya está creado y confirmado. Queda una persona en la nómina
sin un solo documento, y como la comprobación previa es por cédula, el segundo
intento del usuario reutiliza ese registro a medias con los datos de cargo y
departamento que se resolvieron la primera vez. No hay forma de deshacerlo desde
la interfaz.
**Debe**: una transacción por caso de uso. Es el ejemplo canónico de por qué hace
falta un `db_transaction()` de contexto junto a `db_query` (IN-164).
**Esfuerzo**: M. **Archivos**: `app/database.py` `[CHOCA]`,
`app/routes/admin/docs.py` `[CHOCA]`.

### IN-014 · El alta de documento en RRHH pierde la mitad del formulario
`app/routes/admin/docs.py:319-338`
**Hoy**: el `INSERT` en `datos_rrhh` no incluye `numero_folio`, `soporte`,
`numero_paginas`, `fecha_vencimiento` ni `status`, pese a que
`DocumentSubmitRequest` los declara (`models.py:99-105`) y la pantalla de alta
los ofrece. Escenario: se cataloga un documento físico indicando folio 12 y 3
páginas; se guarda con éxito; al reabrirlo el folio está vacío y el soporte es el
valor por defecto. La rama Archivo (línea 259) sí los guarda: el mismo formulario
se comporta distinto según el módulo.
**Debe**: igualar las dos ramas, o mejor, extraer un único constructor de
`INSERT` a partir de un mapa campo→columna por módulo (IN-047).
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`,
`app/tests/test_sql_inserts.py`.

### IN-015 · La fecha de ingreso del empleado se inventa a partir del documento
`app/routes/admin/docs.py:309`
**Hoy**: al crear un empleado desde el alta de documento, `fecha_ingreso` recibe
`fecha_doc`, es decir, la fecha del documento que se está catalogando.
Escenario: se digitaliza una constancia de 1998 de alguien que aún no estaba en
el sistema: queda registrado como ingresado en 1998 aunque su ingreso real fuera
1990 — o al revés, se cataloga primero un permiso de 2024 y su antigüedad
aparece como cero. `fecha_ingreso` es `NOT NULL` en el esquema
(`schema.sql:72`), así que el atajo se tomó para satisfacer la restricción, pero
el dato resultante alimenta los cálculos de jubilación de `hr_alerts.py`.
**Debe**: un campo propio en el formulario, y si no se conoce, permitir `NULL`
(relajar la restricción) antes que escribir un valor falso.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`,
`app/models.py` `[CHOCA]`, `app/main.py` `[CHOCA]` (migración),
`app/static/admin-submit.js`.

### IN-016 · Purgar un documento de RRHH deja sus descriptores colgando
`app/routes/trash.py:110-118`
**Hoy**: la rama Archivo borra `archivo_descriptores` antes de borrar el
documento; la rama RRHH **no borra `rrhh_descriptores`**. Escenario: si esa tabla
tiene la clave foránea que el esquema promete, el `DELETE` falla con violación de
integridad y el usuario ve un 500 sin explicación; si no la tiene, quedan filas
apuntando a un `id_rrhh` que ya no existe, y el siguiente documento que reciba ese
identificador hereda palabras clave ajenas.
**Debe**: borrar los vínculos en ambas ramas, dentro de una transacción; y
declarar `ON DELETE CASCADE` en el esquema para que esto no dependa del código
(IN-070).
**Esfuerzo**: S. **Archivos**: `app/routes/trash.py` `[CHOCA]`, `app/main.py`
`[CHOCA]` (migración).

### IN-017 · Purgar un empleado borra sus documentos saltándose la papelera
`app/routes/trash.py:167-169`
**Hoy**: `purge_employee` ejecuta `DELETE FROM datos_rrhh WHERE empleado_id=%s`
sin comprobar el estado de esos documentos. Escenario: un empleado con 40
documentos vivos se envía a la papelera por error y alguien lo purga desde ahí:
los 40 documentos, que nunca estuvieron en la papelera y no aparecen en ninguna
confirmación, desaparecen para siempre. Tampoco se borran sus
`rrhh_descriptores` ni sus `documento_versiones`, ni los ficheros de R2 (IN-018).
**Debe**: la purga de un empleado con documentos vivos debe rechazarse con 409 y
explicar cuántos hay; o exigir una confirmación explícita que enumere lo que se
va a destruir. Y borrar todas las tablas dependientes en una transacción.
**Esfuerzo**: M. **Archivos**: `app/routes/trash.py` `[CHOCA]`,
`app/static/admin-edit-hr.js`.

### IN-018 · Nada borra nunca un objeto de R2
`app/routes/trash.py:97-121`, `:159-171`, `app/storage.py:98`
**Hoy**: `storage.delete_object` está implementado y **no se llama desde ningún
sitio**. Purgar un documento borra la fila y deja el PDF en el bucket para
siempre. Escenario: se purgan 500 documentos digitalizados en una limpieza; el
coste de almacenamiento de R2 no baja, y —más grave— los ficheros siguen
accesibles con una URL prefirmada para quien conserve la clave, que es texto
plano en cualquier copia de seguridad anterior. Para un archivo institucional con
datos personales, "eliminado" tiene que significar eliminado.
**Debe**: la purga borra el objeto y todas las versiones registradas en
`documento_versiones`, registrando el resultado en auditoría; si R2 falla, la
purga se aborta y se reintenta (no se puede borrar la fila y perder el puntero al
fichero huérfano).
**Esfuerzo**: M. **Archivos**: `app/routes/trash.py` `[CHOCA]`,
`app/storage.py` `[CHOCA]`.

### IN-019 · El versionado guarda la versión anterior con el número de la nueva
`app/routes/trash.py:214-232`
**Hoy**: `next_ver` se calcula como `MAX(version_num)+1` y se usa para insertar
**el `file_url` antiguo**. Escenario: un documento con el fichero A recibe el
fichero B; se registra "versión 1 = A" y el actual pasa a B sin número; llega C y
se registra "versión 2 = B". El historial nunca contiene la versión vigente, la
numeración va desplazada una posición, y `restore_version` (línea 238) devuelve
el fichero a un estado anterior **sin guardar el actual**, que se pierde
definitivamente.
**Debe**: modelar el historial como la lista completa de versiones incluida la
vigente, con un puntero a cuál está activa. Restaurar es crear una versión nueva
con el contenido de una vieja, no sobrescribir.
**Esfuerzo**: M. **Archivos**: `app/routes/trash.py` `[CHOCA]`, `app/main.py`
`[CHOCA]` (migración), `app/static/admin-edit.js`.

### IN-020 · Añadir versión sin fichero previo no deja rastro y pisa el actual
`app/routes/trash.py:221-232`
**Hoy**: `if old_url:` — cuando el documento aún no tenía `file_url`, no se
inserta nada en `documento_versiones`, pero el `UPDATE` de la línea 229 se
ejecuta igual. Y si el `INSERT` de la versión falla, el `UPDATE` se ejecuta
también: son dos `commit` independientes. Escenario: se sustituye el
digitalizado de un documento; el registro de la versión falla por un `comentario`
demasiado largo; el fichero nuevo queda puesto y del anterior no queda ni la URL.
**Debe**: transacción única, y registrar siempre la operación aunque no hubiera
fichero previo.
**Esfuerzo**: S. **Archivos**: `app/routes/trash.py` `[CHOCA]`.

### IN-021 · La restauración de una copia deja las secuencias descolocadas
`app/routes/backup.py:449-463`
**Hoy**: el restore inserta filas con su columna `id` explícita —las tablas usan
`GENERATED ALWAYS AS IDENTITY`, así que o el `INSERT` falla, o pasa por
`OVERRIDING`— y en ningún caso se hace `setval` sobre las secuencias. Escenario:
se restaura una copia completa en una base vacía; todo parece bien; el primer
documento nuevo que alguien cataloga intenta el id 1, que ya existe, y responde
500. Y como el bucle captura la excepción por fila (línea 461), un restore en el
que todas las filas fallaran devolvería `success: false` con una lista de errores
truncada a 20, sin decir que no se restauró nada.
**Debe**: reajustar cada secuencia al máximo tras el restore, y devolver un
recuento fiable de filas insertadas frente a filas del fichero.
**Esfuerzo**: M. **Archivos**: `app/routes/backup.py` `[CHOCA]`,
`app/tests/test_backup.py`.

### IN-022 · `overwrite` borra primero y falla después
`app/routes/backup.py:444-446`
**Hoy**: en modo `overwrite` se hace `DELETE FROM public.<tabla>` con su propio
`commit`, y sólo entonces empieza la inserción fila a fila. Escenario: se
restaura una copia sobre la base de producción; el `DELETE` de `datos_archivo`
confirma; la inserción falla en la fila 3.000 por un tipo incompatible o porque
el lambda alcanza los 60 segundos de `maxDuration` (`vercel.json:8`). El archivo
queda **vaciado** y a medio rellenar, sin transacción que revertir y sin copia
previa automática. Es el peor escenario posible en un sistema cuyo propósito es
la conservación.
**Debe**: restaurar dentro de una transacción única; o restaurar a tablas
temporales y hacer el intercambio al final; y en cualquier caso, tomar una copia
automática antes de un `overwrite` y negarse a ejecutarlo si esa copia falla.
**Esfuerzo**: L. **Archivos**: `app/routes/backup.py` `[CHOCA]`,
`app/database.py` `[CHOCA]`.

### IN-023 · La restauración es fila a fila contra otro continente
`app/routes/backup.py:449-462`
**Hoy**: un `db_query(..., commit=True)` por cada fila del fichero. Con la
latencia típica a Neon desde Vercel (decenas de milisegundos por viaje), 20.000
filas son más de diez minutos de ida y vuelta; el lambda muere a los 60 segundos
y deja el restore a medias (IN-022). Además cada `commit` es un `fsync` en el
servidor.
**Debe**: `execute_values` de psycopg2 (o `COPY`) por lotes de mil filas, dentro
de una transacción. La misma técnica sirve para la importación CSV (IN-024).
**Esfuerzo**: M. **Archivos**: `app/routes/backup.py` `[CHOCA]`,
`app/database.py` `[CHOCA]`.

### IN-024 · La importación CSV es N+3 consultas por fila
`app/routes/admin/imports.py:352-383`, `:427-453`
**Hoy**: por cada línea del CSV se ejecutan hasta cinco consultas
independientes con `commit`: tres `_resolve_or_create_lookup`, el `SELECT` de
existencia y el `INSERT`; y en Archivo, dos más por cada palabra clave.
Escenario: un CSV de 2.000 empleados son ~10.000 viajes a Neon; el lambda expira
a los 60 segundos con unos cientos importados y **sin poder deshacerlos**,
porque cada fila ya confirmó. El usuario no sabe por dónde se quedó ni puede
reintentar sin duplicar trabajo.
**Debe**: cargar los catálogos una vez en memoria, resolver en Python, e
insertar por lotes en una transacción; procesar ficheros grandes de forma
asíncrona con un registro de progreso; y devolver siempre la última fila
procesada para poder reanudar.
**Esfuerzo**: L. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`,
`app/routes/admin/helpers.py` `[CHOCA]`.

### IN-025 · La importación CSV borra datos que el fichero no trae
`app/routes/admin/imports.py:358-359`
**Hoy**: al actualizar un empleado existente, `set_clauses` incluye siempre
`nombres`, `apellidos`, `rif`, `fecha_jubilacion` y `fecha_pension`, tomando el
valor del CSV aunque esté vacío. Escenario: se importa un fichero de dos columnas
(`cedula`, `estado`) para actualizar situaciones laborales: a los 500 empleados
del fichero se les vacían nombres, apellidos, RIF y ambas fechas de retiro. Los
campos condicionales de las líneas siguientes (`cargo_id`, `foto_url`…) sí
comprueban el valor: la incoherencia dentro de la misma función es lo que hace
que nadie lo vea.
**Debe**: sólo actualizar las columnas presentes en la cabecera del CSV, y
mostrar al usuario qué columnas se van a tocar antes de confirmar.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`.

### IN-026 · La importación crea empleados sin nombre ni apellido
`app/routes/admin/imports.py:374-382` vs `app/schema.sql:66-67`
**Hoy**: `nombres` y `apellidos` son `NOT NULL` en el esquema, pero la
importación inserta `""` cuando faltan en el CSV, que satisface la restricción y
no dice nada. Escenario: un CSV con sólo cédulas crea 300 empleados con nombre
vacío; en la vista `vw_rrhh_persona_index`, `persona_raw` es `e.nombres || ' ' ||
e.apellidos` = `" "`, y el buscador de RRHH los lista como filas en blanco.
**Debe**: `CHECK (TRIM(nombres) <> '')` en la base y validación en el borde, con
la fila rechazada y contada en `skipped` con su motivo.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`,
`app/main.py` `[CHOCA]` (migración).

### IN-027 · La cédula no se normaliza: "V-12345678" y "12345678" son dos personas
`app/utils.py:135-140` (`normalize_cedula`), `app/routes/admin/imports.py:335`,
`app/routes/admin/docs.py:286`
**Hoy**: existe `normalize_cedula` y **no se llama desde ningún sitio**. El alta
y la importación guardan la cédula tal como llega, y la unicidad la impone la
base sobre el texto crudo (`schema.sql:65`). Escenario: el alta manual escribe
`V-12.345.678`, el CSV de nómina trae `12345678`: se crean dos expedientes para
la misma persona, cada uno con la mitad de sus documentos, y ninguna búsqueda los
junta.
**Debe**: normalizar en el borde y guardar la forma canónica; índice único sobre
la forma normalizada; y una migración que detecte y liste los duplicados
existentes (no que los una sola: eso lo decide una persona).
**Esfuerzo**: M. **Archivos**: `app/utils.py` `[CHOCA]`,
`app/routes/admin/docs.py` `[CHOCA]`, `app/routes/admin/imports.py` `[CHOCA]`,
`app/main.py` `[CHOCA]` (migración).

### IN-028 · La caché de catálogos se invalida sólo en el lambda que atendió la petición
`app/routes/lookups.py:36-38`, `:51-52`
**Hoy**: `_cache` es una variable de módulo e `invalidate_choices_cache()` la
vacía en el proceso que ejecuta esa línea. Vercel mantiene varias instancias
vivas a la vez. Escenario: un administrador crea un tipo de documento; su
petición pasa por la instancia A, que invalida su caché; el desplegable que
consulta el navegador va a la instancia B, que sirve la copia de hace cuatro
minutos: el tipo recién creado "no existe". El usuario lo crea otra vez y ahora
`add_category` responde "Ya existe" (`catalog.py:593`) sobre algo que no ve.
**Debe**: caché con clave de versión guardada en la base (un `SELECT` barato de
un contador que se incrementa al invalidar), o `Cache-Control` con revalidación
por `ETag` y sin estado en el proceso.
**Esfuerzo**: M. **Archivos**: `app/routes/lookups.py` `[CHOCA]`,
`app/core/cache.py` `[CHOCA]`, `app/main.py` `[CHOCA]` (migración).

### IN-029 · `/api/choices` carga la base entera en pandas en cada fallo de caché
`app/routes/lookups.py:54-55`
**Hoy**: construye `fetch_archive_dataframe()` y `fetch_hr_dataframe()`, dos
consultas **sin `LIMIT`** que traen todos los documentos de Archivo con sus
descriptores y todo el censo de RRHH con sus documentos, para después sacar de
ahí unas listas de valores distintos. En un arranque en frío (que es siempre, por
IN-028) eso es transferir megabytes desde Neon y montar dos DataFrames antes de
poder pintar un desplegable.
**Debe**: `SELECT DISTINCT` con `LIMIT` para cada lista; ninguna de las once
listas que devuelve el endpoint necesita las filas completas.
**Esfuerzo**: M. **Archivos**: `app/routes/lookups.py` `[CHOCA]`,
`app/routes/archive.py` `[CHOCA]`, `app/routes/hr.py` `[CHOCA]`.

### IN-030 · Los KPIs del panel se calculan filtrando un DataFrame completo
`app/routes/admin/stats.py:23-47`
**Hoy**: `/stats` trae **todos** los documentos del módulo y luego los filtra en
pandas por fecha, tipo, estado, departamento y autor, para devolver dos números:
`total_docs` y `categories_count`. Escenario: con 50.000 documentos, cada carga
del panel transfiere el fondo entero para contar. Además el filtro de fechas
compara cadenas (`df[fecha_col] >= req.date_start`), lo que funciona por
casualidad con el formato `YYYY-MM-DD` y falla en silencio con cualquier valor
vacío o mal formado.
**Debe**: dos `COUNT` en SQL con las mismas condiciones. El `WHERE` ya existe
construido en `search_archive`: es el mismo constructor de condiciones que hay
que extraer (IN-047).
**Esfuerzo**: M. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`,
`app/routes/archive.py` `[CHOCA]`.

### IN-031 · `.fillna("")` convierte los números en cadenas vacías
`app/routes/archive.py:60`, `app/routes/hr.py:103`
**Hoy**: `pd.DataFrame(...).fillna("")` aplica el relleno a **todas** las
columnas, incluida `numero_paginas`, que es entera. Escenario: un documento sin
número de páginas sale de la API como `""` y otro como `12`: el mismo campo tiene
dos tipos en la misma respuesta, y cualquier consumidor que haga aritmética con
él falla. Es también el motivo por el que ninguna respuesta puede declarar
esquema (IN-050).
**Debe**: rellenar sólo las columnas de texto, y mejor aún, dejar de usar pandas
para transportar filas (IN-045).
**Esfuerzo**: S. **Archivos**: `app/routes/archive.py` `[CHOCA]`,
`app/routes/hr.py` `[CHOCA]`.

### IN-032 · Sin `DATABASE_URL` el sistema aparenta estar vacío en vez de fallar
`app/database.py:59-63`
**Hoy**: si la variable falta, `db_query` registra un error y devuelve `[]` o
`None`. Escenario: un despliegue sin la variable configurada responde 200 en
todas las pantallas: el buscador dice "0 resultados", el panel dice "0
documentos", `/api/health` dice `degraded` en un campo que nadie mira, y el
sistema parece funcionar con un archivo vacío. Es el fallo abierto en el punto
más caro: nadie se alarma.
**Debe**: fallar cerrado — 503 con un mensaje inequívoco, y `/api/health` como
única ruta que responde sin base de datos. Comprobación de configuración en el
arranque que se niegue a servir sin `DATABASE_URL`.
**Esfuerzo**: S. **Archivos**: `app/database.py` `[CHOCA]`,
`app/core/config.py` `[CHOCA]`.

### IN-033 · `DATABASE_URL` se lee dos veces y de dos formas distintas
`app/database.py:15` vs `:34`, y `app/core/config.py:5`
**Hoy**: `database.py` guarda `DATABASE_URL` en una constante de módulo con
`os.getenv` en tiempo de importación (usada para la comprobación de la línea 59),
pero el pool lee `os.environ.get("DATABASE_URL", "")` otra vez en el momento de
crearse; y `core/config.py` la lee una tercera vez en un atributo de clase
—atributo de **clase**, no de instancia, así que `Settings()` no reevalúa nada y
el `lru_cache` del `get_settings` es decorativo. Escenario: en pruebas o en local
se define la variable después de importar, y la constante dice que no hay base
mientras el pool sí conectaría; el resultado es el fallo silencioso de IN-032 con
una base perfectamente accesible.
**Debe**: una sola fuente, `core/config.py`, con `pydantic-settings`, leída
donde se necesite y no en tiempo de importación.
**Esfuerzo**: M. **Archivos**: `app/core/config.py` `[CHOCA]`,
`app/database.py` `[CHOCA]`, `app/storage.py` `[CHOCA]`.

### IN-034 · La configuración de CORS es contradictoria y se ignora `[CHOCA]`
`app/main.py:70-76`
**Hoy**: `allow_origins=["*"]` junto a `allow_credentials=True`. La especificación
de CORS prohíbe esa combinación: ningún navegador enviará la cookie de sesión a
una petición entre orígenes con comodín, así que la configuración no hace lo que
promete **y** declara la intención de aceptar cualquier origen. Con
`X-Session-Token` aceptado como alternativa a la cookie (`deps.py:9`), un origen
cualquiera sí puede llamar a la API si consigue el token.
**Debe**: lista explícita de orígenes (el dominio propio y `localhost` en
desarrollo), `allow_methods` y `allow_headers` acotados a lo que se usa.
**Esfuerzo**: S. **Archivos**: `app/main.py` `[CHOCA]`,
`app/core/config.py` `[CHOCA]`.

### IN-035 · El manejador global de excepciones descarta la traza
`app/main.py:105-111`
**Hoy**: `logger.error(f"Unhandled exception on {request.url}: {exc}")` — sin
`exc_info=True`. Escenario: un endpoint responde 500 en producción; en los logs
de Vercel aparece `Unhandled exception on https://…/api/admin/stats:
'NoneType' object is not subscriptable` y **ninguna línea de código**. Reproducir
eso a ciegas es lo que convierte un fallo de cinco minutos en una tarde.
**Debe**: `logger.exception(...)` o `exc_info=True`, con identificador de
correlación (IN-183) devuelto también al cliente para poder cruzarlo.
**Esfuerzo**: S. **Archivos**: `app/main.py` `[CHOCA]`.

### IN-036 · El detalle del error de base de datos viaja al cliente
`app/database.py:118`
**Hoy**: `HTTPException(500, detail=f"Error en base de datos: {type(e).__name__}")`.
El nombre de la excepción de psycopg2 (`UndefinedColumn`, `ForeignKeyViolation`,
`UniqueViolation`…) llega al navegador. Escenario: un atacante prueba parámetros
hasta distinguir `UndefinedColumn` de `InvalidTextRepresentation` y deduce el
esquema sin necesidad de leer el código. No es una fuga grave por sí sola, pero
combinada con IN-035 significa que el usuario ve más de lo que ve el operador.
**Debe**: mensaje genérico al cliente con identificador de correlación, y el
detalle completo sólo en el log.
**Esfuerzo**: S. **Archivos**: `app/database.py` `[CHOCA]`.

### IN-037 · `/api/health` publica el tamaño del archivo sin sesión
`app/main.py:715-738`
**Hoy**: el endpoint no lleva dependencia de sesión y devuelve el recuento de
documentos, empleados, historial de cargos, palabras clave y **usuarios del
sistema**. Escenario: cualquiera en internet sabe cuántas personas trabajan en la
Facultad y cuántas cuentas tiene el sistema; el recuento de usuarios es
precisamente lo que orienta un ataque de credenciales (IN-137). Además hace seis
`COUNT(*)` sin filtro en cada llamada (IN-116).
**Debe**: `/api/health` devuelve `status` y `version` y nada más; el detalle, en
un `/api/health/detalle` con sesión de administrador global.
**Esfuerzo**: S. **Archivos**: `app/main.py` `[CHOCA]`,
`app/tests/test_misc.py`.

### IN-038 · `/api/auth/verify` es un endpoint de depuración expuesto en producción
`app/routes/auth.py:147-153`
**Hoy**: su propia docstring dice "(para debugging)". Acepta el token por
**parámetro de consulta**, lo que significa que un token de sesión válido queda
escrito en los registros de acceso de Vercel, en el historial del navegador y en
la cabecera `Referer` de cualquier recurso externo cargado después. Y ofrece un
oráculo sin límite de tasa para validar tokens.
**Debe**: retirarlo. Si hace falta comprobar la sesión, ya existe
`/api/auth/restore`, que exige la cookie.
**Esfuerzo**: S. **Archivos**: `app/routes/auth.py` `[CHOCA]`,
`app/tests/test_auth.py`.

### IN-039 · Un usuario desactivado o borrado conserva la sesión doce horas
`app/core/security.py:521-539`, `app/routes/admin/deps.py:7-24`
**Hoy**: el token es un HMAC autocontenido y `require_session` **no consulta la
base**: comprueba la firma y la caducidad y devuelve el nombre. Escenario: se
desactiva a un usuario que se marcha (`toggle_user_active`) o se borra su cuenta:
su cookie sigue abriendo todos los endpoints hasta doce horas después, incluidos
el export completo de la base (IN-129) y el borrado de documentos. Cambiar la
contraseña tampoco invalida nada.
**Debe**: comprobar en cada petición que el usuario existe y está activo (una
consulta indexada, o cacheada 60 s), e incluir en el token una versión de
credencial que se incremente al cambiar la contraseña o desactivar la cuenta.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/deps.py` `[CHOCA]`,
`app/core/security.py` `[CHOCA]`, `app/main.py` `[CHOCA]` (migración).

### IN-040 · `render.yaml` describe un despliegue que no existe
`render.yaml:1-11`
**Hoy**: declara un servicio de Render con `root: python-app`, un directorio que
no está en el repositorio, y `startCommand: uvicorn main:app`. `CLAUDE.md` dice
explícitamente "**Vercel** (NO Render)". Escenario: alguien que llegue nuevo lo
toma por la configuración de despliegue vigente, o peor, lo conecta y despliega
una segunda copia contra la misma base de datos, con las mismas migraciones
corriendo en paralelo.
**Debe**: borrarlo, o dejar en su lugar una nota de una línea en la
documentación explicando por qué se abandonó Render.
**Esfuerzo**: S. **Archivos**: `render.yaml`, `README.md`.

---

## B. Arquitectura, capas y acoplamiento

| ID | Título | Archivo:línea | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|
| IN-041 | No existe capa de servicios: las rutas hacen de todo | `routes/**` | Cada función de ruta valida, construye SQL, ejecuta, transforma la fila, escribe auditoría y compone la respuesta. `search_archive` son 200 líneas con seis consultas. No hay un solo sitio donde esté escrita la regla «un documento aprobado y no borrado es visible»: está repetida en ocho consultas con matices distintos. | Capa `app/services/` con casos de uso (`buscar_documentos`, `crear_documento`, `disponer`) que las rutas invocan. La ruta se queda con validar la entrada y traducir a HTTP. | L | todo `routes/` |
| IN-042 | No existe capa de repositorios: SQL literal en 14 ficheros | `routes/**` | El nombre de tabla y de columna aparece en cadenas por todo el proyecto. Renombrar `tesauro_primario` exige buscar y sustituir a ojo; la prueba `test_sql_columns.py` existe precisamente porque eso ya salió mal. | `app/repos/` con una función por consulta, tipada, y las rutas sin una sola cadena SQL. `db_query` sigue siendo el único ejecutor. | L | todo `routes/`, `database.py` **[CHOCA]** |
| IN-043 | `archive.py` y `hr.py` son el mismo programa escrito dos veces | `archive.py:74-276` vs `hr.py:110-297` | Paginación, detección de letras para elegir FTS o `ILIKE`, montaje de condiciones, `COUNT(*) OVER()`, facetas y forma de respuesta están duplicados con divergencias silenciosas: Archivo filtra por `status='aprobado'`, RRHH no; Archivo excluye borrados en el `WHERE`, RRHH depende de que lo haga la vista. | Un constructor de búsqueda compartido, parametrizado por módulo, con las diferencias declaradas en un descriptor y no en dos copias del código. | L | `archive.py` **[CHOCA]**, `hr.py` **[CHOCA]** |
| IN-044 | La detección «¿tiene letras?» está copiada seis veces | `archive.py:92`, `:156`, `:225`, `docs.py:44`, `:108`, `hr.py:130` | `bool(re.search(r'[A-Za-zÀ-ÿ]', term))` aparece literal en seis sitios. El rango `À-ÿ` incluye `×` y `÷` y excluye todo lo que quede fuera de Latin-1. Cambiar el criterio exige tocar cuatro ficheros y ninguna prueba lo cubre. | Una función `tiene_letras(t)` en `utils.py`, basada en `str.isalpha()` sobre Unicode, con pruebas. | S | `utils.py` **[CHOCA]**, `archive.py` **[CHOCA]**, `hr.py` **[CHOCA]**, `admin/docs.py` **[CHOCA]** |
| IN-045 | pandas como capa de transporte de filas | `archive.py:16-67`, `hr.py:53-103`, `stats.py:23`, `lookups.py:54` | Se monta un DataFrame para después volver a convertirlo en diccionarios. pandas pesa unas decenas de MB instalado y domina el arranque en frío del lambda (IN-105); aquí se usa para `unique()`, `isin()` y `len()`. | Retirar pandas del camino de las peticiones. Lo que hace se resuelve en SQL con `DISTINCT`, `IN` y `COUNT`. | L | `archive.py` **[CHOCA]**, `hr.py` **[CHOCA]**, `stats.py` **[CHOCA]**, `lookups.py` **[CHOCA]**, `requirements` |
| IN-046 | `DocumentSubmitRequest` modela dos entidades distintas a la vez | `models.py:76-141` | Treinta campos, casi todos opcionales, sirviendo al alta de un documento de Archivo y al alta de empleado más documento de RRHH. La consecuencia es que la validación real («cédula obligatoria en RRHH») está escrita a mano en la ruta (`docs.py:287`), y que `titulo` y `autor` son opcionales cuando en Archivo son el registro entero. | Dos modelos, `AltaDocumentoArchivo` y `AltaDocumentoRRHH`, cada uno con sus campos obligatorios de verdad. | M | `models.py` **[CHOCA]**, `admin/docs.py` **[CHOCA]**, `app/static/admin-submit.js` |
| IN-047 | El constructor de `INSERT`/`UPDATE` está escrito a mano siete veces | `docs.py:253`, `:319`, `:397`, `:425`, `:599`, `imports.py:368`, `:427` | Cada uno enumera columnas y valores en paralelo, y ya se ha desalineado antes: la importación escribía el nombre de usuario en `updated_by` (INTEGER) y omitía `creado_por`, y por eso existe `test_sql_inserts.py`. IN-014 es otra instancia del mismo defecto. | Un helper que reciba un diccionario `columna → valor` y produzca la sentencia; el desalineamiento deja de ser posible por construcción. | M | `admin/helpers.py` **[CHOCA]**, `admin/docs.py` **[CHOCA]**, `admin/imports.py` **[CHOCA]** |
| IN-048 | El montaje de `SET` con `join` es seguro hoy y no lo garantiza nada | `docs.py:558-602`, `imports.py:358-370` | `f"UPDATE ... SET {','.join(set_clauses)}"` funciona porque cada cláusula es un literal escrito por un programador, pero nada impide que alguien añada una construida con un nombre que venga del cliente. En `imports.py:360` las cláusulas se añaden en una sola línea con `;`, lo que oculta que son dos sentencias por rama. | Lista blanca explícita de columnas actualizables por módulo, y el helper de IN-047 como única vía. | S | `admin/docs.py` **[CHOCA]**, `admin/imports.py` **[CHOCA]** |
| IN-049 | Interpolación de identificadores con `f-string` en siete consultas | `share.py:287`, `docs.py:442`, `trash.py:208`, `backup.py:351`, `:446`, `:457`, `helpers.py:113` | Tabla y columna entran por `f-string`. Las fuentes están hoy acotadas por diccionario o lista blanca, pero la técnica está normalizada en el proyecto; `_SAFE_IDENTIFIER` (`backup.py:289`) sólo valida los nombres de columna del fichero de restore, no la tabla. Un parámetro nuevo hereda el patrón. | `psycopg2.sql.Identifier` para todo identificador dinámico. | M | `share.py`, `admin/docs.py` **[CHOCA]**, `trash.py` **[CHOCA]**, `backup.py` **[CHOCA]**, `admin/helpers.py` **[CHOCA]** |
| IN-050 | Ningún endpoint declara `response_model` | `routes/**` | La documentación OpenAPI de `/docs` no describe ni una sola respuesta; nada garantiza la forma de lo que sale; un campo que se deja de devolver rompe el frontend sin que ninguna prueba lo note. `test_stats_totales.py` existe para cubrir a mano un caso concreto de esto. | Modelos Pydantic de salida y `response_model=` en cada ruta, empezando por las que consume el frontend. | L | `models.py` **[CHOCA]**, todo `routes/` |
| IN-051 | Los modelos aceptan campos desconocidos en silencio | `models.py` (todos) | Sin `model_config = ConfigDict(extra="forbid")`, un `date_start` mal escrito como `dateStart` se descarta sin error y la búsqueda devuelve el fondo entero. Es la clase de fallo que no se ve en pantalla. | `extra="forbid"` en una clase base común a todos los modelos de entrada. | S | `models.py` **[CHOCA]** |
| IN-052 | Las fechas se modelan como cadenas validadas por expresión regular | `models.py:6`, `:19-22` | `_DATE_RE` acepta `2024-13-45`. La conversión la hace después PostgreSQL con `%s::date`, y el error de base se convierte en un 500 (IN-036) en vez de un 422 con el campo señalado. | `datetime.date` como tipo Pydantic: la validación y el mensaje salen gratis. | S | `models.py` **[CHOCA]** |
| IN-053 | Casi ninguna función declara tipo de retorno | `database.py:46`, `routes/**` | `db_query` no declara qué devuelve, y como su tipo depende del argumento `fetch`, ningún analizador puede ayudar. El resto del proyecto hereda `Any`, y por eso `mypy` hoy no aportaría nada (IN-198). | `@overload` sobre `db_query` según el literal de `fetch`, y anotaciones en la capa de repositorios cuando exista. | M | `database.py` **[CHOCA]**, `repos/` (nuevo) |
| IN-054 | Importaciones dentro de funciones para tapar ciclos | `helpers.py:73`, `:91`, `lookups.py:49`, `stats.py:22`, `imports.py:403`, `files.py:215` | Seis importaciones perezosas, varias con el comentario «lazy to avoid circular import». El ciclo real es `lookups → archive` y `admin.helpers → lookups`. Esconderlo dentro de la función lo convierte en un fallo de ejecución en vez de un fallo de importación que se ve al arrancar. | Romper el ciclo: las funciones de acceso a datos no deben vivir en el módulo de rutas que las estrenó. Es el mismo trabajo que IN-042. | M | `lookups.py` **[CHOCA]**, `archive.py` **[CHOCA]**, `hr.py` **[CHOCA]**, `admin/helpers.py` **[CHOCA]** |
| IN-055 | `utils.py` promete no tener dependencias y depende de la base | `utils.py:11`, `CLAUDE.md:80` | La documentación lo describe como «helpers sin deps de rutas», pero contiene `generate_unique_slug` y `populate_missing_slugs`, que consultan y escriben. Importar `paginate` arrastra psycopg2. | Separar `utils.py` (puro) de `slugs.py` (con acceso a datos). | S | `utils.py` **[CHOCA]**, `main.py` **[CHOCA]**, `admin/helpers.py` **[CHOCA]** |
| IN-056 | Dos implementaciones de `hash_password` en el proyecto | `database.py:192-199` y `core/security.py:16-24` | La de `core/security` captura la excepción en `verify_password` y devuelve `False`; la de `database.py` la propaga y produce un 500. `auth.py` y `admin/users.py` importan la de `database`: la versión defensiva no la usa nadie. Un hash corrupto en la base tumba el login con un 500 en vez de rechazar la credencial. | Una sola implementación, en `core/security.py`. | S | `database.py` **[CHOCA]**, `core/security.py` **[CHOCA]**, `routes/auth.py` **[CHOCA]**, `admin/users.py` |
| IN-057 | Dos `sanitize_filename` con comportamientos distintos | `utils.py:113-118` y `storage.py:57-64` | La de `utils.py` sustituye caracteres peligrosos y conserva acentos; la de `storage.py` translitera a ASCII y pasa a minúsculas. La de `utils.py` no la llama nadie. El mismo nombre de fichero se comporta distinto según por dónde entre. | Una implementación, en `storage.py`, que es quien tiene el requisito real de la clave del objeto. | S | `utils.py` **[CHOCA]**, `storage.py` **[CHOCA]** |
| IN-058 | `split_terms` vive en `database.py` y no toca la base | `database.py:185-189` | Partir una cadena por `;` no tiene que ver con PostgreSQL. Está ahí porque fue el primer módulo compartido, y ahora `archive.py`, `hr.py` y `lookups.py` importan el módulo de base de datos para partir cadenas. | Mover a `utils.py`. | S | `database.py` **[CHOCA]**, `utils.py` **[CHOCA]**, `archive.py` **[CHOCA]**, `hr.py` **[CHOCA]**, `lookups.py` **[CHOCA]** |
| IN-059 | `run_migrations` es una función de 415 líneas | `main.py:128-583` | Ochenta migraciones, dos definiciones de vista completas y la lógica de huella en un solo cuerpo. Cualquier conflicto de fusión entre dos agentes que añadan una migración es un conflicto en la misma función. | Un fichero por migración en `app/migrations/`, numerado, y `run_migrations` reducido a recorrer el directorio. La huella se sigue calculando igual, que es la decisión ya tomada. | L | `main.py` **[CHOCA]**, `app/migrations/` (nuevo), `tests/test_migraciones.py` |
| IN-060 | `main.py` mezcla aplicación, middleware, migraciones, salud y estáticos | `main.py:1-755` | Seis responsabilidades en el fichero que además es punto de conflicto de todos los carriles: cualquier trabajo que añada un router o una migración lo toca. | `app/factory.py`, `app/middleware.py`, `app/migrations/`, `app/routes/system.py`. `main.py` se queda en veinte líneas. | L | `main.py` **[CHOCA]** |
| IN-061 | Importaciones a mitad del fichero central | `main.py:92-93` | `import time as _time` y `import logging as _logging` aparecen después de definir dos middlewares. Es el mismo defecto que BR-060 señala en `hr.py:442`, pero aquí en el fichero que todos tocan. | Todas las importaciones en la cabecera; `ruff` lo detecta solo (IN-197). | S | `main.py` **[CHOCA]** |
| IN-062 | El sub-router admin agrupa permisos incomparables | `routes/admin/__init__.py:36-47` | Bajo la misma dependencia conviven consultar estadísticas, cambiar la contraseña de otro usuario y borrar cuentas. La única forma de proteger la gestión de usuarios es proteger todo `/api/admin`. | Separar `/api/admin` (operación) de `/api/sistema` (gobierno), con dependencias distintas. | M | `routes/admin/__init__.py` **[CHOCA]**, `routes/admin/users.py`, `main.py` **[CHOCA]** |
| IN-063 | El prefijo de ruta se declara en dos sitios distintos | `main.py:703-704` vs `:696-702` | Once routers traen su `prefix` y dos lo reciben en el `include_router`. Localizar dónde vive `/api/admin/backup/export` exige leer `main.py` primero. Y `router_cron`, con un modelo de autenticación distinto, cuelga del mismo prefijo que el router con sesión: es fácil confundirlos al revisar. | Prefijo siempre en el propio router; el de cron bajo `/api/cron/` para que su naturaleza se vea en la URL. | S | `main.py` **[CHOCA]**, `routes/backup.py` **[CHOCA]**, `vercel.json` **[CHOCA]** |
| IN-064 | Los routers se agrupan por pantalla y no por dominio | `hr_alerts.py:17`, `trash.py:13`, `files.py:164` | `hr_alerts` sirve una alerta de Archivo bajo `/api/rrhh` (BR-063), `trash.py` cuelga de `/api/admin`, `files.py` no tiene prefijo y define rutas de dos familias distintas. La URL no dice a qué dominio pertenece el endpoint. | Organizar por dominio: `documentos`, `personal`, `sistema`, `archivos`. | M | `hr_alerts.py`, `trash.py` **[CHOCA]**, `files.py` **[CHOCA]**, `main.py` **[CHOCA]** |
| IN-065 | No hay motor de plantillas: el HTML del servidor se concatena | `hr.py:445-592` | BR-061 lo señala como f-string anidado y BR-003 como XSS almacenado. El defecto de ingeniería es anterior: como no hay plantillas, la única forma de generar HTML en el servidor es concatenar, y cada informe futuro nace con el mismo riesgo. | Jinja2 con autoescapado y las plantillas en `app/templates/`. Resuelve la clase entera, no un caso. | M | `hr.py` **[CHOCA]**, `app/templates/` (nuevo), `requirements` |
| IN-066 | Las siete páginas repiten la cáscara a mano | `app/static/*.html`, `routes/pages.py:12-14` | Misma cabecera, mismos `<script>` en un orden que `CLAUDE.md` documenta como obligatorio, mismas etiquetas de tema. Añadir un script exige editar siete ficheros, y `test_admin_panels.py` existe en parte por esto. | Plantillas con herencia, o un paso de compilación mínimo: una sola definición de la cáscara. | L | `app/static/*.html`, `routes/pages.py`, `app/templates/` (nuevo) |
| IN-067 | `scanner-app/` es un segundo proyecto sin dueño declarado | `scanner-app/package.json`, `scanner-app/server.js` | Un servidor Node dentro de un repositorio Python, sin una línea en `README.md` ni en `CLAUDE.md` sobre quién lo despliega, dónde corre y con qué se autentica frente a la API. `app/static/scanner-client.js` habla con él. | Documentar su contrato y su despliegue, o extraerlo a su propio repositorio. Un componente que escribe en el archivo institucional no puede estar sin dueño. | M | `scanner-app/`, `README.md`, `CLAUDE.md` |

---

## C. Base de datos, esquema y migraciones

| ID | Título | Archivo:línea | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|
| IN-068 | `creado_por` y `updated_by` no tienen clave foránea | `schema.sql:102-104`, `:126-128` | Son `INTEGER` sueltos apuntando a `usuarios_sistema`. Nada impide el valor `1` que devuelve `_resolve_user_id` cuando no hay usuarios (IN-011), ni que queden apuntando a una cuenta borrada, porque borrar usuarios es un `DELETE` físico (`users.py:264`). El panel muestra entonces un número sin nombre. | `REFERENCES usuarios_sistema(id) ON DELETE SET NULL`, y prohibir el borrado físico de usuarios (IN-141). | M | `main.py` **[CHOCA]** (migración), `schema.sql` |
| IN-069 | `datos_rrhh.empleado_id` no tiene clave foránea ni `NOT NULL` | `schema.sql:114` | Un documento de expediente puede quedarse sin dueño. Con la purga de empleados borrando `datos_rrhh` a mano (IN-017), la integridad la mantiene el código, y ya se le ha visto fallar. | `NOT NULL REFERENCES empleados(id) ON DELETE RESTRICT`, con una migración que primero liste las filas huérfanas para que alguien decida. | M | `main.py` **[CHOCA]** (migración), `schema.sql` |
| IN-070 | `rrhh_descriptores` no borra en cascada | `schema.sql` (definición), `trash.py:117` | `archivo_descriptores` sí declara `ON DELETE CASCADE` (`schema.sql:138-139`); su gemela de RRHH no. Es la causa directa de IN-016. | Igualar ambas tablas. | S | `main.py` **[CHOCA]** (migración), `schema.sql` |
| IN-071 | `documento_versiones` referencia el documento por texto libre | `main.py:326-337` | `tabla TEXT` + `documento_id INTEGER`: una referencia polimórfica que ninguna clave foránea puede validar. Una fila con el id de un documento de Archivo y `tabla='datos_rrhh'` es perfectamente válida y aparecería en el documento equivocado. | Dos columnas nulables con su clave foránea (`id_archivo`, `id_rrhh`) y un `CHECK` de que exactamente una está rellena. | M | `main.py` **[CHOCA]** (migración), `trash.py` **[CHOCA]** |
| IN-072 | `deleted_by` es TEXT y `updated_by` es INTEGER para lo mismo | `main.py:344`, `:238`, `:307` | En `datos_archivo` conviven `updated_by INTEGER` (id) y `deleted_by TEXT` (nombre); en `empleados`, `updated_by` es TEXT. Tres representaciones del mismo concepto en columnas contiguas: ninguna consulta puede responder «quién ha tocado este documento». | Una sola representación, `INTEGER` con clave foránea, en todas. Migración con traducción del texto existente. | M | `main.py` **[CHOCA]** (migración), `admin/docs.py` **[CHOCA]**, `trash.py` **[CHOCA]** |
| IN-073 | Ningún `CHECK` de dominio: la validación vive sólo en Python | `schema.sql:96`, `:100`, `main.py:282`, `:528` | `status`, `soporte`, `disposicion`, `sexo`, `idioma`, `modulo` y `rol` son texto libre. La ruta valida; la importación valida a medias; el restore de copias **no valida nada**. Escenario: un restore introduce `status='Aprobado'` con mayúscula y esos documentos desaparecen de la búsqueda pública para siempre, porque el filtro compara con `'aprobado'`. | `CHECK` en la base para cada uno: es la única capa por la que pasan todos los caminos de escritura. | M | `main.py` **[CHOCA]** (migración), `schema.sql` |
| IN-074 | `descriptores_libres.nombre` es único distinguiendo mayúsculas | `schema.sql:134` vs `catalog.py:522` | La restricción es `UNIQUE(nombre)` y la comprobación previa usa `LOWER(nombre)`. Escenario: existe «Presupuesto»; `create_keyword` rechaza «presupuesto», pero `upsert_descriptors` (`helpers.py:78`) hace `ON CONFLICT (nombre)` sin `LOWER` y **sí lo crea**. Quedan dos palabras clave que son la misma y ninguna búsqueda las junta. | Índice único sobre `LOWER(nombre)` y una migración que fusione las existentes. | M | `main.py` **[CHOCA]** (migración), `catalog.py`, `admin/helpers.py` **[CHOCA]** |
| IN-075 | `tipo_documento.nombre` tiene el mismo problema | `schema.sql:24` vs `helpers.py:132` | `UNIQUE` sensible a mayúsculas y búsqueda con `LOWER`. Con dos tipos que sólo difieren en capitalización, `_resolve_or_create_tipo_documento` devuelve el primero que encuentre **sin orden determinista**: el mismo documento acaba en un tipo o en otro según el plan que elija PostgreSQL. | Índice único sobre `LOWER(nombre)`; y `ORDER BY id` mientras tanto. | S | `main.py` **[CHOCA]** (migración), `admin/helpers.py` **[CHOCA]** |
| IN-076 | El índice de borrado lógico indexa una columna siempre nula | `main.py:354-359` | `ON datos_archivo(deleted_at) WHERE deleted_at IS NULL`: la condición parcial garantiza que la única columna indexada vale `NULL` en todas las filas del índice. No ayuda a ningún `ORDER BY` ni a ningún filtro que acompañe al borrado lógico, que es siempre. | Índices parciales útiles: `(fecha_documento DESC) WHERE deleted_at IS NULL`, `(tesauro_primario) WHERE deleted_at IS NULL AND status='aprobado'`. Sustituyen a los tres actuales. | M | `main.py` **[CHOCA]** (migración) |
| IN-077 | No hay índice para el orden por defecto del buscador | `archive.py:150-154` | El orden por defecto es `titulo ASC` y las alternativas son `fecha_documento DESC/ASC`. No hay índice sobre ninguna. Cada página ordena el fondo entero. | Índices parciales sobre `(titulo)` y `(fecha_documento DESC NULLS LAST)`. | S | `main.py` **[CHOCA]** (migración) |
| IN-078 | No hay índice sobre `tesauro_primario`, que filtra y facetea | `archive.py:119`, `:253`, `docs.py:59` | Es la columna del filtro de tipología, de la faceta por tipo y del filtro del monitor. Sin índice, cada faceta es un recorrido completo con agrupación. | Índice parcial. Y a medio plazo, filtrar por `id_tipo_documento` (BA-004). | S | `main.py` **[CHOCA]** (migración) |
| IN-079 | El login recorre la tabla: `TRIM(usuario)` anula el índice único | `auth.py:76`, `:121`, `helpers.py:159`, `users.py:211` | `WHERE TRIM(usuario) = %s` es una expresión sobre la columna, así que el índice de `UNIQUE(usuario)` no se usa. Con pocos usuarios no se nota; lo que importa es que el `TRIM` delata que hay nombres con espacios guardados, y entonces la restricción `UNIQUE` no los distingue de los limpios: pueden coexistir «ana» y «ana ». | Limpiar los datos una vez, normalizar en el borde, consultar por igualdad, e índice único sobre `LOWER(usuario)` para que el login sea insensible a mayúsculas de forma consistente. | M | `main.py` **[CHOCA]** (migración), `auth.py` **[CHOCA]**, `admin/helpers.py` **[CHOCA]**, `admin/users.py` |
| IN-080 | `audit_log` no tiene ni un índice | `database.py:136-144`, `catalog.py:627-636` | Se crea sin índices y se consulta siempre con `ORDER BY timestamp DESC` más `COUNT(*)` completo más `unaccent(accion) ILIKE`. Es además la tabla que más crece. Al año, la pestaña Auditoría tarda segundos y `global_summary` (`stats.py:285-287`) añade dos recorridos completos en cada carga del panel de Sistema. | Índice `(timestamp DESC)`, índice trigram para la búsqueda, y paginación por cursor en vez de `OFFSET` (IN-121). | S | `database.py` **[CHOCA]**, `main.py` **[CHOCA]** (migración) |
| IN-081 | `audit_log` crece sin política de retención | `database.py:131-178` | Cada login, cada consulta por enlace externo y cada operación deja una fila, para siempre. No hay purga, archivado ni tope. En un sistema cuyo valor es la trazabilidad, el registro no puede ser también lo que lo frene. | Retención declarada (por ejemplo 24 meses en línea) con archivado a R2 antes de purgar, y particionado por mes si el volumen lo pide. Documentarla: es una decisión archivística. | M | `main.py` **[CHOCA]** (migración), `routes/backup.py` **[CHOCA]**, `README.md` |
| IN-082 | La auditoría no guarda el identificador del objeto afectado | `database.py:153-178` | `detalle` es texto libre (`f"ID: {doc_id}, Titulo: ..."`). Para saber todo lo que le ha pasado al documento 412 hay que hacer `ILIKE '%412%'`, que encuentra también el 1412 y el 4120. Un archivo institucional tiene que poder reconstruir la historia de una unidad documental. | Columnas `objeto_tipo`, `objeto_id` e `ip`, indexadas; `detalle` pasa a JSON estructurado. | M | `database.py` **[CHOCA]**, `main.py` **[CHOCA]** (migración), todo `routes/` |
| IN-083 | La auditoría es incompleta: media aplicación no registra nada | `archive.py`, `hr.py`, `lookups.py`, `stats.py`, `catalog.py:534`, `:547`, `backup.py:370` | Ninguna búsqueda queda registrada (BR-006 lo señala para RRHH; es general), ni la edición o el borrado de palabras clave, ni el listado de usuarios. `log_event` se llama donde alguien se acordó. | Registro automático por middleware para toda escritura, y decisión explícita y documentada sobre qué lecturas se registran: las de datos personales, sí. | M | `main.py` **[CHOCA]**, `database.py` **[CHOCA]** |
| IN-084 | La auditoría falla abierta | `database.py:173-174` | El `except` registra en el log del servidor y sigue. Si la tabla se llena o Neon rechaza la conexión, las operaciones se completan sin dejar rastro y el hueco no se descubre hasta que alguien mira la pestaña. | Para borrado, purga, cambio de permisos y export, el registro es parte de la transacción: si no se puede auditar, no se hace. | M | `database.py` **[CHOCA]**, `trash.py` **[CHOCA]**, `backup.py` **[CHOCA]**, `admin/users.py` |
| IN-085 | `schema.sql` es documentación falsa | `schema.sql:1-5`, `CLAUDE.md:82` | Se declara «esquema de referencia (NO modificar)» mientras `main.py` aplica más de sesenta `ALTER TABLE` que no están ahí: `deleted_at`, `status`, `disposicion`, las tablas de IA, `schema_version`. Quien lo lea para entender el modelo se lleva una versión antigua. `test_sql_columns.py` ya tiene que reconstruir el esquema real sumando fichero y migraciones. | Generar `schema.sql` desde la base (`pg_dump --schema-only`) en cada cambio, o marcarlo explícitamente como «estado inicial histórico» y publicar el vigente aparte. | M | `schema.sql`, `README.md`, `CLAUDE.md` |
| IN-086 | La huella global obliga a reejecutar las ochenta migraciones | `main.py:546-552` | La huella es el SHA-256 de la lista entera: cambiar una coma en la primera migración lanza las ochenta otra vez. Y no queda registro de cuáles se aplicaron, sólo el estado global. | Registro por migración —una fila por identificador aplicado, con marca de tiempo y duración— manteniendo la huella global como atajo de arranque rápido, que es la decisión ya tomada. | M | `main.py` **[CHOCA]**, `tests/test_migraciones.py` |
| IN-087 | Las migraciones no se pueden ejecutar fuera del arranque | `main.py:128` | No hay comando, ni endpoint, ni script: la única forma de aplicarlas es arrancar la aplicación, y en Vercel eso no ocurre (IN-001). Tampoco hay manera de probarlas contra una copia antes de desplegar. | Un `python -m app.migrate` que sirva para el arranque, para la integración continua y para una rama de Neon de pruebas. | M | `main.py` **[CHOCA]**, `app/migrations/` (nuevo) |
| IN-088 | Ninguna migración se ejecuta jamás en las pruebas | `tests/test_migraciones.py` | La prueba verifica la lógica de la huella con `db_query` mockeado. Una migración con error de sintaxis pasa toda la suite, se despliega, el bucle captura la excepción, la huella no se registra, y en cada arranque se reintenta y vuelve a fallar en silencio. Eso ya pasó con el `%%`, como documenta `CLAUDE.md`. | Una rama efímera de Neon en integración continua donde se apliquen todas las migraciones desde cero y también sobre el esquema anterior. | M | `.github/workflows/` (nuevo), `tests/test_migraciones.py` |
| IN-089 | Alembic descartado sin decisión escrita | `main.py:128-583` | El proyecto ha reinventado un motor de migraciones —idempotencia, huella, orden— y le faltan las piezas caras: reversión, aplicación parcial, generación desde el modelo y pruebas. La decisión de no usar Alembic puede ser la correcta a este tamaño, pero no está escrita en ninguna parte, así que cada persona nueva la vuelve a plantear. | Evaluar y **documentar** la decisión. Si se mantiene el motor propio, adoptar de Alembic lo barato: un fichero por migración con identificador y una tabla de aplicadas (IN-086). | M | `README.md`, `CLAUDE.md`, `main.py` **[CHOCA]** |
| IN-090 | Hay backfills de tabla completa en cada arranque | `main.py:36`, `:654-667`, `:670-689` | `_backfill_archivo_tipo_fk` y `_backfill_rrhh_tipo_fk` son `UPDATE` sin cota sobre las tablas de documentos. El segundo se llama **desde el `lifespan`, fuera de `run_migrations`**, así que ni siquiera lo protege la huella: en cada arranque en frío recorrería `datos_rrhh` entera. | Un backfill es una migración de datos: se ejecuta una vez, se registra y se hace por lotes. Nunca en cada arranque. | M | `main.py` **[CHOCA]** |
| IN-091 | `_migrate_archivo_tipos` es N+1 en el arranque | `main.py:626-651` | Un `SELECT DISTINCT` y después un `SELECT` y un `INSERT` **por cada tipo distinto**, cada uno con su viaje a Neon, y con `generate_unique_slug` dentro, que a su vez consulta en bucle. | Una sola sentencia `INSERT ... SELECT DISTINCT ... ON CONFLICT DO NOTHING`. | S | `main.py` **[CHOCA]** |
| IN-092 | `generate_unique_slug` es un bucle de consultas sin tope | `utils.py:261-282` | `while True` con un `SELECT` por intento. Con un texto muy repetido el bucle crece sin límite; y con dos procesos a la vez, ambos leen «libre» y el segundo `INSERT` falla por unicidad, sin reintento. | Calcular el siguiente sufijo en una sola consulta, o insertar con `ON CONFLICT` y reintentar; tope de intentos en cualquier caso. | S | `utils.py` **[CHOCA]** |
| IN-093 | `populate_missing_slugs` actualiza fila a fila en el arranque | `utils.py:289-313` | Un `UPDATE` con `commit` por cada tipo sin slug, más las consultas de `generate_unique_slug`. Con cincuenta tipos sin slug son más de cien viajes antes de la primera respuesta. | Una sentencia con `regexp_replace` y `unaccent` en SQL, o dejar de generar slugs en el arranque. | S | `utils.py` **[CHOCA]**, `main.py` **[CHOCA]** |
| IN-094 | La vista se define dos veces en el mismo fichero | `main.py:156-187` y `:362-393` | Dos migraciones con `CREATE OR REPLACE VIEW` casi idénticas; la segunda dice añadir `WHERE e.deleted_at IS NULL`, que **la primera ya tiene**. Treinta líneas duplicadas que hay que mantener sincronizadas a mano, y la vista se crea dos veces en cada arranque que no cuadre huella. | Una definición. Si la vista cambia, se sustituye la migración y la huella se ocupa del resto. | S | `main.py` **[CHOCA]** |
| IN-095 | La vista no está materializada: se recalcula en cada búsqueda | `main.py:362-393`, `hr.py:110` | Señalado como BR-049; la consecuencia de ingeniería es más amplia de lo que se ve desde la pantalla: la vista agrupa **toda** la plantilla con `STRING_AGG` y `COUNT` antes de aplicar ningún filtro, de modo que la paginación, el `COUNT(*) OVER()` y las facetas trabajan sobre el agregado completo. | Vista materializada con refresco al escribir, o columnas denormalizadas (`doc_count`, `tipos`) mantenidas por disparador. | L | `main.py` **[CHOCA]** (migración), `hr.py` **[CHOCA]** |
| IN-096 | Cuatro caminos con consultas sin `LIMIT` | `archive.py:52`, `hr.py:95`, `backup.py:351`, `catalog.py:504-511` | `fetch_archive_dataframe`, `fetch_hr_dataframe`, el `SELECT *` de cada tabla del backup y el listado de palabras clave con su recuento no tienen cota. Cualquiera puede agotar la memoria del lambda cuando el fondo crezca. | Cota explícita en toda consulta y paginación donde el resultado pueda crecer. | M | `archive.py` **[CHOCA]**, `hr.py` **[CHOCA]**, `backup.py` **[CHOCA]**, `catalog.py` |
| IN-097 | `get_keywords` agrega la tabla de vínculos entera sin paginar | `catalog.py:502-513` | `LEFT JOIN` de `descriptores_libres` con `archivo_descriptores` y `COUNT(DISTINCT)` por palabra, sin `LIMIT`. Con mil palabras clave y cien mil vínculos es un recorrido completo en cada apertura de la pestaña Tipos. | Paginar, y precalcular el uso con un contador mantenido o una vista materializada. | S | `catalog.py` |
| IN-098 | Ningún plan de ejecución revisado, ningún presupuesto por consulta | todo el proyecto | No hay rastro de que se haya mirado un `EXPLAIN`: lo demuestran el índice GIN que no casa con la consulta (BA-002), los índices parciales inútiles (IN-076) y los `unaccent()` en el `WHERE` que impiden usar índice (BR-051). | Un cuaderno de planes: para las diez consultas del camino caliente, `EXPLAIN (ANALYZE, BUFFERS)` guardado en `docs/`, y una prueba que falle si una consulta supera un umbral de coste. | M | `docs/rendimiento.md` (nuevo), `.github/workflows/` (nuevo) |
| IN-099 | Las estadísticas de consulta de Neon no se consultan nunca | operación | Ninguna referencia en la documentación a cómo se averigua qué consulta duele. Un sistema con seis pantallas y veinte consultas distintas no se optimiza a ojo. | Documentar cómo activar y leer `pg_stat_statements` en Neon, y revisarlo al cerrar cada trabajo de rendimiento. | S | `README.md` |
| IN-100 | Sin réplica de lectura ni separación lectura/escritura | `database.py:28-43` | Todo va a la conexión primaria, incluidas las búsquedas públicas y los recuentos del panel. Neon ofrece réplicas de lectura y el proyecto no las contempla. | Dejar preparado un parámetro `readonly` en `db_query` y un segundo pool contra la réplica, para activarlo cuando el volumen lo justifique. | M | `database.py` **[CHOCA]**, `core/config.py` **[CHOCA]** |
| IN-101 | El pool no usa el punto de conexión agrupado de Neon | `database.py:34`, `.env.example:4` | La cadena de ejemplo apunta al punto directo. En serverless con muchas instancias lo indicado es el agrupado, que multiplexa; sin él cada instancia consume una conexión real del cupo (IN-107). | Usar el punto agrupado para la aplicación y el directo sólo para migraciones, y documentar la diferencia. | S | `.env.example`, `README.md`, `database.py` **[CHOCA]** |
| IN-102 | El esquema no tiene ninguna restricción temporal | `schema.sql`, `main.py:311-321` | Nada impide una `fecha_documento` en el año 3000, una `fecha_nacimiento` posterior al ingreso, o un `historial_cargos` con `fecha_fin` anterior a `fecha_inicio` — que es justo el defecto que BR-067 describe desde la interfaz. | `CHECK` temporales en la base, incluida la exclusión de solapamientos en `historial_cargos` con `EXCLUDE USING gist`. | M | `main.py` **[CHOCA]** (migración) |
| IN-103 | Los enteros de negocio admiten valores absurdos | `schema.sql:98`, `:28` | `numero_paginas` y `plazo_retencion_anios` son `INTEGER` sin `CHECK`. La ruta valida el plazo entre 1 y 100 y la importación valida las páginas; el restore no valida nada. | `CHECK (numero_paginas > 0)` y `CHECK (plazo_retencion_anios BETWEEN 1 AND 100)`. | S | `main.py` **[CHOCA]** (migración) |
| IN-104 | No existe una columna que sea la fuente única del texto de búsqueda | `main.py:250-256`, `archive.py:181-188` | La expresión `to_tsvector(...)` se escribe a mano en el índice, en el `WHERE` y en el `ts_rank_cd`, con tres redacciones distintas. BA-002 señala la consecuencia; la causa es la ausencia de una fuente única. | Columna generada `tsv tsvector GENERATED ALWAYS AS (...) STORED`, índice GIN sobre ella y las tres consultas contra la columna. Elimina la clase entera de fallo. | M | `main.py` **[CHOCA]** (migración), `archive.py` **[CHOCA]**, `hr.py` **[CHOCA]** |

---

## D. Rendimiento, serverless y coste

| ID | Título | Archivo:línea | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|
| IN-105 | pandas domina el arranque en frío de cada lambda | `api/requirements.txt:3`, `archive.py:3`, `hr.py:4`, `stats.py:6`, `lookups.py:5` | pandas y numpy se importan en el arranque de la función serverless y son con diferencia las dependencias más pesadas del paquete. Ese coste se paga en **cada** arranque en frío, que en un sistema de uso intermitente como éste es la mayoría de las primeras peticiones del día. Todo para hacer `unique()`, `isin()` y `len()`. | Retirar pandas (IN-045). Es la mejora de latencia percibida más grande y barata del proyecto. | L | `archive.py` **[CHOCA]**, `hr.py` **[CHOCA]**, `stats.py` **[CHOCA]**, `lookups.py` **[CHOCA]**, `api/requirements.txt` |
| IN-106 | `boto3` se importa aunque no se vaya a subir nada | `storage.py:16-17`, `main.py:22` | `import boto3` está a nivel de módulo y `main.py` importa `routes.files` siempre. Es la segunda dependencia más pesada, y la mayoría de las peticiones (búsquedas, panel) no tocan R2. | Importación perezosa dentro de `_get_client()`. | S | `storage.py` **[CHOCA]** |
| IN-107 | `ThreadedConnectionPool(1,5)` dentro de un lambda es un pool por instancia | `database.py:31-42` | El pool no se comparte entre instancias: veinte lambdas activas son hasta cien conexiones contra Neon, más las que quedan *idle in transaction* por IN-004. El límite del proyecto se agota y las peticiones empiezan a recibir el 503 de `PoolError`. Además `minconn=1` no precalienta nada útil porque la instancia se destruye a los pocos minutos. | `maxconn` de 1 o 2 por instancia (una función serverless atiende una petición a la vez), punto de conexión agrupado de Neon (IN-101), y el número como variable de entorno ya prevista en `config.py:20-21` pero **no usada** por el pool. | M | `database.py` **[CHOCA]**, `core/config.py` **[CHOCA]** |
| IN-108 | `DB_POOL_MIN` y `DB_POOL_MAX` se declaran y se ignoran | `core/config.py:20-21` vs `database.py:32-33` | Las variables existen en `Settings` y el pool tiene los números escritos a mano. Escenario: alguien ajusta la variable en Vercel para contener el problema anterior, redespliega, y no cambia nada; la conclusión será que el pool no es el problema. | Leerlas donde se crea el pool, o borrarlas de la configuración. Una opción que no hace nada es peor que no tenerla. | S | `database.py` **[CHOCA]**, `core/config.py` **[CHOCA]** |
| IN-109 | La aplicación es síncrona de principio a fin | `routes/**` (`def`, no `async def`) | Con psycopg2 bloqueante y endpoints `def`, FastAPI ejecuta cada petición en el pool de hilos de Starlette. Es una decisión defendible, pero convive con endpoints `async def` que hacen trabajo bloqueante (`imports.py:311`, `backup.py:413`, `ai.py`): esos sí bloquean el bucle de eventos. Escenario: una importación CSV de tres minutos en un `async def` congela **todas** las peticiones de esa instancia. | Coherencia: o todo síncrono, o migrar a psycopg3 en modo asíncrono (IN-209). Mientras tanto, ningún `async def` puede hacer trabajo bloqueante sin `run_in_threadpool`. | M | `admin/imports.py` **[CHOCA]**, `backup.py` **[CHOCA]**, `routes/ai.py`, `files.py` **[CHOCA]** |
| IN-110 | La llamada al modelo de IA puede durar más que el propio lambda | `core/ai.py:178`, `vercel.json:8` | `urllib.request.urlopen(req, timeout=90)` con `maxDuration: 60`. El tiempo de espera nunca se alcanza: la plataforma corta antes, la petición muere sin respuesta y sin registrar el turno, y el usuario ve un error genérico. El coste de los tokens ya consumidos se pierde sin quedar contabilizado en el tope diario. | Tiempo de espera por debajo del presupuesto del lambda (45 s), con reserva para escribir el registro; y respuesta en flujo para que el usuario vea avance. | M | `core/ai.py`, `vercel.json` **[CHOCA]** |
| IN-111 | El catálogo de modelos se descarga con espera de 30 segundos | `core/ai.py:201-204` | Otra llamada bloqueante a una API externa dentro de la petición, sin caché declarada. Media hora de arranques en frío es media hora de descargas del mismo catálogo. | Cachear en la base con TTL largo; el catálogo de modelos cambia semanas, no minutos. | S | `core/ai.py` |
| IN-112 | Las respuestas no se comprimen | `main.py:70-121` | No hay `GZipMiddleware` ni configuración de compresión. La búsqueda devuelve el resumen completo de cada documento (BR-007 lo señala para RRHH), y el export de copia devuelve un JSON con la base entera **sin comprimir**. Desde Venezuela, con conexiones que no siempre son buenas, eso se nota. | `GZipMiddleware(minimum_size=1000)`, y compresión explícita del JSON de copia antes de subirlo a R2 (reduce también el coste de almacenamiento). | S | `main.py` **[CHOCA]**, `backup.py` **[CHOCA]** |
| IN-113 | Toda respuesta de `/api/` se marca como no cacheable | `main.py:85-86` | `no-store, no-cache, must-revalidate` para **todo** `/api/`, incluidos `/api/choices` (que ya tiene su propia caché de cinco minutos) y las facetas. El navegador vuelve a pedir el catálogo completo en cada carga de página. | Distinguir: datos personales y de sesión, `no-store`; catálogos y listas públicas, `private, max-age` con `ETag`. | S | `main.py` **[CHOCA]** |
| IN-114 | Los estáticos se declaran cacheables en dos sitios que se contradicen | `main.py:83-84` vs `vercel.json:75-83` | `vercel.json` da a JS y CSS `max-age=3600`; el middleware de `main.py` da `no-cache, must-revalidate` a todo `/static/` que termine en `.js`, `.css` o `.html`. Cuál gana depende de si la petición la sirve el CDN estático o pasa por la función: dos comportamientos para el mismo fichero, imposible de razonar. | Una sola política, en `vercel.json`, con nombres versionados (`app.js?v=<hash>`) para poder cachear de verdad. | M | `main.py` **[CHOCA]**, `vercel.json` **[CHOCA]**, `app/static/*.html` |
| IN-115 | No hay versionado de recursos: cada despliegue arrastra caché vieja | `app/static/*.html` | Los `<script src="/static/app.js">` no llevan huella. Con la caché de una hora de `vercel.json`, tras un despliegue conviven durante una hora un HTML nuevo y un JS viejo en el navegador de cada usuario. Ese es exactamente el tipo de fallo que sólo aparece en producción y no se reproduce. | Huella de contenido en el nombre o en la consulta, generada en el paso de compilación (IN-203). | M | `app/static/*.html`, paso de compilación (nuevo) |
| IN-116 | `/api/health` hace seis recorridos completos por sonda | `main.py:719-728` | Seis `COUNT(*)` sin filtro sobre las tablas grandes. Una sonda de monitoreo cada minuto son ocho mil seiscientos recorridos al día de las tablas principales, en la conexión primaria. | Sonda barata: `SELECT 1`. Los recuentos, en un endpoint aparte con sesión (IN-037) y con `reltuples` en vez de `COUNT(*)` si basta la aproximación. | S | `main.py` **[CHOCA]** |
| IN-117 | `presigned_get_url` hace dos viajes a R2 por cada visualización | `storage.py:79-95` | `head_object` antes de firmar, para poder distinguir el 404. Cada apertura de un PDF son dos llamadas a Cloudflare, y la firma en sí no requiere ninguna. | Firmar directamente y dejar que el 404 lo dé R2 al seguir el redireccionamiento; o comprobar la existencia sólo cuando el registro diga que hay fichero y la clave no case con el patrón esperado. | S | `storage.py` **[CHOCA]**, `files.py` **[CHOCA]** |
| IN-118 | La descarga se resuelve con un redireccionamiento y sin flujo | `files.py:247`, `share.py:350` | `RedirectResponse` a la URL prefirmada. El fichero nunca pasa por la aplicación, lo que es bueno para el coste, pero significa que no hay `Content-Disposition` (el nombre original se pierde y el navegador muestra la clave con el UUID), no se puede registrar la descarga completada, y la URL firmada queda en el historial del navegador durante una hora. | Mantener el redireccionamiento por coste, pero firmar con `ResponseContentDisposition` para recuperar el nombre original, y acortar la caducidad a minutos. | S | `storage.py` **[CHOCA]**, `files.py` **[CHOCA]** |
| IN-119 | La subida lee el fichero entero en memoria antes de mirar el tamaño | `files.py:207-209` | `contents = await file.read()` y **después** se compara con `MAX_FILE_SIZE`. Un fichero de 500 MB se carga entero en la memoria del lambda antes de rechazarlo con un 413. Es una denegación de servicio de una sola petición. | Leer por trozos acumulando el tamaño y abortar al superar el límite; o mejor, subida directa del navegador a R2 con URL prefirmada de escritura, que quita el fichero del camino del lambda por completo. | M | `files.py` **[CHOCA]**, `storage.py` **[CHOCA]**, `app/static/admin-submit.js` |
| IN-120 | El export de copia serializa la base entera en memoria | `backup.py:389-409`, `:540-541` | `_construir_backup` acumula todas las tablas en un diccionario y `json.dumps(..., indent=2)` produce una cadena completa antes de empezar a enviar. Con la base crecida, el lambda se queda sin memoria; y el `indent=2` infla el resultado en torno a un tercio, que se paga en memoria, en transferencia y en almacenamiento de R2. | Serialización en flujo tabla a tabla con `StreamingResponse`, sin sangrado, y comprimido. | M | `backup.py` **[CHOCA]** |
| IN-121 | Paginación por `OFFSET` en las siete listas del sistema | `utils.py:8-11` y todos sus llamantes | `OFFSET` obliga a PostgreSQL a recorrer y descartar las filas anteriores: la página 200 del monitor cuesta doscientas veces la primera. Con `audit_log` sin índice (IN-080) el efecto se multiplica. | Paginación por cursor (clave estable + `WHERE id < ?`) al menos en auditoría y monitor. `paginate()` puede seguir existiendo para el resto. | M | `utils.py` **[CHOCA]**, `catalog.py`, `admin/docs.py` **[CHOCA]**, `trash.py` **[CHOCA]** |
| IN-122 | El tope de página es distinto en cada endpoint | `utils.py:8` (100), `archive.py:85` (50), `hr.py:123` (50), `catalog.py:609` (100) | Cuatro topes: 20, 25, 50 y 100 según el endpoint, sin criterio. BR-053 lo señala como incoherencia de producto; desde ingeniería, el problema es que el tope de coste de una consulta se decide en el sitio equivocado y de forma inconsistente. | Un único tope por defecto en `paginate()`, con excepciones justificadas y escritas. | S | `utils.py` **[CHOCA]** |
| IN-123 | Cada búsqueda de Archivo son tres consultas y dos de ellas repiten el filtro | `archive.py:203`, `:252`, `:259` | La consulta principal más dos de facetas, cada una reconstruyendo condiciones parecidas pero no iguales (BA-015 documenta que además difieren en los filtros que aplican). Tres recorridos de la misma tabla por búsqueda, sin índice que los sostenga (IN-077, IN-078). | Una sola consulta con `GROUPING SETS` o un CTE compartido; o facetas calculadas en una segunda petición sólo cuando el panel esté abierto. | M | `archive.py` **[CHOCA]** |
| IN-124 | El coste de egreso de R2 no tiene tope ni medida | `files.py:227-247`, `storage.py` | Cada visualización de un PDF es una descarga desde R2 sin límite de tasa (IN-152), sin caché de CDN por delante y sin ninguna métrica de cuánto se está transfiriendo. Un bucle de descarga de un usuario autenticado —o de un enlace externo compartido— se traduce directamente en factura. | Cloudflare CDN por delante del bucket para los ficheros públicos por enlace, límite de tasa por usuario, y un panel con la transferencia mensual. | M | `files.py` **[CHOCA]**, `share.py`, `README.md` |
| IN-125 | R2 no tiene reglas de ciclo de vida ni rotación de copias | `backup.py:542`, `storage.py` | El backup diario deja un objeto nuevo cada día bajo `backups/AAAA/MM/DD-HHMMSS-completo.json` y **nada los borra nunca**. A un año son 365 copias completas de la base pagándose indefinidamente, y ninguna de ellas se ha verificado (IN-176). | Regla de ciclo de vida: diarias 30 días, semanales 3 meses, mensuales 2 años. Documentar la política de retención de copias. | S | `README.md`, configuración de R2 |
| IN-126 | La subida y la descarga no comparten cliente ni configuración de reintento | `storage.py:32-45` | El cliente de boto3 se crea sin `retries` configurado ni `connect_timeout`/`read_timeout` explícitos, así que hereda los valores por defecto —que incluyen reintentos que pueden llevar la operación más allá del presupuesto del lambda. Y `_client_lock` está declarado (`storage.py:22`) y no se usa: la creación del cliente no está protegida frente a concurrencia. | `Config(retries={'max_attempts': 2, 'mode': 'standard'}, connect_timeout=3, read_timeout=10)`, y quitar la variable de bloqueo muerta o usarla. | S | `storage.py` **[CHOCA]** |
| IN-127 | No hay ninguna medida de latencia por consulta | `database.py:46-124` | El middleware mide la petición completa (`main.py:114-121`) pero nada mide cuánto tarda cada consulta ni cuántas se hacen por petición. Con Neon en otro continente, el número de viajes es la variable que manda, y hoy nadie la conoce. | Contador de consultas y tiempo acumulado por petición, expuesto en el log estructurado (IN-181) y en una cabecera `Server-Timing` en desarrollo. | M | `database.py` **[CHOCA]**, `main.py` **[CHOCA]** |
| IN-128 | Ningún presupuesto de rendimiento declarado | `README.md` | No hay ningún número escrito: cuánto debe tardar una búsqueda, cuántos documentos debe soportar el sistema, cuántos usuarios simultáneos. Sin eso, ninguna de las mejoras anteriores se puede priorizar con criterio ni verificar. | Declarar objetivos (p. ej. búsqueda p95 < 800 ms con 50.000 documentos) y una prueba de carga mínima que los compruebe contra una rama de Neon con datos sintéticos. | M | `README.md`, `docs/rendimiento.md` (nuevo) |

---

## E. Seguridad y autorización

| ID | Título | Archivo:línea | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|
| IN-129 | Cualquier usuario con sesión descarga la base completa, con los hashes | `backup.py:293`, `:370-409`, `:318` | El router sólo exige `require_session` pese a documentarse como «Solo accesible para el administrador máximo (Global)». `EXPORTABLE_TABLES` incluye `usuarios_sistema`, y el export devuelve todas sus columnas: usuario, módulo, rol y **`contrasena`**, el hash bcrypt. Escenario: un usuario Normal del módulo Archivo llama a `/api/admin/backup/export` y obtiene el censo completo de personal, todos los expedientes y los hashes de todos los administradores para atacarlos sin límite fuera de línea. | Rol de administrador global comprobado contra la base; exclusión de la columna de contraseña del export; y auditoría obligatoria de cada export (IN-084). | M | `backup.py` **[CHOCA]**, `routes/admin/deps.py` **[CHOCA]** |
| IN-130 | Cualquier usuario con sesión puede sobrescribir la base | `backup.py:412-485` | `/restore` con `mode=overwrite` tiene la misma protección: sesión y nada más. Además el fichero lo aporta el atacante: puede contener una fila de `usuarios_sistema` con un hash bcrypt propio y rol `Global`, insertada en modo `merge` sin borrar nada. Escenario: escalada a administrador global en una sola petición, con la aplicación funcionando con normalidad. | Rol global, confirmación explícita, copia automática previa, y **nunca** aceptar filas de `usuarios_sistema` desde un fichero subido. | M | `backup.py` **[CHOCA]** |
| IN-131 | `require_session` sólo comprueba que hay cookie: no hay autorización | `routes/admin/deps.py:7-24` | Devuelve el nombre de usuario y **ningún endpoint del sistema comprueba rol ni módulo con él**. BR-002 lo describe para la lectura de RRHH; aquí queda constatado que es la regla general del proyecto: no existe una sola dependencia de autorización en todo el código. El rol `Admin`/`Normal` y el módulo se guardan en `usuarios_sistema`, se envían al navegador en el login, y el servidor no los vuelve a mirar jamás. La autorización es puramente decorativa: la hace `configureSidebarVisibilities()` en el navegador. | Dependencias `require_rol("Admin")` y `require_modulo(...)` que resuelvan el perfil desde la base, aplicadas endpoint por endpoint. Es el pendiente estructural de mayor impacto del sistema. | L | `routes/admin/deps.py` **[CHOCA]**, todo `routes/` |
| IN-132 | Cualquier usuario con sesión gestiona todas las cuentas | `admin/users.py:176-266` | Consecuencia directa de lo anterior, y la más grave: `POST /users/create` permite crear un usuario con `modulo='Global'` y `rol='Admin'`; `PUT /users/{uid}/password` cambia la contraseña de cualquiera **sin pedir la actual**; `DELETE /users/{uid}` borra cuentas. Escenario: un usuario Normal se crea un administrador global, o directamente cambia la contraseña del administrador existente y entra con ella. | Rol global obligatorio; el cambio de contraseña propia exige la contraseña actual; el de otros, rol global y registro en auditoría; y ningún usuario puede borrar ni desactivar su propia cuenta ni el último administrador. | M | `admin/users.py`, `routes/admin/deps.py` **[CHOCA]**, `models.py` **[CHOCA]** |
| IN-133 | El cliente declara quién es en 22 endpoints | `docs.py:436`, `:459`, `trash.py:76`, `:98`, `backup.py:372`, `users.py:245`, `:259`, `share.py:303`, `files.py:183`, `imports.py:313` | `usuario`, `requester` y `creator` viajan en la consulta o el cuerpo y el servidor los cree, cuando `require_session` ya devuelve el nombre verificado. BR-064 lo señala en dos endpoints de RRHH; el recuento aquí es de todo el proyecto. Escenario: cualquiera firma sus borrados con el nombre de otra persona, y la auditoría lo registra como cierto. Es lo que convierte el registro de auditoría en inservible como prueba. | El usuario sale siempre de la dependencia de sesión; los parámetros `usuario`/`requester`/`creator` se retiran de la API. | M | todo `routes/`, `models.py` **[CHOCA]** |
| IN-134 | La búsqueda de Archivo no exige sesión | `archive.py:9`, `:74` | El router no lleva dependencia alguna. BR-001 señala lo mismo para RRHH; se recoge aquí porque la decisión de fondo —qué es público en este sistema— no está tomada en ninguna parte: `hr.py` protege unos endpoints y no otros (`hr.py:110` sin `_auth`, `hr.py:387` con), `archive.py` ninguno, `lookups.py` ninguno. La incoherencia es el hallazgo. | Decidir y **escribir** el modelo de acceso: qué se puede consultar sin sesión y qué no. Después, aplicarlo por defecto (router protegido salvo excepción explícita), nunca al revés. | M | `archive.py` **[CHOCA]**, `hr.py` **[CHOCA]**, `lookups.py` **[CHOCA]**, `main.py` **[CHOCA]**, `README.md` |
| IN-135 | Las excepciones a la sesión no están inventariadas | `main.py:696-708` | Doce routers, cada uno con su propia decisión sobre autenticación tomada en su propia línea. No hay ningún sitio donde se pueda leer de un vistazo qué endpoints son públicos. Escenario: se añade un router nuevo sin dependencia y nadie lo nota hasta que alguien lo encuentra. | Dependencia global en la aplicación y lista explícita de rutas públicas; y una prueba que enumere todas las rutas registradas y falle si alguna no está clasificada. | M | `main.py` **[CHOCA]**, `tests/test_seguridad.py` (nuevo) |
| IN-136 | `SECRET_KEY` tiene un valor por defecto conocido | `core/config.py:9` | `os.environ.get("SECRET_KEY", "ciencias-ucv-dev-key-change-in-prod")`. Si la variable falta en Vercel, la aplicación arranca con normalidad y firma con una clave que está escrita en el repositorio público. Escenario: cualquiera genera un token de sesión válido para el usuario que quiera (`generate_session_token` es determinista) y un enlace de compartición para cualquier documento. Nada avisa: `CLAUDE.md` la describe como «opcional pero recomendado en prod». | Sin `SECRET_KEY`, la aplicación no arranca en producción. Es exactamente el caso donde hay que fallar cerrado. | S | `core/config.py` **[CHOCA]**, `main.py` **[CHOCA]**, `.env.example`, `CLAUDE.md` |
| IN-137 | El login no tiene límite de intentos | `auth.py:70-102` | Nada limita la frecuencia. bcrypt frena el ataque a unos pocos intentos por segundo y por conexión, pero con `maxconn=5` y varias instancias, un atacante puede probar miles de contraseñas por minuto **y de paso agotar el pool** (IN-107), tumbando el sistema para todos. `audit_log` registra cada fallo, así que el ataque también llena la tabla (IN-081). | Límite por usuario y por IP con bloqueo temporal creciente, respaldado en la base para que funcione entre instancias; y un aviso al administrador ante un pico de fallos. | M | `auth.py` **[CHOCA]**, `main.py` **[CHOCA]** (migración) |
| IN-138 | Ningún endpoint tiene límite de tasa | `main.py`, `routes/**` | No sólo el login: el autocompletado con `ILIKE '%q%'` sobre dos tablas (BA-027), el export de copia, la generación de enlaces de compartición y las descargas de R2 (IN-124) son todos gratuitos e ilimitados para quien tenga sesión, y algunos para quien no la tenga. | Límite de tasa general en el borde (Vercel) más límites específicos por endpoint caro. | M | `vercel.json` **[CHOCA]**, `main.py` **[CHOCA]** |
| IN-139 | Los usuarios se enumeran por diferencia de tiempo en el login | `auth.py:80-83` | Con un usuario inexistente, la comprobación bcrypt no llega a ejecutarse y la respuesta vuelve en milisegundos; con uno existente, bcrypt tarda cientos. La diferencia es medible sin ninguna herramienta especial. Combinado con `/api/health`, que dice cuántas cuentas hay (IN-037), orienta el ataque. | Ejecutar siempre una verificación bcrypt contra un hash señuelo cuando el usuario no exista. | S | `auth.py` **[CHOCA]** |
| IN-140 | El login itera sobre varias filas del mismo usuario | `auth.py:72-83` | `fetch="all"` sobre `TRIM(usuario) = %s` y un bucle que prueba la contraseña contra **cada fila**. Que exista más de una fila por usuario ya es un defecto (IN-079), pero la consecuencia de seguridad es concreta: basta que **una** de las filas tenga la contraseña correcta para entrar, y los módulos y roles se acumulan de todas (`_build_user_response`). Una cuenta antigua desactivada se filtra bien (línea 81), pero dos filas activas con contraseñas distintas dan dos llaves para la misma puerta. | Una fila por usuario garantizada por la base; los módulos, en su propia tabla de asignación. | M | `main.py` **[CHOCA]** (migración), `auth.py` **[CHOCA]** |
| IN-141 | El borrado de usuario es físico y rompe la trazabilidad | `admin/users.py:258-266` | `DELETE FROM usuarios_sistema`. Todos los `creado_por`/`updated_by` que apuntaban a esa cuenta quedan colgando (IN-068), y con ellos la posibilidad de saber quién catalogó cada documento. En un archivo institucional, la identidad de quien describió un documento es parte del documento. | Borrado lógico, como ya se hace con documentos y empleados. Un usuario nunca se elimina: se desactiva. | S | `admin/users.py`, `main.py` **[CHOCA]** (migración) |
| IN-142 | La contraseña mínima son seis caracteres y no hay más requisitos | `models.py:169`, `:174-179`, `:272` | Seis caracteres, sin comprobación contra listas de contraseñas comunes, sin caducidad y sin obligación de cambio en el primer acceso. Para las cuentas que pueden exportar la base entera (IN-129), es insuficiente. | Mínimo de doce, comprobación contra una lista de las más usadas, cambio obligatorio en el primer acceso, y segundo factor para el rol global. | M | `models.py` **[CHOCA]**, `admin/users.py`, `auth.py` **[CHOCA]** |
| IN-143 | El identificador de sesión es el nombre de usuario firmado | `core/security.py:26-34` | El token es `base64(usuario:marca:firma)`. No hay identificador de sesión, así que no se puede revocar una sesión concreta, ni listar las sesiones activas, ni distinguir dos dispositivos del mismo usuario. Con IN-039 (no se consulta la base) el resultado es que **ninguna sesión se puede cortar nunca** salvo rotando `SECRET_KEY`, que corta también todos los enlaces compartidos (IN-146). | Identificador de sesión aleatorio en el token, con una tabla de sesiones activas que permita revocar de forma selectiva; o al menos una versión de credencial por usuario. | M | `core/security.py` **[CHOCA]**, `routes/admin/deps.py` **[CHOCA]**, `main.py` **[CHOCA]** (migración) |
| IN-144 | El formato del token se rompe con un nombre de usuario con dos puntos | `core/security.py:29`, `:35` | Se firma `f"{username}:{ts}"` y se verifica con `raw.rsplit(":", 2)`. Un usuario llamado `a:b` produce un token cuyo `rsplit` devuelve `("a", "b", ts)` en las variables equivocadas... y la firma se recalcula sobre esa reconstrucción, así que **coincide**: se acepta la sesión para el usuario «a». Nada valida hoy el nombre al crear la cuenta. | Serializar con un formato que no dependa de un separador (JSON firmado), y validar el nombre de usuario contra un patrón restrictivo al crearlo. | S | `core/security.py` **[CHOCA]**, `models.py` **[CHOCA]** |
| IN-145 | El token no lleva marca de propósito: sesión y compartición usan la misma clave | `core/security.py:26-34`, `:69-79` | Ambos son HMAC-SHA256 con `settings.secret_key` sobre cadenas separadas por dos puntos. Hoy los formatos no colisionan por el número de campos, pero nada lo garantiza y el cambio de uno afecta al otro. | Claves derivadas distintas por propósito (`HKDF` con etiqueta), o un campo de tipo dentro del contenido firmado. | S | `core/security.py` **[CHOCA]** |
| IN-146 | Los enlaces compartidos no se pueden listar ni revocar | `share.py:298-314`, `core/security.py:69-79` | El diseño sin tabla es una decisión tomada y documentada, y tiene sentido; pero su consecuencia no está resuelta: nadie puede saber cuántos enlaces vivos hay, para qué documentos, ni cortar uno concreto. El único mecanismo es rotar `SECRET_KEY`, que además cierra todas las sesiones. Para un archivo que debe rastrear lo que sale hacia fuera, «no se puede saber» es un problema. | Mantener el token autocontenido y añadir **sólo** una tabla de revocación (identificador del enlace y motivo), consultada en la validación. Sigue sin haber estado que limpiar salvo lo revocado. | M | `share.py`, `core/security.py` **[CHOCA]**, `main.py` **[CHOCA]** (migración) |
| IN-147 | Un enlace compartido sigue vivo aunque cambien los permisos del documento | `share.py:317-334` | El token se valida contra su firma y su caducidad y nada más: no comprueba el estado del documento más allá de la papelera. Un documento que pasa a `rechazado` o `draft` —es decir, que se retira de la vista pública— sigue siendo visible por el enlace externo hasta 30 días. | Comprobar también `status='aprobado'` y la disposición documental en `_leer_documento`. | S | `share.py` |
| IN-148 | No se valida el contenido real del fichero subido | `files.py:198-219` | Se comprueba la extensión y el tamaño; el `content_type` que se guarda en R2 **lo declara el cliente** (`file.content_type`). Escenario: se sube `informe.pdf` que en realidad es HTML con JavaScript, declarando `text/html`; la URL prefirmada lo sirve desde el dominio de R2 con ese tipo, y el navegador lo ejecuta. Es XSS almacenado en un dominio distinto, lo que limita el daño a la sesión, pero permite alojar páginas de suplantación bajo la infraestructura de la Facultad. | Comprobar los primeros bytes contra la extensión declarada, y **fijar** el `Content-Type` en el servidor a partir de la extensión validada, nunca aceptar el del cliente. | M | `files.py` **[CHOCA]**, `storage.py` **[CHOCA]** |
| IN-149 | La comprobación de recorrido de ruta se hace después de firmar la clave | `files.py:239` | `if not key or ".." in key or key.startswith("/")` está en `serve_file` y protege bien esa ruta, pero la misma comprobación **no existe** en `add_version` (`trash.py:203`, que sólo valida el esquema de la URL) ni en el `file_url` de los modelos (`models.py:13-17`, que acepta cualquier ruta que empiece por `/`). Escenario: se guarda `file_url = "/api/files/../../otra-cosa"` en un documento y el visor lo sigue. | Una función única `validar_clave_objeto()` usada en todos los puntos donde una clave o una URL interna entra en el sistema. | S | `files.py` **[CHOCA]**, `trash.py` **[CHOCA]**, `models.py` **[CHOCA]**, `storage.py` **[CHOCA]** |
| IN-150 | El acceso a un fichero no comprueba a qué documento pertenece | `files.py:227-247` | BA-020 señala que `?u=` no protege nada; el defecto de fondo es anterior: aunque la sesión fuese correcta, la ruta autoriza **por conocer la clave**, no por poder ver el documento que la contiene. Escenario: un usuario del módulo Archivo, con sesión legítima, abre la fotografía o la partida de nacimiento de un empleado guardadas en `rrhh/2025/…` con sólo tener la clave, que viaja en cualquier respuesta de la API de RRHH. | Resolver la clave hasta el documento que la referencia y aplicar la autorización de ese documento; una clave que no pertenezca a ningún documento visible, 404. | M | `files.py` **[CHOCA]**, `routes/admin/deps.py` **[CHOCA]** |
| IN-151 | Las claves de objeto son adivinables por estructura | `storage.py:47-52` | `<modulo>/<año>/<uuid8>-<nombre-sano>`. Ocho caracteres hexadecimales son 4.300 millones de combinaciones, suficiente contra la fuerza bruta, pero el nombre original va en claro: `rrhh/2025/a1b2c3d4-partida-nacimiento-maria-perez.pdf`. Esa clave aparece en el `file_url` de cualquier respuesta y en los registros de acceso. | UUID completo y sin el nombre original en la clave; el nombre para descarga se recupera del registro de la base (IN-118). | S | `storage.py` **[CHOCA]** |
| IN-152 | Ningún control sobre cuántos ficheros descarga un usuario | `files.py:227` | Sin límite de tasa ni registro de descarga completada. Un usuario legítimo puede vaciar el fondo digitalizado en una tarde y la auditoría sólo tendrá el `Upload File` de cuando entraron. La exfiltración masiva por parte de alguien con acceso es el riesgo más realista de un archivo institucional. | Registro de cada acceso a fichero con identificador de documento, y alerta ante volumen anómalo por usuario. | M | `files.py` **[CHOCA]**, `database.py` **[CHOCA]** |
| IN-153 | Sin `Content-Security-Policy` ni `HSTS` en las páginas | `vercel.json:49-74` | Las cabeceras de seguridad se aplican **sólo a `/api/`**, que es donde menos falta hacen: el HTML se sirve sin `Content-Security-Policy` (BR-056), sin `Strict-Transport-Security` y sin `X-Frame-Options`. Escenario: un XSS almacenado como los que describen BR-003 e IN-065 se ejecuta sin ninguna barrera; y la aplicación se puede embeber en un iframe de otro sitio. | Cabeceras de seguridad para `/(.*)`, con `Content-Security-Policy` acotada a los CDN que se usan de verdad y `Strict-Transport-Security` con `preload`. | M | `vercel.json` **[CHOCA]**, `app/static/*.html` |
| IN-154 | `X-XSS-Protection` está puesto y es contraproducente | `vercel.json:66-69` | La cabecera está obsoleta; los navegadores modernos la ignoran y en los antiguos su filtro introdujo vulnerabilidades propias. Su presencia da la impresión de que la protección contra XSS está cubierta, cuando lo que falta es `Content-Security-Policy` (IN-153). | Retirarla y poner `Content-Security-Policy` en su lugar. | S | `vercel.json` **[CHOCA]** |
| IN-155 | No hay defensa contra CSRF más allá de `SameSite=Lax` | `auth.py:58-67`, `deps.py:16` | `SameSite=Lax` bloquea el envío de la cookie en peticiones POST entre sitios, lo que cubre el caso común. Pero `require_session` acepta también `X-Session-Token` como alternativa, y `CORS` está con comodín (IN-034): el modelo de amenaza no está escrito y las tres piezas se contradicen. | Escribir el modelo: cookie `SameSite=Strict` para el navegador, cabecera sólo para integraciones declaradas, `CORS` acotado, y token anti-CSRF si se mantiene la cabecera. | M | `auth.py` **[CHOCA]**, `routes/admin/deps.py` **[CHOCA]**, `main.py` **[CHOCA]** |
| IN-156 | La cookie de sesión sólo es segura si `ENVIRONMENT` vale `production` | `auth.py:66`, `core/config.py:14` | `secure=settings.environment == "production"`. El valor por defecto es `"production"`, así que hoy funciona; pero basta que alguien defina `ENVIRONMENT=prod` o `Production` en Vercel para que la cookie viaje sin `Secure`. Un fallo de una sola letra que no da ningún síntoma. | Deducirlo del esquema de la petición, o invertir la condición: sin `Secure` sólo si `ENVIRONMENT` vale exactamente `development`. | S | `auth.py` **[CHOCA]**, `core/config.py` **[CHOCA]** |
| IN-157 | El backup programado se autentica comparando cadenas sin tiempo constante | `backup.py:534` | `if authorization != f"Bearer {esperado}"`. La comparación de cadenas de Python cortocircuita en el primer byte distinto. Es un vector estrecho por la latencia de red, pero el proyecto ya usa `hmac.compare_digest` en `core/security.py`: la incoherencia es lo llamativo. Además `x_vercel_cron` se declara como parámetro y **no se usa**. | `hmac.compare_digest`, y usar o retirar la cabecera de Vercel. | S | `backup.py` **[CHOCA]** |
| IN-158 | No hay política de protección de datos personales | `schema.sql:63-82`, `routes/hr.py` | El sistema guarda cédula, RIF, fecha de nacimiento, sexo, nivel educativo, fotografía y expediente laboral completo de cada trabajador, y no hay ninguna declaración de finalidad, plazo de conservación, base legal ni procedimiento de acceso o rectificación. BR-009 lo señala para el personal egresado; aquí falta el marco entero. | Documento de tratamiento de datos personales: qué se guarda, por qué, cuánto tiempo, quién accede y cómo se ejerce el derecho de acceso. Es requisito legal, no una mejora. | M | `docs/datos-personales.md` (nuevo), `README.md` |
| IN-159 | Los datos personales no se cifran ni se enmascaran en ningún sitio | `schema.sql:63-82`, `backup.py` | Cédula y RIF viajan en claro en cada respuesta de búsqueda, se guardan en claro en la base y se copian en claro en el JSON de R2 (que además es accesible con las credenciales de IN-002). | Cifrado en reposo para las columnas más sensibles, o al menos cifrado del fichero de copia antes de subirlo, con la clave fuera de R2. | L | `main.py` **[CHOCA]** (migración), `backup.py` **[CHOCA]**, `storage.py` **[CHOCA]** |
| IN-160 | Las copias en R2 no tienen control de acceso propio | `backup.py:542`, `storage.py` | Las copias completas de la base se guardan en el **mismo bucket** que los documentos digitalizados, bajo el prefijo `backups/`. Cualquier credencial que sirva para servir un PDF sirve para descargar la base entera; y `serve_file` acepta cualquier clave (IN-150), incluida `backups/2026/…`. Escenario: un usuario con sesión pide `/api/files/backups/2026/09/02-071000-completo.json` y se lleva todo. | Bucket separado para copias, con credenciales distintas de sólo escritura para la aplicación; y prohibición explícita del prefijo `backups/` en `serve_file`. | M | `storage.py` **[CHOCA]**, `files.py` **[CHOCA]**, `backup.py` **[CHOCA]** |
| IN-161 | Ningún endpoint del asistente distingue lo que puede ver el usuario | `routes/ai.py:49-76`, `core/ai_tools.py` | El contexto se construye desde la sesión, lo cual está bien pensado, pero las herramientas que el modelo invoca consultan el archivo a través de las mismas funciones sin autorización que el resto del sistema (IN-131). Escenario: un usuario de Archivo pregunta por una persona y el asistente le lee su expediente de RRHH, porque la herramienta no comprueba módulo. | Las herramientas del asistente reciben el contexto de autorización y consultan a través de la misma capa de servicios con permisos que todo lo demás. | M | `core/ai_tools.py`, `routes/ai.py`, `services/` (nuevo) |
| IN-162 | El tope de gasto diario se comprueba antes y no después | `routes/ai.py:90-101`, `core/ai.py:110-120` | El gasto se suma sobre `ia_mensajes`, y el mensaje se registra **después** de la respuesta. Con varias peticiones a la vez, todas leen el mismo total y todas pasan el tope. Escenario: diez preguntas simultáneas con el tope al 95% lo superan las diez. Y si la petición muere por tiempo (IN-110), el gasto no se registra nunca. | Reservar el coste estimado antes de llamar y ajustarlo después, con una comprobación atómica en la base. | M | `routes/ai.py`, `core/ai.py` |
| IN-163 | Sin cabecera de permisos ni aislamiento para el contenido de R2 | `files.py:247`, `vercel.json` | Los ficheros se sirven desde el dominio de Cloudflare, fuera del control de las cabeceras del proyecto: ni `Content-Disposition`, ni `X-Content-Type-Options`, ni `Content-Security-Policy` se aplican ahí. Es lo que hace explotable IN-148. | Servir los ficheros a través de un dominio propio con las cabeceras controladas, o forzar `Content-Disposition: attachment` en la firma para todo lo que no sea PDF o imagen validada. | M | `storage.py` **[CHOCA]**, `files.py` **[CHOCA]** |

---

## F. Fiabilidad, transacciones, recuperación y operación

| ID | Título | Archivo:línea | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|
| IN-164 | No existe la transacción de varios pasos | `database.py:46-124` | `db_query(commit=True)` confirma su propia sentencia y devuelve la conexión al pool: no hay forma de agrupar dos escrituras. Toda operación compuesta del sistema puede quedar a medias: alta de empleado más documento (IN-013), purga (IN-016, IN-017), versionado (IN-020), restauración (IN-022), palabras clave (`helpers.py:77-86`), importación (IN-024). Es la causa raíz de siete pendientes de la sección A. | Un gestor de contexto `db_transaction()` que reserve una conexión, la ceda a varias llamadas y confirme o revierta al salir. `db_query` sigue siendo el único helper para el caso simple, que es la decisión ya tomada. | M | `database.py` **[CHOCA]**, `admin/docs.py` **[CHOCA]**, `trash.py` **[CHOCA]**, `backup.py` **[CHOCA]**, `admin/imports.py` **[CHOCA]** |
| IN-165 | Ninguna escritura es idempotente ni tiene clave de operación | `admin/docs.py:245`, `trash.py:193`, `backup.py:412` | Si la respuesta se pierde por un corte o por el tiempo del lambda, el usuario reintenta y se crea un duplicado: dos documentos idénticos, dos versiones, dos ejecuciones de la misma importación. Nada lo detecta. | Clave de idempotencia por operación de escritura, generada en el cliente y almacenada con el resultado durante 24 horas. | M | `models.py` **[CHOCA]**, `admin/docs.py` **[CHOCA]**, `database.py` **[CHOCA]** |
| IN-166 | El reintento de `db_query` puede duplicar una escritura | `database.py:97-108` | Ante `OperationalError` se reintenta la sentencia hasta dos veces. Si el error se produjo **después** de que el servidor ejecutara el `INSERT` pero antes de recibir la confirmación, el reintento lo ejecuta otra vez. Escenario: un corte momentáneo con Neon durante un alta crea el documento por duplicado, y el usuario ve un error. | Reintentar sólo lecturas de forma automática; para escrituras, reintento sólo con clave de idempotencia (IN-165). | M | `database.py` **[CHOCA]** |
| IN-167 | El reintento del pool duerme dentro del hilo de la petición | `database.py:94`, `:107` | `_time.sleep(0.15 * (attempt+1))` bloquea el trabajador mientras el pool está agotado, lo que empeora exactamente la situación que intenta resolver: menos trabajadores disponibles justo cuando faltan conexiones. | Fallo rápido con 503 y `Retry-After`; que reintente el cliente. Y arreglar la causa (IN-005, IN-107). | S | `database.py` **[CHOCA]** |
| IN-168 | `connect_timeout=10` es la mitad del presupuesto del lambda | `database.py:41`, `vercel.json:8` | Diez segundos por intento de conexión, hasta tres intentos: treinta segundos sólo conectando, dentro de un presupuesto de sesenta. Si Neon está frío o inaccesible, la petición se agota sin responder nada útil. | Tiempo de espera de conexión de 3 s y `statement_timeout` en la sesión de PostgreSQL para que ninguna consulta pueda consumir el lambda entero. | S | `database.py` **[CHOCA]** |
| IN-169 | No hay `statement_timeout`: una consulta puede consumir el lambda entero | `database.py:31-42` | Nada limita cuánto puede tardar una sentencia en el servidor. Con los recorridos completos de las secciones C y D, una búsqueda sobre un fondo crecido puede superar los sesenta segundos y matar la función sin dejar respuesta ni error claro. | `options='-c statement_timeout=20000'` en la cadena de conexión, y manejo explícito del error para devolver un 504 con mensaje. | S | `database.py` **[CHOCA]** |
| IN-170 | El arranque en frío falla abierto con las migraciones | `main.py:566-583` | Si una migración falla, se registra un aviso y la aplicación **sigue arrancando** y sirviendo peticiones con el esquema a medias. La decisión está documentada y tiene sentido para no congelar un fallo; lo que falta es que alguien se entere: el aviso queda en el log de Vercel, donde nadie mira. | Mantener el comportamiento y añadir una alerta activa (IN-190) y un indicador en `/api/health/detalle` que diga «esquema desactualizado». | S | `main.py` **[CHOCA]** |
| IN-171 | El fallo de R2 se propaga como 502 sin degradación | `files.py:220-221`, `:245-246` | Cualquier error de Cloudflare, incluido uno transitorio, se convierte en 502 sin reintento. Boto3 reintenta por su cuenta con la configuración por defecto, que no está declarada (IN-126), de modo que el comportamiento real es desconocido. | Reintento explícito y acotado para errores transitorios, y mensaje que distinga «no configurado», «no encontrado» y «no disponible ahora». | S | `files.py` **[CHOCA]**, `storage.py` **[CHOCA]** |
| IN-172 | Un `_construir_backup` con errores produce una copia incompleta marcada como completa | `backup.py:349-361`, `:341-347` | Si una tabla falla, se guarda `[]` y una clave `_error_<tabla>`, pero `_metadata.partial` sigue siendo `False` porque se calcula por el número de tablas **pedidas**, no por las que salieron bien. El resumen que se registra en `backup_history` dice «Export completo». Escenario: la copia diaria lleva seis meses sin la tabla `empleados` y el historial dice que todo va bien. | Si alguna tabla falla, la copia se marca fallida, no se sube, y se registra como fallo. Una copia incompleta que se cree completa es peor que no tener copia. | S | `backup.py` **[CHOCA]** |
| IN-173 | La copia no incluye todas las tablas | `backup.py:300-319` | `EXPORTABLE_TABLES` no contiene `audit_log`, `backup_history`, `documento_versiones`, `schema_version`, `ia_conversaciones`, `ia_mensajes`, `ia_propuestas`, `ia_adjuntos` ni `ia_config`. Escenario: se pierde la base y se restaura: se recupera el archivo pero **se pierde entera la pista de auditoría** —lo único que no se puede reconstruir— y el historial de versiones de los ficheros. | Decidir tabla por tabla qué entra en la copia y escribirlo; la auditoría entra. La copia de la auditoría puede ir aparte y con más retención. | M | `backup.py` **[CHOCA]**, `README.md` |
| IN-174 | La copia no incluye los ficheros de R2 | `backup.py:540-545` | La copia es sólo de la base. Los documentos digitalizados —el objeto que el sistema existe para conservar— no se copian a ninguna parte: viven en un único bucket sin versionado declarado. Escenario: un borrado accidental con las credenciales de IN-002, o un error de configuración de ciclo de vida, y no hay segunda copia de nada. | Versionado de objetos activado en R2 y réplica a un segundo destino; y la copia de la base debe registrar el inventario de claves para poder detectar huérfanos y ausencias. | L | `README.md`, configuración de R2, `backup.py` **[CHOCA]** |
| IN-175 | No hay procedimiento de restauración escrito ni probado | `README.md`, `backup.py:412` | Existe un endpoint de restauración con los defectos de IN-021 a IN-023, y ninguna documentación de qué hacer cuando la base se pierde: en qué orden, con qué credenciales, cuánto tarda, cómo se comprueba que salió bien. Una copia que nadie ha restaurado nunca no es una copia. | Un simulacro documentado: restaurar la última copia en una rama de Neon, comprobar recuentos y arrancar la aplicación contra ella. Repetirlo cada trimestre y dejar constancia. | M | `docs/recuperacion.md` (nuevo), `README.md` |
| IN-176 | La copia diaria no se verifica | `backup.py:519-557` | Se sube a R2 y se registra el tamaño. Nada comprueba que el JSON sea válido, que contenga las trece tablas ni que los recuentos se parezcan a los del día anterior. Escenario: un cambio de esquema rompe el export y durante meses se suben ficheros de 200 bytes que el historial registra como copias correctas. | Verificación posterior a la subida: releer, validar el JSON, comparar recuentos con la ejecución anterior, y fallar ruidosamente ante una caída brusca. | M | `backup.py` **[CHOCA]** |
| IN-177 | Sin objetivos de recuperación declarados | `README.md` | No hay ningún número: cuánto tiempo puede estar caído el sistema (RTO) ni cuántos datos se pueden perder (RPO). Con una copia diaria a las 03:10, el RPO real es de 24 horas, y nadie ha decidido si eso es aceptable para un archivo institucional. | Declarar RTO y RPO, y ajustar la frecuencia de copia en consecuencia. Neon ofrece restauración a un instante concreto: conviene documentar su ventana de retención, que es la protección real. | S | `README.md`, `docs/recuperacion.md` (nuevo) |
| IN-178 | Ningún procedimiento para crear el primer administrador | `README.md`, `admin/users.py:207` | Crear un usuario exige una sesión válida, y no hay ningún camino documentado para conseguir la primera. Una instalación nueva no se puede poner en marcha siguiendo la documentación. | Comando de creación del primer administrador, o migración que lo cree con una contraseña de un solo uso obligada a cambiarse. | S | `README.md`, `app/cli.py` (nuevo) |
| IN-179 | Ningún procedimiento de rotación de secretos | `.env.example`, `core/config.py` | Rotar `SECRET_KEY` cierra todas las sesiones y todos los enlaces compartidos (IN-146); rotar `CRON_SECRET` exige coordinar con la configuración de Vercel; rotar las credenciales de R2 hoy exige tocar código (IN-002). Nada de esto está escrito, y la rotación va a hacer falta ya, por IN-002. | Procedimiento escrito para cada secreto, con el efecto colateral de cada rotación declarado. | S | `README.md`, `docs/operacion.md` (nuevo) |
| IN-180 | No hay entorno de pruebas ni forma de levantar el sistema en local | `README.md`, sin `docker-compose.yml` | Para trabajar hace falta una base PostgreSQL con `unaccent`, `pg_trgm` y el esquema aplicado, y no hay ninguna instrucción ni fichero que lo prepare. Cada persona improvisa, y por eso las pruebas mockean todo (IN-192). | `docker-compose.yml` con PostgreSQL y las extensiones, más un `make dev` que aplique migraciones y siembre datos de ejemplo. Es lo que desbloquea las pruebas de integración. | M | `docker-compose.yml` (nuevo), `README.md`, `app/seed.py` (nuevo) |

---

## G. Observabilidad

| ID | Título | Archivo:línea | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|
| IN-181 | El registro es texto plano formateado a mano | `database.py:17-18`, `main.py:120` | `logging.basicConfig(format="%(asctime)s - %(levelname)s - %(message)s")` y mensajes con f-strings. En Vercel los registros de todas las instancias se mezclan en un flujo; sin campos estructurados no se puede filtrar por endpoint, por usuario ni por código de estado. | Registro en JSON con campos (`ruta`, `metodo`, `estado`, `ms`, `usuario`, `peticion_id`), que es lo que cualquier agregador espera. | M | `database.py` **[CHOCA]**, `main.py` **[CHOCA]** |
| IN-182 | `logging.basicConfig` se ejecuta al importar el módulo de base de datos | `database.py:17` | Configurar el registro global es un efecto secundario de importar `database`, lo que pisa la configuración del servidor de aplicaciones y la de las pruebas, y depende del orden de importación. | La configuración del registro se hace una vez, en la creación de la aplicación. | S | `database.py` **[CHOCA]**, `main.py` **[CHOCA]** |
| IN-183 | No hay identificador de correlación | `main.py:114-121` | Una petición que hace ocho consultas y falla en la última deja ocho líneas sueltas que nada relaciona. Con veinte instancias concurrentes es imposible reconstruir qué pasó en una petición concreta. | `X-Request-ID` generado o propagado, incluido en cada línea de registro y devuelto en la respuesta de error para que el usuario lo pueda citar. | M | `main.py` **[CHOCA]**, `database.py` **[CHOCA]** |
| IN-184 | El registro de peticiones no incluye al usuario ni al que falla | `main.py:114-121` | Se registran método, ruta, estado y milisegundos. No el usuario, no el tamaño de la respuesta, no el identificador del recurso. Y sólo se registran las rutas `/api/`: las páginas y los estáticos no dejan rastro. | Añadir usuario (cuando haya sesión), tamaño y correlación; registrar todas las rutas. | S | `main.py` **[CHOCA]** |
| IN-185 | Los dos middlewares se declaran en orden confuso | `main.py:78-89`, `:114-121` | El de caché se registra antes que el de registro, lo que en Starlette significa que el de registro envuelve al de caché. Funciona, pero el orden efectivo es el inverso al de lectura y no hay ningún comentario que lo advierta; el manejador de excepciones queda además declarado entre ambos. | Agrupar los middlewares en un módulo propio, en un orden explícito y comentado (IN-060). | S | `main.py` **[CHOCA]** |
| IN-186 | No hay seguimiento de errores | proyecto completo | Ningún Sentry ni equivalente. Un 500 en producción sólo existe si alguien abre los registros de Vercel en la ventana de retención y lo encuentra entre el ruido. Con IN-035 (sin traza) es directamente irrecuperable. | Integrar un servicio de seguimiento de errores con la traza completa, el identificador de correlación y el usuario, y filtrado de datos personales antes del envío. | M | `main.py` **[CHOCA]**, `requirements` |
| IN-187 | No hay ninguna métrica | proyecto completo | No se mide el número de peticiones, la latencia por percentil, la tasa de error, el uso del pool, el número de arranques en frío, ni el gasto del asistente por día. Todas las decisiones de rendimiento de la sección D se tomarían a ciegas. | Métricas mínimas expuestas o enviadas a un agregador: latencia por endpoint, errores por tipo, consultas por petición (IN-127), conexiones en uso. | M | `main.py` **[CHOCA]**, `database.py` **[CHOCA]** |
| IN-188 | No hay ninguna alerta | proyecto completo | Nadie se entera de que el backup diario falló (el endpoint devuelve 502 al cron de Vercel y ahí acaba), de que las migraciones no aplicaron, de que el pool está agotado o de que hay un pico de fallos de autenticación. | Alertas sobre los cuatro sucesos anteriores, al menos por correo. Un backup que falla en silencio es lo mismo que no tener backup. | M | `backup.py` **[CHOCA]**, `main.py` **[CHOCA]**, `docs/operacion.md` (nuevo) |
| IN-189 | El resultado del cron no se comprueba nunca | `vercel.json:85-90`, `backup.py:519` | Vercel invoca la ruta y registra el código de estado; nadie lo revisa. `backup_history` guarda el intento fallido, que es correcto, pero la fila queda en una pestaña que se mira cuando ya se necesita la copia. | Una comprobación diaria activa: si no hay copia correcta de las últimas 26 horas, alerta. | S | `backup.py` **[CHOCA]**, `main.py` **[CHOCA]** |
| IN-190 | `/api/health` no comprueba lo que importa | `main.py:715-738` | Sólo comprueba que se puedan contar filas. No comprueba que R2 responda, que las migraciones estén al día (`schema_version` frente a la huella actual), ni que la última copia sea reciente. Un sistema sin almacenamiento de ficheros responde `ok`. | Comprobaciones separadas y etiquetadas: base, almacenamiento, esquema, última copia. Cada una con su propio estado. | S | `main.py` **[CHOCA]**, `storage.py` **[CHOCA]** |
| IN-191 | Los mensajes de registro mezclan tres idiomas y dos estilos de formato | `main.py:551`, `:563`, `:583`, `database.py:106`, `:117` | Conviven español con y sin tildes, inglés (`"Migrations completadas"`, `"Backfill ... OK"`) y las dos formas de interpolación (`f-string` y `%s` perezoso). El `f-string` evalúa siempre aunque el nivel esté desactivado. | Un solo idioma, y `%s` perezoso siempre. `ruff` con la regla correspondiente lo detecta (IN-197). | S | `main.py` **[CHOCA]**, `database.py` **[CHOCA]** |

---

## H. Calidad, pruebas y herramientas

| ID | Título | Archivo:línea | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|
| IN-192 | Ninguna prueba toca una base de datos real | `tests/conftest.py:4`, todos los `test_*.py` | `db_query` se mockea siempre. La consecuencia es que **ningún SQL del proyecto se ejecuta jamás en la suite**: no se comprueban tipos, ni claves foráneas, ni `NULL`, ni el `%%` literal, ni el comportamiento de `ON CONFLICT`. Las tres guardas que existen (`test_sql_columns`, `test_sql_inserts`, `test_migraciones`) son reconstrucciones estáticas del esquema precisamente porque no hay base — y existen porque cinco endpoints respondían 500 en producción sin que la suite lo viera. | Pruebas de integración contra PostgreSQL efímero (contenedor en local, rama de Neon en integración continua), con el esquema aplicado desde las migraciones reales. Las pruebas con mock se quedan para la lógica pura. | L | `tests/conftest.py`, `docker-compose.yml` (nuevo), `.github/workflows/` (nuevo) |
| IN-193 | La suite desactiva la autorización en todas las pruebas | `tests/conftest.py:37-42` | `app.dependency_overrides[require_session] = lambda: "test_user"` en la fixture que usa **todo** el proyecto. Ninguna prueba comprueba jamás que un endpoint exija sesión, y con IN-131 tampoco habría nada que comprobar sobre roles. Un endpoint nuevo sin protección pasa la suite entera. | Un cliente anónimo además del autenticado, y una prueba parametrizada sobre **todas** las rutas registradas que verifique que cada una exige lo que su clasificación dice (IN-135). | M | `tests/conftest.py`, `tests/test_seguridad.py` (nuevo) |
| IN-194 | `test_secrets.py` está documentado y no existe | `CLAUDE.md:36`, `CLAUDE.md:365`, `app/tests/` | La documentación afirma dos veces que la suite «rechaza credenciales escritas en el código» y que «`test_secrets.py` lo verifica en cada corrida». El fichero no está en el directorio. Es la razón por la que IN-002 lleva ahí sin que nadie lo vea: la guarda existe sólo en el documento. | Escribir la prueba de verdad, con detección de claves de AWS/R2, cadenas de conexión y claves de API. Y revisar qué otras guardas documentadas no existen. | S | `tests/test_secrets.py` (nuevo), `CLAUDE.md` |
| IN-195 | No hay integración continua | ausencia de `.github/` | Las pruebas sólo corren si alguien se acuerda de ejecutarlas antes de desplegar, y con hasta veinte agentes trabajando en paralelo eso no se sostiene. Nada impide fusionar código que no compila. | Flujo de trabajo que en cada propuesta de cambio ejecute pruebas, análisis estático, formato y migraciones contra una rama efímera de Neon. Es el pendiente de calidad de mayor efecto. | M | `.github/workflows/ci.yml` (nuevo) |
| IN-196 | No hay ganchos de pre-commit | ausencia de `.pre-commit-config.yaml` | Las guardas que ya existen (UTF-8 sin BOM, `node --check`, `pyflakes`) sólo actúan al ejecutar pytest. Los fallos que documentan —comillas tipográficas, nombres indefinidos— se detectarían antes de crear la confirmación. | `pre-commit` con `ruff`, `black`, comprobación de BOM y `node --check`, reutilizando lo que ya hacen las pruebas. | S | `.pre-commit-config.yaml` (nuevo), `requirements-dev.txt` |
| IN-197 | Sin `ruff` ni `black`: no hay estilo verificado | `requirements-dev.txt` | Sólo `pyflakes`, que detecta nombres indefinidos e importaciones muertas y nada más. El proyecto tiene importaciones a mitad de fichero (IN-061), dos sentencias en una línea (`imports.py:360`), longitudes de línea dispares y f-strings en el registro (IN-191): todo eso lo señala `ruff` sin configurar nada. | `ruff` con un conjunto de reglas acordado y `black` para el formato, ejecutados en pre-commit y en integración continua. | S | `requirements-dev.txt`, `pyproject.toml` (nuevo) |
| IN-198 | Sin `mypy`: los tipos que hay no se comprueban | `requirements-dev.txt` | Hay anotaciones en algunos sitios (`utils.py:5`, `core/security.py:29`) y en ninguno se verifican. Hoy `mypy` no encontraría gran cosa porque `db_query` devuelve `Any` (IN-053), pero ése es el orden correcto: primero tipar el borde de datos, después comprobar. | `mypy` en modo permisivo sobre `core/`, `utils.py` y `models.py` primero, ampliando conforme aparezcan los repositorios tipados. | M | `requirements-dev.txt`, `pyproject.toml` (nuevo) |
| IN-199 | Sin `eslint` ni `prettier` para veinte ficheros JavaScript | `app/static/*.js` | El frontend son unas veinte mil líneas sin ninguna herramienta más allá de `node --check`, que sólo comprueba que el fichero parsee. Las clases de fallo que las auditorías hermanas encuentran una y otra vez —manejadores en línea con comillas mal escapadas (BA-001), llamadas a APIs de librerías que no se cargan (BA-013), variables de estado sin inicializar (BR-073)— las detecta `eslint` sin esfuerzo. | `eslint` con configuración para navegador y las globales del proyecto declaradas, más `prettier`, en pre-commit y en integración continua. | M | `eslint.config.js` (nuevo), `app/static/*.js`, `package.json` (nuevo) |
| IN-200 | Sin medida de cobertura | `requirements-dev.txt` | No hay `pytest-cov` ni ningún umbral. No se sabe qué porcentaje del backend ejecutan las pruebas, y por IN-192 la cifra real de código con verificación efectiva es mucho menor de lo que cualquier medida diría. | `pytest-cov` con informe en integración continua y un umbral que no pueda bajar, aunque se empiece con uno modesto. | S | `requirements-dev.txt`, `.github/workflows/` (nuevo) |
| IN-201 | Los ficheros grandes del sistema no tienen ninguna prueba | `backup.py` (297 líneas), `trash.py` (272), `files.py` (94), `share.py`, `routes/ai.py` (535), `core/ai_tools.py` (868) | Existen `test_backup.py` y `test_share.py`, pero `trash.py` (papelera y versionado, con IN-016 a IN-020 dentro), `files.py` (subida y descarga) y las 1.400 líneas del asistente no tienen ninguna. BR-057 señala la ausencia para RRHH; el hueco es más ancho. | Pruebas para papelera, versionado, subida y herramientas del asistente, con base real (IN-192). | L | `tests/` |
| IN-202 | No hay ni una prueba de extremo a extremo del frontend | `requirements-dev.txt:12-14` | Playwright se menciona como opcional «para revisión visual» y no se usa. Las auditorías hermanas han encontrado por lectura fallos que sólo existen al renderizar: facetas que no responden a ningún clic, paneles que abren vacíos, esqueletos que giran para siempre. Nada de eso lo detecta el backend. | Suite de Playwright con los seis recorridos principales (entrar, buscar, abrir documento, catalogar, editar, exportar), ejecutada en integración continua contra la aplicación con datos sembrados. | L | `tests/e2e/` (nuevo), `.github/workflows/` (nuevo), `docker-compose.yml` (nuevo) |
| IN-203 | El frontend no se compila: veinte ficheros servidos crudos | `app/static/*.js`, `app/static/*.html` | Sin empaquetado, sin minificación, sin huella de contenido (IN-115), sin módulos: todo son globales compartidas entre ficheros cargados en un orden que hay que respetar a mano y que `CLAUDE.md` documenta como frágil. Cada página descarga una decena de ficheros. | Un paso de compilación mínimo (esbuild) que produzca un paquete por página con huella en el nombre. No exige reescribir el código: sólo declarar las entradas. | L | `app/static/*.js`, `package.json` (nuevo), `vercel.json` **[CHOCA]** |
| IN-204 | El JavaScript no tiene documentación de funciones | `app/static/*.js` | Ni JSDoc ni tipos. `app-core.js` define el estado global que usan todos los demás ficheros y no hay ningún sitio donde esté escrito qué contiene `state`. Con veinte agentes trabajando en paralelo, eso es el origen de la mitad de las colisiones. | JSDoc al menos para el estado global y las funciones compartidas de `app-core.js`, `app-shell.js` y `app-choices.js`; comprobación de tipos con `checkJs` cuando exista compilación. | M | `app/static/app-core.js`, `app-shell.js`, `app-choices.js` |
| IN-205 | Los ficheros de compilación de LaTeX están versionados | `requerimientos_resumen.{aux,log,out,pdf}`, `docs/tex/main.{aux,log,out,pdf}`, `docs/tex/figures/generated/*.pdf` | Artefactos generados en el control de versiones: cambian en cada compilación, no se pueden fusionar y engordan el repositorio. `main.log` son 18 KB de salida de compilador. | `.gitignore` para los artefactos de LaTeX; los PDF finales, si hacen falta, se publican como versión y no se versionan. | S | `.gitignore` |
| IN-206 | `audit_log.csv` y `Pendientes.xlsx` están en el repositorio | `audit_log.csv`, `Pendientes.xlsx` | Un volcado de la tabla de auditoría en la raíz del repositorio contradice directamente la regla del holding de que ningún dato de cliente va en git fuera de `archivo/` y `_datos/`. `Pendientes.xlsx` es además un binario que nadie puede revisar en un cambio. | Retirar ambos del control de versiones (y del historial si el CSV contiene nombres de usuario reales), y llevar los pendientes a Markdown. | S | `.gitignore`, `audit_log.csv`, `Pendientes.xlsx` |
| IN-207 | `.pytest_cache` y los `__pycache__` están en el árbol de trabajo | `.pytest_cache/`, `app/core/__pycache__/`, `app/routes/__pycache__/` | No están versionados, pero sí presentes en el directorio del proyecto, y los `.pyc` compilados son de Python 3.12 mientras `.python-version` fija 3.11 (IN-212): la versión con la que se está trabajando no es la que se declara. | Confirmar la exclusión en `.gitignore`, limpiar el árbol y alinear la versión de Python (IN-212). | S | `.gitignore`, `.python-version` |

---

## I. Modernidad, dependencias y documentación

| ID | Título | Archivo:línea | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|
| IN-208 | Tres ficheros de dependencias que se contradicen | `api/requirements.txt`, `app/requirements.txt`, `requirements-dev.txt` | `app/requirements.txt` tiene `boto3` y `aws-psycopg2`; `api/requirements.txt`, el que instala Vercel, no tiene ninguno de los dos (IN-003) y sí `pandas`. Nadie sabe cuál es la verdad, y el fichero que manda es el que menos se lee. | Un fichero de producción, uno de desarrollo que lo incluya con `-r`, y ninguno más. | S | `api/requirements.txt`, `app/requirements.txt`, `requirements-dev.txt` |
| IN-209 | Versiones sin fijar y sin fichero de bloqueo | `api/requirements.txt:7-9`, `app/requirements.txt` | `pydantic` sin versión, `mangum>=0.17.0`, `boto3>=1.34.0`, `pytest>=8.0`. Cada despliegue de Vercel resuelve lo que haya ese día: una versión mayor de `pydantic` cambiaría el comportamiento de todos los modelos sin que nadie tocara nada. La compilación no es reproducible. | Fijar todo con `==` y generar un fichero de bloqueo con `pip-compile` o `uv`, actualizado deliberadamente. | S | `api/requirements.txt`, `requirements.lock` (nuevo) |
| IN-210 | Sin revisión de vulnerabilidades de dependencias | ausencia de configuración | Nada comprueba si alguna de las dependencias tiene vulnerabilidades conocidas, y varias llevan más de un año sin actualizarse en el proyecto. | `pip-audit` en integración continua y actualizaciones automáticas de seguridad. | S | `.github/workflows/` (nuevo), `requirements-dev.txt` |
| IN-211 | FastAPI 0.110 y uvicorn 0.28 son de principios de 2024 | `api/requirements.txt:1-2` | Dos años de correcciones y mejoras sin incorporar, incluidas las de rendimiento en la validación de Pydantic v2 y las de seguridad de `python-multipart`, que aquí se usa para la subida de ficheros. `bcrypt==4.1.3` está fijado a una versión concreta que ya ha tenido revisiones. | Actualizar por saltos, con la suite de pruebas y las de extremo a extremo (IN-202) como red. La ausencia de esa red es lo que hace que actualizar dé miedo hoy. | M | `api/requirements.txt`, `requirements.lock` (nuevo) |
| IN-212 | La versión de Python declarada no es la que se usa | `.python-version` (3.11) vs `app/core/__pycache__/*.cpython-312.pyc` | El fichero fija 3.11 y el árbol tiene bytecode de 3.12. Vercel elige el tiempo de ejecución por su cuenta si no se lo dice el fichero, así que hay al menos dos versiones en juego. En 3.12, `datetime.utcnow()` está formalmente obsoleto y emite aviso (IN-213). | Declarar una sola versión —3.12, que es a lo que apunta el trabajo real— en `.python-version`, en `vercel.json` y en la integración continua. | S | `.python-version`, `vercel.json` **[CHOCA]**, `.github/workflows/` (nuevo) |
| IN-213 | `datetime.utcnow()` está obsoleto y se usa en cuatro sitios | `admin/docs.py:439`, `:634`, `backup.py:342`, `:404`, `:542` | Obsoleto desde Python 3.12 y con eliminación anunciada; devuelve un instante *naive* que es exactamente la causa de IN-009. Cuando el aviso pase a error, cinco líneas del sistema fallan a la vez, incluidas dos del backup. | `datetime.now(timezone.utc)`, y para las columnas de instante, `NOW()` en SQL (IN-009). | S | `admin/docs.py` **[CHOCA]**, `backup.py` **[CHOCA]** |
| IN-214 | psycopg2 en lugar de psycopg3 | `api/requirements.txt:5`, `database.py:7-9` | psycopg2 está en mantenimiento; psycopg3 aporta agrupación de conexiones propia y pensada para este caso, soporte asíncrono, mejor manejo de tipos y `ClientCursor` para depurar el SQL con los parámetros ya sustituidos —que es justo lo que hoy no se puede ver en los registros (IN-127). El paso también resolvería el duplicado `aws-psycopg2` (IN-003). | Migrar a `psycopg[binary,pool]`. Con `db_query` como único punto de acceso, el cambio queda contenido en un fichero, que es precisamente el valor de esa decisión ya tomada. | M | `database.py` **[CHOCA]**, `api/requirements.txt` |
| IN-215 | `.env.example` no lista las variables de R2 | `.env.example`, `CLAUDE.md:355-362` | `CLAUDE.md` documenta `R2_ENDPOINT`, `R2_ACCESS_KEY`, `R2_SECRET_KEY` y `R2_BUCKET` como requeridas, y `.env.example` —que se presenta como «la lista completa y comentada»— no las menciona, porque el código no las lee (IN-002). Quien despliegue siguiendo el fichero se lleva un sistema que escribe en un bucket ajeno. | Añadirlas junto con el resto, después de arreglar IN-002. Y una comprobación de arranque que enumere las variables que faltan. | S | `.env.example`, `core/config.py` **[CHOCA]** |
| IN-216 | La documentación describe un sistema que no es el desplegado | `CLAUDE.md`, `README.md`, `CHANGELOG.md` | Tres afirmaciones centrales de `CLAUDE.md` no se sostienen contra el código: las migraciones corren en cada arranque en frío (IN-001: no corren), ninguna credencial se escribe en el código y `test_secrets.py` lo verifica (IN-002 e IN-194: sí se escriben y la prueba no existe), y el backup es «solo para el admin Global» (IN-129: lo es para cualquiera con sesión). Para un documento que es el contrato de trabajo de veinte agentes, cada afirmación falsa se propaga a todo lo que se construya encima. | Revisar `CLAUDE.md` afirmación por afirmación contra el código, y establecer que toda decisión documentada tenga una prueba que la sostenga —el proyecto ya trabaja así con las guardas; falta aplicarlo a estas tres. | M | `CLAUDE.md`, `README.md`, `tests/` |

---

## Orden de ataque

Cuatro bloques. El orden no es por gravedad sino por dependencia: cada bloque
desbloquea al siguiente, y los tres primeros son requisito para que el cuarto se
pueda hacer sin romper nada.

### Bloque 0 — Parar la hemorragia (días, no semanas)

Cinco cosas, ninguna negociable, y ninguna depende de las demás:

- **IN-002** rotar y sacar del código las credenciales de R2, con **IN-194**
  (escribir de verdad `test_secrets.py`) en el mismo cambio.
- **IN-129** e **IN-130**: cerrar el export y el restore de la copia al rol
  global. Son dos líneas y quitan la escalada a administrador.
- **IN-132**: cerrar la gestión de usuarios.
- **IN-001**: decidir y arreglar cómo se aplican las migraciones en Vercel; hasta
  que esté hecho, cualquier cambio de esquema es teatro.
- **IN-003**: confirmar que el paquete de Vercel resuelve `boto3`.

Después, **IN-136** (`SECRET_KEY` obligatoria), **IN-038** (retirar `/verify`),
**IN-037** (cerrar `/api/health`) e **IN-004** (el `rollback` que falta), que son
todas de menos de una hora.

### Bloque 1 — La red de seguridad (2–3 semanas)

Sin esto, nada de lo demás se puede tocar con confianza. Es lo que convierte el
resto de la auditoría en trabajo hacible en vez de en apuestas:

- **IN-180** entorno local reproducible → **IN-192** pruebas contra base real →
  **IN-195** integración continua. En ese orden, porque cada uno necesita al
  anterior.
- **IN-193** y **IN-135**: la prueba que enumera todas las rutas y comprueba qué
  exige cada una. Es la que impide que la sección E se vuelva a abrir.
- **IN-196**, **IN-197**, **IN-199**: ganchos de pre-commit, `ruff` y `eslint`.
  Baratos y con efecto inmediato en un equipo de veinte agentes.
- **IN-186**, **IN-183**, **IN-181**: seguimiento de errores, correlación y
  registro estructurado. Sin esto, los fallos que quedan seguirán siendo
  invisibles.
- **IN-188** y **IN-189**: alerta si el backup falla. Es lo único de fiabilidad
  que no puede esperar al bloque 3.

### Bloque 2 — Autorización y transacciones (3–4 semanas)

Los dos defectos estructurales, y conviene hacerlos juntos porque ambos tocan
todas las rutas:

- **IN-131** las dependencias de rol y módulo, aplicadas endpoint por endpoint,
  con **IN-133** (el usuario sale de la sesión, no del cuerpo) en el mismo
  recorrido: es el mismo fichero abierto y la misma revisión. Cierra además
  IN-008, IN-011, IN-134, IN-150 y IN-161.
- **IN-164** el gestor de transacciones, y con él IN-013, IN-016, IN-017,
  IN-020, IN-022 y IN-165. Un fichero (`database.py`) y luego seis
  sustituciones.
- **IN-039** y **IN-143**: revocación de sesión, que es lo que hace que
  desactivar a alguien signifique algo.
- Los bugs de datos que ya no se pueden posponer: **IN-006**, **IN-007**,
  **IN-009**, **IN-012**, **IN-014**, **IN-025**, **IN-027**.

### Bloque 3 — Rendimiento, esquema y arquitectura (continuo)

Ya con red y con permisos, por orden de relación coste/beneficio:

1. **IN-045** e **IN-105**: sacar pandas. Es la mejora de latencia percibida más
   grande y toca cuatro ficheros. Arrastra IN-029, IN-030 e IN-031.
2. **IN-104**, **IN-076**, **IN-077**, **IN-078**, **IN-080**: los índices y la
   columna generada de búsqueda. Una migración, medida antes y después con
   **IN-098**.
3. **IN-107**, **IN-108**, **IN-101**, **IN-005**: el pool y la auditoría en
   segundo plano. Es lo que sostiene la concurrencia.
4. **IN-068** a **IN-075** y **IN-102**, **IN-103**: la integridad del esquema.
   Cada migración precedida de una consulta que liste lo que ya está mal, porque
   ninguna de estas restricciones se puede aplicar a ciegas sobre datos reales.
5. **IN-042**, **IN-043**, **IN-047**, **IN-050**: repositorios, el constructor
   único de sentencias y los esquemas de respuesta. Es refactorización pura y
   sólo se hace bien con el bloque 1 terminado.
6. **IN-173** a **IN-177**: la recuperación de verdad —qué se copia, dónde,
   verificado y ensayado. Va al final por dependencia, no por importancia: exige
   que el export y el restore de los bloques anteriores estén arreglados.
7. **IN-202**, **IN-203**: extremo a extremo y compilación del frontend, el
   trabajo largo que cierra el hueco que ninguna auditoría de lectura puede
   cubrir.

Y de forma transversal, **IN-216**: cada bloque que se cierre actualiza
`CLAUDE.md` en el mismo cambio. La documentación que describe un sistema que no
existe es lo que ha permitido que IN-002 llevara ahí desde el principio.
