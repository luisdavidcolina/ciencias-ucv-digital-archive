# Archivo Institucional Digital — Facultad de Ciencias, UCV

Sistema de gestión documental e inventario de RRHH para la Facultad de Ciencias de la Universidad Central de Venezuela.

## Stack Técnico

- **Backend**: Python 3.11 + FastAPI 0.110
- **Base de datos**: PostgreSQL (Neon serverless) via psycopg2
- **Frontend**: Vanilla JS + Bootstrap 4.6 + FontAwesome 6 — todo por CDN
- **Despliegue**: **Vercel** (NO Render) — `api/index.py` usa `mangum` como adaptador ASGI
- **Charts**: Chart.js 4.4 via CDN
- **Archivos**: Cloudflare R2 (API compatible con S3) via boto3

**AdminLTE NO se carga.** Parte del marcado usa sus clases (`info-box`,
`card-outline`, `content-wrapper`) porque el proyecto nació de una plantilla
suya, pero la hoja de estilos de AdminLTE no está enlazada en ninguna página:
todo eso lo implementa `styles.css`. Si añades marcado con clases de AdminLTE,
comprueba que exista una regla propia — de lo contrario sale sin estilos.

## Pruebas

```bash
pip install -r requirements-dev.txt
python -m pytest app/tests -q
```

Además de los tests de endpoints, la suite incluye guardas que cubren fallos que
ya llegaron a `main` una vez:

- `test_static_assets.py` — cada estático es UTF-8 válido, sin BOM ni texto
  doble-codificado, y cada `.js` pasa `node --check`. Existe porque llegaron a
  `main` dos archivos con comillas tipográficas como delimitador de string, que
  no parseaban.
- `test_static_analysis.py` — `pyflakes`: un nombre indefinido es un error. Un
  refactor de renombrado dejó cinco llamadas a funciones que ya no existían.
- `test_secrets.py` — rechaza credenciales escritas en el código.
- `test_sql_columns.py` — el SQL sólo puede citar columnas que existen, y todo
  `%` literal debe ir duplicado. Existe porque cinco endpoints respondían 500 en
  producción por esas dos causas y la suite no los veía: mockea `db_query`, así
  que ninguna consulta se ejecuta nunca. El esquema se deriva de `schema.sql`
  más las migraciones, sin necesidad de base de datos.
- `test_migraciones.py` — la huella del esquema: con el esquema al día no se
  ejecuta ninguna sentencia, y si una migración falla la huella NO se registra.
- `test_stats_totales.py` — la forma de la respuesta de cifras del tablero.
- `test_sql_inserts.py` — cada `INSERT` cuadra columnas contra valores y no
  omite columnas `NOT NULL`. La importación CSV nunca funcionó: escribía el
  nombre de usuario en `updated_by` (INTEGER) y omitía `creado_por`. Como el
  bucle captura la excepción por fila, devolvía HTTP 200 con `inserted: 0` y la
  pantalla decía "Importación completada".
- `test_share.py` — los enlaces externos: caducan, no se pueden manipular, no
  sirven para otro documento, no sobreviven a un cambio de `SECRET_KEY` y no
  exponen la ruta interna del archivo.
- `test_paginas.py` — cada HTML tiene ruta y cada ruta su HTML. Existe porque
  `index.html` (1328 líneas) no estaba enrutado: sólo se alcanzaba en
  `/static/index.html`, nadie lo enlazaba, y llevaba tiempo divergiendo del
  resto. Se retiró junto con las ramas de `app.js` que sólo servían a esa SPA.
- `test_admin_panels.py` — cada pestaña tiene panel, cada panel tiene pestaña,
  ambos módulos ofrecen la misma navegación, los encabezados del monitor cuadran
  con las celdas que emite la plantilla, y ninguna página vuelve a traer la
  cáscara escrita a mano.

## Estructura de Directorios

```
/
├── api/
│   ├── index.py          # Punto de entrada Vercel (mangum wrapper)
│   └── requirements.txt  # Dependencias para Vercel
├── app/
│   ├── main.py           # App FastAPI, run_migrations(), include_router()
│   ├── database.py       # ThreadedConnectionPool + db_query() helper
│   ├── models.py         # Pydantic models para todos los endpoints
│   ├── utils.py          # paginate(), generate_slug() y helpers sin deps de rutas
│   ├── storage.py        # Cloudflare R2: subida, URLs prefirmadas, borrado
│   ├── schema.sql        # Esquema SQL de referencia (NO modificar)
│   ├── core/
│   │   ├── config.py     # Settings class con variables de entorno
│   │   ├── security.py   # hash_password(), verify_password()
│   │   ├── cache.py      # TTLCache para choices
│   │   ├── ai.py         # Cliente OpenRouter, catálogo de modelos, gasto
│   │   ├── ai_prompts.py # Prompt de sistema del asistente
│   │   ├── ai_tools.py   # Herramientas que el asistente puede invocar
│   │   └── ai_proposals.py # Propuestas de cambio que aprueba una persona
│   ├── routes/
│   │   ├── admin/        # docs, catalog, stats, retention, imports, users,
│   │   │                 # helpers, deps
│   │   ├── archive.py    # Búsqueda y CRUD de Archivo institucional
│   │   ├── hr.py         # Búsqueda y CRUD de RRHH + report PDF
│   │   ├── hr_alerts.py  # Alertas jubilaciones + historial de cargos
│   │   ├── auth.py       # Login, restore session, password change
│   │   ├── backup.py     # Export/restore/history de backups
│   │   ├── lookups.py    # Datos para dropdowns (con cache TTL 300s)
│   │   ├── pages.py      # Serve HTML pages
│   │   ├── files.py      # Proxy/serve de archivos desde R2
│   │   ├── trash.py      # Papelera + versiones de archivos digitales
│   │   └── ai.py         # Asistente IA (chat, propuestas, config)
│   ├── tests/            # pytest — endpoints + guardas (ver "Pruebas")
│   └── static/
│       ├── archive.html / hr.html          # Búsqueda pública
│       ├── admin_archive.html / admin_hr.html   # Paneles de módulo
│       ├── admin_system.html   # Admin Global (backup, audit, alertas)
│       ├── admin_ai.html       # Consola del asistente IA
│       ├── login.html · ayuda.html · investigacion.html
│       ├── compartido.html     # Vista pública de un documento compartido
│       ├── styles.css          # TODA la hoja de estilos propia
│       │
│       ├── app-shell.js        # Barra superior y menú lateral (definición única)
│       ├── app-core.js         # state, API_BASE, escHtml, showToast, helpers
│       ├── app.js              # Sesión, navegación entre secciones, listeners
│       ├── app-theme.js        # Temas, modo oscuro, densidad, notificaciones
│       ├── app-choices.js      # /api/choices, TomSelect, controles de fecha
│       ├── viz-tokens.js       # Puente entre los tokens --viz-* y Chart.js
│       │
│       ├── archive.js / hr.js  # Búsqueda pública de cada módulo
│       ├── admin.js            # loadAdminTab(): el switch de pestañas
│       ├── admin-stats.js      # KPIs de la cabecera
│       ├── admin-charts.js     # Gráficas Chart.js + importación CSV
│       ├── admin-monitor.js    # Tabla del monitor
│       ├── admin-submit.js     # Alta de documentos y empleados
│       ├── admin-edit.js / admin-edit-hr.js   # Edición, papelera, versiones
│       ├── admin-categories.js · admin-users.js · admin-ui.js
│       ├── ai-widget.js        # Burbuja de chat del asistente
│       └── scanner-client.js   # Integración con el escáner local
├── requirements-dev.txt  # pytest, httpx, pyflakes
└── vercel.json           # Config Vercel: builds + routes
```

## Modelo de Datos Principal

### Módulo Archivo
- `datos_archivo` — documentos institucionales (titulo, autor, fecha_documento, tesauro_primario, id_tipo_documento, abstract, ubicacion, file_url, personas_relacionadas, updated_at, updated_by)
- `archivo_descriptores` — relación N:N entre datos_archivo y descriptores_libres
- `descriptores_libres` — palabras clave (nombre UNIQUE)
- `tipo_documento` (scope archivo) — tipos organizados bajo categoria slug='archivo'

### Módulo RRHH
- `empleados` — personal docente (cedula, nombres, apellidos, cargo, departamento, estado, rif, fecha_jubilacion, fecha_pension, foto_url, is_active, last_login)
- `datos_rrhh` — documentos por empleado (empleado_id FK, id_tipo_documento FK, personas_relacionadas, notas, fecha_documento, ubicacion, file_url)
- `rrhh_descriptores` — relación (raro, mayormente RRHH no usa descriptores libres)
- `tipo_documento` (scope RRHH) — 4 Partes: parte-i, parte-ii, parte-iii, parte-iv

### Sistema
- `categoria` — categorías con slug (parte-i, parte-ii, parte-iii, parte-iv, archivo)
- `usuarios_sistema` — usuarios con modulo, rol, is_active, last_login
- `audit_log` — eventos del sistema (accion, usuario, modulo, detalle, status, timestamp)
  ⚠️ La marca de tiempo se llama `timestamp`, **no** `created_at`: usarla mal
  costó un 500 en el panel de Sistema.
- `backup_history` — registro de exports/restores

### Vista
- `vw_rrhh_persona_index` — vista agregada por empleado para búsqueda (persona_raw, cedula, cargo, departamento, estado, fecha_ingreso, foto_url, doc_count, tipos)

## Patrones de Código Importantes

### db_query
```python
# Único helper para todas las queries. Usa ThreadedConnectionPool(1,5).
rows = db_query("SELECT * FROM tabla WHERE id = %s", [id], fetch="all")
row  = db_query("SELECT * FROM tabla WHERE id = %s", [id], fetch="one")
db_query("UPDATE tabla SET x=%s WHERE id=%s", [x, id], fetch="none", commit=True)
```
- `fetch="all"` → lista (nunca None, puede ser [])
- `fetch="one"` → dict o None
- `fetch="none"` → None, úsalo para INSERT/UPDATE/DELETE con commit=True

### run_migrations()
En `main.py`, lista de tuplas `(description, sql)`. Se ejecuta al arrancar la app.

**Corre en cada arranque en frío.** En serverless eso son ~80 viajes de ida y
vuelta a Neon —que está en otro continente— antes de poder responder la primera
petición. Por eso se guarda una huella SHA-256 del conjunto en
`public.schema_version`: si coincide, no se aplica nada y basta una consulta.

- La huella se registra **sólo si todas las migraciones aplicaron**. Si alguna
  falla se reintenta en el siguiente arranque; congelar un fallo sería peor.
- Si no se puede leer `schema_version`, se aplican igual: son idempotentes y
  perder un arranque rápido es mejor que saltarse una migración real.
- Añadir o editar una migración cambia la huella sola. No hay número de versión
  que recordar.

⚠️ **Todo `%` literal dentro de SQL va duplicado (`%%`).** `db_query` hace
`cur.execute(sql, params or ())`, así que psycopg2 recibe siempre una tupla e
interpreta cualquier `%` como marcador aunque no haya parámetros. Un
`LIKE '%algo%'` lanza `IndexError` en tiempo de ejecución. Una migración llevaba
así desde el principio, fallando en silencio porque el bucle captura la
excepción por paso.
- Siempre usar `CREATE TABLE IF NOT EXISTS`, `ADD COLUMN IF NOT EXISTS`, `CREATE INDEX IF NOT EXISTS`
- Nunca usar `DROP` en migraciones
- Para INSERT en tablas de catálogo: `ON CONFLICT (slug) DO NOTHING` o `ON CONFLICT DO NOTHING`

### Caché de choices
`lookups.py` tiene cache TTL 300s. Llama `invalidate_choices_cache()` después de modificar categoria o tipo_documento.

### Autenticación
- Sesión guardada en `localStorage` como JSON con TTL 12h
- `/api/auth/restore` valida la sesión en background
- `state.user.modules` es array de strings: `["Archivo"]`, `["RRHH"]`, o `["Archivo","RRHH"]` (Global)
- Global admin = tiene AMBOS módulos

### Admin Panels
- Dos páginas separadas: `admin_archive.html` y `admin_hr.html`
- `adminSuffixFromTab()` retorna "archivo" o "rrhh"
- IDs de elementos HTML tienen sufijo: `#monitor-table-archivo`, `#monitor-table-rrhh`
- `loadAdminTab(tabId)` es el switch principal de tabs en `admin.js`

Las nueve pestañas están **agrupadas por intención**, con un `<li class="ds-tab-sep">`
entre grupos:

| Grupo | Pestañas |
|---|---|
| Uso diario | Resumen · Ingresar |
| Gestionar | Documentos / Expedientes · Tipos · Papelera |
| Controlar | Retención · Auditoría |
| Administrar | Acceso · Exportar |

Están agrupadas **por verbo**, que es como lo resuelven los sistemas de
descripción archivística — AtoM organiza su menú en *Add / Manage / Import /
Admin*, no en una lista plana de funciones.

El nombre dice **qué hay dentro**, y por eso difiere entre módulos: en Archivo la
unidad de trabajo es el documento y en RRHH el expediente. "Monitor" no
significaba nada. Los identificadores internos (`monitor`, `stats`, …) no
cambian; sólo la etiqueta.

Al añadir una pestaña hacen falta **tres** cosas, o queda un panel en blanco que
nadie reporta: el `<li>` con `id="tab-admin-{suf}-{x}"`, el
`<div id="pane-admin-{suf}-{x}">`, y su rama en `loadAdminTab`. Ambos módulos
deben ofrecer la misma navegación. `test_admin_panels.py` verifica las tres.

### Cáscara compartida (`app-shell.js`)
La barra superior y el menú lateral se renderizan desde una sola definición.
Cada página aporta dos huecos y el script, **en este orden**:

```html
<div id="app-shell-navbar"></div>
<div id="app-shell-sidebar"></div>
<script src="/static/app-shell.js"></script>
```

Se inyectan de forma **síncrona** al parsear, no en `DOMContentLoaded`:
`configureSidebarVisibilities()` busca los enlaces por id y necesita que ya
existan. Por eso el `<script>` va en el `<body>`, no al final.

- El enlace activo sale de `document.body.dataset.page`. Sin `data-page` no se
  marca ninguno.
- **Qué ve cada quien lo decide el rol**, no el marcado — por eso el menú puede
  ser idéntico en todas las páginas. La lógica está en
  `configureSidebarVisibilities()` (`app.js`), que además hace el control de
  acceso de la página: **al añadir una página con `data-page` hay que darle su
  rama ahí**, o rebotará a quien sí tiene permiso.
- Al añadir un enlace, se añade en `SHELL_SECCIONES` y en ningún HTML.

### Colores de datos (tokens `--viz-*`)
La paleta de gráficos vive en `styles.css` como tokens `--viz-1` … `--viz-8`,
con su equivalente para `body.dark-mode`. `viz-tokens.js` los lee en runtime y
los entrega a Chart.js.

- **No escribas colores de serie en JS**: usa `vizSeries()`.
- El **orden** de los slots es el mecanismo de seguridad para daltonismo (los
  pares adyacentes están validados). No se reordena ni se cicla: una novena
  serie va a "Otros".
- Series únicas (una sola barra o línea) usan siempre el slot 1. Un hue distinto
  por gráfico sugiere una diferencia que no existe.
- El color pertenece al dato, no al marco: las tarjetas de gráfico llevan
  `.ds-chart-card`, que es neutro.
- Un `<canvas>` no se repinta solo al cambiar de tema. Por eso `app-theme.js`
  emite `ds:theme-change` y `admin-charts.js` lo escucha.
- Con `maintainAspectRatio:false` la altura la pone el contenedor: envuelve el
  canvas en `.ds-chart-box`.

### Movimiento
La capa de animación vive al final de `styles.css`. Duraciones cortas
(120–260 ms) y salidas suaves; el movimiento explica de dónde viene el
contenido, no decora. Se apaga por dos vías independientes: el interruptor de la
app (`body.ds-no-anim`) y `prefers-reduced-motion` del sistema.

### Búsqueda Pública (FTS)
- Usa `plainto_tsquery('spanish', term)` + `ts_rank_cd()` para ranking
- Fallback a `unaccent(ILIKE)` cuando no hay letras en el término
- GIN indexes: `idx_datos_archivo_fts`, `idx_empleados_nombre_fts`
- Respuesta paginada: `{records: [...], total: N, page: P, per_page: N}`

### Backup System
- `GET /api/admin/backup/export` — JSON de todas las tablas
- `POST /api/admin/backup/restore?mode=merge|overwrite`
- Solo para el admin Global (ambos módulos)

### Compartición externa (`share.py`)
La comparativa de mercado marcaba esta función como ausente frente a Alfresco,
SharePoint y Nextcloud. Un enlace `/compartido/<token>` deja consultar **un**
documento sin sesión.

- **Sin tabla**: el token es HMAC firmado con `SECRET_KEY` y lleva dentro módulo,
  id y caducidad. No hay estado que mantener ni limpiar.
- Sólo lectura, y sólo de ese documento. No da acceso a la búsqueda.
- Un documento en papelera deja de verse aunque el enlace siga vigente.
- El `file_url` interno no se expone: se sirve por la propia ruta del enlace,
  para que el token siga siendo la única llave.
- Crear el enlace y cada consulta quedan en auditoría — compartir hacia fuera es
  precisamente lo que un archivo necesita poder rastrear.

## Terminología (UI)
- **NUNCA usar "Tesauro"** → siempre "Palabras Clave"
- Las 4 Partes de RRHH: Parte I (Ingreso), II (Escalafón), III (Permisos), IV (Documentos Personales)
- "Clasificación" = `tesauro_secundario` en Archivo

## Variables de Entorno Requeridas (Vercel)
```
DATABASE_URL      # Neon PostgreSQL connection string
SECRET_KEY        # Para firmar sesiones (opcional pero recomendado en prod)

R2_ENDPOINT       # https://<account_id>.r2.cloudflarestorage.com
R2_ACCESS_KEY     # Access Key ID del token de API de R2
R2_SECRET_KEY     # Secret Access Key del token de API de R2
R2_BUCKET         # Nombre del bucket

OPENROUTER_API_KEY  # Sin esto, la burbuja del asistente no aparece
```
`.env.example` lleva la lista completa y comentada. **Ninguna credencial se
escribe en el código**: `test_secrets.py` lo verifica en cada corrida.

## Convenciones de Desarrollo
- Sin comentarios obvios — solo comentar el "por qué" no el "qué"
- Toast system en lugar de `alert()` — `showToast(msg, type)` donde type = success|error|warning|info
- Siempre usar `db_query()`, nunca abrir conexiones directas
- Validar en el borde del sistema (input del usuario) — no validar datos internos que ya son correctos
- `log_event(usuario, evento, modulo, detalle)` para auditoría en endpoints importantes
- Los archivos se guardan en **UTF-8 sin BOM**. Cuidado con los editores que
  "embellecen" las comillas: una comilla tipográfica como delimitador de string
  no es un detalle estético, es un `SyntaxError` que tumba el archivo entero.
- Toda página que quiera tema y modo oscuro necesita `styles.css` **y**
  `app-theme.js`. Este último se auto-arranca.
- Si una tabla puede no caber, envuélvela en `.table-responsive`. Si una columna
  se oculta en móvil, la clase `ds-hide-sm`/`ds-hide-xs` va en el `<th>` **y** en
  el `<td>`: si solo va en uno, la fila se desalinea.

### Antes de dar algo por terminado
El backend tiene pruebas; el frontend no se prueba solo mirando el código. Los
fallos de esta clase — tabla corrida, panel que abre vacío, desborde horizontal —
solo aparecen al renderizar. Levanta la página con datos representativos, mírala
a 390 px y en modo oscuro, y revisa la consola.
