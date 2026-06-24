# Archivo Institucional Digital — Facultad de Ciencias, UCV

Sistema de gestión documental e inventario de RRHH para la Facultad de Ciencias de la Universidad Central de Venezuela.

## Stack Técnico

- **Backend**: Python 3.11 + FastAPI 0.110
- **Base de datos**: PostgreSQL (Neon serverless) via psycopg2
- **Frontend**: Vanilla JS + AdminLTE 3 + Bootstrap 4.6 + FontAwesome 5
- **Despliegue**: **Vercel** (NO Render) — `api/index.py` usa `mangum` como adaptador ASGI
- **Charts**: Chart.js 4.4 via CDN

## Estructura de Directorios

```
/
├── api/
│   ├── index.py          # Punto de entrada Vercel (mangum wrapper)
│   └── requirements.txt  # Dependencias para Vercel
├── python-app/
│   ├── main.py           # App FastAPI, run_migrations(), include_router()
│   ├── database.py       # ThreadedConnectionPool + db_query() helper
│   ├── models.py         # Pydantic models para todos los endpoints
│   ├── schema.sql        # Esquema SQL de referencia (NO modificar)
│   ├── core/
│   │   ├── config.py     # Settings class con variables de entorno
│   │   ├── security.py   # hash_password(), verify_password()
│   │   └── cache.py      # TTLCache para choices
│   ├── routes/
│   │   ├── admin.py      # CRUD admin, users, audit, charts, import CSV
│   │   ├── archivo.py    # Búsqueda y CRUD de Archivo institucional
│   │   ├── auth.py       # Login, restore session, password change
│   │   ├── backup.py     # Export/restore/history de backups
│   │   ├── choices.py    # Datos para dropdowns (con cache TTL 300s)
│   │   ├── pages.py      # Serve HTML pages
│   │   └── rrhh.py       # Búsqueda y CRUD de RRHH + report PDF
│   ├── static/
│   │   ├── index.html    # SPA principal (tabs Archivo + RRHH)
│   │   ├── app.js        # Estado global, sesión, helpers compartidos
│   │   ├── admin.js      # Toda la lógica de admin panels (~1000+ líneas)
│   │   ├── archivo.js    # Búsqueda pública Archivo
│   │   ├── rrhh.js       # Búsqueda pública RRHH + dossier
│   │   ├── admin_archivo.html  # Admin panel Archivo
│   │   ├── admin_rrhh.html    # Admin panel RRHH
│   │   ├── admin_sistema.html # Admin Global (backup, audit, stats)
│   │   ├── archivo.html  # Página pública Archivo
│   │   ├── rrhh.html     # Página pública RRHH
│   │   └── login.html    # Página de login
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
- `datos_rrhh` — documentos por empleado (id_empleado FK, id_tipo_documento FK, personas_relacionadas, notas, fecha_documento, ubicacion, file_url)
- `rrhh_descriptores` — relación (raro, mayormente RRHH no usa descriptores libres)
- `tipo_documento` (scope RRHH) — 4 Partes: parte-i, parte-ii, parte-iii, parte-iv

### Sistema
- `categoria` — categorías con slug (parte-i, parte-ii, parte-iii, parte-iv, archivo)
- `usuarios_sistema` — usuarios con modulo, rol, is_active, last_login
- `audit_log` — eventos del sistema (accion, usuario, modulo, detalle, status, timestamp)
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
En `main.py`, lista de tuplas `(description, sql)`. Se ejecuta en `@app.on_event("startup")`.
- Siempre usar `CREATE TABLE IF NOT EXISTS`, `ADD COLUMN IF NOT EXISTS`, `CREATE INDEX IF NOT EXISTS`
- Nunca usar `DROP` en migraciones
- Para INSERT en tablas de catálogo: `ON CONFLICT (slug) DO NOTHING` o `ON CONFLICT DO NOTHING`

### Caché de choices
`choices.py` tiene cache TTL 300s. Llama `invalidate_choices_cache()` después de modificar categoria o tipo_documento.

### Autenticación
- Sesión guardada en `localStorage` como JSON con TTL 12h
- `/api/auth/restore` valida la sesión en background
- `state.user.modules` es array de strings: `["Archivo"]`, `["RRHH"]`, o `["Archivo","RRHH"]` (Global)
- Global admin = tiene AMBOS módulos

### Admin Panels
- Dos páginas separadas: `admin_archivo.html` y `admin_rrhh.html`
- `adminSuffixFromTab()` retorna "archivo" o "rrhh"
- IDs de elementos HTML tienen sufijo: `#monitor-table-archivo`, `#monitor-table-rrhh`
- `loadAdminTab(tabId)` es el switch principal de tabs en `admin.js`

### Búsqueda Pública (FTS)
- Usa `plainto_tsquery('spanish', term)` + `ts_rank_cd()` para ranking
- Fallback a `unaccent(ILIKE)` cuando no hay letras en el término
- GIN indexes: `idx_datos_archivo_fts`, `idx_empleados_nombre_fts`
- Respuesta paginada: `{records: [...], total: N, page: P, per_page: N}`

### Backup System
- `GET /api/admin/backup/export` — JSON de todas las tablas
- `POST /api/admin/backup/restore?mode=merge|overwrite`
- Solo para el admin Global (ambos módulos)

## Terminología (UI)
- **NUNCA usar "Tesauro"** → siempre "Palabras Clave"
- Las 4 Partes de RRHH: Parte I (Ingreso), II (Escalafón), III (Permisos), IV (Documentos Personales)
- "Clasificación" = `tesauro_secundario` en Archivo

## Variables de Entorno Requeridas (Vercel)
```
DATABASE_URL      # Neon PostgreSQL connection string
SECRET_KEY        # Para firmar sesiones (opcional pero recomendado en prod)
```

## Convenciones de Desarrollo
- Sin comentarios obvios — solo comentar el "por qué" no el "qué"
- Toast system en lugar de `alert()` — `showToast(msg, type)` donde type = success|error|warning|info
- Siempre usar `db_query()`, nunca abrir conexiones directas
- Validar en el borde del sistema (input del usuario) — no validar datos internos que ya son correctos
- `log_event(usuario, evento, modulo, detalle)` para auditoría en endpoints importantes
