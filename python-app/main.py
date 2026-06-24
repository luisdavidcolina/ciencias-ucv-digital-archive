import os

from fastapi import FastAPI, Request, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from fastapi.staticfiles import StaticFiles

from database import ensure_audit_table, db_query, logger
from utils import populate_missing_slugs, generate_unique_slug
from routes.auth    import router as auth_router
from routes.archivo import router as archivo_router
from routes.rrhh    import router as rrhh_router
from routes.admin   import router as admin_router
from routes.choices import router as choices_router
from routes.pages   import router as pages_router
from routes.backup  import router as backup_router

# =============================================================================
# APLICACION
# =============================================================================

app = FastAPI(title="Ciencias UCV Digital Archive", version="3.0.0-Neon")

# CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Sin cacheo de estaticos en el cliente
@app.middleware("http")
async def add_no_cache_header(request, call_next):
    response = await call_next(request)
    response.headers["Cache-Control"] = "no-store, no-cache, must-revalidate, max-age=0"
    return response


import time as _time
import logging as _logging

_req_logger = _logging.getLogger("app.requests")

@app.exception_handler(HTTPException)
async def http_exception_handler(request: Request, exc: HTTPException):
    return JSONResponse(
        status_code=exc.status_code,
        content={"detail": exc.detail, "status_code": exc.status_code},
    )


@app.exception_handler(Exception)
async def generic_exception_handler(request: Request, exc: Exception):
    logger.error(f"Unhandled exception on {request.url}: {exc}")
    return JSONResponse(
        status_code=500,
        content={"detail": "Error interno del servidor. Intente nuevamente.", "status_code": 500},
    )


@app.middleware("http")
async def _log_requests(request: Request, call_next):
    t0 = _time.time()
    response = await call_next(request)
    ms = round((_time.time() - t0) * 1000)
    if request.url.path.startswith("/api/"):
        _req_logger.info(f"{request.method} {request.url.path} → {response.status_code} ({ms}ms)")
    return response


# =============================================================================
# STARTUP
# =============================================================================

def run_migrations():
    """Migraciones idempotentes que se ejecutan al iniciar la app."""
    migrations = [
        ("file_url en datos_archivo",
         "ALTER TABLE public.datos_archivo ADD COLUMN IF NOT EXISTS file_url TEXT"),
        ("file_url en datos_rrhh",
         "ALTER TABLE public.datos_rrhh ADD COLUMN IF NOT EXISTS file_url TEXT"),
        ("VIEW vw_rrhh_persona_index", """
            CREATE OR REPLACE VIEW public.vw_rrhh_persona_index AS
            SELECT
                e.id                                            AS empleado_id,
                e.cedula,
                e.rif,
                e.nombres || ' ' || e.apellidos                 AS persona_raw,
                COALESCE(c.nombre,  'Sin cargo asignado')       AS cargo,
                COALESCE(d.nombre,  '')                         AS departamento,
                COALESCE(el.estados,'Sin estado')               AS estado,
                TO_CHAR(e.fecha_ingreso,    'YYYY-MM-DD')       AS fecha_ingreso,
                TO_CHAR(e.fecha_jubilacion, 'YYYY-MM-DD')       AS fecha_jubilacion,
                TO_CHAR(e.fecha_pension,    'YYYY-MM-DD')       AS fecha_pension,
                COALESCE(e.foto_url, '')                        AS foto_url,
                COUNT(dr.id_rrhh)                               AS doc_count,
                COALESCE(
                    STRING_AGG(DISTINCT COALESCE(td.nombre_corto, td.nombre, ''), '; ')
                    FILTER (WHERE td.nombre IS NOT NULL AND td.nombre <> ''),
                    ''
                )                                               AS tipos
            FROM public.empleados e
            LEFT JOIN public.cargos            c  ON e.cargo_id        = c.id
            LEFT JOIN public.departamentos     d  ON e.departamento_id = d.id
            LEFT JOIN public.estados_laborales el ON e.estado_id       = el.id
            LEFT JOIN public.datos_rrhh        dr ON dr.empleado_id    = e.id
            LEFT JOIN public.tipo_documento    td ON dr.id_tipo_documento = td.id
            GROUP BY e.id, c.nombre, d.nombre, el.estados
        """),
        ("categoria Archivo",
         "INSERT INTO public.categoria (nombre, slug) VALUES ('Archivo', 'archivo') ON CONFLICT (slug) DO NOTHING"),
        ("id_tipo_documento en datos_archivo",
         "ALTER TABLE public.datos_archivo ADD COLUMN IF NOT EXISTS id_tipo_documento INTEGER"),
        ("tipo Reincorporacion a labores Docentes",
         """INSERT INTO public.tipo_documento (nombre, nombre_corto, slug, id_categoria)
            SELECT 'Reincorporacion a labores Docentes', 'Reincorporación Labores Docentes',
                   'reincorporacion-labores-docentes',
                   (SELECT id FROM public.categoria WHERE slug = 'parte-i')
            WHERE NOT EXISTS (
                SELECT 1 FROM public.tipo_documento WHERE LOWER(nombre) LIKE '%reincorporaci%labores%'
            )"""),
        ("tipo Rotapama",
         """INSERT INTO public.tipo_documento (nombre, nombre_corto, slug, id_categoria)
            SELECT 'Rotapama', 'Rotapama',
                   'rotapama',
                   (SELECT id FROM public.categoria WHERE slug = 'parte-iv')
            WHERE NOT EXISTS (
                SELECT 1 FROM public.tipo_documento WHERE LOWER(nombre) = 'rotapama'
            )"""),
        ("FK fk_datos_archivo_tipo", """
            DO $$ BEGIN
                IF NOT EXISTS (
                    SELECT 1 FROM pg_constraint WHERE conname = 'fk_datos_archivo_tipo'
                ) THEN
                    ALTER TABLE public.datos_archivo
                    ADD CONSTRAINT fk_datos_archivo_tipo
                    FOREIGN KEY (id_tipo_documento) REFERENCES public.tipo_documento(id) ON DELETE SET NULL;
                END IF;
            END $$
        """),
        ("is_active en usuarios_sistema",
         "ALTER TABLE public.usuarios_sistema ADD COLUMN IF NOT EXISTS is_active BOOLEAN DEFAULT TRUE"),
        ("created_at en usuarios_sistema",
         "ALTER TABLE public.usuarios_sistema ADD COLUMN IF NOT EXISTS created_at TIMESTAMP DEFAULT NOW()"),
        ("last_login en usuarios_sistema",
         "ALTER TABLE public.usuarios_sistema ADD COLUMN IF NOT EXISTS last_login TIMESTAMP"),
        ("updated_at en datos_archivo",
         "ALTER TABLE public.datos_archivo ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP"),
        ("updated_by en datos_archivo",
         "ALTER TABLE public.datos_archivo ADD COLUMN IF NOT EXISTS updated_by INTEGER"),
        ("updated_at en datos_rrhh",
         "ALTER TABLE public.datos_rrhh ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP"),
        ("updated_by en datos_rrhh",
         "ALTER TABLE public.datos_rrhh ADD COLUMN IF NOT EXISTS updated_by INTEGER"),
        ("idx_tipo_doc_nombre_lower",
         "CREATE INDEX IF NOT EXISTS idx_tipo_doc_nombre_lower ON public.tipo_documento (LOWER(nombre))"),
        ("personas_relacionadas en datos_rrhh",
         "ALTER TABLE public.datos_rrhh ADD COLUMN IF NOT EXISTS personas_relacionadas TEXT"),
        ("personas_relacionadas en datos_archivo",
         "ALTER TABLE public.datos_archivo ADD COLUMN IF NOT EXISTS personas_relacionadas TEXT"),
        ("GIN index FTS datos_archivo",
         """CREATE INDEX IF NOT EXISTS idx_datos_archivo_fts
            ON public.datos_archivo
            USING GIN(to_tsvector('spanish',
              coalesce(titulo,'') || ' ' || coalesce(autor,'') || ' ' ||
              coalesce(abstract,'') || ' ' || coalesce(tesauro_primario,'') || ' ' ||
              coalesce(tesauro_secundario,'')))"""),
        ("GIN index FTS vw nombres empleados",
         """CREATE INDEX IF NOT EXISTS idx_empleados_nombre_fts
            ON public.empleados
            USING GIN(to_tsvector('spanish', coalesce(nombres,'') || ' ' || coalesce(apellidos,'')))"""),
        ("Tabla backup_history",
         """CREATE TABLE IF NOT EXISTS public.backup_history (
             id          SERIAL PRIMARY KEY,
             usuario     TEXT NOT NULL DEFAULT 'sistema',
             tipo        TEXT NOT NULL DEFAULT 'export',
             tabla_count INTEGER DEFAULT 0,
             total_rows  INTEGER DEFAULT 0,
             notas       TEXT,
             created_at  TIMESTAMPTZ NOT NULL DEFAULT NOW()
         )"""),
        ("status en datos_archivo",
         "ALTER TABLE public.datos_archivo ADD COLUMN IF NOT EXISTS status TEXT DEFAULT 'aprobado'"),
        ("status en datos_rrhh",
         "ALTER TABLE public.datos_rrhh ADD COLUMN IF NOT EXISTS status TEXT DEFAULT 'aprobado'"),
        ("idx status datos_archivo",
         "CREATE INDEX IF NOT EXISTS idx_datos_archivo_status ON public.datos_archivo(status)"),
    ]
    for label, sql in migrations:
        try:
            db_query(sql.strip(), fetch="none", commit=True)
            logger.info(f"Migración OK: {label}")
        except Exception as e:
            logger.warning(f"Migración omitida ({label}): {e}")

    _migrate_archivo_tipos()
    _backfill_archivo_tipo_fk()


def _migrate_archivo_tipos():
    """Inserta en tipo_documento los tipos de Archivo tomados de datos_archivo.tesauro_primario."""
    arch_cat = db_query("SELECT id FROM public.categoria WHERE slug = 'archivo'", fetch="one")
    if not arch_cat:
        logger.warning("Categoría 'archivo' no existe, saltando migración de tipos.")
        return
    arch_cat_id = arch_cat["id"]

    rows = db_query(
        "SELECT DISTINCT TRIM(tesauro_primario) AS tipo FROM public.datos_archivo "
        "WHERE tesauro_primario IS NOT NULL AND TRIM(tesauro_primario) != ''",
        fetch="all",
    ) or []

    for row in rows:
        tipo_name = str(row["tipo"]).strip()
        if not tipo_name:
            continue
        existing = db_query(
            "SELECT id FROM public.tipo_documento WHERE LOWER(nombre) = LOWER(%s)",
            (tipo_name,), fetch="one",
        )
        if not existing:
            try:
                slug = generate_unique_slug(tipo_name, "tipo_documento")
                db_query(
                    "INSERT INTO public.tipo_documento (nombre, nombre_corto, slug, id_categoria) "
                    "VALUES (%s, %s, %s, %s)",
                    (tipo_name, tipo_name, slug, arch_cat_id),
                    fetch="none", commit=True,
                )
                logger.info(f"Tipo Archivo creado: {tipo_name}")
            except Exception as exc:
                logger.warning(f"Tipo Archivo omitido ({tipo_name}): {exc}")


def _backfill_archivo_tipo_fk():
    """Rellena id_tipo_documento en datos_archivo donde coincida el nombre."""
    try:
        db_query(
            """UPDATE public.datos_archivo da
               SET id_tipo_documento = td.id
               FROM public.tipo_documento td
               WHERE da.id_tipo_documento IS NULL
                 AND LOWER(TRIM(da.tesauro_primario)) = LOWER(TRIM(td.nombre))""",
            fetch="none", commit=True,
        )
        logger.info("Backfill id_tipo_documento OK")
    except Exception as exc:
        logger.warning(f"Backfill id_tipo_documento omitido: {exc}")


@app.on_event("startup")
def on_startup():
    ensure_audit_table()
    populate_missing_slugs()
    run_migrations()


# =============================================================================
# ROUTERS
# =============================================================================

app.include_router(auth_router)
app.include_router(archivo_router)
app.include_router(rrhh_router)
app.include_router(admin_router)
app.include_router(choices_router)
app.include_router(pages_router)
app.include_router(backup_router, prefix="/api/admin/backup", tags=["backup"])


# =============================================================================
# HEALTH CHECK
# =============================================================================

@app.get("/api/health", tags=["system"])
def health_check():
    """Endpoint de salud para monitoreo básico."""
    try:
        row = db_query("SELECT 1 AS ok", fetch="one")
        db_ok = row is not None
    except Exception:
        db_ok = False
    return {
        "status": "ok" if db_ok else "degraded",
        "db": "connected" if db_ok else "error",
        "version": "3.0.0-Neon",
    }


# =============================================================================
# ARCHIVOS ESTATICOS
# =============================================================================

static_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "static")
if os.path.exists(static_path):
    app.mount("/static", StaticFiles(directory=static_path), name="static")
    assets_path = os.path.join(static_path, "assets")
    if os.path.exists(assets_path):
        app.mount("/assets", StaticFiles(directory=assets_path), name="assets")


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="127.0.0.1", port=8000)
