import os

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles

from database import ensure_audit_table, db_query, logger
from utils import populate_missing_slugs
from routes.auth    import router as auth_router
from routes.archivo import router as archivo_router
from routes.rrhh    import router as rrhh_router
from routes.admin   import router as admin_router
from routes.choices import router as choices_router
from routes.pages   import router as pages_router

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
    ]
    for label, sql in migrations:
        try:
            db_query(sql.strip(), fetch="none", commit=True)
            logger.info(f"Migración OK: {label}")
        except Exception as e:
            logger.warning(f"Migración omitida ({label}): {e}")


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
