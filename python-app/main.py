import os

from fastapi import FastAPI, Request, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from fastapi.staticfiles import StaticFiles

from database import ensure_audit_table, db_query, logger
from utils import populate_missing_slugs, generate_unique_slug
from routes.auth         import router as auth_router
from routes.archivo      import router as archivo_router
from routes.rrhh         import router as rrhh_router
from routes.rrhh_alertas import router as rrhh_alertas_router
from routes.admin        import router as admin_router
from routes.choices      import router as choices_router
from routes.pages        import router as pages_router
from routes.backup       import router as backup_router
from routes.files        import router as files_router
from routes.papelera     import router as papelera_router
from routes.ia           import router as ia_router

# =============================================================================
# APLICACION
# =============================================================================

app = FastAPI(
    title="Archivo Institucional Digital — Facultad de Ciencias, UCV",
    version="3.2.1-Neon",
    description="""
Sistema de gestión documental e inventario de RRHH de la Facultad de Ciencias,
Universidad Central de Venezuela.

**Estándares aplicados:**
- ISAD(G): Norma Internacional General de Descripción Archivística
- ISO 15489-1:2016: Gestión de Documentos — Conceptos y principios
- LOTTT (Venezuela): Campos de empleados conforme a legislación laboral vigente
- Dublin Core Metadata Initiative (DC 1.1): Metadatos de documentos

**Módulos:**
- `/api/archivo` — Búsqueda del Archivo Institucional
- `/api/rrhh` — Búsqueda de Expedientes de RRHH
- `/api/admin` — Gestión, estadísticas, usuarios, backup, retención
- `/api/auth` — Autenticación y sesiones
- `/api/choices` — Catálogos para formularios (cacheado 5 min)
""",
    contact={
        "name": "Facultad de Ciencias — UCV",
        "url": "https://www.ciens.ucv.ve",
    },
    license_info={
        "name": "Uso Institucional — Facultad de Ciencias UCV",
    },
)

# CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

@app.middleware("http")
async def add_no_cache_header(request, call_next):
    response = await call_next(request)
    path = request.url.path
    # Assets estáticos (imágenes, fuentes, íconos) pueden cachearse; JS/CSS/HTML no.
    if path.startswith("/static/") and not path.endswith((".js", ".css", ".html")):
        response.headers["Cache-Control"] = "public, max-age=86400, immutable"
    elif path.startswith("/api/"):
        response.headers["Cache-Control"] = "no-store, no-cache, must-revalidate, max-age=0"
    else:
        response.headers["Cache-Control"] = "no-cache, must-revalidate, max-age=0"
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
                TO_CHAR(e.fecha_nacimiento, 'YYYY-MM-DD')       AS fecha_nacimiento,
                COALESCE(e.nivel_educativo, '')                  AS nivel_educativo,
                COALESCE(e.sexo, '')                             AS sexo,
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
        ("FK fk_datos_rrhh_tipo", """
            DO $$ BEGIN
                IF NOT EXISTS (
                    SELECT 1 FROM pg_constraint WHERE conname = 'fk_datos_rrhh_tipo'
                ) THEN
                    ALTER TABLE public.datos_rrhh
                    ADD CONSTRAINT fk_datos_rrhh_tipo
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

        # ── ISAD(G) / ISO 15489: enriquecimiento documental ──────────────────
        ("numero_folio en datos_archivo",
         "ALTER TABLE public.datos_archivo ADD COLUMN IF NOT EXISTS numero_folio VARCHAR(50)"),
        ("soporte en datos_archivo",
         "ALTER TABLE public.datos_archivo ADD COLUMN IF NOT EXISTS soporte VARCHAR(20) DEFAULT 'Físico'"),
        ("numero_paginas en datos_archivo",
         "ALTER TABLE public.datos_archivo ADD COLUMN IF NOT EXISTS numero_paginas INTEGER"),
        ("idioma en datos_archivo",
         "ALTER TABLE public.datos_archivo ADD COLUMN IF NOT EXISTS idioma VARCHAR(10) DEFAULT 'es'"),
        ("numero_folio en datos_rrhh",
         "ALTER TABLE public.datos_rrhh ADD COLUMN IF NOT EXISTS numero_folio VARCHAR(50)"),
        ("soporte en datos_rrhh",
         "ALTER TABLE public.datos_rrhh ADD COLUMN IF NOT EXISTS soporte VARCHAR(20) DEFAULT 'Físico'"),
        ("numero_paginas en datos_rrhh",
         "ALTER TABLE public.datos_rrhh ADD COLUMN IF NOT EXISTS numero_paginas INTEGER"),
        ("plazo_retencion_anios en tipo_documento",
         "ALTER TABLE public.tipo_documento ADD COLUMN IF NOT EXISTS plazo_retencion_anios INTEGER DEFAULT 5"),
        ("idx soporte datos_archivo",
         "CREATE INDEX IF NOT EXISTS idx_datos_archivo_soporte ON public.datos_archivo(soporte)"),

        # ── LOTTT Venezuela: campos empleados ─────────────────────────────────
        ("fecha_nacimiento en empleados",
         "ALTER TABLE public.empleados ADD COLUMN IF NOT EXISTS fecha_nacimiento DATE"),
        ("nivel_educativo en empleados",
         "ALTER TABLE public.empleados ADD COLUMN IF NOT EXISTS nivel_educativo VARCHAR(60)"),
        ("sexo en empleados",
         "ALTER TABLE public.empleados ADD COLUMN IF NOT EXISTS sexo CHAR(1)"),
        ("updated_at en empleados",
         "ALTER TABLE public.empleados ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP"),
        ("updated_by en empleados",
         "ALTER TABLE public.empleados ADD COLUMN IF NOT EXISTS updated_by TEXT"),

        # ── Historial de cargos: habilitar tabla del schema ───────────────────
        ("historial_cargos tabla",
         """CREATE TABLE IF NOT EXISTS public.historial_cargos (
             id           INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
             empleado_id  INTEGER NOT NULL REFERENCES public.empleados(id) ON DELETE CASCADE,
             cargo_id     INTEGER NOT NULL REFERENCES public.cargos(id),
             fecha_inicio DATE NOT NULL,
             fecha_fin    DATE,
             motivo       TEXT,
             registrado_por TEXT,
             created_at   TIMESTAMPTZ NOT NULL DEFAULT NOW()
         )"""),
        ("idx historial_cargos_empleado",
         "CREATE INDEX IF NOT EXISTS idx_historial_cargos_emp ON public.historial_cargos(empleado_id)"),

        # ── Versioning de archivos digitales ──────────────────────────────────
        ("tabla documento_versiones",
         """CREATE TABLE IF NOT EXISTS public.documento_versiones (
             id             INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
             tabla          TEXT NOT NULL,
             documento_id   INTEGER NOT NULL,
             version_num    INTEGER NOT NULL DEFAULT 1,
             file_url       TEXT,
             file_key       TEXT,
             comentario     TEXT,
             subido_por     TEXT,
             created_at     TIMESTAMPTZ NOT NULL DEFAULT NOW()
         )"""),
        ("idx versiones_documento",
         "CREATE INDEX IF NOT EXISTS idx_doc_versiones ON public.documento_versiones(tabla, documento_id)"),

        # ── Soft-delete: papelera de reciclaje ────────────────────────────────
        ("deleted_at en datos_archivo",
         "ALTER TABLE public.datos_archivo ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ"),
        ("deleted_by en datos_archivo",
         "ALTER TABLE public.datos_archivo ADD COLUMN IF NOT EXISTS deleted_by TEXT"),
        ("deleted_at en datos_rrhh",
         "ALTER TABLE public.datos_rrhh ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ"),
        ("deleted_by en datos_rrhh",
         "ALTER TABLE public.datos_rrhh ADD COLUMN IF NOT EXISTS deleted_by TEXT"),
        ("deleted_at en empleados",
         "ALTER TABLE public.empleados ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ"),
        ("deleted_by en empleados",
         "ALTER TABLE public.empleados ADD COLUMN IF NOT EXISTS deleted_by TEXT"),
        ("idx deleted_at datos_archivo",
         "CREATE INDEX IF NOT EXISTS idx_datos_archivo_deleted ON public.datos_archivo(deleted_at) WHERE deleted_at IS NULL"),
        ("idx deleted_at datos_rrhh",
         "CREATE INDEX IF NOT EXISTS idx_datos_rrhh_deleted ON public.datos_rrhh(deleted_at) WHERE deleted_at IS NULL"),
        ("idx deleted_at empleados",
         "CREATE INDEX IF NOT EXISTS idx_empleados_deleted ON public.empleados(deleted_at) WHERE deleted_at IS NULL"),

        # ── Actualizar vista para excluir soft-deleted ────────────────────────
        ("VIEW vw_rrhh_persona_index v2 (excluye soft-deleted)", """
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
                TO_CHAR(e.fecha_nacimiento, 'YYYY-MM-DD')       AS fecha_nacimiento,
                COALESCE(e.nivel_educativo, '')                  AS nivel_educativo,
                COALESCE(e.sexo, '')                             AS sexo,
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
            LEFT JOIN public.datos_rrhh        dr ON dr.empleado_id    = e.id AND dr.deleted_at IS NULL
            LEFT JOIN public.tipo_documento    td ON dr.id_tipo_documento = td.id
            WHERE e.deleted_at IS NULL
            GROUP BY e.id, c.nombre, d.nombre, el.estados
        """),

        # ── Fecha de vencimiento explícita por documento ──────────────────────
        ("fecha_vencimiento en datos_archivo",
         "ALTER TABLE public.datos_archivo ADD COLUMN IF NOT EXISTS fecha_vencimiento DATE"),
        ("fecha_vencimiento en datos_rrhh",
         "ALTER TABLE public.datos_rrhh ADD COLUMN IF NOT EXISTS fecha_vencimiento DATE"),
        ("idx fecha_vencimiento datos_archivo",
         "CREATE INDEX IF NOT EXISTS idx_datos_archivo_vencimiento ON public.datos_archivo(fecha_vencimiento) WHERE fecha_vencimiento IS NOT NULL AND deleted_at IS NULL"),
        ("idx fecha_vencimiento datos_rrhh",
         "CREATE INDEX IF NOT EXISTS idx_datos_rrhh_vencimiento ON public.datos_rrhh(fecha_vencimiento) WHERE fecha_vencimiento IS NOT NULL AND deleted_at IS NULL"),

        # ── Índices de búsqueda en datos_rrhh ─────────────────────────────────
        ("idx datos_rrhh notas trgm",
         "CREATE INDEX IF NOT EXISTS idx_datos_rrhh_notas ON public.datos_rrhh USING gin(to_tsvector('spanish', COALESCE(notas,'') || ' ' || COALESCE(titulo,''))) WHERE deleted_at IS NULL"),
        ("idx datos_rrhh titulo gin",
         "CREATE INDEX IF NOT EXISTS idx_datos_rrhh_titulo ON public.datos_rrhh(LOWER(titulo)) WHERE deleted_at IS NULL"),
        ("idx datos_rrhh id_tipo",
         "CREATE INDEX IF NOT EXISTS idx_datos_rrhh_tipo_doc ON public.datos_rrhh(id_tipo_documento) WHERE deleted_at IS NULL"),

        # ── Tipo "Sin clasificar" de emergencia para documentos RRHH huérfanos ─
        ("tipo Sin clasificar RRHH",
         """INSERT INTO public.tipo_documento (nombre, nombre_corto, slug, id_categoria)
            SELECT 'Sin clasificar', 'Sin clasificar', 'sin-clasificar-rrhh',
                   (SELECT id FROM public.categoria WHERE slug = 'parte-iv' LIMIT 1)
            WHERE NOT EXISTS (SELECT 1 FROM public.tipo_documento WHERE slug = 'sin-clasificar-rrhh')
              AND EXISTS (SELECT 1 FROM public.categoria WHERE slug = 'parte-iv')"""),

        # ── Asistente IA ──────────────────────────────────────────────────────
        # Guardar las conversaciones NO hace que el modelo "aprenda solo": no reentrena nada.
        # Lo que compra es que NOSOTROS veamos dónde falla —sobre todo cuando contesta "no
        # encontré nada"— y con eso completemos el conocimiento institucional o ajustemos el
        # prompt. El aprendizaje lo hace una persona leyendo esto. Y sirve para lo que se
        # siente enseguida: ver el gasto real y auditar qué herramienta llamó el asistente.
        ("tabla ia_conversaciones", """
            CREATE TABLE IF NOT EXISTS public.ia_conversaciones (
                id         SERIAL PRIMARY KEY,
                canal      VARCHAR(20)   NOT NULL DEFAULT 'web',
                modo       VARCHAR(20)   NOT NULL DEFAULT 'publico',  -- staff | publico
                usuario    VARCHAR(50)   NULL,      -- solo en modo staff
                titulo     VARCHAR(160)  NULL,      -- la primera pregunta, para la lista
                modelo     VARCHAR(120)  NULL,
                -- Contadores acumulados: la pantalla que lista conversaciones los necesita en
                -- cada fila. Sin esto, listar 50 conversaciones son 50 sumas.
                mensajes   INTEGER       NOT NULL DEFAULT 0,
                tokens     INTEGER       NOT NULL DEFAULT 0,
                -- 6 decimales porque una respuesta cuesta del orden de $0.002: con 2 sería 0.
                costo      NUMERIC(12,6) NOT NULL DEFAULT 0,
                ultima_at  TIMESTAMP     NULL,
                created_at TIMESTAMP     NULL
            )"""),
        ("tabla ia_mensajes", """
            CREATE TABLE IF NOT EXISTS public.ia_mensajes (
                id              SERIAL PRIMARY KEY,
                conversacion_id INTEGER NOT NULL
                    REFERENCES public.ia_conversaciones(id) ON DELETE CASCADE,
                rol          VARCHAR(20)   NOT NULL,   -- user | assistant
                contenido    TEXT          NOT NULL,
                -- Qué herramientas llamó y con qué argumentos (JSON). Es la auditoría: sin
                -- esto no se puede saber si una respuesta salió del archivo o se la inventó.
                herramientas TEXT          NULL,
                modelo       VARCHAR(120)  NULL,
                tokens       INTEGER       NULL,
                costo        NUMERIC(12,6) NULL,
                ms           INTEGER       NULL,
                created_at   TIMESTAMP     NULL
            )"""),
        ("idx ia_conversaciones",
         "CREATE INDEX IF NOT EXISTS idx_ia_conv_ultima ON public.ia_conversaciones(ultima_at DESC)"),
        ("idx ia_conversaciones usuario",
         "CREATE INDEX IF NOT EXISTS idx_ia_conv_usuario ON public.ia_conversaciones(usuario)"),
        ("idx ia_mensajes conv",
         "CREATE INDEX IF NOT EXISTS idx_ia_msj_conv ON public.ia_mensajes(conversacion_id, id)"),
        # El tope de gasto diario se calcula sumando el costo de hoy en cada mensaje enviado:
        # sin este índice esa suma recorre la tabla entera y se paga en cada pregunta.
        ("idx ia_mensajes fecha",
         "CREATE INDEX IF NOT EXISTS idx_ia_msj_fecha ON public.ia_mensajes((created_at::date))"),
        # Identidad, tono, conocimiento institucional y modelo: editables desde el panel.
        # Viven en la base y no en variables de entorno porque cambiar cómo se presenta el
        # asistente no debería requerir un redespliegue ni un desarrollador.
        ("tabla ia_config", """
            CREATE TABLE IF NOT EXISTS public.ia_config (
                clave      VARCHAR(40) PRIMARY KEY,
                valor      TEXT        NULL,
                updated_at TIMESTAMP   NULL,
                updated_by VARCHAR(50) NULL
            )"""),

        # ── Bandeja de aprobación ─────────────────────────────────────────────
        # El asistente no escribe en el archivo: propone. Cada propuesta espera aquí a que
        # una persona la apruebe. `datos` lleva los campos a aplicar Y el estado anterior,
        # porque quien aprueba necesita ver de qué a qué cambia — un "titulo -> X" suelto
        # no se puede juzgar. Y al quedar la fila, el archivo puede demostrar quién aprobó
        # cada cambio y cuándo, que es justo lo que un archivo institucional debe poder
        # demostrar.
        ("tabla ia_propuestas", """
            CREATE TABLE IF NOT EXISTS public.ia_propuestas (
                id              SERIAL PRIMARY KEY,
                conversacion_id INTEGER NULL
                    REFERENCES public.ia_conversaciones(id) ON DELETE SET NULL,
                usuario     VARCHAR(50)  NULL,      -- quién lo pidió
                accion      VARCHAR(30)  NOT NULL,  -- crear | actualizar | palabras_clave
                modulo      VARCHAR(20)  NOT NULL,  -- archivo | rrhh
                objetivo_id INTEGER      NULL,      -- NULL cuando la acción es crear
                datos       TEXT         NOT NULL,  -- JSON: campos a aplicar + estado anterior
                resumen     VARCHAR(400) NULL,      -- lo que se le muestra a quien aprueba
                estado      VARCHAR(20)  NOT NULL DEFAULT 'pendiente',
                resuelto_por VARCHAR(50) NULL,
                resuelto_at  TIMESTAMP   NULL,
                created_at   TIMESTAMP   NULL
            )"""),
        ("idx ia_propuestas pendientes",
         "CREATE INDEX IF NOT EXISTS idx_ia_prop_pend ON public.ia_propuestas(estado, id DESC)"),
        ("idx ia_propuestas conv",
         "CREATE INDEX IF NOT EXISTS idx_ia_prop_conv ON public.ia_propuestas(conversacion_id)"),

        # ── Archivos que el usuario suelta en el chat ─────────────────────────
        # Suben a R2 como cualquier digitalizado, pero todavía no cuelgan de ningún
        # documento: engancharlos exige una propuesta aprobada. Se anclan a la conversación
        # a propósito — es lo que impide que el asistente alcance el adjunto de otro hilo
        # adivinando un id.
        ("tabla ia_adjuntos", """
            CREATE TABLE IF NOT EXISTS public.ia_adjuntos (
                id              SERIAL PRIMARY KEY,
                conversacion_id INTEGER NOT NULL
                    REFERENCES public.ia_conversaciones(id) ON DELETE CASCADE,
                usuario        VARCHAR(50)  NULL,
                nombre_archivo VARCHAR(200) NOT NULL,
                file_url       TEXT         NOT NULL,
                tamano_bytes   INTEGER      NULL,
                created_at     TIMESTAMP    NULL
            )"""),
        ("idx ia_adjuntos conv",
         "CREATE INDEX IF NOT EXISTS idx_ia_adj_conv ON public.ia_adjuntos(conversacion_id, id DESC)"),
    ]
    t_total = _time.time()
    slow_threshold_ms = 500
    for label, sql in migrations:
        try:
            t0 = _time.time()
            db_query(sql.strip(), fetch="none", commit=True)
            elapsed = round((_time.time() - t0) * 1000)
            if elapsed > slow_threshold_ms:
                logger.warning(f"Migración LENTA ({elapsed}ms): {label}")
            else:
                logger.info(f"Migración OK ({elapsed}ms): {label}")
        except Exception as e:
            logger.warning(f"Migración omitida ({label}): {e}")

    total_ms = round((_time.time() - t_total) * 1000)
    logger.info(f"Migrations completadas en {total_ms}ms ({len(migrations)} pasos)")

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
        logger.info("Backfill id_tipo_documento Archivo OK")
    except Exception as exc:
        logger.warning(f"Backfill id_tipo_documento Archivo omitido: {exc}")


def _backfill_rrhh_tipo_fk():
    """Rellena id_tipo_documento en datos_rrhh donde el valor actual sea inválido (no existe en tipo_documento)."""
    try:
        fallback = db_query(
            "SELECT id FROM public.tipo_documento WHERE slug = 'sin-clasificar-rrhh' LIMIT 1",
            fetch="one",
        )
        if not fallback:
            return
        db_query(
            """UPDATE public.datos_rrhh dr
               SET id_tipo_documento = %s
               WHERE NOT EXISTS (
                 SELECT 1 FROM public.tipo_documento td WHERE td.id = dr.id_tipo_documento
               )""",
            [fallback["id"]], fetch="none", commit=True,
        )
        logger.info("Backfill id_tipo_documento RRHH OK")
    except Exception as exc:
        logger.warning(f"Backfill id_tipo_documento RRHH omitido: {exc}")


@app.on_event("startup")
def on_startup():
    ensure_audit_table()
    populate_missing_slugs()
    run_migrations()
    _backfill_rrhh_tipo_fk()


# =============================================================================
# ROUTERS
# =============================================================================

app.include_router(auth_router)
app.include_router(archivo_router)
app.include_router(rrhh_router)
app.include_router(rrhh_alertas_router)
app.include_router(admin_router)
app.include_router(choices_router)
app.include_router(pages_router)
app.include_router(backup_router, prefix="/api/admin/backup", tags=["backup"])
app.include_router(files_router)
app.include_router(papelera_router)
app.include_router(ia_router)


# =============================================================================
# HEALTH CHECK
# =============================================================================

@app.get("/api/health", tags=["system"])
def health_check():
    """Endpoint de salud para monitoreo básico."""
    from core.config import settings
    try:
        counts = db_query(
            """SELECT
                (SELECT COUNT(*) FROM public.datos_archivo)    AS archivo,
                (SELECT COUNT(*) FROM public.datos_rrhh)       AS docs_rrhh,
                (SELECT COUNT(*) FROM public.empleados)        AS empleados,
                (SELECT COUNT(*) FROM public.historial_cargos) AS historial_cargos,
                (SELECT COUNT(*) FROM public.descriptores_libres) AS palabras_clave,
                (SELECT COUNT(*) FROM public.usuarios_sistema) AS usuarios""",
            fetch="one",
        )
        db_ok = counts is not None
    except Exception:
        db_ok = False
        counts = None
    return {
        "status": "ok" if db_ok else "degraded",
        "db": "connected" if db_ok else "error",
        "version": settings.app_version,
        "counts": {k: int(v) for k, v in dict(counts).items()} if db_ok and counts else None,
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
