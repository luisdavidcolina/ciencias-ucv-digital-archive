"""Consultas SQL para /api/choices (dropdowns de Archivo, RRHH y Catálogo).

IN-042 paso 1: primer módulo de `app/repos/` — funciones de consulta ya
existentes en `routes/lookups.py`, movidas aquí sin cambiar su comportamiento.
`routes/lookups.py` las importa y las usa; `db_query` sigue siendo el único
ejecutor, ahora invocado desde este módulo en vez de desde el de rutas.
"""

from datetime import datetime

from database import db_query, split_terms


def user_modules(usuario: str) -> list:
    """Módulos activos de `usuario`, con 'Global' expandido a Archivo + RRHH."""
    rows = db_query(
        "SELECT modulo FROM public.usuarios_sistema "
        "WHERE TRIM(usuario) = %s AND is_active = true",
        [usuario],
        fetch="all",
    ) or []
    modules: list = []
    for r in rows:
        mod = str(r.get("modulo", "")).strip()
        if mod and mod not in modules:
            modules.append(mod)
    if "Global" in modules:
        modules = [m for m in modules if m != "Global"]
        if "Archivo" not in modules:
            modules.append("Archivo")
        if "RRHH" not in modules:
            modules.append("RRHH")
    return modules


def build_archivo_choices() -> dict:
    tipo_rows = db_query(
        """SELECT td.nombre_corto
           FROM public.tipo_documento td
           JOIN public.categoria c ON td.id_categoria = c.id
           WHERE c.slug = 'archivo'
           ORDER BY td.nombre_corto""",
        fetch="all",
    ) or []
    arch_tipos_catalog = [r["nombre_corto"] for r in tipo_rows]

    tesauro_set: set = set()
    tesauro_rows = db_query(
        """SELECT DISTINCT tesauro_primario, tesauro_secundario
           FROM public.datos_archivo
           WHERE tesauro_primario IS NOT NULL OR tesauro_secundario IS NOT NULL""",
        fetch="all",
    ) or []
    for r in tesauro_rows:
        tesauro_set.update(split_terms(r.get("tesauro_primario")))
        tesauro_set.update(split_terms(r.get("tesauro_secundario")))

    keywords_rows = db_query(
        "SELECT nombre FROM public.descriptores_libres ORDER BY nombre LIMIT 500",
        fetch="all",
    ) or []
    for r in keywords_rows:
        tesauro_set.update(split_terms(r.get("nombre")))
    arch_tesauro = sorted(tesauro_set)

    if not arch_tipos_catalog:
        arch_tipos_catalog = sorted(
            {r.get("tesauro_primario") for r in tesauro_rows if r.get("tesauro_primario")}
        )

    date_row = db_query(
        """SELECT TO_CHAR(MIN(fecha_documento), 'YYYY-MM-DD') AS min_fecha,
                  TO_CHAR(MAX(fecha_documento), 'YYYY-MM-DD') AS max_fecha
           FROM public.datos_archivo""",
        fetch="one",
    ) or {}
    min_arch = date_row.get("min_fecha") or "2000-01-01"
    max_arch = date_row.get("max_fecha") or datetime.now().strftime("%Y-%m-%d")

    years_rows = db_query(
        """SELECT DISTINCT EXTRACT(YEAR FROM fecha_documento)::int AS anio
           FROM public.datos_archivo
           WHERE fecha_documento IS NOT NULL
           ORDER BY anio DESC""",
        fetch="all",
    ) or []
    arch_years = [r["anio"] for r in years_rows]

    soportes = ["Físico", "Digital", "Digitalizado"]
    idiomas = [
        {"value": "es", "label": "Español"},
        {"value": "en", "label": "Inglés"},
        {"value": "fr", "label": "Francés"},
        {"value": "pt", "label": "Portugués"},
    ]

    return {
        "doc_types": arch_tipos_catalog,
        "tesauro":   arch_tesauro,
        "min_date":  min_arch,
        "max_date":  max_arch,
        "years":     arch_years,
        "keywords":  [r["nombre"] for r in keywords_rows],
        "soportes":  soportes,
        "idiomas":   idiomas,
    }


def build_rrhh_choices() -> dict:
    rrhh_tipos_rows = db_query(
        """SELECT td.nombre_corto, c.nombre AS parte
           FROM public.tipo_documento td
           JOIN public.categoria c ON td.id_categoria = c.id
           WHERE c.slug IN ('parte-i','parte-ii','parte-iii','parte-iv')
           ORDER BY c.id, td.nombre_corto""",
        fetch="all",
    ) or []
    rrhh_tipos_por_parte: dict = {}
    for r in rrhh_tipos_rows:
        rrhh_tipos_por_parte.setdefault(r["parte"], []).append(r["nombre_corto"])

    doc_types_rows = db_query(
        """SELECT DISTINCT COALESCE(td.nombre_corto, td.nombre) AS doc_type
           FROM public.datos_rrhh dr
           JOIN public.tipo_documento td ON dr.id_tipo_documento = td.id
           WHERE COALESCE(td.nombre_corto, td.nombre) IS NOT NULL""",
        fetch="all",
    ) or []
    rh_doc_types = sorted({r["doc_type"] for r in doc_types_rows if r.get("doc_type")})

    estados_rows_dyn = db_query(
        """SELECT DISTINCT el.estados
           FROM public.empleados e
           JOIN public.estados_laborales el ON e.estado_id = el.id
           WHERE el.estados IS NOT NULL""",
        fetch="all",
    ) or []
    rh_estados = sorted({r["estados"] for r in estados_rows_dyn if r.get("estados")})

    people_rows = db_query(
        "SELECT nombres || ' ' || apellidos AS empleado FROM public.empleados",
        fetch="all",
    ) or []
    rh_people = sorted({r["empleado"].strip() for r in people_rows if r.get("empleado") and r["empleado"].strip()})

    date_row = db_query(
        """SELECT TO_CHAR(MIN(fecha_ingreso), 'YYYY-MM-DD') AS min_fecha,
                  TO_CHAR(MAX(fecha_ingreso), 'YYYY-MM-DD') AS max_fecha
           FROM public.empleados""",
        fetch="one",
    ) or {}
    min_rh = date_row.get("min_fecha") or "2000-01-01"
    max_rh = date_row.get("max_fecha") or datetime.now().strftime("%Y-%m-%d")

    years_rows = db_query(
        """SELECT DISTINCT EXTRACT(YEAR FROM fecha_ingreso)::int AS anio
           FROM public.empleados
           WHERE fecha_ingreso IS NOT NULL
           ORDER BY anio DESC""",
        fetch="all",
    ) or []
    rh_years = [r["anio"] for r in years_rows]

    departamentos_rows = db_query(
        "SELECT nombre FROM public.departamentos ORDER BY nombre", fetch="all",
    ) or []
    cargos_rows = db_query(
        "SELECT nombre FROM public.cargos ORDER BY nombre", fetch="all",
    ) or []
    estados_rows = db_query(
        "SELECT estados FROM public.estados_laborales ORDER BY estados", fetch="all",
    ) or []

    niveles_educativos = [
        "Bachiller", "TSU", "Universitario", "Especialización",
        "Maestría", "Doctorado", "Postdoctorado"
    ]
    sexos = [
        {"value": "M", "label": "Masculino"},
        {"value": "F", "label": "Femenino"},
        {"value": "O", "label": "Otro / No especificado"},
    ]
    soportes = ["Físico", "Digital", "Digitalizado"]

    return {
        "doc_types":       rh_doc_types,
        "estados":         rh_estados,
        "people":          rh_people,
        "min_date":        min_rh,
        "max_date":        max_rh,
        "years":           rh_years,
        "tipos_por_parte": rrhh_tipos_por_parte,
        "departamentos":   [r["nombre"] for r in departamentos_rows],
        "cargos":          [r["nombre"] for r in cargos_rows],
        "estados_catalog": [r["estados"] for r in estados_rows],
        "niveles_educativos": niveles_educativos,
        "sexos":           sexos,
        "soportes":        soportes,
    }


def build_catalogo_choices() -> dict:
    retencion_rows = db_query(
        """SELECT td.id, td.nombre_corto, COALESCE(td.plazo_retencion_anios, 5) AS plazo,
                  c.slug AS cat_slug
           FROM public.tipo_documento td
           JOIN public.categoria c ON td.id_categoria = c.id
           ORDER BY c.id, td.nombre_corto""",
        fetch="all",
    ) or []
    return {"retencion": [dict(r) for r in retencion_rows]}


def build_choices() -> dict:
    return {
        "archivo":  build_archivo_choices(),
        "rrhh":     build_rrhh_choices(),
        "catalogo": build_catalogo_choices(),
    }
