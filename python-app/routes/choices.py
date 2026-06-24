import time
from datetime import datetime

from fastapi import APIRouter
import pandas as pd

from database import db_query, split_terms
from .archivo import fetch_archivo_dataframe
from .rrhh import fetch_rrhh_dataframe

router = APIRouter(tags=["choices"])

_cache: dict = {}
_cache_ts: float = 0.0
_TTL = 300.0  # 5 minutos


def invalidate_choices_cache() -> None:
    global _cache
    _cache = {}


@router.get("/api/choices")
def get_choices():
    """Retorna las opciones disponibles para los filtros del UI. Cacheado 5 min."""
    global _cache, _cache_ts
    if _cache and time.monotonic() - _cache_ts < _TTL:
        return _cache

    df_arch = fetch_archivo_dataframe()
    df_rh   = fetch_rrhh_dataframe()

    # ── Archivo ───────────────────────────────────────────────────────────────
    arch_doc_types = sorted([x for x in df_arch["doc_type"].dropna().unique().tolist() if str(x).strip()])

    tesauro_set: set = set()
    for col in ["doc_type", "tesauro_primario", "tesauro_secundario", "descriptores_libres"]:
        if col in df_arch.columns:
            for val in df_arch[col].dropna():
                tesauro_set.update(split_terms(val))
    arch_tesauro = sorted(tesauro_set)

    arch_dates = (
        pd.to_datetime(df_arch["fecha"], errors="coerce").dropna()
        if "fecha" in df_arch.columns else pd.Series([], dtype="datetime64[ns]")
    )
    min_arch = arch_dates.min().strftime("%Y-%m-%d") if not arch_dates.empty else "2000-01-01"
    max_arch = arch_dates.max().strftime("%Y-%m-%d") if not arch_dates.empty else datetime.now().strftime("%Y-%m-%d")

    # ── RRHH ──────────────────────────────────────────────────────────────────
    rh_doc_types = sorted([x for x in df_rh["doc_type"].dropna().unique().tolist() if str(x).strip()])
    rh_estados   = sorted([x for x in df_rh["estado"].dropna().unique().tolist()   if str(x).strip()])

    people_set: set = set()
    if "empleado" in df_rh.columns:
        people_set.update([str(x).strip() for x in df_rh["empleado"].dropna() if str(x).strip()])
    rh_people = sorted(people_set)

    rh_dates = (
        pd.to_datetime(df_rh["fecha_ingreso"], errors="coerce").dropna()
        if "fecha_ingreso" in df_rh.columns else pd.Series([], dtype="datetime64[ns]")
    )
    min_rh = rh_dates.min().strftime("%Y-%m-%d") if not rh_dates.empty else "2000-01-01"
    max_rh = rh_dates.max().strftime("%Y-%m-%d") if not rh_dates.empty else datetime.now().strftime("%Y-%m-%d")

    # Tipos de documento para RRHH agrupados por Parte (I, II, III, IV)
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
        p = r["parte"]
        rrhh_tipos_por_parte.setdefault(p, []).append(r["nombre_corto"])

    # Tipos de documento para Archivo (categoría 'archivo')
    arch_tipo_rows = db_query(
        """SELECT td.nombre_corto
           FROM public.tipo_documento td
           JOIN public.categoria c ON td.id_categoria = c.id
           WHERE c.slug = 'archivo'
           ORDER BY td.nombre_corto""",
        fetch="all",
    ) or []
    arch_tipos_catalog = [r["nombre_corto"] for r in arch_tipo_rows] or arch_doc_types

    # Años disponibles para selector de año
    arch_years = sorted({d.year for d in arch_dates}, reverse=True) if not arch_dates.empty else []
    rh_years   = sorted({d.year for d in rh_dates},   reverse=True) if not rh_dates.empty else []

    # Catálogos de RRHH para formularios de nuevo empleado
    departamentos_rows = db_query(
        "SELECT nombre FROM public.departamentos ORDER BY nombre", fetch="all",
    ) or []
    cargos_rows = db_query(
        "SELECT nombre FROM public.cargos ORDER BY nombre", fetch="all",
    ) or []
    estados_rows = db_query(
        "SELECT estados FROM public.estados_laborales ORDER BY estados", fetch="all",
    ) or []

    # Palabras clave del archivo para autocompletar
    keywords_rows = db_query(
        "SELECT nombre FROM public.descriptores_libres ORDER BY nombre LIMIT 500", fetch="all",
    ) or []

    # Tipos de documento con plazos de retención (para admin de retención)
    retencion_rows = db_query(
        """SELECT td.id, td.nombre_corto, COALESCE(td.plazo_retencion_anios, 5) AS plazo,
                  c.slug AS cat_slug
           FROM public.tipo_documento td
           JOIN public.categoria c ON td.id_categoria = c.id
           ORDER BY c.id, td.nombre_corto""",
        fetch="all",
    ) or []

    # Valores estáticos de catálogo (ISAD(G) / Dublin Core)
    soportes = ["Físico", "Digital", "Digitalizado"]
    niveles_educativos = [
        "Bachiller", "TSU", "Universitario", "Especialización",
        "Maestría", "Doctorado", "Postdoctorado"
    ]
    sexos = [
        {"value": "M", "label": "Masculino"},
        {"value": "F", "label": "Femenino"},
        {"value": "O", "label": "Otro / No especificado"},
    ]
    idiomas = [
        {"value": "es", "label": "Español"},
        {"value": "en", "label": "Inglés"},
        {"value": "fr", "label": "Francés"},
        {"value": "pt", "label": "Portugués"},
    ]

    result = {
        "archivo": {
            "doc_types": arch_tipos_catalog,
            "tesauro":   arch_tesauro,
            "min_date":  min_arch,
            "max_date":  max_arch,
            "years":     arch_years,
            "keywords":  [r["nombre"] for r in keywords_rows],
            "soportes":  soportes,
            "idiomas":   idiomas,
        },
        "rrhh": {
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
        },
        "catalogo": {
            "retencion":       [dict(r) for r in retencion_rows],
        },
    }

    _cache    = result
    _cache_ts = time.monotonic()
    return result
