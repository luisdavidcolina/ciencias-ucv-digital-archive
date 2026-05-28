from datetime import datetime

from fastapi import APIRouter
import pandas as pd

from database import split_terms
from .archivo import fetch_archivo_dataframe
from .rrhh import fetch_rrhh_dataframe

router = APIRouter(tags=["choices"])


@router.get("/api/choices")
def get_choices():
    """Retorna las opciones disponibles para los filtros del UI (tipologías, tesauro, fechas, etc.)."""
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

    return {
        "archivo": {
            "doc_types": arch_doc_types,
            "tesauro":   arch_tesauro,
            "min_date":  min_arch,
            "max_date":  max_arch,
        },
        "rrhh": {
            "doc_types": rh_doc_types,
            "estados":   rh_estados,
            "people":    rh_people,
            "min_date":  min_rh,
            "max_date":  max_rh,
        },
    }
