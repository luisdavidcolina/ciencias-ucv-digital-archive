from typing import List, Dict, Any

from fastapi import APIRouter, HTTPException
import pandas as pd

from database import db_query, split_terms
from models import RrhhSearchRequest, RrhhProfileRequest

router = APIRouter(prefix="/api/rrhh", tags=["rrhh"])


# =============================================================================
# HELPERS DE NEGOCIO
# =============================================================================

def format_rrhh_person_name(name: str) -> str:
    """Normaliza el nombre de una persona al formato 'Apellido(s), Nombre(s)'."""
    name = (name or "").strip()
    if not name:
        return ""
    if "," in name:
        parts = [p.strip() for p in name.split(",") if p.strip()]
        if len(parts) >= 2:
            return f"{parts[0]}, {' '.join(parts[1:])}"
        return name
    parts = name.split()
    if len(parts) <= 1:
        return name
    if len(parts) == 2:
        return f"{parts[1]}, {parts[0]}"
    surnames = " ".join(parts[-2:])
    given_names = " ".join(parts[:-2])
    return f"{surnames}, {given_names}"


def first_non_empty_value(values) -> str:
    """Retorna el primer valor no vacío de una serie o lista."""
    if values is None:
        return ""
    series = pd.Series(values).dropna().astype(str).str.strip()
    series = series[series != ""]
    return series.iloc[0] if not series.empty else ""


# =============================================================================
# DATAFRAME FETCHER
# =============================================================================

def fetch_rrhh_dataframe(filters_sql: str = "", filter_params=None) -> pd.DataFrame:
    """Retorna el expediente completo de RRHH como DataFrame.

    Args:
        filters_sql:   Cláusula WHERE sin la palabra 'WHERE' (opcional).
        filter_params: Parámetros para la cláusula WHERE.
    """
    base_sql = """
        SELECT
            e.id                                      AS empleado_id,
            e.cedula,
            e.nombres || ' ' || e.apellidos           AS empleado,
            e.rif,
            c.nombre                                  AS cargo,
            d.nombre                                  AS departamento,
            el.estados                                AS estado,
            TO_CHAR(e.fecha_ingreso,    'YYYY-MM-DD') AS fecha_ingreso,
            TO_CHAR(e.fecha_jubilacion, 'YYYY-MM-DD') AS fecha_jubilacion,
            TO_CHAR(e.fecha_pension,    'YYYY-MM-DD') AS fecha_pension,
            COALESCE(e.foto_url, '')                  AS foto_url,
            da.id_archivo,
            da.codigo_documento,
            COALESCE(td.nombre_corto, td.nombre, '') AS doc_type,
            COALESCE(cat.nombre, '')               AS categoria,
            COALESCE(da.ubicacion, '')                AS ubicacion,
            COALESCE(da.titulo, '')                   AS titulo_doc,
            COALESCE(da.autor_ente, '')               AS autor_ente,
            COALESCE(da.abstract, '')                 AS abstract,
            TO_CHAR(da.fecha_documento, 'YYYY-MM-DD') AS fecha_documento,
            e.nombres || ' ' || e.apellidos           AS personas_relacionadas
        FROM public.empleados e
        LEFT JOIN public.cargos            c  ON e.cargo_id        = c.id
        LEFT JOIN public.departamentos     d  ON e.departamento_id = d.id
        LEFT JOIN public.estados_laborales el ON e.estado_id       = el.id
        LEFT JOIN public.datos_archivo     da ON da.empleado_id    = e.id
        LEFT JOIN public.tipo_documento    td ON da.id_tipo_documento = td.id
        LEFT JOIN public.categoria        cat ON td.id_categoria = cat.id
    """
    if filters_sql:
        base_sql += " WHERE " + filters_sql

    rows = db_query(base_sql, filter_params, fetch="all")
    if not rows:
        return pd.DataFrame(columns=[
            "cedula", "empleado", "personas_relacionadas", "departamento", "estado",
            "doc_type", "categoria", "fecha_ingreso", "ubicacion", "foto_url",
            "fecha_jubilacion", "fecha_pension", "rif", "cargo", "id_archivo",
        ])
    return pd.DataFrame([dict(r) for r in rows]).fillna("")


# =============================================================================
# ÍNDICE DE PERSONAS
# =============================================================================

def build_rrhh_person_index(df: pd.DataFrame) -> List[Dict[str, Any]]:
    """Agrega el DataFrame por persona y retorna una lista de perfiles."""
    if df.empty:
        return []

    all_persons: set = set()
    for _, row in df.iterrows():
        emp = str(row.get("empleado", "")).strip()
        if emp:
            all_persons.add(emp)
        all_persons.update(split_terms(str(row.get("personas_relacionadas", ""))))

    profiles = []
    for p in sorted(all_persons):
        p_rows, p_indices = [], []
        for idx, row in df.iterrows():
            emp = str(row.get("empleado", "")).strip()
            rel = split_terms(str(row.get("personas_relacionadas", "")))
            if p == emp or p in rel:
                p_rows.append(row)
                p_indices.append(idx)
        if not p_rows:
            continue

        p_df = pd.DataFrame(p_rows)
        doc_count = (
            int((p_df["id_archivo"].astype(str).str.strip() != "").sum())
            if "id_archivo" in p_df.columns else len(p_df)
        )
        primary_rows = p_df[p_df["empleado"] == p]

        cedula = str(primary_rows["cedula"].iloc[0]) if not primary_rows.empty else str(p_df["cedula"].iloc[0])
        rif    = str(primary_rows["rif"].iloc[0])    if not primary_rows.empty and "rif"   in primary_rows.columns else ""
        cargo  = str(primary_rows["cargo"].iloc[0])  if not primary_rows.empty and "cargo" in primary_rows.columns else ""

        profiles.append({
            "persona_raw":   p,
            "persona":       format_rrhh_person_name(p),
            "doc_count":     doc_count,
            "primary_count": int((p_df["empleado"] == p).sum()),
            "cedulas":       cedula,
            "rifs":          rif,
            "departamentos": "; ".join(sorted(set(p_df["departamento"].dropna().astype(str).tolist()))),
            "cargos":        cargo or "Sin cargo asignado",
            "estatuses":     "; ".join(sorted(set(primary_rows["estado"].dropna().astype(str).tolist()))) if not primary_rows.empty else "; ".join(sorted(set(p_df["estado"].dropna().astype(str).tolist()))) or "Sin estado",
            "tipos":         "; ".join(sorted(set([t for t in p_df["doc_type"].dropna().astype(str).tolist() if t.strip()]))),
            "fecha_ingreso": "; ".join(sorted(set([f for f in p_df["fecha_ingreso"].dropna().astype(str).tolist() if f.strip()]))),
            "foto_url":      first_non_empty_value(p_df["foto_url"] if "foto_url" in p_df.columns else []),
            "row_indices":   p_indices,
        })
    return profiles


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.post("/buscar")
def search_rrhh(req: RrhhSearchRequest):
    df = fetch_rrhh_dataframe()

    if req.doc_types:
        df = df[df["doc_type"].isin(req.doc_types)]
    if req.estados:
        df = df[df["estado"].isin(req.estados)]
    if req.date_start and req.date_end:
        df = df[(df["fecha_ingreso"] >= req.date_start) & (df["fecha_ingreso"] <= req.date_end)]
    if req.people_terms:
        keep = []
        for idx, row in df.iterrows():
            row_people = {str(row.get("empleado", "")).strip()}
            row_people.update(split_terms(str(row.get("personas_relacionadas", ""))))
            if any(p in row_people for p in req.people_terms):
                keep.append(idx)
        df = df.loc[keep]

    profiles = build_rrhh_person_index(df)

    if req.search_term:
        term = req.search_term.lower().strip()
        profiles = [
            p for p in profiles
            if (
                term in p["persona"].lower()
                or term in p["persona_raw"].lower()
                or term in p["cedulas"].lower()
                or term in p["departamentos"].lower()
                or term in p["cargos"].lower()
                or term in p["estatuses"].lower()
                or term in p["tipos"].lower()
            )
        ]

    if req.sort_mode == "Alfabético (A-Z)":
        profiles = sorted(profiles, key=lambda x: x["persona"].lower())
    elif req.sort_mode == "Alfabético (Z-A)":
        profiles = sorted(profiles, key=lambda x: x["persona"].lower(), reverse=True)
    elif req.sort_mode == "Más recientes primero":
        profiles = sorted(profiles, key=lambda x: x["fecha_ingreso"], reverse=True)
    elif req.sort_mode == "Más antiguos primero":
        profiles = sorted(profiles, key=lambda x: x["fecha_ingreso"])

    return profiles


@router.post("/person/profile")
def get_rrhh_person_profile(req: RrhhProfileRequest):
    df = fetch_rrhh_dataframe()
    p = req.persona

    row_list = []
    for idx, row in df.iterrows():
        emp = str(row.get("empleado", "")).strip()
        rel = split_terms(str(row.get("personas_relacionadas", "")))
        if p == emp or p in rel:
            row_dict = row.to_dict()
            row_dict["__idx"] = int(idx) + 1
            row_list.append(row_dict)

    if not row_list:
        raise HTTPException(status_code=404, detail="Persona no encontrada en expedientes")

    p_df = pd.DataFrame(row_list)

    def _join_unique(series_or_col):
        col = p_df.get(series_or_col, pd.Series([], dtype=str)) if isinstance(series_or_col, str) else series_or_col
        return "; ".join(sorted(set([x for x in col.dropna().astype(str).tolist() if x.strip()])))

    return {
        "persona_raw":      p,
        "persona":          format_rrhh_person_name(p),
        "foto_url":         first_non_empty_value(p_df["foto_url"] if "foto_url" in p_df.columns else []),
        "cedulas":          _join_unique(p_df["cedula"]),
        "rifs":             _join_unique("rif"),
        "departamentos":    _join_unique(p_df["departamento"]),
        "cargos":           _join_unique("cargo") or "Sin cargo asignado",
        "statuses":         _join_unique(p_df["estado"]),
        "fecha_ingreso":    _join_unique(p_df["fecha_ingreso"]),
        "fecha_jubilacion": _join_unique("fecha_jubilacion"),
        "fecha_pension":    _join_unique("fecha_pension"),
        "categories":       sorted(set([x for x in p_df["categoria"].dropna().astype(str).tolist() if x.strip()])),
        "rows":             row_list,
    }
