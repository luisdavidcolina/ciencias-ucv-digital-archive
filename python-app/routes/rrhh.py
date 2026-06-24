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
# DATAFRAME FETCHER (usado por admin stats y person/profile)
# =============================================================================

def fetch_rrhh_dataframe(filters_sql: str = "", filter_params=None) -> pd.DataFrame:
    """Retorna el expediente completo de RRHH como DataFrame."""
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
            dr.id_rrhh,
            COALESCE(td.nombre_corto, td.nombre, '') AS doc_type,
            COALESCE(cat.nombre, '')               AS categoria,
            COALESCE(dr.ubicacion, '')                AS ubicacion,
            COALESCE(dr.titulo, '')                   AS titulo_doc,
            COALESCE(dr.autor, '')                    AS autor,
            COALESCE(dr.abstract, '')                 AS abstract,
            TO_CHAR(dr.fecha_documento, 'YYYY-MM-DD') AS fecha_documento,
            e.nombres || ' ' || e.apellidos           AS personas_relacionadas,
            COALESCE(STRING_AGG(dl.nombre, '; '), '') AS descriptores_libres
        FROM public.empleados e
        LEFT JOIN public.cargos            c  ON e.cargo_id        = c.id
        LEFT JOIN public.departamentos     d  ON e.departamento_id = d.id
        LEFT JOIN public.estados_laborales el ON e.estado_id       = el.id
        LEFT JOIN public.datos_rrhh        dr ON dr.empleado_id    = e.id
        LEFT JOIN public.tipo_documento    td ON dr.id_tipo_documento = td.id
        LEFT JOIN public.categoria        cat ON td.id_categoria = cat.id
        LEFT JOIN public.rrhh_descriptores rd ON dr.id_rrhh = rd.id_rrhh
        LEFT JOIN public.descriptores_libres dl ON rd.id_descriptor = dl.id_descriptor
    """
    if filters_sql:
        base_sql += " WHERE " + filters_sql

    base_sql += " GROUP BY e.id, c.id, d.id, el.id, dr.id_rrhh, td.id, cat.id"

    rows = db_query(base_sql, filter_params, fetch="all")
    if not rows:
        return pd.DataFrame(columns=[
            "cedula", "empleado", "personas_relacionadas", "departamento", "estado",
            "doc_type", "categoria", "fecha_ingreso", "ubicacion", "foto_url",
            "fecha_jubilacion", "fecha_pension", "rif", "cargo", "id_archivo", "descriptores_libres"
        ])
    return pd.DataFrame([dict(r) for r in rows]).fillna("")


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.post("/buscar")
def search_rrhh(req: RrhhSearchRequest):
    """Busca personas en el índice RRHH usando la VIEW vw_rrhh_persona_index con paginación SQL."""
    page     = max(1, req.page)
    per_page = max(1, min(req.per_page, 50))
    offset   = (page - 1) * per_page

    conditions: list = []
    params: list = []

    if req.search_term:
        term = f"%{req.search_term}%"
        conditions.append(
            "(unaccent(v.persona_raw) ILIKE unaccent(%s) OR v.cedula ILIKE %s)"
        )
        params.extend([term, term])

    if req.doc_types:
        type_clauses = ["v.tipos ILIKE %s" for _ in req.doc_types]
        conditions.append("(" + " OR ".join(type_clauses) + ")")
        params.extend([f"%{t}%" for t in req.doc_types])

    if req.estados:
        conditions.append("v.estado = ANY(%s)")
        params.append(req.estados)

    if req.people_terms:
        people_clauses = ["unaccent(v.persona_raw) ILIKE unaccent(%s)" for _ in req.people_terms]
        conditions.append("(" + " OR ".join(people_clauses) + ")")
        params.extend([f"%{p}%" for p in req.people_terms])

    if req.date_start:
        conditions.append("v.fecha_ingreso >= %s")
        params.append(req.date_start)

    if req.date_end:
        conditions.append("v.fecha_ingreso <= %s")
        params.append(req.date_end)

    where = ("WHERE " + " AND ".join(conditions)) if conditions else ""

    sort_map = {
        "Más recientes primero": "v.fecha_ingreso DESC NULLS LAST",
        "Más antiguos primero":  "v.fecha_ingreso ASC NULLS LAST",
    }
    order = sort_map.get(req.sort_mode, "v.persona_raw ASC")

    sql = f"""
        SELECT
            v.empleado_id, v.cedula, v.rif, v.persona_raw AS empleado,
            v.cargo, v.departamento, v.estado,
            v.fecha_ingreso, v.foto_url,
            v.doc_count, v.tipos,
            COUNT(*) OVER() AS total_count
        FROM public.vw_rrhh_persona_index v
        {where}
        ORDER BY {order}
        LIMIT %s OFFSET %s
    """
    params.extend([per_page, offset])

    rows = db_query(sql, params, fetch="all") or []

    total = int(rows[0]["total_count"]) if rows else 0

    records = []
    for i, r in enumerate(rows):
        tipos_str = r.get("tipos") or ""
        first_tipo = split_terms(tipos_str)[0] if split_terms(tipos_str) else ""
        records.append({
            "empleado_id":   r["empleado_id"],
            "persona_raw":   r["empleado"],
            "persona":       format_rrhh_person_name(r["empleado"]),
            "doc_count":     int(r["doc_count"]),
            "cedulas":       r["cedula"] or "",
            "rifs":          r["rif"]    or "",
            "departamentos": r["departamento"] or "",
            "cargos":        r["cargo"]  or "Sin cargo asignado",
            "estatuses":     r["estado"] or "Sin estado",
            "tipos":         tipos_str,
            "fecha_ingreso": r["fecha_ingreso"] or "",
            "foto_url":      r["foto_url"] or "",
            "doc_type":      first_tipo,
            "__idx":         offset + i + 1,
        })

    return {
        "records":  records,
        "total":    total,
        "page":     page,
        "per_page": per_page,
    }


@router.post("/person/profile")
def get_rrhh_person_profile(req: RrhhProfileRequest):
    df = fetch_rrhh_dataframe(
        "e.nombres || ' ' || e.apellidos = %s",
        (req.persona,),
    )

    if df.empty:
        raise HTTPException(status_code=404, detail="Persona no encontrada en expedientes")

    row_list = []
    for idx, row in df.iterrows():
        row_dict = row.to_dict()
        row_dict["__idx"] = int(idx) + 1
        row_list.append(row_dict)

    p_df = pd.DataFrame(row_list)

    def _join_unique(col_name: str) -> str:
        col = p_df.get(col_name, pd.Series([], dtype=str))
        return "; ".join(sorted(set([x for x in col.dropna().astype(str).tolist() if x.strip()])))

    return {
        "persona_raw":      req.persona,
        "persona":          format_rrhh_person_name(req.persona),
        "foto_url":         first_non_empty_value(p_df["foto_url"] if "foto_url" in p_df.columns else []),
        "cedulas":          _join_unique("cedula"),
        "rifs":             _join_unique("rif"),
        "departamentos":    _join_unique("departamento"),
        "cargos":           _join_unique("cargo") or "Sin cargo asignado",
        "statuses":         _join_unique("estado"),
        "fecha_ingreso":    _join_unique("fecha_ingreso"),
        "fecha_jubilacion": _join_unique("fecha_jubilacion"),
        "fecha_pension":    _join_unique("fecha_pension"),
        "categories":       sorted(set([x for x in p_df["categoria"].dropna().astype(str).tolist() if x.strip()])),
        "rows":             row_list,
    }
