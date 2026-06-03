from fastapi import APIRouter, Query
import pandas as pd

from database import db_query, split_terms
from models import ArchivoSearchRequest

router = APIRouter(prefix="/api/archivo", tags=["archivo"])


# =============================================================================
# DATAFRAME FETCHER
# =============================================================================

def fetch_archivo_dataframe(filters_sql: str = "", filter_params=None) -> pd.DataFrame:
    """Retorna todos los documentos de archivo como DataFrame.

    Args:
        filters_sql:   Cláusula WHERE sin la palabra 'WHERE' (opcional).
        filter_params: Parámetros para la cláusula WHERE.
    """
    base_sql = """
        SELECT
            da.id_archivo AS id,
            da.titulo,
            COALESCE(da.autor, '')         AS autor,
            TO_CHAR(da.fecha_documento, 'YYYY-MM-DD') AS fecha,
            COALESCE(da.tesauro_primario, '')   AS doc_type,
            COALESCE(da.tesauro_secundario, '') AS categoria,
            COALESCE(da.ubicacion, '')          AS ubicacion,
            COALESCE(da.tesauro_primario, '')   AS tesauro_primario,
            COALESCE(da.tesauro_secundario, '') AS tesauro_secundario,
            COALESCE(da.descriptores_libres, '') AS descriptores_libres,
            COALESCE(da.abstract, '')           AS resumen
        FROM public.datos_archivo da
    """
    if filters_sql:
        base_sql += " WHERE " + filters_sql

    rows = db_query(base_sql, filter_params, fetch="all")
    if not rows:
        return pd.DataFrame(columns=[
            "id", "titulo", "autor", "fecha", "doc_type", "categoria", "ubicacion",
            "tesauro_primario", "tesauro_secundario", "descriptores_libres", "resumen",
        ])

    df = pd.DataFrame([dict(r) for r in rows]).fillna("")

    # Usar doc_type como tesauro_primario cuando está vacío
    if "tesauro_primario" in df.columns and "doc_type" in df.columns:
        empty_mask = df["tesauro_primario"].astype(str).str.strip() == ""
        df.loc[empty_mask, "tesauro_primario"] = df.loc[empty_mask, "doc_type"]

    return df


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.post("/buscar")
def search_archivo(req: ArchivoSearchRequest):
    df = fetch_archivo_dataframe()

    # Búsqueda de texto libre
    if req.search_term:
        term = req.search_term.lower().strip()
        df = df[
            df["titulo"].astype(str).str.lower().str.contains(term, na=False)
            | df["autor"].astype(str).str.lower().str.contains(term, na=False)
            | df["resumen"].astype(str).str.lower().str.contains(term, na=False)
            | df["ubicacion"].astype(str).str.lower().str.contains(term, na=False)
        ]

    # Tipología
    if req.doc_types:
        df = df[df["doc_type"].isin(req.doc_types)]

    # Tesauro / descriptores
    if req.tesauro_terms:
        keep = []
        for idx, row in df.iterrows():
            row_terms = set(split_terms(row.get("doc_type", "")))
            for col in ["tesauro_primario", "tesauro_secundario", "descriptores_libres"]:
                row_terms.update(split_terms(row.get(col, "")))
            if any(t in row_terms for t in req.tesauro_terms):
                keep.append(idx)
        df = df.loc[keep]

    # Rango de fechas
    if req.date_start and req.date_end:
        df = df[(df["fecha"] >= req.date_start) & (df["fecha"] <= req.date_end)]

    # Ordenamiento
    if req.sort_mode == "Alfabético (A-Z)":
        df = df.sort_values(by="titulo", key=lambda c: c.astype(str).str.lower())
    elif req.sort_mode == "Alfabético (Z-A)":
        df = df.sort_values(by="titulo", key=lambda c: c.astype(str).str.lower(), ascending=False)
    elif req.sort_mode == "Más recientes primero":
        df = df.sort_values(by="fecha", ascending=False)
    elif req.sort_mode == "Más antiguos primero":
        df = df.sort_values(by="fecha")

    records = df.to_dict(orient="records")
    for idx, rec in enumerate(records):
        rec["__idx"] = idx + 1
        rec["tesauro_badges"] = sorted(set(
            split_terms(rec.get("doc_type", ""))
            + split_terms(rec.get("tesauro_primario", ""))
            + split_terms(rec.get("tesauro_secundario", ""))
            + split_terms(rec.get("descriptores_libres", ""))
        ))
    return records


@router.get("/documentos/buscar")
def buscar_tipo_documento(q: str = Query(..., description="Palabra clave a buscar")):
    rows = db_query(
        """
        SELECT DISTINCT val AS nombre_corto
        FROM (
            SELECT UNNEST(ARRAY[tesauro_primario, tesauro_secundario]) AS val
            FROM public.datos_archivo
        ) sub
        WHERE val IS NOT NULL AND val != ''
          AND unaccent(val) ILIKE unaccent(%s)
        ORDER BY val
        LIMIT 20
        """,
        (f"%{q}%",),
        fetch="all",
    )
    return [{"nombre_corto": r["nombre_corto"]} for r in rows]
