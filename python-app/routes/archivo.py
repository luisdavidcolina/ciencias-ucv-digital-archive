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
            COALESCE(STRING_AGG(dl.nombre, '; '), '') AS descriptores_libres,
            COALESCE(da.abstract, '')           AS resumen,
            COALESCE(da.file_url, '')           AS file_url
        FROM public.datos_archivo da
        LEFT JOIN public.archivo_descriptores ad ON da.id_archivo = ad.id_archivo
        LEFT JOIN public.descriptores_libres dl ON ad.id_descriptor = dl.id_descriptor
    """
    if filters_sql:
        base_sql += " WHERE " + filters_sql

    base_sql += " GROUP BY da.id_archivo"

    rows = db_query(base_sql, filter_params, fetch="all")
    if not rows:
        return pd.DataFrame(columns=[
            "id", "titulo", "autor", "fecha", "doc_type", "categoria", "ubicacion",
            "tesauro_primario", "tesauro_secundario", "descriptores_libres", "resumen", "file_url",
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
    page     = max(1, req.page)
    per_page = max(1, min(req.per_page, 50))
    offset   = (page - 1) * per_page

    conditions: list = []
    params: list = []

    if req.search_term:
        term = f"%{req.search_term}%"
        conditions.append(
            "(unaccent(da.titulo) ILIKE unaccent(%s)"
            " OR unaccent(COALESCE(da.autor,'')) ILIKE unaccent(%s)"
            " OR unaccent(COALESCE(da.abstract,'')) ILIKE unaccent(%s))"
        )
        params.extend([term, term, term])

    if req.doc_types:
        conditions.append("da.tesauro_primario = ANY(%s)")
        params.append(req.doc_types)

    if req.tesauro_terms:
        conditions.append(
            """da.id_archivo IN (
                SELECT ad2.id_archivo FROM public.archivo_descriptores ad2
                JOIN public.descriptores_libres dl2 ON ad2.id_descriptor = dl2.id_descriptor
                WHERE dl2.nombre = ANY(%s)
                UNION
                SELECT da3.id_archivo FROM public.datos_archivo da3
                WHERE da3.tesauro_primario = ANY(%s) OR da3.tesauro_secundario = ANY(%s)
            )"""
        )
        params.extend([req.tesauro_terms, req.tesauro_terms, req.tesauro_terms])

    if req.date_start:
        conditions.append("da.fecha_documento >= %s::date")
        params.append(req.date_start)

    if req.date_end:
        conditions.append("da.fecha_documento <= %s::date")
        params.append(req.date_end)

    where = ("WHERE " + " AND ".join(conditions)) if conditions else ""

    sort_map = {
        "Más recientes primero": "da.fecha_documento DESC NULLS LAST",
        "Más antiguos primero":  "da.fecha_documento ASC NULLS LAST",
        "Alfabético (Z-A)":      "da.titulo DESC",
    }
    order = sort_map.get(req.sort_mode, "da.titulo ASC")

    sql = f"""
        SELECT
            da.id_archivo AS id,
            da.titulo,
            COALESCE(da.autor, '')         AS autor,
            TO_CHAR(da.fecha_documento, 'YYYY-MM-DD') AS fecha,
            COALESCE(da.tesauro_primario, '')   AS doc_type,
            COALESCE(da.tesauro_secundario, '') AS tesauro_secundario,
            COALESCE(da.ubicacion, '')          AS ubicacion,
            COALESCE(da.abstract, '')           AS resumen,
            COALESCE(da.file_url, '')           AS file_url,
            COALESCE(STRING_AGG(DISTINCT dl.nombre, '; ') FILTER (WHERE dl.nombre IS NOT NULL), '') AS descriptores_libres,
            COUNT(*) OVER() AS total_count
        FROM public.datos_archivo da
        LEFT JOIN public.archivo_descriptores ad ON da.id_archivo = ad.id_archivo
        LEFT JOIN public.descriptores_libres dl ON ad.id_descriptor = dl.id_descriptor
        {where}
        GROUP BY da.id_archivo, da.titulo, da.autor, da.fecha_documento,
                 da.tesauro_primario, da.tesauro_secundario, da.ubicacion, da.abstract, da.file_url
        ORDER BY {order}
        LIMIT %s OFFSET %s
    """
    params.extend([per_page, offset])

    rows = db_query(sql, params, fetch="all") or []

    total = int(rows[0]["total_count"]) if rows else 0

    records = []
    for i, r in enumerate(rows):
        rec = dict(r)
        rec.pop("total_count", None)
        rec["__idx"] = offset + i + 1
        badges = set(split_terms(rec.get("doc_type", "")))
        badges.update(split_terms(rec.get("tesauro_secundario", "")))
        badges.update(split_terms(rec.get("descriptores_libres", "")))
        rec["tesauro_badges"] = sorted(badges)
        records.append(rec)

    return {
        "records":  records,
        "total":    total,
        "page":     page,
        "per_page": per_page,
    }


@router.get("/documentos/buscar")
def buscar_tipo_documento(q: str = Query(..., description="Palabra clave a buscar")):
    rows = db_query(
        """
        SELECT DISTINCT val AS nombre_corto
        FROM (
            SELECT UNNEST(ARRAY[tesauro_primario, tesauro_secundario]) AS val
            FROM public.datos_archivo
            UNION
            SELECT nombre AS val
            FROM public.descriptores_libres
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
