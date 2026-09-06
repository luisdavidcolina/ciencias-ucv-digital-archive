import re
from fastapi import APIRouter, HTTPException, Query
import pandas as pd

from database import db_query, log_event, logger, split_terms
from models import ArchivoSearchRequest
from utils import paginate

router = APIRouter(prefix="/api/archivo", tags=["archivo"])

# BA-016: valor centinela para la faceta "Sin tipo" — el catálogo de tipos es
# texto libre y una cadena vacía no se puede pasar como opción seleccionable
# en el `<select>` sin ambigüedad, así que se traduce a `tesauro_primario = ''`.
SIN_TIPO_SENTINEL = "__sin_tipo__"


def _build_common_conditions(req: "ArchivoSearchRequest", *, fts_fields: str = "full") -> tuple:
    """Condiciones compartidas por la búsqueda principal y las facetas.

    BA-161: esta lógica estaba escrita dos veces con variaciones sutiles (la
    de facetas omitía `personas_relacionadas`/`tesauro_secundario` del FTS),
    lo que ya había causado BA-015. `fts_fields="full"` usa las mismas columnas
    que el índice GIN; `fts_fields="short"` es la variante reducida que ya
    usaban las facetas y se conserva para no cambiar su comportamiento de
    golpe fuera del alcance de este pase.
    """
    conditions: list = ["da.deleted_at IS NULL", "COALESCE(da.status, 'aprobado') = 'aprobado'"]
    params: list = []

    if req.search_term:
        term = f"%{req.search_term}%"
        has_letters = bool(re.search(r'[A-Za-zÀ-ÿ]', req.search_term))
        if has_letters:
            if fts_fields == "full":
                tsv = (
                    "coalesce(da.titulo,'') || ' ' ||"
                    "coalesce(da.autor,'') || ' ' ||"
                    "coalesce(da.abstract,'') || ' ' ||"
                    "coalesce(da.tesauro_primario,'') || ' ' ||"
                    "coalesce(da.tesauro_secundario,'') || ' ' ||"
                    "coalesce(da.personas_relacionadas,'')"
                )
                # BA-122: `websearch_to_tsquery` es un reemplazo compatible de
                # `plainto_tsquery` que además entiende comillas ("frase exacta"),
                # `OR` y `-exclusión` sin exigir ningún cambio de contrato con el
                # frontend — un término plano se comporta igual que antes.
                conditions.append(
                    f"(to_tsvector('spanish', {tsv}) @@ websearch_to_tsquery('spanish', %s)"
                    " OR unaccent(da.titulo) ILIKE unaccent(%s)"
                    " OR unaccent(COALESCE(da.autor,'')) ILIKE unaccent(%s)"
                    " OR unaccent(COALESCE(da.personas_relacionadas,'')) ILIKE unaccent(%s))"
                )
                params.extend([req.search_term, term, term, term])
            else:
                tsv = (
                    "coalesce(da.titulo,'') || ' ' || coalesce(da.autor,'') || ' ' ||"
                    "coalesce(da.abstract,'') || ' ' || coalesce(da.tesauro_primario,'')"
                )
                conditions.append(
                    f"(to_tsvector('spanish', {tsv}) @@ websearch_to_tsquery('spanish', %s)"
                    " OR unaccent(da.titulo) ILIKE unaccent(%s)"
                    " OR unaccent(COALESCE(da.autor,'')) ILIKE unaccent(%s))"
                )
                params.extend([req.search_term, term, term])
        else:
            conditions.append(
                "(unaccent(da.titulo) ILIKE unaccent(%s)"
                " OR unaccent(COALESCE(da.autor,'')) ILIKE unaccent(%s)"
                " OR unaccent(COALESCE(da.ubicacion,'')) ILIKE unaccent(%s))"
            )
            params.extend([term, term, term])

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

    if getattr(req, "soporte", None) and req.soporte in ("Físico", "Digital", "Digitalizado"):
        conditions.append("COALESCE(da.soporte,'Físico') = %s")
        params.append(req.soporte)

    return conditions, params


# =============================================================================
# DATAFRAME FETCHER
# =============================================================================

def fetch_archive_dataframe() -> pd.DataFrame:
    """Retorna todos los documentos de archivo (no borrados) como DataFrame.

    BA-185: antes aceptaba `filters_sql`/`filter_params` para una cláusula
    WHERE adicional que nadie pasaba nunca (única llamada: `admin/stats.py`,
    sin argumentos) — una superficie de inyección SQL por interpolación de
    cadena esperando a que alguien la usara. Se retiraron ambos parámetros.
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
            COALESCE(da.file_url, '')           AS file_url,
            COALESCE(da.numero_folio, '')       AS numero_folio,
            COALESCE(da.soporte, 'Físico')      AS soporte,
            da.numero_paginas
        FROM public.datos_archivo da
        LEFT JOIN public.archivo_descriptores ad ON da.id_archivo = ad.id_archivo
        LEFT JOIN public.descriptores_libres dl ON ad.id_descriptor = dl.id_descriptor
    """
    base_sql += " WHERE da.deleted_at IS NULL"
    base_sql += " GROUP BY da.id_archivo"

    rows = db_query(base_sql, None, fetch="all")
    if not rows:
        return pd.DataFrame(columns=[
            "id", "titulo", "autor", "fecha", "doc_type", "categoria", "ubicacion",
            "tesauro_primario", "tesauro_secundario", "descriptores_libres", "resumen", "file_url",
            "numero_folio", "soporte", "numero_paginas",
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

@router.post("/buscar", summary="Búsqueda de documentos institucionales")
def search_archive(req: ArchivoSearchRequest):
    """
    Búsqueda full-text de documentos en el Archivo Institucional.

    Implementa `plainto_tsquery('spanish')` con ranking por `ts_rank_cd()`.
    Cuando el término no contiene letras (e.g., años, números), hace fallback
    a `unaccent(ILIKE)` para compatibilidad con búsquedas numéricas.

    Retorna paginación server-side: `{records, total, page, per_page}`.
    """
    page, per_page, offset = paginate(req.page, req.per_page, max_per_page=50)

    conditions, params = _build_common_conditions(req, fts_fields="full")

    if req.doc_types:
        # BA-016: "Sin tipo" viaja como valor centinela, no como la etiqueta
        # visible, porque `tesauro_primario = ANY(['Sin tipo'])` no casaba con
        # ninguna fila real y la faceta no filtraba nada.
        real_types = [t for t in req.doc_types if t != SIN_TIPO_SENTINEL]
        wants_sin_tipo = len(real_types) != len(req.doc_types)
        type_conds = []
        if real_types:
            type_conds.append("da.tesauro_primario = ANY(%s)")
            params.append(real_types)
        if wants_sin_tipo:
            type_conds.append("COALESCE(da.tesauro_primario,'') = ''")
        if type_conds:
            conditions.append("(" + " OR ".join(type_conds) + ")")

    where = ("WHERE " + " AND ".join(conditions)) if conditions else ""

    # BA-005: el orden elegido por el usuario se ignoraba en cuanto el
    # término tenía letras. Ahora sólo se sustituye por relevancia cuando el
    # usuario no ha elegido explícitamente otro criterio (el valor por
    # defecto del `<select>` es "Alfabético (A-Z)"), o cuando pide
    # "Relevancia" a propósito.
    sort_map = {
        "Alfabético (A-Z)":      "da.titulo ASC",
        "Alfabético (Z-A)":      "da.titulo DESC",
        "Más recientes primero": "da.fecha_documento DESC NULLS LAST",
        "Más antiguos primero":  "da.fecha_documento ASC NULLS LAST",
        "Relevancia":            "relevance DESC, da.titulo ASC",
    }
    base_order = sort_map.get(req.sort_mode, "da.titulo ASC")

    _search_has_letters = bool(req.search_term) and bool(re.search(r'[A-Za-zÀ-ÿ]', req.search_term))
    _default_sort = req.sort_mode in (None, "", "Alfabético (A-Z)")

    if req.sort_mode == "Relevancia" or (_search_has_letters and _default_sort):
        order = "relevance DESC, da.titulo ASC"
    else:
        order = base_order

    # BA-167: to_tsvector()/plainto_tsquery() se evaluaban por cada fila
    # devuelta aunque no hubiese término de búsqueda, porque `order` no los
    # usaba pero la columna se seguía calculando. Sin término, `relevance` es
    # una constante y no cuesta nada.
    if _search_has_letters:
        relevance_sql = """ts_rank_cd(
              to_tsvector('spanish',
                coalesce(da.titulo,'') || ' ' || coalesce(da.autor,'') || ' ' ||
                coalesce(da.abstract,'') || ' ' || coalesce(da.tesauro_primario,'') || ' ' ||
                coalesce(da.tesauro_secundario,'') || ' ' || coalesce(da.personas_relacionadas,'')
              ),
              websearch_to_tsquery('spanish', %s)
            )"""
        relevance_params = [req.search_term]
    else:
        relevance_sql = "0::real"
        relevance_params = []

    sql = f"""
        SELECT
            da.id_archivo AS id,
            da.titulo,
            COALESCE(da.autor, '')               AS autor,
            TO_CHAR(da.fecha_documento, 'YYYY-MM-DD') AS fecha,
            COALESCE(da.tesauro_primario, '')    AS doc_type,
            COALESCE(da.tesauro_secundario, '')  AS tesauro_secundario,
            COALESCE(da.ubicacion, '')           AS ubicacion,
            COALESCE(da.abstract, '')            AS resumen,
            COALESCE(da.file_url, '')            AS file_url,
            COALESCE(da.personas_relacionadas, '') AS personas_relacionadas,
            COALESCE(da.numero_folio, '')           AS numero_folio,
            COALESCE(da.soporte, 'Físico')          AS soporte,
            da.numero_paginas,
            COALESCE(STRING_AGG(DISTINCT dl.nombre, '; ') FILTER (WHERE dl.nombre IS NOT NULL), '') AS descriptores_libres,
            {relevance_sql} AS relevance,
            COUNT(*) OVER() AS total_count
        FROM public.datos_archivo da
        LEFT JOIN public.archivo_descriptores ad ON da.id_archivo = ad.id_archivo
        LEFT JOIN public.descriptores_libres dl ON ad.id_descriptor = dl.id_descriptor
        {where}
        GROUP BY da.id_archivo, da.titulo, da.autor, da.fecha_documento,
                 da.tesauro_primario, da.tesauro_secundario, da.ubicacion, da.abstract,
                 da.file_url, da.personas_relacionadas, da.numero_folio, da.soporte, da.numero_paginas
        ORDER BY {order}
        LIMIT %s OFFSET %s
    """
    params = relevance_params + params
    params.extend([per_page, offset])

    # BA-181: la política del CHANGELOG 3.1.0 es registrar completo y no
    # exponer el error de SQL al cliente. No había ningún `try/except`
    # alrededor de estas tres consultas.
    try:
        rows = db_query(sql, params, fetch="all") or []

        # BA-161/BA-015: comparte `_build_common_conditions` con la búsqueda
        # principal en vez de repetir la lógica de filtros con variaciones —
        # la duplicación anterior ya había divergido (el filtro de Palabras
        # Clave no se aplicaba en facetas, así que con una activa los
        # conteos describían un conjunto distinto al que se veía en pantalla).
        facet_conds, facet_params = _build_common_conditions(req, fts_fields="short")
        facet_where = "WHERE " + " AND ".join(facet_conds)

        facet_type_rows = db_query(
            f"""SELECT COALESCE(NULLIF(da.tesauro_primario,''),'{SIN_TIPO_SENTINEL}') AS name, COUNT(*) AS cnt
                FROM public.datos_archivo da {facet_where}
                GROUP BY da.tesauro_primario ORDER BY cnt DESC LIMIT 20""",
            facet_params or None, fetch="all"
        ) or []

        facet_year_rows = db_query(
            f"""SELECT EXTRACT(YEAR FROM da.fecha_documento)::INT AS yr, COUNT(*) AS cnt
                FROM public.datos_archivo da {facet_where}
                  AND da.fecha_documento IS NOT NULL
                GROUP BY yr ORDER BY yr DESC LIMIT 15""",
            facet_params or None, fetch="all"
        ) or []
    except Exception as e:
        logger.error(f"Archivo: falló la búsqueda ({req.search_term!r}, página {page}): {e}")
        raise HTTPException(status_code=500, detail="No se pudo completar la búsqueda. Intente de nuevo.")

    total = int(rows[0]["total_count"]) if rows else 0

    # BA-021: `COUNT(*) OVER()` se lee de `rows[0]`; si el `offset` cae más
    # allá del final (una página fuera de rango), `rows` viene vacío y
    # `total` caía a 0 — la interfaz mostraba "0 resultados" en vez del total
    # real que permitiría volver a una página válida. Sólo en ese caso se
    # paga una consulta extra para conocer el total real.
    if not rows and page > 1:
        try:
            count_conds, count_params = _build_common_conditions(req, fts_fields="full")
            count_where = "WHERE " + " AND ".join(count_conds)
            count_row = db_query(
                f"SELECT COUNT(*) AS cnt FROM public.datos_archivo da {count_where}",
                count_params or None, fetch="one",
            )
            total = int(count_row["cnt"]) if count_row else 0
        except Exception as e:
            logger.error(f"Archivo: falló el conteo de respaldo tras página vacía: {e}")

    records = []
    for i, r in enumerate(rows):
        rec = dict(r)
        rec.pop("total_count", None)
        rec.pop("relevance", None)
        rec["__idx"] = offset + i + 1
        badges = set(split_terms(rec.get("doc_type", "")))
        badges.update(split_terms(rec.get("tesauro_secundario", "")))
        badges.update(split_terms(rec.get("descriptores_libres", "")))
        rec["tesauro_badges"] = sorted(badges)
        records.append(rec)

    # BA-180: ninguna búsqueda quedaba en auditoría (a diferencia de
    # `share.py`, que audita cada consulta externa). El endpoint es anónimo
    # (sin sesión, ver nota A2-archivo-backend en `_BUZON.md`), así que el
    # usuario se registra como "anónimo"; sólo se audita cuando hay
    # resultados, igual que pide el ticket.
    if total > 0:
        log_event("anónimo", "Búsqueda realizada", "Archivo",
                   f"term={req.search_term!r} total={total} page={page}")

    return {
        "records":  records,
        "total":    total,
        "page":     page,
        "per_page": per_page,
        "facets": {
            "by_type": [{"name": r["name"], "count": int(r["cnt"])} for r in facet_type_rows],
            "by_year": [{"year": r["yr"], "count": int(r["cnt"])} for r in facet_year_rows],
        },
    }


@router.get("/documentos/buscar")
def lookup_document_type(q: str = Query(..., description="Palabra clave a buscar")):
    # BA-027: un solo carácter fuerza un escaneo completo de dos tablas en
    # cada pulsación; con menos de 2 caracteres no vale la pena consultar.
    if len(q.strip()) < 2:
        return []
    try:
        rows = db_query(
            """
            SELECT DISTINCT val AS nombre_corto
            FROM (
                SELECT UNNEST(ARRAY[tesauro_primario, tesauro_secundario]) AS val
                FROM public.datos_archivo
                -- BA-026: sin estos dos filtros se sugerían términos que sólo
                -- existen en la papelera o en material sin aprobar; al elegirlos
                -- la búsqueda devolvía siempre cero resultados.
                WHERE deleted_at IS NULL AND COALESCE(status, 'aprobado') = 'aprobado'
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
    except Exception as e:
        # BA-181: misma política que el buscador principal — registrar
        # completo, no exponer el error de SQL al cliente.
        logger.error(f"Archivo: falló el autocompletado ({q!r}): {e}")
        raise HTTPException(status_code=500, detail="No se pudo completar la sugerencia. Intente de nuevo.")
    return [{"nombre_corto": r["nombre_corto"]} for r in rows]
