"""Estadísticas y gráficas del panel de administración."""
from datetime import datetime

from fastapi import APIRouter, Depends

import pandas as pd

from database import db_query
from models import StatsRequest
from ..archive import fetch_archive_dataframe
from .deps import require_role

router = APIRouter()


@router.post("/stats", dependencies=[Depends(require_role("Archivo", "RRHH"))])
def get_admin_stats(req: StatsRequest):
    """Cifras de la fila de KPIs, con los filtros del panel aplicados.

    El desglose por tipo y por año lo sirve /charts: es lo que dibujan las
    graficas. Este endpoint solo devuelve los totales.
    """
    import routes.hr as _rrhh_mod  # lazy to avoid circular import
    df = fetch_archive_dataframe() if req.modulo == "Archivo" else _rrhh_mod.fetch_hr_dataframe()
    if df.empty:
        return {"total_docs": 0, "categories_count": 0}

    fecha_col = "fecha" if req.modulo == "Archivo" else "fecha_ingreso"

    if req.date_start and req.date_end:
        df = df[(df[fecha_col] >= req.date_start) & (df[fecha_col] <= req.date_end)]
    if req.doc_types:
        df = df[df["doc_type"].isin(req.doc_types)]
    if req.modulo == "RRHH":
        if req.status:
            df = df[df["estado"] == req.status]
        if req.dept:
            df = df[df["departamento"] == req.dept]
    else:
        if req.author:
            df = df[df["autor"] == req.author]
        if req.only_recent:
            cutoff = datetime.now().year - 2
            df = df[pd.to_datetime(df["fecha"], errors="coerce").dt.year >= cutoff]

    return {
        "total_docs":       len(df),
        "categories_count": len(df["doc_type"].dropna().unique()),
    }


# Las cifras del tablero se devuelven como enteros, pero no todas lo son:
# `ultimo_ingreso` es una fecha en texto. Coaccionar el diccionario entero con
# int() lanzaba ValueError y el endpoint respondia 500 — un error de Python, no
# de base de datos, por eso el mensaje era generico.
def _normalizar_totales(fila) -> dict:
    salida = {}
    for k, v in (dict(fila) if fila else {}).items():
        if v is None:
            salida[k] = None if k.startswith("ultimo_") else 0
            continue
        try:
            salida[k] = int(v)
        except (TypeError, ValueError):
            salida[k] = v          # fechas y cualquier otro texto, tal cual
    return salida


@router.get("/charts", dependencies=[Depends(require_role("Archivo", "RRHH"))])
def get_charts_data(modulo: str = "Archivo", date_start: str = "", date_end: str = ""):
    from .helpers import _require_modulo
    _require_modulo(modulo)

    # OA-033/OR-018/OR-019/RQ-012: la fila de KPIs mezclaba cifras filtradas
    # (/stats, que sí acepta rango de fechas) con cifras sin filtrar (/charts,
    # que lo ignoraba). Mismo rango aquí, para que /charts sea la única fuente
    # de la fila de KPIs — el filtro sólo aplica a lo que tiene fecha de
    # documento; los desgloses por plantilla (departamento, sexo, nivel
    # educativo) siguen siendo del personal activo completo, no del rango.
    rango_activo = bool(date_start and date_end)
    rango_sql = " AND fecha_documento BETWEEN %s AND %s" if rango_activo else ""
    rango_params = [date_start, date_end] if rango_activo else []

    if modulo == "Archivo":
        # Todas las consultas excluyen los borrados logicos: hasta ahora el panel
        # contaba documentos que ya estaban en la papelera.
        by_type = db_query(f"""
            WITH conteo AS (
                SELECT da.id_tipo_documento AS id,
                       COALESCE(td.nombre_corto, da.tesauro_primario, 'Sin tipo') AS label,
                       COUNT(*) AS value
                FROM public.datos_archivo da
                LEFT JOIN public.tipo_documento td ON da.id_tipo_documento = td.id
                WHERE da.deleted_at IS NULL{rango_sql}
                GROUP BY da.id_tipo_documento, label
            ), ordenado AS (
                SELECT id, label, value, ROW_NUMBER() OVER (ORDER BY value DESC, label) AS pos
                FROM conteo
            )
            SELECT id, label, value, pos FROM ordenado WHERE pos <= 7
            UNION ALL
            SELECT NULL, 'Otros', SUM(value), 8 FROM ordenado WHERE pos > 7
            HAVING SUM(value) > 0
            ORDER BY pos
        """, rango_params, fetch="all") or []
        by_year = db_query(f"""
            SELECT EXTRACT(YEAR FROM fecha_documento)::TEXT AS label, COUNT(*) AS value
            FROM public.datos_archivo
            WHERE fecha_documento IS NOT NULL AND deleted_at IS NULL{rango_sql}
            GROUP BY label ORDER BY label DESC LIMIT 10
        """, rango_params, fetch="all") or []
        by_month_sql = " AND fecha_documento >= NOW() - INTERVAL '24 months'" if not rango_activo else ""
        by_month = db_query(f"""
            SELECT TO_CHAR(fecha_documento, 'Mon YYYY') AS label,
                   DATE_TRUNC('month', fecha_documento) AS sort_key, COUNT(*) AS value
            FROM public.datos_archivo
            WHERE fecha_documento IS NOT NULL AND deleted_at IS NULL{rango_sql}{by_month_sql}
            GROUP BY label, sort_key ORDER BY sort_key
        """, rango_params, fetch="all") or []

        # El proyecto se llama "Archivo Institucional Digital": cuanto del fondo
        # esta efectivamente digitalizado es la medida que da sentido al resto,
        # y hasta ahora no aparecia por ninguna parte.
        by_soporte = db_query(f"""
            SELECT COALESCE(NULLIF(TRIM(soporte),''), 'Físico') AS label, COUNT(*) AS value
            FROM public.datos_archivo WHERE deleted_at IS NULL{rango_sql}
            GROUP BY label ORDER BY value DESC
        """, rango_params, fetch="all") or []

        totals = db_query(f"""
            SELECT COUNT(*) AS total_docs,
                   COUNT(DISTINCT da.id_tipo_documento)
                     FILTER (WHERE da.id_tipo_documento IS NOT NULL)      AS total_types,
                   (SELECT COUNT(*) FROM public.descriptores_libres)      AS total_keywords,
                   COUNT(DISTINCT da.autor)
                     FILTER (WHERE da.autor IS NOT NULL AND da.autor<>'') AS total_autores,
                   COUNT(*) FILTER (WHERE da.file_url IS NOT NULL
                                      AND da.file_url <> '')              AS total_digitalizados,
                   COUNT(*) FILTER (WHERE COALESCE(da.status,'aprobado')
                                          IN ('revision','draft'))        AS total_pendientes,
                   TO_CHAR(MAX(da.created_at), 'YYYY-MM-DD')             AS ultimo_ingreso,
                   COUNT(*) FILTER (
                       WHERE da.fecha_documento IS NOT NULL
                         AND (da.fecha_documento
                              + (COALESCE(td.plazo_retencion_anios,5) || ' years')::INTERVAL
                             )::DATE < CURRENT_DATE
                   )                                                      AS total_vencidos
            FROM public.datos_archivo da
            LEFT JOIN public.tipo_documento td ON da.id_tipo_documento = td.id
            WHERE da.deleted_at IS NULL{rango_sql.replace('fecha_documento', 'da.fecha_documento')}
        """, rango_params, fetch="one")
        return {
            "modulo": "Archivo",
            "charts": {
                "by_type":    [{"id": r["id"], "label": r["label"], "value": int(r["value"])} for r in by_type],
                "by_year":    [{"label": r["label"], "value": int(r["value"])} for r in by_year],
                "by_month":   [{"label": r["label"], "value": int(r["value"])} for r in by_month],
                "by_soporte": [{"label": r["label"], "value": int(r["value"])} for r in by_soporte],
                "totals": _normalizar_totales(totals),
            }
        }

    else:  # RRHH
        by_dept = db_query("""
            SELECT COALESCE(d.nombre,'Sin departamento') AS label, COUNT(*) AS value
            FROM public.empleados e
            LEFT JOIN public.departamentos d ON e.departamento_id = d.id
            WHERE e.deleted_at IS NULL
            GROUP BY d.nombre ORDER BY value DESC LIMIT 10
        """, fetch="all") or []
        by_status = db_query("""
            SELECT COALESCE(el.estados,'Sin estado') AS label, COUNT(*) AS value
            FROM public.empleados e
            LEFT JOIN public.estados_laborales el ON e.estado_id = el.id
            WHERE e.deleted_at IS NULL
            GROUP BY el.estados ORDER BY value DESC
        """, fetch="all") or []
        rango_dr_sql = " AND dr.fecha_documento BETWEEN %s AND %s" if rango_activo else ""
        by_doc_type = db_query(f"""
            SELECT td.nombre_corto AS label, COUNT(*) AS value
            FROM public.datos_rrhh dr
            JOIN public.tipo_documento td ON dr.id_tipo_documento = td.id
            WHERE dr.deleted_at IS NULL{rango_dr_sql}
            GROUP BY td.nombre_corto ORDER BY value DESC LIMIT 10
        """, rango_params, fetch="all") or []

        # Cobertura, no volumen. Contar documentos por Parte no dice si los
        # expedientes estan completos: mil titulos en la Parte I y ninguna
        # evaluacion en la II se veria "bien". Lo que importa es a cuantos
        # empleados les falta cada Parte.
        cobertura = db_query("""
            WITH activos AS (
                SELECT e.id FROM public.empleados e WHERE e.deleted_at IS NULL
            ), partes AS (
                SELECT c.id, c.nombre FROM public.categoria c
                WHERE c.slug LIKE 'parte-%%'
            )
            SELECT p.nombre AS label,
                   (SELECT COUNT(*) FROM activos)                       AS total,
                   COUNT(DISTINCT dr.empleado_id)                       AS value
            FROM partes p
            LEFT JOIN public.tipo_documento td ON td.id_categoria = p.id
            LEFT JOIN public.datos_rrhh dr
                   ON dr.id_tipo_documento = td.id
                  AND dr.deleted_at IS NULL
                  AND dr.empleado_id IN (SELECT id FROM activos)
            GROUP BY p.nombre, p.id ORDER BY p.id
        """, fetch="all") or []

        by_nivel = db_query("""
            SELECT COALESCE(NULLIF(TRIM(nivel_educativo),''), 'Sin especificar') AS label,
                   COUNT(*) AS value
            FROM public.empleados WHERE deleted_at IS NULL
            GROUP BY nivel_educativo ORDER BY value DESC
        """, fetch="all") or []
        by_sexo = db_query("""
            SELECT CASE sexo
                     WHEN 'M' THEN 'Masculino'
                     WHEN 'F' THEN 'Femenino'
                     WHEN 'O' THEN 'Otro'
                     ELSE 'Sin especificar'
                   END AS label,
                   COUNT(*) AS value
            FROM public.empleados WHERE deleted_at IS NULL
            GROUP BY sexo ORDER BY value DESC
        """, fetch="all") or []

        rango_dr_totales_sql = " AND fecha_documento BETWEEN %s AND %s" if rango_activo else ""
        totals = db_query(f"""
            SELECT (SELECT COUNT(*) FROM public.empleados WHERE deleted_at IS NULL)
                       AS total_employees,
                   (SELECT COUNT(*) FROM public.datos_rrhh
                    WHERE deleted_at IS NULL{rango_dr_totales_sql})
                       AS total_documents,
                   (SELECT COUNT(*) FROM public.empleados e
                    JOIN public.estados_laborales el ON e.estado_id = el.id
                    WHERE el.estados = 'Activo' AND e.deleted_at IS NULL)
                       AS total_activos,
                   (SELECT COUNT(*) FROM public.empleados e
                    JOIN public.estados_laborales el ON e.estado_id = el.id
                    WHERE el.estados IN ('Jubilado','Pensionado') AND e.deleted_at IS NULL)
                       AS total_jubilados,
                   (SELECT COUNT(*) FROM public.historial_cargos)
                       AS total_movimientos_cargo,
                   (SELECT TO_CHAR(MAX(created_at), 'YYYY-MM-DD')
                    FROM public.datos_rrhh WHERE deleted_at IS NULL)
                       AS ultimo_ingreso,
                   -- Expedientes sin un solo documento: existen en la nomina pero
                   -- no tienen nada archivado.
                   (SELECT COUNT(*) FROM public.empleados e
                    WHERE e.deleted_at IS NULL AND NOT EXISTS (
                        SELECT 1 FROM public.datos_rrhh dr
                        WHERE dr.empleado_id = e.id AND dr.deleted_at IS NULL))
                       AS total_sin_documentos,
                   -- Jubilaciones o pensiones que caen dentro de los proximos 12
                   -- meses: es el aviso que da tiempo a preparar el expediente.
                   (SELECT COUNT(*) FROM public.empleados e
                    WHERE e.deleted_at IS NULL
                      AND COALESCE(e.fecha_jubilacion, e.fecha_pension)
                          BETWEEN CURRENT_DATE AND CURRENT_DATE + INTERVAL '365 days')
                       AS total_jubilaciones_proximas
        """, rango_params, fetch="one")
        return {
            "modulo": "RRHH",
            "charts": {
                "by_department": [{"label": r["label"], "value": int(r["value"])} for r in by_dept],
                "by_status":     [{"label": r["label"], "value": int(r["value"])} for r in by_status],
                "by_doc_type":   [{"label": r["label"], "value": int(r["value"])} for r in by_doc_type],
                "cobertura":     [{"label": r["label"], "value": int(r["value"]),
                                   "total": int(r["total"])} for r in cobertura],
                "by_nivel":      [{"label": r["label"], "value": int(r["value"])} for r in by_nivel],
                "by_sexo":       [{"label": r["label"], "value": int(r["value"])} for r in by_sexo],
                "totals": _normalizar_totales(totals),
            }
        }


@router.get("/global_summary", dependencies=[Depends(require_role("Global"))])
def get_global_summary():
    """Resumen global para el panel de sistema (Admin Global)."""
    row = db_query("""
        SELECT
            (SELECT COUNT(*) FROM public.datos_archivo WHERE deleted_at IS NULL)  AS total_docs,
            (SELECT COUNT(*) FROM public.empleados     WHERE deleted_at IS NULL)  AS total_empleados,
            (SELECT COUNT(*) FROM public.datos_rrhh    WHERE deleted_at IS NULL)  AS total_rrhh_docs,
            (SELECT COUNT(*) FROM public.descriptores_libres)     AS total_keywords,
            (SELECT COUNT(*) FROM public.usuarios_sistema WHERE COALESCE(is_active,TRUE)) AS total_usuarios,
            (SELECT COUNT(*) FROM public.datos_archivo
              WHERE COALESCE(status,'aprobado') = 'revision'  AND deleted_at IS NULL) AS arch_en_revision,
            (SELECT COUNT(*) FROM public.datos_archivo
              WHERE COALESCE(status,'aprobado') = 'draft'     AND deleted_at IS NULL) AS arch_borradores,
            (SELECT COUNT(*) FROM public.datos_archivo
              WHERE COALESCE(status,'aprobado') = 'rechazado' AND deleted_at IS NULL) AS arch_rechazados,
            (SELECT COUNT(*) FROM public.datos_rrhh
              WHERE COALESCE(status,'aprobado') = 'revision'  AND deleted_at IS NULL) AS rrhh_en_revision,
            (SELECT COUNT(*) FROM public.datos_rrhh
              WHERE COALESCE(status,'aprobado') = 'draft'     AND deleted_at IS NULL) AS rrhh_borradores,
            (SELECT COUNT(*) FROM public.empleados e
             JOIN public.estados_laborales el ON e.estado_id = el.id
             WHERE LOWER(el.estados) LIKE '%%activo%%' AND e.deleted_at IS NULL) AS empleados_activos,
            (SELECT MAX(al.timestamp) FROM public.audit_log al)   AS ultima_actividad,
            (SELECT COUNT(*) FROM public.audit_log al
              WHERE al.timestamp >= NOW() - INTERVAL '24 hours')  AS eventos_24h,
            (SELECT COUNT(*) FROM public.backup_history)          AS total_backups
    """, fetch="one")
    if not row:
        return {}
    d = dict(row)
    for k in d:
        if hasattr(d[k], 'isoformat'):
            d[k] = d[k].isoformat()
        elif d[k] is None:
            d[k] = 0 if k != "ultima_actividad" else None
        else:
            try:
                d[k] = int(d[k])
            except (TypeError, ValueError):
                pass
    return d
