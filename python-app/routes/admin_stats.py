"""Estadísticas y gráficas del panel de administración."""
import multiprocessing
import platform
from datetime import datetime

from fastapi import APIRouter

import pandas as pd

from database import db_query
from models import StatsRequest
from .archivo import fetch_archivo_dataframe
from .rrhh import fetch_rrhh_dataframe

router = APIRouter()


@router.post("/stats")
def get_admin_stats(req: StatsRequest):
    df = fetch_archivo_dataframe() if req.modulo == "Archivo" else fetch_rrhh_dataframe()
    if df.empty:
        return {"total_docs": 0, "categories_count": 0, "by_type": [], "timeline": []}

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

    total_docs = len(df)
    categories_count = len(df["doc_type"].dropna().unique())

    by_type = []
    if total_docs > 0:
        for t_name, count in df["doc_type"].value_counts().items():
            by_type.append({
                "type":  t_name,
                "count": int(count),
                "pct":   int(round(count / total_docs * 100)),
            })

    timeline = []
    if total_docs > 0:
        years = pd.to_datetime(df[fecha_col], errors="coerce").dt.year.dropna().astype(int)
        if not years.empty:
            conteos = years.value_counts().sort_index()
            max_c = int(conteos.max())
            for y_val, count in conteos.items():
                timeline.append({
                    "year":      str(y_val),
                    "count":     int(count),
                    "pct_width": int(round(count / max_c * 100)) if max_c > 0 else 0,
                })

    return {
        "total_docs":       total_docs,
        "categories_count": categories_count,
        "by_type":          by_type,
        "timeline":         timeline,
        "system": {
            "status": "Operativo" if total_docs > 0 else "Sin Datos",
            "ram":    f"{round(30 + total_docs * 0.005, 2)} MB",
            "cpu":    multiprocessing.cpu_count(),
            "os":     f"{platform.system()} {platform.release()}",
        },
    }


@router.get("/charts")
def get_charts_data(modulo: str = "Archivo"):
    if modulo == "Archivo":
        by_type = db_query("""
            SELECT COALESCE(tesauro_primario, 'Sin tipo') AS label, COUNT(*) AS value
            FROM public.datos_archivo GROUP BY tesauro_primario ORDER BY value DESC LIMIT 12
        """, fetch="all") or []
        by_year = db_query("""
            SELECT EXTRACT(YEAR FROM fecha_documento)::TEXT AS label, COUNT(*) AS value
            FROM public.datos_archivo WHERE fecha_documento IS NOT NULL
            GROUP BY label ORDER BY label DESC LIMIT 10
        """, fetch="all") or []
        by_month = db_query("""
            SELECT TO_CHAR(fecha_documento, 'Mon YYYY') AS label,
                   DATE_TRUNC('month', fecha_documento) AS sort_key, COUNT(*) AS value
            FROM public.datos_archivo WHERE fecha_documento IS NOT NULL
              AND fecha_documento >= NOW() - INTERVAL '24 months'
            GROUP BY label, sort_key ORDER BY sort_key
        """, fetch="all") or []
        totals = db_query("""
            SELECT COUNT(*) AS total_docs,
                   COUNT(DISTINCT COALESCE(tesauro_primario,'')) AS total_types,
                   (SELECT COUNT(*) FROM public.descriptores_libres) AS total_keywords,
                   COUNT(DISTINCT autor) FILTER (WHERE autor IS NOT NULL AND autor<>'') AS total_autores
            FROM public.datos_archivo
        """, fetch="one")
        return {
            "modulo": "Archivo",
            "charts": {
                "by_type":  [{"label": r["label"], "value": int(r["value"])} for r in by_type],
                "by_year":  [{"label": r["label"], "value": int(r["value"])} for r in by_year],
                "by_month": [{"label": r["label"], "value": int(r["value"])} for r in by_month],
                "totals": {k: int(v or 0) for k, v in (dict(totals) if totals else {}).items()},
            }
        }
    else:  # RRHH
        by_dept = db_query("""
            SELECT COALESCE(d.nombre,'Sin departamento') AS label, COUNT(*) AS value
            FROM public.empleados e
            LEFT JOIN public.departamentos d ON e.departamento_id = d.id
            GROUP BY d.nombre ORDER BY value DESC LIMIT 10
        """, fetch="all") or []
        by_status = db_query("""
            SELECT COALESCE(el.estados,'Sin estado') AS label, COUNT(*) AS value
            FROM public.empleados e
            LEFT JOIN public.estados_laborales el ON e.estado_id = el.id
            GROUP BY el.estados ORDER BY value DESC
        """, fetch="all") or []
        by_doc_type = db_query("""
            SELECT td.nombre_corto AS label, COUNT(*) AS value
            FROM public.datos_rrhh dr
            JOIN public.tipo_documento td ON dr.id_tipo_documento = td.id
            GROUP BY td.nombre_corto ORDER BY value DESC LIMIT 10
        """, fetch="all") or []
        by_parte = db_query("""
            SELECT c.nombre AS label, COUNT(*) AS value
            FROM public.datos_rrhh dr
            JOIN public.tipo_documento td ON dr.id_tipo_documento = td.id
            JOIN public.categoria c ON td.id_categoria = c.id
            WHERE c.slug LIKE 'parte-%%'
            GROUP BY c.nombre, c.id ORDER BY c.id
        """, fetch="all") or []
        totals = db_query("""
            SELECT (SELECT COUNT(*) FROM public.empleados) AS total_employees,
                   (SELECT COUNT(*) FROM public.datos_rrhh) AS total_documents,
                   (SELECT COUNT(*) FROM public.empleados e
                    JOIN public.estados_laborales el ON e.estado_id = el.id
                    WHERE el.estados = 'Activo') AS total_activos,
                   (SELECT COUNT(*) FROM public.empleados e
                    JOIN public.estados_laborales el ON e.estado_id = el.id
                    WHERE el.estados IN ('Jubilado','Pensionado')) AS total_jubilados
        """, fetch="one")
        return {
            "modulo": "RRHH",
            "charts": {
                "by_department": [{"label": r["label"], "value": int(r["value"])} for r in by_dept],
                "by_status":     [{"label": r["label"], "value": int(r["value"])} for r in by_status],
                "by_doc_type":   [{"label": r["label"], "value": int(r["value"])} for r in by_doc_type],
                "by_parte":      [{"label": r["label"], "value": int(r["value"])} for r in by_parte],
                "totals": {k: int(v or 0) for k, v in (dict(totals) if totals else {}).items()},
            }
        }
