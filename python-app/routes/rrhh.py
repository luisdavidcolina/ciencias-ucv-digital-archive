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

@router.post("/buscar", summary="Búsqueda de expedientes de personal")
def search_rrhh(req: RrhhSearchRequest):
    """
    Búsqueda de expedientes RRHH sobre la vista agregada `vw_rrhh_persona_index`.

    La búsqueda es por persona (no por documento): agrupa todos los documentos
    del expediente bajo un único resultado por empleado. Usa FTS español con
    fallback a ILIKE para términos numéricos (cédulas, años).

    Retorna paginación server-side: `{records, total, page, per_page}`.
    Cada registro incluye el conteo de documentos (`doc_count`) y los tipos
    de documento presentes en el expediente (`tipos`).
    """
    page     = max(1, req.page)
    per_page = max(1, min(req.per_page, 50))
    offset   = (page - 1) * per_page

    conditions: list = []
    params: list = []

    import re as _re
    if req.search_term:
        term = f"%{req.search_term}%"
        _has_letters = bool(_re.search(r'[A-Za-zÀ-ÿ]', req.search_term))
        if _has_letters:
            conditions.append(
                "("
                "  to_tsvector('spanish',"
                "    coalesce(v.persona_raw,'') || ' ' ||"
                "    coalesce(v.cargo,'') || ' ' ||"
                "    coalesce(v.departamento,'') || ' ' ||"
                "    coalesce(v.cedula,'') || ' ' ||"
                "    coalesce(v.rif,'')"
                "  ) @@ plainto_tsquery('spanish', %s)"
                "  OR unaccent(v.persona_raw) ILIKE unaccent(%s)"
                "  OR v.cedula ILIKE %s"
                "  OR unaccent(v.cargo) ILIKE unaccent(%s)"
                "  OR unaccent(v.departamento) ILIKE unaccent(%s)"
                ")"
            )
            params.extend([req.search_term, term, term, term, term])
        else:
            conditions.append(
                "(unaccent(v.persona_raw) ILIKE unaccent(%s)"
                " OR v.cedula ILIKE %s"
                " OR unaccent(v.departamento) ILIKE unaccent(%s))"
            )
            params.extend([term, term, term])

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
    base_order = sort_map.get(req.sort_mode, "v.persona_raw ASC")

    import re as _re2
    _search_has_letters = req.search_term and bool(_re2.search(r'[A-Za-zÀ-ÿ]', req.search_term))

    if _search_has_letters:
        order = "relevance DESC, v.persona_raw ASC"
    else:
        order = base_order

    _fts_param = req.search_term if _search_has_letters else ""

    sql = f"""
        SELECT
            v.empleado_id, v.cedula, v.rif, v.persona_raw AS empleado,
            v.cargo, v.departamento, v.estado,
            v.fecha_ingreso, v.foto_url,
            v.fecha_nacimiento, v.nivel_educativo, v.sexo,
            v.doc_count, v.tipos,
            ts_rank_cd(
              to_tsvector('spanish',
                coalesce(v.persona_raw,'') || ' ' || coalesce(v.cargo,'') || ' ' ||
                coalesce(v.departamento,'')
              ),
              plainto_tsquery('spanish', %s)
            ) AS relevance,
            COUNT(*) OVER() AS total_count
        FROM public.vw_rrhh_persona_index v
        {where}
        ORDER BY {order}
        LIMIT %s OFFSET %s
    """
    params = [_fts_param] + params
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
            "cedula":        r["cedula"] or "",
            "rifs":          r["rif"]    or "",
            "departamentos": r["departamento"] or "",
            "cargo":         r["cargo"]  or "Sin cargo asignado",
            "cargos":        r["cargo"]  or "Sin cargo asignado",
            "estado":        r["estado"] or "Sin estado",
            "estatuses":     r["estado"] or "Sin estado",
            "tipos":         tipos_str,
            "fecha_ingreso":    r["fecha_ingreso"] or "",
            "fecha_nacimiento": r["fecha_nacimiento"] or "",
            "nivel_educativo":  r["nivel_educativo"] or "",
            "sexo":             r["sexo"] or "",
            "foto_url":         r["foto_url"] or "",
            "doc_type":         first_tipo,
            "__idx":            offset + i + 1,
        })

    # ── Facetas: conteos por departamento y por estado ──────────────────────
    import re as _ref
    facet_conds: list = []
    facet_params: list = []
    if req.search_term:
        _has_l = bool(_ref.search(r'[A-Za-zÀ-ÿ]', req.search_term))
        term_f = f"%{req.search_term}%"
        if _has_l:
            facet_conds.append(
                "(to_tsvector('spanish',"
                "  coalesce(v.persona_raw,'') || ' ' || coalesce(v.cargo,'') || ' ' || coalesce(v.departamento,'')"
                " ) @@ plainto_tsquery('spanish', %s)"
                " OR unaccent(v.persona_raw) ILIKE unaccent(%s)"
                " OR v.cedula ILIKE %s)"
            )
            facet_params.extend([req.search_term, term_f, term_f])
        else:
            facet_conds.append("(unaccent(v.persona_raw) ILIKE unaccent(%s) OR v.cedula ILIKE %s)")
            facet_params.extend([term_f, term_f])
    if req.date_start:
        facet_conds.append("v.fecha_ingreso >= %s"); facet_params.append(req.date_start)
    if req.date_end:
        facet_conds.append("v.fecha_ingreso <= %s"); facet_params.append(req.date_end)

    facet_where = ("WHERE " + " AND ".join(facet_conds)) if facet_conds else ""

    facet_dept_rows = db_query(
        f"""SELECT COALESCE(NULLIF(v.departamento,''), 'Sin departamento') AS name, COUNT(*) AS cnt
            FROM public.vw_rrhh_persona_index v {facet_where}
            GROUP BY v.departamento ORDER BY cnt DESC LIMIT 15""",
        facet_params or None, fetch="all"
    ) or []

    facet_estado_rows = db_query(
        f"""SELECT COALESCE(NULLIF(v.estado,''), 'Sin estado') AS name, COUNT(*) AS cnt
            FROM public.vw_rrhh_persona_index v {facet_where}
            GROUP BY v.estado ORDER BY cnt DESC LIMIT 10""",
        facet_params or None, fetch="all"
    ) or []

    return {
        "records":  records,
        "total":    total,
        "page":     page,
        "per_page": per_page,
        "facets": {
            "by_dept":   [{"name": r["name"], "count": int(r["cnt"])} for r in facet_dept_rows],
            "by_estado": [{"name": r["name"], "count": int(r["cnt"])} for r in facet_estado_rows],
        },
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

    # Obtener datos adicionales del empleado (LOTTT) via SQL directo
    emp_data = {}
    first_row = row_list[0] if row_list else {}
    if first_row.get("empleado_id"):
        from database import db_query as _dq
        emp_extra = _dq(
            """SELECT TO_CHAR(fecha_nacimiento,'YYYY-MM-DD') AS fecha_nacimiento,
                      COALESCE(nivel_educativo,'') AS nivel_educativo,
                      COALESCE(sexo,'') AS sexo
               FROM public.empleados WHERE id = %s""",
            [first_row["empleado_id"]], fetch="one"
        )
        if emp_extra:
            emp_data = dict(emp_extra)

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
        "fecha_nacimiento": emp_data.get("fecha_nacimiento") or "",
        "nivel_educativo":  emp_data.get("nivel_educativo") or "",
        "sexo":             emp_data.get("sexo") or "",
        "categories":       sorted(set([x for x in p_df["categoria"].dropna().astype(str).tolist() if x.strip()])),
        "rows":             row_list,
    }


@router.get("/empleado/por-cedula/{cedula}")
def get_empleado_por_cedula(cedula: str):
    """Retorna el expediente de un empleado dado su cédula."""
    row = db_query(
        """SELECT e.id AS empleado_id,
                  e.nombres || ' ' || e.apellidos AS persona_raw,
                  e.cedula, e.rif,
                  COALESCE(c.nombre,'')   AS cargo,
                  COALESCE(d.nombre,'')   AS departamento,
                  COALESCE(el.estados,'') AS estado,
                  TO_CHAR(e.fecha_ingreso,'YYYY-MM-DD')    AS fecha_ingreso,
                  TO_CHAR(e.fecha_jubilacion,'YYYY-MM-DD') AS fecha_jubilacion,
                  TO_CHAR(e.fecha_pension,'YYYY-MM-DD')    AS fecha_pension,
                  COALESCE(e.foto_url,'') AS foto_url
           FROM public.empleados e
           LEFT JOIN public.cargos            c  ON e.cargo_id        = c.id
           LEFT JOIN public.departamentos     d  ON e.departamento_id = d.id
           LEFT JOIN public.estados_laborales el ON e.estado_id       = el.id
           WHERE TRIM(e.cedula) = TRIM(%s)""",
        (cedula.strip(),), fetch="one",
    )
    if not row:
        raise HTTPException(404, "Empleado no encontrado")
    return dict(row)


# =============================================================================
# BÚSQUEDA DENTRO DEL EXPEDIENTE
# =============================================================================

@router.get("/empleado/{emp_id}/documentos")
def get_empleado_documentos(
    emp_id: int,
    search: str = "",
    parte: str = "",
    page: int = 1,
    per_page: int = 20,
):
    """Documentos de un empleado con filtro y paginación."""
    page = max(1, page)
    per_page = max(1, min(per_page, 100))
    offset = (page - 1) * per_page
    conditions = ["dr.empleado_id = %s"]
    params: list = [emp_id]
    if search:
        conditions.append(
            "(unaccent(td.nombre_corto) ILIKE unaccent(%s) OR unaccent(COALESCE(dr.notas,'')) ILIKE unaccent(%s))"
        )
        term = f"%{search}%"
        params.extend([term, term])
    if parte:
        conditions.append("c.slug = %s")
        params.append(parte)
    where = "WHERE " + " AND ".join(conditions)
    count_row = db_query(
        f"""SELECT COUNT(*) AS total FROM public.datos_rrhh dr
            LEFT JOIN public.tipo_documento td ON dr.id_tipo_documento=td.id_tipo_documento
            LEFT JOIN public.categoria c ON td.id_categoria=c.id
            {where}""",
        params, fetch="one"
    )
    total = int(count_row["total"]) if count_row else 0
    rows = db_query(
        f"""SELECT dr.id_rrhh,
                   TO_CHAR(dr.fecha_documento,'YYYY-MM-DD') AS fecha,
                   dr.notas, dr.ubicacion, dr.file_url, dr.personas_relacionadas,
                   COALESCE(td.nombre_corto,'Sin tipo') AS tipo_nombre,
                   COALESCE(c.nombre,'Sin clasificar') AS parte_nombre,
                   COALESCE(c.slug,'') AS parte_slug
            FROM public.datos_rrhh dr
            LEFT JOIN public.tipo_documento td ON dr.id_tipo_documento=td.id_tipo_documento
            LEFT JOIN public.categoria c ON td.id_categoria=c.id
            {where}
            ORDER BY c.id NULLS LAST, dr.fecha_documento DESC
            LIMIT %s OFFSET %s""",
        params + [per_page, offset], fetch="all"
    ) or []
    return {
        "total": total, "page": page, "per_page": per_page,
        "records": [dict(r) for r in rows],
    }


# =============================================================================
# REPORTE HTML IMPRIMIBLE DEL EXPEDIENTE
# =============================================================================

from fastapi.responses import HTMLResponse as _HTMLResponse


@router.get("/report/{emp_id}", response_class=_HTMLResponse)
def generate_rrhh_report(emp_id: int):
    """Genera reporte HTML imprimible del expediente de un empleado."""
    emp = db_query("""
        SELECT e.id, e.cedula, e.nombres, e.apellidos, e.rif,
               e.fecha_jubilacion, e.fecha_pension,
               e.fecha_nacimiento, e.nivel_educativo, e.sexo,
               TO_CHAR(e.fecha_ingreso, 'YYYY-MM-DD') AS fecha_ingreso,
               COALESCE(c.nombre,'—') AS cargo,
               COALESCE(d.nombre,'—') AS departamento,
               COALESCE(el.estados,'—') AS estado
        FROM public.empleados e
        LEFT JOIN public.cargos c ON e.cargo_id = c.id
        LEFT JOIN public.departamentos d ON e.departamento_id = d.id
        LEFT JOIN public.estados_laborales el ON e.estado_id = el.id
        WHERE e.id=%s
    """,[emp_id],fetch="one")
    if not emp:
        raise HTTPException(404,"Empleado no encontrado")
    emp = dict(emp)
    docs = db_query("""
        SELECT dr.id_rrhh, dr.fecha_documento, dr.notas, dr.ubicacion, dr.file_url,
               COALESCE(td.nombre_corto,'Sin tipo') AS tipo_nombre,
               COALESCE(c.nombre,'Sin clasificar') AS parte_nombre,
               COALESCE(c.id, 999) AS parte_orden
        FROM public.datos_rrhh dr
        LEFT JOIN public.tipo_documento td ON dr.id_tipo_documento=td.id_tipo_documento
        LEFT JOIN public.categoria c ON td.id_categoria=c.id
        WHERE dr.empleado_id=%s ORDER BY parte_orden, dr.fecha_documento DESC
    """,[emp_id],fetch="all") or []
    partes = {}
    for d in docs:
        k = d["parte_nombre"]
        partes.setdefault(k,[]).append(dict(d))

    # Historial de cargos para la sección adicional del reporte
    historial_cargos = db_query("""
        SELECT c.nombre AS cargo,
               TO_CHAR(hc.fecha_inicio,'YYYY-MM-DD') AS fecha_inicio,
               TO_CHAR(hc.fecha_fin,'YYYY-MM-DD')    AS fecha_fin,
               hc.motivo
        FROM public.historial_cargos hc
        JOIN public.cargos c ON hc.cargo_id = c.id
        WHERE hc.empleado_id = %s
        ORDER BY hc.fecha_inicio DESC
    """, [emp_id], fetch="all") or []

    def fd(v):
        if not v: return "—"
        return str(v)[:10]
    colores = {
        "Parte I — Ingreso y Contratación":"#0d6efd",
        "Parte II — Escalafón y Desarrollo":"#198754",
        "Parte III — Permisos y Formación":"#fd7e14",
        "Parte IV — Documentos Personales":"#6f42c1",
    }
    nombre_completo = f"{emp.get('apellidos','')}, {emp.get('nombres','')}".strip(", ")
    _SEXO_MAP = {"M": "Masculino", "F": "Femenino", "O": "Otro"}
    rows_html = ""
    for pn, pdocs in partes.items():
        col = colores.get(pn,"#6c757d")
        rows_html += f'<tr style="background:{col}18"><td colspan="4" style="font-weight:700;color:{col};border-left:4px solid {col};padding:8px 14px">{pn} <span style="font-weight:normal;font-size:.8rem">({len(pdocs)} documentos)</span></td></tr>'
        for d in pdocs:
            rows_html += f'<tr><td style="padding:6px 14px;border-bottom:1px solid #eee">{d["tipo_nombre"]}</td><td style="padding:6px 14px;border-bottom:1px solid #eee">{fd(d["fecha_documento"])}</td><td style="padding:6px 14px;border-bottom:1px solid #eee;color:#555;font-size:.85rem">{d["notas"] or "—"}</td><td style="padding:6px 14px;border-bottom:1px solid #eee;color:#555;font-size:.85rem">{d["ubicacion"] or "—"}</td></tr>'
    if not rows_html:
        rows_html = '<tr><td colspan="4" style="text-align:center;padding:24px;color:#999">Sin documentos registrados</td></tr>'
    from datetime import datetime as _dt
    now_str = _dt.now().strftime("%d/%m/%Y %H:%M")
    html = f"""<!DOCTYPE html><html lang="es"><head><meta charset="UTF-8">
<title>Expediente — {nombre_completo}</title>
<style>
@media print{{@page{{size:A4;margin:2cm}}button{{display:none!important}}}}
*{{box-sizing:border-box;margin:0;padding:0}}
body{{font-family:'Segoe UI',Arial,sans-serif;color:#222;background:#fff}}
.header{{border-bottom:3px solid #003366;padding-bottom:16px;margin-bottom:20px;display:flex;justify-content:space-between;align-items:flex-end}}
.logo h1{{color:#003366;font-size:1.3rem}}
.logo p{{color:#666;font-size:.82rem;margin-top:2px}}
.print-date{{font-size:.78rem;color:#999;text-align:right}}
.emp-grid{{display:grid;grid-template-columns:repeat(3,1fr);gap:10px;background:#f8f9fa;border:1px solid #dee2e6;border-radius:8px;padding:14px;margin-bottom:18px}}
.ef label{{font-size:.7rem;text-transform:uppercase;color:#888;display:block}}
.ef span{{font-weight:600;color:#222}}
.stats{{display:flex;gap:16px;margin-bottom:18px}}
.sbox{{background:#003366;color:#fff;border-radius:8px;padding:10px 18px;text-align:center}}
.sbox .n{{font-size:1.7rem;font-weight:700}}
.sbox .l{{font-size:.7rem;opacity:.8}}
table{{width:100%;border-collapse:collapse;font-size:.88rem}}
th{{background:#003366;color:#fff;padding:8px 14px;text-align:left;font-size:.82rem}}
tr:nth-child(even) td{{background:#f9f9f9}}
.pbtn{{position:fixed;top:14px;right:14px;background:#003366;color:#fff;border:none;padding:9px 18px;border-radius:6px;cursor:pointer;font-size:.88rem;z-index:999}}
.badge{{display:inline-block;padding:2px 8px;border-radius:10px;font-size:.75rem;font-weight:600}}
.badge-activo{{background:#d1e7dd;color:#0f5132}}
.badge-retirado{{background:#f8d7da;color:#842029}}
.badge-jubilado,.badge-pensionado{{background:#e2d9f3;color:#432874}}
</style></head><body>
<button class="pbtn" onclick="window.print()">Imprimir / PDF</button>
<div style="max-width:900px;margin:30px auto;padding:20px">
<div class="header">
<div class="logo">
<h1>Universidad Central de Venezuela — Facultad de Ciencias</h1>
<p>Dirección de Recursos Humanos · Archivo Institucional Digital</p>
<p style="margin-top:6px;font-size:.95rem;font-weight:600;color:#003366">EXPEDIENTE DEL PERSONAL DOCENTE Y DE INVESTIGACIÓN</p>
</div>
<div class="print-date">Generado: {now_str}<br>N.° Expediente: {emp_id}</div>
</div>
<div class="emp-grid">
<div class="ef"><label>Nombre Completo</label><span>{nombre_completo}</span></div>
<div class="ef"><label>Cédula</label><span>{emp.get('cedula','—')}</span></div>
<div class="ef"><label>Estado</label><span class="badge badge-{str(emp.get('estado','') or '').lower()}">{emp.get('estado','—')}</span></div>
<div class="ef"><label>Cargo</label><span>{emp.get('cargo','—')}</span></div>
<div class="ef"><label>Departamento</label><span>{emp.get('departamento','—')}</span></div>
<div class="ef"><label>RIF</label><span>{emp.get('rif','—') or '—'}</span></div>
<div class="ef"><label>Fecha de Ingreso</label><span>{fd(emp.get('fecha_ingreso'))}</span></div>
<div class="ef"><label>Jubilación</label><span>{fd(emp.get('fecha_jubilacion'))}</span></div>
<div class="ef"><label>Pensión</label><span>{fd(emp.get('fecha_pension'))}</span></div>
<div class="ef"><label>Fecha de Nacimiento</label><span>{fd(emp.get('fecha_nacimiento'))}</span></div>
<div class="ef"><label>Nivel Educativo</label><span>{emp.get('nivel_educativo') or '—'}</span></div>
<div class="ef"><label>Sexo</label><span>{_SEXO_MAP.get(str(emp.get('sexo') or ''),'—')}</span></div>
</div>
<div class="stats">
<div class="sbox"><div class="n">{len(docs)}</div><div class="l">Documentos</div></div>
<div class="sbox"><div class="n">{len(partes)}</div><div class="l">Secciones</div></div>
</div>
<table>
<thead><tr><th>Tipo de Documento</th><th>Fecha</th><th>Notas / Descripción</th><th>Ubicación</th></tr></thead>
<tbody>{rows_html}</tbody>
</table>
{f'''
<h3 style="margin-top:24px;margin-bottom:10px;font-size:.95rem;color:#003366;border-bottom:2px solid #003366;padding-bottom:6px">
  Historial de Cargos
</h3>
<table>
  <thead><tr><th>Cargo</th><th>Desde</th><th>Hasta</th><th>Motivo</th></tr></thead>
  <tbody>
    {''.join(
        f"<tr><td style='padding:6px 14px;border-bottom:1px solid #eee'>{h['cargo']}</td>"
        f"<td style='padding:6px 14px;border-bottom:1px solid #eee'>{h['fecha_inicio'] or '—'}</td>"
        f"<td style='padding:6px 14px;border-bottom:1px solid #eee'>{h['fecha_fin'] or '<span style=\"color:#198754;font-weight:600\">Actual</span>'}</td>"
        f"<td style='padding:6px 14px;border-bottom:1px solid #eee;color:#555;font-size:.85rem'>{h['motivo'] or '—'}</td></tr>"
        for h in historial_cargos
    ) if historial_cargos else "<tr><td colspan='4' style='text-align:center;padding:14px;color:#999'>Sin historial registrado</td></tr>"}
  </tbody>
</table>
''' if historial_cargos is not None else ''}
<p style="margin-top:20px;font-size:.72rem;color:#aaa;text-align:center">
Documento generado automáticamente por el Sistema de Archivo Institucional — Ciencias UCV. Confidencial. Solo para uso oficial.
</p>
</div></body></html>"""
    return _HTMLResponse(content=html)
