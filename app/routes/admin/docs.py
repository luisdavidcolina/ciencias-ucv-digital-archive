"""CRUD de documentos y empleados en el panel de administración.

Autorización (OA-035, IN-131, IN-132): cada endpoint exige `require_role`
además de `require_session` heredado del router de `/api/admin`. Este
archivo sirve documentos de dos módulos distintos según el parámetro
`modulo` de la petición (Archivo o RRHH), así que los endpoints
compartidos exigen pertenecer a cualquiera de los dos; los que sólo tienen
sentido para expedientes de personal (`/empleado/*`) exigen "RRHH". Ninguno
de los borrados de este archivo es definitivo (son soft-delete a
papelera), así que usan `require_role`, no `require_admin_role` —eso queda
para purgar en `trash.py`.
"""
import re
from datetime import datetime
from typing import Optional

from fastapi import APIRouter, Depends, HTTPException, Query

from database import db_query, log_event
from models import DocumentSubmitRequest, DocumentUpdateRequest, EmpleadoUpdateRequest
from .deps import require_session, require_role
from .helpers import (
    _resolve_or_create_lookup,
    _resolve_or_create_tipo_documento,
    _resolve_user_id,
    _require_modulo,
    invalidate_choices_cache,
    module_meta,
    paginate,
    upsert_descriptors,
)

router = APIRouter()

VALID_STATUS = ("draft", "revision", "aprobado", "rechazado")

# OA-104/OR-128: orden real server-side sobre /list_all. Lista blanca de
# columnas ordenables por módulo — nunca se interpola `sort` directo en el
# SQL, sólo se usa para elegir una expresión ya escrita aquí. Un valor fuera
# de la lista cae al orden por defecto de siempre, en vez de dar 400: el
# frontend puede mandar un `sort` obsoleto sin romper la página.
_SORT_COLUMNS = {
    "Archivo": {
        "titulo": "da.titulo",
        "autor": "da.autor",
        "fecha": "da.fecha_documento",
        "doc_type": "da.tesauro_primario",
        "status": "da.status",
        "soporte": "da.soporte",
    },
    "RRHH": {
        "empleado": "e.apellidos, e.nombres",
        "cedula": "e.cedula",
        "departamento": "d.nombre",
        "estado": "el.estados",
        "cargo": "c.nombre",
        "fecha_ingreso": "e.fecha_ingreso",
        "doc_count": "COUNT(DISTINCT dr.id_rrhh)",
    },
}


@router.get("/list_all")
def list_all_files(
    modulo: str,
    search: Optional[str] = "",
    type_filter: Optional[str] = "",
    person_filter: Optional[str] = "",
    status_filter: Optional[str] = "",
    # OR-126: `status_filter` en Archivo significa estado del documento
    # (draft/revision/aprobado/rechazado); en RRHH significa estado laboral
    # (catálogo `estados_laborales`), un concepto distinto que merece su
    # propio parámetro en vez de compartir semántica por nombre.
    estado_laboral_filter: Optional[str] = "",
    # OR-125: reemplaza al desplegable «Persona…» en RRHH, que duplicaba al
    # buscador con un <select> de 400 opciones sin filtro propio de búsqueda.
    department_filter: Optional[str] = "",
    sort: Optional[str] = None,
    dir: Optional[str] = "asc",
    page: int = 1,
    per_page: int = 25,
    usuario_sesion: str = Depends(require_session),
    _autorizado: str = Depends(require_role("Archivo", "RRHH")),
):
    if modulo not in ("Archivo", "RRHH"):
        raise HTTPException(400, "modulo debe ser 'Archivo' o 'RRHH'")

    page, per_page, offset = paginate(page, per_page)

    sort_dir = "DESC" if (dir or "").lower() == "desc" else "ASC"
    sort_expr = _SORT_COLUMNS.get(modulo, {}).get(sort or "")
    # `empleado` son dos columnas (apellidos, nombres): la dirección se aplica
    # a cada una, si no la segunda quedaría siempre en ASC.
    order_by = (
        ", ".join(f"{col.strip()} {sort_dir}" for col in sort_expr.split(","))
        if sort_expr else None
    )

    if modulo == "Archivo":
        conditions, params = ["da.deleted_at IS NULL"], []
        if search:
            _has_letters = bool(re.search(r'[A-Za-zÀ-ÿ]', search))
            if _has_letters:
                conditions.append(
                    "(to_tsvector('spanish', coalesce(da.titulo,'') || ' ' || coalesce(da.autor,'')) "
                    "@@ plainto_tsquery('spanish', %s)"
                    " OR unaccent(da.titulo) ILIKE unaccent(%s)"
                    " OR unaccent(COALESCE(da.autor,'')) ILIKE unaccent(%s))"
                )
                params.extend([search, f"%{search}%", f"%{search}%"])
            else:
                conditions.append(
                    "(unaccent(da.titulo) ILIKE unaccent(%s) OR unaccent(COALESCE(da.autor,'')) ILIKE unaccent(%s))"
                )
                params.extend([f"%{search}%", f"%{search}%"])
        if type_filter:
            conditions.append("da.tesauro_primario = %s")
            params.append(type_filter)
        if person_filter:
            conditions.append("unaccent(COALESCE(da.autor,'')) ILIKE unaccent(%s)")
            params.append(f"%{person_filter}%")
        if status_filter and status_filter in ("aprobado", "revision", "draft", "rechazado"):
            conditions.append("COALESCE(da.status, 'aprobado') = %s")
            params.append(status_filter)

        where = ("WHERE " + " AND ".join(conditions)) if conditions else ""

        count_row = db_query(
            f"SELECT COUNT(*) AS total FROM public.datos_archivo da {where}",
            params or None, fetch="one",
        )
        total = int(count_row["total"]) if count_row else 0

        rows = db_query(
            f"""
            SELECT
                da.id_archivo AS id,
                da.titulo,
                COALESCE(da.autor, '')            AS autor,
                TO_CHAR(da.fecha_documento, 'YYYY-MM-DD') AS fecha,
                COALESCE(da.tesauro_primario, '')  AS doc_type,
                COALESCE(da.tesauro_secundario,'') AS tesauro_secundario,
                COALESCE(da.ubicacion, '')         AS ubicacion,
                COALESCE(da.abstract, '')          AS resumen,
                COALESCE(da.file_url, '')          AS file_url,
                COALESCE(da.status, 'aprobado')   AS status,
                COALESCE(da.numero_folio,'')       AS numero_folio,
                COALESCE(da.soporte,'Físico')      AS soporte,
                da.numero_paginas,
                COALESCE(da.disposicion, '')       AS disposicion
            FROM public.datos_archivo da
            {where}
            ORDER BY {order_by or "da.fecha_documento DESC"} NULLS LAST
            LIMIT %s OFFSET %s
            """,
            (params + [per_page, offset]) if params else [per_page, offset],
            fetch="all",
        ) or []

        records = [dict(r) for r in rows]
        for idx, r in enumerate(records):
            r["__idx"] = offset + idx + 1

    else:
        conditions, params = [], []
        if search:
            _has_letters = bool(re.search(r'[A-Za-zÀ-ÿ]', search))
            if _has_letters:
                conditions.append(
                    "(to_tsvector('spanish', coalesce(e.nombres,'') || ' ' || coalesce(e.apellidos,'')) "
                    "@@ plainto_tsquery('spanish', %s)"
                    " OR unaccent(e.nombres || ' ' || e.apellidos) ILIKE unaccent(%s)"
                    " OR e.cedula ILIKE %s)"
                )
                params.extend([search, f"%{search}%", f"%{search}%"])
            else:
                conditions.append(
                    "(unaccent(e.nombres || ' ' || e.apellidos) ILIKE unaccent(%s) OR e.cedula ILIKE %s)"
                )
                params.extend([f"%{search}%", f"%{search}%"])
        if type_filter:
            conditions.append("COALESCE(td.nombre_corto, td.nombre) = %s")
            params.append(type_filter)
        if person_filter:
            conditions.append("unaccent(e.nombres || ' ' || e.apellidos) ILIKE unaccent(%s)")
            params.append(f"%{person_filter}%")
        # OR-126: igualdad exacta contra el catálogo `estados_laborales`, no
        # `ILIKE '%...%'` — con ese comodín, filtrar "Activo" también traía
        # "Reactivado" o "Inactivo" en cuanto ese estado existiera. Un valor
        # que no está en el catálogo (URL manipulada, catálogo cambiado) se
        # ignora en vez de devolver un 400: coincide con cómo se trata ya
        # `status_filter` en Archivo (fuera de la lista blanca -> se ignora).
        if estado_laboral_filter:
            _valid_estado = db_query(
                "SELECT 1 FROM public.estados_laborales WHERE estados = %s",
                [estado_laboral_filter], fetch="one",
            )
            if _valid_estado:
                conditions.append("el.estados = %s")
                params.append(estado_laboral_filter)
        if department_filter:
            conditions.append("d.nombre = %s")
            params.append(department_filter)

        join = """
            FROM public.empleados e
            LEFT JOIN public.cargos c ON e.cargo_id = c.id
            LEFT JOIN public.departamentos d ON e.departamento_id = d.id
            LEFT JOIN public.estados_laborales el ON e.estado_id = el.id
            LEFT JOIN public.datos_rrhh dr ON dr.empleado_id = e.id AND dr.deleted_at IS NULL
            LEFT JOIN public.tipo_documento td ON dr.id_tipo_documento = td.id
            LEFT JOIN public.categoria cat ON td.id_categoria = cat.id
        """
        conditions.insert(0, "e.deleted_at IS NULL")
        where = ("WHERE " + " AND ".join(conditions)) if conditions else ""

        count_row = db_query(
            f"SELECT COUNT(DISTINCT e.id) AS total {join} {where}",
            params or None, fetch="one",
        )
        total = int(count_row["total"]) if count_row else 0

        rows = db_query(
            f"""
            SELECT
                e.id AS empleado_id,
                e.cedula,
                e.apellidos || ', ' || e.nombres AS empleado,
                COALESCE(e.rif, '')                    AS rif,
                COALESCE(d.nombre, '')                 AS departamento,
                COALESCE(el.estados, '')               AS estado,
                COALESCE(c.nombre, '')                 AS cargo,
                TO_CHAR(e.fecha_ingreso, 'YYYY-MM-DD') AS fecha_ingreso,
                TO_CHAR(e.fecha_nacimiento, 'YYYY-MM-DD') AS fecha_nacimiento,
                COALESCE(e.nivel_educativo, '')        AS nivel_educativo,
                COALESCE(e.sexo, '')                   AS sexo,
                TO_CHAR(e.updated_at, 'YYYY-MM-DD')    AS updated_at,
                COUNT(DISTINCT dr.id_rrhh)             AS doc_count,
                COALESCE(STRING_AGG(DISTINCT COALESCE(td.nombre_corto, td.nombre), '; '), '')
                                                       AS tipos,
                COALESCE(MIN(dr.ubicacion), '')        AS ubicacion,
                COALESCE(e.foto_url, '')               AS foto_url,
                COUNT(DISTINCT dr.id_rrhh) FILTER (WHERE cat.slug = 'parte-i')   AS partes_i,
                COUNT(DISTINCT dr.id_rrhh) FILTER (WHERE cat.slug = 'parte-ii')  AS partes_ii,
                COUNT(DISTINCT dr.id_rrhh) FILTER (WHERE cat.slug = 'parte-iii') AS partes_iii,
                COUNT(DISTINCT dr.id_rrhh) FILTER (WHERE cat.slug = 'parte-iv')  AS partes_iv
            {join}
            {where}
            GROUP BY e.id, e.cedula, e.apellidos, e.nombres, e.rif, d.nombre,
                     el.estados, c.nombre, e.fecha_ingreso, e.fecha_nacimiento,
                     e.nivel_educativo, e.sexo, e.updated_at, e.foto_url
            ORDER BY {order_by or "e.apellidos ASC, e.nombres ASC"}
            LIMIT %s OFFSET %s
            """,
            (params + [per_page, offset]) if params else [per_page, offset],
            fetch="all",
        ) or []

        records = [dict(r) for r in rows]
        for idx, r in enumerate(records):
            r["__idx"] = offset + idx + 1

    return {
        "total":    total,
        "page":     page,
        "per_page": per_page,
        "records":  records,
    }


@router.get("/documento/{doc_id}")
def get_documento(
    doc_id: int,
    modulo: str = "Archivo",
    usuario_sesion: str = Depends(require_session),
    _autorizado: str = Depends(require_role("Archivo", "RRHH")),
):
    _require_modulo(modulo)
    if modulo == "Archivo":
        row = db_query(
            """SELECT id_archivo AS id, titulo, autor, abstract AS resumen,
                      TO_CHAR(fecha_documento,'YYYY-MM-DD')    AS fecha,
                      COALESCE(tesauro_primario,'')            AS doc_type,
                      COALESCE(tesauro_secundario,'')          AS tesauro_secundario,
                      COALESCE(ubicacion,'')                   AS ubicacion,
                      COALESCE(file_url,'')                    AS file_url,
                      COALESCE(status,'aprobado')              AS status,
                      COALESCE(personas_relacionadas,'')       AS personas_relacionadas,
                      COALESCE(numero_folio,'')                AS numero_folio,
                      COALESCE(soporte,'Físico')               AS soporte,
                      numero_paginas,
                      COALESCE(idioma,'es')                    AS idioma,
                      TO_CHAR(fecha_vencimiento,'YYYY-MM-DD')  AS fecha_vencimiento,
                      updated_at, updated_by
               FROM public.datos_archivo WHERE id_archivo = %s AND deleted_at IS NULL""",
            [doc_id], fetch="one"
        )
        if row:
            desc_rows = db_query(
                """SELECT dl.nombre FROM public.archivo_descriptores ad
                   JOIN public.descriptores_libres dl ON dl.id_descriptor = ad.id_descriptor
                   WHERE ad.id_archivo = %s ORDER BY dl.nombre""",
                [doc_id], fetch="all"
            ) or []
            row = dict(row)
            row["palabras_clave"] = ", ".join(d["nombre"] for d in desc_rows)
    else:
        row = db_query(
            """SELECT dr.id_rrhh AS id, dr.notas AS resumen,
                      TO_CHAR(dr.fecha_documento,'YYYY-MM-DD') AS fecha,
                      COALESCE(dr.ubicacion,'') AS ubicacion,
                      COALESCE(dr.file_url,'') AS file_url,
                      COALESCE(dr.status,'aprobado') AS status,
                      COALESCE(dr.personas_relacionadas,'') AS personas_relacionadas,
                      COALESCE(td.nombre,'') AS doc_type,
                      e.nombres || ' ' || e.apellidos AS empleado,
                      dr.updated_at, dr.updated_by
               FROM public.datos_rrhh dr
               LEFT JOIN public.tipo_documento td ON dr.id_tipo_documento = td.id
               LEFT JOIN public.empleados e ON dr.empleado_id = e.id
               WHERE dr.id_rrhh = %s AND dr.deleted_at IS NULL""",
            [doc_id], fetch="one"
        )
    if not row:
        raise HTTPException(404, "Documento no encontrado")
    return dict(row)


@router.post("/submit")
def admin_submit(
    req: DocumentSubmitRequest,
    usuario_sesion: str = Depends(require_session),
    _autorizado: str = Depends(require_role("Archivo", "RRHH")),
):
    # IN-133: el actor de auditoría y de `creado_por` sale de la sesión
    # verificada (`require_session`), no de `req.usuario` —un campo que el
    # propio cliente rellena en el cuerpo de la petición y que antes se
    # escribía tal cual en la columna, permitiendo falsificar quién crea un
    # documento, no sólo el registro de auditoría.
    log_event(usuario_sesion, "Create Document", req.modulo, f"Tipo: {req.doc_type}, Ubicacion: {req.ubicacion}, Titulo: {(req.titulo or req.empleado or '')[:60]}")
    creado_por = _resolve_user_id(usuario_sesion)
    fecha_doc  = req.fecha or datetime.now().strftime("%Y-%m-%d")

    if req.modulo == "Archivo":
        tipo_id = _resolve_or_create_tipo_documento(req.doc_type, cat_slug="archivo")
        new_row = db_query(
            """
            INSERT INTO public.datos_archivo
                (titulo, abstract, autor,
                 fecha_documento, ubicacion, creado_por, tesauro_primario,
                 tesauro_secundario, id_tipo_documento,
                 numero_folio, soporte, numero_paginas, file_url, fecha_vencimiento)
            VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
            RETURNING id_archivo
            """,
            (
                req.titulo or "Sin título",
                req.resumen or "",
                req.autor or "Anónimo",
                fecha_doc, req.ubicacion, creado_por, req.doc_type,
                req.tesauro_secundario or "", tipo_id,
                req.numero_folio or None,
                req.soporte or "Físico",
                req.numero_paginas or None,
                req.file_url or None,
                getattr(req, "fecha_vencimiento", None) or None,
            ),
            fetch="one",
            commit=True,
        )

        upsert_descriptors(req.descriptores_libres, new_row["id_archivo"],
                           "archivo_descriptores", "id_archivo")
        invalidate_choices_cache()
        return {"success": True, "id": str(new_row["id_archivo"])}

    # ── Módulo RRHH ──────────────────────────────────────────────────────────
    tipo_id = _resolve_or_create_tipo_documento(req.doc_type)
    cedula = (req.cedula or "").strip()
    if not cedula:
        raise HTTPException(status_code=400, detail="Cédula es requerida para RRHH")

    emp_row = db_query(
        "SELECT id, deleted_at FROM public.empleados WHERE cedula = %s", (cedula,), fetch="one"
    )
    if emp_row and emp_row["deleted_at"] is not None:
        # OR-032: sin esto, el documento se ataba a un expediente en papelera
        # y desaparecía con él en cuanto se filtrara `deleted_at` en las
        # consultas (como ya hace `/list_all`) — silencioso, sin aviso.
        raise HTTPException(
            status_code=409,
            detail="Existe un expediente con esta cédula en la papelera. Restáurelo antes de archivar un documento nuevo.",
        )
    if not emp_row:
        cargo_id   = _resolve_or_create_lookup("cargos",            req.cargo,        "Por Asignar")
        dept_id    = _resolve_or_create_lookup("departamentos",     req.departamento, "Por Asignar")
        estado_id  = _resolve_or_create_lookup("estados_laborales", req.estado,       "Pendiente de Registro")
        emp_row = db_query(
            """
            INSERT INTO public.empleados
                (cedula, nombres, apellidos, rif, cargo_id, departamento_id,
                 estado_id, fecha_ingreso, fecha_jubilacion, fecha_pension, foto_url,
                 fecha_nacimiento, nivel_educativo, sexo)
            VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
            RETURNING id
            """,
            (
                cedula,
                (req.nombres or req.empleado or cedula).strip(),
                (req.apellidos or "").strip(),
                req.rif or None,
                cargo_id, dept_id, estado_id, fecha_doc,
                req.fecha_jubilacion or None, req.fecha_pension or None, req.foto_url or None,
                getattr(req, "fecha_nacimiento", None) or None,
                getattr(req, "nivel_educativo", None) or None,
                getattr(req, "sexo", None) or None,
            ),
            fetch="one",
            commit=True,
        )

    new_row = db_query(
        """
        INSERT INTO public.datos_rrhh
            (titulo, autor, id_tipo_documento,
             empleado_id, fecha_documento, ubicacion, creado_por,
             tesauro_primario, tesauro_secundario, abstract, personas_relacionadas, file_url)
        VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
        RETURNING id_rrhh
        """,
        (
            # OR-100: `personas_relacionadas` es texto libre (admite listas
            # con ";") pensado para el campo de metadata, no para componer un
            # título — "Susana Pérez; Dirección RRHH" salía tal cual como
            # título del documento. Se usa `req.titulo` si viene informado, y
            # el título compuesto queda sólo como valor por defecto.
            (req.titulo or "").strip() or f"{req.doc_type} de {req.personas_relacionadas or req.empleado}",
            "Recursos Humanos",
            tipo_id, emp_row["id"], fecha_doc, req.ubicacion, creado_por,
            req.doc_type, req.tesauro_secundario or "", req.resumen or "",
            req.personas_relacionadas or "",
            req.file_url or None,
        ),
        fetch="one",
        commit=True,
    )

    upsert_descriptors(req.descriptores_libres, new_row["id_rrhh"],
                       "rrhh_descriptores", "id_rrhh")
    invalidate_choices_cache()
    return {"success": True, "id": str(new_row["id_rrhh"])}


@router.put("/documento/{doc_id}")
def update_documento(
    doc_id: int,
    req: DocumentUpdateRequest,
    usuario_sesion: str = Depends(require_session),
    _autorizado: str = Depends(require_role("Archivo", "RRHH")),
):
    # IN-133: mismo criterio que en `admin_submit` — `updated_by` sale de la
    # sesión verificada, no de `req.usuario`.
    updated_by = _resolve_user_id(usuario_sesion)
    updated_at = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    def _common(sc, p):
        """Append shared fields present in both Archivo and RRHH."""
        if req.titulo is not None:
            sc.append("titulo = %s"); p.append(req.titulo)
        if req.autor is not None:
            sc.append("autor = %s"); p.append(req.autor)
        if req.resumen is not None:
            sc.append("abstract = %s"); p.append(req.resumen)
        if req.fecha is not None:
            sc.append("fecha_documento = %s"); p.append(req.fecha)
        if req.ubicacion is not None:
            sc.append("ubicacion = %s"); p.append(req.ubicacion)
        if req.tesauro_secundario is not None:
            sc.append("tesauro_secundario = %s"); p.append(req.tesauro_secundario)
        if req.file_url is not None:
            sc.append("file_url = %s"); p.append(req.file_url or None)
        if req.status is not None and req.status in VALID_STATUS:
            sc.append("status = %s"); p.append(req.status)
        if req.personas_relacionadas is not None:
            sc.append("personas_relacionadas = %s"); p.append(req.personas_relacionadas or None)
        sc.append("updated_at = %s"); p.append(updated_at)
        sc.append("updated_by = %s"); p.append(updated_by)

    if req.modulo == "Archivo":
        set_clauses, params = [], []
        _common(set_clauses, params)

        if req.doc_type is not None:
            tipo_id = _resolve_or_create_tipo_documento(req.doc_type, cat_slug="archivo")
            set_clauses.append("id_tipo_documento = %s"); params.append(tipo_id)
            set_clauses.append("tesauro_primario = %s"); params.append(req.doc_type)

        # Campos ISAD(G) / ISO 15489
        if req.numero_folio is not None:
            set_clauses.append("numero_folio = %s"); params.append(req.numero_folio or None)
        if req.soporte is not None and req.soporte in ("Físico", "Digital", "Digitalizado"):
            set_clauses.append("soporte = %s"); params.append(req.soporte)
        if req.numero_paginas is not None:
            set_clauses.append("numero_paginas = %s"); params.append(req.numero_paginas or None)
        if req.idioma is not None and req.idioma in ("es", "en", "fr", "pt"):
            set_clauses.append("idioma = %s"); params.append(req.idioma)
        if req.fecha_vencimiento is not None:
            set_clauses.append("fecha_vencimiento = %s"); params.append(req.fecha_vencimiento or None)

        if set_clauses:
            params.append(doc_id)
            db_query(
                f"UPDATE public.datos_archivo SET {', '.join(set_clauses)} WHERE id_archivo = %s",
                params, fetch="none", commit=True,
            )

        if req.palabras_clave is not None:
            db_query(
                "DELETE FROM public.archivo_descriptores WHERE id_archivo = %s",
                (doc_id,), fetch="none", commit=True,
            )
            upsert_descriptors(req.palabras_clave, doc_id,
                               "archivo_descriptores", "id_archivo")

        invalidate_choices_cache()
        log_event(usuario_sesion, "Update Document", "Archivo", f"ID: {doc_id}, Titulo: {(req.titulo or '')[:60]}, Status: {req.status or 'aprobado'}")
        return {"success": True}

    else:  # RRHH
        set_clauses, params = [], []
        _common(set_clauses, params)

        if req.doc_type is not None:
            # OR-007: `tesauro_primario` es sólo el texto mostrado; el monitor,
            # las gráficas, la cobertura por Parte y la retención leen
            # `id_tipo_documento`. Sin resolverlo aquí, cambiar el tipo desde
            # el modal de edición no cambiaba nada de lo que de verdad se usa.
            tipo_id = _resolve_or_create_tipo_documento(req.doc_type)
            set_clauses.append("id_tipo_documento = %s"); params.append(tipo_id)
            set_clauses.append("tesauro_primario = %s"); params.append(req.doc_type)
        if req.notas is not None:
            set_clauses.append("notas = %s"); params.append(req.notas)

        if set_clauses:
            params.append(doc_id)
            db_query(
                f"UPDATE public.datos_rrhh SET {', '.join(set_clauses)} WHERE id_rrhh = %s",
                params, fetch="none", commit=True,
            )

        invalidate_choices_cache()
        log_event(usuario_sesion, "Update Document", "RRHH", f"ID: {doc_id}, Status: {req.status or 'aprobado'}")
        return {"success": True}


@router.delete("/documento/{doc_id}")
def delete_documento(
    doc_id: int,
    modulo: str,
    usuario: str,
    deleted_reason: Optional[str] = Query(default=None, max_length=500),
    usuario_sesion: str = Depends(require_session),
    _autorizado: str = Depends(require_role("Archivo", "RRHH")),
):
    """Soft-delete: marca el documento como eliminado (papelera). No borra físicamente."""
    _require_modulo(modulo)
    now = datetime.utcnow().isoformat()
    table, pk = module_meta(modulo)
    result = db_query(
        f"UPDATE public.{table} SET deleted_at=%s, deleted_by=%s, deleted_reason=%s "
        f"WHERE {pk}=%s AND deleted_at IS NULL RETURNING {pk}",
        [now, usuario, (deleted_reason or None), doc_id], fetch="one", commit=True,
    )

    if not result:
        raise HTTPException(404, "Documento no encontrado o ya eliminado")

    invalidate_choices_cache()
    log_event(usuario, "Delete Document (soft)", modulo, f"ID: {doc_id}")
    return {"success": True}


@router.patch("/documento/{doc_id}/status")
def update_documento_status(
    doc_id: int,
    status: str = Query(...),
    modulo: str = Query(default="Archivo"),
    requester: str = Query(default=""),
    usuario_sesion: str = Depends(require_session),
    _autorizado: str = Depends(require_role("Archivo", "RRHH")),
):
    """Cambia el status de un documento: draft → revision → aprobado | rechazado."""
    _require_modulo(modulo)
    if status not in VALID_STATUS:
        raise HTTPException(400, f"Status inválido. Válidos: {VALID_STATUS}")

    table, pk = module_meta(modulo)

    # OA-005: sin este filtro se podía cambiar el status de un documento que
    # ya está en la papelera (el badge que lleva ahí sigue viniendo de
    # OA-004 sin filtrar). 409, no 404, para distinguir "no existe" de
    # "existe pero está borrado" — la papelera es un estado, no un olvido.
    existe = db_query(
        f"SELECT deleted_at FROM public.{table} WHERE {pk}=%s",
        [doc_id], fetch="one",
    )
    if not existe:
        raise HTTPException(404, "Documento no encontrado")
    if existe["deleted_at"] is not None:
        raise HTTPException(409, "El documento está en la papelera; restáurelo antes de cambiar su estado")

    result = db_query(
        f"UPDATE public.{table} SET status=%s, updated_at=NOW() WHERE {pk}=%s AND deleted_at IS NULL RETURNING {pk}",
        [status, doc_id], fetch="one", commit=True,
    )
    if not result:
        raise HTTPException(404, "Documento no encontrado")

    # IN-133: mismo criterio que admin_submit/update_documento/update_empleado
    # — el actor de auditoría sale de la sesión verificada, no de `requester`
    # (parámetro de query que rellena el propio cliente).
    log_event(usuario_sesion, "Status Documento", modulo, f"doc_id={doc_id} → {status}")
    return {"success": True, "doc_id": doc_id, "status": status}


@router.get("/documentos/pendientes")
def get_documentos_pendientes(
    modulo: str = "Archivo",
    page: int = 1,
    per_page: int = 25,
    usuario_sesion: str = Depends(require_session),
    _autorizado: str = Depends(require_role("Archivo", "RRHH")),
):
    """Lista documentos en estado draft o revision para revisión/aprobación."""
    _require_modulo(modulo)
    page, per_page, offset = paginate(page, per_page)

    if modulo == "Archivo":
        count_row = db_query(
            "SELECT COUNT(*) AS total FROM public.datos_archivo WHERE status IN ('draft','revision') AND deleted_at IS NULL",
            fetch="one",
        )
        rows = db_query(
            """SELECT id_archivo AS id, titulo, autor, tesauro_primario AS tipo,
                      TO_CHAR(fecha_documento,'YYYY-MM-DD') AS fecha,
                      COALESCE(status,'aprobado') AS status, updated_at, updated_by
               FROM public.datos_archivo WHERE status IN ('draft','revision') AND deleted_at IS NULL
               ORDER BY updated_at DESC NULLS LAST
               LIMIT %s OFFSET %s""",
            [per_page, offset], fetch="all",
        ) or []
    else:
        count_row = db_query(
            "SELECT COUNT(*) AS total FROM public.datos_rrhh WHERE status IN ('draft','revision') AND deleted_at IS NULL",
            fetch="one",
        )
        rows = db_query(
            """SELECT dr.id_rrhh AS id, td.nombre_corto AS titulo, dr.notas AS autor,
                      td.nombre_corto AS tipo, TO_CHAR(dr.fecha_documento,'YYYY-MM-DD') AS fecha,
                      COALESCE(dr.status,'aprobado') AS status, dr.updated_at, dr.updated_by
               FROM public.datos_rrhh dr
               LEFT JOIN public.tipo_documento td ON dr.id_tipo_documento = td.id
               WHERE dr.status IN ('draft','revision') AND dr.deleted_at IS NULL
               ORDER BY dr.updated_at DESC NULLS LAST
               LIMIT %s OFFSET %s""",
            [per_page, offset], fetch="all",
        ) or []

    total = int(count_row["total"]) if count_row else 0
    return {"total": total, "page": page, "per_page": per_page, "records": [dict(r) for r in rows]}


@router.post("/documento/{doc_id}/upload")
async def upload_documento_file(
    doc_id: int,
    modulo: str = "Archivo",
    usuario: str = "",
    usuario_sesion: str = Depends(require_session),
    _autorizado: str = Depends(require_role("Archivo", "RRHH")),
):
    """Stub para subida de archivos. Implementar cuando se elija proveedor de storage."""
    _require_modulo(modulo)
    return {
        "success": False,
        "detail": "Servicio de almacenamiento pendiente de configuración. Por favor ingrese la URL del archivo manualmente.",
        "doc_id": doc_id,
    }


@router.get("/empleado/{emp_id}")
def get_empleado(
    emp_id: int,
    usuario_sesion: str = Depends(require_session),
    _autorizado: str = Depends(require_role("RRHH")),
):
    row = db_query(
        """SELECT e.id, e.cedula, e.rif, e.nombres, e.apellidos,
                  COALESCE(c.nombre,'')   AS cargo,
                  COALESCE(d.nombre,'')   AS departamento,
                  COALESCE(el.estados,'') AS estado,
                  TO_CHAR(e.fecha_ingreso,    'YYYY-MM-DD') AS fecha_ingreso,
                  TO_CHAR(e.fecha_jubilacion, 'YYYY-MM-DD') AS fecha_jubilacion,
                  TO_CHAR(e.fecha_pension,    'YYYY-MM-DD') AS fecha_pension,
                  TO_CHAR(e.fecha_nacimiento, 'YYYY-MM-DD') AS fecha_nacimiento,
                  COALESCE(e.nivel_educativo, '') AS nivel_educativo,
                  COALESCE(e.sexo, '')            AS sexo,
                  COALESCE(e.foto_url, '')        AS foto_url
           FROM public.empleados e
           LEFT JOIN public.cargos            c  ON e.cargo_id        = c.id
           LEFT JOIN public.departamentos     d  ON e.departamento_id = d.id
           LEFT JOIN public.estados_laborales el ON e.estado_id       = el.id
           WHERE e.id = %s AND e.deleted_at IS NULL""",
        (emp_id,), fetch="one",
    )
    if not row:
        raise HTTPException(404, "Empleado no encontrado")
    return dict(row)


@router.put("/empleado/{emp_id}")
def update_empleado(
    emp_id: int,
    req: EmpleadoUpdateRequest,
    usuario_sesion: str = Depends(require_session),
    _autorizado: str = Depends(require_role("RRHH")),
):
    # OR-031: sin este filtro se podía editar (y guardar cambios sobre) un
    # empleado que ya está en la papelera —una pestaña abierta antes del
    # borrado, o el botón «atrás», seguían dejando pasar el `PUT`. Mismo
    # criterio 409 (existe pero está borrado) que OA-005 aplica a documentos.
    existe = db_query(
        "SELECT deleted_at FROM public.empleados WHERE id = %s",
        (emp_id,), fetch="one",
    )
    if not existe:
        raise HTTPException(404, "Empleado no encontrado")
    if existe["deleted_at"] is not None:
        raise HTTPException(409, "El empleado está en la papelera; restáurelo antes de editarlo")

    set_clauses, params = [], []

    if req.nombres is not None:
        set_clauses.append("nombres = %s"); params.append(req.nombres)
    if req.apellidos is not None:
        set_clauses.append("apellidos = %s"); params.append(req.apellidos)
    if req.rif is not None:
        set_clauses.append("rif = %s"); params.append(req.rif)
    if req.foto_url is not None:
        set_clauses.append("foto_url = %s"); params.append(req.foto_url)
    if req.fecha_jubilacion is not None:
        set_clauses.append("fecha_jubilacion = %s")
        params.append(req.fecha_jubilacion if req.fecha_jubilacion else None)
    if req.fecha_pension is not None:
        set_clauses.append("fecha_pension = %s")
        params.append(req.fecha_pension if req.fecha_pension else None)

    if req.cargo is not None:
        cargo_id = _resolve_or_create_lookup("cargos", req.cargo)
        set_clauses.append("cargo_id = %s"); params.append(cargo_id)
    if req.departamento is not None:
        dept_id = _resolve_or_create_lookup("departamentos", req.departamento)
        set_clauses.append("departamento_id = %s"); params.append(dept_id)
    if req.estado is not None:
        estado_id = _resolve_or_create_lookup("estados_laborales", req.estado)
        set_clauses.append("estado_id = %s"); params.append(estado_id)

    # Campos LOTTT (nuevos)
    if req.fecha_nacimiento is not None:
        set_clauses.append("fecha_nacimiento = %s")
        params.append(req.fecha_nacimiento if req.fecha_nacimiento else None)
    if req.nivel_educativo is not None:
        set_clauses.append("nivel_educativo = %s"); params.append(req.nivel_educativo or None)
    if req.sexo is not None:
        set_clauses.append("sexo = %s"); params.append(req.sexo or None)

    if set_clauses:
        set_clauses.append("updated_at = NOW()")
        # IN-133: `updated_by` sale de la sesión verificada, no de `req.usuario`.
        set_clauses.append("updated_by = %s"); params.append(usuario_sesion)
        params.append(emp_id)
        db_query(
            f"UPDATE public.empleados SET {', '.join(set_clauses)} WHERE id = %s AND deleted_at IS NULL",
            params, fetch="none", commit=True,
        )

    log_event(usuario_sesion, "Update Empleado", "RRHH", f"ID: {emp_id} nombres={req.nombres}")
    return {"success": True}


@router.get("/status_counts")
def get_status_counts(
    modulo: str = "Archivo",
    usuario_sesion: str = Depends(require_session),
    _autorizado: str = Depends(require_role("Archivo", "RRHH")),
):
    """Retorna conteo de documentos por status para badges en el monitor.

    OA-004: los badges deben reflejar la misma vista que `/list_all` (que ya
    filtra `deleted_at IS NULL`). Sin el filtro aquí, mandar documentos a la
    papelera dejaba el número del badge inflado respecto de lo que la tabla
    de verdad muestra al pulsarlo.
    """
    _require_modulo(modulo)
    if modulo == "Archivo":
        rows = db_query(
            """SELECT COALESCE(status, 'aprobado') AS status, COUNT(*) AS cnt
               FROM public.datos_archivo
               WHERE deleted_at IS NULL
               GROUP BY COALESCE(status, 'aprobado')""",
            fetch="all",
        ) or []
    else:
        rows = db_query(
            """SELECT COALESCE(status, 'aprobado') AS status, COUNT(*) AS cnt
               FROM public.datos_rrhh
               WHERE deleted_at IS NULL
               GROUP BY COALESCE(status, 'aprobado')""",
            fetch="all",
        ) or []
    return {r["status"]: int(r["cnt"]) for r in rows}


@router.delete("/empleado/{emp_id}")
def delete_empleado(
    emp_id: int,
    usuario: str,
    deleted_reason: Optional[str] = Query(default=None, max_length=500),
    usuario_sesion: str = Depends(require_session),
    _autorizado: str = Depends(require_role("RRHH")),
):
    """Soft-delete: envía el empleado a la papelera. No borra físicamente."""
    result = db_query(
        "UPDATE public.empleados SET deleted_at=%s, deleted_by=%s, deleted_reason=%s "
        "WHERE id=%s AND deleted_at IS NULL RETURNING id",
        [datetime.utcnow().isoformat(), usuario, (deleted_reason or None), emp_id], fetch="one", commit=True,
    )
    if not result:
        raise HTTPException(404, "Empleado no encontrado o ya eliminado")
    log_event(usuario, "Delete Empleado (soft)", "RRHH", f"ID: {emp_id}")
    return {"success": True}
