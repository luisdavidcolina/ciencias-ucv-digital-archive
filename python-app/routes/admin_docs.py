"""CRUD de documentos y empleados en el panel de administración."""
from datetime import datetime
from typing import Optional

from fastapi import APIRouter, HTTPException, Query

from database import db_query, log_event
from models import DocumentSubmitRequest, DocumentUpdateRequest, EmpleadoUpdateRequest
from .admin_helpers import (
    _resolve_or_create_lookup,
    _resolve_or_create_tipo_documento,
    _resolve_user_id,
    invalidate_choices_cache,
)

router = APIRouter()

VALID_STATUS = ("draft", "revision", "aprobado", "rechazado")


@router.get("/list_all")
def list_all_files(
    modulo: str,
    search: Optional[str] = "",
    type_filter: Optional[str] = "",
    person_filter: Optional[str] = "",
    status_filter: Optional[str] = "",
    page: int = 1,
    per_page: int = 25,
):
    page     = max(1, page)
    per_page = max(1, min(per_page, 100))
    offset   = (page - 1) * per_page

    import re as _re
    if modulo == "Archivo":
        conditions, params = [], []
        if search:
            _has_letters = bool(_re.search(r'[A-Za-zÀ-ÿ]', search))
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
                COALESCE(da.autor, '') AS autor,
                TO_CHAR(da.fecha_documento, 'YYYY-MM-DD') AS fecha,
                COALESCE(da.tesauro_primario, '') AS doc_type,
                COALESCE(da.tesauro_secundario, '') AS tesauro_secundario,
                COALESCE(da.ubicacion, '') AS ubicacion,
                COALESCE(da.abstract, '') AS resumen,
                COALESCE(da.file_url, '') AS file_url,
                COALESCE(da.status, 'aprobado') AS status
            FROM public.datos_archivo da
            {where}
            ORDER BY da.fecha_documento DESC NULLS LAST
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
            _has_letters = bool(_re.search(r'[A-Za-zÀ-ÿ]', search))
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
        if status_filter:
            conditions.append("COALESCE(el.estados, '') ILIKE %s")
            params.append(f"%{status_filter}%")

        join = """
            FROM public.empleados e
            LEFT JOIN public.cargos c ON e.cargo_id = c.id
            LEFT JOIN public.departamentos d ON e.departamento_id = d.id
            LEFT JOIN public.estados_laborales el ON e.estado_id = el.id
            LEFT JOIN public.datos_rrhh dr ON dr.empleado_id = e.id
            LEFT JOIN public.tipo_documento td ON dr.id_tipo_documento = td.id
        """
        where = ("WHERE " + " AND ".join(conditions)) if conditions else ""

        count_row = db_query(
            f"SELECT COUNT(DISTINCT e.id) AS total {join} {where}",
            params or None, fetch="one",
        )
        total = int(count_row["total"]) if count_row else 0

        rows = db_query(
            f"""
            SELECT DISTINCT
                e.id AS empleado_id,
                e.cedula,
                e.nombres || ' ' || e.apellidos AS empleado,
                COALESCE(d.nombre, '') AS departamento,
                COALESCE(el.estados, '') AS estado,
                COALESCE(c.nombre, '') AS cargo,
                TO_CHAR(e.fecha_ingreso, 'YYYY-MM-DD') AS fecha_ingreso,
                COALESCE(td.nombre_corto, td.nombre, '') AS doc_type,
                COALESCE(dr.ubicacion, '') AS ubicacion
            {join}
            {where}
            ORDER BY e.nombres ASC
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
def get_documento(doc_id: int, modulo: str = "Archivo"):
    if modulo == "Archivo":
        row = db_query(
            """SELECT id_archivo AS id, titulo, autor, abstract AS resumen,
                      TO_CHAR(fecha_documento,'YYYY-MM-DD') AS fecha,
                      COALESCE(tesauro_primario,'') AS doc_type,
                      COALESCE(tesauro_secundario,'') AS tesauro_secundario,
                      COALESCE(ubicacion,'') AS ubicacion,
                      COALESCE(file_url,'') AS file_url,
                      COALESCE(status,'aprobado') AS status,
                      COALESCE(personas_relacionadas,'') AS personas_relacionadas,
                      updated_at, updated_by
               FROM public.datos_archivo WHERE id_archivo = %s""",
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
               WHERE dr.id_rrhh = %s""",
            [doc_id], fetch="one"
        )
    if not row:
        raise HTTPException(404, "Documento no encontrado")
    return dict(row)


@router.post("/submit")
def admin_submit(req: DocumentSubmitRequest):
    log_event(req.usuario, "Create Document", req.modulo, f"Tipo: {req.doc_type}, Ubicacion: {req.ubicacion}, Titulo: {(req.titulo or req.empleado or '')[:60]}")
    creado_por = _resolve_user_id(req.usuario)
    fecha_doc  = req.fecha or datetime.now().strftime("%Y-%m-%d")

    if req.modulo == "Archivo":
        tipo_id = _resolve_or_create_tipo_documento(req.doc_type, cat_slug="archivo")
        new_row = db_query(
            """
            INSERT INTO public.datos_archivo
                (titulo, abstract, autor,
                 fecha_documento, ubicacion, creado_por, tesauro_primario,
                 tesauro_secundario, id_tipo_documento)
            VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s)
            RETURNING id_archivo
            """,
            (
                req.titulo or "Sin título",
                req.resumen or "",
                req.autor or "Anónimo",
                fecha_doc, req.ubicacion, creado_por, req.doc_type,
                req.tesauro_secundario or "", tipo_id,
            ),
            fetch="one",
            commit=True,
        )

        if req.descriptores_libres:
            raw_descs = req.descriptores_libres.replace(";", ",").split(",")
            descriptores = [d.strip() for d in raw_descs if d.strip()]
            for desc in descriptores:
                desc_row = db_query(
                    """
                    INSERT INTO public.descriptores_libres (nombre)
                    VALUES (%s)
                    ON CONFLICT (nombre) DO UPDATE SET nombre = EXCLUDED.nombre
                    RETURNING id_descriptor
                    """,
                    (desc,),
                    fetch="one",
                    commit=True,
                )
                db_query(
                    """
                    INSERT INTO public.archivo_descriptores (id_archivo, id_descriptor)
                    VALUES (%s, %s)
                    ON CONFLICT DO NOTHING
                    """,
                    (new_row["id_archivo"], desc_row["id_descriptor"]),
                    fetch="none",
                    commit=True,
                )

        invalidate_choices_cache()
        return {"success": True, "id": str(new_row["id_archivo"])}

    # ── Módulo RRHH ──────────────────────────────────────────────────────────
    tipo_id = _resolve_or_create_tipo_documento(req.doc_type)
    cedula = (req.cedula or "").strip()
    if not cedula:
        raise HTTPException(status_code=400, detail="Cédula es requerida para RRHH")

    emp_row = db_query("SELECT id FROM public.empleados WHERE cedula = %s", (cedula,), fetch="one")
    if not emp_row:
        cargo_id   = _resolve_or_create_lookup("cargos",            req.cargo,        "Por Asignar")
        dept_id    = _resolve_or_create_lookup("departamentos",     req.departamento, "Por Asignar")
        estado_id  = _resolve_or_create_lookup("estados_laborales", req.estado,       "Pendiente de Registro")
        emp_row = db_query(
            """
            INSERT INTO public.empleados
                (cedula, nombres, apellidos, rif, cargo_id, departamento_id,
                 estado_id, fecha_ingreso, fecha_jubilacion, fecha_pension, foto_url)
            VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
            RETURNING id
            """,
            (
                cedula,
                (req.nombres or req.empleado or cedula).strip(),
                (req.apellidos or "").strip(),
                req.rif or None,
                cargo_id, dept_id, estado_id, fecha_doc,
                req.fecha_jubilacion or None, req.fecha_pension or None, req.foto_url or None,
            ),
            fetch="one",
            commit=True,
        )

    new_row = db_query(
        """
        INSERT INTO public.datos_rrhh
            (titulo, autor, id_tipo_documento,
             empleado_id, fecha_documento, ubicacion, creado_por,
             tesauro_primario, tesauro_secundario, abstract, personas_relacionadas)
        VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
        RETURNING id_rrhh
        """,
        (
            f"{req.doc_type} de {req.personas_relacionadas or req.empleado}",
            "Recursos Humanos",
            tipo_id, emp_row["id"], fecha_doc, req.ubicacion, creado_por,
            req.doc_type, req.tesauro_secundario or "", req.resumen or "",
            req.personas_relacionadas or "",
        ),
        fetch="one",
        commit=True,
    )

    if req.descriptores_libres:
        raw_descs = req.descriptores_libres.replace(";", ",").split(",")
        descriptores = [d.strip() for d in raw_descs if d.strip()]
        for desc in descriptores:
            desc_row = db_query(
                """
                INSERT INTO public.descriptores_libres (nombre)
                VALUES (%s)
                ON CONFLICT (nombre) DO UPDATE SET nombre = EXCLUDED.nombre
                RETURNING id_descriptor
                """,
                (desc,),
                fetch="one",
                commit=True,
            )
            db_query(
                """
                INSERT INTO public.rrhh_descriptores (id_rrhh, id_descriptor)
                VALUES (%s, %s)
                ON CONFLICT DO NOTHING
                """,
                (new_row["id_rrhh"], desc_row["id_descriptor"]),
                fetch="none",
                commit=True,
            )

    invalidate_choices_cache()
    return {"success": True, "id": str(new_row["id_rrhh"])}


@router.put("/documento/{doc_id}")
def update_documento(doc_id: int, req: DocumentUpdateRequest):
    updated_by = _resolve_user_id(req.usuario)
    updated_at = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    if req.modulo == "Archivo":
        set_clauses, params = [], []

        if req.titulo is not None:
            set_clauses.append("titulo = %s"); params.append(req.titulo)
        if req.autor is not None:
            set_clauses.append("autor = %s"); params.append(req.autor)
        if req.resumen is not None:
            set_clauses.append("abstract = %s"); params.append(req.resumen)
        if req.fecha is not None:
            set_clauses.append("fecha_documento = %s"); params.append(req.fecha)
        if req.ubicacion is not None:
            set_clauses.append("ubicacion = %s"); params.append(req.ubicacion)
        if req.tesauro_secundario is not None:
            set_clauses.append("tesauro_secundario = %s"); params.append(req.tesauro_secundario)

        if req.doc_type is not None:
            tipo_id = _resolve_or_create_tipo_documento(req.doc_type, cat_slug="archivo")
            set_clauses.append("id_tipo_documento = %s"); params.append(tipo_id)
            set_clauses.append("tesauro_primario = %s"); params.append(req.doc_type)

        if req.file_url is not None:
            set_clauses.append("file_url = %s"); params.append(req.file_url or None)

        if req.status is not None and req.status in VALID_STATUS:
            set_clauses.append("status = %s"); params.append(req.status)

        set_clauses.append("updated_at = %s"); params.append(updated_at)
        set_clauses.append("updated_by = %s"); params.append(updated_by)

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
            raw_descs = req.palabras_clave.replace(";", ",").split(",")
            descriptores = [d.strip() for d in raw_descs if d.strip()]
            for desc in descriptores:
                desc_row = db_query(
                    "INSERT INTO public.descriptores_libres (nombre) VALUES (%s) "
                    "ON CONFLICT (nombre) DO UPDATE SET nombre = EXCLUDED.nombre RETURNING id_descriptor",
                    (desc,), fetch="one", commit=True,
                )
                db_query(
                    "INSERT INTO public.archivo_descriptores (id_archivo, id_descriptor) VALUES (%s, %s) ON CONFLICT DO NOTHING",
                    (doc_id, desc_row["id_descriptor"]), fetch="none", commit=True,
                )

        invalidate_choices_cache()
        log_event(req.usuario, "Update Document", "Archivo", f"ID: {doc_id}, Titulo: {(req.titulo or '')[:60]}, Status: {req.status or 'aprobado'}")
        return {"success": True}

    else:  # RRHH
        set_clauses, params = [], []

        if req.titulo is not None:
            set_clauses.append("titulo = %s"); params.append(req.titulo)
        if req.autor is not None:
            set_clauses.append("autor = %s"); params.append(req.autor)
        if req.resumen is not None:
            set_clauses.append("abstract = %s"); params.append(req.resumen)
        if req.fecha is not None:
            set_clauses.append("fecha_documento = %s"); params.append(req.fecha)
        if req.ubicacion is not None:
            set_clauses.append("ubicacion = %s"); params.append(req.ubicacion)
        if req.tesauro_secundario is not None:
            set_clauses.append("tesauro_secundario = %s"); params.append(req.tesauro_secundario)
        if req.doc_type is not None:
            set_clauses.append("tesauro_primario = %s"); params.append(req.doc_type)
        if req.personas_relacionadas is not None:
            set_clauses.append("personas_relacionadas = %s"); params.append(req.personas_relacionadas)
        if req.notas is not None:
            set_clauses.append("notas = %s"); params.append(req.notas)

        if req.file_url is not None:
            set_clauses.append("file_url = %s"); params.append(req.file_url or None)

        if req.status is not None and req.status in VALID_STATUS:
            set_clauses.append("status = %s"); params.append(req.status)

        set_clauses.append("updated_at = %s"); params.append(updated_at)
        set_clauses.append("updated_by = %s"); params.append(updated_by)

        if set_clauses:
            params.append(doc_id)
            db_query(
                f"UPDATE public.datos_rrhh SET {', '.join(set_clauses)} WHERE id_rrhh = %s",
                params, fetch="none", commit=True,
            )

        invalidate_choices_cache()
        log_event(req.usuario, "Update Document", "RRHH", f"ID: {doc_id}, Status: {req.status or 'aprobado'}")
        return {"success": True}


@router.delete("/documento/{doc_id}")
def delete_documento(doc_id: int, modulo: str, usuario: str):
    if modulo == "Archivo":
        db_query(
            "DELETE FROM public.archivo_descriptores WHERE id_archivo = %s",
            (doc_id,), fetch="none", commit=True,
        )
        db_query(
            "DELETE FROM public.datos_archivo WHERE id_archivo = %s",
            (doc_id,), fetch="none", commit=True,
        )
    else:
        db_query(
            "DELETE FROM public.rrhh_descriptores WHERE id_rrhh = %s",
            (doc_id,), fetch="none", commit=True,
        )
        db_query(
            "DELETE FROM public.datos_rrhh WHERE id_rrhh = %s",
            (doc_id,), fetch="none", commit=True,
        )

    invalidate_choices_cache()
    log_event(usuario, "Delete Document", modulo, f"ID: {doc_id}")
    return {"success": True}


@router.patch("/documento/{doc_id}/status")
def update_documento_status(
    doc_id: int,
    status: str = Query(...),
    modulo: str = Query(default="Archivo"),
    requester: str = Query(default=""),
):
    """Cambia el status de un documento: draft → revision → aprobado | rechazado."""
    if status not in VALID_STATUS:
        raise HTTPException(400, f"Status inválido. Válidos: {VALID_STATUS}")

    table = "datos_archivo" if modulo == "Archivo" else "datos_rrhh"
    pk    = "id_archivo"    if modulo == "Archivo" else "id_rrhh"

    result = db_query(
        f"UPDATE public.{table} SET status=%s, updated_at=NOW() WHERE {pk}=%s RETURNING {pk}",
        [status, doc_id], fetch="one", commit=True,
    )
    if not result:
        raise HTTPException(404, "Documento no encontrado")

    log_event(requester, f"Status Documento", modulo, f"doc_id={doc_id} → {status}")
    return {"success": True, "doc_id": doc_id, "status": status}


@router.get("/documentos/pendientes")
def get_documentos_pendientes(modulo: str = "Archivo", page: int = 1, per_page: int = 25):
    """Lista documentos en estado draft o revision para revisión/aprobación."""
    page = max(1, page)
    per_page = max(1, min(per_page, 100))
    offset = (page - 1) * per_page

    if modulo == "Archivo":
        count_row = db_query(
            "SELECT COUNT(*) AS total FROM public.datos_archivo WHERE status IN ('draft','revision')",
            fetch="one",
        )
        rows = db_query(
            """SELECT id_archivo AS id, titulo, autor, tesauro_primario AS tipo,
                      TO_CHAR(fecha_documento,'YYYY-MM-DD') AS fecha,
                      COALESCE(status,'aprobado') AS status, updated_at, updated_by
               FROM public.datos_archivo WHERE status IN ('draft','revision')
               ORDER BY updated_at DESC NULLS LAST
               LIMIT %s OFFSET %s""",
            [per_page, offset], fetch="all",
        ) or []
    else:
        count_row = db_query(
            "SELECT COUNT(*) AS total FROM public.datos_rrhh WHERE status IN ('draft','revision')",
            fetch="one",
        )
        rows = db_query(
            """SELECT dr.id_rrhh AS id, td.nombre_corto AS titulo, dr.notas AS autor,
                      td.nombre_corto AS tipo, TO_CHAR(dr.fecha_documento,'YYYY-MM-DD') AS fecha,
                      COALESCE(dr.status,'aprobado') AS status, dr.updated_at, dr.updated_by
               FROM public.datos_rrhh dr
               LEFT JOIN public.tipo_documento td ON dr.id_tipo_documento = td.id
               WHERE dr.status IN ('draft','revision')
               ORDER BY dr.updated_at DESC NULLS LAST
               LIMIT %s OFFSET %s""",
            [per_page, offset], fetch="all",
        ) or []

    total = int(count_row["total"]) if count_row else 0
    return {"total": total, "page": page, "per_page": per_page, "records": [dict(r) for r in rows]}


@router.post("/documento/{doc_id}/upload")
async def upload_documento_file(doc_id: int, modulo: str = "Archivo", usuario: str = ""):
    """Stub para subida de archivos. Implementar cuando se elija proveedor de storage."""
    return {
        "success": False,
        "detail": "Servicio de almacenamiento pendiente de configuración. Por favor ingrese la URL del archivo manualmente.",
        "doc_id": doc_id,
    }


@router.get("/empleado/{emp_id}")
def get_empleado(emp_id: int):
    row = db_query(
        """SELECT e.id, e.cedula, e.rif, e.nombres, e.apellidos,
                  COALESCE(c.nombre,'')  AS cargo,
                  COALESCE(d.nombre,'')  AS departamento,
                  COALESCE(el.estados,'') AS estado,
                  TO_CHAR(e.fecha_ingreso,'YYYY-MM-DD')    AS fecha_ingreso,
                  TO_CHAR(e.fecha_jubilacion,'YYYY-MM-DD') AS fecha_jubilacion,
                  TO_CHAR(e.fecha_pension,'YYYY-MM-DD')    AS fecha_pension,
                  COALESCE(e.foto_url,'') AS foto_url
           FROM public.empleados e
           LEFT JOIN public.cargos            c  ON e.cargo_id        = c.id
           LEFT JOIN public.departamentos     d  ON e.departamento_id = d.id
           LEFT JOIN public.estados_laborales el ON e.estado_id       = el.id
           WHERE e.id = %s""",
        (emp_id,), fetch="one",
    )
    if not row:
        raise HTTPException(404, "Empleado no encontrado")
    return dict(row)


@router.put("/empleado/{emp_id}")
def update_empleado(emp_id: int, req: EmpleadoUpdateRequest):
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

    if set_clauses:
        params.append(emp_id)
        db_query(
            f"UPDATE public.empleados SET {', '.join(set_clauses)} WHERE id = %s",
            params, fetch="none", commit=True,
        )

    log_event(req.usuario, "Update Empleado", "RRHH", f"ID: {emp_id}")
    return {"success": True}


@router.get("/status_counts")
def get_status_counts(modulo: str = "Archivo"):
    """Retorna conteo de documentos por status para badges en el monitor."""
    if modulo == "Archivo":
        rows = db_query(
            """SELECT COALESCE(status, 'aprobado') AS status, COUNT(*) AS cnt
               FROM public.datos_archivo
               GROUP BY COALESCE(status, 'aprobado')""",
            fetch="all",
        ) or []
    else:
        rows = db_query(
            """SELECT COALESCE(status, 'aprobado') AS status, COUNT(*) AS cnt
               FROM public.datos_rrhh
               GROUP BY COALESCE(status, 'aprobado')""",
            fetch="all",
        ) or []
    return {r["status"]: int(r["cnt"]) for r in rows}


@router.delete("/empleado/{emp_id}")
def delete_empleado(emp_id: int, usuario: str):
    db_query(
        """DELETE FROM public.rrhh_descriptores
           WHERE id_rrhh IN (SELECT id_rrhh FROM public.datos_rrhh WHERE empleado_id = %s)""",
        (emp_id,), fetch="none", commit=True,
    )
    db_query(
        "DELETE FROM public.datos_rrhh WHERE empleado_id = %s",
        (emp_id,), fetch="none", commit=True,
    )
    db_query(
        "DELETE FROM public.empleados WHERE id = %s",
        (emp_id,), fetch="none", commit=True,
    )
    log_event(usuario, "Delete Empleado", "RRHH", f"ID: {emp_id}")
    return {"success": True}
