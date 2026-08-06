"""Papelera de reciclaje y versiones de archivos digitales."""
import re
from datetime import datetime
from typing import Optional

_SAFE_URL_RE = re.compile(r'^(/|https?://)', re.IGNORECASE)

from fastapi import APIRouter, Depends, HTTPException, Query

from database import db_query, log_event
from routes.admin.deps import require_session
from routes.admin.helpers import _require_modulo, module_meta, paginate

router = APIRouter(prefix="/api/admin", tags=["papelera"], dependencies=[Depends(require_session)])


# =============================================================================
# PAPELERA DE RECICLAJE
# =============================================================================

@router.get("/papelera")
def list_trash(
    modulo: str = "Archivo",
    page: int = 1,
    per_page: int = 25,
):
    """Lista los documentos (y empleados en RRHH) en la papelera."""
    _require_modulo(modulo)
    page, per_page, offset = paginate(page, per_page)

    if modulo == "Archivo":
        count_row = db_query(
            "SELECT COUNT(*) AS total FROM public.datos_archivo WHERE deleted_at IS NOT NULL",
            fetch="one",
        )
        rows = db_query(
            """SELECT id_archivo AS id, titulo, autor,
                      COALESCE(tesauro_primario,'') AS doc_type,
                      TO_CHAR(fecha_documento,'YYYY-MM-DD') AS fecha,
                      TO_CHAR(deleted_at,'YYYY-MM-DD HH24:MI') AS deleted_at,
                      deleted_by
               FROM public.datos_archivo
               WHERE deleted_at IS NOT NULL
               ORDER BY deleted_at DESC
               LIMIT %s OFFSET %s""",
            [per_page, offset], fetch="all",
        ) or []
    else:
        count_row = db_query(
            """SELECT COUNT(*) AS total FROM public.datos_rrhh dr
               LEFT JOIN public.empleados e ON dr.empleado_id = e.id
               WHERE dr.deleted_at IS NOT NULL""",
            fetch="one",
        )
        rows = db_query(
            """SELECT dr.id_rrhh AS id,
                      COALESCE(td.nombre,'') AS titulo,
                      e.nombres || ' ' || e.apellidos AS autor,
                      COALESCE(td.nombre,'') AS doc_type,
                      TO_CHAR(dr.fecha_documento,'YYYY-MM-DD') AS fecha,
                      TO_CHAR(dr.deleted_at,'YYYY-MM-DD HH24:MI') AS deleted_at,
                      dr.deleted_by
               FROM public.datos_rrhh dr
               LEFT JOIN public.empleados e ON dr.empleado_id = e.id
               LEFT JOIN public.tipo_documento td ON dr.id_tipo_documento = td.id
               WHERE dr.deleted_at IS NOT NULL
               ORDER BY dr.deleted_at DESC
               LIMIT %s OFFSET %s""",
            [per_page, offset], fetch="all",
        ) or []

    total = int(count_row["total"]) if count_row else 0
    return {"total": total, "page": page, "per_page": per_page, "records": [dict(r) for r in rows]}


@router.post("/papelera/{doc_id}/restaurar")
def restore_document(doc_id: int, modulo: str = "Archivo", usuario: str = ""):
    """Recupera un documento de la papelera (deshace el soft-delete)."""
    _require_modulo(modulo)
    if modulo == "Archivo":
        result = db_query(
            "UPDATE public.datos_archivo SET deleted_at=NULL, deleted_by=NULL WHERE id_archivo=%s AND deleted_at IS NOT NULL RETURNING id_archivo",
            [doc_id], fetch="one", commit=True,
        )
    else:
        result = db_query(
            "UPDATE public.datos_rrhh SET deleted_at=NULL, deleted_by=NULL WHERE id_rrhh=%s AND deleted_at IS NOT NULL RETURNING id_rrhh",
            [doc_id], fetch="one", commit=True,
        )

    if not result:
        raise HTTPException(404, "Documento no encontrado en papelera")

    log_event(usuario or "sistema", "Restaurar Documento", modulo, f"ID: {doc_id}")
    return {"success": True}


@router.delete("/papelera/{doc_id}/purgar")
def purge_document(doc_id: int, modulo: str, usuario: str):
    """Elimina permanentemente un documento de la papelera. Irreversible."""
    if modulo == "Archivo":
        existing = db_query(
            "SELECT id_archivo FROM public.datos_archivo WHERE id_archivo=%s AND deleted_at IS NOT NULL",
            [doc_id], fetch="one",
        )
        if not existing:
            raise HTTPException(404, "Documento no está en papelera")
        db_query("DELETE FROM public.archivo_descriptores WHERE id_archivo=%s", [doc_id], fetch="none", commit=True)
        db_query("DELETE FROM public.documento_versiones WHERE tabla='datos_archivo' AND documento_id=%s", [doc_id], fetch="none", commit=True)
        db_query("DELETE FROM public.datos_archivo WHERE id_archivo=%s", [doc_id], fetch="none", commit=True)
    else:
        existing = db_query(
            "SELECT id_rrhh FROM public.datos_rrhh WHERE id_rrhh=%s AND deleted_at IS NOT NULL",
            [doc_id], fetch="one",
        )
        if not existing:
            raise HTTPException(404, "Documento no está en papelera")
        db_query("DELETE FROM public.documento_versiones WHERE tabla='datos_rrhh' AND documento_id=%s", [doc_id], fetch="none", commit=True)
        db_query("DELETE FROM public.datos_rrhh WHERE id_rrhh=%s", [doc_id], fetch="none", commit=True)

    log_event(usuario, "Purgar Documento (permanente)", modulo, f"ID: {doc_id}")
    return {"success": True}


# Papelera de empleados
@router.get("/papelera/empleados")
def list_trash_employees(page: int = 1, per_page: int = 25):
    page, per_page, offset = paginate(page, per_page)

    count_row = db_query(
        "SELECT COUNT(*) AS total FROM public.empleados WHERE deleted_at IS NOT NULL",
        fetch="one",
    )
    rows = db_query(
        """SELECT id, cedula, nombres || ' ' || apellidos AS nombre,
                  TO_CHAR(deleted_at,'YYYY-MM-DD HH24:MI') AS deleted_at, deleted_by
           FROM public.empleados
           WHERE deleted_at IS NOT NULL
           ORDER BY deleted_at DESC
           LIMIT %s OFFSET %s""",
        [per_page, offset], fetch="all",
    ) or []

    total = int(count_row["total"]) if count_row else 0
    return {"total": total, "page": page, "per_page": per_page, "records": [dict(r) for r in rows]}


@router.post("/papelera/empleados/{emp_id}/restaurar")
def restore_employee(emp_id: int, usuario: str = ""):
    result = db_query(
        "UPDATE public.empleados SET deleted_at=NULL, deleted_by=NULL WHERE id=%s AND deleted_at IS NOT NULL RETURNING id",
        [emp_id], fetch="one", commit=True,
    )
    if not result:
        raise HTTPException(404, "Empleado no encontrado en papelera")
    log_event(usuario or "sistema", "Restaurar Empleado", "RRHH", f"ID: {emp_id}")
    return {"success": True}


@router.delete("/papelera/empleados/{emp_id}/purgar")
def purge_employee(emp_id: int, usuario: str):
    existing = db_query(
        "SELECT id FROM public.empleados WHERE id=%s AND deleted_at IS NOT NULL",
        [emp_id], fetch="one",
    )
    if not existing:
        raise HTTPException(404, "Empleado no está en papelera")
    db_query("DELETE FROM public.historial_cargos WHERE empleado_id=%s", [emp_id], fetch="none", commit=True)
    db_query("DELETE FROM public.datos_rrhh WHERE empleado_id=%s", [emp_id], fetch="none", commit=True)
    db_query("DELETE FROM public.empleados WHERE id=%s", [emp_id], fetch="none", commit=True)
    log_event(usuario, "Purgar Empleado (permanente)", "RRHH", f"ID: {emp_id}")
    return {"success": True}


# =============================================================================
# VERSIONES DE ARCHIVOS DIGITALES
# =============================================================================

@router.get("/documento/{doc_id}/versiones")
def list_versions(doc_id: int, modulo: str = "Archivo"):
    _require_modulo(modulo)
    tabla, _ = module_meta(modulo)
    rows = db_query(
        """SELECT id, version_num, file_url, comentario, subido_por,
                  TO_CHAR(created_at,'YYYY-MM-DD HH24:MI') AS created_at
           FROM public.documento_versiones
           WHERE tabla=%s AND documento_id=%s
           ORDER BY version_num DESC""",
        [tabla, doc_id], fetch="all",
    ) or []
    return {"versiones": [dict(r) for r in rows]}


@router.post("/documento/{doc_id}/versiones")
def add_version(
    doc_id: int,
    modulo: str = "Archivo",
    file_url: str = "",
    comentario: Optional[str] = None,
    usuario: str = "",
):
    """Registra una nueva versión del archivo digital para un documento."""
    _require_modulo(modulo)
    if file_url and not _SAFE_URL_RE.match(file_url):
        raise HTTPException(400, "file_url inválida")
    tabla, pk = module_meta(modulo)

    current = db_query(
        f"SELECT file_url FROM public.{tabla} WHERE {pk}=%s AND deleted_at IS NULL",
        [doc_id], fetch="one",
    )
    if not current:
        raise HTTPException(404, "Documento no encontrado")

    last_ver = db_query(
        "SELECT COALESCE(MAX(version_num),0) AS vn FROM public.documento_versiones WHERE tabla=%s AND documento_id=%s",
        [tabla, doc_id], fetch="one",
    )
    next_ver = (last_ver["vn"] if last_ver else 0) + 1

    # Guardar versión anterior antes de actualizar
    old_url = current["file_url"] or ""
    if old_url:
        db_query(
            "INSERT INTO public.documento_versiones (tabla, documento_id, version_num, file_url, comentario, subido_por) VALUES (%s,%s,%s,%s,%s,%s)",
            [tabla, doc_id, next_ver, old_url, comentario or "Versión anterior", usuario],
            fetch="none", commit=True,
        )

    db_query(
        f"UPDATE public.{tabla} SET file_url=%s, updated_at=NOW() WHERE {pk}=%s",
        [file_url, doc_id], fetch="none", commit=True,
    )

    log_event(usuario or "sistema", "Nueva Versión", modulo, f"doc_id={doc_id} v{next_ver}")
    return {"success": True, "version_num": next_ver}


@router.post("/documento/{doc_id}/versiones/{ver_id}/restaurar")
def restore_version(doc_id: int, ver_id: int, modulo: str = "Archivo", usuario: str = ""):
    """Restaura el archivo digital de una versión anterior como versión actual."""
    _require_modulo(modulo)
    tabla, pk = module_meta(modulo)

    ver = db_query(
        "SELECT file_url FROM public.documento_versiones WHERE id=%s AND tabla=%s AND documento_id=%s",
        [ver_id, tabla, doc_id], fetch="one",
    )
    if not ver:
        raise HTTPException(404, "Versión no encontrada")

    db_query(
        f"UPDATE public.{tabla} SET file_url=%s, updated_at=NOW() WHERE {pk}=%s",
        [ver["file_url"], doc_id], fetch="none", commit=True,
    )

    log_event(usuario or "sistema", "Restaurar Versión", modulo, f"doc_id={doc_id}, ver_id={ver_id}")
    return {"success": True}


@router.delete("/documento/{doc_id}/versiones/{ver_id}")
def delete_version(doc_id: int, ver_id: int, modulo: str = "Archivo", usuario: str = ""):
    """Elimina una versión del historial (permanente, no afecta el archivo actual)."""
    _require_modulo(modulo)
    tabla, _ = module_meta(modulo)
    result = db_query(
        "DELETE FROM public.documento_versiones WHERE id=%s AND tabla=%s AND documento_id=%s RETURNING id",
        [ver_id, tabla, doc_id], fetch="one", commit=True,
    )
    if not result:
        raise HTTPException(404, "Versión no encontrada")
    log_event(usuario or "sistema", "Eliminar Versión", modulo, f"doc_id={doc_id}, ver_id={ver_id}")
    return {"success": True}
