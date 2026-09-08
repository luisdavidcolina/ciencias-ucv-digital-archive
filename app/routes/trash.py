"""Papelera de reciclaje y versiones de archivos digitales."""
import re
from typing import Optional

_SAFE_URL_RE = re.compile(r'^(/|https?://)', re.IGNORECASE)
# OA-006/OA-007: prefijo de `file_url` para las claves propias de R2 (ver
# `routes/files.py` y el mismo patrón en `routes/backup.py:_extraer_claves_r2`
# y `routes/share.py`). Un enlace externo antiguo no lo lleva y no se toca.
_R2_KEY_PREFIX = "/api/files/"

import storage
from fastapi import APIRouter, Depends, HTTPException

from database import db_query, db_transaction, log_event
from repos.trash_repo import (
    list_trash_archivo_count, list_trash_archivo_rows,
    list_trash_rrhh_count, list_trash_rrhh_rows,
    list_trash_employees_count, list_trash_employees_rows,
    list_versions_rows,
)
from routes.admin.deps import require_admin_role, require_role, require_session
from routes.admin.helpers import _require_modulo, module_meta, paginate

router = APIRouter(prefix="/api/admin", tags=["papelera"], dependencies=[Depends(require_session)])


def _r2_key(file_url: Optional[str]) -> Optional[str]:
    """Extrae la clave de R2 de un `file_url` propio, o `None` si no aplica."""
    url = (file_url or "").strip()
    if url.startswith(_R2_KEY_PREFIX):
        key = url[len(_R2_KEY_PREFIX):].strip()
        return key or None
    return None


def _delete_r2_objects(keys: list) -> list:
    """Borra cada clave de R2, sin abortar el purgado si R2 falla en una.

    OA-006: el purgado es irreversible por definición — si un objeto ya no
    está o R2 da error, se registra y se sigue con el resto en vez de dejar
    el registro de base de datos a medio purgar por un fallo de almacenamiento
    que ya no se puede deshacer.
    """
    borrados = []
    if not keys or not storage.is_configured():
        return borrados
    for key in keys:
        try:
            storage.delete_object(key)
            borrados.append(key)
        except Exception:
            pass
    return borrados


# =============================================================================
# PAPELERA DE RECICLAJE
# =============================================================================

@router.get("/papelera")
def list_trash(
    modulo: str = "Archivo",
    page: int = 1,
    per_page: int = 25,
    _autorizado: str = Depends(require_role("Archivo", "RRHH")),
):
    """Lista los documentos (y empleados en RRHH) en la papelera."""
    _require_modulo(modulo)
    page, per_page, offset = paginate(page, per_page)

    if modulo == "Archivo":
        count_row = list_trash_archivo_count()
        rows = list_trash_archivo_rows(per_page, offset)
    else:
        count_row = list_trash_rrhh_count()
        rows = list_trash_rrhh_rows(per_page, offset)

    total = int(count_row["total"]) if count_row else 0
    return {"total": total, "page": page, "per_page": per_page, "records": [dict(r) for r in rows]}


@router.post("/papelera/{doc_id}/restaurar")
def restore_document(
    doc_id: int,
    modulo: str = "Archivo",
    usuario: str = "",
    _autorizado: str = Depends(require_role("Archivo", "RRHH")),
):
    """Recupera un documento de la papelera (deshace el soft-delete)."""
    _require_modulo(modulo)
    if modulo == "Archivo":
        result = db_query(
            "UPDATE public.datos_archivo SET deleted_at=NULL, deleted_by=NULL, deleted_reason=NULL WHERE id_archivo=%s AND deleted_at IS NOT NULL RETURNING id_archivo",
            [doc_id], fetch="one", commit=True,
        )
    else:
        result = db_query(
            "UPDATE public.datos_rrhh SET deleted_at=NULL, deleted_by=NULL, deleted_reason=NULL WHERE id_rrhh=%s AND deleted_at IS NOT NULL RETURNING id_rrhh",
            [doc_id], fetch="one", commit=True,
        )

    if not result:
        raise HTTPException(404, "Documento no encontrado en papelera")

    log_event(usuario or "sistema", "Restaurar Documento", modulo, f"ID: {doc_id}")
    return {"success": True}


@router.delete("/papelera/{doc_id}/purgar")
def purge_document(
    doc_id: int,
    modulo: str,
    usuario: str,
    _autorizado: str = Depends(require_admin_role("Archivo", "RRHH")),
):
    """Elimina permanentemente un documento de la papelera. Irreversible.

    IN-164 / OR-011 / OR-012: las 2-3 escrituras relacionadas (descriptores,
    versiones y el registro principal) van en una sola `db_transaction()` para
    que no puedan quedar a medias — o se borra todo el rastro del documento, o
    no se borra nada.

    Borrado irreversible: exige `require_admin_role`, no basta con pertenecer
    al módulo (IN-008/OR-009 a OR-012).

    OA-006: el `file_url` actual y el de cada versión histórica se recogen
    antes del `DELETE` y sus objetos se borran de R2 después de confirmar la
    transacción — antes sólo se borraban las filas y el fondo digital crecía
    con huérfanos que ninguna pantalla podía enumerar.
    """
    if modulo == "Archivo":
        existing = db_query(
            "SELECT file_url FROM public.datos_archivo WHERE id_archivo=%s AND deleted_at IS NOT NULL",
            [doc_id], fetch="one",
        )
        if not existing:
            raise HTTPException(404, "Documento no está en papelera")
        version_urls = db_query(
            "SELECT file_url FROM public.documento_versiones WHERE tabla='datos_archivo' AND documento_id=%s",
            [doc_id], fetch="all",
        ) or []
        with db_transaction() as execute:
            execute("DELETE FROM public.archivo_descriptores WHERE id_archivo=%s", [doc_id])
            execute("DELETE FROM public.documento_versiones WHERE tabla='datos_archivo' AND documento_id=%s", [doc_id])
            execute("DELETE FROM public.datos_archivo WHERE id_archivo=%s", [doc_id])
    else:
        existing = db_query(
            "SELECT file_url FROM public.datos_rrhh WHERE id_rrhh=%s AND deleted_at IS NOT NULL",
            [doc_id], fetch="one",
        )
        if not existing:
            raise HTTPException(404, "Documento no está en papelera")
        version_urls = db_query(
            "SELECT file_url FROM public.documento_versiones WHERE tabla='datos_rrhh' AND documento_id=%s",
            [doc_id], fetch="all",
        ) or []
        with db_transaction() as execute:
            execute("DELETE FROM public.documento_versiones WHERE tabla='datos_rrhh' AND documento_id=%s", [doc_id])
            execute("DELETE FROM public.datos_rrhh WHERE id_rrhh=%s", [doc_id])

    keys = {_r2_key(existing.get("file_url"))} | {_r2_key(v.get("file_url")) for v in version_urls}
    keys.discard(None)
    borrados = _delete_r2_objects(sorted(keys))

    log_event(usuario, "Purgar Documento (permanente)", modulo,
              f"ID: {doc_id}, objetos R2 borrados: {len(borrados)}/{len(keys)}")
    return {"success": True}


# Papelera de empleados
@router.get("/papelera/empleados")
def list_trash_employees(
    page: int = 1,
    per_page: int = 25,
    _autorizado: str = Depends(require_role("RRHH")),
):
    page, per_page, offset = paginate(page, per_page)

    count_row = list_trash_employees_count()
    rows = list_trash_employees_rows(per_page, offset)

    total = int(count_row["total"]) if count_row else 0
    return {"total": total, "page": page, "per_page": per_page, "records": [dict(r) for r in rows]}


@router.post("/papelera/empleados/{emp_id}/restaurar")
def restore_employee(
    emp_id: int,
    usuario: str = "",
    _autorizado: str = Depends(require_role("RRHH")),
):
    result = db_query(
        "UPDATE public.empleados SET deleted_at=NULL, deleted_by=NULL, deleted_reason=NULL WHERE id=%s AND deleted_at IS NOT NULL RETURNING id",
        [emp_id], fetch="one", commit=True,
    )
    if not result:
        raise HTTPException(404, "Empleado no encontrado en papelera")
    log_event(usuario or "sistema", "Restaurar Empleado", "RRHH", f"ID: {emp_id}")
    return {"success": True}


@router.delete("/papelera/empleados/{emp_id}/purgar")
def purge_employee(
    emp_id: int,
    usuario: str,
    _autorizado: str = Depends(require_admin_role("RRHH")),
):
    """Borrado irreversible del empleado y sus documentos: exige `require_admin_role`.

    OA-007: antes se borraban `historial_cargos`, `datos_rrhh` y `empleados`
    pero no `documento_versiones` de esos `id_rrhh` — quedaban versiones
    colgando de documentos ya inexistentes, incoherente con `purge_document`
    (que sí las limpia). Ahora se recogen los `id_rrhh` del expediente y sus
    `file_url` (actual + versiones) antes de borrar, y las tres tablas más
    `documento_versiones` van en una sola transacción (IN-164): o se borra el
    expediente entero, o no se borra nada.

    OR-012 (parte de backend, mismo patrón de `deleted_at` ignorado visto en
    `hr.py`/`admin/docs.py`/`files.py` esta ronda): el `DELETE FROM
    datos_rrhh WHERE empleado_id=%s` borraba TODOS los documentos del
    empleado, sin filtrar por `deleted_at` — incluidos documentos vivos que
    nunca pasaron por la papelera. `datos_rrhh.empleado_id` tiene
    `ON DELETE CASCADE` hacia `empleados` (`main.py:381`), así que ni
    siquiera filtrar el `DELETE` explícito basta: borrar la fila de
    `empleados` al final de la transacción arrastra en cascada cualquier
    documento vivo que quedara. Sin poder tocar esa FK (`main.py`
    **[CHOCA]**), el arreglo seguro dentro de este archivo es negarse a
    purgar mientras el empleado tenga documentos vivos, en vez de
    destruirlos en silencio. La lista/confirmación con cédula que pide el
    resto de la ficha (enumerar qué se va a borrar en el modal) sigue
    pendiente, `admin-edit.js` **[CHOCA]**.
    """
    existing = db_query(
        "SELECT id FROM public.empleados WHERE id=%s AND deleted_at IS NOT NULL",
        [emp_id], fetch="one",
    )
    if not existing:
        raise HTTPException(404, "Empleado no está en papelera")

    vivos = db_query(
        "SELECT COUNT(*) AS total FROM public.datos_rrhh WHERE empleado_id=%s AND deleted_at IS NULL",
        [emp_id], fetch="one",
    )
    if vivos and int(vivos["total"]) > 0:
        raise HTTPException(
            409,
            "El empleado tiene documentos que no están en la papelera; "
            "envíalos a la papelera antes de purgar el expediente.",
        )

    doc_rows = db_query(
        "SELECT id_rrhh, file_url FROM public.datos_rrhh WHERE empleado_id=%s",
        [emp_id], fetch="all",
    ) or []
    doc_ids = [d["id_rrhh"] for d in doc_rows]
    version_urls = []
    if doc_ids:
        version_urls = db_query(
            "SELECT file_url FROM public.documento_versiones WHERE tabla='datos_rrhh' AND documento_id = ANY(%s)",
            [doc_ids], fetch="all",
        ) or []

    with db_transaction() as execute:
        execute("DELETE FROM public.historial_cargos WHERE empleado_id=%s", [emp_id])
        if doc_ids:
            execute(
                "DELETE FROM public.documento_versiones WHERE tabla='datos_rrhh' AND documento_id = ANY(%s)",
                [doc_ids],
            )
        execute("DELETE FROM public.datos_rrhh WHERE empleado_id=%s", [emp_id])
        execute("DELETE FROM public.empleados WHERE id=%s", [emp_id])

    keys = {_r2_key(d.get("file_url")) for d in doc_rows} | {_r2_key(v.get("file_url")) for v in version_urls}
    keys.discard(None)
    borrados = _delete_r2_objects(sorted(keys))

    log_event(usuario, "Purgar Empleado (permanente)", "RRHH",
              f"ID: {emp_id}, documentos: {len(doc_ids)}, objetos R2 borrados: {len(borrados)}/{len(keys)}")
    return {"success": True}


# =============================================================================
# VERSIONES DE ARCHIVOS DIGITALES
# =============================================================================

@router.get("/documento/{doc_id}/versiones")
def list_versions(
    doc_id: int,
    modulo: str = "Archivo",
    _autorizado: str = Depends(require_role("Archivo", "RRHH")),
):
    _require_modulo(modulo)
    tabla, _ = module_meta(modulo)
    rows = list_versions_rows(tabla, doc_id)
    return {"versiones": [dict(r) for r in rows]}


@router.post("/documento/{doc_id}/versiones")
def add_version(
    doc_id: int,
    modulo: str = "Archivo",
    file_url: str = "",
    comentario: Optional[str] = None,
    usuario: str = "",
    _autorizado: str = Depends(require_role("Archivo", "RRHH")),
):
    """Registra una nueva versión del archivo digital para un documento."""
    _require_modulo(modulo)
    if file_url and not _SAFE_URL_RE.match(file_url):
        raise HTTPException(400, "file_url inválida")
    # IN-149: la misma comprobación de recorrido de ruta que `files.py:serve_file`
    # aplica al `key`, pero acá sólo se validaba el esquema (`/` o `http(s)://`),
    # no el contenido. Un `file_url` como "/api/files/../../otra-cosa" pasaba el
    # regex y quedaba guardado como versión "válida" para que el visor lo siguiera.
    if file_url and ".." in file_url:
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
def restore_version(
    doc_id: int,
    ver_id: int,
    modulo: str = "Archivo",
    usuario: str = "",
    _autorizado: str = Depends(require_role("Archivo", "RRHH")),
):
    """Restaura el archivo digital de una versión anterior como versión actual.

    OA-008: antes esto sobreescribía `file_url` sin archivar la versión que
    estaba vigente — restaurar v1 por error sobre una v3 buena hacía
    desaparecer v3 de cualquier parte. El historial de versiones sólo tiene
    sentido si es aditivo: la vigente se guarda como versión nueva antes de
    sustituirla, en la misma transacción.
    """
    _require_modulo(modulo)
    tabla, pk = module_meta(modulo)

    ver = db_query(
        "SELECT file_url FROM public.documento_versiones WHERE id=%s AND tabla=%s AND documento_id=%s",
        [ver_id, tabla, doc_id], fetch="one",
    )
    if not ver:
        raise HTTPException(404, "Versión no encontrada")

    current = db_query(
        f"SELECT file_url FROM public.{tabla} WHERE {pk}=%s AND deleted_at IS NULL",
        [doc_id], fetch="one",
    )
    if not current:
        raise HTTPException(404, "Documento no encontrado")

    with db_transaction() as execute:
        current_url = current["file_url"] or ""
        if current_url and current_url != ver["file_url"]:
            last_ver = execute(
                "SELECT COALESCE(MAX(version_num),0) AS vn FROM public.documento_versiones WHERE tabla=%s AND documento_id=%s",
                [tabla, doc_id], fetch="one",
            )
            next_ver = (last_ver["vn"] if last_ver else 0) + 1
            execute(
                "INSERT INTO public.documento_versiones (tabla, documento_id, version_num, file_url, comentario, subido_por) VALUES (%s,%s,%s,%s,%s,%s)",
                [tabla, doc_id, next_ver, current_url, "Reemplazada al restaurar una versión anterior", usuario or "sistema"],
            )

        execute(
            f"UPDATE public.{tabla} SET file_url=%s, updated_at=NOW() WHERE {pk}=%s",
            [ver["file_url"], doc_id],
        )

    log_event(usuario or "sistema", "Restaurar Versión", modulo, f"doc_id={doc_id}, ver_id={ver_id}")
    return {"success": True}


@router.delete("/documento/{doc_id}/versiones/{ver_id}")
def delete_version(
    doc_id: int,
    ver_id: int,
    modulo: str = "Archivo",
    usuario: str = "",
    _autorizado: str = Depends(require_role("Archivo", "RRHH")),
):
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
