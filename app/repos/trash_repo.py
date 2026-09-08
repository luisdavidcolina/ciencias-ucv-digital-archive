"""Consultas SQL de sólo lectura para `routes/trash.py` (papelera y versiones).

IN-042 paso 7: sólo las funciones de listado -- sin `db_transaction()`, sin
lógica condicional multi-paso -- se movieron tal cual. `purge_document`,
`purge_employee`, `restore_version`, `add_version` y `delete_version` se
quedan en `routes/trash.py`: mezclan varias sentencias con decisiones según
el estado y no son "mover tal cual" como esto.
"""

from database import db_query


def list_trash_archivo_count() -> dict | None:
    return db_query(
        "SELECT COUNT(*) AS total FROM public.datos_archivo WHERE deleted_at IS NOT NULL",
        fetch="one",
    )


def list_trash_archivo_rows(per_page: int, offset: int) -> list:
    return db_query(
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


def list_trash_rrhh_count() -> dict | None:
    return db_query(
        """SELECT COUNT(*) AS total FROM public.datos_rrhh dr
           LEFT JOIN public.empleados e ON dr.empleado_id = e.id
           WHERE dr.deleted_at IS NOT NULL""",
        fetch="one",
    )


def list_trash_rrhh_rows(per_page: int, offset: int) -> list:
    return db_query(
        """SELECT dr.id_rrhh AS id,
                  COALESCE(td.nombre,'') AS titulo,
                  COALESCE(e.nombres || ' ' || e.apellidos, '') AS autor,
                  COALESCE(e.nombres || ' ' || e.apellidos, '') AS empleado,
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


def list_trash_employees_count() -> dict | None:
    return db_query(
        "SELECT COUNT(*) AS total FROM public.empleados WHERE deleted_at IS NOT NULL",
        fetch="one",
    )


def list_trash_employees_rows(per_page: int, offset: int) -> list:
    return db_query(
        """SELECT id, cedula, nombres || ' ' || apellidos AS nombre,
                  TO_CHAR(deleted_at,'YYYY-MM-DD HH24:MI') AS deleted_at, deleted_by
           FROM public.empleados
           WHERE deleted_at IS NOT NULL
           ORDER BY deleted_at DESC
           LIMIT %s OFFSET %s""",
        [per_page, offset], fetch="all",
    ) or []


def list_versions_rows(tabla: str, doc_id: int) -> list:
    return db_query(
        """SELECT id, version_num, file_url, comentario, subido_por,
                  TO_CHAR(created_at,'YYYY-MM-DD HH24:MI') AS created_at
           FROM public.documento_versiones
           WHERE tabla=%s AND documento_id=%s
           ORDER BY version_num DESC""",
        [tabla, doc_id], fetch="all",
    ) or []
