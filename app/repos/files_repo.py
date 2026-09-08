"""Consultas SQL para `routes/files.py` (subida/descarga de archivos en R2).

IN-042 paso 2: funciones de consulta movidas tal cual desde `routes/files.py`,
sin cambiar SQL ni comportamiento. `routes/files.py` las importa y las usa;
`db_query` sigue siendo el único ejecutor, ahora invocado desde este módulo.
"""

from database import db_query


def usuario_modulo_activo(usuario: str) -> dict | None:
    """Fila `{modulo, is_active}` de `usuarios_sistema` para `usuario`, o None.

    Usada por `_modulo_permite_clave` (routes/files.py) para decidir si una
    sesión puede leer una clave R2 de un módulo dado.
    """
    return db_query(
        "SELECT modulo, is_active FROM public.usuarios_sistema WHERE usuario = %s LIMIT 1",
        [usuario], fetch="one",
    )


def clave_en_papelera(url: str) -> bool:
    """True si `url` es el `file_url` vigente de un documento en la papelera
    (`datos_archivo` o `datos_rrhh`, `deleted_at IS NOT NULL`).
    """
    fila = db_query(
        "SELECT 1 AS x FROM public.datos_archivo WHERE file_url = %s AND deleted_at IS NOT NULL "
        "UNION ALL "
        "SELECT 1 AS x FROM public.datos_rrhh WHERE file_url = %s AND deleted_at IS NOT NULL "
        "LIMIT 1",
        [url, url], fetch="one",
    )
    return fila is not None
