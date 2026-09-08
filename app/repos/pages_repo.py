"""Consultas SQL para `routes/pages.py` (control de acceso a páginas internas).

IN-042 paso 6: función de consulta movida tal cual desde `routes/pages.py`,
sin cambiar SQL ni comportamiento. `routes/pages.py` la importa y la usa;
`db_query` sigue siendo el único ejecutor.
"""

from database import db_query


def usuario_modulo(usuario: str) -> dict | None:
    """Fila `{modulo}` de `usuarios_sistema` para `usuario`, o None.

    Usada por `pages.py` sólo para exigir rol Global en páginas internas
    (comparativa competitiva). No es duplicado de `lookups_repo.user_modules`
    ni de `files_repo.usuario_modulo_activo`: cada una resuelve una pregunta
    distinta sobre la misma tabla y no comparten SQL a propósito --
    `user_modules` expande "Global" a la lista completa de módulos activos
    (para filtrar choices), `usuario_modulo_activo` valida si una sesión
    puede leer una clave R2 concreta (usa `is_active` explícito, sin TRIM),
    y ésta sólo compara `modulo == "Global"` (usa `COALESCE(is_active, TRUE)`,
    sin TRIM). No fusionar sin verificar los tres casos de uso.
    """
    return db_query(
        "SELECT modulo FROM public.usuarios_sistema "
        "WHERE usuario = %s AND COALESCE(is_active, TRUE) LIMIT 1",
        [usuario], fetch="one",
    )
