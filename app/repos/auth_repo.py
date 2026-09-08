"""Consultas de acceso a datos de routes/auth.py (IN-042, paso 5).

Movidas tal cual desde routes/auth.py -- sin cambiar SQL ni comportamiento.
db_query sigue siendo el unico ejecutor.
"""
from database import db_query

_LOCK_WINDOW_SECONDS = 300
_LOCK_THRESHOLD = 5
_LOCK_SECONDS = 30

_UPSERT_ATTEMPT_SQL = f"""
    INSERT INTO public.login_attempts
        (usuario, ip, intentos, primer_intento_at, ultimo_intento_at, bloqueado_hasta)
    VALUES (%s, %s, 1, NOW(), NOW(), NULL)
    ON CONFLICT (usuario, ip) DO UPDATE SET
        intentos = CASE
            WHEN public.login_attempts.ultimo_intento_at
                 < NOW() - INTERVAL '{_LOCK_WINDOW_SECONDS} seconds'
            THEN 1
            ELSE public.login_attempts.intentos + 1
        END,
        primer_intento_at = CASE
            WHEN public.login_attempts.ultimo_intento_at
                 < NOW() - INTERVAL '{_LOCK_WINDOW_SECONDS} seconds'
            THEN NOW()
            ELSE public.login_attempts.primer_intento_at
        END,
        ultimo_intento_at = NOW(),
        bloqueado_hasta = CASE
            WHEN (CASE
                    WHEN public.login_attempts.ultimo_intento_at
                         < NOW() - INTERVAL '{_LOCK_WINDOW_SECONDS} seconds'
                    THEN 1
                    ELSE public.login_attempts.intentos + 1
                  END) >= {_LOCK_THRESHOLD}
            THEN NOW() + INTERVAL '{_LOCK_SECONDS} seconds'
            ELSE public.login_attempts.bloqueado_hasta
        END
"""


def login_lock_row(usuario: str, ip: str):
    return db_query(
        "SELECT bloqueado_hasta FROM public.login_attempts WHERE usuario = %s AND ip = %s",
        (usuario, ip),
        fetch="one",
    )


def register_login_failure(usuario: str, ip: str) -> None:
    db_query(_UPSERT_ATTEMPT_SQL, (usuario, ip), fetch="none", commit=True)


def clear_login_failures(usuario: str, ip: str) -> None:
    db_query(
        "DELETE FROM public.login_attempts WHERE usuario = %s AND ip = %s",
        (usuario, ip),
        fetch="none",
        commit=True,
    )


def login_user_rows(username: str):
    """Filas para /login: incluye contrasena para verificar la clave."""
    return db_query(
        "SELECT usuario, nombre_usuario, contrasena, modulo, rol, "
        "COALESCE(is_active, TRUE) AS is_active "
        "FROM public.usuarios_sistema "
        "WHERE TRIM(usuario) = %s",
        (username,),
        fetch="all",
    )


def update_last_login(username: str) -> None:
    db_query(
        "UPDATE public.usuarios_sistema SET last_login = NOW() WHERE TRIM(usuario) = %s",
        (username,), fetch="none", commit=True,
    )


def restore_user_rows(username: str):
    """Filas para /restore: sin contrasena, no hace falta reverificar la clave."""
    return db_query(
        "SELECT usuario, nombre_usuario, modulo, rol, "
        "COALESCE(is_active, TRUE) AS is_active "
        "FROM public.usuarios_sistema "
        "WHERE TRIM(usuario) = %s",
        (username,),
        fetch="all",
    )
