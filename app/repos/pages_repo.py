from database import db_query


def usuario_modulo(usuario: str):
    return db_query(
        "SELECT modulo FROM public.usuarios_sistema "
        "WHERE usuario = %s AND COALESCE(is_active, TRUE) LIMIT 1",
        [usuario], fetch="one",
    )
