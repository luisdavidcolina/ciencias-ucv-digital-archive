import os
import logging
from typing import List

import psycopg2
from psycopg2 import pool as pg_pool
from psycopg2.extras import RealDictCursor
from dotenv import load_dotenv
from fastapi import HTTPException
import bcrypt

load_dotenv()
DATABASE_URL = os.getenv("DATABASE_URL")

logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger("DigitalArchive")


# =============================================================================
# CONNECTION POOL
# =============================================================================

_pool: pg_pool.ThreadedConnectionPool = None


def _get_pool() -> pg_pool.ThreadedConnectionPool:
    global _pool
    if _pool is None:
        _pool = pg_pool.ThreadedConnectionPool(
            minconn=1,
            maxconn=5,
            dsn=os.environ.get("DATABASE_URL", ""),
        )
    return _pool


# Kept for backwards-compatibility (not used by db_query anymore)
def get_db_connection():
    """Crea y retorna una conexión a Neon (PostgreSQL)."""
    if not DATABASE_URL:
        logger.error("DATABASE_URL no está definido en .env")
        return None
    try:
        conn = psycopg2.connect(DATABASE_URL, cursor_factory=RealDictCursor)
        return conn
    except Exception as e:
        logger.error(f"Error conectando a Neon: {e}")
        return None


def db_query(sql: str, params=None, fetch: str = "all", commit: bool = False):
    """Helper unificado para ejecutar queries en Neon usando ThreadedConnectionPool.

    Args:
        sql:    Sentencia SQL a ejecutar.
        params: Parámetros para la sentencia (tupla o lista).
        fetch:  'all' | 'one' | 'none'
        commit: Si True realiza commit al terminar.

    Returns:
        Lista de filas, una fila o None según `fetch`.
    """
    if not DATABASE_URL:
        logger.error("DATABASE_URL no está definido en .env")
        if fetch == "all":
            return []
        return None

    conn = None
    try:
        conn = _get_pool().getconn()
        with conn.cursor(cursor_factory=RealDictCursor) as cur:
            cur.execute(sql, params or ())
            result = None
            if fetch == "all":
                result = cur.fetchall()
                if result is None:
                    result = []
            elif fetch == "one":
                result = cur.fetchone()
                # None is a valid return for fetchone (no row found)
            # fetch == "none": no fetch needed
        if commit:
            conn.commit()
        return result
    except Exception as e:
        if conn is not None:
            try:
                conn.rollback()
            except Exception:
                pass
        logger.error(f"Error en query SQL: {e}\nSQL: {sql}")
        raise HTTPException(status_code=500, detail=f"Error en base de datos: {e}")
    finally:
        if conn is not None:
            try:
                _get_pool().putconn(conn)
            except Exception:
                pass


# =============================================================================
# AUDITORÍA
# =============================================================================

def ensure_audit_table():
    """Crea la tabla audit_log en Neon si no existe."""
    try:
        db_query(
            """
            CREATE TABLE IF NOT EXISTS public.audit_log (
                id        SERIAL PRIMARY KEY,
                timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                usuario   VARCHAR(100),
                accion    VARCHAR(150),
                modulo    VARCHAR(100),
                detalle   TEXT,
                status    VARCHAR(30) DEFAULT 'Success'
            )
            """,
            fetch="none",
            commit=True,
        )
    except Exception as e:
        logger.error(f"No se pudo crear tabla audit_log: {e}")


def log_event(
    usuario: str,
    accion: str,
    modulo: str,
    detalle: str,
    status_str: str = "Success",
) -> None:
    """Inserta un evento en audit_log. Falla silenciosamente."""
    try:
        db_query(
            "INSERT INTO public.audit_log (usuario, accion, modulo, detalle, status) "
            "VALUES (%s, %s, %s, %s, %s)",
            (usuario, accion, modulo, detalle, status_str),
            fetch="none",
            commit=True,
        )
    except Exception as e:
        logger.error(f"No se pudo registrar evento de auditoría: {e}")


# =============================================================================
# UTILIDADES COMPARTIDAS
# =============================================================================

def split_terms(val_str: str) -> List[str]:
    """Divide una cadena separada por ';' en términos limpios."""
    if not val_str:
        return []
    return [t.strip() for t in str(val_str).split(";") if t.strip()]


def hash_password(password: str) -> str:
    """Retorna el hash bcrypt de una contraseña."""
    return bcrypt.hashpw(password.encode("utf-8"), bcrypt.gensalt()).decode("utf-8")


def verify_password(plain_password: str, hashed_password: str) -> bool:
    """Verifica si una contraseña en texto plano coincide con su hash."""
    return bcrypt.checkpw(plain_password.encode("utf-8"), hashed_password.encode("utf-8"))
