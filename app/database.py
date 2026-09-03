import os
import logging
import threading
import time as _time
from contextlib import contextmanager
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
            # Keepalives TCP: evita que Neon cierre conexiones inactivas del pool,
            # eliminando el ciclo lento de "conexión rota → reintento" en la 1ra petición
            keepalives=1,
            keepalives_idle=30,
            keepalives_interval=10,
            keepalives_count=3,
            connect_timeout=10,
        )
    return _pool


def db_query(sql: str, params=None, fetch: str = "all", commit: bool = False, _retries: int = 2):
    """Helper unificado para ejecutar queries en Neon usando ThreadedConnectionPool.

    Args:
        sql:      Sentencia SQL a ejecutar.
        params:   Parámetros para la sentencia (tupla o lista).
        fetch:    'all' | 'one' | 'none'
        commit:   Si True realiza commit al terminar.
        _retries: Reintentos cuando el pool está agotado (PoolError).

    Returns:
        Lista de filas, una fila o None según `fetch`.
    """
    if not DATABASE_URL:
        logger.error("DATABASE_URL no está definido en .env")
        if fetch == "all":
            return []
        return None

    for attempt in range(_retries + 1):
        conn = None
        try:
            conn = _get_pool().getconn()
            # Verifica que la conexión esté activa; la descarta si está cerrada
            if conn.closed:
                _get_pool().putconn(conn, close=True)
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
                # fetch == "none": no fetch needed
            if commit:
                conn.commit()
            else:
                # IN-004: psycopg2 abre una transacción implícita en el primer
                # execute(), también para un SELECT. Sin este rollback, la
                # conexión vuelve al pool en estado "idle in transaction":
                # retiene el snapshot de Neon, bloquea VACUUM y, con varias
                # instancias concurrentes, agota el cupo de conexiones del
                # proyecto (IN-107). Un rollback sin cambios que confirmar es
                # barato y deja la conexión limpia para el siguiente préstamo.
                conn.rollback()
            return result
        except pg_pool.PoolError:
            if conn is not None:
                try:
                    _get_pool().putconn(conn)
                    conn = None
                except Exception:
                    pass
            if attempt < _retries:
                _time.sleep(0.15 * (attempt + 1))
                continue
            raise HTTPException(503, "Base de datos temporalmente no disponible")
        except psycopg2.OperationalError as e:
            # Conexión rota — descarta del pool y reintenta
            if conn is not None:
                try:
                    _get_pool().putconn(conn, close=True)
                    conn = None
                except Exception:
                    pass
            if attempt < _retries:
                logger.warning(f"Conexión perdida, reintentando (intento {attempt+1}): {e}")
                _time.sleep(0.2 * (attempt + 1))
                continue
            raise HTTPException(503, "Base de datos no disponible")
        except Exception as e:
            if conn is not None:
                try:
                    conn.rollback()
                except Exception:
                    pass
            sql_preview = sql.strip()[:120].replace("\n", " ")
            logger.error(f"Error SQL [{type(e).__name__}]: {e} | SQL: {sql_preview}…")
            raise HTTPException(status_code=500, detail=f"Error en base de datos: {type(e).__name__}")
        finally:
            if conn is not None:
                try:
                    _get_pool().putconn(conn)
                except Exception:
                    pass


@contextmanager
def db_transaction():
    """Gestor de contexto para agrupar varias escrituras en una sola transacción.

    IN-164: `db_query(commit=True)` confirma su propia sentencia y devuelve la
    conexión al pool — no hay forma de agrupar dos escrituras relacionadas en
    una sola unidad atómica. Esto ya causó pérdida de datos real (OR-004,
    OR-011, OR-012): una operación compuesta (alta de empleado + su documento,
    purga de un registro + sus descriptores, importación CSV de varios
    campos) puede fallar a mitad de camino y dejar el estado a medias.

    `db_transaction()` toma UNA conexión del mismo pool que usa `db_query`,
    cede un `execute(sql, params=None, fetch="none")` ligado a esa conexión
    para correr todas las sentencias que hagan falta, y al salir del bloque
    `with`:
      - si no hubo excepción: hace commit una sola vez, para todas las
        sentencias juntas.
      - si hubo cualquier excepción: hace rollback — NINGUNA de las
        sentencias del bloque persiste — y relanza la excepción (envuelta en
        HTTPException 500 si no lo era ya).
    La conexión siempre vuelve al pool al salir, éxito o fallo, igual que
    `db_query`.

    `db_query` sigue siendo el helper para el caso simple (una sola
    sentencia) — esa decisión no cambia. `db_transaction()` es solo para
    cuando dos o más escrituras deben tener éxito o fallar juntas.

    Uso (ejemplo copiable):

        from database import db_transaction

        with db_transaction() as execute:
            execute("DELETE FROM public.archivo_descriptores WHERE id_archivo=%s", [doc_id])
            execute("DELETE FROM public.documento_versiones WHERE tabla='datos_archivo' AND documento_id=%s", [doc_id])
            execute("DELETE FROM public.datos_archivo WHERE id_archivo=%s", [doc_id])
        # si llegó hasta aquí sin excepción, las tres sentencias ya hicieron commit juntas.
        # si cualquiera lanzó, ninguna de las anteriores quedó persistida.

    Para leer un resultado dentro de la misma transacción (por ejemplo un
    `RETURNING` o un `SELECT ... FOR UPDATE`), usa `fetch`:

        with db_transaction() as execute:
            row = execute("UPDATE ... RETURNING id", [x], fetch="one")
            execute("INSERT INTO ... VALUES (%s)", [row["id"]])
    """
    if not DATABASE_URL:
        logger.error("DATABASE_URL no está definido en .env")
        raise HTTPException(503, "Base de datos no disponible")

    conn = None
    try:
        conn = _get_pool().getconn()
        if conn.closed:
            _get_pool().putconn(conn, close=True)
            conn = _get_pool().getconn()

        def execute(sql: str, params=None, fetch: str = "none"):
            with conn.cursor(cursor_factory=RealDictCursor) as cur:
                cur.execute(sql, params or ())
                if fetch == "all":
                    return cur.fetchall() or []
                if fetch == "one":
                    return cur.fetchone()
                return None

        yield execute
        conn.commit()
    except HTTPException:
        if conn is not None:
            try:
                conn.rollback()
            except Exception:
                pass
        raise
    except Exception as e:
        if conn is not None:
            try:
                conn.rollback()
            except Exception:
                pass
        logger.error(f"Error en transacción, revertida: [{type(e).__name__}] {e}")
        raise HTTPException(status_code=500, detail=f"Error en base de datos: {type(e).__name__}")
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
    """Inserta un evento en audit_log en segundo plano (no bloquea la respuesta).

    Falla silenciosamente.
    """
    def _do_insert():
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

    # daemon=False: el proceso no termina hasta que el INSERT se complete,
    # garantizando durabilidad en entornos serverless (Vercel).
    threading.Thread(target=_do_insert, daemon=False).start()


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
