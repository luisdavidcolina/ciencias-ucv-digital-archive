"""Pruebas de app/database.py: `db_query` (IN-004) y `db_transaction` (IN-164).

La suite no tiene una Neon real disponible, así que estas pruebas sustituyen
el pool de conexiones por un `FakeConn`/`FakeCursor` en memoria que registra
qué se ejecutó y cuántas veces se llamó a `commit()`/`rollback()`. Es una
prueba de la LÓGICA de confirmación/reversión de `database.py` (qué decide
hacer con la conexión), no del comportamiento transaccional real de
PostgreSQL — eso ya lo garantiza psycopg2/Postgres y no hace falta
reprobarlo aquí.
"""
import pytest
from unittest.mock import patch

import database


class FakeCursor:
    def __init__(self, conn):
        self.conn = conn

    def execute(self, sql, params=None):
        self.conn.executed.append((sql, params))
        if self.conn.fail_on and self.conn.fail_on in sql:
            raise RuntimeError("boom")

    def fetchall(self):
        return self.conn.fetch_results.get("all", [])

    def fetchone(self):
        return self.conn.fetch_results.get("one")

    def __enter__(self):
        return self

    def __exit__(self, *exc_info):
        return False


class FakeConn:
    def __init__(self, fail_on=None, fetch_results=None):
        self.executed = []
        self.committed = 0
        self.rolled_back = 0
        self.closed = 0
        self.fail_on = fail_on
        self.fetch_results = fetch_results or {}

    def cursor(self, cursor_factory=None):
        return FakeCursor(self)

    def commit(self):
        self.committed += 1

    def rollback(self):
        self.rolled_back += 1


class FakePool:
    """Sustituye a ThreadedConnectionPool: siempre entrega la misma FakeConn."""

    def __init__(self, conn):
        self.conn = conn
        self.putconn_calls = []

    def getconn(self):
        return self.conn

    def putconn(self, conn, close=False):
        self.putconn_calls.append(close)


@pytest.fixture(autouse=True)
def _database_url():
    with patch.object(database, "DATABASE_URL", "postgres://fake"):
        yield


# =============================================================================
# IN-004: un SELECT sin commit no debe dejar "idle in transaction"
# =============================================================================

class TestDbQueryIdleInTransaction:
    def test_select_sin_commit_hace_rollback_antes_de_devolver_al_pool(self):
        """fetch sin commit=True: antes del fix, la conexión volvía al pool con
        una transacción implícita abierta (IN-004). Ahora debe revertirse."""
        conn = FakeConn(fetch_results={"all": []})
        pool = FakePool(conn)
        with patch.object(database, "_get_pool", return_value=pool):
            result = database.db_query("SELECT 1", fetch="all", commit=False)
        assert result == []
        assert conn.rolled_back == 1
        assert conn.committed == 0
        assert pool.putconn_calls == [False]

    def test_escritura_con_commit_hace_commit_y_no_rollback(self):
        conn = FakeConn()
        pool = FakePool(conn)
        with patch.object(database, "_get_pool", return_value=pool):
            database.db_query("UPDATE x SET y=1", fetch="none", commit=True)
        assert conn.committed == 1
        assert conn.rolled_back == 0


# =============================================================================
# IN-164: db_transaction() agrupa varias escrituras en una unidad atómica
# =============================================================================

class TestDbTransaction:
    def test_dos_escrituras_persisten_juntas_cuando_no_hay_error(self):
        conn = FakeConn()
        pool = FakePool(conn)
        with patch.object(database, "_get_pool", return_value=pool):
            with database.db_transaction() as execute:
                execute("DELETE FROM a WHERE id=%s", [1])
                execute("DELETE FROM b WHERE id=%s", [1])
        assert len(conn.executed) == 2
        assert conn.committed == 1
        assert conn.rolled_back == 0
        # la conexión siempre vuelve al pool, sin descartarla
        assert pool.putconn_calls == [False]

    def test_excepcion_a_mitad_revierte_las_dos_ninguna_persiste(self):
        """Fuerza que la 2ª sentencia falle y comprueba que NINGUNA de las dos
        quedó confirmada: se ejecutaron ambas (la 1ª sí llegó a la conexión)
        pero rollback() se llamó y commit() nunca."""
        conn = FakeConn(fail_on="DELETE FROM b")
        pool = FakePool(conn)
        with patch.object(database, "_get_pool", return_value=pool):
            with pytest.raises(Exception):
                with database.db_transaction() as execute:
                    execute("DELETE FROM a WHERE id=%s", [1])
                    execute("DELETE FROM b WHERE id=%s", [1])  # lanza RuntimeError
        assert len(conn.executed) == 2
        assert conn.committed == 0
        assert conn.rolled_back == 1
        assert pool.putconn_calls == [False]

    def test_execute_permite_leer_resultados_dentro_de_la_transaccion(self):
        conn = FakeConn(fetch_results={"one": {"id": 7}})
        pool = FakePool(conn)
        with patch.object(database, "_get_pool", return_value=pool):
            with database.db_transaction() as execute:
                row = execute("UPDATE a SET x=1 WHERE id=%s RETURNING id", [7], fetch="one")
                assert row == {"id": 7}
                execute("INSERT INTO b (a_id) VALUES (%s)", [row["id"]])
        assert conn.committed == 1
