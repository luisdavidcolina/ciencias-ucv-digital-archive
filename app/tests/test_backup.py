"""Tests para el módulo de backup: sanitización de columnas y exportación."""
import json
import pytest
from unittest.mock import patch, MagicMock


def _mock_row(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    row.items = lambda: data.items()
    return row


# IN-131/SI-002: desde que /api/admin/backup/{export,restore} exigen
# require_role("Global"), el fixture `client` (usuario "test_user" sin fila
# real en usuarios_sistema) necesita que `routes.admin.deps.db_query` devuelva
# una fila de administrador Global para que las pruebas de sanitización de
# columnas -que no son sobre autorización- sigan pudiendo ejercitar el
# endpoint. La autorización en sí (403 sin ese módulo) se prueba aparte en
# app/tests/test_autorizacion_deps.py.
# Diccionario simple (no MagicMock): `require_role` llama a `.get()` sobre la
# fila y un MagicMock sin `.get` explícito devolvería otro Mock "truthy" en
# vez del valor real, dejando pasar la comparación de módulo por accidente.
_FILA_ADMIN_GLOBAL = {"modulo": "Global", "rol": "Admin", "is_active": True}


class TestColumnSanitization:
    """Verifica que restore rechaza nombres de columna con caracteres peligrosos."""

    def test_columna_valida_insertada(self, client):
        backup = {
            "_metadata": {"version": "1.0", "tables": ["categoria"], "partial": False},
            "categoria": [{"id": 99, "nombre": "Test", "slug": "test"}],
        }
        content = json.dumps(backup).encode()

        call_log = []

        def mock_query(sql, params=None, fetch="all", commit=False):
            call_log.append(sql)
            if "INSERT" in sql:
                return None
            return None

        with patch("routes.backup.db_query", side_effect=mock_query), \
             patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            import io
            res = client.post(
                "/api/admin/backup/restore?mode=merge&requester=admin",
                files={"file": ("backup.json", io.BytesIO(content), "application/json")},
            )

        assert res.status_code == 200
        inserts = [s for s in call_log if "INSERT" in s]
        assert any("id" in s and "nombre" in s and "slug" in s for s in inserts)

    def test_columna_con_inyeccion_descartada(self, client):
        malicious_col = "id); DROP TABLE empleados;--"
        backup = {
            "_metadata": {"version": "1.0", "tables": ["categoria"], "partial": False},
            "categoria": [{malicious_col: 1, "nombre": "Injected"}],
        }
        content = json.dumps(backup).encode()

        inserted_sqls = []

        def mock_query(sql, params=None, fetch="all", commit=False):
            if "INSERT" in sql:
                inserted_sqls.append(sql)
            return None

        with patch("routes.backup.db_query", side_effect=mock_query), \
             patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            import io
            res = client.post(
                "/api/admin/backup/restore?mode=merge&requester=admin",
                files={"file": ("backup.json", io.BytesIO(content), "application/json")},
            )

        assert res.status_code == 200
        for sql in inserted_sqls:
            assert "DROP" not in sql
            assert malicious_col not in sql

    def test_fila_sin_columnas_validas_se_omite(self, client):
        backup = {
            "_metadata": {"version": "1.0", "tables": ["categoria"], "partial": False},
            "categoria": [{"123badname": "x", "spaces here": "y"}],
        }
        content = json.dumps(backup).encode()

        inserted_sqls = []

        def mock_query(sql, params=None, fetch="all", commit=False):
            if "INSERT" in sql:
                inserted_sqls.append(sql)
            return None

        with patch("routes.backup.db_query", side_effect=mock_query), \
             patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            import io
            res = client.post(
                "/api/admin/backup/restore?mode=merge&requester=admin",
                files={"file": ("backup.json", io.BytesIO(content), "application/json")},
            )

        assert res.status_code == 200
        data_inserts = [s for s in inserted_sqls if "backup_history" not in s]
        assert len(data_inserts) == 0

    def test_json_invalido_retorna_400(self, client):
        import io
        with patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            res = client.post(
                "/api/admin/backup/restore?mode=merge",
                files={"file": ("bad.json", io.BytesIO(b"not json at all"), "application/json")},
            )
        assert res.status_code == 400

    def test_sin_metadata_retorna_400(self, client):
        import io
        content = json.dumps({"categoria": []}).encode()
        with patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            res = client.post(
                "/api/admin/backup/restore?mode=merge",
                files={"file": ("bad.json", io.BytesIO(content), "application/json")},
            )
        assert res.status_code == 400

    def test_mode_invalido_retorna_400(self, client):
        import io
        with patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            res = client.post(
                "/api/admin/backup/restore?mode=drop_all",
                files={"file": ("x.json", io.BytesIO(b"{}"), "application/json")},
            )
        assert res.status_code == 400
