"""Tests para el panel de administración: list_all, stats y users."""
import pytest
from unittest.mock import patch, MagicMock
from fastapi.testclient import TestClient


def _mock_row(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


def _fila_usuario(**data):
    """Fila de `usuarios_sistema` para mockear `routes.admin.deps.db_query`
    (H3-pruebas-legacy: los endpoints de este archivo ahora exigen
    `require_role`/`require_admin_role`, construidos por el carril O1)."""
    base = {"modulo": "Archivo", "rol": "Admin", "is_active": True}
    base.update(data)
    row = MagicMock()
    row.__getitem__ = lambda self, k: base[k]
    row.get = lambda k, default=None: base.get(k, default)
    row.keys = lambda: base.keys()
    row.__iter__ = lambda self: iter(base)
    return row


class TestListAll:
    def test_list_all_archivo_retorna_paginado(self, client_as):
        c = client_as("archivero")
        count_row  = _mock_row(total=1)
        data_row   = _mock_row(
            id=1, titulo="Informe Test", autor="Test",
            fecha="2024-01-10", doc_type="Informe", ubicacion="Estante A",
        )

        def mock_query(sql, params=None, fetch="all", commit=False):
            if "COUNT" in sql:
                return count_row
            return [data_row]

        with (
            patch("routes.admin.deps.db_query", return_value=_fila_usuario(modulo="Archivo", rol="Normal")),
            patch("routes.admin.docs.db_query", side_effect=mock_query),
        ):
            res = c.get("/api/admin/list_all?modulo=Archivo&page=1&per_page=10")

        assert res.status_code == 200
        body = res.json()
        assert "total"   in body
        assert "page"    in body
        assert "per_page" in body
        assert "records" in body
        assert body["total"] == 1
        assert body["page"]  == 1
        assert len(body["records"]) == 1
        assert body["records"][0]["titulo"] == "Informe Test"

    def test_list_all_paginacion_segunda_pagina(self, client_as):
        c = client_as("archivero2")
        count_row = _mock_row(total=30)

        def mock_query(sql, params=None, fetch="all", commit=False):
            if "COUNT" in sql:
                return count_row
            return []

        with (
            patch("routes.admin.deps.db_query", return_value=_fila_usuario(modulo="Archivo", rol="Normal")),
            patch("routes.admin.docs.db_query", side_effect=mock_query),
        ):
            res = c.get("/api/admin/list_all?modulo=Archivo&page=2&per_page=25")

        assert res.status_code == 200
        body = res.json()
        assert body["page"]  == 2
        assert body["total"] == 30

    def test_list_all_per_page_maxima_100(self, client_as):
        c = client_as("archivero3")
        count_row = _mock_row(total=0)

        def mock_query(sql, params=None, fetch="all", commit=False):
            if "COUNT" in sql:
                return count_row
            return []

        with (
            patch("routes.admin.deps.db_query", return_value=_fila_usuario(modulo="Archivo", rol="Normal")),
            patch("routes.admin.docs.db_query", side_effect=mock_query),
        ):
            res = c.get("/api/admin/list_all?modulo=Archivo&per_page=9999")

        assert res.status_code == 200
        assert res.json()["per_page"] == 100  # Clamped to 100

    def test_list_all_sin_resultados(self, client_as):
        c = client_as("archivero4")
        count_row = _mock_row(total=0)

        def mock_query(sql, params=None, fetch="all", commit=False):
            if "COUNT" in sql:
                return count_row
            return []

        with (
            patch("routes.admin.deps.db_query", return_value=_fila_usuario(modulo="Archivo", rol="Normal")),
            patch("routes.admin.docs.db_query", side_effect=mock_query),
        ):
            res = c.get("/api/admin/list_all?modulo=Archivo")

        assert res.status_code == 200
        body = res.json()
        assert body["total"]   == 0
        assert body["records"] == []


class TestAuthGuard:
    def test_admin_sin_sesion_retorna_401(self, anon_client):
        """Sin cookie ds_session, los endpoints admin deben retornar 401.

        Usa `anon_client` (SI-226): require_session corre de verdad, sin
        override que lo aplane a "hay sesión siempre".
        """
        res = anon_client.get("/api/admin/list_all?modulo=Archivo")
        assert res.status_code == 401


class TestGetUsers:
    def test_usuarios_ocultan_contrasena(self, client_as):
        c = client_as("global_admin")
        user_row = _mock_row(
            id=1, usuario="archivero", nombre_usuario="Test",
            modulo="Archivo", rol="Normal",
        )
        with (
            patch("routes.admin.deps.db_query", return_value=_fila_usuario(modulo="Global", rol="Admin")),
            patch("routes.admin.users.db_query", return_value=[user_row]),
        ):
            res = c.get("/api/admin/users")

        assert res.status_code == 200
        users = res.json()
        assert len(users) == 1
        assert users[0]["password"] == "••••••••"
        assert "contrasena" not in users[0]
