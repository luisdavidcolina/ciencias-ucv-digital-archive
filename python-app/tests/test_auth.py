"""Tests para rutas de autenticación: /api/auth/login y /api/auth/restore."""
import bcrypt
import pytest
from unittest.mock import patch, MagicMock


def _make_user_row(usuario="archivero", modulo="Archivo", rol="Normal"):
    hashed = bcrypt.hashpw(b"clave123", bcrypt.gensalt()).decode()
    row = MagicMock()
    data = {
        "id": 1, "usuario": usuario, "nombre_usuario": "Archivero Test",
        "contrasena": hashed, "modulo": modulo, "rol": rol,
    }
    row.__getitem__ = lambda self, k: data[k]
    row.keys = lambda: data.keys()
    return row, hashed


class TestLogin:
    def test_login_correcto(self, client):
        row, _ = _make_user_row()
        with (
            patch("database.db_query", return_value=row),
            patch("database.log_event"),
        ):
            res = client.post("/api/auth/login", json={"username": "archivero", "password": "clave123"})
        assert res.status_code == 200
        body = res.json()
        assert body["success"] is True
        assert body["user"]["username"] == "archivero"
        assert body["user"]["modulo"] == "Archivo"

    def test_login_usuario_no_existe(self, client):
        with (
            patch("database.db_query", return_value=None),
            patch("database.log_event"),
        ):
            res = client.post("/api/auth/login", json={"username": "nadie", "password": "x"})
        assert res.status_code == 401

    def test_login_contrasena_incorrecta(self, client):
        row, _ = _make_user_row()
        with (
            patch("database.db_query", return_value=row),
            patch("database.log_event"),
        ):
            res = client.post("/api/auth/login", json={"username": "archivero", "password": "INCORRECTA"})
        assert res.status_code == 401

    def test_login_payload_vacio(self, client):
        res = client.post("/api/auth/login", json={})
        assert res.status_code == 422  # Pydantic validation error


class TestRestoreSession:
    def test_restore_usuario_valido(self, client):
        row, _ = _make_user_row()
        with patch("database.db_query", return_value=row):
            res = client.post("/api/auth/restore", json={"username": "archivero"})
        assert res.status_code == 200
        assert res.json()["success"] is True

    def test_restore_usuario_no_existe(self, client):
        with patch("database.db_query", return_value=None):
            res = client.post("/api/auth/restore", json={"username": "fantasma"})
        assert res.status_code == 404
