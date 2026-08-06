"""Tests para rutas de autenticación: /api/auth/login y /api/auth/restore."""
import bcrypt
import pytest
from unittest.mock import patch, MagicMock


def _make_user_row(usuario="archivero", modulo="Archivo", rol="Normal"):
    hashed = bcrypt.hashpw(b"clave123", bcrypt.gensalt()).decode()
    data = {
        "id": 1, "usuario": usuario, "nombre_usuario": "Archivero Test",
        "contrasena": hashed, "modulo": modulo, "rol": rol, "is_active": True,
    }
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.get = lambda k, default=None: data.get(k, default)
    row.keys = lambda: data.keys()
    return row, hashed


def _make_token(username: str) -> str:
    """Genera un token HMAC válido para el usuario dado."""
    from core.security import generate_session_token
    return generate_session_token(username)


class TestLogin:
    def test_login_correcto(self, client):
        row, _ = _make_user_row()
        call_n = [0]
        def _qry(sql, params=None, fetch="all", commit=False):
            call_n[0] += 1
            return [row] if call_n[0] == 1 else None
        with (
            patch("routes.auth.db_query", side_effect=_qry),
            patch("routes.auth.log_event"),
        ):
            res = client.post("/api/auth/login", json={"username": "archivero", "password": "clave123"})
        assert res.status_code == 200
        body = res.json()
        assert body["success"] is True
        assert body["user"]["username"] == "archivero"
        assert body["user"]["modulo"] == "Archivo"

    def test_login_usuario_no_existe(self, client):
        with (
            patch("routes.auth.db_query", return_value=[]),
            patch("routes.auth.log_event"),
        ):
            res = client.post("/api/auth/login", json={"username": "nadie", "password": "x"})
        assert res.status_code == 401

    def test_login_contrasena_incorrecta(self, client):
        row, _ = _make_user_row()
        with (
            patch("routes.auth.db_query", return_value=[row]),
            patch("routes.auth.log_event"),
        ):
            res = client.post("/api/auth/login", json={"username": "archivero", "password": "INCORRECTA"})
        assert res.status_code == 401

    def test_login_payload_vacio(self, client):
        res = client.post("/api/auth/login", json={})
        assert res.status_code == 422

    def test_login_username_demasiado_largo(self, client):
        res = client.post("/api/auth/login", json={"username": "u" * 101, "password": "clave"})
        assert res.status_code == 422


class TestRestoreSession:
    def test_restore_con_token_valido(self, client):
        """Con cookie de sesión válida, restore renueva la sesión correctamente."""
        row, _ = _make_user_row()
        token = _make_token("archivero")
        with (
            patch("routes.auth.db_query", return_value=[row]),
            patch("routes.auth.log_event"),
        ):
            res = client.post(
                "/api/auth/restore",
                json={"username": "archivero"},
                cookies={"ds_session": token},
            )
        assert res.status_code == 200
        assert res.json()["success"] is True

    def test_restore_sin_cookie_retorna_401(self, client):
        """Sin cookie, restore debe rechazar — previene acceso por solo conocer el username."""
        res = client.post("/api/auth/restore", json={"username": "archivero"})
        assert res.status_code == 401

    def test_restore_token_de_otro_usuario_retorna_401(self, client):
        """Token de usuario B no sirve para restaurar sesión de usuario A."""
        token_otro = _make_token("otro_usuario")
        res = client.post(
            "/api/auth/restore",
            json={"username": "archivero"},
            cookies={"ds_session": token_otro},
        )
        assert res.status_code == 401

    def test_restore_token_invalido_retorna_401(self, client):
        res = client.post(
            "/api/auth/restore",
            json={"username": "archivero"},
            cookies={"ds_session": "token.falso.invalido"},
        )
        assert res.status_code == 401

    def test_restore_usuario_no_existe_en_bd(self, client):
        """Token válido pero usuario ya no existe en BD."""
        token = _make_token("archivero")
        with (
            patch("routes.auth.db_query", return_value=[]),
            patch("routes.auth.log_event"),
        ):
            res = client.post(
                "/api/auth/restore",
                json={"username": "archivero"},
                cookies={"ds_session": token},
            )
        assert res.status_code == 401

    def test_restore_cuenta_desactivada(self, client):
        row, _ = _make_user_row()
        row.get = lambda k, d=None: False if k == "is_active" else {"is_active": False}.get(k, d)
        token = _make_token("archivero")
        with (
            patch("routes.auth.db_query", return_value=[row]),
            patch("routes.auth.log_event"),
        ):
            res = client.post(
                "/api/auth/restore",
                json={"username": "archivero"},
                cookies={"ds_session": token},
            )
        assert res.status_code == 403
