"""Pruebas de autorización de `app/routes/admin/users.py` (C6-usuarios-backend).

Contexto (OR-043/044/045, backoffice-rrhh.md): `GET /users?modulo=RRHH`
devolvía también los administradores `Global`, y las mutaciones (crear,
resetear contraseña, activar/desactivar, borrar) no comprobaban rol ni
módulo de quien pedía la operación — un admin de RRHH podía resetear la
clave del admin Global desde su propio panel y heredar el control del
sistema entero, incluidas las copias de seguridad y el módulo Archivo.

`app/routes/admin/users.py` ahora exige `require_admin_role("Global")` para
todo el router (mismo patrón que `app/routes/backup.py`, demostrado y
probado en `test_autorizacion_deps.py`): gestionar OTRO usuario del sistema
—sea cual sea su módulo— es una operación que sólo el administrador Global
debería poder hacer.

Estas pruebas usan `anon_client` y `client_as` (SI-226,
`app/tests/conftest.py`) y mockean `routes.admin.deps.db_query` con la fila
de `usuarios_sistema` que se quiera simular, para ejercitar la lógica real
de autorización.
"""
from unittest.mock import MagicMock, patch


def _fila(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.get = lambda k, default=None: data.get(k, default)
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


class TestListarUsuarios:
    """`GET /api/admin/users`."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.get("/api/admin/users")
        assert res.status_code == 401

    def test_usuario_normal_recibe_403(self, client_as):
        c = client_as("archivo_normal")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/users")
        assert res.status_code == 403

    def test_admin_de_rrhh_recibe_403(self, client_as):
        """OR-045: un admin de RRHH no debe ni siquiera listar usuarios de
        otros módulos (incluido el Global) desde este endpoint."""
        c = client_as("rrhh_admin")
        fila = _fila(modulo="RRHH", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/users")
        assert res.status_code == 403

    def test_admin_global_si_puede_listar(self, client_as):
        c = client_as("admin_global")
        fila = _fila(modulo="Global", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.users.db_query", return_value=[]):
            res = c.get("/api/admin/users")
        assert res.status_code == 200


class TestCrearUsuario:
    """`POST /api/admin/users/create`."""

    _payload = {
        "usuario": "nuevo", "password": "clave123segura",
        "modulo": "RRHH", "rol": "Normal", "creator": "quien_sea",
    }

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.post("/api/admin/users/create", json=self._payload)
        assert res.status_code == 401

    def test_admin_de_rrhh_no_puede_crear_usuarios(self, client_as):
        c = client_as("rrhh_admin")
        fila = _fila(modulo="RRHH", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.post("/api/admin/users/create", json=self._payload)
        assert res.status_code == 403

    def test_admin_global_si_puede_crear(self, client_as):
        c = client_as("admin_global")
        fila = _fila(modulo="Global", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.users.db_query", return_value=None), \
             patch("routes.admin.users.log_event", return_value=None), \
             patch("routes.admin.users.hash_password", return_value="hashed"):
            res = c.post("/api/admin/users/create", json=self._payload)
        assert res.status_code == 200


class TestResetearContrasena:
    """`PUT /api/admin/users/{uid}/password` — el caso central de OR-045:
    resetear la clave de OTRO usuario, sea cual sea su módulo."""

    _payload = {"new_password": "otra_clave123", "requester": "rrhh_admin"}

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.put("/api/admin/users/1/password", json=self._payload)
        assert res.status_code == 401

    def test_admin_de_rrhh_no_puede_resetear_la_clave_del_admin_global(self, client_as):
        """El escenario documentado en OR-045: un admin de RRHH intenta
        resetear la clave del admin Global desde su propio panel."""
        c = client_as("rrhh_admin")
        fila = _fila(modulo="RRHH", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.put("/api/admin/users/1/password", json=self._payload)
        assert res.status_code == 403

    def test_usuario_normal_de_archivo_tampoco_puede(self, client_as):
        c = client_as("archivo_normal")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.put("/api/admin/users/1/password", json=self._payload)
        assert res.status_code == 403

    def test_admin_global_si_puede_resetear(self, client_as):
        c = client_as("admin_global")
        fila = _fila(modulo="Global", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.users.db_query", return_value=None), \
             patch("routes.admin.users.log_event", return_value=None), \
             patch("routes.admin.users.hash_password", return_value="hashed"):
            res = c.put("/api/admin/users/1/password", json=self._payload)
        assert res.status_code == 200


class TestActivarDesactivarUsuario:
    """`PATCH /api/admin/users/{uid}/active`."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.patch("/api/admin/users/1/active")
        assert res.status_code == 401

    def test_admin_de_rrhh_no_puede_desactivar_al_admin_global(self, client_as):
        c = client_as("rrhh_admin")
        fila = _fila(modulo="RRHH", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.patch("/api/admin/users/1/active")
        assert res.status_code == 403

    def test_admin_global_si_puede(self, client_as):
        c = client_as("admin_global")
        fila = _fila(modulo="Global", rol="Admin", is_active=True)
        row = _fila(is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.users.db_query", return_value=row), \
             patch("routes.admin.users.log_event", return_value=None):
            res = c.patch("/api/admin/users/1/active")
        assert res.status_code == 200


class TestBorrarUsuario:
    """`DELETE /api/admin/users/{uid}`."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.delete("/api/admin/users/1")
        assert res.status_code == 401

    def test_admin_de_rrhh_no_puede_borrar_al_admin_global(self, client_as):
        c = client_as("rrhh_admin")
        fila = _fila(modulo="RRHH", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.delete("/api/admin/users/1")
        assert res.status_code == 403

    def test_usuario_desactivado_recibe_403_aunque_sea_global(self, client_as):
        c = client_as("admin_baja")
        fila = _fila(modulo="Global", rol="Admin", is_active=False)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.delete("/api/admin/users/1")
        assert res.status_code == 403

    def test_admin_global_si_puede_borrar(self, client_as):
        c = client_as("admin_global")
        fila = _fila(modulo="Global", rol="Admin", is_active=True)
        row = _fila(usuario="objetivo")
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.users.db_query", return_value=row), \
             patch("routes.admin.users.log_event", return_value=None):
            res = c.delete("/api/admin/users/1")
        assert res.status_code == 200
