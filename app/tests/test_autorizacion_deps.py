"""Pruebas de `require_role` / `require_admin_role` (IN-131, IN-132, SI-002).

Contexto: `require_session` sólo comprueba que exista una sesión legible; no
mira módulo ni rol. Diez auditorías documentan el mismo patrón: un usuario
Normal de Archivo puede exportar y restaurar la base entera vía
`/api/admin/backup/export` y `/api/admin/backup/restore` (SI-002), que hoy
llevan `Depends(require_role("Global"))` como ejemplo demostrativo de esta
pieza (ver `app/routes/backup.py`).

Estas pruebas usan los fixtures `anon_client` y `client_as` (SI-226,
`app/tests/conftest.py`) para ejercitar la dependencia contra ese endpoint
real, mockeando `routes.admin.deps.db_query` con la fila de
`usuarios_sistema` que se quiera simular — así se prueba la lógica real de
autorización, no un override que siempre da "true".
"""
from unittest.mock import MagicMock, patch


def _fila(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.get = lambda k, default=None: data.get(k, default)
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


class TestRequireRoleEnBackupExport:
    """`/api/admin/backup/export` lleva `Depends(require_role("Global"))`."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.get("/api/admin/backup/export")
        assert res.status_code == 401

    def test_usuario_normal_de_archivo_recibe_403(self, client_as):
        """El escenario documentado en SI-002: un usuario Normal de Archivo
        no debe poder exportar la base completa."""
        c = client_as("archivo_normal")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/backup/export")
        assert res.status_code == 403

    def test_usuario_admin_de_rrhh_tambien_recibe_403(self, client_as):
        """Ser Admin no basta: el módulo tiene que ser Global. Un Admin de
        RRHH sigue sin acceso al export completo."""
        c = client_as("rrhh_admin")
        fila = _fila(modulo="RRHH", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/backup/export")
        assert res.status_code == 403

    def test_usuario_desactivado_recibe_403_aunque_sea_global(self, client_as):
        c = client_as("admin_baja")
        fila = _fila(modulo="Global", rol="Admin", is_active=False)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/backup/export")
        assert res.status_code == 403

    def test_usuario_sin_fila_en_usuarios_sistema_recibe_403(self, client_as):
        c = client_as("usuario_fantasma")
        with patch("routes.admin.deps.db_query", return_value=None):
            res = c.get("/api/admin/backup/export")
        assert res.status_code == 403

    def test_usuario_global_si_puede_exportar(self, client_as):
        c = client_as("admin_global")
        fila = _fila(modulo="Global", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.backup.db_query", return_value=[]):
            res = c.get("/api/admin/backup/export")
        assert res.status_code == 200


class TestRequireRoleEnBackupRestore:
    """`/api/admin/backup/restore` lleva la misma dependencia."""

    def test_sin_sesion_recibe_401(self, anon_client):
        import io
        res = anon_client.post(
            "/api/admin/backup/restore?mode=merge",
            files={"file": ("x.json", io.BytesIO(b"{}"), "application/json")},
        )
        assert res.status_code == 401

    def test_usuario_normal_de_archivo_no_puede_restaurar(self, client_as):
        import io
        c = client_as("archivo_normal")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.post(
                "/api/admin/backup/restore?mode=merge",
                files={"file": ("x.json", io.BytesIO(b'{"_metadata": {}}'), "application/json")},
            )
        assert res.status_code == 403
