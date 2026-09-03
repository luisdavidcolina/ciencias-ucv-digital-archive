"""Pruebas de autorización de `app/routes/admin/catalog.py` (C2-catalogo).

Contexto: el catálogo de tipos documentales y palabras clave lo comparten
Archivo y RRHH (pestaña "Tipos" en ambos paneles admin, OA-123.../OR-163...).
Antes de este cambio, ningún endpoint de `catalog.py` comprobaba módulo ni
rol más allá de `require_session` (sesión válida cualquiera). Estas pruebas
ejercitan `require_role`/`require_admin_role` reales contra el router montado,
mockeando `routes.admin.deps.db_query` con la fila de `usuarios_sistema` que
se quiera simular — igual que `test_autorizacion_deps.py`.
"""
from unittest.mock import MagicMock, patch


def _fila(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.get = lambda k, default=None: data.get(k, default)
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


class TestRequireRoleEnKeywords:
    """`/api/admin/keywords` (GET) exige `require_role("Archivo", "RRHH")`."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.get("/api/admin/keywords")
        assert res.status_code == 401

    def test_usuario_de_modulo_global_no_relacionado_recibe_403(self, client_as):
        """Un usuario cuyo módulo no es Archivo, RRHH ni Global no debe pasar."""
        c = client_as("usuario_otro")
        fila = _fila(modulo="Otro", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/keywords")
        assert res.status_code == 403

    def test_usuario_normal_de_archivo_puede_leer(self, client_as):
        c = client_as("archivo_normal")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.catalog.db_query", return_value=[]):
            res = c.get("/api/admin/keywords")
        assert res.status_code == 200

    def test_usuario_normal_de_rrhh_puede_leer(self, client_as):
        """El catálogo es compartido: RRHH también debe poder consultarlo."""
        c = client_as("rrhh_normal")
        fila = _fila(modulo="RRHH", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.catalog.db_query", return_value=[]):
            res = c.get("/api/admin/keywords")
        assert res.status_code == 200


class TestRequireAdminRoleEnAddCategory:
    """`/api/admin/add_category` (POST) exige `require_admin_role("Archivo", "RRHH")`
    porque crear un tipo documental se propaga a todos los desplegables y
    documentos (OA-123)."""

    def _payload(self):
        return {"name": "Constancia de Trabajo", "desc": "", "scope": "Archivo",
                 "usuario": "archivo_normal", "parte": ""}

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.post("/api/admin/add_category", json=self._payload())
        assert res.status_code == 401

    def test_usuario_normal_de_archivo_no_puede_crear_tipo(self, client_as):
        """Ser Normal no basta: crear tipos exige rol Admin."""
        c = client_as("archivo_normal")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.post("/api/admin/add_category", json=self._payload())
        assert res.status_code == 403

    def test_usuario_admin_de_modulo_no_relacionado_recibe_403(self, client_as):
        c = client_as("otro_admin")
        fila = _fila(modulo="Otro", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.post("/api/admin/add_category", json=self._payload())
        assert res.status_code == 403

    def test_usuario_admin_de_archivo_si_puede_crear_tipo(self, client_as):
        c = client_as("archivo_admin")
        fila = _fila(modulo="Archivo", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.catalog.log_event", return_value=None), \
             patch("routes.admin.catalog.db_query", side_effect=[
                 {"id": 1},                # SELECT categoria por slug
                 None,                     # SELECT tipo_documento existente
                 None,                     # INSERT tipo_documento
             ]), \
             patch("routes.admin.catalog.generate_unique_slug", return_value="constancia-de-trabajo"), \
             patch("routes.admin.catalog.invalidate_choices_cache", return_value=None):
            res = c.post("/api/admin/add_category", json=self._payload())
        assert res.status_code == 200

    def test_usuario_admin_global_si_puede_crear_tipo(self, client_as):
        c = client_as("admin_global")
        fila = _fila(modulo="Global", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.catalog.log_event", return_value=None), \
             patch("routes.admin.catalog.db_query", side_effect=[
                 {"id": 1},
                 None,
                 None,
             ]), \
             patch("routes.admin.catalog.generate_unique_slug", return_value="constancia-de-trabajo"), \
             patch("routes.admin.catalog.invalidate_choices_cache", return_value=None):
            res = c.post("/api/admin/add_category", json=self._payload())
        assert res.status_code == 200


class TestRequireAdminRoleEnAuditLog:
    """`/api/admin/audit_log` (GET) exige `require_admin_role("Archivo", "RRHH")`:
    expone eventos de ambos módulos, incluido detalle con datos de personal
    (OA-152, OR-191)."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.get("/api/admin/audit_log")
        assert res.status_code == 401

    def test_usuario_normal_recibe_403(self, client_as):
        c = client_as("archivo_normal")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/audit_log")
        assert res.status_code == 403

    def test_usuario_desactivado_recibe_403(self, client_as):
        c = client_as("archivo_admin_baja")
        fila = _fila(modulo="Archivo", rol="Admin", is_active=False)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/audit_log")
        assert res.status_code == 403

    def test_usuario_admin_de_rrhh_si_puede_leer(self, client_as):
        c = client_as("rrhh_admin")
        fila = _fila(modulo="RRHH", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.catalog.db_query", side_effect=[{"total": 0}, []]):
            res = c.get("/api/admin/audit_log")
        assert res.status_code == 200


class TestRequireRoleEnNotifications:
    """`/api/admin/notifications` (GET) exige `require_role("Archivo", "RRHH")`."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.get("/api/admin/notifications")
        assert res.status_code == 401

    def test_usuario_de_modulo_no_relacionado_recibe_403(self, client_as):
        c = client_as("usuario_otro")
        fila = _fila(modulo="Otro", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/notifications")
        assert res.status_code == 403

    def test_usuario_normal_de_rrhh_puede_leer(self, client_as):
        c = client_as("rrhh_normal")
        fila = _fila(modulo="RRHH", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.catalog.db_query", return_value=[]):
            res = c.get("/api/admin/notifications")
        assert res.status_code == 200
