"""Pruebas de autorización en `app/routes/admin/retention.py` (C4-retencion,
OA-035, OA-046).

`app/routes/admin/deps.py` (carril O1) ya trae `require_role` y
`require_admin_role`, demostradas contra `app/routes/backup.py`. Este archivo
comprueba que la pestaña "Retención" (compartida por Archivo y RRHH — el
catálogo de plazos es el mismo `tipo_documento` para ambos módulos, aunque
OA-001 documenta que hoy la vista de RRHH abre vacía por otro bug, no tocado
aquí) exige sesión + módulo, y que "disponer" un documento vencido — acción
irreversible en la práctica, exige acta — además exige rol Admin.

Mismo patrón que `test_autorizacion_deps.py`: `client_as`/`anon_client`
(SI-226, `conftest.py`) contra el router real, mockeando
`routes.admin.deps.db_query` con la fila de `usuarios_sistema` simulada.
"""
from unittest.mock import MagicMock, patch


def _fila(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.get = lambda k, default=None: data.get(k, default)
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


class TestRequireRoleEnListaTipos:
    """`GET /api/admin/retencion/tipos`: catálogo compartido, Archivo o RRHH."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.get("/api/admin/retencion/tipos")
        assert res.status_code == 401

    def test_usuario_sin_modulo_de_archivo_ni_rrhh_recibe_403(self, client_as):
        c = client_as("ia_normal")
        # Módulo que no es "Global" ni está en la lista permitida.
        fila = _fila(modulo="Otro", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/retencion/tipos")
        assert res.status_code == 403

    def test_usuario_de_rrhh_si_puede_listar_tipos(self, client_as):
        """El catálogo de plazos es compartido: RRHH también entra, aunque
        OA-001 documente que su vista de vencimientos hoy sale vacía."""
        c = client_as("rrhh_normal")
        fila = _fila(modulo="RRHH", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.retention.db_query", return_value=[]):
            res = c.get("/api/admin/retencion/tipos")
        assert res.status_code == 200


class TestRequireRoleEnVencimientos:
    """`GET /api/admin/retencion/vencimientos`: sólo opera sobre datos_archivo."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.get("/api/admin/retencion/vencimientos")
        assert res.status_code == 401

    def test_usuario_normal_de_rrhh_recibe_403(self, client_as):
        c = client_as("rrhh_normal2")
        fila = _fila(modulo="RRHH", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/retencion/vencimientos")
        assert res.status_code == 403

    def test_usuario_de_archivo_si_puede_ver_vencimientos(self, client_as):
        c = client_as("archivo_normal2")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.retention.db_query", return_value=[]):
            res = c.get("/api/admin/retencion/vencimientos")
        assert res.status_code == 200


class TestRequireRoleEnVencimientosRRHH:
    """`GET /api/admin/retencion/vencimientos-rrhh`: endpoint propio de RRHH
    que faltaba (OR-183/OR-184) — sólo opera sobre `datos_rrhh`, simétrico
    al de Archivo pero exigiendo el módulo RRHH en vez de Archivo."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.get("/api/admin/retencion/vencimientos-rrhh")
        assert res.status_code == 401

    def test_usuario_normal_de_archivo_recibe_403(self, client_as):
        c = client_as("archivo_normal4")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/retencion/vencimientos-rrhh")
        assert res.status_code == 403

    def test_usuario_de_rrhh_si_puede_ver_vencimientos_rrhh(self, client_as):
        c = client_as("rrhh_normal3")
        fila = _fila(modulo="RRHH", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.retention.db_query", return_value=[]):
            res = c.get("/api/admin/retencion/vencimientos-rrhh")
        assert res.status_code == 200


class TestRequireAdminRoleEnDisponer:
    """`POST /api/admin/retencion/disponer/{id}`: exige rol Admin en Archivo."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.post(
            "/api/admin/retencion/disponer/1", json={"disposicion": "conservar"}
        )
        assert res.status_code == 401

    def test_usuario_normal_de_archivo_no_puede_disponer(self, client_as):
        """Ser del módulo correcto no basta: disponer exige rol Admin."""
        c = client_as("archivo_normal3")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.post(
                "/api/admin/retencion/disponer/1", json={"disposicion": "conservar"}
            )
        assert res.status_code == 403

    def test_admin_de_rrhh_no_puede_disponer_documentos_de_archivo(self, client_as):
        c = client_as("rrhh_admin2")
        fila = _fila(modulo="RRHH", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.post(
                "/api/admin/retencion/disponer/1", json={"disposicion": "conservar"}
            )
        assert res.status_code == 403

    def test_admin_de_archivo_si_puede_disponer(self, client_as):
        c = client_as("archivo_admin")
        fila = _fila(modulo="Archivo", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.retention.db_query", return_value=None):
            res = c.post(
                "/api/admin/retencion/disponer/1", json={"disposicion": "conservar"}
            )
        # No existe el documento (db_query mockeado a None) -> 404, pero eso
        # ya es aguas abajo de la autorización: demuestra que pasó el 403.
        assert res.status_code == 404
