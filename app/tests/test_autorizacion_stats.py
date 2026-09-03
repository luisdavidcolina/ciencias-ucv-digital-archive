"""Pruebas de autorización para `app/routes/admin/stats.py` (C3-stats-backend,
carril del abanico O1 — ver `app/routes/admin/deps.py`).

Antes de esto, `POST /api/admin/stats`, `GET /api/admin/charts` y
`GET /api/admin/global_summary` sólo exigían sesión válida (`require_session`,
aplicado a todo el router `/api/admin` en `app/routes/admin/__init__.py`): no
miraban módulo ni rol. Un usuario Normal de RRHH podía pedir
`POST /stats {"modulo": "Archivo"}` o `GET /charts?modulo=Archivo` y ver las
cifras del otro módulo, y cualquier usuario de un solo módulo podía leer
`/global_summary`, que agrega Archivo + RRHH + usuarios del sistema.

Estas pruebas siguen el patrón de `test_autorizacion_deps.py`: usan
`anon_client`/`client_as` (SI-226, `app/tests/conftest.py`) y mockean
`routes.admin.deps.db_query` con la fila de `usuarios_sistema` que se quiere
simular, para ejercitar la lógica real de `require_role`, no un override que
siempre da "true".
"""
from unittest.mock import MagicMock, patch


def _fila(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.get = lambda k, default=None: data.get(k, default)
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


class TestRequireRoleEnStats:
    """`POST /api/admin/stats` lleva `Depends(require_role("Archivo", "RRHH"))`."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.post("/api/admin/stats", json={"modulo": "Archivo"})
        assert res.status_code == 401

    def test_usuario_normal_de_archivo_puede_ver_stats_de_archivo(self, client_as):
        c = client_as("archivo_normal")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.stats.fetch_archive_dataframe") as mock_df:
            import pandas as pd
            mock_df.return_value = pd.DataFrame()
            res = c.post("/api/admin/stats", json={"modulo": "Archivo"})
        assert res.status_code == 200

    def test_usuario_desactivado_recibe_403(self, client_as):
        c = client_as("archivo_baja")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=False)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.post("/api/admin/stats", json={"modulo": "Archivo"})
        assert res.status_code == 403

    def test_usuario_sin_fila_en_usuarios_sistema_recibe_403(self, client_as):
        c = client_as("fantasma")
        with patch("routes.admin.deps.db_query", return_value=None):
            res = c.post("/api/admin/stats", json={"modulo": "RRHH"})
        assert res.status_code == 403


class TestRequireRoleEnCharts:
    """`GET /api/admin/charts` lleva la misma dependencia que `/stats`."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.get("/api/admin/charts?modulo=RRHH")
        assert res.status_code == 401

    def test_usuario_normal_de_rrhh_puede_ver_charts_de_rrhh(self, client_as):
        c = client_as("rrhh_normal")
        fila = _fila(modulo="RRHH", rol="Normal", is_active=True)
        vacio = MagicMock(fetch="all")
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.stats.db_query", return_value=[]):
            res = c.get("/api/admin/charts?modulo=RRHH")
        assert res.status_code == 200

    def test_usuario_global_puede_ver_charts_de_cualquier_modulo(self, client_as):
        c = client_as("admin_global")
        fila = _fila(modulo="Global", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.stats.db_query", return_value=[]):
            res = c.get("/api/admin/charts?modulo=Archivo")
        assert res.status_code == 200


class TestRequireRoleEnGlobalSummary:
    """`GET /api/admin/global_summary` lleva `Depends(require_role("Global"))`:
    agrega Archivo + RRHH + usuarios del sistema, así que ningún módulo suelto
    debe verlo, sólo "Global" (el comodín de `require_role`, ver `deps.py`)."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.get("/api/admin/global_summary")
        assert res.status_code == 401

    def test_usuario_admin_de_archivo_recibe_403(self, client_as):
        """Ser Admin no basta: hace falta módulo Global. Un Admin de Archivo
        sigue sin acceso al resumen agregado de todo el sistema."""
        c = client_as("archivo_admin")
        fila = _fila(modulo="Archivo", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/global_summary")
        assert res.status_code == 403

    def test_usuario_normal_de_rrhh_recibe_403(self, client_as):
        c = client_as("rrhh_normal")
        fila = _fila(modulo="RRHH", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/global_summary")
        assert res.status_code == 403

    def test_usuario_global_si_puede_ver_el_resumen(self, client_as):
        c = client_as("admin_global")
        fila = _fila(modulo="Global", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.stats.db_query", return_value=None):
            res = c.get("/api/admin/global_summary")
        assert res.status_code == 200
