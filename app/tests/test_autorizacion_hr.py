"""Pruebas de autorización del módulo RRHH (BR-001, BR-002).

Contexto: `app/routes/hr.py` declaraba `_auth = [Depends(require_session)]`
pero sólo lo aplicaba en `/empleado/{id}/documentos` y `/report/{id}` — los
tres endpoints que realmente alimentan la pantalla de búsqueda
(`POST /buscar`, `POST /person/profile`, `GET /empleado/por-cedula/{cedula}`)
se podían leer sin ninguna cookie de sesión (BR-001). Y ni `hr.py` ni
`hr_alerts.py` comprobaban módulo: cualquier usuario con sesión, aunque su
módulo fuera "Archivo", podía leer cualquier expediente de personal (BR-002).

Estas pruebas usan `anon_client` (sin sesión) y `client_as` (sesión de un
usuario concreto, con `routes.admin.deps.db_query` mockeado para simular su
fila de `usuarios_sistema`) — el mismo patrón de
`app/tests/test_autorizacion_deps.py`.
"""
from unittest.mock import MagicMock, patch


def _fila(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.get = lambda k, default=None: data.get(k, default)
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


ROL_RRHH_NORMAL = _fila(modulo="RRHH", rol="Normal", is_active=True)
ROL_ARCHIVO_NORMAL = _fila(modulo="Archivo", rol="Normal", is_active=True)


# =============================================================================
# BR-001 — sin sesión, 401, en los tres endpoints que la búsqueda usa
# =============================================================================

class TestSinSesionDevuelve401:

    def test_buscar_sin_sesion(self, anon_client):
        res = anon_client.post("/api/rrhh/buscar", json={"per_page": 50})
        assert res.status_code == 401

    def test_person_profile_sin_sesion(self, anon_client):
        res = anon_client.post("/api/rrhh/person/profile", json={"persona": "Juan Perez"})
        assert res.status_code == 401

    def test_empleado_por_cedula_sin_sesion(self, anon_client):
        res = anon_client.get("/api/rrhh/empleado/por-cedula/V-12345678")
        assert res.status_code == 401

    def test_documentos_sin_sesion(self, anon_client):
        res = anon_client.get("/api/rrhh/empleado/1/documentos")
        assert res.status_code == 401

    def test_report_sin_sesion(self, anon_client):
        res = anon_client.get("/api/rrhh/report/1")
        assert res.status_code == 401

    def test_alertas_jubilaciones_sin_sesion(self, anon_client):
        res = anon_client.get("/api/rrhh/alertas/jubilaciones")
        assert res.status_code == 401

    def test_historial_cargos_sin_sesion(self, anon_client):
        res = anon_client.get("/api/rrhh/empleado/1/historial_cargos")
        assert res.status_code == 401


# =============================================================================
# BR-002 — sesión de módulo Archivo no da acceso a RRHH
# =============================================================================

class TestModuloArchivoRecibe403EnRrhh:

    def test_buscar_con_modulo_archivo(self, client_as):
        c = client_as("archivo_normal")
        with patch("routes.admin.deps.db_query", return_value=ROL_ARCHIVO_NORMAL):
            res = c.post("/api/rrhh/buscar", json={"per_page": 50})
        assert res.status_code == 403

    def test_person_profile_con_modulo_archivo(self, client_as):
        c = client_as("archivo_normal")
        with patch("routes.admin.deps.db_query", return_value=ROL_ARCHIVO_NORMAL):
            res = c.post("/api/rrhh/person/profile", json={"persona": "Juan Perez"})
        assert res.status_code == 403

    def test_empleado_por_cedula_con_modulo_archivo(self, client_as):
        c = client_as("archivo_normal")
        with patch("routes.admin.deps.db_query", return_value=ROL_ARCHIVO_NORMAL):
            res = c.get("/api/rrhh/empleado/por-cedula/V-12345678")
        assert res.status_code == 403

    def test_alertas_jubilaciones_con_modulo_archivo(self, client_as):
        c = client_as("archivo_normal")
        with patch("routes.admin.deps.db_query", return_value=ROL_ARCHIVO_NORMAL):
            res = c.get("/api/rrhh/alertas/jubilaciones")
        assert res.status_code == 403

    def test_historial_cargos_con_modulo_archivo(self, client_as):
        c = client_as("archivo_normal")
        with patch("routes.admin.deps.db_query", return_value=ROL_ARCHIVO_NORMAL):
            res = c.get("/api/rrhh/empleado/1/historial_cargos")
        assert res.status_code == 403


# =============================================================================
# Un usuario de módulo RRHH sigue pudiendo usar el módulo (regresión positiva)
# =============================================================================

class TestModuloRrhhSiTieneAcceso:

    def test_buscar_con_modulo_rrhh(self, client_as):
        c = client_as("rrhh_normal")
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("routes.hr.db_query", return_value=[]),
        ):
            res = c.post("/api/rrhh/buscar", json={"per_page": 50})
        assert res.status_code == 200

    def test_empleado_por_cedula_con_modulo_rrhh_pero_sin_registro(self, client_as):
        c = client_as("rrhh_normal")
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("routes.hr.db_query", return_value=None),
        ):
            res = c.get("/api/rrhh/empleado/por-cedula/V-00000000")
        # Pasa la autorización (no 401/403); el 404 es del dato, no del acceso.
        assert res.status_code == 404
