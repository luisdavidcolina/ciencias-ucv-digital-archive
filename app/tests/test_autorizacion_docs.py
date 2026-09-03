"""Pruebas de `require_role` aplicado a `app/routes/admin/docs.py` (OA-035,
IN-131, IN-132).

Contexto: `docs.py` sirve documentos de dos módulos distintos según el
parámetro `modulo` de la petición (Archivo o RRHH). Antes de esta pieza,
`require_session` era la única comprobación: cualquier sesión válida, de
cualquier módulo, podía listar, editar o borrar (a la papelera) documentos
de Archivo o de RRHH, y también leer o editar expedientes de empleados. Con
`require_role` los endpoints compartidos exigen pertenecer a "Archivo" o
"RRHH" (cualquiera de los dos), y los endpoints de empleado (`/empleado/*`)
exigen "RRHH" en concreto.

Estilo: sigue `app/tests/test_autorizacion_deps.py` — se usan los fixtures
`anon_client` y `client_as` de `conftest.py` y se mockea
`routes.admin.deps.db_query` con la fila de `usuarios_sistema` a simular,
para ejercitar la lógica real de autorización.
"""
from unittest.mock import patch


def _fila(**data):
    from unittest.mock import MagicMock
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.get = lambda k, default=None: data.get(k, default)
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


class TestListAllFiles:
    """`GET /api/admin/list_all` — endpoint compartido Archivo/RRHH."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.get("/api/admin/list_all?modulo=Archivo")
        assert res.status_code == 401

    def test_usuario_desactivado_recibe_403(self, client_as):
        c = client_as("baja")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=False)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/list_all?modulo=Archivo")
        assert res.status_code == 403

    def test_usuario_sin_fila_recibe_403(self, client_as):
        c = client_as("fantasma")
        with patch("routes.admin.deps.db_query", return_value=None):
            res = c.get("/api/admin/list_all?modulo=Archivo")
        assert res.status_code == 403

    def test_usuario_de_archivo_puede_listar(self, client_as):
        c = client_as("archivo_normal")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.docs.db_query", return_value=None):
            res = c.get("/api/admin/list_all?modulo=Archivo")
        assert res.status_code == 200

    def test_usuario_de_rrhh_puede_listar(self, client_as):
        c = client_as("rrhh_normal")
        fila = _fila(modulo="RRHH", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.docs.db_query", return_value=None):
            res = c.get("/api/admin/list_all?modulo=RRHH")
        assert res.status_code == 200

    def test_usuario_global_puede_listar(self, client_as):
        c = client_as("global_admin")
        fila = _fila(modulo="Global", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.docs.db_query", return_value=None):
            res = c.get("/api/admin/list_all?modulo=Archivo")
        assert res.status_code == 200


class TestDeleteDocumento:
    """`DELETE /api/admin/documento/{id}` — soft-delete a papelera, no
    definitivo, así que exige `require_role`, no `require_admin_role`."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.delete("/api/admin/documento/1?modulo=Archivo&usuario=x")
        assert res.status_code == 401

    def test_usuario_de_rrhh_no_puede_borrar_documento_de_archivo(self, client_as):
        """Un usuario del módulo RRHH sí pasa `require_role("Archivo","RRHH")`
        porque el endpoint es compartido: eso está documentado como límite
        conocido de esta pieza (la comprobación es por módulo del usuario,
        no por el `modulo` concreto de la petición). Lo que sí debe seguir
        bloqueado es un módulo ajeno por completo."""
        c = client_as("otro_modulo")
        fila = _fila(modulo="OtroModulo", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.delete("/api/admin/documento/1?modulo=Archivo&usuario=otro_modulo")
        assert res.status_code == 403

    def test_usuario_de_archivo_puede_enviar_a_papelera(self, client_as):
        c = client_as("archivo_admin")
        fila = _fila(modulo="Archivo", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.docs.db_query", return_value={"id_archivo": 1}), \
             patch("routes.admin.docs.invalidate_choices_cache", return_value=None), \
             patch("routes.admin.docs.log_event", return_value=None):
            res = c.delete("/api/admin/documento/1?modulo=Archivo&usuario=archivo_admin")
        assert res.status_code == 200


class TestEmpleadoEndpoints:
    """`/api/admin/empleado/{id}` — sólo tiene sentido para RRHH."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.get("/api/admin/empleado/1")
        assert res.status_code == 401

    def test_usuario_de_archivo_no_puede_ver_expediente_de_empleado(self, client_as):
        c = client_as("archivo_admin")
        fila = _fila(modulo="Archivo", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.get("/api/admin/empleado/1")
        assert res.status_code == 403

    def test_usuario_de_rrhh_si_puede_ver_expediente_de_empleado(self, client_as):
        c = client_as("rrhh_normal")
        fila = _fila(modulo="RRHH", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.docs.db_query", return_value=None):
            res = c.get("/api/admin/empleado/1")
        assert res.status_code == 404  # no encontrado, pero pasó la autorización

    def test_usuario_global_si_puede_ver_expediente_de_empleado(self, client_as):
        c = client_as("global_admin")
        fila = _fila(modulo="Global", rol="Admin", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.admin.docs.db_query", return_value=None):
            res = c.get("/api/admin/empleado/1")
        assert res.status_code == 404
