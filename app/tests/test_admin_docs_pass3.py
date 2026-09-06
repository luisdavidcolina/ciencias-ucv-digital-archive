"""Tercer pase dedicado sobre `app/routes/admin/docs.py` (OA-004, OA-005,
OR-007, OR-031, OR-032, OR-100).

Estilo: sigue `test_autorizacion_docs.py` — mockea `routes.admin.deps.db_query`
para pasar la autorización y `routes.admin.docs.db_query` para simular la
respuesta de la base, sin abrir conexión real.
"""
from unittest.mock import MagicMock, patch


def _fila(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.get = lambda k, default=None: data.get(k, default)
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


_ARCHIVO_ADMIN = _fila(modulo="Archivo", rol="Admin", is_active=True)
_RRHH_ADMIN = _fila(modulo="RRHH", rol="Admin", is_active=True)


class TestStatusCountsExcluyePapelera:
    """OA-004: los badges no deben contar documentos en la papelera."""

    def test_archivo_filtra_deleted_at(self, client_as):
        c = client_as("archivo_admin")
        with patch("routes.admin.deps.db_query", return_value=_ARCHIVO_ADMIN), \
             patch("routes.admin.docs.db_query", return_value=[]) as mock_docs:
            res = c.get("/api/admin/status_counts?modulo=Archivo")
        assert res.status_code == 200
        sql = mock_docs.call_args[0][0]
        assert "deleted_at IS NULL" in sql

    def test_rrhh_filtra_deleted_at(self, client_as):
        c = client_as("rrhh_admin")
        with patch("routes.admin.deps.db_query", return_value=_RRHH_ADMIN), \
             patch("routes.admin.docs.db_query", return_value=[]) as mock_docs:
            res = c.get("/api/admin/status_counts?modulo=RRHH")
        assert res.status_code == 200
        sql = mock_docs.call_args[0][0]
        assert "deleted_at IS NULL" in sql


class TestUpdateStatusRespetaPapelera:
    """OA-005: no se puede cambiar el status de un documento en la papelera."""

    def test_documento_en_papelera_recibe_409(self, client_as):
        c = client_as("archivo_admin")
        with patch("routes.admin.deps.db_query", return_value=_ARCHIVO_ADMIN), \
             patch("routes.admin.docs.db_query", return_value={"deleted_at": "2026-01-01T00:00:00"}), \
             patch("routes.admin.docs.log_event", return_value=None):
            res = c.patch("/api/admin/documento/1/status?status=aprobado&modulo=Archivo")
        assert res.status_code == 409

    def test_documento_inexistente_recibe_404(self, client_as):
        c = client_as("archivo_admin")
        with patch("routes.admin.deps.db_query", return_value=_ARCHIVO_ADMIN), \
             patch("routes.admin.docs.db_query", return_value=None), \
             patch("routes.admin.docs.log_event", return_value=None):
            res = c.patch("/api/admin/documento/1/status?status=aprobado&modulo=Archivo")
        assert res.status_code == 404

    def test_documento_activo_se_actualiza(self, client_as):
        c = client_as("archivo_admin")
        respuestas = iter([{"deleted_at": None}, {"id_archivo": 1}])
        with patch("routes.admin.deps.db_query", return_value=_ARCHIVO_ADMIN), \
             patch("routes.admin.docs.db_query", side_effect=lambda *a, **k: next(respuestas)), \
             patch("routes.admin.docs.log_event", return_value=None):
            res = c.patch("/api/admin/documento/1/status?status=aprobado&modulo=Archivo")
        assert res.status_code == 200


class TestUpdateDocumentoRRHHSincronizaTipo:
    """OR-007: cambiar el tipo de un documento RRHH debe escribir
    `id_tipo_documento`, no sólo el texto de `tesauro_primario`."""

    def test_put_con_doc_type_resuelve_id_tipo_documento(self, client_as):
        c = client_as("rrhh_admin")
        with patch("routes.admin.deps.db_query", return_value=_RRHH_ADMIN), \
             patch("routes.admin.docs.db_query", return_value=None) as mock_docs, \
             patch("routes.admin.docs._resolve_or_create_tipo_documento", return_value=42) as mock_resolve, \
             patch("routes.admin.docs._resolve_user_id", return_value=1), \
             patch("routes.admin.docs.invalidate_choices_cache", return_value=None), \
             patch("routes.admin.docs.log_event", return_value=None):
            res = c.put(
                "/api/admin/documento/1",
                json={"modulo": "RRHH", "id": 1, "usuario": "rrhh_admin", "doc_type": "Constancia"},
            )
        assert res.status_code == 200
        mock_resolve.assert_called_once_with("Constancia")
        update_sql = mock_docs.call_args[0][0]
        assert "id_tipo_documento" in update_sql
        assert 42 in mock_docs.call_args[0][1]


class TestEmpleadoRespetaPapelera:
    """OR-031: no se puede ver ni editar un empleado que está en la papelera."""

    def test_get_empleado_en_papelera_recibe_404(self, client_as):
        c = client_as("rrhh_admin")
        with patch("routes.admin.deps.db_query", return_value=_RRHH_ADMIN), \
             patch("routes.admin.docs.db_query", return_value=None):
            res = c.get("/api/admin/empleado/1")
        assert res.status_code == 404

    def test_put_empleado_en_papelera_recibe_409(self, client_as):
        c = client_as("rrhh_admin")
        with patch("routes.admin.deps.db_query", return_value=_RRHH_ADMIN), \
             patch("routes.admin.docs.db_query", return_value={"deleted_at": "2026-01-01T00:00:00"}):
            res = c.put(
                "/api/admin/empleado/1",
                json={"usuario": "rrhh_admin", "nombres": "X"},
            )
        assert res.status_code == 409

    def test_put_empleado_activo_recibe_404_si_desaparece(self, client_as):
        c = client_as("rrhh_admin")
        with patch("routes.admin.deps.db_query", return_value=_RRHH_ADMIN), \
             patch("routes.admin.docs.db_query", return_value=None):
            res = c.put(
                "/api/admin/empleado/1",
                json={"usuario": "rrhh_admin", "nombres": "X"},
            )
        assert res.status_code == 404


class TestSubmitRRHHEmpleadoEnPapelera:
    """OR-032: no se debe colgar un documento nuevo de un expediente borrado."""

    def _payload(self):
        return {
            "modulo": "RRHH", "usuario": "rrhh_admin", "doc_type": "Constancia",
            "fecha": "2026-01-01", "ubicacion": "Archivo Central",
            "nombres": "Ana", "apellidos": "Perez", "cedula": "12345678",
            "departamento": "Decanato", "estado": "Activo",
        }

    def test_cedula_de_empleado_en_papelera_recibe_409(self, client_as):
        c = client_as("rrhh_admin")
        with patch("routes.admin.deps.db_query", return_value=_RRHH_ADMIN), \
             patch("routes.admin.docs.db_query", return_value={"id": 5, "deleted_at": "2026-01-01T00:00:00"}), \
             patch("routes.admin.docs.log_event", return_value=None), \
             patch("routes.admin.docs._resolve_user_id", return_value=1), \
             patch("routes.admin.docs._resolve_or_create_tipo_documento", return_value=7):
            res = c.post("/api/admin/submit", json=self._payload())
        assert res.status_code == 409


class TestSubmitRRHHUsaTituloPropio:
    """OR-100: si viene `titulo`, no se debe componer uno a partir de
    `personas_relacionadas`, que es texto libre pensado para metadata."""

    def test_titulo_explicito_se_respeta(self, client_as):
        c = client_as("rrhh_admin")
        payload = {
            "modulo": "RRHH", "usuario": "rrhh_admin", "doc_type": "Constancia",
            "fecha": "2026-01-01", "ubicacion": "Archivo Central",
            "nombres": "Ana", "apellidos": "Perez", "cedula": "12345678",
            "departamento": "Decanato", "estado": "Activo",
            "titulo": "Constancia de trabajo 2026",
            "personas_relacionadas": "Susana Pérez; Dirección RRHH",
        }
        respuestas = iter([
            None,  # búsqueda por cédula: no existe
            {"id": 9},  # INSERT empleados
            {"id_rrhh": 3},  # INSERT datos_rrhh
        ])
        with patch("routes.admin.deps.db_query", return_value=_RRHH_ADMIN), \
             patch("routes.admin.docs.db_query", side_effect=lambda *a, **k: next(respuestas)) as mock_docs, \
             patch("routes.admin.docs.log_event", return_value=None), \
             patch("routes.admin.docs._resolve_user_id", return_value=1), \
             patch("routes.admin.docs._resolve_or_create_tipo_documento", return_value=7), \
             patch("routes.admin.docs._resolve_or_create_lookup", return_value=1), \
             patch("routes.admin.docs.upsert_descriptors", return_value=None), \
             patch("routes.admin.docs.invalidate_choices_cache", return_value=None):
            res = c.post("/api/admin/submit", json=payload)
        assert res.status_code == 200
        insert_call = mock_docs.call_args_list[-1]
        assert insert_call[0][1][0] == "Constancia de trabajo 2026"
