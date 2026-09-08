"""Pruebas de `GET /api/files/{key}` (`app/routes/files.py`).

Contexto (C10-files-py-tercer-pase): la ruta ya prueba identidad con
`require_session` (DG-083) y módulo de la clave (OA-042), cubierto en
`test_upload.py` sólo para la subida. Aquí se cubre la descarga: acceso
cruzado de módulo, un documento en papelera (bug real encontrado en este
pase — la ruta no consultaba `datos_archivo`/`datos_rrhh` en absoluto, así
que un enlace viejo seguía sirviendo el archivo de un documento ya "borrado"
igual que corrigió `share.py` para los enlaces externos, IN-147) y que la
descarga queda en auditoría (OA-047, mitad de la ficha que vive en este
archivo).
"""
from unittest.mock import patch


def _fila_usuario(modulo, activo=True):
    return {"modulo": modulo, "is_active": activo}


class TestSinSesion:
    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.get("/api/files/archivo/2024/x-documento.pdf", follow_redirects=False)
        assert res.status_code == 401


class TestModulo:
    def test_usuario_rrhh_no_accede_a_clave_de_archivo(self, client_as):
        c = client_as("usuario_rrhh")
        with patch("storage.is_configured", return_value=True), \
             patch("repos.files_repo.db_query", return_value=_fila_usuario("RRHH")):
            res = c.get("/api/files/archivo/2024/x-documento.pdf", follow_redirects=False)
        assert res.status_code == 403

    def test_usuario_archivo_no_accede_a_clave_de_rrhh(self, client_as):
        c = client_as("usuario_archivo")
        with patch("storage.is_configured", return_value=True), \
             patch("repos.files_repo.db_query", return_value=_fila_usuario("Archivo")):
            res = c.get("/api/files/rrhh/2024/x-expediente.pdf", follow_redirects=False)
        assert res.status_code == 403

    def test_usuario_inactivo_no_accede(self, client_as):
        c = client_as("usuario_desactivado")
        with patch("storage.is_configured", return_value=True), \
             patch("repos.files_repo.db_query", return_value=_fila_usuario("Archivo", activo=False)):
            res = c.get("/api/files/archivo/2024/x-documento.pdf", follow_redirects=False)
        assert res.status_code == 403

    def test_global_accede_a_cualquier_modulo(self, client_as):
        c = client_as("usuario_global")

        def _db_query(sql, params=None, **kw):
            if "usuarios_sistema" in sql:
                return _fila_usuario("Global")
            return None  # sin fila en datos_archivo/datos_rrhh: no está en papelera

        with patch("storage.is_configured", return_value=True), \
             patch("repos.files_repo.db_query", side_effect=_db_query), \
             patch("storage.presigned_get_url", return_value="https://r2.example/firmado"), \
             patch("routes.files.log_event"):
            res = c.get("/api/files/rrhh/2024/x-expediente.pdf", follow_redirects=False)
        assert res.status_code == 307


class TestPapelera:
    """Bug real encontrado en este pase: la ruta nunca miraba `deleted_at`."""

    def test_documento_en_papelera_no_se_sirve(self, client_as):
        c = client_as("usuario_archivo")

        def _db_query(sql, params=None, **kw):
            if "usuarios_sistema" in sql:
                return _fila_usuario("Archivo")
            # _documento_en_papelera: el UNION ALL contra datos_archivo/datos_rrhh
            return {"x": 1}

        with patch("storage.is_configured", return_value=True), \
             patch("repos.files_repo.db_query", side_effect=_db_query), \
             patch("storage.presigned_get_url") as mock_presigned:
            res = c.get("/api/files/archivo/2024/x-documento.pdf", follow_redirects=False)
        assert res.status_code == 404
        mock_presigned.assert_not_called()

    def test_documento_no_en_papelera_si_se_sirve(self, client_as):
        c = client_as("usuario_archivo")

        def _db_query(sql, params=None, **kw):
            if "usuarios_sistema" in sql:
                return _fila_usuario("Archivo")
            return None

        with patch("storage.is_configured", return_value=True), \
             patch("repos.files_repo.db_query", side_effect=_db_query), \
             patch("storage.presigned_get_url", return_value="https://r2.example/firmado"), \
             patch("routes.files.log_event"):
            res = c.get("/api/files/archivo/2024/x-documento.pdf", follow_redirects=False)
        assert res.status_code == 307
        assert res.headers["location"] == "https://r2.example/firmado"


class TestAuditoria:
    """OA-047 (mitad de este archivo): cada descarga real queda en audit_log."""

    def test_descarga_exitosa_registra_evento(self, client_as):
        c = client_as("usuario_archivo")

        def _db_query(sql, params=None, **kw):
            if "usuarios_sistema" in sql:
                return _fila_usuario("Archivo")
            return None

        with patch("storage.is_configured", return_value=True), \
             patch("repos.files_repo.db_query", side_effect=_db_query), \
             patch("storage.presigned_get_url", return_value="https://r2.example/firmado"), \
             patch("routes.files.log_event") as mock_log:
            res = c.get("/api/files/archivo/2024/x-documento.pdf", follow_redirects=False)
        assert res.status_code == 307
        mock_log.assert_called_once()
        args = mock_log.call_args[0]
        assert args[0] == "usuario_archivo"
        assert args[1] == "Descargar Archivo"
        assert args[2] == "Archivo"

    def test_acceso_denegado_no_registra_evento(self, client_as):
        c = client_as("usuario_rrhh")
        with patch("storage.is_configured", return_value=True), \
             patch("repos.files_repo.db_query", return_value=_fila_usuario("RRHH")), \
             patch("routes.files.log_event") as mock_log:
            res = c.get("/api/files/archivo/2024/x-documento.pdf", follow_redirects=False)
        assert res.status_code == 403
        mock_log.assert_not_called()
