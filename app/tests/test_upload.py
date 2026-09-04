"""Pruebas de `POST /api/admin/upload` (`app/routes/files.py`).

Contexto (DG-169, pedido por C10-ficheros-r2 en `_BUZON.md`): cubre los
rechazos del endpoint (extensión no permitida, archivo vacío, tamaño por
encima de 25 MB, sin sesión) y confirma DG-083 — la identidad de quien sube
ya no se puede falsear desde el campo `usuario` del formulario, solo cuenta
`require_session` (cookie/token). `storage.is_configured()` se mockea a
`True` para no depender de credenciales reales de R2, y `storage.upload_fileobj`
se mockea para no llamar a boto3 de verdad.
"""
from unittest.mock import patch

import storage


def _upload(client, filename="documento.pdf", content=b"contenido", **extra):
    files = {"file": (filename, content, "application/pdf")}
    data = {"modulo": "archivo", **extra}
    return client.post("/api/admin/upload", files=files, data=data)


class TestSinSesion:
    def test_sin_sesion_recibe_401(self, anon_client):
        res = _upload(anon_client)
        assert res.status_code == 401


class TestValidaciones:
    def test_extension_no_permitida_retorna_400(self, client):
        with patch("storage.is_configured", return_value=True):
            res = _upload(client, filename="documento.exe")
        assert res.status_code == 400
        assert "no permitida" in res.json()["detail"].lower()

    def test_sin_extension_retorna_400(self, client):
        with patch("storage.is_configured", return_value=True):
            res = _upload(client, filename="documento")
        assert res.status_code == 400

    def test_archivo_vacio_retorna_400(self, client):
        with patch("storage.is_configured", return_value=True):
            res = _upload(client, content=b"")
        assert res.status_code == 400
        assert "vacío" in res.json()["detail"].lower() or "vacio" in res.json()["detail"].lower()

    def test_archivo_excede_tamano_maximo_retorna_413(self, client):
        contenido = b"x" * (storage.MAX_FILE_SIZE + 1)
        with patch("storage.is_configured", return_value=True):
            res = _upload(client, content=contenido)
        assert res.status_code == 413

    def test_almacenamiento_no_configurado_retorna_503(self, client):
        with patch("storage.is_configured", return_value=False):
            res = _upload(client)
        assert res.status_code == 503

    def test_fallo_al_subir_a_r2_retorna_502(self, client):
        with patch("storage.is_configured", return_value=True), \
             patch("storage.upload_fileobj", side_effect=RuntimeError("boom")), \
             patch("routes.files.log_event"):
            res = _upload(client)
        assert res.status_code == 502


class TestSubidaOk:
    def test_subida_valida_retorna_file_url(self, client):
        with patch("storage.is_configured", return_value=True), \
             patch("storage.upload_fileobj") as mock_upload, \
             patch("storage.build_object_key", return_value="archivo/2024/abcd1234-documento.pdf"), \
             patch("routes.files.log_event") as mock_log:
            res = _upload(client)
        assert res.status_code == 200
        body = res.json()
        assert body["success"] is True
        assert body["file_url"] == "/api/files/archivo/2024/abcd1234-documento.pdf"
        mock_upload.assert_called_once()
        mock_log.assert_called_once()


class TestIdentidadNoFalseable:
    """DG-083: el campo `usuario` del formulario ya no decide la identidad
    de quien sube; solo la sesión verificada por `require_session`."""

    def test_campo_usuario_del_formulario_se_ignora_para_auditoria(self, client_as):
        c = client_as("usuario_real_de_la_sesion")
        with patch("storage.is_configured", return_value=True), \
             patch("storage.upload_fileobj"), \
             patch("storage.build_object_key", return_value="archivo/2024/x-documento.pdf"), \
             patch("routes.files.log_event") as mock_log:
            res = _upload(c, usuario="alguien_suplantado")
        assert res.status_code == 200
        # log_event(usuario_sesion, ...): el primer argumento posicional debe
        # ser el usuario de la sesión real, nunca el campo del formulario.
        llamado_con = mock_log.call_args[0][0]
        assert llamado_con == "usuario_real_de_la_sesion"
        assert llamado_con != "alguien_suplantado"
