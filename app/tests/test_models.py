"""Tests para validadores de modelos Pydantic."""
import pytest
from pydantic import ValidationError

import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from models import (
    DocumentSubmitRequest, DocumentUpdateRequest,
    EmpleadoUpdateRequest, UserCreateRequest,
)


_BASE_SUBMIT = dict(modulo="Archivo", usuario="u", doc_type="Acta",
                    fecha="2024-01-01", ubicacion="A-1")
_BASE_UPDATE = dict(modulo="Archivo", id=1, usuario="u")


# =============================================================================
# file_url / foto_url scheme
# =============================================================================

class TestFileUrlValidator:
    def test_relative_url_accepted(self):
        r = DocumentUpdateRequest(**_BASE_UPDATE, file_url="/api/files/abc")
        assert r.file_url == "/api/files/abc"

    def test_https_url_accepted(self):
        r = DocumentUpdateRequest(**_BASE_UPDATE, file_url="https://cdn.example.com/doc.pdf")
        assert r.file_url == "https://cdn.example.com/doc.pdf"

    def test_none_accepted(self):
        r = DocumentUpdateRequest(**_BASE_UPDATE, file_url=None)
        assert r.file_url is None

    def test_empty_string_normalised_to_none(self):
        r = DocumentUpdateRequest(**_BASE_UPDATE, file_url="")
        assert r.file_url is None

    def test_javascript_rejected(self):
        with pytest.raises(ValidationError, match="file_url debe comenzar"):
            DocumentUpdateRequest(**_BASE_UPDATE, file_url="javascript:alert(1)")

    def test_data_uri_rejected(self):
        with pytest.raises(ValidationError, match="file_url debe comenzar"):
            DocumentUpdateRequest(**_BASE_UPDATE, file_url="data:text/html,<script>")

    def test_vbscript_rejected(self):
        with pytest.raises(ValidationError, match="file_url debe comenzar"):
            DocumentUpdateRequest(**_BASE_UPDATE, file_url="vbscript:msgbox(1)")


class TestFotoUrlValidator:
    def test_https_foto_accepted(self):
        r = DocumentSubmitRequest(**_BASE_SUBMIT, foto_url="https://bucket.s3.amazonaws.com/x.jpg")
        assert r.foto_url == "https://bucket.s3.amazonaws.com/x.jpg"

    def test_data_uri_foto_rejected(self):
        with pytest.raises(ValidationError, match="file_url debe comenzar"):
            DocumentSubmitRequest(**_BASE_SUBMIT, foto_url="data:image/png;base64,abc")

    def test_empleado_foto_javascript_rejected(self):
        with pytest.raises(ValidationError, match="file_url debe comenzar"):
            EmpleadoUpdateRequest(usuario="u", foto_url="javascript:void(0)")

    def test_empleado_foto_https_accepted(self):
        r = EmpleadoUpdateRequest(usuario="u", foto_url="https://cdn.example.com/foto.jpg")
        assert r.foto_url == "https://cdn.example.com/foto.jpg"


# =============================================================================
# Longitudes máximas
# =============================================================================

class TestLengthLimits:
    def test_titulo_max_500(self):
        with pytest.raises(ValidationError):
            DocumentSubmitRequest(**_BASE_SUBMIT, titulo="x" * 501)

    def test_titulo_500_exacto_ok(self):
        r = DocumentSubmitRequest(**_BASE_SUBMIT, titulo="x" * 500)
        assert len(r.titulo) == 500

    def test_resumen_max_4000(self):
        with pytest.raises(ValidationError):
            DocumentSubmitRequest(**_BASE_SUBMIT, resumen="x" * 4001)

    def test_cedula_max_20(self):
        with pytest.raises(ValidationError):
            DocumentSubmitRequest(**_BASE_SUBMIT, cedula="V-" + "1" * 20)

    def test_username_login_max_100(self):
        from models import LoginRequest
        with pytest.raises(ValidationError):
            LoginRequest(username="u" * 101, password="pass")

    def test_usuario_create_max_100(self):
        with pytest.raises(ValidationError):
            UserCreateRequest(usuario="u" * 101, password="clave123", modulo="Archivo", rol="Normal", creator="admin")


# =============================================================================
# Validación de soporte
# =============================================================================

class TestSoporteValidator:
    def test_fisico_accepted(self):
        r = DocumentSubmitRequest(**_BASE_SUBMIT, soporte="Físico")
        assert r.soporte == "Físico"

    def test_digital_accepted(self):
        r = DocumentSubmitRequest(**_BASE_SUBMIT, soporte="Digital")
        assert r.soporte == "Digital"

    def test_digitalizado_accepted(self):
        r = DocumentSubmitRequest(**_BASE_SUBMIT, soporte="Digitalizado")
        assert r.soporte == "Digitalizado"

    def test_invalido_rechazado(self):
        with pytest.raises(ValidationError, match="soporte debe ser"):
            DocumentSubmitRequest(**_BASE_SUBMIT, soporte="Holográfico")

    def test_none_defaults_to_fisico(self):
        r = DocumentSubmitRequest(**_BASE_SUBMIT, soporte=None)
        assert r.soporte == "Físico"


# =============================================================================
# Validación de fechas
# =============================================================================

_BASE_SUBMIT_NO_FECHA = dict(modulo="Archivo", usuario="u", doc_type="Acta", ubicacion="A-1")

class TestFechaValidator:
    def test_fecha_valida_ok(self):
        r = DocumentSubmitRequest(**_BASE_SUBMIT_NO_FECHA, fecha="2023-06-15")
        assert r.fecha == "2023-06-15"

    def test_fecha_dd_mm_yyyy_rechazada(self):
        with pytest.raises(ValidationError, match="YYYY-MM-DD"):
            DocumentSubmitRequest(**_BASE_SUBMIT_NO_FECHA, fecha="15/06/2023")

    def test_fecha_sin_guiones_rechazada(self):
        with pytest.raises(ValidationError, match="YYYY-MM-DD"):
            DocumentSubmitRequest(**_BASE_SUBMIT_NO_FECHA, fecha="20230615")

    def test_fecha_vencimiento_opcional_valida(self):
        r = DocumentSubmitRequest(**_BASE_SUBMIT, fecha_vencimiento="2030-12-31")
        assert r.fecha_vencimiento == "2030-12-31"

    def test_fecha_vencimiento_mal_formato(self):
        with pytest.raises(ValidationError, match="YYYY-MM-DD"):
            DocumentSubmitRequest(**_BASE_SUBMIT, fecha_vencimiento="31-12-2030")

    def test_fecha_none_ok_para_campos_opcionales(self):
        r = EmpleadoUpdateRequest(usuario="u", fecha_jubilacion=None)
        assert r.fecha_jubilacion is None

    def test_fecha_jubilacion_formato_valido(self):
        r = EmpleadoUpdateRequest(usuario="u", fecha_jubilacion="2030-01-15")
        assert r.fecha_jubilacion == "2030-01-15"

    def test_fecha_jubilacion_formato_invalido(self):
        with pytest.raises(ValidationError, match="YYYY-MM-DD"):
            EmpleadoUpdateRequest(usuario="u", fecha_jubilacion="15-01-2030")


# =============================================================================
# Validación de sexo y nivel_educativo
# =============================================================================

class TestEmpleadoValidators:
    def test_sexo_m_ok(self):
        r = EmpleadoUpdateRequest(usuario="u", sexo="m")
        assert r.sexo == "M"

    def test_sexo_invalido(self):
        with pytest.raises(ValidationError, match="sexo"):
            EmpleadoUpdateRequest(usuario="u", sexo="X")

    def test_nivel_educativo_valido(self):
        r = EmpleadoUpdateRequest(usuario="u", nivel_educativo="Maestría")
        assert r.nivel_educativo == "Maestría"

    def test_nivel_educativo_invalido(self):
        with pytest.raises(ValidationError, match="nivel_educativo inválido"):
            EmpleadoUpdateRequest(usuario="u", nivel_educativo="Primaria")
