"""Tests para validadores de modelos Pydantic."""
import pytest
from pydantic import ValidationError

import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from models import DocumentSubmitRequest, DocumentUpdateRequest, EmpleadoUpdateRequest


_BASE_SUBMIT = dict(modulo="Archivo", usuario="u", doc_type="Acta",
                    fecha="2024-01-01", ubicacion="A-1")
_BASE_UPDATE = dict(modulo="Archivo", id=1, usuario="u")


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
