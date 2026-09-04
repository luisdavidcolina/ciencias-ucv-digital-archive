"""Guarda de cabeceras de seguridad declaradas en `vercel.json` (SI-235).

Pedida en `_BUZON.md` por agente-h4-despliegue (H4-despliegue): confirma que
la ruta `/(.*)` (todo el sitio) y `/api/(.*)` llevan las cabeceras de
seguridad que ese carril fue añadiendo (IN-153/IN-154/SI-014), y que no
desaparecen en un cambio futuro de `vercel.json` sin que la suite se entere.
No se levanta ningún servidor: se lee el fichero de configuración tal cual
lo sirve Vercel.
"""
import json
from pathlib import Path

import pytest

VERCEL_JSON = Path(__file__).resolve().parent.parent.parent / "vercel.json"


@pytest.fixture(scope="module")
def config():
    with open(VERCEL_JSON, encoding="utf-8") as f:
        return json.load(f)


def _headers_for(config, source):
    for entry in config.get("headers", []):
        if entry.get("source") == source:
            return {h["key"]: h["value"] for h in entry.get("headers", [])}
    return None


class TestCabecerasGlobales:
    """`/(.*)` cubre toda página servida, estática o dinámica."""

    def test_existe_bloque_para_todo_el_sitio(self, config):
        assert _headers_for(config, "/(.*)") is not None

    def test_x_frame_options_sameorigin(self, config):
        headers = _headers_for(config, "/(.*)")
        assert headers["X-Frame-Options"] == "SAMEORIGIN"

    def test_referrer_policy_restringida(self, config):
        headers = _headers_for(config, "/(.*)")
        assert headers["Referrer-Policy"] == "strict-origin-when-cross-origin"

    def test_hsts_incluye_subdominios_y_preload(self, config):
        headers = _headers_for(config, "/(.*)")
        valor = headers["Strict-Transport-Security"]
        assert "includeSubDomains" in valor
        assert "preload" in valor

    def test_permissions_policy_restringe_camara_microfono_geolocalizacion(self, config):
        headers = _headers_for(config, "/(.*)")
        valor = headers["Permissions-Policy"]
        assert "camera=()" in valor
        assert "microphone=()" in valor
        assert "geolocation=()" in valor

    def test_no_hay_x_xss_protection_retirada(self, config):
        """IN-154: X-XSS-Protection se retiró de las cabeceras de /api/; no
        debe reaparecer aquí tampoco (cabecera obsoleta, sin efecto en
        navegadores modernos y con historial de introducir vulnerabilidades
        en algunos)."""
        headers = _headers_for(config, "/(.*)")
        assert "X-XSS-Protection" not in headers


class TestCabecerasApi:
    """`/api/(.*)` son las respuestas JSON del backend."""

    def test_existe_bloque_para_api(self, config):
        assert _headers_for(config, "/api/(.*)") is not None

    def test_x_content_type_options_nosniff(self, config):
        headers = _headers_for(config, "/api/(.*)")
        assert headers["X-Content-Type-Options"] == "nosniff"

    def test_x_frame_options_sameorigin(self, config):
        headers = _headers_for(config, "/api/(.*)")
        assert headers["X-Frame-Options"] == "SAMEORIGIN"

    def test_referrer_policy_restringida(self, config):
        headers = _headers_for(config, "/api/(.*)")
        assert headers["Referrer-Policy"] == "strict-origin-when-cross-origin"

    def test_permissions_policy_restringe_camara_microfono_geolocalizacion(self, config):
        headers = _headers_for(config, "/api/(.*)")
        valor = headers["Permissions-Policy"]
        assert "camera=()" in valor
        assert "microphone=()" in valor
        assert "geolocation=()" in valor

    def test_no_hay_x_xss_protection_retirada(self, config):
        headers = _headers_for(config, "/api/(.*)")
        assert "X-XSS-Protection" not in headers
