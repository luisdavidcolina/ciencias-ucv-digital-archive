"""Tests para /api/choices (estructura del payload y caché)."""
import pytest
from unittest.mock import patch
import pandas as pd

from tests.conftest import _archivo_row, _rrhh_view_row


def _empty_rrhh_df():
    return pd.DataFrame(columns=[
        "cedula", "empleado", "personas_relacionadas", "departamento", "estado",
        "doc_type", "categoria", "fecha_ingreso", "ubicacion", "foto_url",
        "fecha_jubilacion", "fecha_pension", "rif", "cargo", "id_archivo",
        "descriptores_libres",
    ])


def _archivo_df():
    return pd.DataFrame([{
        "id": 1, "titulo": "Doc", "autor": "A", "fecha": "2024-01-01",
        "doc_type": "Informe", "categoria": "Parte I", "ubicacion": "Estante 1",
        "tesauro_primario": "Informe", "tesauro_secundario": "Parte I",
        "descriptores_libres": "gestión; académico", "resumen": "", "file_url": "",
    }])


class TestChoices:
    def test_estructura_correcta(self, client):
        with (
            patch("routes.archivo.fetch_archivo_dataframe", return_value=_archivo_df()),
            patch("routes.rrhh.fetch_rrhh_dataframe",      return_value=_empty_rrhh_df()),
        ):
            # Limpiar caché antes del test
            import routes.choices as ch
            ch._cache = {}

            res = client.get("/api/choices")

        assert res.status_code == 200
        body = res.json()
        assert "archivo" in body
        assert "rrhh"    in body
        assert "doc_types" in body["archivo"]
        assert "tesauro"   in body["archivo"]
        assert "min_date"  in body["archivo"]
        assert "max_date"  in body["archivo"]
        assert "doc_types" in body["rrhh"]
        assert "estados"   in body["rrhh"]
        assert "people"    in body["rrhh"]

    def test_cache_activo(self, client):
        """Segunda llamada devuelve el caché sin tocar la BD."""
        import routes.choices as ch
        ch._cache = {"archivo": {"doc_types": ["Cached"]}, "rrhh": {}}
        ch._cache_ts = 1e18  # Timestamp muy futuro → nunca expira

        res = client.get("/api/choices")
        assert res.status_code == 200
        assert res.json()["archivo"]["doc_types"] == ["Cached"]

        # Limpiar después del test
        ch._cache = {}
        ch._cache_ts = 0.0
