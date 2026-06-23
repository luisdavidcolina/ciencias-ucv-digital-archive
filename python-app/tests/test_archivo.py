"""Tests para búsqueda de archivo institucional."""
import pytest
from unittest.mock import patch
import pandas as pd


def _make_df(rows=None):
    default = [{
        "id": 1, "titulo": "Informe Anual 2023", "autor": "Decanato",
        "fecha": "2024-01-10", "doc_type": "Informe",
        "categoria": "Parte I", "ubicacion": "Digitalizado Exclusivo",
        "tesauro_primario": "Informe", "tesauro_secundario": "Parte I",
        "descriptores_libres": "gestión; rendición de cuentas",
        "resumen": "Informe anual de gestión.", "file_url": "",
    }]
    return pd.DataFrame(rows or default)


class TestArchivoSearch:
    def test_busqueda_vacia_retorna_resultados(self, client):
        with patch("routes.archivo.fetch_archivo_dataframe", return_value=_make_df()):
            res = client.post("/api/archivo/buscar", json={
                "search_term": "", "doc_types": [], "tesauro_terms": [],
                "date_start": "", "date_end": "", "sort_mode": "Alfabético (A-Z)"
            })
        assert res.status_code == 200
        body = res.json()
        assert isinstance(body, list)
        assert len(body) == 1
        assert body[0]["titulo"] == "Informe Anual 2023"
        assert "__idx" in body[0]
        assert "tesauro_badges" in body[0]

    def test_busqueda_por_texto(self, client):
        df = _make_df([
            {"id": 1, "titulo": "Informe Anual",   "autor": "A", "fecha": "2024-01-01",
             "doc_type": "Informe",  "categoria": "", "ubicacion": "X",
             "tesauro_primario": "Informe", "tesauro_secundario": "",
             "descriptores_libres": "", "resumen": "", "file_url": ""},
            {"id": 2, "titulo": "Plan Regulador",  "autor": "B", "fecha": "2023-06-01",
             "doc_type": "Plano",    "categoria": "", "ubicacion": "Y",
             "tesauro_primario": "Plano", "tesauro_secundario": "",
             "descriptores_libres": "", "resumen": "", "file_url": ""},
        ])
        with patch("routes.archivo.fetch_archivo_dataframe", return_value=df):
            res = client.post("/api/archivo/buscar", json={
                "search_term": "informe", "doc_types": [], "tesauro_terms": [],
                "date_start": "", "date_end": "", "sort_mode": "Alfabético (A-Z)"
            })
        body = res.json()
        assert len(body) == 1
        assert body[0]["titulo"] == "Informe Anual"

    def test_filtro_por_doc_type(self, client):
        df = _make_df([
            {"id": 1, "titulo": "Informe A", "autor": "X", "fecha": "2024-01-01",
             "doc_type": "Informe", "categoria": "", "ubicacion": "X",
             "tesauro_primario": "Informe", "tesauro_secundario": "",
             "descriptores_libres": "", "resumen": "", "file_url": ""},
            {"id": 2, "titulo": "Plano B",   "autor": "Y", "fecha": "2023-06-01",
             "doc_type": "Plano",   "categoria": "", "ubicacion": "Y",
             "tesauro_primario": "Plano", "tesauro_secundario": "",
             "descriptores_libres": "", "resumen": "", "file_url": ""},
        ])
        with patch("routes.archivo.fetch_archivo_dataframe", return_value=df):
            res = client.post("/api/archivo/buscar", json={
                "search_term": "", "doc_types": ["Plano"], "tesauro_terms": [],
                "date_start": "", "date_end": "", "sort_mode": "Alfabético (A-Z)"
            })
        body = res.json()
        assert len(body) == 1
        assert body[0]["doc_type"] == "Plano"

    def test_sin_datos_retorna_lista_vacia(self, client):
        with patch("routes.archivo.fetch_archivo_dataframe", return_value=pd.DataFrame(columns=[
            "id", "titulo", "autor", "fecha", "doc_type", "categoria", "ubicacion",
            "tesauro_primario", "tesauro_secundario", "descriptores_libres", "resumen", "file_url",
        ])):
            res = client.post("/api/archivo/buscar", json={
                "search_term": "", "doc_types": [], "tesauro_terms": [],
                "date_start": "", "date_end": "", "sort_mode": "Alfabético (A-Z)"
            })
        assert res.status_code == 200
        assert res.json() == []
