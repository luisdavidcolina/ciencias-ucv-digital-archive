"""Tests para búsqueda de archivo institucional."""
import pytest
from unittest.mock import patch, MagicMock


def _mock_row(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


def _doc_row(**kwargs):
    base = {
        "id_archivo": 1, "titulo": "Informe Anual 2023", "autor": "Decanato",
        "fecha_documento": "2024-01-10", "tesauro_primario": "Informe",
        "tesauro_secundario": "Parte I", "ubicacion": "Estante A",
        "abstract": "Resumen.", "file_url": "", "personas_relacionadas": "",
        "numero_folio": None, "soporte": "Físico", "numero_paginas": None,
        "descriptores_libres": "gestión",
        "relevance": 1.0, "total_count": 1,
    }
    base.update(kwargs)
    return _mock_row(**base)


def _search_mock(doc_rows):
    """Mock para db_query: devuelve docs en la query principal, vacío en las facets."""
    call_count = [0]

    def _side(sql, params=None, fetch="all", commit=False):
        call_count[0] += 1
        return doc_rows if call_count[0] == 1 else []

    return _side


class TestArchivoSearch:
    def test_busqueda_vacia_retorna_resultados(self, client):
        doc = _doc_row()

        with patch("routes.archive.db_query", side_effect=_search_mock([doc])):
            res = client.post("/api/archivo/buscar", json={
                "search_term": "", "doc_types": [], "tesauro_terms": [],
                "descriptors": [], "date_start": "", "date_end": "",
                "page": 1, "per_page": 20,
            })
        assert res.status_code == 200
        body = res.json()
        assert "records" in body
        assert body["total"] == 1
        assert body["records"][0]["titulo"] == "Informe Anual 2023"

    def test_busqueda_por_texto(self, client):
        doc = _doc_row(titulo="Informe Anual")

        with patch("routes.archive.db_query", side_effect=_search_mock([doc])):
            res = client.post("/api/archivo/buscar", json={
                "search_term": "informe", "doc_types": [], "tesauro_terms": [],
                "descriptors": [], "date_start": "", "date_end": "",
                "page": 1, "per_page": 20,
            })
        assert res.status_code == 200
        body = res.json()
        assert body["total"] == 1
        assert body["records"][0]["titulo"] == "Informe Anual"

    def test_filtro_por_doc_type(self, client):
        doc = _doc_row(tesauro_primario="Plano", titulo="Plano B")

        with patch("routes.archive.db_query", side_effect=_search_mock([doc])):
            res = client.post("/api/archivo/buscar", json={
                "search_term": "", "doc_types": ["Plano"], "tesauro_terms": [],
                "descriptors": [], "date_start": "", "date_end": "",
                "page": 1, "per_page": 20,
            })
        assert res.status_code == 200
        body = res.json()
        assert body["total"] == 1

    def test_sin_datos_retorna_lista_vacia(self, client):
        with patch("routes.archive.db_query", side_effect=_search_mock([])):
            res = client.post("/api/archivo/buscar", json={
                "search_term": "", "doc_types": [], "tesauro_terms": [],
                "descriptors": [], "date_start": "", "date_end": "",
                "page": 1, "per_page": 20,
            })
        assert res.status_code == 200
        body = res.json()
        assert body["total"] == 0
        assert body["records"] == []
