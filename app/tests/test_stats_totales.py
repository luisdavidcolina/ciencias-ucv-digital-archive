"""
Normalización de las cifras del tablero.

Motivación: la respuesta convertía el diccionario entero con `int(v or 0)`. En
cuanto se añadió `ultimo_ingreso` —una fecha en texto— eso lanzó `ValueError` y
los dos endpoints de gráficos respondieron 500 en producción. Como era un error
de Python y no de base de datos, el mensaje era genérico y no decía nada.

La suite no lo vio porque mockea `db_query`: nadie comprobaba qué forma tiene la
fila que devuelve la consulta ni qué hace el código con ella.
"""
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from routes.admin.stats import _normalizar_totales  # noqa: E402


def test_convierte_numeros_a_entero():
    assert _normalizar_totales({"total_docs": "20", "total_types": 15}) == {
        "total_docs": 20, "total_types": 15,
    }


def test_respeta_las_fechas_en_texto():
    """El fallo original: int('2024-05-12') revienta."""
    r = _normalizar_totales({"total_docs": 20, "ultimo_ingreso": "2024-05-12"})
    assert r["ultimo_ingreso"] == "2024-05-12"
    assert r["total_docs"] == 20


def test_nulos_numericos_pasan_a_cero():
    assert _normalizar_totales({"total_docs": None})["total_docs"] == 0


def test_una_fecha_nula_se_queda_en_nulo():
    """Cero no es una fecha: un archivo vacío no tiene "último ingreso"."""
    assert _normalizar_totales({"ultimo_ingreso": None})["ultimo_ingreso"] is None


def test_fila_vacia():
    assert _normalizar_totales(None) == {}


@pytest.mark.parametrize("valor", ["", "N/A", "texto"])
def test_cualquier_texto_no_numerico_sobrevive(valor):
    assert _normalizar_totales({"campo": valor})["campo"] == valor
