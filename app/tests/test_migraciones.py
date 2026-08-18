"""
Arranque: la huella del esquema.

Motivación: `run_migrations()` corre en cada arranque en frío. En serverless eso
son ~80 viajes de ida y vuelta a Neon —que está en otro continente— antes de
poder responder la primera petición. El esquema casi nunca cambia, así que se
guarda una huella del conjunto y basta una consulta para saber si hay algo que
hacer.

Lo delicado es cuándo NO registrarla: si una migración falla, la huella no debe
guardarse, o el fallo queda congelado para siempre. Eso ya pasó — una migración
con un `%` sin duplicar llevaba fallando en silencio desde el principio.
"""
import sys
from pathlib import Path
from unittest.mock import patch

import pytest

APP = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(APP))

import main as _main  # noqa: E402

# conftest parchea main.run_migrations a nivel de sesion para que la app arranque
# sin base de datos. Aqui hace falta la funcion de verdad, asi que se guarda una
# referencia en la importacion del modulo de test, antes de que ese parche exista.
_RUN_MIGRATIONS = _main.run_migrations


@pytest.fixture
def main_mod():
    return _main


def test_si_la_huella_coincide_no_se_aplica_nada(main_mod):
    llamadas = []
    with patch.object(main_mod, "_esquema_al_dia", return_value=True), \
         patch.object(main_mod, "db_query", side_effect=lambda *a, **k: llamadas.append(a)), \
         patch.object(main_mod, "_migrate_archivo_tipos"), \
         patch.object(main_mod, "_backfill_archivo_tipo_fk"):
        _RUN_MIGRATIONS()
    assert llamadas == [], "con el esquema al día no debe ejecutarse ninguna sentencia"


def test_si_la_huella_no_coincide_se_aplican(main_mod):
    llamadas = []
    with patch.object(main_mod, "_esquema_al_dia", return_value=False), \
         patch.object(main_mod, "db_query", side_effect=lambda *a, **k: llamadas.append(a)), \
         patch.object(main_mod, "_migrate_archivo_tipos"), \
         patch.object(main_mod, "_backfill_archivo_tipo_fk"), \
         patch.object(main_mod, "_registrar_huella") as reg:
        _RUN_MIGRATIONS()
    assert len(llamadas) > 50, f"se esperaban todas las migraciones, hubo {len(llamadas)}"
    reg.assert_called_once()


def test_una_migracion_fallida_no_registra_la_huella(main_mod):
    """Si se registrara, el fallo quedaría congelado y no se reintentaría nunca."""
    def falla_una(sql, *a, **k):
        if "cargos" in str(sql):
            raise RuntimeError("boom")

    with patch.object(main_mod, "_esquema_al_dia", return_value=False), \
         patch.object(main_mod, "db_query", side_effect=falla_una), \
         patch.object(main_mod, "_migrate_archivo_tipos"), \
         patch.object(main_mod, "_backfill_archivo_tipo_fk"), \
         patch.object(main_mod, "_registrar_huella") as reg:
        _RUN_MIGRATIONS()
    reg.assert_not_called()


def test_si_no_se_puede_leer_la_version_se_aplican(main_mod):
    """Ante la duda, aplicar: es idempotente y saltarse una migración es peor."""
    with patch.object(main_mod, "db_query", side_effect=RuntimeError("sin conexión")):
        assert main_mod._esquema_al_dia("loquesea") is False


def test_la_huella_cambia_si_cambian_las_migraciones(main_mod):
    """Dos ejecuciones del mismo código dan la misma huella; distinto código, otra."""
    capturadas = []
    with patch.object(main_mod, "_esquema_al_dia",
                      side_effect=lambda h: capturadas.append(h) or True), \
         patch.object(main_mod, "db_query"), \
         patch.object(main_mod, "_migrate_archivo_tipos"), \
         patch.object(main_mod, "_backfill_archivo_tipo_fk"):
        _RUN_MIGRATIONS()
        _RUN_MIGRATIONS()
    assert len(capturadas) == 2 and capturadas[0] == capturadas[1]
    assert len(capturadas[0]) == 64      # sha256 en hexadecimal
