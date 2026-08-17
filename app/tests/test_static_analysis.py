"""
Análisis estático del backend.

Motivación: el refactor de renombrado a inglés dejó cinco llamadas a
`modelo_actual()` y `catalogo_modelos()` en `core/ai.py` después de haber
renombrado sus definiciones a `current_model()` y `list_models()`. Cada una era
un `NameError` en tiempo de ejecución — el chat, la validación de modelo y el
endpoint de disponibilidad fallaban con 500 — y la suite no lo notó, porque
ningún test importa ese módulo.

Un nombre indefinido nunca es intencional. Esta guarda lo trata como error.
"""
import subprocess
import sys
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
APP = REPO / "app"

# Nombres que el análisis marca como "importados y sin usar" pero que existen a
# propósito: otros módulos los importan desde aquí.
REEXPORTACIONES = {
    ("routes/admin/helpers.py", "utils.paginate"),
    ("routes/admin/helpers.py", "..lookups.invalidate_choices_cache"),
}


def _pyflakes():
    try:
        import pyflakes  # noqa: F401
    except ImportError:
        pytest.skip("pyflakes no está instalado")
    res = subprocess.run(
        [sys.executable, "-m", "pyflakes", str(APP), str(REPO / "api")],
        capture_output=True, text=True, timeout=120,
    )
    return [l for l in res.stdout.splitlines() if l.strip()]


def test_sin_nombres_indefinidos():
    """Un nombre indefinido es un NameError esperando a que alguien pase por ahí."""
    fallos = [l for l in _pyflakes() if "undefined name" in l]
    assert not fallos, "nombres indefinidos:\n" + "\n".join(fallos)


def test_sin_imports_muertos():
    """Los imports sin usar sobreviven a los refactors y despistan al siguiente."""
    fallos = []
    for linea in _pyflakes():
        if "imported but unused" not in linea:
            continue
        if "tests" in linea.replace("\\", "/"):
            continue           # los tests importan pytest por convención
        if any(f in linea.replace("\\", "/") and n in linea
               for f, n in REEXPORTACIONES):
            continue
        fallos.append(linea)
    assert not fallos, "imports sin usar:\n" + "\n".join(fallos)


def test_todos_los_modulos_importan():
    """Cada módulo debe poder importarse: un error de sintaxis o un import roto
    en una ruta poco transitada no debería descubrirse en producción."""
    import importlib

    sys.path.insert(0, str(APP))
    rotos = []
    for py in sorted(APP.rglob("*.py")):
        rel = py.relative_to(APP)
        if rel.parts[0] in {"tests", "__pycache__"}:
            continue
        modulo = ".".join(rel.with_suffix("").parts)
        try:
            importlib.import_module(modulo)
        except Exception as exc:                       # noqa: BLE001
            rotos.append(f"{modulo}: {type(exc).__name__}: {exc}")
    assert not rotos, "módulos que no importan:\n" + "\n".join(rotos)
