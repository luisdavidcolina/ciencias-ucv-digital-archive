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
    # IN-058: split_terms se movió a utils.py; database.py la re-exporta para
    # no obligar a tocar archive.py/hr.py/lookups.py, que la importan como
    # `from database import split_terms`.
    ("database.py", "utils.split_terms"),
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


def test_sin_backslash_en_expresion_fstring():
    """R16 (2026-09-08, commit cf246ee): `generate_hr_report` (hr.py) tenía un
    f-string con comillas escapadas (`\\"color:#198754...\\"`) dentro de la
    propia expresión `{}` de reemplazo — válido en Python 3.12+ (PEP 701) pero
    `SyntaxError: f-string expression part cannot include a backslash` en
    Python <3.12. `.python-version` fija 3.11 (la versión real de Vercel), y
    esta máquina corre 3.12: `ast.parse` en 3.12 NO detecta el problema por sí
    solo (compila igual), así que hace falta inspeccionar el árbol y buscar el
    patrón directamente, en vez de depender sólo de que CI corra en 3.11 real
    (R18, commit 27cfb92) — R17 (ronda 2026-09-08) escribió este mismo chequeo
    ad-hoc para verificar el hallazgo de R16 pero no lo dejó como test
    permanente; se persiste aquí para que una regresión futura no dependa de
    que alguien repita el script a mano."""
    import ast

    fallos = []
    for py in sorted(APP.rglob("*.py")):
        if py.parent.name == "tests" or "__pycache__" in py.parts:
            continue
        source = py.read_text(encoding="utf-8")
        try:
            tree = ast.parse(source, filename=str(py))
        except SyntaxError:
            continue  # otro test (test_todos_los_modulos_importan) ya lo cubre
        for node in ast.walk(tree):
            if not isinstance(node, ast.JoinedStr):
                continue
            for value in node.values:
                if not isinstance(value, ast.FormattedValue):
                    continue
                segmento = ast.get_source_segment(source, value.value)
                if segmento and "\\" in segmento:
                    fallos.append(
                        f"{py.relative_to(REPO)}:{value.lineno}: "
                        f"backslash dentro de una expresión f-string: {segmento!r}"
                    )
    assert not fallos, (
        "backslash dentro de la expresión {} de un f-string — SyntaxError en "
        "Python <3.12 (PEP 701), rompe en producción (Vercel usa 3.11):\n"
        + "\n".join(fallos)
    )


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
