"""
Cada HTML servible debe tener ruta, y cada ruta su HTML.

Motivación: `index.html` —el archivo más grande del proyecto, 1328 líneas— no
estaba enrutado en ninguna parte. Sólo se alcanzaba en `/static/index.html`, que
nadie enlaza. Llevaba tiempo divergiendo del resto: su menú tenía otras
etiquetas y le faltaban secciones, y `app.js` arrastraba ramas para tres ids que
sólo existían ahí. El propio README ya lo marcaba como «evaluar si mantener o
deprecar».
"""
import re
from pathlib import Path

APP = Path(__file__).resolve().parents[1]
STATIC = APP / "static"
PAGES_PY = APP / "routes" / "pages.py"

# HTML que no se sirven por ruta a propósito (fragmentos, plantillas parciales).
# Si añades uno, justifícalo aquí.
SIN_RUTA_A_PROPOSITO: set[str] = set()


def _servidos():
    txt = PAGES_PY.read_text(encoding="utf-8")
    return set(re.findall(r'_page\(["\']([\w.]+\.html)["\']\)', txt))


def test_no_hay_paginas_huerfanas():
    en_disco = {p.name for p in STATIC.glob("*.html")}
    huerfanas = sorted(en_disco - _servidos() - SIN_RUTA_A_PROPOSITO)
    assert not huerfanas, (
        "HTML sin ninguna ruta que los sirva (código muerto que se queda "
        f"divergiendo en silencio): {huerfanas}. Si es intencional, añádelo a "
        "SIN_RUTA_A_PROPOSITO con el motivo."
    )


def test_toda_ruta_apunta_a_un_html_existente():
    faltan = sorted(n for n in _servidos() if not (STATIC / n).exists())
    assert not faltan, f"rutas que sirven un HTML inexistente: {faltan}"


def test_se_detectaron_rutas():
    """Si el patrón deja de casar, las dos pruebas de arriba pasarían en vacío."""
    assert len(_servidos()) >= 8, f"sólo se detectaron {len(_servidos())} rutas"
