"""
Contraste de color — WCAG 2.1 AA.

Motivación: una auditoría con axe-core sobre el despliegue encontró 42 nodos por
debajo del mínimo de 4,5:1. Varios estaban a un pelo — el gris de Bootstrap da
4,45:1 sobre el fondo de las tarjetas —, que es justo el tipo de fallo que nadie
detecta a ojo.

Esta comprobación no necesita navegador: calcula la razón de contraste sobre los
colores declarados en el código. No sustituye a axe (no sabe qué se pinta sobre
qué), pero fija los valores que ya costó calcular una vez.
"""
import re
from pathlib import Path

import pytest

APP = Path(__file__).resolve().parents[1]
STATIC = APP / "static"

# Fondos reales sobre los que se pinta el texto en modo claro.
BLANCO = "#ffffff"
GRIS_TARJETA = "#f8f9fa"      # .card, barra de paginación, cajas de ayuda
GRIS_MIGAJA = "#e9ecef"       # .ds-breadcrumb

MINIMO = 4.5                  # AA para texto normal


def _luminancia(hex_color: str) -> float:
    h = hex_color.lstrip("#")
    if len(h) == 3:
        h = "".join(c * 2 for c in h)
    canales = [int(h[i:i + 2], 16) / 255 for i in (0, 2, 4)]
    lineal = [c / 12.92 if c <= 0.03928 else ((c + 0.055) / 1.055) ** 2.4 for c in canales]
    return 0.2126 * lineal[0] + 0.7152 * lineal[1] + 0.0722 * lineal[2]


def contraste(a: str, b: str) -> float:
    la, lb = _luminancia(a), _luminancia(b)
    return (max(la, lb) + 0.05) / (min(la, lb) + 0.05)


def test_el_calculo_es_correcto():
    """Ancla conocida: negro sobre blanco son 21:1."""
    assert round(contraste("#000000", "#ffffff"), 1) == 21.0
    assert round(contraste("#ffffff", "#ffffff"), 1) == 1.0


# --- colores de estado: llevan texto blanco encima --------------------------

def _colores_de_estado():
    js = (STATIC / "app-core.js").read_text(encoding="utf-8")
    cuerpo = js[js.index("function getStatusColor"):]
    cuerpo = cuerpo[: cuerpo.index("\n}")]
    return dict(re.findall(r'case "(\w+)":\s*return "(#[0-9a-fA-F]{6})"', cuerpo))


@pytest.mark.parametrize("estado,color", sorted(_colores_de_estado().items()))
def test_las_insignias_de_estado_se_leen(estado, color):
    """El verde de Bootstrap daba 3,13:1 con el blanco de su propio texto."""
    r = contraste(BLANCO, color)
    assert r >= MINIMO, (
        f'la insignia "{estado}" ({color}) da {r:.2f}:1 con texto blanco; '
        f"hace falta {MINIMO}"
    )


# --- grises de texto sobre los fondos reales --------------------------------

GRISES = [
    ("--ds-muted-aa", GRIS_TARJETA),
    ("--ds-muted-aa-oscuro", GRIS_MIGAJA),
]


@pytest.mark.parametrize("token,fondo", GRISES)
def test_los_grises_de_texto_cumplen(token, fondo):
    css = (STATIC / "styles.css").read_text(encoding="utf-8")
    m = re.search(re.escape(token) + r":\s*(#[0-9a-fA-F]{6})", css)
    assert m, f"el token {token} ya no está declarado en styles.css"
    color = m.group(1)
    r = contraste(color, fondo)
    assert r >= MINIMO, (
        f"{token} ({color}) da {r:.2f}:1 sobre {fondo}; hace falta {MINIMO}. "
        "Ojo con el fondo: el mismo gris cumple sobre blanco y falla sobre gris."
    )


def test_no_vuelve_el_gris_de_bootstrap_en_estilos_en_linea():
    """#6c757d sobre #f8f9fa da 4,44:1, y en un style en línea ninguna hoja de
    estilos puede corregirlo."""
    culpables = []
    for f in sorted(STATIC.glob("*.js")):
        for n, linea in enumerate(f.read_text(encoding="utf-8").splitlines(), 1):
            if re.search(r"color:\s*#6c757d", linea):
                culpables.append(f"{f.name}:{n}")
    assert not culpables, (
        "color #6c757d en un estilo en línea (4,44:1 sobre el fondo de las "
        f"tarjetas): {culpables}"
    )
