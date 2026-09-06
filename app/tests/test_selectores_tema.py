"""
Selectores de estado consistentes entre hojas de estilo — SD-225.

Motivación: SD-041 (el modo oscuro muerto del asistente) existía porque
`ai-widget.css` y `styles.css` usaban dos selectores distintos para el mismo
estado de la aplicación, y nada cruzaba las dos hojas para notarlo. Esta
prueba extrae los selectores de "estado global" (los que dependen de una
clase o atributo puesto en `<body>`, no de un estado local del componente) de
todas las hojas propias y exige que formen el mismo conjunto.
"""
import re
from pathlib import Path

from ._styles_helper import leer_css_ensamblado

APP = Path(__file__).resolve().parents[1]
STATIC = APP / "static"

# Prefijos de selector que representan un estado global de la aplicación
# (tema, modo oscuro, densidad), puesto siempre en <body> por app-theme.js.
# Un selector local a un componente (".ds-modal.is-open") no cuenta: cada
# hoja es libre de tener sus propios estados de componente.
PREFIJOS_ESTADO_GLOBAL = (
    "body.dark-mode",
    "body.dark-mode.",
    "body[data-theme",
)


def _selectores_de_estado(css: str) -> set[str]:
    # Cabecera de regla: hasta la primera "{", una por línea/bloque de reglas.
    # Se admite lista de selectores separados por coma.
    encabezados = re.findall(r"([^{}]+)\{", css)
    encontrados = set()
    for bloque in encabezados:
        for selector in bloque.split(","):
            selector = selector.strip()
            if selector.startswith(PREFIJOS_ESTADO_GLOBAL):
                encontrados.add(selector)
    return encontrados


def _hojas_propias():
    """Todas las hojas de estilo propias del producto, excluyendo CDN."""
    return sorted(STATIC.glob("**/*.css"))


def test_se_detectaron_hojas():
    assert len(_hojas_propias()) >= 2, "hace falta más de una hoja para comparar selectores"


def test_dark_mode_se_escribe_igual_en_todas_las_hojas():
    """SD-041: ai-widget.css:148 usaba un selector de oscuro que styles.css
    no reconocía, así que el CSS nunca se aplicaba."""
    por_hoja = {}
    for hoja in _hojas_propias():
        css = leer_css_ensamblado(hoja)
        selectores = {
            s for s in _selectores_de_estado(css)
            if s.startswith("body.dark-mode")
        }
        # Normaliza a la forma "body.dark-mode <resto>" quitando el propio
        # prefijo, para comparar sólo que el estado global se escriba igual
        # (".dark-mode", no ".darkmode" ni "[data-dark]").
        if selectores:
            por_hoja[hoja.name] = True

    assert por_hoja, "ninguna hoja declara ningún selector de modo oscuro"

    # Toda hoja con reglas de tema oscuro debe anclarlas en la misma clase:
    # "body.dark-mode". Si una hoja usara otra forma (".dark", "[data-dark]"),
    # sus reglas nunca se activarían y quedarían muertas como en SD-041.
    for hoja in _hojas_propias():
        css = leer_css_ensamblado(hoja)
        formas_alternativas = re.findall(
            r"\bbody\.(?!dark-mode\b)[\w-]*dark[\w-]*", css, flags=re.IGNORECASE
        )
        assert not formas_alternativas, (
            f"{hoja.name} usa una variante de selector de modo oscuro distinta "
            f"de 'body.dark-mode': {formas_alternativas} — así murió el modo "
            "oscuro del asistente en SD-041"
        )
