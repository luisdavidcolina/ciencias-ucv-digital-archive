"""Helper de lectura de hojas CSS para tests — carril LX-3 (SD-213).

`styles.css` puede ser sólo el punto de entrada de un `@import` hacia
`app/static/styles/*.css` (la partición real de SD-213). Los tests que
antes hacían `STYLES.read_text(...)` asumiendo un único archivo siguen
razonando sobre la misma cadena de texto: esta función expande cada
`@import url("...")` local sustituyéndolo por el contenido real del
archivo importado, en el mismo orden en que aparece, de forma recursiva.
Los `@import` externos (http/https, p. ej. de un CDN) se dejan intactos.

Es un cambio de fontanería, no de comportamiento: el resto de cada test
sigue viendo la misma hoja de estilos ensamblada de siempre.
"""
import re
from pathlib import Path

APP = Path(__file__).resolve().parents[1]
STATIC = APP / "static"
STYLES = STATIC / "styles.css"

_IMPORT_RE = re.compile(r'@import\s+url\(\s*["\']([^"\']+)["\']\s*\)[^;]*;')


def leer_css_ensamblado(ruta: Path = STYLES) -> str:
    """Lee `ruta` y expande sus `@import` locales, recursivamente."""
    texto = ruta.read_text(encoding="utf-8")

    def _expandir(match: re.Match) -> str:
        destino = match.group(1)
        if destino.startswith("http://") or destino.startswith("https://"):
            return match.group(0)
        return leer_css_ensamblado((ruta.parent / destino).resolve())

    return _IMPORT_RE.sub(_expandir, texto)
