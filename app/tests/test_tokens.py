"""
Inventario del bloque de tokens (L0) — SD-022, SD-226.

Motivación: los 301 hexes que documenta `sistema-diseno.md` entraron uno a uno
sin ninguna resistencia, porque nada comprobaba que un color nuevo debía ser un
token. Esta suite no reescribe `styles.css` (eso es trabajo de los lotes L1-L15,
consumiendo el bloque que ya declaró L0); sólo evita que la deuda vuelva a
crecer y detecta las roturas típicas de un sistema de tokens: uno que se
consume sin estar definido, o uno de color claro sin su par en
`body.dark-mode`.
"""
import re
from pathlib import Path

import pytest

from ._styles_helper import leer_css_ensamblado

APP = Path(__file__).resolve().parents[1]
STATIC = APP / "static"
STYLES = STATIC / "styles.css"


def _css_texto():
    return leer_css_ensamblado()


def _bloque_tokens_l0(css: str) -> tuple[int, int]:
    """Rango (inicio, fin) del primer bloque `:root { ... }` del archivo.

    Es el bloque que añadió L0 (SD-001 a SD-030): el bloque de tokens vive al
    principio del archivo y no se toca en los lotes siguientes.
    """
    inicio = css.index(":root {")
    profundidad = 0
    for i in range(inicio, len(css)):
        if css[i] == "{":
            profundidad += 1
        elif css[i] == "}":
            profundidad -= 1
            if profundidad == 0:
                return inicio, i
    raise AssertionError("el bloque :root inicial de styles.css no cierra")


def _hexes_fuera_del_bloque(css: str) -> list[str]:
    inicio, fin = _bloque_tokens_l0(css)
    resto = css[:inicio] + css[fin + 1:]
    return re.findall(r"#[0-9a-fA-F]{3,8}\b", resto)


# Fijado el 2026-09-03 (SD-022) con margen: había 549 hexes fuera del bloque
# de tokens con el árbol de trabajo compartido en movimiento (hasta veinte
# agentes tokenizando reglas en paralelo bajan la cifra en cualquier commit).
# Esta guarda no exige bajarlo de golpe —eso son los lotes L1-L15—; sólo
# impide que vuelva a subir con holgura. Baja este número a mano si conviene
# apretar la guarda una vez que el árbol se estabilice.
MAXIMO_HEXES_FUERA_DE_TOKENS = 570


def test_no_crecen_los_hexes_sueltos():
    css = _css_texto()
    hexes = _hexes_fuera_del_bloque(css)
    assert len(hexes) <= MAXIMO_HEXES_FUERA_DE_TOKENS, (
        f"{len(hexes)} colores hex fuera del bloque de tokens (máximo "
        f"{MAXIMO_HEXES_FUERA_DE_TOKENS}). Usa un token existente del bloque "
        "`:root` al principio de styles.css, o añade uno nuevo ahí si de "
        "verdad falta (SD-021/SD-022)."
    )


def test_el_bloque_de_tokens_no_se_ha_movido():
    """Ancla: si esto falla, MAXIMO_HEXES_FUERA_DE_TOKENS quedó calculado
    sobre un bloque distinto y hay que recalcularlo, no simplemente subirlo."""
    css = _css_texto()
    inicio, fin = _bloque_tokens_l0(css)
    bloque = css[inicio:fin]
    assert "--gray-50" in bloque
    assert "--viz-1" not in bloque  # los tokens de componente no viven en L0


# --- SD-226: inventario de tokens -------------------------------------------

def _tokens_definidos(css: str) -> set[str]:
    return set(re.findall(r"(--[\w-]+)\s*:", css))


def _tokens_consumidos_sin_fallback(css: str) -> set[str]:
    """var(--x) sin segundo argumento: si --x no existe, no hay red de
    seguridad. `var(--x, fallback)` sí la tiene —así se resolvió SD-012 para
    --ds-link— y por eso queda fuera de esta comprobación."""
    return set(re.findall(r"var\(\s*(--[\w-]+)\s*\)", css))


def _tokens_consumidos_cualquiera(css: str) -> set[str]:
    encontrados = re.findall(r"var\(\s*(--[\w-]+)", css)
    # Descarta artefactos de template literals JS, var(--font-size-${x}):
    # la regex no incluye "$", así que corta justo antes dejando un "-" final.
    return {t for t in encontrados if not t.endswith("-")}


def _tokens_definidos_en_todo_el_frontend() -> set[str]:
    """styles.css más los bloques <style> propios de cada HTML: algunas
    páginas (investigacion.html) declaran su propio juego de tokens locales,
    autocontenido, y no pretenden vivir en el sistema global de styles.css."""
    definidos = _tokens_definidos(_css_texto())
    for f in STATIC.glob("*.html"):
        definidos |= _tokens_definidos(f.read_text(encoding="utf-8"))
    return definidos


def test_todo_token_consumido_esta_definido():
    definidos = _tokens_definidos_en_todo_el_frontend()
    consumidos = set()
    for f in STATIC.glob("**/*.css"):
        consumidos |= _tokens_consumidos_sin_fallback(f.read_text(encoding="utf-8"))
    for f in STATIC.glob("*.html"):
        consumidos |= _tokens_consumidos_sin_fallback(f.read_text(encoding="utf-8"))
    huerfanos = sorted(consumidos - definidos)
    assert not huerfanos, (
        f"tokens usados con var(--x) sin fallback pero nunca definidos: {huerfanos} "
        "(SD-012: --ds-panel-item-bg se consumía sin existir)"
    )


# SD-226 se escribe a mitad de la migración de tokens (L0 ya declaró el
# bloque; L1-L15 lo van consumiendo lote a lote, ver "Lotes de trabajo en
# paralelo" en sistema-diseno.md). Estos siguen sin ningún selector que los
# lea todavía porque su lote aún no ha corrido, no porque sobren. Quítalos de
# aquí cuando el lote que les toca los conecte; si uno se queda aquí mucho
# tiempo sin que ningún lote lo reclame, es candidato real a borrar.
TOKENS_RESERVADOS_SIN_CONSUMIR = {
    "--ds-accent-hover", "--ds-accent-light", "--ds-accent-on",  # L11/L14 (SD-048)
    "--ease-spring",                                              # L13 (SD-017)
    "--font-size-2xl", "--font-size-3xl", "--font-size-lg", "--font-size-xl",
    "--font-weight-normal",                                       # SD-016
    "--gray-800",                                                 # SD-018
    "--letter-spacing-base", "--letter-spacing-tight",            # SD-016
    "--space-7", "--space-8",                                     # SD-005
    "--z-base", "--z-modal", "--z-skip",                          # SD-009
}


def test_todo_token_definido_se_usa():
    css = _css_texto()
    definidos = _tokens_definidos(css)
    consumidos = _tokens_consumidos_cualquiera(css)
    for f in STATIC.glob("*.js"):
        texto = f.read_text(encoding="utf-8")
        consumidos |= _tokens_consumidos_cualquiera(texto)
        consumidos |= set(re.findall(r"getPropertyValue\(['\"](--[\w-]+)", texto))
        # Consumo dinámico por plantilla: getPropertyValue(`--viz-${i}`) en
        # viz-tokens.js no casa con var(--x) literal.
        for prefijo in re.findall(r"`(--[\w-]+)-\$\{", texto):
            consumidos |= {t for t in definidos if t.startswith(prefijo + "-")}
    sin_uso = sorted(definidos - consumidos - TOKENS_RESERVADOS_SIN_CONSUMIR)
    assert not sin_uso, (
        f"tokens declarados en styles.css que ningún selector ni script consume: "
        f"{sin_uso}. Si es intencional (p. ej. reservado para un lote futuro), "
        "añádelo a TOKENS_RESERVADOS_SIN_CONSUMIR con una nota, no lo dejes sin "
        "uso en silencio."
    )


# Prefijos de tokens de color que deben tener redefinición en modo oscuro.
# Los que no son de color (espaciado, tipografía, radios, duración, z-index)
# quedan fuera a propósito: no tienen "par oscuro".
PREFIJOS_COLOR = (
    "--surface-", "--text", "--border", "--ds-muted-aa", "--ds-accent",
    "--color-", "--mark-", "--viz-",
)


def _es_token_de_color(nombre: str) -> bool:
    return nombre.startswith(PREFIJOS_COLOR)


def test_todo_token_de_color_claro_tiene_par_en_modo_oscuro():
    css = _css_texto()
    inicio_dark = css.find("body.dark-mode")
    assert inicio_dark != -1, "no se encontró ningún bloque body.dark-mode en styles.css"

    claro = css[:inicio_dark]
    oscuro = css[inicio_dark:]

    definidos_claro = {
        t for t in _tokens_definidos(claro) if _es_token_de_color(t)
    }
    redefinidos_oscuro = _tokens_definidos(oscuro)

    sin_par = sorted(definidos_claro - redefinidos_oscuro)
    assert not sin_par, (
        f"tokens de color sin redefinición bajo body.dark-mode: {sin_par}. "
        "Si el valor es intencionalmente el mismo en ambos modos, redefínelo "
        "igual para que quede explícito (SD-226)."
    )
