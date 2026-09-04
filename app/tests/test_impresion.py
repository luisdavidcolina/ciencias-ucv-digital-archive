"""
Impresión — SD-236.

Motivación (SD-229): la hoja de impresión original eran cinco líneas con
lista negra de selectores que no existían, y no ocultaba ni el velo del
cajón, ni el panel de temas, ni la burbuja del asistente — todo lo que es
`position: fixed`. Se reescribió como lista blanca (`* { position: static }`
dentro de `@media print`), que es justo lo que esta prueba exige que exista y
no se vuelva a perder.
"""
import re
from pathlib import Path

APP = Path(__file__).resolve().parents[1]
STATIC = APP / "static"
STYLES = STATIC / "styles.css"


def _bloques_print(css: str) -> list[str]:
    bloques = []
    for m in re.finditer(r"@media\s+print\s*\{", css):
        inicio = m.end() - 1
        profundidad = 0
        for i in range(inicio, len(css)):
            if css[i] == "{":
                profundidad += 1
            elif css[i] == "}":
                profundidad -= 1
                if profundidad == 0:
                    bloques.append(css[inicio + 1:i])
                    break
    return bloques


def test_existe_al_menos_un_bloque_de_impresion():
    css = STYLES.read_text(encoding="utf-8")
    assert _bloques_print(css), "no hay ningún @media print en styles.css"


def test_todo_position_fixed_tiene_contrapartida_en_impresion():
    """Lista blanca: basta con que algún bloque @media print neutralice
    `position: fixed` de forma universal (`* { position: static }`), o que
    cada selector que declara `position: fixed` fuera de un bloque de
    impresión aparezca también dentro de alguno."""
    css = STYLES.read_text(encoding="utf-8")
    bloques = _bloques_print(css)
    texto_print = "\n".join(bloques)

    reset_universal = re.search(
        r"\*\s*\{[^}]*position\s*:\s*(static|initial|unset)", texto_print
    )
    if reset_universal:
        return

    # Sin reset universal, cada selector fijo debe aparecer explícitamente.
    fuera_de_print = css
    for bloque in bloques:
        fuera_de_print = fuera_de_print.replace(bloque, "")

    encabezados = re.findall(r"([^{}]+)\{([^{}]*)\}", fuera_de_print)
    faltan = []
    for selector, cuerpo in encabezados:
        if re.search(r"position\s*:\s*fixed", cuerpo):
            nombre = selector.strip().split(",")[0].strip()
            clave = re.sub(r"^[.#]", "", nombre.split()[0]) if nombre else ""
            if clave and clave not in texto_print:
                faltan.append(nombre)

    assert not faltan, (
        f"selectores con position:fixed sin contrapartida en @media print: "
        f"{faltan} (SD-229: imprimir salía con la burbuja del asistente encima)"
    )
