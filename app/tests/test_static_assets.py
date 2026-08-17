"""
Guardas sobre los archivos estáticos.

Motivación: el repositorio acumuló cuatro commits `fix(encoding)` y aun así llegó
a `main` un `app-theme.js` con comillas tipográficas usadas como delimitador de
string — un SyntaxError duro que dejaba muerto todo el panel de personalización
sin que ninguna prueba lo notara, porque las pruebas sólo cubren el backend.

Estas comprobaciones son baratas y cierran esa clase de fallo: si un editor
vuelve a "embellecer" comillas o un archivo se guarda con la codificación
equivocada, la suite falla antes del despliegue en vez de romper producción.
"""
import re
import subprocess
from pathlib import Path

import pytest

STATIC = Path(__file__).resolve().parents[1] / "static"

JS_FILES = sorted(STATIC.glob("*.js"))
TEXT_FILES = sorted(
    p for p in STATIC.rglob("*") if p.suffix in {".js", ".html", ".css"}
)

# Comillas tipográficas. Son legítimas dentro de texto visible, pero nunca deben
# aparecer donde JavaScript espera un delimitador de string.
CURLY = "“”"

# Restos de UTF-8 que en algún momento se guardó interpretándolo como cp1252.
MOJIBAKE = re.compile("[ÂÃâ][-¿]")

# El reemplazo de Unicode: señal de que el archivo se decodificó mal en origen.
REPLACEMENT = "�"


def _node_available():
    try:
        subprocess.run(["node", "--version"], capture_output=True, timeout=10)
        return True
    except (OSError, subprocess.SubprocessError):
        return False


def test_hay_archivos_estaticos():
    assert JS_FILES, "no se encontró ningún .js en app/static"


@pytest.mark.parametrize("path", TEXT_FILES, ids=lambda p: p.name)
def test_es_utf8_valido(path):
    """Cada estático debe decodificar como UTF-8 sin pérdida."""
    raw = path.read_bytes()
    try:
        text = raw.decode("utf-8")
    except UnicodeDecodeError as exc:
        pytest.fail(f"{path.name} no es UTF-8 válido: {exc}")
    assert REPLACEMENT not in text, (
        f"{path.name} contiene el carácter de reemplazo U+FFFD: "
        "el archivo se decodificó mal en algún punto de su historia"
    )


@pytest.mark.parametrize("path", TEXT_FILES, ids=lambda p: p.name)
def test_sin_mojibake(path):
    """Detecta secuencias de doble codificación (ej. 'Ã³' donde debería ir 'ó')."""
    text = path.read_text(encoding="utf-8")
    hits = MOJIBAKE.findall(text)
    assert not hits, (
        f"{path.name} tiene {len(hits)} secuencia(s) de texto doble-codificado, "
        f"por ejemplo {hits[0]!r}. Se repara re-codificando a cp1252 y "
        "decodificando como UTF-8."
    )


@pytest.mark.parametrize("path", JS_FILES, ids=lambda p: p.name)
def test_sin_comillas_tipograficas_en_codigo(path):
    """Una comilla tipográfica como delimitador de string rompe el archivo entero."""
    malas = []
    for numero, linea in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        if not any(c in linea for c in CURLY):
            continue
        # Sólo molesta cuando la comilla está pegada a sintaxis de código:
        # tras '(' ',' '=' ':' o justo antes de ')' ',' ';'.
        if re.search(r"[(,=:]\s*[" + CURLY + r"]", linea) or re.search(
            r"[" + CURLY + r"]\s*[),;]", linea
        ):
            malas.append((numero, linea.strip()[:80]))
    assert not malas, (
        f"{path.name} usa comillas tipográficas como delimitador de string en "
        f"{len(malas)} línea(s): {malas[:3]}"
    )


@pytest.mark.skipif(not _node_available(), reason="node no está disponible")
@pytest.mark.parametrize("path", JS_FILES, ids=lambda p: p.name)
def test_javascript_parsea(path):
    """`node --check`: la red de seguridad definitiva contra un JS que no carga."""
    res = subprocess.run(
        ["node", "--check", str(path)], capture_output=True, text=True, timeout=30
    )
    assert res.returncode == 0, (
        f"{path.name} no parsea como JavaScript:\n{res.stderr.strip()}"
    )
