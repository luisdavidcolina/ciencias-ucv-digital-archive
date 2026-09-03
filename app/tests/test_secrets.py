"""
Guarda contra credenciales escritas en el código fuente.

Motivación (IN-002, SI-227): `app/storage.py` tuvo durante un tiempo las cuatro
credenciales de Cloudflare R2 como literales, bajo un comentario "TODO: mover a
variables de entorno" que nadie llegó a cumplir. `CLAUDE.md` afirma que este
archivo existe y verifica eso en cada corrida "desde el principio" — no era
cierto hasta ahora.

El test escanea el árbol de `app/` (código Python y JavaScript estático)
buscando formas obvias de secreto: claves de AWS/R2 (`AKIA...`), tokens de
OpenRouter (`sk-or-...`) o de otros proveedores con prefijo `sk-`, cadenas
hexadecimales o base64 largas, y cualquier asignación literal a una variable
cuyo nombre sugiera que es un secreto (KEY, SECRET, TOKEN, PASSWORD) que no
venga de `os.environ` / `process.env`.
"""
import re
from pathlib import Path

import pytest

APP = Path(__file__).resolve().parents[1]
_SELF = Path(__file__).resolve()

PY_FILES = sorted(APP.rglob("*.py"))
JS_FILES = sorted((APP / "static").glob("*.js"))

SOURCE_FILES = [
    p for p in PY_FILES + JS_FILES
    if p.resolve() != _SELF and "__pycache__" not in p.parts
]

# Patrones de forma de secreto, independientes del nombre de la variable.
SECRET_SHAPE_PATTERNS = [
    re.compile(r"AKIA[0-9A-Z]{16}"),            # Access Key ID estilo AWS/R2
    re.compile(r"sk-[A-Za-z0-9-]{20,}"),        # tokens estilo OpenAI/OpenRouter
    re.compile(r"\b[0-9a-f]{32,}\b"),           # hex largo: secretos, hashes de API
]

# Nombre de variable que sugiere secreto, asignada a una cadena literal.
# Excluye explícitamente cuando el valor viene de os.environ / process.env,
# de un placeholder ("...", "cambiar-en-produccion", "") o de una referencia a
# otra variable/función (sin comillas).
_SECRET_NAME = r"(?:[A-Za-z_][A-Za-z0-9_]*_)?(?:KEY|SECRET|TOKEN|PASSWORD)"
ASSIGNMENT_PATTERN = re.compile(
    r"\b(" + _SECRET_NAME + r")\s*[:=]\s*[\"']([^\"']+)[\"']"
)

# Nombres de variable que son la propia lectura del entorno o una constante de
# configuración pública, no un secreto (ej. "SECRET_KEY=cambiar-en-produccion"
# en .env.example, o el propio nombre de la variable de entorno como texto).
_PLACEHOLDER_VALUES = {
    "", "...", "cambiar-en-produccion", "cadena-larga-y-aleatoria",
}


def _is_placeholder(valor: str) -> bool:
    if valor.strip(".") == "" or valor in _PLACEHOLDER_VALUES:
        return True
    # Documentación tipo "R2_ACCESS_KEY=access-key-id-del-token-de-r2": todo
    # minúsculas-y-guiones sin dígitos ni mayúsculas es prosa, no un secreto.
    if re.fullmatch(r"[a-z][a-z-]*", valor):
        return True
    # Nombre de clave para localStorage/cookies (ej. "ds_scanner_url",
    # "archive_session"): snake_case en minúsculas, sin la entropía de un
    # secreto real. Un secreto de verdad mezcla mayúsculas, dígitos o símbolos.
    if re.fullmatch(r"[a-z][a-z0-9_]*", valor) and "_" in valor:
        return True
    return False


def _lineas_sospechosas(path: Path):
    hallazgos = []
    for numero, linea in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        stripped = linea.strip()
        if stripped.startswith("#") or stripped.startswith("//") or stripped.startswith("*"):
            continue

        for patron in SECRET_SHAPE_PATTERNS:
            m = patron.search(linea)
            if m:
                hallazgos.append((numero, f"forma de secreto {m.group(0)[:12]}…"))

        for m in ASSIGNMENT_PATTERN.finditer(linea):
            nombre, valor = m.group(1), m.group(2)
            if "environ" in linea or "process.env" in linea or "getenv" in linea:
                continue
            if _is_placeholder(valor):
                continue
            hallazgos.append((numero, f"{nombre} asignado a literal '{valor[:12]}…'"))

    return hallazgos


@pytest.mark.parametrize("path", SOURCE_FILES, ids=lambda p: str(p.relative_to(APP)))
def test_sin_credenciales_en_codigo(path):
    """Ninguna clave, secreto o token debe vivir como literal en el código."""
    hallazgos = _lineas_sospechosas(path)
    assert not hallazgos, (
        f"{path.relative_to(APP)} contiene posibles credenciales en claro:\n"
        + "\n".join(f"  línea {n}: {msg}" for n, msg in hallazgos)
    )


def test_hay_archivos_para_revisar():
    """Si esta lista queda vacía el test anterior pasa por no ejecutarse: guarda
    contra que un cambio de estructura silencie la comprobación."""
    assert SOURCE_FILES, "no se encontró ningún .py/.js bajo app/ para escanear"
