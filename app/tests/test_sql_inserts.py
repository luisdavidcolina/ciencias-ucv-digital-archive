"""
Integridad de los INSERT: columnas contra valores, y columnas obligatorias.

Motivación: la importación CSV nunca funcionó. Escribía el nombre de usuario en
`updated_by`, que es INTEGER, y además omitía `creado_por`, que es NOT NULL sin
valor por defecto. Cada fila moría, pero el bucle captura la excepción por fila,
así que el endpoint devolvía HTTP 200 con `inserted: 0` y la pantalla mostraba
"Importación completada" sin haber importado nada.

No basta con contar `%s`: hay INSERT que mezclan marcadores con literales
(`NOW()`, `'export'`). Lo que debe cuadrar es columnas contra VALORES.
"""
import re
from pathlib import Path

APP = Path(__file__).resolve().parents[1]
FUENTES = [p for p in APP.rglob("*.py") if "tests" not in p.parts]

# Se localiza el comienzo y luego se recorre contando paréntesis: una regex no
# greedy corta en el primer ")", que en `NOW()` está anidado y trunca la lista.
INSERT_INICIO = re.compile(r"INSERT INTO (?:public\.)?(\w+)\s*\(", re.I)

# Columnas NOT NULL sin valor por defecto: omitirlas revienta la fila entera.
OBLIGATORIAS = {
    "datos_archivo": {"creado_por"},
    "datos_rrhh": {"creado_por"},
}


def _bloque(src, apertura):
    """Contenido del paréntesis balanceado que empieza en `apertura`."""
    prof, i, comilla = 0, apertura, None
    while i < len(src):
        ch = src[i]
        if comilla:
            if ch == comilla:
                comilla = None
        elif ch in ("'", '"'):
            comilla = ch
        elif ch == "(":
            prof += 1
        elif ch == ")":
            prof -= 1
            if prof == 0:
                return src[apertura + 1:i], i + 1
        i += 1
    return "", len(src)


def _valores(txt):
    """Separa la lista de VALUES por comas de primer nivel, respetando comillas."""
    partes, prof, actual, comilla = [], 0, "", None
    for ch in txt:
        if comilla:
            actual += ch
            if ch == comilla:
                comilla = None
            continue
        if ch in ("'", '"'):
            comilla = ch
            actual += ch
            continue
        if ch == "(":
            prof += 1
        elif ch == ")":
            prof -= 1
        if ch == "," and prof == 0:
            partes.append(actual.strip())
            actual = ""
        else:
            actual += ch
    if actual.strip():
        partes.append(actual.strip())
    return [p for p in partes if p]


def _inserts():
    for f in sorted(FUENTES):
        src = f.read_text(encoding="utf-8")
        for m in INSERT_INICIO.finditer(src):
            cuerpo, fin = _bloque(src, m.end() - 1)
            cuerpo = cuerpo.replace("\n", " ")
            if re.search(r"\bSELECT\b", cuerpo, re.I):
                continue                       # INSERT ... SELECT
            mv = re.match(r"\s*VALUES\s*\(", src[fin:fin + 40], re.I)
            if not mv:
                continue                       # sin lista de VALUES literal
            crudo, _ = _bloque(src, fin + mv.end() - 1)
            cols = [c.strip() for c in cuerpo.split(",") if c.strip()]
            valores = _valores(crudo.replace("\n", " "))
            linea = src[: m.start()].count("\n") + 1
            yield f, linea, m.group(1).lower(), cols, valores


def test_se_detectaron_inserts():
    """Si el patrón deja de casar, las pruebas de abajo pasarían en vacío."""
    assert len(list(_inserts())) >= 5


def test_columnas_y_valores_cuadran():
    malas = [
        f"{f.relative_to(APP)}:{n}  {t}: {len(c)} columnas vs {len(v)} valores"
        for f, n, t, c, v in _inserts()
        if v and len(c) != len(v)
    ]
    assert not malas, "INSERT descuadrados:\n" + "\n".join(malas)


def test_no_se_omiten_columnas_obligatorias():
    faltan = []
    for f, n, tabla, cols, valores in _inserts():
        req = OBLIGATORIAS.get(tabla)
        if not req or not valores:
            continue
        ausentes = req - {c.lower() for c in cols}
        if ausentes:
            faltan.append(f"{f.relative_to(APP)}:{n}  {tabla} sin {sorted(ausentes)}")
    assert not faltan, (
        "INSERT que omiten una columna NOT NULL sin valor por defecto:\n"
        + "\n".join(faltan)
    )
