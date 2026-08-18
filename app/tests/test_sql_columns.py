"""
Comprueba que el SQL solo referencie columnas que existen.

Motivación: cuatro de los cinco endpoints que respondían 500 en producción lo
hacían por esto — `da.id` cuando la columna es `id_archivo`, `dr.id_empleado`
cuando es `empleado_id`, `audit_log.created_at` cuando es `timestamp`. La suite
no los veía porque mockea `db_query`: ninguna consulta se ejecuta nunca.

El esquema se deriva del propio código (schema.sql más las migraciones de
main.py), así que la comprobación no necesita base de datos.
"""
import re
from pathlib import Path

import pytest

APP = Path(__file__).resolve().parents[1]

# --- esquema declarado en el código -----------------------------------------

CREATE = re.compile(
    r'CREATE TABLE (?:IF NOT EXISTS )?(?:public\.)?(\w+)\s*\((.*?)\n\s*\)',
    re.S | re.I)
ADD_COL = re.compile(
    r'ALTER TABLE (?:public\.)?(\w+)\s+ADD COLUMN (?:IF NOT EXISTS )?(\w+)', re.I)


def _columnas_del_bloque(cuerpo: str):
    cols = []
    for linea in cuerpo.split("\n"):
        linea = linea.strip().strip(",")
        if not linea or linea.startswith("--"):
            continue
        primera = linea.split()[0].strip('"').lower()
        if primera in {"primary", "foreign", "unique", "constraint", "check", "references"}:
            continue
        if re.fullmatch(r"\w+", primera):
            cols.append(primera)
    return cols


def esquema_declarado():
    tablas = {}
    fuentes = [APP / "schema.sql", APP / "main.py"]
    for f in fuentes:
        if not f.exists():
            continue
        txt = f.read_text(encoding="utf-8")
        for tabla, cuerpo in CREATE.findall(txt):
            tablas.setdefault(tabla.lower(), set()).update(_columnas_del_bloque(cuerpo))
        for tabla, col in ADD_COL.findall(txt):
            tablas.setdefault(tabla.lower(), set()).add(col.lower())
    return tablas


ESQUEMA = esquema_declarado()

# --- referencias en el SQL del código ---------------------------------------

TABLA_ALIAS = re.compile(r'\b(?:FROM|JOIN)\s+public\.(\w+)\s+(?:AS\s+)?(\w+)?', re.I)
REFERENCIA = re.compile(r'\b([a-z]\w{0,3})\.(\w+)\b')
RESERVADAS = {"where", "on", "left", "inner", "group", "order", "set", "as",
              "join", "using", "right", "full", "cross", "limit", "having",
              "union", "with", "and", "or"}
NO_TABLA = {"public", "information_schema", "pg_catalog"}

FUENTES = [p for p in APP.rglob("*.py") if "tests" not in p.parts]


def _referencias():
    for f in sorted(FUENTES):
        src = f.read_text(encoding="utf-8")
        for m in re.finditer(r'"""(.*?)"""', src, re.S):
            sql = m.group(1)
            if not re.search(r"\bSELECT\b|\bUPDATE\b|\bINSERT\b|\bDELETE\b", sql, re.I):
                continue
            linea = src[: m.start()].count("\n") + 1
            alias = {}
            for tabla, al in TABLA_ALIAS.findall(sql):
                t = tabla.lower()
                if al and al.lower() not in RESERVADAS:
                    alias[al] = t
                alias[tabla] = t
            for al, col in REFERENCIA.findall(sql):
                if al in NO_TABLA or al not in alias:
                    continue
                yield f, linea, alias[al], al, col.lower()


def test_el_esquema_se_pudo_derivar():
    """Si el extractor deja de encontrar tablas, la guarda pasaría en vacío."""
    assert len(ESQUEMA) >= 10, f"solo se derivaron {len(ESQUEMA)} tablas del código"
    assert "datos_archivo" in ESQUEMA and "empleados" in ESQUEMA


def test_las_consultas_solo_usan_columnas_existentes():
    malas = []
    for f, linea, tabla, al, col in _referencias():
        if tabla not in ESQUEMA:
            continue                      # tabla creada fuera del repo
        if col not in ESQUEMA[tabla]:
            malas.append(
                f"{f.relative_to(APP)}:{linea}  {al}.{col} — "
                f'"{tabla}" no tiene esa columna'
            )
    # Deduplicar: la misma consulta puede repetir la referencia.
    malas = sorted(set(malas))
    assert not malas, "referencias a columnas inexistentes:\n" + "\n".join(malas)


@pytest.mark.parametrize("tabla,col,existe", [
    ("datos_archivo", "id_archivo", True),
    ("datos_archivo", "id", False),        # el fallo real de notifications
    ("datos_rrhh", "empleado_id", True),
    ("datos_rrhh", "id_empleado", False),  # el otro fallo real
])
def test_el_esquema_derivado_es_fiable(tabla, col, existe):
    """Ancla el extractor a columnas concretas: si se rompe, esto avisa."""
    assert (col in ESQUEMA.get(tabla, set())) is existe


# ---------------------------------------------------------------------------
# El otro fallo de producción: % sin duplicar
# ---------------------------------------------------------------------------
# db_query hace `cur.execute(sql, params or ())`, así que psycopg2 recibe
# SIEMPRE una tupla e interpreta cualquier % como marcador de parámetro, aunque
# no haya parámetros. Un LIKE '%activo%' escrito así revienta con IndexError en
# tiempo de ejecución — que fue exactamente lo que tumbó /api/admin/global_summary.

PLACEHOLDER = re.compile(r"%(s|\(|%)")


def test_los_literales_por_ciento_van_duplicados():
    malas = []
    for f in sorted(FUENTES):
        src = f.read_text(encoding="utf-8")
        for m in re.finditer(r'"""(.*?)"""', src, re.S):
            sql = m.group(1)
            if not re.search(r"\bSELECT\b|\bUPDATE\b|\bINSERT\b|\bDELETE\b", sql, re.I):
                continue
            base = src[: m.start()].count("\n") + 1
            for n, linea in enumerate(sql.split("\n")):
                if "%" not in linea:
                    continue
                if "%" in PLACEHOLDER.sub("", linea):
                    malas.append(
                        f"{f.relative_to(APP)}:{base + n}  {linea.strip()[:80]}"
                    )
    assert not malas, (
        "literales % sin duplicar dentro de SQL (psycopg2 los toma como "
        "marcador y lanza IndexError):\n" + "\n".join(sorted(set(malas)))
    )
