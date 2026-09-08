"""Servidor local del Archivo Institucional con la base de datos simulada.

No requiere Neon ni .env: sustituye `database.db_query` por un generador de
filas verosímiles. Las columnas se deducen del propio SQL (alias del SELECT) y,
cuando el SQL usa `*`, del esquema de `app/schema.sql`.

    python docs/auditoria/capturas/_harness/fakedb.py      # sirve en :8099
"""
import os
import re
import sys
import random
import datetime as _dt

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", ".."))
APP = os.path.join(ROOT, "app")
sys.path.insert(0, APP)

os.environ.setdefault("DATABASE_URL", "postgresql://fake/fake")
os.environ.setdefault("SECRET_KEY", "auditoria-visual-key")
os.environ.setdefault("ENVIRONMENT", "development")
os.environ.setdefault("OPENROUTER_API_KEY", "sk-or-fake-para-auditoria")

# ---------------------------------------------------------------------------
# Datos representativos
# ---------------------------------------------------------------------------
NOMBRES = [
    "María José Rodríguez Peña", "José Ángel Muñoz Álvarez",
    "Ana Cecilia Bermúdez de la Peña Castañeda", "Luis Gómez",
    "Rosángela Núñez", "", "Ñ. Sáenz-Bolívar", "Iñaki Etxebarría Goikoetxea",
]
TITULOS = [
    "Resolución del Consejo de Facultad Nº 2024-118 sobre la reestructuración "
    "de la Escuela de Biología y la creación de la Coordinación de Postgrado "
    "en Ecología Tropical y Conservación de la Biodiversidad Amazónica",
    "Acta de grado",
    "Informe técnico de evaluación de laboratorios",
    "", None,
    "Comunicación interna DIR-CIENCIAS-2023-0451",
    "Convenio marco de cooperación interinstitucional UCV — IVIC — CONICIT",
    "Memorándum",
]
TEXTO_LARGO = (
    "El presente documento recoge las deliberaciones del Consejo de Facultad "
    "en torno a la reorganización administrativa, incluyendo consideraciones "
    "presupuestarias, de personal docente y de infraestructura, así como los "
    "anexos correspondientes al inventario de bienes muebles adscritos a cada "
    "una de las dependencias evaluadas durante el ejercicio fiscal anterior."
)
DEPTOS = ["Química Orgánica", "Biología Celular", "Matemáticas", "", "Física Teórica y Computacional Aplicada"]
CARGOS = ["Profesor Titular a Dedicación Exclusiva", "Instructor", "Asistente", "", "Agregado"]
ESTADOS = ["Activo", "Jubilado", "Pensionado", "Permiso", ""]
TIPOS = ["Resolución", "Acta", "Informe", "Comunicación", "Oficio", "Memorándum", "Convenio", "Constancia"]


def _fecha(i):
    if i % 7 == 3:
        return None
    return (_dt.date(2019, 1, 1) + _dt.timedelta(days=i * 97 % 2200)).isoformat()


def _valor(key, i):
    k = key.lower()
    if k in ("id", "empleado_id", "documento_id", "doc_id", "usuario_id", "categoria_id",
             "id_categoria", "id_tipo_documento", "tipo_id", "version_id", "parent_id"):
        return i + 1
    if k in ("is_active", "activo", "active"):
        return i % 9 != 4          # la fila 0 siempre activa: es el usuario de la sesión
    if k.startswith("is_") or k.startswith("has_") or k in ("aprobada", "leido"):
        return i % 3 != 0
    if k in ("value", "valor", "pos", "orden", "peso", "monto", "score"):
        return [3, 18, 42, 7, 121, 66, 9, 1284][i % 8]
    if k == "label":
        return [TIPOS[i % len(TIPOS)], DEPTOS[i % len(DEPTOS)], str(2019 + i % 7)][i % 3] or "Sin tipo"
    if k in ("cnt", "c", "n", "qty", "num", "cant", "cuenta", "veces"):
        return [0, 1, 7, 42, 1284, 3][i % 6]
    if k in ("anio", "year", "ano", "ejercicio"):
        return 2019 + (i % 7)
    if "count" in k or "total" in k or k in ("n", "num", "cantidad", "docs", "size", "bytes",
                                             "tamano", "anios", "plazo_retencion_anios", "dias"):
        return [0, 1, 7, 42, 1284, 3][i % 6]
    if "porcentaje" in k or "pct" in k or "ratio" in k or "costo" in k or "gasto" in k:
        return round((i * 13.7) % 100, 2)
    if "fecha" in k or k.endswith("_at") or k in ("timestamp", "date", "created", "updated"):
        return _fecha(i)
    if "cedula" in k or k == "ci":
        return ["V-12.345.678", "V-4.567.890", "E-81.234.567", "", "V-26.111.222"][i % 5]
    if "rif" in k:
        return "J-30112233-4"
    if "email" in k or "correo" in k:
        return ["mjose.rodriguez@ciencias.ucv.ve", "", "usuario.con.un.correo.institucional.muy.largo@postgrado.ciencias.ucv.ve"][i % 3]
    if "url" in k or k in ("foto_url", "file_url", "archivo"):
        return ["/static/logo.png", None, "https://r2.example.com/docs/expediente-2024-0118.pdf"][i % 3]
    if "modulo" in k:
        return ["Archivo", "RRHH", "Global"][i % 3]
    if k == "rol":
        return ["Admin", "Normal"][i % 2]
    if "usuario" in k or "user" in k or k in ("creado_por", "updated_by", "responsable"):
        return ["mjrodriguez", "admin.global", "", "jose.angel.munoz.alvarez"][i % 4]
    if "cargo" in k:
        return CARGOS[i % len(CARGOS)]
    if "departamento" in k or "dependencia" in k or "escuela" in k:
        return DEPTOS[i % len(DEPTOS)]
    if "estado" in k or k == "status":
        return ESTADOS[i % len(ESTADOS)]
    if "tipo" in k or "categoria" in k or "tesauro" in k or "clasificacion" in k:
        return TIPOS[i % len(TIPOS)]
    if "titulo" in k or k == "asunto":
        return TITULOS[i % len(TITULOS)]
    if "nombre" in k or "apellido" in k or "persona" in k or "autor" in k or "docente" in k:
        return NOMBRES[i % len(NOMBRES)]
    if "abstract" in k or "nota" in k or "detalle" in k or "descripcion" in k or "observ" in k or "resumen" in k:
        return [TEXTO_LARGO, "", None, "Sin observaciones."][i % 4]
    if "ubicacion" in k:
        return ["Sala 3 · Estante B · Caja 14", "", "Depósito principal, pasillo 2, módulo de estantería móvil número 17, bandeja superior"][i % 3]
    if "descriptor" in k or "palabra" in k or "badge" in k or "tipos" in k or "tags" in k:
        return [["Presupuesto", "Consejo de Facultad", "Reestructuración"], [], ["Ñandú"]][i % 3]
    if "accion" in k or "evento" in k:
        return ["Login Success", "Documento actualizado", "Importación CSV", "Borrado"][i % 4]
    return [f"Valor {i + 1}", "", None, TEXTO_LARGO[:80]][i % 4]


class Fila(dict):
    def __init__(self, keys, i):
        super().__init__({k: _valor(k, i) for k in keys})
        self._i = i

    def __missing__(self, key):
        v = _valor(key, self._i)
        self[key] = v
        return v

    def get(self, key, default=None):
        if key in self:
            return dict.get(self, key)
        return self.__missing__(key)


# ---------------------------------------------------------------------------
# Columnas del esquema, para los SELECT *
# ---------------------------------------------------------------------------
def _cargar_esquema():
    txt = open(os.path.join(APP, "schema.sql"), encoding="utf-8").read()
    tablas = {}
    for m in re.finditer(r"CREATE TABLE (?:IF NOT EXISTS )?(?:public\.)?(\w+)\s*\((.*?)\n\);", txt, re.S):
        cols = []
        for linea in m.group(2).split("\n"):
            linea = linea.strip()
            mm = re.match(r"([a-z_][a-z0-9_]*)\s+[A-Z]", linea)
            if mm and mm.group(1).lower() not in ("constraint", "primary", "unique", "foreign", "check"):
                cols.append(mm.group(1))
        tablas[m.group(1).lower()] = cols
    return tablas


ESQUEMA = _cargar_esquema()
COMUNES = ["id", "nombre", "titulo", "slug", "fecha_documento", "created_at", "updated_at",
           "modulo", "usuario", "estado", "total", "count"]


def _split_top(s):
    partes, prof, actual = [], 0, ""
    for ch in s:
        if ch == "(":
            prof += 1
        elif ch == ")":
            prof -= 1
        if ch == "," and prof == 0:
            partes.append(actual); actual = ""
        else:
            actual += ch
    if actual.strip():
        partes.append(actual)
    return partes


def _columnas(sql):
    s = " ".join(sql.split())
    m = re.search(r"\bSELECT\b(.*?)\bFROM\b", s, re.I | re.S)
    if not m:
        return list(COMUNES)
    tabla = None
    mt = re.search(r"\bFROM\s+(?:public\.)?(\w+)", s, re.I)
    if mt:
        tabla = mt.group(1).lower()
    cols = []
    for parte in _split_top(m.group(1)):
        parte = parte.strip()
        if not parte:
            continue
        if parte.endswith("*"):
            cols += ESQUEMA.get(tabla, COMUNES)
            continue
        ma = re.search(r"\bAS\s+\"?([a-zA-Z_][\w]*)\"?$", parte, re.I)
        if ma:
            cols.append(ma.group(1)); continue
        mb = re.search(r"([a-zA-Z_][\w]*)\s*$", parte)
        if mb:
            cols.append(mb.group(1))
    # columnas que el SQL menciona en WHERE/JOIN también suelen leerse luego
    for extra in re.findall(r"\b([a-z_][a-z0-9_]{3,})\b", s.lower()):
        if extra in ("select", "from", "where", "order", "group", "limit", "offset",
                     "inner", "left", "join", "having", "count", "coalesce", "public"):
            continue
    vistos, out = set(), []
    for c in cols:
        if c and c not in vistos:
            vistos.add(c); out.append(c)
    return out or list(COMUNES)


FILAS_POR_DEFECTO = int(os.environ.get("FAKE_ROWS", "14"))


def fake_db_query(sql, params=None, fetch="all", commit=False, _retries=2):
    if fetch == "none":
        return None
    cols = _columnas(sql)
    s = " ".join(sql.split()).lower()

    # `admin.global` es el usuario de sesión que usa todo el arnés visual
    # (capturar.py / estados.py, vía generate_session_token("admin.global")).
    # `require_role`/`require_admin_role` (app/routes/admin/deps.py) hacen una
    # consulta real contra `usuarios_sistema` en cada llamada para resolver su
    # módulo/rol/estado; sin este caso especial, la fila genérica que arma
    # `_valor()` para esa tabla no garantiza `modulo='Global'`, así que la
    # autorización fallaba (403) al entrar a pestañas admin. Cualquier
    # consulta contra `usuarios_sistema` cuyos params mencionen ese usuario
    # devuelve una fila fija que siempre pasa el chequeo "Global" + Admin.
    if "usuarios_sistema" in s and params and "admin.global" in [str(p) for p in params]:
        overrides = {
            "modulo": "Global",
            "rol": "Admin",
            "is_active": True,
            "activo": True,
            "usuario": "admin.global",
            "id": 1,
            "nombre": "Administrador Global",
        }

        def _fila_fija():
            fila = Fila(cols, 0)
            for k in cols:
                if k.lower() in overrides:
                    fila[k] = overrides[k.lower()]
            return fila

        if fetch == "one":
            return _fila_fija()
        return [_fila_fija()]

    if fetch == "one":
        return Fila(cols, 0)
    n = FILAS_POR_DEFECTO
    if " count(" in s and " group by " not in s:
        n = 1
    if " limit 1" in s:
        n = 1
    return [Fila(cols, i) for i in range(n)]


import database  # noqa: E402
database.db_query = fake_db_query
database.log_event = lambda *a, **k: None
database.ensure_audit_table = lambda *a, **k: None

import utils  # noqa: E402
utils.db_query = fake_db_query

import main  # noqa: E402
main.db_query = fake_db_query
main.run_migrations = lambda *a, **k: None
main.ensure_audit_table = lambda *a, **k: None
main.populate_missing_slugs = lambda *a, **k: None
main._backfill_rrhh_tipo_fk = lambda *a, **k: None

for _mod in list(sys.modules.values()):
    if getattr(_mod, "__name__", "").startswith(("routes", "core")) and hasattr(_mod, "db_query"):
        _mod.db_query = fake_db_query
    if hasattr(_mod, "log_event") and getattr(_mod, "__name__", "").startswith(("routes", "core")):
        _mod.log_event = lambda *a, **k: None

app = main.app

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="127.0.0.1", port=int(os.environ.get("PORT", "8099")), log_level="warning")
