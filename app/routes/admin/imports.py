"""Importación masiva de datos via CSV."""
import csv as _csv_module
import io as _io_module
import unicodedata as _unicodedata_module
from datetime import datetime

from fastapi import APIRouter, UploadFile, File, Query, Depends

from database import db_query, db_transaction, log_event
from .deps import require_session
from .helpers import (_resolve_or_create_lookup, _resolve_or_create_tipo_documento,
                      _resolve_user_id)

router = APIRouter()

_CSV_ENCODINGS = ("utf-8-sig", "utf-8", "latin-1", "cp1252")
_VALID_SOPORTE = ("Físico", "Digital", "Digitalizado")

# OR-111: sin tope, un CSV de cientos de miles de filas -o uno malicioso de
# varios cientos de MB- agota la memoria del lambda sin dejar ni resultado
# parcial ni rastro del motivo.
_MAX_CSV_BYTES = 10 * 1024 * 1024
_MAX_CSV_ROWS = 20000

# OR-107: cabeceras habituales que no coinciden con el nombre exacto de columna.
_HEADER_SYNONYMS = {
    "ci": "cedula", "cedula_identidad": "cedula", "documento": "cedula", "c.i.": "cedula",
    "nombre": "nombres", "apellido": "apellidos",
    "fecha_de_ingreso": "fecha_ingreso", "fecha_de_nacimiento": "fecha_nacimiento",
    "correo": "email", "telefono": "phone",
}

_EMPLEADOS_COLUMNAS = {
    "cedula", "nombres", "apellidos", "cargo", "departamento", "estado", "rif",
    "fecha_ingreso", "fecha_jubilacion", "fecha_pension", "foto_url",
    "fecha_nacimiento", "nivel_educativo", "sexo",
}
_EMPLEADOS_REQUERIDAS = {"cedula"}

_DOCS_ARCHIVO_COLUMNAS = {
    "titulo", "autor", "fecha", "tipo_documento", "abstract", "ubicacion",
    "palabras_clave", "numero_folio", "soporte", "numero_paginas",
}
_DOCS_ARCHIVO_REQUERIDAS = {"titulo"}

_DOCS_RRHH_COLUMNAS = {
    "cedula_empleado", "tipo_documento", "fecha", "notas", "ubicacion",
    "numero_folio", "soporte", "numero_paginas", "titulo",
}
_DOCS_RRHH_REQUERIDAS = {"cedula_empleado"}


def _coerce_soporte(raw: str) -> str:
    v = (raw or "").strip()
    return v if v in _VALID_SOPORTE else "Físico"


def _decode_csv(content: bytes) -> str | None:
    for enc in _CSV_ENCODINGS:
        try:
            return content.decode(enc)
        except (UnicodeDecodeError, LookupError):
            continue
    return None


def _normalize_header(h: str) -> str:
    """OR-107: minúsculas, sin acentos ni espacios, con sinónimos habituales
    - "Cédula", "CEDULA" o " cedula " deben reconocerse todas igual."""
    h = (h or "").strip().lower().replace(" ", "_")
    h = _unicodedata_module.normalize("NFKD", h).encode("ascii", "ignore").decode("ascii")
    return _HEADER_SYNONYMS.get(h, h)


def _sniff_delimiter(text: str) -> str:
    """OR-106: Excel en español exporta con `;`, no con `,`. Sin detección,
    un CSV así se lee como una sola columna y todas las filas se omiten en
    silencio."""
    try:
        return _csv_module.Sniffer().sniff(text[:4096], delimiters=",;\t").delimiter
    except _csv_module.Error:
        return ","


def _open_csv_reader(text: str):
    """DictReader con delimitador detectado y cabeceras normalizadas.
    Retorna (reader, delimiter, fieldnames) o (None, delimiter, []) si el
    archivo no tiene ni una fila de cabecera."""
    delimiter = _sniff_delimiter(text)
    sio = _io_module.StringIO(text)
    raw_reader = _csv_module.reader(sio, delimiter=delimiter)
    try:
        raw_header = next(raw_reader)
    except StopIteration:
        return None, delimiter, []
    fieldnames = [_normalize_header(h) for h in raw_header]
    # DictReader construye su propio csv.reader interno: se le pasa el mismo
    # `sio` (ya posicionado tras la cabecera), no el `raw_reader` -que
    # entrega listas ya parseadas- que sólo sirvió para leer esa cabecera.
    reader = _csv_module.DictReader(sio, fieldnames=fieldnames, delimiter=delimiter)
    return reader, delimiter, fieldnames


def _columnas_reporte(fieldnames, conocidas: set, requeridas: set) -> dict:
    """OR-108: una columna desconocida o una obligatoria ausente se ignoraban
    sin ningún aviso; quien preparó el CSV creía que se había guardado."""
    presentes = set(fieldnames)
    return {
        "columnas_desconocidas": sorted(presentes - conocidas),
        "columnas_faltantes": sorted(requeridas - presentes),
    }


def _parse_date(v):
    v = str(v or "").strip()
    if not v:
        return None
    for fmt in ("%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y"):
        try:
            return datetime.strptime(v, fmt).date()
        except Exception:
            pass
    return None


@router.post("/import/empleados")
async def import_empleados_csv(
    file: UploadFile = File(...),
    usuario_sesion: str = Depends(require_session),
):
    """
    Importa o actualiza empleados desde CSV.

    IN-133: `log_event` registra `usuario_sesion` (de la sesión verificada
    por `require_session`), nunca un campo declarado por el cliente.

    Columnas reconocidas (mínimo `cedula` requerida):
    - `cedula`, `nombres`, `apellidos`, `cargo`, `departamento`, `estado`
    - `rif`, `fecha_ingreso`, `fecha_jubilacion`, `fecha_pension`, `foto_url`
    - `fecha_nacimiento` (LOTTT), `nivel_educativo`, `sexo` (M/F/O)

    Encodings soportados: UTF-8 (con o sin BOM), Latin-1, CP1252.
    Si la cédula ya existe, actualiza SOLO las columnas que la fila trae (una
    columna ausente o vacía nunca borra un valor existente). Si no existe, se
    inserta (requiere `fecha_ingreso`, NOT NULL en el esquema; si el CSV no la
    trae, se usa la fecha de hoy y la fila queda listada en `fecha_ingreso_por_defecto`).
    """
    content = await file.read()
    if len(content) > _MAX_CSV_BYTES:
        return {"inserted": 0, "updated": 0, "skipped": 0, "errors": [
            f"Archivo demasiado grande ({len(content) // (1024*1024)} MB). Máximo {_MAX_CSV_BYTES // (1024*1024)} MB."]}
    text = _decode_csv(content)
    if text is None:
        return {"inserted": 0, "updated": 0, "skipped": 0, "errors": ["No se pudo decodificar el archivo. Use UTF-8 o Latin-1."]}
    reader, delimiter, fieldnames = _open_csv_reader(text)
    if reader is None:
        return {"inserted": 0, "updated": 0, "skipped": 0, "errors": ["Archivo CSV vacío o sin encabezados."]}
    results = {
        "inserted": 0, "updated": 0, "skipped": 0, "errors": [],
        "fecha_ingreso_por_defecto": [],  # cédulas insertadas sin fecha_ingreso en el CSV
        "en_papelera": [],  # OR-113: cédulas que sólo existen en la papelera
        "delimitador_detectado": delimiter,
        **_columnas_reporte(fieldnames, _EMPLEADOS_COLUMNAS, _EMPLEADOS_REQUERIDAS),
    }
    # IN-217 paso 1: cargo/departamento/estado se repiten muchísimo entre
    # filas (ej. 500 filas con "Profesor Titular" = 500 consultas idénticas
    # antes de este cambio). Se memoiza en memoria por (tabla, nombre) dentro
    # de esta importación: la primera vez que aparece un nombre se resuelve
    # contra la BD (y se crea si no existe, igual que antes) y las siguientes
    # filas con el mismo nombre reutilizan el id ya resuelto sin consultar de
    # nuevo. Como una fila puede crear un cargo/departamento/estado que otra
    # fila más adelante use, la caché se actualiza fila a fila (no se
    # precarga toda de una vez): así una creación de la fila 3 sigue estando
    # disponible para la fila 40 sin repetir la consulta.
    _lookup_cache: dict[tuple[str, str], int] = {}

    def _cached_lookup(table: str, nombre: str, default_name: str) -> int:
        key = (table, (nombre or "").strip() or default_name)
        if key in _lookup_cache:
            return _lookup_cache[key]
        resolved = _resolve_or_create_lookup(table, nombre, default_name)
        _lookup_cache[key] = resolved
        return resolved

    for i, row in enumerate(reader, 1):
        if i > _MAX_CSV_ROWS:
            results["errors"].append(
                f"Se detuvo en la fila {_MAX_CSV_ROWS}: el archivo trae más filas que el máximo soportado por importación.")
            break
        cedula = str(row.get("cedula", "") or "").strip()
        if not cedula:
            results["skipped"] += 1
            continue
        nombres         = str(row.get("nombres",   "") or "").strip()
        apellidos       = str(row.get("apellidos", "") or "").strip()
        cargo           = str(row.get("cargo",     "") or "").strip()
        depto           = str(row.get("departamento", "") or "").strip()
        estado          = str(row.get("estado", "") or "").strip()
        rif             = str(row.get("rif",       "") or "").strip()
        foto_url        = str(row.get("foto_url",  "") or "").strip() or None
        fecha_ing_raw   = str(row.get("fecha_ingreso", "") or "").strip()
        fecha_ing       = _parse_date(fecha_ing_raw)
        fecha_jub       = _parse_date(row.get("fecha_jubilacion"))
        fecha_pen       = _parse_date(row.get("fecha_pension"))
        fecha_nac       = _parse_date(row.get("fecha_nacimiento"))
        nivel_educativo = str(row.get("nivel_educativo", "") or "").strip() or None
        sexo_raw        = str(row.get("sexo", "") or "").strip().upper()
        sexo            = sexo_raw if sexo_raw in ("M", "F", "O") else None
        try:
            # cargo_id/departamento_id/estado_id son NOT NULL en el esquema:
            # se resuelven siempre (usan su fila "Por Asignar"/"Pendiente de
            # Registro" por defecto cuando el CSV no trae el nombre) para no
            # repetir en el alta el mismo fallo de OR-002.
            existing = db_query(
                "SELECT id, deleted_at FROM public.empleados WHERE cedula=%s", [cedula], fetch="one")
            if existing and existing.get("deleted_at"):
                # OR-113: esta cédula sólo existe en la papelera. Actualizarla
                # como si estuviera activa la "revive" en silencio: se cuenta
                # como updated pero sigue invisible en toda pantalla.
                results["en_papelera"].append(cedula)
                continue
            if not existing and (not nombres or not apellidos):
                # IN-026: nombres/apellidos son NOT NULL en el esquema pero
                # "" los satisface; sin esta guarda, un CSV de sólo cédulas
                # crea empleados en blanco que el buscador de RRHH lista
                # como filas vacías (persona_raw = " ").
                results["errors"].append(
                    f"Fila {i} ({cedula}): nombres y apellidos son obligatorios para un empleado nuevo.")
                continue
            cargo_id = dept_id = estado_id = None
            if cargo:
                cargo_id = _cached_lookup("cargos", cargo, "Por Asignar")
            if depto:
                dept_id = _cached_lookup("departamentos", depto, "Por Asignar")
            if estado:
                estado_id = _cached_lookup("estados_laborales", estado, "Pendiente de Registro")
            if not existing:
                if cargo_id is None:
                    cargo_id = _cached_lookup("cargos", "", "Por Asignar")
                if dept_id is None:
                    dept_id = _cached_lookup("departamentos", "", "Por Asignar")
                if estado_id is None:
                    estado_id = _cached_lookup("estados_laborales", "", "Pendiente de Registro")
            with db_transaction() as execute:
                if existing:
                    # OR-004: una columna ausente o vacía en el CSV significa
                    # "no tocar" — nunca se sobreescribe con NULL/vacío lo que
                    # ya está en la ficha. Toda columna es condicional.
                    set_clauses, set_params = [], []
                    if nombres:         set_clauses.append("nombres=%s");          set_params.append(nombres)
                    if apellidos:       set_clauses.append("apellidos=%s");        set_params.append(apellidos)
                    if rif:             set_clauses.append("rif=%s");              set_params.append(rif)
                    if fecha_jub:       set_clauses.append("fecha_jubilacion=%s"); set_params.append(fecha_jub)
                    if fecha_pen:       set_clauses.append("fecha_pension=%s");    set_params.append(fecha_pen)
                    if fecha_ing:       set_clauses.append("fecha_ingreso=%s");    set_params.append(fecha_ing)
                    if cargo_id:        set_clauses.append("cargo_id=%s");         set_params.append(cargo_id)
                    if dept_id:         set_clauses.append("departamento_id=%s");  set_params.append(dept_id)
                    if estado_id:       set_clauses.append("estado_id=%s");        set_params.append(estado_id)
                    if foto_url:        set_clauses.append("foto_url=%s");         set_params.append(foto_url)
                    if fecha_nac:       set_clauses.append("fecha_nacimiento=%s"); set_params.append(fecha_nac)
                    if nivel_educativo: set_clauses.append("nivel_educativo=%s");  set_params.append(nivel_educativo)
                    if sexo:            set_clauses.append("sexo=%s");             set_params.append(sexo)
                    if set_clauses:
                        set_params.append(cedula)
                        execute(
                            f"UPDATE public.empleados SET {','.join(set_clauses)} WHERE cedula=%s",
                            set_params,
                        )
                        results["updated"] += 1
                    else:
                        results["skipped"] += 1
                else:
                    # OR-002: fecha_ingreso es NOT NULL sin DEFAULT en el
                    # esquema; si el CSV no la trae se usa la fecha de hoy y
                    # se avisa explícitamente en la respuesta.
                    if not fecha_ing:
                        fecha_ing = datetime.now().date()
                        results["fecha_ingreso_por_defecto"].append(cedula)
                    execute(
                        """INSERT INTO public.empleados
                           (cedula,nombres,apellidos,rif,cargo_id,departamento_id,estado_id,
                            fecha_ingreso,fecha_jubilacion,fecha_pension,foto_url,fecha_nacimiento,
                            nivel_educativo,sexo)
                           VALUES(%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)""",
                        [cedula, nombres, apellidos, rif or None, cargo_id, dept_id, estado_id,
                         fecha_ing, fecha_jub, fecha_pen, foto_url, fecha_nac, nivel_educativo, sexo],
                    )
                    results["inserted"] += 1
        except Exception as e:
            results["errors"].append(f"Fila {i} ({cedula}): {str(e)[:100]}")
    huboExito = results["inserted"] > 0 or results["updated"] > 0
    resumen = (
        f"inserted={results['inserted']} updated={results['updated']} "
        f"skipped={results['skipped']} errors={len(results['errors'])}"
    )
    results["success"] = huboExito
    results["message"] = (
        f"Importación completada: {resumen}." if huboExito
        else f"Importación sin cambios: ninguna fila se insertó ni actualizó ({resumen})."
    )
    log_event(usuario_sesion, "Import CSV Empleados", "RRHH", resumen)
    return results


@router.post("/import/documentos")
async def import_documentos_csv(
    file: UploadFile = File(...),
    modulo: str = Query(default="Archivo"),
    usuario_sesion: str = Depends(require_session),
):
    """
    Archivo: columnas titulo,autor,fecha,tipo_documento,abstract,ubicacion,palabras_clave[,numero_folio,soporte,numero_paginas]
    RRHH:    columnas cedula_empleado,tipo_documento,fecha,notas,ubicacion[,numero_folio,soporte,numero_paginas]

    IN-133: `creado_por`/`updated_by` y `log_event` usan `usuario_sesion`,
    nunca un campo declarado por el cliente.
    """
    from .helpers import _require_modulo
    _require_modulo(modulo)
    content = await file.read()
    if len(content) > _MAX_CSV_BYTES:
        return {"inserted": 0, "skipped": 0, "errors": [
            f"Archivo demasiado grande ({len(content) // (1024*1024)} MB). Máximo {_MAX_CSV_BYTES // (1024*1024)} MB."]}
    text = _decode_csv(content)
    if text is None:
        return {"inserted": 0, "skipped": 0, "errors": ["No se pudo decodificar el archivo. Use UTF-8 o Latin-1."]}
    reader, delimiter, fieldnames = _open_csv_reader(text)
    if reader is None:
        return {"inserted": 0, "skipped": 0, "errors": ["Archivo CSV vacío o sin encabezados."]}
    # updated_by es INTEGER: hay que guardar el id del usuario, no su nombre.
    _uid = _resolve_user_id(usuario_sesion)
    _columnas, _requeridas = (
        (_DOCS_ARCHIVO_COLUMNAS, _DOCS_ARCHIVO_REQUERIDAS) if modulo == "Archivo"
        else (_DOCS_RRHH_COLUMNAS, _DOCS_RRHH_REQUERIDAS)
    )
    results = {
        "inserted": 0, "updated": 0, "skipped": 0, "errors": [],
        "titulo_por_defecto": [],  # filas de RRHH sin `titulo` en el CSV: se derivó del tipo de documento
        "delimitador_detectado": delimiter,
        **_columnas_reporte(fieldnames, _columnas, _requeridas),
    }
    # IN-217 paso 1: tipo_documento se repite entre filas igual que
    # cargo/departamento en import_empleados_csv. Se memoiza por (nombre,
    # cat_slug) dentro de esta importación; ver comentario equivalente
    # arriba sobre por qué se cachea fila a fila y no se precarga entera.
    _tipo_cache: dict[tuple[str, str], int] = {}

    def _cached_tipo_documento(nombre: str, cat_slug: str) -> int:
        key = (nombre, cat_slug or "")
        if key in _tipo_cache:
            return _tipo_cache[key]
        resolved = _resolve_or_create_tipo_documento(nombre, cat_slug)
        _tipo_cache[key] = resolved
        return resolved

    for i, row in enumerate(reader, 1):
        if i > _MAX_CSV_ROWS:
            results["errors"].append(
                f"Se detuvo en la fila {_MAX_CSV_ROWS}: el archivo trae más filas que el máximo soportado por importación.")
            break
        try:
            if modulo == "Archivo":
                titulo = str(row.get("titulo", "") or "").strip()
                if not titulo:
                    results["skipped"] += 1
                    continue
                tipo_nombre = str(row.get("tipo_documento", "") or "").strip()
                tipo_id = _cached_tipo_documento(tipo_nombre, "archivo") if tipo_nombre else None
                _soporte = _coerce_soporte(row.get("soporte", ""))
                _paginas_raw = str(row.get("numero_paginas", "") or "").strip()
                _paginas = int(_paginas_raw) if _paginas_raw.isdigit() and int(_paginas_raw) > 0 else None
                # El documento y sus palabras clave se confirman en una sola
                # transacción: si un INSERT de palabra clave falla a mitad,
                # antes quedaba el documento creado sin ninguna de sus
                # palabras clave y sin forma de saberlo desde la respuesta.
                with db_transaction() as execute:
                    doc_row = execute(
                        """INSERT INTO public.datos_archivo
                               (titulo,autor,fecha_documento,tesauro_primario,id_tipo_documento,
                                abstract,ubicacion,creado_por,updated_by,
                                numero_folio,soporte,numero_paginas)
                           VALUES(%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s) RETURNING id_archivo""",
                        [
                            titulo, row.get("autor", ""), _parse_date(row.get("fecha")),
                            tipo_nombre, tipo_id, row.get("abstract", ""), row.get("ubicacion", ""),
                            _uid, _uid,
                            str(row.get("numero_folio", "") or "").strip() or None,
                            _soporte, _paginas,
                        ],
                        fetch="one",
                    )
                    pk_str = str(row.get("palabras_clave", "") or "").strip()
                    if pk_str and doc_row:
                        for kw in [k.strip() for k in pk_str.split(";") if k.strip()]:
                            kw_row = execute(
                                "INSERT INTO public.descriptores_libres(nombre) VALUES(%s) ON CONFLICT(nombre) DO UPDATE SET nombre=EXCLUDED.nombre RETURNING id_descriptor",
                                [kw], fetch="one",
                            )
                            if kw_row:
                                execute(
                                    "INSERT INTO public.archivo_descriptores(id_archivo,id_descriptor) VALUES(%s,%s) ON CONFLICT DO NOTHING",
                                    [doc_row["id_archivo"], kw_row["id_descriptor"]], fetch="none",
                                )
                results["inserted"] += 1
            else:
                cedula = str(row.get("cedula_empleado", "") or "").strip()
                if not cedula:
                    results["skipped"] += 1
                    continue
                emp = db_query("SELECT id FROM public.empleados WHERE cedula=%s", [cedula], fetch="one")
                if not emp:
                    results["errors"].append(f"Fila {i}: cédula {cedula} no existe")
                    continue
                tipo_nombre = str(row.get("tipo_documento", "") or "").strip()
                # OR-008: sin cat_slug, un tipo nuevo caía en la primera
                # categoría por id (normalmente Archivo) y desaparecía del
                # desplegable de RRHH y de la cobertura por Parte.
                tipo_id = _cached_tipo_documento(tipo_nombre, "parte-i") if tipo_nombre else None
                if not tipo_id:
                    results["errors"].append(f"Fila {i}: tipo_documento inválido")
                    continue
                _soporte_r = _coerce_soporte(row.get("soporte", ""))
                _paginas_r_raw = str(row.get("numero_paginas", "") or "").strip()
                _paginas_r = int(_paginas_r_raw) if _paginas_r_raw.isdigit() and int(_paginas_r_raw) > 0 else None
                # OR-003: datos_rrhh.titulo es NOT NULL y esta rama nunca lo
                # incluía. Se toma la columna `titulo` del CSV si la trae; si
                # no, se compone del tipo de documento (como hace
                # admin_submit en docs.py) y se avisa en la respuesta.
                titulo = str(row.get("titulo", "") or "").strip()
                if not titulo:
                    titulo = f"{tipo_nombre} - {cedula}"
                    results["titulo_por_defecto"].append(f"Fila {i} ({cedula})")
                ubicacion = str(row.get("ubicacion", "") or "").strip() or "Por Ubicar"
                db_query(
                    """INSERT INTO public.datos_rrhh
                           (titulo,empleado_id,id_tipo_documento,fecha_documento,notas,ubicacion,
                            creado_por,updated_by,numero_folio,soporte,numero_paginas)
                       VALUES(%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)""",
                    [titulo, emp["id"], tipo_id, _parse_date(row.get("fecha")), row.get("notas", ""),
                     ubicacion, _uid, _uid,
                     str(row.get("numero_folio", "") or "").strip() or None, _soporte_r, _paginas_r],
                    fetch="none", commit=True,
                )
                results["inserted"] += 1
        except Exception as e:
            results["errors"].append(f"Fila {i}: {str(e)[:100]}")
    huboExito = results["inserted"] > 0 or results["updated"] > 0
    resumen = (
        f"inserted={results['inserted']} updated={results['updated']} "
        f"skipped={results['skipped']} errors={len(results['errors'])}"
    )
    results["success"] = huboExito
    results["message"] = (
        f"Importación completada: {resumen}." if huboExito
        else f"Importación sin cambios: ninguna fila se insertó ni actualizó ({resumen})."
    )
    log_event(usuario_sesion, f"Import CSV Docs ({modulo})", modulo, resumen)
    return results
