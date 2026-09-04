"""Importación masiva de datos via CSV."""
import csv as _csv_module
import io as _io_module
from datetime import datetime

from fastapi import APIRouter, UploadFile, File, Query

from database import db_query, db_transaction, log_event
from .helpers import (_resolve_or_create_lookup, _resolve_or_create_tipo_documento,
                      _resolve_user_id)

router = APIRouter()

_CSV_ENCODINGS = ("utf-8-sig", "utf-8", "latin-1", "cp1252")
_VALID_SOPORTE = ("Físico", "Digital", "Digitalizado")


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
    requester: str = Query(default=""),
):
    """
    Importa o actualiza empleados desde CSV.

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
    text = _decode_csv(content)
    if text is None:
        return {"inserted": 0, "updated": 0, "skipped": 0, "errors": ["No se pudo decodificar el archivo. Use UTF-8 o Latin-1."]}
    reader = _csv_module.DictReader(_io_module.StringIO(text))
    if reader.fieldnames is None:
        return {"inserted": 0, "updated": 0, "skipped": 0, "errors": ["Archivo CSV vacío o sin encabezados."]}
    results = {
        "inserted": 0, "updated": 0, "skipped": 0, "errors": [],
        "fecha_ingreso_por_defecto": [],  # cédulas insertadas sin fecha_ingreso en el CSV
    }
    for i, row in enumerate(reader, 1):
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
            existing = db_query("SELECT id FROM public.empleados WHERE cedula=%s", [cedula], fetch="one")
            cargo_id = dept_id = estado_id = None
            if cargo:
                cargo_id = _resolve_or_create_lookup("cargos", cargo, "Por Asignar")
            if depto:
                dept_id = _resolve_or_create_lookup("departamentos", depto, "Por Asignar")
            if estado:
                estado_id = _resolve_or_create_lookup("estados_laborales", estado, "Pendiente de Registro")
            if not existing:
                if cargo_id is None:
                    cargo_id = _resolve_or_create_lookup("cargos", "", "Por Asignar")
                if dept_id is None:
                    dept_id = _resolve_or_create_lookup("departamentos", "", "Por Asignar")
                if estado_id is None:
                    estado_id = _resolve_or_create_lookup("estados_laborales", "", "Pendiente de Registro")
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
    log_event(requester, "Import CSV Empleados", "RRHH", resumen)
    return results


@router.post("/import/documentos")
async def import_documentos_csv(
    file: UploadFile = File(...),
    modulo: str = Query(default="Archivo"),
    requester: str = Query(default=""),
):
    """
    Archivo: columnas titulo,autor,fecha,tipo_documento,abstract,ubicacion,palabras_clave[,numero_folio,soporte,numero_paginas]
    RRHH:    columnas cedula_empleado,tipo_documento,fecha,notas,ubicacion[,numero_folio,soporte,numero_paginas]
    """
    from .helpers import _require_modulo
    _require_modulo(modulo)
    content = await file.read()
    text = _decode_csv(content)
    if text is None:
        return {"inserted": 0, "skipped": 0, "errors": ["No se pudo decodificar el archivo. Use UTF-8 o Latin-1."]}
    reader = _csv_module.DictReader(_io_module.StringIO(text))
    if reader.fieldnames is None:
        return {"inserted": 0, "skipped": 0, "errors": ["Archivo CSV vacío o sin encabezados."]}
    # updated_by es INTEGER: hay que guardar el id del usuario, no su nombre.
    _uid = _resolve_user_id(requester)
    results = {
        "inserted": 0, "updated": 0, "skipped": 0, "errors": [],
        "titulo_por_defecto": [],  # filas de RRHH sin `titulo` en el CSV: se derivó del tipo de documento
    }
    for i, row in enumerate(reader, 1):
        try:
            if modulo == "Archivo":
                titulo = str(row.get("titulo", "") or "").strip()
                if not titulo:
                    results["skipped"] += 1
                    continue
                tipo_nombre = str(row.get("tipo_documento", "") or "").strip()
                tipo_id = _resolve_or_create_tipo_documento(tipo_nombre, "archivo") if tipo_nombre else None
                _soporte = _coerce_soporte(row.get("soporte", ""))
                _paginas_raw = str(row.get("numero_paginas", "") or "").strip()
                _paginas = int(_paginas_raw) if _paginas_raw.isdigit() and int(_paginas_raw) > 0 else None
                doc_row = db_query(
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
                    fetch="one", commit=True,
                )
                pk_str = str(row.get("palabras_clave", "") or "").strip()
                if pk_str and doc_row:
                    for kw in [k.strip() for k in pk_str.split(";") if k.strip()]:
                        kw_row = db_query(
                            "INSERT INTO public.descriptores_libres(nombre) VALUES(%s) ON CONFLICT(nombre) DO UPDATE SET nombre=EXCLUDED.nombre RETURNING id_descriptor",
                            [kw], fetch="one", commit=True,
                        )
                        if kw_row:
                            db_query(
                                "INSERT INTO public.archivo_descriptores(id_archivo,id_descriptor) VALUES(%s,%s) ON CONFLICT DO NOTHING",
                                [doc_row["id_archivo"], kw_row["id_descriptor"]], fetch="none", commit=True,
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
                tipo_id = _resolve_or_create_tipo_documento(tipo_nombre) if tipo_nombre else None
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
    log_event(requester, f"Import CSV Docs ({modulo})", modulo, resumen)
    return results
