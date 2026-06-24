"""Importación masiva de datos via CSV."""
import csv as _csv_module
import io as _io_module
from datetime import datetime

from fastapi import APIRouter, UploadFile, File, Query

from database import db_query, log_event
from .admin_helpers import _resolve_or_create_lookup, _resolve_or_create_tipo_documento

router = APIRouter()


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
    - `rif`, `fecha_jubilacion`, `fecha_pension`, `foto_url`
    - `fecha_nacimiento` (LOTTT), `nivel_educativo`, `sexo` (M/F/O)

    Encodings soportados: UTF-8 (con o sin BOM), Latin-1, CP1252.
    Si la cédula ya existe, actualiza el registro; si no, lo inserta.
    """
    content = await file.read()
    for enc in ("utf-8-sig", "utf-8", "latin-1", "cp1252"):
        try:
            text = content.decode(enc)
            break
        except (UnicodeDecodeError, LookupError):
            continue
    else:
        return {"inserted": 0, "updated": 0, "skipped": 0, "errors": ["No se pudo decodificar el archivo. Use UTF-8 o Latin-1."]}
    reader = _csv_module.DictReader(_io_module.StringIO(text))
    if reader.fieldnames is None:
        return {"inserted": 0, "updated": 0, "skipped": 0, "errors": ["Archivo CSV vacío o sin encabezados."]}
    results = {"inserted": 0, "updated": 0, "skipped": 0, "errors": []}
    for i, row in enumerate(reader, 1):
        cedula = str(row.get("cedula", "") or "").strip()
        if not cedula:
            results["skipped"] += 1
            continue
        nombres         = str(row.get("nombres",   "") or "").strip()
        apellidos       = str(row.get("apellidos", "") or "").strip()
        cargo           = str(row.get("cargo",     "") or "").strip()
        depto           = str(row.get("departamento", "") or "").strip()
        estado          = str(row.get("estado", "Activo") or "Activo").strip()
        rif             = str(row.get("rif",       "") or "").strip()
        foto_url        = str(row.get("foto_url",  "") or "").strip() or None
        fecha_jub       = _parse_date(row.get("fecha_jubilacion"))
        fecha_pen       = _parse_date(row.get("fecha_pension"))
        fecha_nac       = _parse_date(row.get("fecha_nacimiento"))
        nivel_educativo = str(row.get("nivel_educativo", "") or "").strip() or None
        sexo_raw        = str(row.get("sexo", "") or "").strip().upper()
        sexo            = sexo_raw if sexo_raw in ("M", "F", "O") else None
        try:
            cargo_id  = _resolve_or_create_lookup("cargos", cargo, "Por Asignar") if cargo else None
            dept_id   = _resolve_or_create_lookup("departamentos", depto, "Por Asignar") if depto else None
            estado_id = _resolve_or_create_lookup("estados_laborales", estado, "Pendiente de Registro") if estado else None
            existing = db_query("SELECT id FROM public.empleados WHERE cedula=%s", [cedula], fetch="one")
            if existing:
                set_clauses = ["nombres=%s", "apellidos=%s", "rif=%s", "fecha_jubilacion=%s", "fecha_pension=%s"]
                set_params  = [nombres, apellidos, rif, fecha_jub, fecha_pen]
                if cargo_id:        set_clauses.append("cargo_id=%s");        set_params.append(cargo_id)
                if dept_id:         set_clauses.append("departamento_id=%s"); set_params.append(dept_id)
                if estado_id:       set_clauses.append("estado_id=%s");       set_params.append(estado_id)
                if foto_url:        set_clauses.append("foto_url=%s");        set_params.append(foto_url)
                if fecha_nac:       set_clauses.append("fecha_nacimiento=%s"); set_params.append(fecha_nac)
                if nivel_educativo: set_clauses.append("nivel_educativo=%s"); set_params.append(nivel_educativo)
                if sexo:            set_clauses.append("sexo=%s");            set_params.append(sexo)
                set_params.append(cedula)
                db_query(
                    f"UPDATE public.empleados SET {','.join(set_clauses)} WHERE cedula=%s",
                    set_params, fetch="none", commit=True,
                )
                results["updated"] += 1
            else:
                db_query(
                    """INSERT INTO public.empleados
                       (cedula,nombres,apellidos,rif,cargo_id,departamento_id,estado_id,
                        fecha_jubilacion,fecha_pension,foto_url,fecha_nacimiento,nivel_educativo,sexo)
                       VALUES(%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)""",
                    [cedula, nombres, apellidos, rif, cargo_id, dept_id, estado_id,
                     fecha_jub, fecha_pen, foto_url, fecha_nac, nivel_educativo, sexo],
                    fetch="none", commit=True,
                )
                results["inserted"] += 1
        except Exception as e:
            results["errors"].append(f"Fila {i} ({cedula}): {str(e)[:100]}")
    log_event(
        requester, "Import CSV Empleados", "RRHH",
        f"inserted={results['inserted']} updated={results['updated']} errors={len(results['errors'])}",
    )
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
    content = await file.read()
    for enc in ("utf-8-sig", "utf-8", "latin-1", "cp1252"):
        try:
            text = content.decode(enc)
            break
        except (UnicodeDecodeError, LookupError):
            continue
    else:
        return {"inserted": 0, "skipped": 0, "errors": ["No se pudo decodificar el archivo. Use UTF-8 o Latin-1."]}
    reader = _csv_module.DictReader(_io_module.StringIO(text))
    if reader.fieldnames is None:
        return {"inserted": 0, "skipped": 0, "errors": ["Archivo CSV vacío o sin encabezados."]}
    results = {"inserted": 0, "skipped": 0, "errors": []}
    for i, row in enumerate(reader, 1):
        try:
            if modulo == "Archivo":
                titulo = str(row.get("titulo", "") or "").strip()
                if not titulo:
                    results["skipped"] += 1
                    continue
                tipo_nombre = str(row.get("tipo_documento", "") or "").strip()
                tipo_id = _resolve_or_create_tipo_documento(tipo_nombre, "archivo") if tipo_nombre else None
                _soporte = str(row.get("soporte", "") or "Físico").strip()
                if _soporte not in ("Físico", "Digital", "Digitalizado"):
                    _soporte = "Físico"
                _paginas_raw = str(row.get("numero_paginas", "") or "").strip()
                _paginas = int(_paginas_raw) if _paginas_raw.isdigit() and int(_paginas_raw) > 0 else None
                doc_row = db_query(
                    """INSERT INTO public.datos_archivo
                           (titulo,autor,fecha_documento,tesauro_primario,id_tipo_documento,
                            abstract,ubicacion,updated_by,numero_folio,soporte,numero_paginas)
                       VALUES(%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s) RETURNING id_archivo""",
                    [
                        titulo, row.get("autor", ""), _parse_date(row.get("fecha")),
                        tipo_nombre, tipo_id, row.get("abstract", ""), row.get("ubicacion", ""), requester,
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
                _soporte_r = str(row.get("soporte", "") or "Físico").strip()
                if _soporte_r not in ("Físico", "Digital", "Digitalizado"):
                    _soporte_r = "Físico"
                _paginas_r_raw = str(row.get("numero_paginas", "") or "").strip()
                _paginas_r = int(_paginas_r_raw) if _paginas_r_raw.isdigit() and int(_paginas_r_raw) > 0 else None
                db_query(
                    """INSERT INTO public.datos_rrhh
                           (empleado_id,id_tipo_documento,fecha_documento,notas,ubicacion,updated_by,
                            numero_folio,soporte,numero_paginas)
                       VALUES(%s,%s,%s,%s,%s,%s,%s,%s,%s)""",
                    [emp["id"], tipo_id, _parse_date(row.get("fecha")), row.get("notas", ""), row.get("ubicacion", ""), requester,
                     str(row.get("numero_folio", "") or "").strip() or None, _soporte_r, _paginas_r],
                    fetch="none", commit=True,
                )
                results["inserted"] += 1
        except Exception as e:
            results["errors"].append(f"Fila {i}: {str(e)[:100]}")
    log_event(
        requester, f"Import CSV Docs ({modulo})", modulo,
        f"inserted={results['inserted']} errors={len(results['errors'])}",
    )
    return results
