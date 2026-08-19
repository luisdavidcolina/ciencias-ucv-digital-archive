"""
Backup y restauración de datos del sistema.
Solo accesible para el administrador máximo (Global).
"""
from fastapi import APIRouter, Depends, File, HTTPException, Header, Query, UploadFile
from fastapi.responses import StreamingResponse
import storage
from database import db_query, logger
import json
import io
import os
import re
from utils import paginate
from datetime import datetime, date
from typing import Optional

_SAFE_IDENTIFIER = re.compile(r'^[a-z_][a-z0-9_]{0,62}$')

from routes.admin.deps import require_session

router = APIRouter(dependencies=[Depends(require_session)])

# El backup programado no puede exigir cookie de sesión: lo llama Vercel Cron,
# no un navegador. Va aparte y se autentica con CRON_SECRET.
router_cron = APIRouter()

# Tablas exportables, en orden de dependencias (sin FK issues)
EXPORTABLE_TABLES = [
    # Catálogos (sin FK)
    "categoria",
    "cargos",
    "departamentos",
    "estados_laborales",
    "tipo_documento",
    "descriptores_libres",
    # Personal
    "empleados",
    "historial_cargos",
    # Documentos RRHH
    "datos_rrhh",
    "rrhh_descriptores",
    # Documentos Archivo
    "datos_archivo",
    "archivo_descriptores",
    # Sistema
    "usuarios_sistema",
]

# Grupos para la UI de selección
TABLE_GROUPS = {
    "catalogos":    {"label": "Catálogos", "tables": ["categoria", "cargos", "departamentos", "estados_laborales", "tipo_documento", "descriptores_libres"]},
    "personal":     {"label": "Personal RRHH", "tables": ["empleados", "historial_cargos"]},
    "docs_rrhh":    {"label": "Documentos RRHH", "tables": ["datos_rrhh", "rrhh_descriptores"]},
    "docs_archivo": {"label": "Documentos Archivo", "tables": ["datos_archivo", "archivo_descriptores"]},
    "sistema":      {"label": "Sistema", "tables": ["usuarios_sistema"]},
}


def _serialize_value(v):
    """Convierte tipos no-JSON a string."""
    if isinstance(v, (date, datetime)):
        return v.isoformat()
    return v


def _construir_backup(selected: list) -> tuple[dict, int]:
    """Serializa las tablas indicadas. Devuelve (payload, filas totales)."""
    backup = {
        "_metadata": {
            "created_at": datetime.utcnow().isoformat(),
            "version": "1.1",
            "tables": selected,
            "partial": len(selected) < len(EXPORTABLE_TABLES),
        }
    }
    total_rows = 0
    for table in selected:
        try:
            rows = db_query(f"SELECT * FROM public.{table} ORDER BY 1", fetch="all")
            serialized = [
                {k: _serialize_value(v) for k, v in dict(row).items()}
                for row in (rows or [])
            ]
            backup[table] = serialized
            total_rows += len(serialized)
        except Exception as e:
            backup[table] = []
            backup[f"_error_{table}"] = str(e)
    return backup, total_rows


@router.get("/groups")
def get_table_groups():
    """Devuelve los grupos de tablas disponibles para selección en el export."""
    return TABLE_GROUPS


@router.get("/export")
def export_backup(
    requester: str = Query(default=""),
    tables: Optional[str] = Query(default=None, description="Tablas separadas por coma; omitir = todas"),
):
    """
    Exporta tablas seleccionadas como JSON descargable.
    El parámetro `tables` acepta nombres separados por coma; si se omite se exporta todo.
    Solo se permiten tablas del conjunto EXPORTABLE_TABLES.
    """
    if tables:
        requested = [t.strip() for t in tables.split(",") if t.strip()]
        # whitelist estricta — nunca ejecutar tabla arbitraria
        selected = [t for t in EXPORTABLE_TABLES if t in requested]
        if not selected:
            raise HTTPException(400, "Ninguna tabla válida seleccionada.")
    else:
        selected = EXPORTABLE_TABLES

    backup, total_rows = _construir_backup(selected)

    notas = f"Export {'parcial' if len(selected) < len(EXPORTABLE_TABLES) else 'completo'} via UI ({len(selected)} tablas)"
    try:
        db_query(
            """INSERT INTO public.backup_history(usuario, tipo, tabla_count, total_rows, notas)
               VALUES(%s, 'export', %s, %s, %s)""",
            (requester or "sistema", len(selected), total_rows, notas),
            fetch="none", commit=True,
        )
    except Exception:
        pass

    json_str = json.dumps(backup, ensure_ascii=False, indent=2)
    suffix = "parcial" if len(selected) < len(EXPORTABLE_TABLES) else "completo"
    filename = f"backup_{suffix}_ciencias_ucv_{datetime.utcnow().strftime('%Y%m%d_%H%M%S')}.json"
    return StreamingResponse(
        io.BytesIO(json_str.encode("utf-8")),
        media_type="application/json",
        headers={"Content-Disposition": f'attachment; filename="{filename}"'},
    )


@router.post("/restore")
async def restore_backup(
    file: UploadFile = File(...),
    requester: str = Query(default=""),
    mode: str = Query(default="merge"),
):
    """
    Restaura datos desde un JSON de backup.
    mode='merge': INSERT ON CONFLICT DO NOTHING (seguro, no borra datos existentes)
    mode='overwrite': DELETE + INSERT (peligroso, SOLO usar para restauración completa)
    """
    if mode not in ("merge", "overwrite"):
        raise HTTPException(400, "mode debe ser 'merge' o 'overwrite'")

    content = await file.read()
    try:
        backup = json.loads(content.decode("utf-8"))
    except Exception:
        raise HTTPException(400, "Archivo JSON inválido o corrupto.")

    if "_metadata" not in backup:
        raise HTTPException(400, "El archivo no es un backup válido del sistema.")

    results = {}
    errors = []

    for table in EXPORTABLE_TABLES:
        rows = backup.get(table, [])
        if not rows:
            results[table] = {"inserted": 0, "skipped": "sin datos"}
            continue
        try:
            if mode == "overwrite" and table not in ("usuarios_sistema",):
                # No borramos usuarios para evitar quedar sin acceso
                db_query(f"DELETE FROM public.{table}", fetch="none", commit=True)

            inserted = 0
            for row in rows:
                cols = [c for c in row.keys() if _SAFE_IDENTIFIER.match(c)]
                if not cols:
                    continue
                vals = [row[c] for c in cols]
                placeholders = ", ".join(["%s"] * len(cols))
                col_names = ", ".join(cols)
                conflict = "ON CONFLICT DO NOTHING" if mode == "merge" else ""
                sql = f"INSERT INTO public.{table} ({col_names}) VALUES ({placeholders}) {conflict}"
                try:
                    db_query(sql, vals, fetch="none", commit=True)
                    inserted += 1
                except Exception as row_err:
                    errors.append(f"{table}: {row_err}")
            results[table] = {"inserted": inserted, "total": len(rows)}
        except Exception as e:
            results[table] = {"error": str(e)}
            errors.append(f"{table}: {e}")

    # Registrar
    try:
        db_query(
            """INSERT INTO public.backup_history(usuario, tipo, tabla_count, total_rows, notas)
               VALUES(%s, 'restore', %s, %s, %s)""",
            (requester or "sistema", len(EXPORTABLE_TABLES),
             sum(r.get("inserted", 0) for r in results.values() if isinstance(r, dict)),
             f"mode={mode}, errors={len(errors)}"),
            fetch="none", commit=True,
        )
    except Exception:
        pass

    return {
        "success": len(errors) == 0,
        "results": results,
        "errors": errors[:20],  # max 20 errores en respuesta
    }


@router.get("/history")
def get_backup_history(page: int = 1, per_page: int = 20):
    """Historial de backups realizados."""
    page, per_page, offset = paginate(page, per_page)

    count_row = db_query("SELECT COUNT(*) AS total FROM public.backup_history", fetch="one")
    total = int(count_row["total"]) if count_row else 0

    rows = db_query(
        """SELECT id, usuario, tipo, tabla_count, total_rows, notas,
                  TO_CHAR(created_at, 'YYYY-MM-DD HH24:MI:SS') AS created_at
           FROM public.backup_history
           ORDER BY created_at DESC
           LIMIT %s OFFSET %s""",
        [per_page, offset], fetch="all",
    ) or []

    return {"total": total, "page": page, "per_page": per_page, "records": [dict(r) for r in rows]}


# =============================================================================
# BACKUP PROGRAMADO
# =============================================================================
# Hasta ahora la unica copia era la que alguien se acordara de descargar a mano
# desde el panel. Si se pierde la base no hay nada: el export no se guardaba en
# ningun sitio, se enviaba al navegador y ya.
#
# Este endpoint lo dispara Vercel Cron y deja el JSON en R2, que es el mismo
# almacenamiento donde ya viven los digitalizados. Es de solo lectura sobre la
# base: no modifica nada, solo copia.

@router_cron.get("/programado")
def backup_programado(
    authorization: str = Header(default=""),
    x_vercel_cron: str = Header(default=""),
):
    """Copia completa a R2. Pensado para ejecutarse por cron, no a mano.

    Se protege con CRON_SECRET: sin el, este endpoint seria una descarga
    completa de la base abierta a cualquiera. Si la variable no esta definida se
    rechaza — fallar cerrado es lo correcto aqui.
    """
    esperado = os.environ.get("CRON_SECRET", "")
    if not esperado:
        raise HTTPException(
            503, "CRON_SECRET no está configurado; el backup programado está deshabilitado.")
    if authorization != f"Bearer {esperado}":
        raise HTTPException(401, "No autorizado")

    if not storage.is_configured():
        raise HTTPException(503, "Almacenamiento R2 sin configurar; no hay dónde guardar la copia.")

    backup, total_rows = _construir_backup(EXPORTABLE_TABLES)
    crudo = json.dumps(backup, ensure_ascii=False).encode("utf-8")
    clave = f"backups/{datetime.utcnow().strftime('%Y/%m/%d-%H%M%S')}-completo.json"

    try:
        storage.upload_fileobj(io.BytesIO(crudo), clave, content_type="application/json")
    except Exception as e:
        logger.error("Backup programado: fallo al subir a R2: %s", e)
        _registrar_backup("cron", len(EXPORTABLE_TABLES), total_rows,
                          f"FALLO al subir: {str(e)[:120]}")
        raise HTTPException(502, "No se pudo guardar la copia en el almacenamiento.")

    tam_kb = round(len(crudo) / 1024, 1)
    _registrar_backup("cron", len(EXPORTABLE_TABLES), total_rows,
                      f"Copia automática en R2 ({clave}, {tam_kb} KB)")
    logger.info("Backup programado OK: %s (%s filas, %s KB)", clave, total_rows, tam_kb)
    return {"ok": True, "clave": clave, "tablas": len(EXPORTABLE_TABLES),
            "filas": total_rows, "kb": tam_kb}


def _registrar_backup(usuario: str, tablas: int, filas: int, notas: str) -> None:
    """Deja constancia en backup_history. Nunca debe tumbar el backup."""
    try:
        db_query(
            """INSERT INTO public.backup_history(usuario, tipo, tabla_count, total_rows, notas)
               VALUES(%s, 'export', %s, %s, %s)""",
            (usuario, tablas, filas, notas), fetch="none", commit=True,
        )
    except Exception as e:
        logger.warning("No se pudo registrar el backup en el historial: %s", e)
