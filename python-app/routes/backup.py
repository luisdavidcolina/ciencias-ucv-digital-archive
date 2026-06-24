"""
Backup y restauración de datos del sistema.
Solo accesible para el administrador máximo (Global).
"""
from fastapi import APIRouter, HTTPException, UploadFile, File, Query
from fastapi.responses import StreamingResponse
from database import db_query
import json
import io
from datetime import datetime, date

router = APIRouter()

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


def _serialize_value(v):
    """Convierte tipos no-JSON a string."""
    if isinstance(v, (date, datetime)):
        return v.isoformat()
    return v


@router.get("/export")
def export_backup(requester: str = Query(default="")):
    """
    Exporta todas las tablas como un JSON descargable.
    Puede tardar varios segundos en bases de datos grandes.
    """
    backup = {
        "_metadata": {
            "created_at": datetime.utcnow().isoformat(),
            "version": "1.0",
            "tables": EXPORTABLE_TABLES,
        }
    }
    total_rows = 0
    for table in EXPORTABLE_TABLES:
        try:
            rows = db_query(f"SELECT * FROM public.{table} ORDER BY 1", fetch="all")
            serialized = []
            for row in (rows or []):
                serialized.append({k: _serialize_value(v) for k, v in dict(row).items()})
            backup[table] = serialized
            total_rows += len(serialized)
        except Exception as e:
            backup[table] = []
            backup[f"_error_{table}"] = str(e)

    # Registrar en historial
    try:
        db_query(
            """INSERT INTO public.backup_history(usuario, tipo, tabla_count, total_rows, notas)
               VALUES(%s, 'export', %s, %s, 'Export completo via UI')""",
            (requester or "sistema", len(EXPORTABLE_TABLES), total_rows),
            fetch="none", commit=True,
        )
    except Exception:
        pass  # no bloquear si la tabla no existe aún

    json_str = json.dumps(backup, ensure_ascii=False, indent=2)
    filename = f"backup_ciencias_ucv_{datetime.utcnow().strftime('%Y%m%d_%H%M%S')}.json"
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
                cols = list(row.keys())
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
    page = max(1, page)
    per_page = max(1, min(per_page, 100))
    offset = (page - 1) * per_page

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
