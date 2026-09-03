"""Subida y descarga de archivos digitalizados (Cloudflare R2)."""
import os

from fastapi import APIRouter, Depends, File, Form, HTTPException, Query, UploadFile
from fastapi.responses import RedirectResponse

import storage
from database import log_event
from routes.admin.deps import require_session

router = APIRouter(tags=["files"], dependencies=[Depends(require_session)])


@router.post("/api/admin/upload")
async def upload_document(
    file: UploadFile = File(...),
    modulo: str = Form("archivo"),
    usuario: str = Form(""),  # DG-083: ya no se usa para identidad, solo se ignora
    usuario_sesion: str = Depends(require_session),
):
    """Sube un documento digitalizado a R2 y retorna su file_url interno.

    El file_url devuelto tiene la forma `/api/files/<key>` y puede guardarse
    directamente en las columnas file_url de datos_archivo / datos_rrhh.

    La identidad de quien sube (para auditoría) sale de la sesión verificada
    por `require_session` (cookie/token), nunca del campo `usuario` del
    formulario: ese campo lo controla el cliente y no prueba nada (DG-083).
    """
    if not storage.is_configured():
        raise HTTPException(
            status_code=503,
            detail="Almacenamiento de archivos no configurado (variables R2_* faltantes)",
        )

    ext = os.path.splitext(file.filename or "")[1].lower()
    if ext not in storage.ALLOWED_EXTENSIONS:
        raise HTTPException(
            status_code=400,
            detail=f"Extensión no permitida: {ext or '(sin extensión)'}. "
                   f"Permitidas: {', '.join(sorted(storage.ALLOWED_EXTENSIONS))}",
        )

    # Validación de tamaño (lee en memoria; límite 25 MB)
    contents = await file.read()
    if len(contents) > storage.MAX_FILE_SIZE:
        raise HTTPException(status_code=413, detail="El archivo excede el límite de 25 MB")
    if not contents:
        raise HTTPException(status_code=400, detail="El archivo está vacío")

    key = storage.build_object_key(modulo, file.filename or "documento")
    try:
        import io
        storage.upload_fileobj(
            io.BytesIO(contents), key,
            content_type=file.content_type or "application/octet-stream",
        )
    except Exception as e:
        raise HTTPException(status_code=502, detail=f"Error subiendo a R2: {type(e).__name__}")

    log_event(usuario_sesion, "Upload File", modulo, f"key={key} ({len(contents)} bytes)")
    return {"success": True, "file_url": f"/api/files/{key}", "key": key}


@router.get("/api/files/{key:path}")
def serve_file(key: str, usuario_sesion: str = Depends(require_session), u: str = Query(default="")):
    """Redirige a una URL prefirmada de R2 (válida 1 hora).

    La identidad se prueba con la sesión verificada por `require_session`
    (cookie o token), no con el parámetro `u` de la query, que un cliente
    podría poner a cualquier nombre de usuario (DG-083). `u` se acepta y se
    ignora solo por compatibilidad con enlaces/llamadas existentes.
    Así el bucket permanece privado y los enlaces guardados en la base de
    datos (`/api/files/<key>`) son estables y no expiran.
    """
    if not storage.is_configured():
        raise HTTPException(status_code=503, detail="Almacenamiento no configurado")
    if not key or ".." in key or key.startswith("/"):
        raise HTTPException(status_code=400, detail="Clave inválida")
    try:
        url = storage.presigned_get_url(key)
    except storage.StorageNotFoundError:
        raise HTTPException(status_code=404, detail="Archivo no encontrado")
    except Exception as e:
        raise HTTPException(status_code=502, detail=f"Error al recuperar archivo: {type(e).__name__}")
    return RedirectResponse(url=url, status_code=307)
