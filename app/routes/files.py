"""Subida y descarga de archivos digitalizados (Cloudflare R2)."""
import os

from fastapi import APIRouter, Depends, File, Form, HTTPException, Query, UploadFile
from fastapi.responses import RedirectResponse

import storage
from database import db_query, log_event
from routes.admin.deps import require_session

router = APIRouter(tags=["files"], dependencies=[Depends(require_session)])


def _modulo_permite_clave(usuario: str, key: str) -> bool:
    """OA-042: la clave lleva el módulo como primer segmento
    (`storage.build_object_key`: `<modulo>/<año>/<uuid>-<nombre>`), así que
    esto es lo mínimo para que el propio nombre de archivo no sea la única
    barrera entre los expedientes de RRHH y quien sólo tiene sesión en
    Archivo (o viceversa).

    `require_session` sólo prueba que la sesión es legible, no a qué módulo
    pertenece — la comprobación de módulo/rol contra `usuarios_sistema` es la
    misma consulta que ya hace `routes/admin/deps.py::_fila_usuario`,
    repetida aquí en vez de importarla porque es un símbolo privado de ese
    módulo (guion bajo) pensado para las dependencias de FastAPI, no para
    llamarse a mano con una clave arbitraria.
    """
    fila = db_query(
        "SELECT modulo, is_active FROM public.usuarios_sistema WHERE usuario = %s LIMIT 1",
        [usuario], fetch="one",
    )
    if not fila or not fila.get("is_active", True):
        return False
    modulo_usuario = fila.get("modulo")
    if modulo_usuario == "Global":
        return True
    modulo_clave = (key.split("/", 1)[0] or "").lower()
    if modulo_clave == "archivo":
        return modulo_usuario == "Archivo"
    if modulo_clave == "rrhh":
        return modulo_usuario == "RRHH"
    # Clave con prefijo desconocido (subida antigua, o fuera del patrón
    # `build_object_key`): no se puede decidir por módulo, así que no se
    # bloquea aquí lo que ya dejaba pasar `require_session` — falla cerrado
    # sería romper enlaces válidos sin ninguna evidencia de que sean ajenos.
    return True


def _documento_en_papelera(key: str) -> bool:
    """True si `key` es el `file_url` vigente de un documento en la papelera.

    Mismo problema que ya resolvió `share.py` para los enlaces externos (ver
    su docstring, IN-147): un documento enviado a la papelera sigue teniendo
    su objeto en R2 -no se purga hasta que alguien lo purgue de verdad-, y
    hasta ahora esta ruta no consultaba `datos_archivo`/`datos_rrhh` en
    absoluto, así que cualquier sesión con módulo correcto podía seguir
    descargando el archivo de un documento ya "borrado" con el enlace viejo.
    Sólo mira el `file_url` **vigente** (no versiones históricas en
    `documento_versiones`, que son de uso administrativo y no se listan por
    esta ruta): si la clave no es el `file_url` actual de ningún documento
    -versión antigua, foto de empleado, clave huérfana- no se puede decidir
    nada aquí y se deja pasar, igual que hace `_modulo_permite_clave` con un
    prefijo desconocido.
    """
    url = f"/api/files/{key}"
    fila = db_query(
        "SELECT 1 AS x FROM public.datos_archivo WHERE file_url = %s AND deleted_at IS NOT NULL "
        "UNION ALL "
        "SELECT 1 AS x FROM public.datos_rrhh WHERE file_url = %s AND deleted_at IS NOT NULL "
        "LIMIT 1",
        [url, url], fetch="one",
    )
    return fila is not None


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

    OA-042: además de probar identidad, se comprueba que la clave pertenezca
    a un módulo al que esa persona tiene acceso — antes cualquier sesión
    válida (Archivo, RRHH o Global) podía leer cualquier archivo, y el nombre
    de usuario que antes "protegía" el enlace ni siquiera es secreto: sale en
    la barra superior, en la tabla de Acceso y en la propia auditoría.
    """
    if not storage.is_configured():
        raise HTTPException(status_code=503, detail="Almacenamiento no configurado")
    if not key or ".." in key or key.startswith("/"):
        raise HTTPException(status_code=400, detail="Clave inválida")
    if not _modulo_permite_clave(usuario_sesion, key):
        raise HTTPException(status_code=403, detail="Sin acceso a este archivo")
    if _documento_en_papelera(key):
        # Mismo mensaje genérico que share.py (404, no 403): no confirma si
        # la clave existe, sólo que no se puede servir ahora mismo.
        raise HTTPException(status_code=404, detail="Archivo no encontrado")
    try:
        url = storage.presigned_get_url(key)
    except storage.StorageNotFoundError:
        raise HTTPException(status_code=404, detail="Archivo no encontrado")
    except Exception as e:
        raise HTTPException(status_code=502, detail=f"Error al recuperar archivo: {type(e).__name__}")
    # OA-047 (parcial, sólo la mitad que vive en este archivo): registra
    # quién descarga qué, no sólo quién sube. `log_event` inserta en segundo
    # plano y falla en silencio, así que no añade latencia ni puede tumbar
    # la descarga si la auditoría falla.
    _modulo_clave = (key.split("/", 1)[0] or "").lower()
    _modulo_log = {"archivo": "Archivo", "rrhh": "RRHH"}.get(_modulo_clave, "Sistema")
    log_event(usuario_sesion, "Descargar Archivo", _modulo_log, f"key={key}")
    return RedirectResponse(url=url, status_code=307)
