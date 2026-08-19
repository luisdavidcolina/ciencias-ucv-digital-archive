"""
Compartición externa de documentos mediante enlaces firmados y con caducidad.

La comparativa de mercado marcaba esta función como ausente frente a Alfresco,
SharePoint y Nextcloud: para enseñarle un documento a alguien de fuera había que
crearle un usuario o mandarle el archivo por correo, que es justo lo que un
archivo institucional no debe hacer.

Decisiones:

- **Sin tabla.** El enlace es un token HMAC firmado que lleva dentro el módulo,
  el id y la caducidad. No hay estado que mantener ni limpiar, y no se puede
  falsificar sin la SECRET_KEY.
- **Solo lectura, y sólo del documento firmado.** El token no da acceso a la
  búsqueda ni a otros registros.
- **Se registra en auditoría.** Tanto la creación del enlace como cada consulta:
  compartir hacia fuera es exactamente lo que un archivo necesita poder rastrear.
- **Nada de documentos en papelera.** Un documento borrado deja de ser visible
  aunque el enlace siga vigente.
"""
from fastapi import APIRouter, Depends, HTTPException, Query
from fastapi.responses import RedirectResponse

from core.security import generate_share_token, verify_share_token
from database import db_query, log_event
from routes.admin.deps import require_session

router = APIRouter(tags=["share"])

MAX_HORAS = 24 * 30          # un mes: más allá, que se genere uno nuevo
_TABLAS = {
    "Archivo": ("datos_archivo", "id_archivo"),
    "RRHH": ("datos_rrhh", "id_rrhh"),
}


def _leer_documento(modulo: str, doc_id: int) -> dict | None:
    tabla, pk = _TABLAS[modulo]
    return db_query(
        f"""SELECT d.{pk} AS id, d.titulo, d.autor, d.file_url, d.ubicacion,
                   TO_CHAR(d.fecha_documento, 'YYYY-MM-DD') AS fecha,
                   COALESCE(td.nombre_corto, td.nombre, '') AS tipo,
                   COALESCE(d.soporte, 'Físico')            AS soporte
            FROM public.{tabla} d
            LEFT JOIN public.tipo_documento td ON d.id_tipo_documento = td.id
            WHERE d.{pk} = %s AND d.deleted_at IS NULL""",
        [doc_id], fetch="one",
    )


@router.post("/api/admin/compartir", dependencies=[Depends(require_session)])
def crear_enlace(
    modulo: str = Query(...),
    doc_id: int = Query(...),
    horas: int = Query(default=72, ge=1, le=MAX_HORAS),
    usuario: str = Query(default=""),
):
    """Genera un enlace de consulta con caducidad para un documento."""
    if modulo not in _TABLAS:
        raise HTTPException(400, "modulo debe ser 'Archivo' o 'RRHH'")
    if not _leer_documento(modulo, doc_id):
        raise HTTPException(404, "El documento no existe o está en la papelera")

    token = generate_share_token(modulo, doc_id, horas)
    log_event(usuario or "sistema", "Crear Enlace Externo", modulo,
              f"doc_id={doc_id}, caduca en {horas}h")
    return {"token": token, "url": f"/compartido/{token}", "horas": horas}


@router.get("/api/compartido/{token}")
def leer_compartido(token: str):
    """Datos del documento detrás de un enlace. No requiere sesión."""
    datos = verify_share_token(token)
    if not datos:
        raise HTTPException(404, "El enlace no es válido o ya caducó")
    modulo, doc_id = datos

    doc = _leer_documento(modulo, doc_id)
    if not doc:
        raise HTTPException(404, "El documento ya no está disponible")

    log_event("enlace-externo", "Consulta por Enlace", modulo, f"doc_id={doc_id}")
    fila = dict(doc)
    # El file_url interno no se expone: se sirve por la ruta del propio enlace,
    # para que el token siga siendo la única llave.
    fila["tiene_archivo"] = bool(fila.pop("file_url", None))
    return {"modulo": modulo, "documento": fila}


@router.get("/api/compartido/{token}/archivo")
def descargar_compartido(token: str):
    """Redirige al archivo digitalizado, si el enlace sigue vigente."""
    datos = verify_share_token(token)
    if not datos:
        raise HTTPException(404, "El enlace no es válido o ya caducó")
    modulo, doc_id = datos

    doc = _leer_documento(modulo, doc_id)
    if not doc or not doc.get("file_url"):
        raise HTTPException(404, "El documento no tiene archivo digitalizado")

    log_event("enlace-externo", "Descarga por Enlace", modulo, f"doc_id={doc_id}")
    return RedirectResponse(url=doc["file_url"], status_code=307)
