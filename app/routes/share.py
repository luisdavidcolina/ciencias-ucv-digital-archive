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
- **Nada fuera de `status = 'aprobado'`.** Un documento que pasa a borrador,
  revisión o rechazado deja de ser visible por el enlace, igual que uno
  enviado a la papelera (IN-147): la comprobación se repite en cada consulta,
  no sólo al crear el enlace.
"""
import base64
from datetime import datetime, timezone

from fastapi import APIRouter, Depends, HTTPException, Query
from fastapi.responses import RedirectResponse

import storage
from core.security import generate_share_token, share_token_jti, verify_share_token
from database import db_query, log_event
from routes.admin.deps import require_admin_role, require_session

router = APIRouter(tags=["share"])

MAX_HORAS = 24 * 30          # un mes: más allá, que se genere uno nuevo
_TABLAS = {
    "Archivo": ("datos_archivo", "id_archivo"),
    "RRHH": ("datos_rrhh", "id_rrhh"),
}


def _token_expira_iso(token: str) -> str | None:
    """Extrae la caducidad (VI-072) de un token que `verify_share_token` ya
    validó: firma y formato correctos, así que basta con leer el payload de
    nuevo sin repetir la verificación HMAC."""
    try:
        relleno = "=" * (-len(token) % 4)
        raw = base64.urlsafe_b64decode((token + relleno).encode()).decode()
        _, _, expira, _ = raw.rsplit(":", 3)
        return datetime.fromtimestamp(int(expira), tz=timezone.utc).isoformat()
    except Exception:
        return None


def _leer_documento(modulo: str, doc_id: int) -> dict | None:
    tabla, pk = _TABLAS[modulo]
    return db_query(
        f"""SELECT d.{pk} AS id, d.titulo, d.autor, d.file_url, d.ubicacion,
                   TO_CHAR(d.fecha_documento, 'YYYY-MM-DD') AS fecha,
                   COALESCE(td.nombre_corto, td.nombre, '') AS tipo,
                   COALESCE(d.soporte, 'Físico')            AS soporte
            FROM public.{tabla} d
            LEFT JOIN public.tipo_documento td ON d.id_tipo_documento = td.id
            WHERE d.{pk} = %s AND d.deleted_at IS NULL
              AND COALESCE(d.status, 'aprobado') = 'aprobado'""",
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
    jti = share_token_jti(token)
    log_event(usuario or "sistema", "Crear Enlace Externo", modulo,
              f"doc_id={doc_id}, caduca en {horas}h, jti={jti}")
    return {"token": token, "url": f"/compartido/{token}", "horas": horas, "jti": jti}


@router.get("/api/admin/compartir/revocados",
            dependencies=[Depends(require_admin_role("Archivo", "RRHH"))])
def listar_revocados():
    """Enlaces revocados hasta ahora (IN-146). El diseño es stateless: no hay
    tabla de enlaces EMITIDOS —sólo de revocados—, así que esto no es un
    listado de "todo enlace activo que exista en el mundo" (eso exigiría
    volver a un diseño con estado), sino de lo que un administrador ya decidió
    apagar. El `jti` de un enlace recién creado se ve en `crear_enlace` (queda
    también en auditoría, "Crear Enlace Externo")."""
    filas = db_query(
        "SELECT jti, motivo, creado_en FROM public.enlaces_revocados "
        "ORDER BY creado_en DESC",
        fetch="all",
    )
    return {"revocados": filas}


@router.post("/api/admin/compartir/revocar",
             dependencies=[Depends(require_admin_role("Archivo", "RRHH"))])
def revocar_enlace(
    jti: str = Query(..., min_length=1, max_length=64),
    motivo: str = Query(default=""),
    usuario: str = Query(default=""),
):
    """Apaga un enlace por su identificador único (IN-146), sin tocar los
    demás y sin rotar SECRET_KEY —que cerraría también todas las sesiones—.
    Idempotente: revocar dos veces el mismo jti no es un error."""
    db_query(
        "INSERT INTO public.enlaces_revocados (jti, motivo) VALUES (%s, %s) "
        "ON CONFLICT (jti) DO NOTHING",
        [jti, motivo or None], fetch="none", commit=True,
    )
    log_event(usuario or "sistema", "Revocar Enlace Externo", "Sistema", f"jti={jti}")
    return {"revocado": True, "jti": jti}


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
    return {"modulo": modulo, "documento": fila, "caduca": _token_expira_iso(token)}


@router.get("/api/compartido/{token}/archivo")
def descargar_compartido(token: str):
    """Redirige al archivo digitalizado, si el enlace sigue vigente."""
    datos = verify_share_token(token)
    if not datos:
        raise HTTPException(404, "El enlace no es válido o ya caducó")
    modulo, doc_id = datos

    doc = _leer_documento(modulo, doc_id)
    file_url = doc.get("file_url") if doc else None
    if not file_url:
        raise HTTPException(404, "El documento no tiene archivo digitalizado")

    log_event("enlace-externo", "Descarga por Enlace", modulo, f"doc_id={doc_id}")

    # `file_url` normalmente es "/api/files/<key>", una ruta que ahora exige
    # sesión (ver files.py, DG-083) — inservible para quien abre este enlace
    # sin sesión. Se genera aquí, directo contra R2, la URL prefirmada que
    # antes daba esa ruta, sin exponer el `file_url` interno ni pasar por
    # una ruta que requiera autenticación.
    prefix = "/api/files/"
    if file_url.startswith(prefix):
        if not storage.is_configured():
            raise HTTPException(503, "Almacenamiento no configurado")
        key = file_url[len(prefix):]
        try:
            url = storage.presigned_get_url(key)
        except storage.StorageNotFoundError:
            raise HTTPException(404, "El archivo ya no está disponible")
        except Exception as e:
            raise HTTPException(502, f"Error al recuperar archivo: {type(e).__name__}")
        return RedirectResponse(url=url, status_code=307)

    # Caso residual: file_url apunta a una URL externa ya pública.
    return RedirectResponse(url=file_url, status_code=307)
