import os

from fastapi import APIRouter, Depends, HTTPException, status
from fastapi.responses import FileResponse, RedirectResponse

from database import db_query
from routes.admin.deps import require_session

router = APIRouter(tags=["pages"])

# Resuelve la carpeta static/ relativa a este archivo (routes/ → ../ → static/)
_static = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "static")


def _page(filename: str) -> FileResponse:
    return FileResponse(os.path.join(_static, filename))


@router.get("/", include_in_schema=False)
async def root():
    return RedirectResponse(url="/login")


@router.get("/login", include_in_schema=False)
async def serve_login():
    return _page("login.html")


@router.get("/archivo", include_in_schema=False)
async def serve_archive():
    return _page("archive.html")


@router.get("/rrhh", include_in_schema=False)
async def serve_hr():
    return _page("hr.html")


@router.get("/admin/archivo", include_in_schema=False)
async def serve_admin_archive():
    return _page("admin_archive.html")


@router.get("/admin/rrhh", include_in_schema=False)
async def serve_admin_hr():
    return _page("admin_hr.html")


@router.get("/admin/sistema", include_in_schema=False)
async def serve_admin_system():
    return _page("admin_system.html")


@router.get("/admin/ia", include_in_schema=False)
async def serve_admin_ai():
    return _page("admin_ai.html")


@router.get("/investigacion", include_in_schema=False)
async def serve_investigacion(usuario: str = Depends(require_session)):
    """Informe interno de comparativa competitiva (SI-156). No es publico: expone
    arquitectura, costes y carencias del propio sistema, asi que exige sesion y
    ademas se restringe al rol Global (el mismo criterio que admin/ia)."""
    fila = db_query(
        "SELECT modulo FROM public.usuarios_sistema "
        "WHERE usuario = %s AND COALESCE(is_active, TRUE) LIMIT 1",
        [usuario], fetch="one",
    )
    if not fila or fila["modulo"] != "Global":
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Solo un administrador Global puede ver este informe.",
        )
    return _page("investigacion.html")


@router.get("/ayuda", include_in_schema=False)
async def serve_ayuda():
    return _page("ayuda.html")


@router.get("/compartido/{token}", include_in_schema=False)
async def serve_compartido(token: str):
    """Pagina publica de un documento compartido. El token lo valida la API.

    La ruta acepta cualquier token: la pagina consulta /api/compartido/<token> y
    es esa llamada la que decide. Asi un enlace caducado muestra un mensaje
    entendible en vez de un 404 crudo del servidor.
    """
    return _page("compartido.html")
