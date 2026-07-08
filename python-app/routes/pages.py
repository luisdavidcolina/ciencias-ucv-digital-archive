import os

from fastapi import APIRouter
from fastapi.responses import FileResponse, RedirectResponse

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
async def serve_archivo():
    return _page("archivo.html")


@router.get("/rrhh", include_in_schema=False)
async def serve_rrhh():
    return _page("rrhh.html")


@router.get("/admin/archivo", include_in_schema=False)
async def serve_admin_archivo():
    return _page("admin_archivo.html")


@router.get("/admin/rrhh", include_in_schema=False)
async def serve_admin_rrhh():
    return _page("admin_rrhh.html")


@router.get("/admin/sistema", include_in_schema=False)
async def serve_admin_sistema():
    return _page("admin_sistema.html")


@router.get("/investigacion", include_in_schema=False)
async def serve_investigacion():
    return _page("investigacion.html")


@router.get("/ayuda", include_in_schema=False)
async def serve_ayuda():
    return _page("ayuda.html")
