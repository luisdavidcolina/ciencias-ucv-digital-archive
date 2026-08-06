"""Sub-paquete admin: agrega todos los sub-routers bajo /api/admin."""
from fastapi import APIRouter, Depends

from .deps      import require_session
from .stats     import router as stats_router
from .docs      import router as docs_router
from .import_   import router as import_router
from .misc      import router as misc_router
from .users     import router as users_router
from .retencion import router as retencion_router

router = APIRouter(
    prefix="/api/admin",
    tags=["admin"],
    dependencies=[Depends(require_session)],
)

router.include_router(stats_router)
router.include_router(docs_router)
router.include_router(import_router)
router.include_router(misc_router)
router.include_router(users_router)
router.include_router(retencion_router)
