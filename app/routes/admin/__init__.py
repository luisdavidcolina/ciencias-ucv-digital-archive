"""Sub-paquete admin: agrega todos los sub-routers bajo /api/admin."""
from fastapi import APIRouter, Depends

from .deps      import require_session
from .stats     import router as stats_router
from .docs      import router as docs_router
from .imports   import router as imports_router
from .catalog   import router as catalog_router
from .users     import router as users_router
from .retention import router as retention_router

router = APIRouter(
    prefix="/api/admin",
    tags=["admin"],
    dependencies=[Depends(require_session)],
)

router.include_router(stats_router)
router.include_router(docs_router)
router.include_router(imports_router)
router.include_router(catalog_router)
router.include_router(users_router)
router.include_router(retention_router)
