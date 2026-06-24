"""Thin orchestrator: agrega todos los sub-routers de admin bajo /api/admin."""
from fastapi import APIRouter

from .admin_stats  import router as stats_router
from .admin_docs   import router as docs_router
from .admin_import import router as import_router
from .admin_misc   import router as misc_router
from .admin_users  import router as users_router

router = APIRouter(prefix="/api/admin", tags=["admin"])

router.include_router(stats_router)
router.include_router(docs_router)
router.include_router(import_router)
router.include_router(misc_router)
router.include_router(users_router)
