from fastapi import APIRouter, Depends, Query

from routes.admin.deps import require_session
from core.cache import TTLCache
from repos.lookups_repo import (
    user_modules as _user_modules,
    build_choices as _build_choices,
)

router = APIRouter(tags=["choices"])

_cache = TTLCache(ttl_seconds=300)
_CACHE_KEY = "choices"


def invalidate_choices_cache() -> None:
    _cache.invalidate(_CACHE_KEY)


@router.get("/api/choices")
def get_choices(
    scope: str | None = Query(default=None),
    usuario: str = Depends(require_session),
):
    """Opciones disponibles para los filtros del UI, cacheadas 5 min y
    segmentadas por los módulos del usuario autenticado (BA-101, BA-145)."""
    modules = _user_modules(usuario)

    all_choices = _cache.get(_CACHE_KEY)
    if all_choices is None:
        all_choices = _build_choices()
        _cache.set(_CACHE_KEY, all_choices)

    allowed_slugs: set = set()
    if (not scope or scope == "archivo") and "Archivo" in modules:
        allowed_slugs.add("archivo")
    if (not scope or scope == "rrhh") and "RRHH" in modules:
        allowed_slugs.update({"parte-i", "parte-ii", "parte-iii", "parte-iv"})

    result: dict = {
        "catalogo": {
            "retencion": [
                r for r in all_choices["catalogo"]["retencion"]
                if r.get("cat_slug") in allowed_slugs
            ]
        }
    }
    if (not scope or scope == "archivo") and "Archivo" in modules:
        result["archivo"] = all_choices["archivo"]
    if (not scope or scope == "rrhh") and "RRHH" in modules:
        result["rrhh"] = all_choices["rrhh"]
    return result
