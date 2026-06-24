"""Categorías, palabras clave y audit log."""
from fastapi import APIRouter, HTTPException

from database import db_query, log_event
from models import CategoryCreateRequest, KeywordRequest
from utils import generate_unique_slug
from .choices import invalidate_choices_cache

router = APIRouter()


@router.get("/keywords")
def get_keywords():
    rows = db_query(
        """SELECT dl.id_descriptor AS id, dl.nombre,
                  COUNT(DISTINCT ad.id_archivo) AS uso_archivo
           FROM public.descriptores_libres dl
           LEFT JOIN public.archivo_descriptores ad ON ad.id_descriptor = dl.id_descriptor
           GROUP BY dl.id_descriptor, dl.nombre
           ORDER BY dl.nombre""",
        fetch="all",
    ) or []
    return [dict(r) for r in rows]


@router.post("/keywords")
def create_keyword(req: KeywordRequest):
    nombre = (req.nombre or "").strip()
    if not nombre:
        raise HTTPException(400, "Nombre vacío")
    existing = db_query(
        "SELECT id_descriptor FROM public.descriptores_libres WHERE LOWER(nombre) = LOWER(%s)",
        (nombre,), fetch="one",
    )
    if existing:
        raise HTTPException(400, "Palabra clave ya existe")
    row = db_query(
        "INSERT INTO public.descriptores_libres (nombre) VALUES (%s) RETURNING id_descriptor AS id",
        (nombre,), fetch="one", commit=True,
    )
    return {"success": True, "id": row["id"]}


@router.put("/keywords/{kid}")
def update_keyword(kid: int, req: KeywordRequest):
    nombre = (req.nombre or "").strip()
    if not nombre:
        raise HTTPException(400, "Nombre vacío")
    db_query(
        "UPDATE public.descriptores_libres SET nombre = %s WHERE id_descriptor = %s",
        (nombre, kid), fetch="none", commit=True,
    )
    return {"success": True}


@router.delete("/keywords/{kid}")
def delete_keyword(kid: int):
    db_query(
        "DELETE FROM public.descriptores_libres WHERE id_descriptor = %s",
        (kid,), fetch="none", commit=True,
    )
    return {"success": True}


@router.post("/add_category")
def add_category(req: CategoryCreateRequest):
    log_event(req.usuario, "Create Category", req.scope, f"Nueva Tipología: {req.name}")
    nombre = (req.name or "").strip()
    if not nombre:
        raise HTTPException(400, "Nombre vacío")

    if req.parte:
        cat_slug = req.parte
    elif req.scope == "Archivo":
        cat_slug = "archivo"
    else:
        cat_slug = "parte-i"

    cat = db_query("SELECT id FROM public.categoria WHERE slug = %s", (cat_slug,), fetch="one")
    if not cat:
        cat = db_query("SELECT id FROM public.categoria ORDER BY id LIMIT 1", fetch="one")
    if not cat:
        raise HTTPException(500, "No hay categorías en la BD")

    existing = db_query("SELECT id FROM public.tipo_documento WHERE LOWER(nombre) = LOWER(%s)", (nombre,), fetch="one")
    if existing:
        return {"success": True, "detail": "Ya existe"}

    slug = generate_unique_slug(nombre, "tipo_documento")
    db_query(
        "INSERT INTO public.tipo_documento (nombre, nombre_corto, slug, id_categoria) VALUES (%s, %s, %s, %s)",
        (nombre, nombre, slug, cat["id"]),
        fetch="none", commit=True,
    )
    invalidate_choices_cache()
    return {"success": True}


@router.get("/audit_log")
def get_audit_log(page: int = 1, per_page: int = 50, search: str = ""):
    """Retorna el log de auditoría con paginación y búsqueda opcional."""
    page     = max(1, page)
    per_page = max(1, min(per_page, 100))
    offset   = (page - 1) * per_page

    conditions, params = [], []
    if search:
        conditions.append(
            "(unaccent(accion) ILIKE unaccent(%s) OR unaccent(usuario) ILIKE unaccent(%s))"
        )
        params.extend([f"%{search}%", f"%{search}%"])

    where = ("WHERE " + " AND ".join(conditions)) if conditions else ""

    count_row = db_query(
        f"SELECT COUNT(*) AS total FROM public.audit_log {where}",
        params or None, fetch="one",
    )
    total = int(count_row["total"]) if count_row else 0

    rows = db_query(
        f"""SELECT id, usuario, accion AS evento, modulo, detalle, status AS resultado,
                   TO_CHAR(timestamp, 'YYYY-MM-DD HH24:MI:SS') AS timestamp
            FROM public.audit_log
            {where}
            ORDER BY timestamp DESC
            LIMIT %s OFFSET %s""",
        (params + [per_page, offset]) if params else [per_page, offset],
        fetch="all",
    ) or []

    return {"total": total, "page": page, "per_page": per_page, "records": [dict(r) for r in rows]}
