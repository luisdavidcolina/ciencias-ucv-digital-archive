"""Helpers privados compartidos entre los sub-módulos de admin."""
from fastapi import HTTPException

from database import db_query, log_event, logger, hash_password
from utils import generate_unique_slug
from ..choices import invalidate_choices_cache


def _resolve_or_create_lookup(table: str, nombre: str, default_name: str = "Por Asignar") -> int:
    """Busca o crea un registro en una tabla de catálogo y retorna su id."""
    nombre = (nombre or "").strip() or default_name
    col = "estados" if table == "estados_laborales" else "nombre"
    row = db_query(f"SELECT id FROM public.{table} WHERE {col} = %s", (nombre,), fetch="one")
    if row:
        return row["id"]
    new_row = db_query(
        f"INSERT INTO public.{table} ({col}) VALUES (%s) RETURNING id",
        (nombre,),
        fetch="one",
        commit=True,
    )
    return new_row["id"]


def _resolve_or_create_tipo_documento(nombre: str, cat_slug: str = None) -> int:
    """Busca o crea un tipo de documento y retorna su id."""
    nombre = (nombre or "").strip()
    if not nombre:
        raise HTTPException(status_code=400, detail="doc_type vacío")
    # Busca por nombre o nombre_corto, insensible a mayúsculas
    row = db_query(
        "SELECT id FROM public.tipo_documento WHERE LOWER(nombre) = LOWER(%s) OR LOWER(COALESCE(nombre_corto,'')) = LOWER(%s)",
        (nombre, nombre),
        fetch="one",
    )
    if row:
        return row["id"]
    if cat_slug:
        cat = db_query("SELECT id FROM public.categoria WHERE slug = %s", (cat_slug,), fetch="one")
    else:
        cat = None
    if not cat:
        cat = db_query("SELECT id FROM public.categoria ORDER BY id LIMIT 1", fetch="one")
    if not cat:
        raise HTTPException(status_code=500, detail="No existen categorías en la BD")
    slug = generate_unique_slug(nombre, "tipo_documento")
    new_row = db_query(
        "INSERT INTO public.tipo_documento (nombre, nombre_corto, slug, id_categoria) VALUES (%s, %s, %s, %s) RETURNING id",
        (nombre, nombre, slug, cat["id"]),
        fetch="one",
        commit=True,
    )
    return new_row["id"]


def _resolve_user_id(usuario: str) -> int:
    """Retorna el id del usuario; usa el primer usuario como fallback."""
    row = db_query(
        "SELECT id FROM public.usuarios_sistema WHERE TRIM(usuario) = %s",
        (usuario.strip(),),
        fetch="one",
    )
    if row:
        return row["id"]
    fallback = db_query("SELECT id FROM public.usuarios_sistema ORDER BY id LIMIT 1", fetch="one")
    return fallback["id"] if fallback else 1
