"""Utilidades compartidas para el proyecto Ciencias UCV Digital Archive."""

import re
import unicodedata
from typing import List


def split_terms(val_str: str) -> List[str]:
    """Divide una cadena separada por ';' en términos limpios.

    IN-058: vivía en `database.py` sin motivo — partir una cadena no toca la
    base. Se define aquí, antes del `from database import ...` de más abajo,
    para que la resolución sea correcta sin importar cuál de los dos módulos
    se importe primero (`database.py` re-exporta esta misma función).
    """
    if not val_str:
        return []
    return [t.strip() for t in str(val_str).split(";") if t.strip()]


def paginate(page: int, per_page: int, max_per_page: int = 100, max_offset: int = 10000) -> tuple[int, int, int]:
    """Clamp pagination params and return (page, per_page, offset).

    `offset` queda topado en `max_offset` (BA-169): sin tope, una página muy
    alta obliga a Postgres a recorrer y descartar millones de filas.
    """
    page = max(1, page)
    per_page = max(1, min(per_page, max_per_page))
    offset = min((page - 1) * per_page, max_offset)
    return page, per_page, offset

from database import db_query, logger


# =============================================================================
# GENERACIÓN DE SLUGS LIMPIOS
# =============================================================================

def generate_slug(text: str, max_length: int = 250) -> str:
    """Genera un slug limpio a partir de un texto.

    Reglas aplicadas:
    - Normalización Unicode (NFD) y eliminación de marcas diacríticas.
    - Conversión a minúsculas.
    - Reemplazo de caracteres no alfanuméricos por guiones.
    - Colapso de guiones múltiples consecutivos en uno solo.
    - Eliminación de guiones al inicio y al final.
    - Truncado al largo máximo sin cortar a mitad de palabra.
    """
    if not text:
        return ""

    # Quitar acentos: NFD descompone, luego se eliminan las marcas combinatorias
    normalized = unicodedata.normalize("NFD", text)
    without_accents = "".join(
        ch for ch in normalized if unicodedata.category(ch) != "Mn"
    )

    # Minúsculas
    lower = without_accents.lower()

    # Reemplazar todo lo que no sea letra, dígito o espacio por espacio
    cleaned = re.sub(r"[^a-z0-9\s]", " ", lower)

    # Reemplazar espacios (uno o más) por un solo guion
    slug = re.sub(r"\s+", "-", cleaned.strip())

    # Colapsar guiones múltiples por seguridad
    slug = re.sub(r"-{2,}", "-", slug)

    # Eliminar guiones al inicio y al final
    slug = slug.strip("-")

    # Truncar sin cortar palabras
    if len(slug) > max_length:
        slug = slug[:max_length].rsplit("-", 1)[0]

    return slug


def generate_unique_slug(text: str, table: str, column: str = "slug", max_length: int = 250) -> str:
    """Genera un slug único verificando contra la base de datos.

    Si el slug base ya existe, se añade un sufijo numérico (-2, -3, ...).
    """
    base_slug = generate_slug(text, max_length - 4)  # Reservar espacio para sufijo
    if not base_slug:
        base_slug = "sin-titulo"

    candidate = base_slug
    counter = 1

    while True:
        row = db_query(
            f"SELECT 1 FROM public.{table} WHERE {column} = %s",
            (candidate,),
            fetch="one",
        )
        if not row:
            return candidate
        counter += 1
        candidate = f"{base_slug}-{counter}"


# =============================================================================
# POBLACIÓN DE SLUGS EN STARTUP
# =============================================================================

def populate_missing_slugs():
    """Genera slugs para registros de tipo_documento que tengan slug NULL.

    Se ejecuta en el startup de la aplicación para garantizar consistencia
    sin depender de lógica SQL compleja.
    """
    try:
        rows = db_query(
            "SELECT id, nombre FROM public.tipo_documento WHERE slug IS NULL OR slug = ''",
            fetch="all",
        )
        if not rows:
            return

        for row in rows:
            slug = generate_unique_slug(row["nombre"], "tipo_documento")
            db_query(
                "UPDATE public.tipo_documento SET slug = %s WHERE id = %s",
                (slug, row["id"]),
                fetch="none",
                commit=True,
            )
        logger.info(f"Slugs generados para {len(rows)} tipo(s) de documento.")
    except Exception as e:
        logger.error(f"Error al poblar slugs: {e}")


# =============================================================================
# UTILIDADES DE TEXTO
# =============================================================================

def truncate_text(text: str, max_length: int = 200, suffix: str = "…") -> str:
    """Trunca texto a max_length caracteres sin cortar palabras."""
    if not text or len(text) <= max_length:
        return text or ""
    truncated = text[:max_length - len(suffix)].rsplit(" ", 1)[0]
    return truncated + suffix


def normalize_cedula(cedula: str) -> str:
    """Normaliza una cédula venezolana: V-12345678 → 12345678 (solo dígitos)."""
    if not cedula:
        return ""
    cleaned = re.sub(r"[^0-9]", "", cedula)
    return cleaned
