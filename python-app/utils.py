"""Utilidades compartidas para el proyecto Ciencias UCV Digital Archive."""

import re
import unicodedata

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
