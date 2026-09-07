"""Configuración centralizada leída desde variables de entorno."""
import os
from functools import lru_cache


def _int_env(name: str, default: str) -> int:
    """int(os.environ.get(name, default)) con un error que nombra la variable.

    Sin esto, un valor mal formado (p. ej. DB_POOL_MAX="5 " con un espacio, o
    un typo) revienta el arranque con "invalid literal for int() with base
    10: '...'" sin decir cuál de las tres variables fue. Con muchas instancias
    serverless arrancando en frío a la vez, ese mensaje críptico es lo único
    que llega al log.
    """
    raw = os.environ.get(name, default)
    try:
        return int(raw)
    except ValueError:
        raise ValueError(
            f"Variable de entorno {name}={raw!r} no es un entero válido"
        ) from None


class Settings:
    # Base de datos (Neon PostgreSQL). IN-033: única fuente de la variable —
    # `database.py` la toma de aquí (`settings.database_url`) en vez de
    # releerla con os.getenv/os.environ.get por su cuenta.
    database_url: str = os.environ.get("DATABASE_URL", "")

    # Seguridad
    secret_key: str = os.environ.get("SECRET_KEY", "ciencias-ucv-dev-key-change-in-prod")

    # Aplicación
    app_name: str = "Archivo Institucional — Ciencias UCV"
    app_version: str = "3.3.0"
    debug: bool = os.environ.get("DEBUG", "false").lower() == "true"

    # Entorno (production / development)
    environment: str = os.environ.get("ENVIRONMENT", "production")

    # Cache TTL en segundos para choices
    choices_cache_ttl: int = _int_env("CHOICES_CACHE_TTL", "300")

    # Pool de conexiones psycopg2
    db_pool_min: int = _int_env("DB_POOL_MIN", "1")
    db_pool_max: int = _int_env("DB_POOL_MAX", "5")


@lru_cache(maxsize=1)
def get_settings() -> Settings:
    return Settings()


settings = get_settings()
