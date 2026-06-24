"""Configuración centralizada leída desde variables de entorno."""
import os
from functools import lru_cache


class Settings:
    # Base de datos (Neon PostgreSQL)
    database_url: str = os.environ.get("DATABASE_URL", "")

    # Seguridad
    secret_key: str = os.environ.get("SECRET_KEY", "ciencias-ucv-dev-key-change-in-prod")

    # Aplicación
    app_name: str = "Archivo Institucional — Ciencias UCV"
    app_version: str = "2.0.0"
    debug: bool = os.environ.get("DEBUG", "false").lower() == "true"

    # Entorno (production / development)
    environment: str = os.environ.get("ENVIRONMENT", "production")

    # Cache TTL en segundos para choices
    choices_cache_ttl: int = int(os.environ.get("CHOICES_CACHE_TTL", "300"))

    # Pool de conexiones psycopg2
    db_pool_min: int = int(os.environ.get("DB_POOL_MIN", "1"))
    db_pool_max: int = int(os.environ.get("DB_POOL_MAX", "5"))


@lru_cache(maxsize=1)
def get_settings() -> Settings:
    return Settings()


settings = get_settings()
