"""Configuración centralizada leída desde variables de entorno."""
import os
from functools import lru_cache


class Settings:
    database_url: str = os.environ.get("DATABASE_URL", "")
    secret_key: str = os.environ.get("SECRET_KEY", "ciencias-ucv-dev-key")
    app_name: str = "Archivo Institucional — Ciencias UCV"
    app_version: str = "2.0.0"
    debug: bool = os.environ.get("DEBUG", "false").lower() == "true"
    environment: str = os.environ.get("ENVIRONMENT", "production")
    choices_cache_ttl: int = int(os.environ.get("CHOICES_CACHE_TTL", "300"))
    db_pool_min: int = int(os.environ.get("DB_POOL_MIN", "1"))
    db_pool_max: int = int(os.environ.get("DB_POOL_MAX", "5"))


@lru_cache(maxsize=1)
def get_settings() -> Settings:
    return Settings()


settings = get_settings()
