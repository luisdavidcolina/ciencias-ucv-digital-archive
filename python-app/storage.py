"""Almacenamiento de objetos en Cloudflare R2 (API compatible con S3).

Variables de entorno requeridas:
    R2_ENDPOINT   → https://<account_id>.r2.cloudflarestorage.com
    R2_ACCESS_KEY → Access Key ID del token de API de R2
    R2_SECRET_KEY → Secret Access Key del token de API de R2
    R2_BUCKET     → Nombre del bucket (p. ej. ciencias-ucv-archivo)

El bucket permanece privado: los archivos se sirven mediante URLs prefirmadas
generadas bajo demanda (véase routes/files.py).
"""
import os
import re
import unicodedata
import uuid
from datetime import datetime

import boto3
from botocore.config import Config
from dotenv import load_dotenv

load_dotenv()

# TODO: Mover a variables de entorno cuando se configure en Vercel
R2_ENDPOINT   = "https://b5a84a6638adcb92e50c91606205cb83.r2.cloudflarestorage.com"
R2_ACCESS_KEY = "79f8272f861775a7d5265852dfb26685"
R2_SECRET_KEY = "40f52439b69b4cdfee3b4a37805ea7bf203fd707067bf1baa146f21c4c2566dc"
R2_BUCKET     = "ciencias-ucv-archivo"

_client = None

# Extensiones permitidas para el archivo digitalizado
ALLOWED_EXTENSIONS = {".pdf", ".png", ".jpg", ".jpeg", ".tiff", ".tif", ".webp"}
MAX_FILE_SIZE = 25 * 1024 * 1024  # 25 MB


def is_configured() -> bool:
    """Indica si las credenciales de R2 están presentes en el entorno."""
    return bool(R2_ENDPOINT and R2_ACCESS_KEY and R2_SECRET_KEY and R2_BUCKET)


def _get_client():
    global _client
    if _client is None:
        _client = boto3.client(
            "s3",
            endpoint_url=R2_ENDPOINT,
            aws_access_key_id=R2_ACCESS_KEY,
            aws_secret_access_key=R2_SECRET_KEY,
            config=Config(signature_version="s3v4", region_name="auto"),
        )
    return _client


def sanitize_filename(filename: str) -> str:
    """Normaliza el nombre de archivo: sin acentos, espacios ni caracteres raros."""
    base, dot, ext = filename.rpartition(".")
    if not dot:
        base, ext = filename, ""
    base = unicodedata.normalize("NFKD", base).encode("ascii", "ignore").decode("ascii")
    base = re.sub(r"[^A-Za-z0-9_-]+", "-", base).strip("-").lower() or "archivo"
    ext = re.sub(r"[^A-Za-z0-9]", "", ext).lower()
    return f"{base}.{ext}" if ext else base


def build_object_key(modulo: str, filename: str) -> str:
    """Genera una clave única: <modulo>/<año>/<uuid8>-<nombre-sano>."""
    safe = sanitize_filename(filename)
    year = datetime.now().strftime("%Y")
    return f"{modulo.lower()}/{year}/{uuid.uuid4().hex[:8]}-{safe}"


def upload_fileobj(fileobj, key: str, content_type: str = "application/octet-stream") -> None:
    """Sube un objeto al bucket R2."""
    _get_client().upload_fileobj(
        fileobj,
        R2_BUCKET,
        key,
        ExtraArgs={"ContentType": content_type},
    )


def presigned_get_url(key: str, expires_seconds: int = 3600) -> str:
    """Genera una URL prefirmada de lectura con expiración (1 h por defecto)."""
    return _get_client().generate_presigned_url(
        "get_object",
        Params={"Bucket": R2_BUCKET, "Key": key},
        ExpiresIn=expires_seconds,
    )


def delete_object(key: str) -> None:
    """Elimina un objeto del bucket (usado al borrar registros)."""
    _get_client().delete_object(Bucket=R2_BUCKET, Key=key)
