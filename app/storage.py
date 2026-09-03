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

# Leidas del entorno (IN-002): antes eran literales en este archivo, visibles
# para quien clonara el repositorio y conservadas para siempre en el historial
# de git. os.environ.get (sin valor por defecto) deja is_configured() en False
# si falta alguna, en vez de fallar en silencio con una cadena vacia.
R2_ENDPOINT   = os.environ.get("R2_ENDPOINT")
R2_ACCESS_KEY = os.environ.get("R2_ACCESS_KEY")
R2_SECRET_KEY = os.environ.get("R2_SECRET_KEY")
R2_BUCKET     = os.environ.get("R2_BUCKET")

_client = None
_client_lock = None  # inicializado bajo demanda para evitar import-time threading


class StorageNotFoundError(Exception):
    """El objeto solicitado no existe en el bucket."""


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
    """Genera una URL prefirmada de lectura con expiración (1 h por defecto).

    Raises StorageNotFoundError si el objeto no existe en el bucket.
    """
    from botocore.exceptions import ClientError
    client = _get_client()
    try:
        client.head_object(Bucket=R2_BUCKET, Key=key)
    except ClientError as ce:
        code = ce.response.get("Error", {}).get("Code", "")
        if code in ("404", "NoSuchKey"):
            raise StorageNotFoundError(key)
        raise
    return client.generate_presigned_url(
        "get_object",
        Params={"Bucket": R2_BUCKET, "Key": key},
        ExpiresIn=expires_seconds,
    )


def delete_object(key: str) -> None:
    """Elimina un objeto del bucket (usado al borrar registros)."""
    _get_client().delete_object(Bucket=R2_BUCKET, Key=key)
