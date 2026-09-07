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
import threading
import unicodedata
import uuid
from datetime import datetime

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
# IN-126: protege la creación perezosa del cliente frente a dos peticiones
# concurrentes en el mismo proceso (Vercel puede servir varias peticiones a
# la vez en el mismo runtime). Antes se declaraba pero nunca se usaba: dos
# hilos podían construir dos boto3.client en paralelo y pisarse la variable
# global sin que nada lo impidiera.
_client_lock = threading.Lock()


class StorageNotFoundError(Exception):
    """El objeto solicitado no existe en el bucket."""


# Extensiones permitidas para el archivo digitalizado
# DG-080: .jp2 (JPEG 2000) añadido — formato habitual de máster de
# preservación que un escáner puede producir y que antes se rechazaba con
# 400 sin motivo técnico (la lista sólo cubría lo que ya se usaba, no lo que
# hace falta para digitalización). No resuelve DG-080 entero (distinguir
# máster de copia de consulta exige tocar files.py/main.py, fuera de zona).
ALLOWED_EXTENSIONS = {".pdf", ".png", ".jpg", ".jpeg", ".tiff", ".tif", ".webp", ".jp2"}
MAX_FILE_SIZE = 25 * 1024 * 1024  # 25 MB

# IN-148: content-type fijado por el servidor para las extensiones que sí
# controlamos (ALLOWED_EXTENSIONS), en vez de confiar en el que declara el
# cliente (`file.content_type`, falsificable). Antes se guardaba en R2 con
# el tipo que mandara el navegador: subir `informe.pdf` declarado como
# `text/html` lo servía como HTML ejecutable desde el dominio de R2 — XSS
# almacenado. `upload_fileobj` ignora el `content_type` recibido cuando la
# clave termina en una de estas extensiones; para el resto (adjuntos de IA,
# copias de backup en JSON) respeta el que le pase el llamador.
_SAFE_CONTENT_TYPES = {
    ".pdf": "application/pdf",
    ".png": "image/png",
    ".jpg": "image/jpeg",
    ".jpeg": "image/jpeg",
    ".tiff": "image/tiff",
    ".tif": "image/tiff",
    ".webp": "image/webp",
    ".jp2": "image/jp2",
}


def is_configured() -> bool:
    """Indica si las credenciales de R2 están presentes en el entorno."""
    return bool(R2_ENDPOINT and R2_ACCESS_KEY and R2_SECRET_KEY and R2_BUCKET)


def _get_client():
    global _client
    if _client is None:
        with _client_lock:
            # Re-comprobar dentro del lock (double-checked locking): otro
            # hilo pudo haber terminado de construirlo mientras esperábamos.
            if _client is None:
                # IN-106: import perezoso — boto3 es de las dependencias más
                # pesadas del paquete y la mayoría de las peticiones
                # (búsquedas, panel) no tocan R2. Antes se importaba a nivel
                # de módulo, pagando su coste en cada arranque en frío aunque
                # nadie fuera a subir ni descargar nada.
                import boto3
                from botocore.config import Config
                # IN-126: sin `retries`/timeouts explícitos el cliente hereda
                # los valores por defecto de botocore, que pueden reintentar
                # más allá del presupuesto de la función serverless. Acotado
                # a 2 intentos y timeouts cortos: mejor un 502 rápido que
                # colgar la petición hasta que Vercel la mate por tiempo.
                _client = boto3.client(
                    "s3",
                    endpoint_url=R2_ENDPOINT,
                    aws_access_key_id=R2_ACCESS_KEY,
                    aws_secret_access_key=R2_SECRET_KEY,
                    config=Config(
                        signature_version="s3v4",
                        region_name="auto",
                        retries={"max_attempts": 2, "mode": "standard"},
                        connect_timeout=3,
                        read_timeout=10,
                    ),
                )
    return _client


def sanitize_filename(filename: str) -> str:
    """Normaliza el nombre de archivo: sin acentos, espacios ni caracteres raros.

    IN-057: existía una segunda implementación en `utils.py` con un
    comportamiento distinto (conservaba acentos, no pasaba a minúsculas) y a
    la que nadie llamaba. Se retiró: ésta es la única, porque `storage.py` es
    quien tiene el requisito real — la clave del objeto en R2.
    """
    base, dot, ext = filename.rpartition(".")
    if not dot:
        base, ext = filename, ""
    base = unicodedata.normalize("NFKD", base).encode("ascii", "ignore").decode("ascii")
    base = re.sub(r"[^A-Za-z0-9_-]+", "-", base).strip("-").lower() or "archivo"
    ext = re.sub(r"[^A-Za-z0-9]", "", ext).lower()
    return f"{base}.{ext}" if ext else base


def build_object_key(modulo: str, filename: str) -> str:
    """Genera una clave única: <modulo>/<año>/<uuid32>.<ext>.

    IN-151: la clave ya no lleva el nombre original del archivo. Antes era
    `<modulo>/<año>/<uuid8>-<nombre-sano>` — con sólo 8 hex de UUID (4.300M
    de combinaciones, suficiente contra fuerza bruta) pero el nombre iba en
    claro: `rrhh/2025/a1b2c3d4-partida-nacimiento-maria-perez.pdf` queda en
    cada respuesta de la API, en los logs de acceso y en el historial del
    navegador cuando se firma la URL. Se conserva la extensión (necesaria
    para el content-type fijado por el servidor, IN-148) y se pasa a UUID
    completo ya que no hace falta acortarlo para que quepa un nombre al lado.
    El nombre original para mostrar al usuario debe salir del registro en
    base de datos, no de la clave (columna de metadatos), fuera de esta zona.
    """
    safe = sanitize_filename(filename)
    _, dot, ext = safe.rpartition(".")
    year = datetime.now().strftime("%Y")
    token = uuid.uuid4().hex
    return f"{modulo.lower()}/{year}/{token}.{ext}" if dot else f"{modulo.lower()}/{year}/{token}"


def upload_fileobj(fileobj, key: str, content_type: str = "application/octet-stream") -> None:
    """Sube un objeto al bucket R2.

    IN-148: para las extensiones de `ALLOWED_EXTENSIONS` el content-type que
    se guarda en R2 lo decide el servidor a partir de la clave, no el que
    declare el cliente — evita servir un archivo con un tipo MIME distinto
    al que su extensión validada indica. Para claves con otra extensión
    (adjuntos de IA, copias de backup en `.json`) se respeta `content_type`.
    """
    ext = "." + key.rsplit(".", 1)[-1].lower() if "." in key.rsplit("/", 1)[-1] else ""
    tipo = _SAFE_CONTENT_TYPES.get(ext, content_type)
    _get_client().upload_fileobj(
        fileobj,
        R2_BUCKET,
        key,
        ExtraArgs={"ContentType": tipo},
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
