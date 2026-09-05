"""Funciones de seguridad: hashing/verificación de contraseñas y tokens de sesión."""
import base64
import hmac
import hashlib
import secrets
import time

import bcrypt

from core.config import settings

# Duración del token de sesión (12 horas, igual que el TTL del cliente)
_SESSION_TTL = 43200


def _derive_key(proposito: str) -> bytes:
    """Deriva una clave distinta por propósito a partir de SECRET_KEY (IN-145).

    Antes, el token de sesión y el de compartición firmaban con la MISMA clave
    (`settings.secret_key` en crudo): no colisionaban hoy sólo porque tienen
    distinto número de campos, algo que nada garantiza si cualquiera de los
    dos formatos cambia. `HMAC(secret_key, proposito)` es la derivación simple
    que basta aquí — no hace falta una HKDF completa — para que un token de un
    tipo nunca pueda ser aceptado como del otro, sin depender de la forma del
    payload.
    """
    return hmac.new(settings.secret_key.encode(), proposito.encode(), hashlib.sha256).digest()


def hash_password(plain: str) -> str:
    return bcrypt.hashpw(plain.encode(), bcrypt.gensalt()).decode()


def verify_password(plain: str, hashed: str) -> bool:
    try:
        return bcrypt.checkpw(plain.encode(), hashed.encode())
    except Exception:
        return False


def generate_session_token(username: str) -> str:
    """Genera un token HMAC stateless firmado con una clave derivada de SECRET_KEY."""
    ts = str(int(time.time()))
    payload = f"{username}:{ts}"
    sig = hmac.new(
        _derive_key("session"), payload.encode(), hashlib.sha256
    ).hexdigest()
    return base64.urlsafe_b64encode(f"{payload}:{sig}".encode()).decode()


def verify_session_token(token: str | None) -> str | None:
    """Retorna el username si el token es válido y no expiró; None en caso contrario."""
    if not token:
        return None
    try:
        raw = base64.urlsafe_b64decode(token.encode()).decode()
        username, ts, sig = raw.rsplit(":", 2)
        expected = hmac.new(
            _derive_key("session"),
            f"{username}:{ts}".encode(),
            hashlib.sha256,
        ).hexdigest()
        if not hmac.compare_digest(sig, expected):
            return None
        if time.time() - int(ts) > _SESSION_TTL:
            return None
        return username
    except Exception:
        return None


# =============================================================================
# ENLACES DE COMPARTICIÓN EXTERNA
# =============================================================================
# La investigación de mercado marcaba "Compartición Externa Segura" como
# ausente: para enseñarle un documento a alguien de fuera había que crearle un
# usuario o mandarle el archivo por correo, que es justo lo que un archivo
# institucional no debe hacer.
#
# El enlace es stateless y firmado: no hay tabla que mantener ni que limpiar, y
# caduca solo. Lleva el módulo y el id del documento para que el servidor no
# tenga que fiarse de nada que venga en la URL sin firmar.

def generate_share_token(modulo: str, doc_id: int, horas: int = 72) -> str:
    """Firma un enlace de consulta de un documento, con caducidad.

    Lleva dentro un `jti` (identificador único, aleatorio — NO secuencial, para
    que no se pueda adivinar ni enumerar) que es lo único que hace falta
    guardar para poder revocar este enlace en concreto sin tocar los demás
    (IN-146): el token sigue siendo autocontenido, no hay tabla de enlaces
    emitidos, sólo una tabla mínima de revocados que `verify_share_token`
    consulta antes de aceptar.
    """
    expira = int(time.time()) + int(horas) * 3600
    jti = secrets.token_urlsafe(12)
    payload = f"{modulo}:{int(doc_id)}:{expira}:{jti}"
    sig = hmac.new(
        _derive_key("share"), payload.encode(), hashlib.sha256
    ).hexdigest()
    return base64.urlsafe_b64encode(f"{payload}:{sig}".encode()).decode().rstrip("=")


def share_token_jti(token: str) -> str | None:
    """Extrae el `jti` de un token ya firmado, sin repetir la verificación HMAC.

    Uso: `crear_enlace` en `share.py` necesita el `jti` para poder ofrecerlo al
    administrador (auditoría, revocación) justo después de generarlo — no hace
    falta volver a comprobar la firma que se acaba de generar aquí mismo.
    """
    try:
        relleno = "=" * (-len(token) % 4)
        raw = base64.urlsafe_b64decode((token + relleno).encode()).decode()
        _, _, _, jti, _ = raw.rsplit(":", 4)
        return jti
    except Exception:
        return None


def _esta_revocado(jti: str) -> bool:
    """Consulta la tabla de revocación. Import diferido: `database.py` importa
    `hash_password`/`verify_password` de este módulo, así que importarlo aquí
    arriba crearía un ciclo. En tiempo de llamada ambos módulos ya están
    cargados y el import diferido no tiene ese problema."""
    from database import db_query
    fila = db_query(
        "SELECT 1 FROM public.enlaces_revocados WHERE jti = %s",
        [jti], fetch="one",
    )
    return fila is not None


def verify_share_token(token: str | None) -> tuple[str, int] | None:
    """Retorna (modulo, doc_id) si el enlace es válido, no expiró y no fue revocado."""
    if not token:
        return None
    try:
        relleno = "=" * (-len(token) % 4)      # se quitó al firmar; hay que reponerlo
        raw = base64.urlsafe_b64decode((token + relleno).encode()).decode()
        modulo, doc_id, expira, jti, sig = raw.rsplit(":", 4)
        esperada = hmac.new(
            _derive_key("share"),
            f"{modulo}:{doc_id}:{expira}:{jti}".encode(),
            hashlib.sha256,
        ).hexdigest()
        if not hmac.compare_digest(sig, esperada):
            return None
        if time.time() > int(expira):
            return None
        if modulo not in ("Archivo", "RRHH"):
            return None
        if _esta_revocado(jti):
            return None
        return modulo, int(doc_id)
    except Exception:
        return None
