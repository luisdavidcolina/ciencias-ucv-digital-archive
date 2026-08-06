"""Funciones de seguridad: hashing/verificación de contraseñas y tokens de sesión."""
import base64
import hmac
import hashlib
import time

import bcrypt

from core.config import settings

# Duración del token de sesión (12 horas, igual que el TTL del cliente)
_SESSION_TTL = 43200


def hash_password(plain: str) -> str:
    return bcrypt.hashpw(plain.encode(), bcrypt.gensalt()).decode()


def verify_password(plain: str, hashed: str) -> bool:
    try:
        return bcrypt.checkpw(plain.encode(), hashed.encode())
    except Exception:
        return False


def generate_session_token(username: str) -> str:
    """Genera un token HMAC stateless firmado con SECRET_KEY."""
    ts = str(int(time.time()))
    payload = f"{username}:{ts}"
    sig = hmac.new(
        settings.secret_key.encode(), payload.encode(), hashlib.sha256
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
            settings.secret_key.encode(),
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
