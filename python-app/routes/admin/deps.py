"""Dependencias compartidas de los endpoints de administración."""
from fastapi import Cookie, Depends, HTTPException, Header, status

from core.security import verify_session_token


def require_session(
    ds_session: str | None = Cookie(default=None),
    x_session_token: str | None = Header(default=None),
) -> str:
    """
    Valida que la petición tenga una sesión activa.
    Acepta cookie HttpOnly (navegador) o cabecera X-Session-Token (scripts/API).
    Retorna el nombre de usuario o lanza 401.
    """
    token = ds_session or x_session_token
    username = verify_session_token(token)
    if not username:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Sesión no autenticada o expirada",
            headers={"WWW-Authenticate": "Cookie"},
        )
    return username
