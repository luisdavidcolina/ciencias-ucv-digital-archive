from fastapi import APIRouter, Cookie, HTTPException, Response, status
from typing import Optional

from core.config import settings
from core.security import generate_session_token, verify_session_token
from database import db_query, log_event, verify_password
from models import LoginRequest, RestoreSessionRequest

router = APIRouter(prefix="/api/auth", tags=["auth"])


# =============================================================================
# HELPERS
# =============================================================================

def _build_user_response(rows, username: str) -> dict:
    """Construye el payload de usuario a partir de las filas devueltas por la BD."""
    modules: list = []
    roles: dict = {}
    for row in rows:
        mod = str(row.get("modulo", "")).strip()
        rol = str(row.get("rol", "Normal")).strip() or "Normal"
        if mod and mod not in modules:
            modules.append(mod)
        if mod:
            roles[mod] = rol

    # Si el usuario tiene módulo "Global", expandir a Archivo + RRHH y quitar "Global"
    if "Global" in modules:
        global_role = roles.get("Global", "Admin")
        modules = [m for m in modules if m != "Global"]
        if "Archivo" not in modules:
            modules.append("Archivo")
        if "RRHH" not in modules:
            modules.append("RRHH")
        roles["Archivo"] = global_role
        roles["RRHH"] = global_role
        roles.pop("Global", None)

    primary_mod = modules[0] if modules else "Archivo"
    primary_role = roles.get(primary_mod, "Normal")
    return {
        "success": True,
        "user": {
            "username": username,
            "modules": modules,
            "roles": roles,
            "modulo": primary_mod,
            "rol": primary_role,
        },
    }


# =============================================================================
# ENDPOINTS
# =============================================================================

def _set_session_cookie(response: Response, username: str) -> None:
    token = generate_session_token(username)
    response.set_cookie(
        key="ds_session",
        value=token,
        max_age=43200,       # 12 horas
        httponly=True,
        samesite="lax",
        secure=settings.environment == "production",
    )


@router.post("/login")
def login(req: LoginRequest, response: Response):
    rows = db_query(
        "SELECT usuario, nombre_usuario, contrasena, modulo, rol, "
        "COALESCE(is_active, TRUE) AS is_active "
        "FROM public.usuarios_sistema "
        "WHERE TRIM(usuario) = %s",
        (req.username.strip(),),
        fetch="all",
    )
    if rows:
        active_rows = [r for r in rows if r.get("is_active", True)]
        for row in active_rows:
            if verify_password(req.password.strip(), row["contrasena"]):
                try:
                    db_query(
                        "UPDATE public.usuarios_sistema SET last_login = NOW() WHERE TRIM(usuario) = %s",
                        (req.username.strip(),), fetch="none", commit=True,
                    )
                except Exception:
                    pass
                payload = _build_user_response(active_rows, req.username.strip())
                modules = payload["user"]["modules"]
                roles = payload["user"]["roles"]
                log_event(req.username, "Login Success", ";".join(modules), f"Roles: {roles}")
                _set_session_cookie(response, req.username.strip())
                return payload

    log_event(req.username, "Login Failure", "Auth", "Credenciales incorrectas o cuenta desactivada", "Failure")
    raise HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Credenciales incorrectas o cuenta desactivada",
    )


@router.post("/restore")
def restore_session(
    req: RestoreSessionRequest,
    response: Response,
    ds_session: Optional[str] = Cookie(default=None),
):
    # Require a valid HMAC session token — otherwise anyone who knows a username
    # could call this endpoint and obtain a fresh authenticated cookie.
    token_user = verify_session_token(ds_session) if ds_session else None
    if not token_user or token_user.lower() != req.username.strip().lower():
        raise HTTPException(status_code=401, detail="Sesión no válida o expirada")

    rows = db_query(
        "SELECT usuario, nombre_usuario, modulo, rol, "
        "COALESCE(is_active, TRUE) AS is_active "
        "FROM public.usuarios_sistema "
        "WHERE TRIM(usuario) = %s",
        (req.username.strip(),),
        fetch="all",
    )
    if rows:
        if not rows[0].get("is_active", True):
            raise HTTPException(status_code=403, detail="Cuenta desactivada")
        payload = _build_user_response(rows, req.username.strip())
        modules = payload["user"]["modules"]
        roles = payload["user"]["roles"]
        log_event(req.username, "Session Restored", ";".join(modules), f"Roles: {roles}")
        _set_session_cookie(response, req.username.strip())
        return payload

    raise HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Sesión no encontrada",
    )


@router.post("/logout")
def logout(response: Response):
    response.delete_cookie("ds_session")
    return {"ok": True}


@router.get("/verify")
def verify_session_endpoint(ds_session: str | None = None):
    """Verifica si el token de sesión enviado como parámetro es válido (para debugging)."""
    username = verify_session_token(ds_session)
    if not username:
        raise HTTPException(status_code=401, detail="Token inválido o expirado")
    return {"username": username}
