import time
from fastapi import APIRouter, Cookie, HTTPException, Request, Response, status
from typing import Optional

from core.config import settings
from core.security import generate_session_token, verify_session_token
from database import db_query, log_event, verify_password
from models import LoginRequest, RestoreSessionRequest

router = APIRouter(prefix="/api/auth", tags=["auth"])

# =============================================================================
# BLOQUEO DE LOGIN (SI-031 / SI-032)
# =============================================================================
_FAILED_ATTEMPTS: dict[str, list[float]] = {}
_LOCK_THRESHOLD = 5
_LOCK_WINDOW_SECONDS = 300
_LOCK_SECONDS = 30


def _throttle_key(username: str, request: Optional[Request]) -> str:
    ip = request.client.host if request and request.client else "?"
    return f"{username.strip().lower()}|{ip}"


def _is_locked(key: str) -> Optional[float]:
    attempts = _FAILED_ATTEMPTS.get(key)
    if not attempts:
        return None
    now = time.time()
    attempts = [t for t in attempts if now - t < _LOCK_WINDOW_SECONDS]
    _FAILED_ATTEMPTS[key] = attempts
    if len(attempts) < _LOCK_THRESHOLD:
        return None
    last = attempts[-1]
    remaining = _LOCK_SECONDS - (now - last)
    return remaining if remaining > 0 else None


def _register_failure(key: str) -> None:
    _FAILED_ATTEMPTS.setdefault(key, []).append(time.time())


def _clear_failures(key: str) -> None:
    _FAILED_ATTEMPTS.pop(key, None)


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
        secure=settings.environment != "development",
    )


@router.post("/login")
def login(req: LoginRequest, response: Response, request: Request):
    key = _throttle_key(req.username, request)
    remaining = _is_locked(key)
    if remaining is not None:
        log_event(req.username, "Login Blocked", "Auth", "Bloqueado por intentos fallidos", "Failure")
        raise HTTPException(
            status_code=status.HTTP_429_TOO_MANY_REQUESTS,
            detail="Demasiados intentos. Intente de nuevo en unos segundos.",
            headers={"Retry-After": str(int(remaining) + 1)},
        )

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
            if verify_password(req.password, row["contrasena"]):
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
                _clear_failures(key)
                return payload

    _register_failure(key)
    log_event(req.username, "Login Failure", "Auth", "Credenciales incorrectas o cuenta desactivada", "Failure")
    raise HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Usuario o contraseña incorrectos",
    )


@router.post("/restore")
def restore_session(
    req: RestoreSessionRequest,
    response: Response,
    ds_session: Optional[str] = Cookie(default=None),
):
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
def verify_session_endpoint(ds_session: Optional[str] = Cookie(default=None)):
    """Verifica si la cookie de sesión es válida (SI-028/IN-038: ya no acepta el token por query)."""
    username = verify_session_token(ds_session)
    if not username:
        raise HTTPException(status_code=401, detail="Token inválido o expirado")
    return {"username": username}
