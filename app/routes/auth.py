import re
import time
from fastapi import APIRouter, Cookie, HTTPException, Request, Response, status
from typing import Optional

from core.config import settings
from core.security import generate_session_token, verify_session_token
from database import db_query, log_event, verify_password
from models import LoginRequest, RestoreSessionRequest

router = APIRouter(prefix="/api/auth", tags=["auth"])

# IN-139: hash señuelo contra el que se verifica cuando el usuario no existe (o
# no tiene ninguna fila activa), para que un intento contra una cuenta
# inexistente tarde lo mismo que uno contra una que sí existe. bcrypt.checkpw
# es el gasto que delataba la diferencia de tiempo; sin este hash, ese camino
# no llamaba a bcrypt en absoluto y volvía en milisegundos.
_DUMMY_HASH = (
    "$2b$12$C6UzMDM.H6dfI/f/IKcEeO4kNEo1BQ4iRcNRuVeH2y8dNCvJz3Kzu"
)

# SI-021: caracteres de control y delimitadores de marcado que no deben llegar
# tal cual a audit_log — la pestaña Auditoría los pinta sin escapar (SI-006),
# así que un intento de login con usuario `<img src=x onerror=...>` quedaba
# guardado y se ejecutaba al abrir esa pestaña, sin ninguna credencial válida.
_LOG_UNSAFE_RE = re.compile(r"[\x00-\x1f\x7f<>]")


def _sanitize_for_log(value: str) -> str:
    """Recorta y limpia un valor de entrada antes de pasarlo a log_event.

    No sustituye el escape al pintar (fuera de esta zona), sólo reduce lo que
    queda guardado en la base: quita caracteres de control y `<`/`>` (que es
    lo único que necesita un payload HTML/JS para ejecutarse) y limita a 100
    caracteres, el ancho real de `audit_log.usuario`.
    """
    return _LOG_UNSAFE_RE.sub("", value or "")[:100]

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
        log_event(_sanitize_for_log(req.username), "Login Blocked", "Auth", "Bloqueado por intentos fallidos", "Failure")
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
    active_rows = [r for r in rows if r.get("is_active", True)] if rows else []
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
            log_event(_sanitize_for_log(req.username), "Login Success", ";".join(modules), f"Roles: {roles}")
            _set_session_cookie(response, req.username.strip())
            _clear_failures(key)
            return payload

    if not active_rows:
        # IN-139: ni siquiera hubo una fila activa contra la que llamar a
        # bcrypt (usuario inexistente, o existente pero desactivado/con todas
        # sus filas inactivas) — sin este chequeo señuelo, este camino nunca
        # llama a bcrypt y responde en milisegundos, delatando por diferencia
        # de tiempo que el usuario no existe. Se descarta el resultado; sólo
        # importa el tiempo gastado.
        verify_password(req.password, _DUMMY_HASH)

    _register_failure(key)
    log_event(_sanitize_for_log(req.username), "Login Failure", "Auth", "Credenciales incorrectas o cuenta desactivada", "Failure")
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
def logout(response: Response, ds_session: Optional[str] = Cookie(default=None)):
    # SI-145: el logout no quedaba en audit_log — la pestaña Auditoría no
    # podía responder "¿quién cerró sesión y cuándo?". El username sale de la
    # propia cookie (si la firma no valida o ya expiró, no hay a quién
    # atribuírselo y no se registra nada: no tiene sentido inventar un valor).
    username = verify_session_token(ds_session) if ds_session else None
    if username:
        log_event(username, "Logout", "Auth", "Cierre de sesión", "Success")
    response.delete_cookie("ds_session")
    return {"ok": True}


@router.get("/verify")
def verify_session_endpoint(ds_session: Optional[str] = Cookie(default=None)):
    """Verifica si la cookie de sesión es válida (SI-028/IN-038: ya no acepta el token por query)."""
    username = verify_session_token(ds_session)
    if not username:
        raise HTTPException(status_code=401, detail="Token inválido o expirado")
    return {"username": username}
