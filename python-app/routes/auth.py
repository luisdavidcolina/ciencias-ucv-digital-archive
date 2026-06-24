from fastapi import APIRouter, HTTPException, status

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

@router.post("/login")
def login(req: LoginRequest):
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
                # Recolecta todos los módulos activos del usuario
                payload = _build_user_response(active_rows, req.username.strip())
                modules = payload["user"]["modules"]
                roles = payload["user"]["roles"]
                log_event(req.username, "Login Success", ";".join(modules), f"Roles: {roles}")
                return payload

    log_event(req.username, "Login Failure", "Auth", "Credenciales incorrectas o cuenta desactivada", "Failure")
    raise HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Credenciales incorrectas o cuenta desactivada",
    )


@router.post("/restore")
def restore_session(req: RestoreSessionRequest):
    rows = db_query(
        "SELECT usuario, nombre_usuario, modulo, rol, "
        "COALESCE(is_active, TRUE) AS is_active "
        "FROM public.usuarios_sistema "
        "WHERE TRIM(usuario) = %s",
        (req.username.strip(),),
        fetch="all",
    )
    if rows:
        # Verificar que el usuario esté activo
        if not rows[0].get("is_active", True):
            raise HTTPException(status_code=403, detail="Cuenta desactivada")
        payload = _build_user_response(rows, req.username.strip())
        modules = payload["user"]["modules"]
        roles = payload["user"]["roles"]
        log_event(req.username, "Session Restored", ";".join(modules), f"Roles: {roles}")
        return payload

    raise HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Sesión no encontrada",
    )
