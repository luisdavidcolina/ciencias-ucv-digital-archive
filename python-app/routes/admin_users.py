"""Gestión de usuarios del sistema."""
from fastapi import APIRouter, HTTPException

from database import db_query, log_event, hash_password
from models import UserCreateRequest, PasswordChangeRequest

router = APIRouter()


@router.get("/users")
def get_users_list(modulo: str = ""):
    """Lista usuarios del sistema.

    Si se indica `modulo` ('Archivo' o 'RRHH'), devuelve solo los usuarios de
    ese módulo. Sin filtro (panel de Sistema Global) devuelve todos, incluidos
    los administradores globales.
    """
    base_sql = (
        "SELECT id, usuario, nombre_usuario, modulo, rol, "
        "COALESCE(is_active, TRUE) AS is_active, last_login "
        "FROM public.usuarios_sistema"
    )
    modulo = modulo.strip()
    if modulo in ("Archivo", "RRHH"):
        rows = db_query(
            base_sql + " WHERE modulo = %s ORDER BY usuario",
            (modulo,),
            fetch="all",
        ) or []
    else:
        rows = db_query(base_sql + " ORDER BY usuario", fetch="all") or []
    out = []
    for r in rows:
        d = dict(r)
        d["password"] = "••••••••"
        out.append(d)
    return out


@router.post("/users/create")
def create_user(req: UserCreateRequest):
    existing = db_query(
        "SELECT id FROM public.usuarios_sistema WHERE TRIM(usuario) = %s",
        (req.usuario.strip(),),
        fetch="one",
    )
    if existing:
        raise HTTPException(status_code=400, detail="Usuario ya existe")

    hashed_pw = hash_password(req.password.strip())
    db_query(
        """
        INSERT INTO public.usuarios_sistema (usuario, nombre_usuario, contrasena, modulo, rol)
        VALUES (%s, %s, %s, %s, %s)
        """,
        (req.usuario.strip(), req.usuario.strip(), hashed_pw, req.modulo, req.rol),
        fetch="none",
        commit=True,
    )
    log_event(req.creator, "Create User", req.modulo, f"Nuevo: {req.usuario} ({req.rol})")
    return {"success": True}


@router.put("/users/{uid}/password")
def change_password(uid: int, req: PasswordChangeRequest):
    if len(req.new_password.strip()) < 6:
        raise HTTPException(status_code=400, detail="La contraseña debe tener al menos 6 caracteres")
    hashed_pw = hash_password(req.new_password.strip())
    db_query(
        "UPDATE public.usuarios_sistema SET contrasena = %s WHERE id = %s",
        (hashed_pw, uid), fetch="none", commit=True,
    )
    log_event(req.requester, "Change Password", "Admin", f"Usuario ID: {uid}")
    return {"success": True}


@router.patch("/users/{uid}/active")
def toggle_user_active(uid: int, requester: str = ""):
    row = db_query("SELECT is_active FROM public.usuarios_sistema WHERE id = %s", (uid,), fetch="one")
    if not row:
        raise HTTPException(404, "Usuario no encontrado")
    new_state = not bool(row.get("is_active", True))
    db_query(
        "UPDATE public.usuarios_sistema SET is_active = %s WHERE id = %s",
        (new_state, uid), fetch="none", commit=True,
    )
    log_event(requester, "Toggle User Active", "Admin", f"uid={uid} → is_active={new_state}")
    return {"success": True, "is_active": new_state}


@router.delete("/users/{uid}")
def delete_user(uid: int, requester: str = ""):
    row = db_query("SELECT usuario FROM public.usuarios_sistema WHERE id = %s", (uid,), fetch="one")
    if not row:
        raise HTTPException(404, "Usuario no encontrado")
    username = row["usuario"]
    db_query("DELETE FROM public.usuarios_sistema WHERE id = %s", (uid,), fetch="none", commit=True)
    log_event(requester, "Delete User", "Admin", f"Eliminado: {username}")
    return {"success": True}
