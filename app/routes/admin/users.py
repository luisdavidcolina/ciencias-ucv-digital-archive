"""Gestión de usuarios del sistema.

OR-043/044/045: crear, listar, editar, borrar o resetear la contraseña de
OTRO usuario del sistema es una operación que sólo el administrador Global
debería poder hacer, sea cual sea el módulo del usuario objetivo — un admin
de RRHH no debe poder resetear la clave del admin Global y heredar así el
control del sistema entero (backup, Archivo, todo). `require_admin_role`
(IN-131/IN-132, `routes/admin/deps.py`) lo exige de verdad para todo el
router, no sólo en el docstring.

OA-039/IN-142/SI-036: la longitud mínima real vive aquí, no en `models.py`
(`UserCreateRequest.password`/`PasswordChangeRequest.new_password` sólo
exigen 6 como piso de Pydantic — subirlo ahí es zona de otro carril, `[CHOCA]`).
Este módulo aplica el mínimo real de 12 y rechaza contraseñas de la lista de
las más comunes antes de guardar nada.
"""
from fastapi import APIRouter, Depends, HTTPException

from database import db_query, log_event, hash_password
from models import UserCreateRequest, PasswordChangeRequest
from routes.admin.deps import require_session, require_admin_role

router = APIRouter(
    dependencies=[Depends(require_session), Depends(require_admin_role("Global"))]
)

# OA-039/IN-142/SI-036: mínimo real de contraseña. `models.py` sólo garantiza
# 6 como piso de Pydantic (zona ajena); el mínimo de la política vive aquí.
_PASSWORD_MIN_LENGTH = 12

# Subconjunto de las contraseñas más filtradas/comunes (listas tipo
# RockYou/NCSC), en minúsculas, para comparar sin distinguir mayúsculas.
# No pretende ser exhaustiva: es la primera barrera contra lo obvio, no un
# verificador contra una brecha completa.
_COMMON_PASSWORDS = {
    "123456", "123456789", "12345678", "12345", "1234567890", "1234567",
    "password", "password1", "password123", "qwerty", "qwerty123",
    "111111", "123123", "abc123", "letmein", "welcome", "admin", "admin123",
    "iloveyou", "monkey", "dragon", "football", "baseball", "master",
    "contraseña", "contrasena", "contrasena123", "12345678910",
    "administrador", "usuario123", "cambiar123", "ciencias123", "ucv12345",
}


def _validar_fuerza_password(plain: str) -> None:
    """Valida longitud mínima y rechazo de contraseñas comunes (OA-039/IN-142).

    Lanza 400 con el detalle si no cumple. Nunca loguea el valor recibido.
    """
    valor = plain.strip()
    if len(valor) < _PASSWORD_MIN_LENGTH:
        raise HTTPException(
            status_code=400,
            detail=f"La contraseña debe tener al menos {_PASSWORD_MIN_LENGTH} caracteres",
        )
    if valor.lower() in _COMMON_PASSWORDS:
        raise HTTPException(
            status_code=400,
            detail="Esa contraseña está entre las más comunes y no se puede usar",
        )


@router.get("/users")
def get_users_list(modulo: str = ""):
    """Lista usuarios del sistema.

    Si se indica `modulo` ('Archivo' o 'RRHH'), devuelve los usuarios de ese
    módulo más los administradores globales (visibles en ambos backoffices
    para que puedan gestionar su propia cuenta). Sin filtro (panel de Sistema
    Global) devuelve todos.
    """
    base_sql = (
        "SELECT id, usuario, nombre_usuario, modulo, rol, "
        "COALESCE(is_active, TRUE) AS is_active, last_login "
        "FROM public.usuarios_sistema"
    )
    modulo = modulo.strip()
    if modulo in ("Archivo", "RRHH"):
        rows = db_query(
            base_sql + " WHERE modulo IN (%s, 'Global') ORDER BY usuario",
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
def create_user(req: UserCreateRequest, actor: str = Depends(require_session)):
    existing = db_query(
        "SELECT id FROM public.usuarios_sistema WHERE TRIM(usuario) = %s",
        (req.usuario.strip(),),
        fetch="one",
    )
    if existing:
        raise HTTPException(status_code=400, detail="Usuario ya existe")

    _validar_fuerza_password(req.password)
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
    # IN-133: el actor sale de la sesión verificada (`require_session`), no del
    # campo `creator` que manda el cliente en el cuerpo — ese campo ya no se usa
    # para la auditoría, aunque el modelo (`models.py`, zona ajena) lo siga
    # exigiendo por compatibilidad.
    log_event(actor, "Create User", req.modulo, f"Nuevo: {req.usuario} ({req.rol})")
    return {"success": True}


@router.put("/users/{uid}/password")
def change_password(uid: int, req: PasswordChangeRequest, actor: str = Depends(require_session)):
    _validar_fuerza_password(req.new_password)
    hashed_pw = hash_password(req.new_password.strip())
    db_query(
        "UPDATE public.usuarios_sistema SET contrasena = %s WHERE id = %s",
        (hashed_pw, uid), fetch="none", commit=True,
    )
    # IN-133: igual que en create_user, el actor real es el de la sesión, no
    # `req.requester` (declarado por el cliente).
    log_event(actor, "Change Password", "Admin", f"Usuario ID: {uid}")
    return {"success": True}


@router.patch("/users/{uid}/active")
def toggle_user_active(uid: int, actor: str = Depends(require_session)):
    row = db_query("SELECT is_active FROM public.usuarios_sistema WHERE id = %s", (uid,), fetch="one")
    if not row:
        raise HTTPException(404, "Usuario no encontrado")
    new_state = not bool(row.get("is_active", True))
    db_query(
        "UPDATE public.usuarios_sistema SET is_active = %s WHERE id = %s",
        (new_state, uid), fetch="none", commit=True,
    )
    # IN-133: antes venía por query string (`requester=""`, sin validar); ahora
    # es el usuario autenticado de la sesión.
    log_event(actor, "Toggle User Active", "Admin", f"uid={uid} → is_active={new_state}")
    return {"success": True, "is_active": new_state}


@router.delete("/users/{uid}")
def delete_user(uid: int, actor: str = Depends(require_session)):
    """Borra un usuario del sistema.

    RONDA29 (integridad referencial): `datos_archivo.creado_por` y
    `datos_rrhh.creado_por` tienen FK `ON DELETE RESTRICT` hacia
    `usuarios_sistema` (schema.sql) — un `DELETE` liso sobre un usuario que
    ya creó documentos no dejaba ningún huérfano (la FK lo impide a nivel de
    base de datos), pero SÍ reventaba con el 500 genérico de `database.py`
    ("Error en base de datos", IN-036) en vez de un error accionable: mismo
    patrón que ya se arregló para `tipo_documento` en
    `admin/catalog.py:delete_category` (comentario OA-123/OR-163), que sí
    comprueba el uso antes de intentar el DELETE. Aquí se hace la misma
    comprobación explícita — 409 con el conteo, sin tocar la fila — antes de
    dejar que la FK decida por sorpresa.
    """
    row = db_query("SELECT usuario FROM public.usuarios_sistema WHERE id = %s", (uid,), fetch="one")
    if not row:
        raise HTTPException(404, "Usuario no encontrado")
    username = row["usuario"]

    uso_archivo = db_query(
        "SELECT COUNT(*) AS cnt FROM public.datos_archivo WHERE creado_por = %s",
        (uid,), fetch="one",
    )
    uso_rrhh = db_query(
        "SELECT COUNT(*) AS cnt FROM public.datos_rrhh WHERE creado_por = %s",
        (uid,), fetch="one",
    )
    uso_count = int((uso_archivo or {}).get("cnt", 0)) + int((uso_rrhh or {}).get("cnt", 0))
    if uso_count > 0:
        raise HTTPException(
            409,
            f"El usuario '{username}' es autor de {uso_count} documento(s) y no se puede "
            "borrar; desactívalo en su lugar.",
        )

    db_query("DELETE FROM public.usuarios_sistema WHERE id = %s", (uid,), fetch="none", commit=True)
    # IN-133: igual, actor de la sesión en vez del query param `requester`.
    log_event(actor, "Delete User", "Admin", f"Eliminado: {username}")
    return {"success": True}
