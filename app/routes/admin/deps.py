"""Dependencias compartidas de los endpoints de administración."""
from fastapi import Cookie, Depends, HTTPException, Header, status

from core.security import verify_session_token
from database import db_query


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


# =============================================================================
# Autorización por rol y módulo (IN-131, IN-132, SI-002, SI-003, BR-001, BR-002)
# =============================================================================
#
# `require_session` sólo comprueba que exista una sesión legible: no mira qué
# módulo ("Archivo", "RRHH" o "Global") ni qué rol ("Admin" o "Normal") tiene
# esa persona en `usuarios_sistema`. Diez auditorías independientes documentan
# el mismo patrón — "una sola ausencia, no cuarenta fallos" (IN-131) — con
# ejemplos ya confirmados: un usuario Normal de Archivo descarga la base
# entera con los hashes bcrypt vía /api/admin/backup/export (SI-002), lee y
# edita expedientes de RRHH (BR-001, BR-002, OR-043), o abre el panel Global
# (SI-003) porque hoy esa comprobación vive sólo en el frontend
# (`app/static/app.js`, función que arma `user.modules`).
#
# El criterio de "es Global" replica exactamente el que ya usa el frontend
# (app.js: `esGlobal = user.modules.includes("Archivo") && ...includes("RRHH")`)
# y el que ya aplica `routes/pages.py:serve_investigacion` contra la base: en
# `usuarios_sistema` un administrador máximo tiene una fila con
# `modulo = 'Global'` **literal** (ver `app/schema.sql`, fila de `admin`), no
# dos filas separadas. Por eso "Global" se trata aquí como comodín: pasa
# cualquier `require_role(...)`, tenga o no ese módulo en la lista de
# permitidos, porque en la práctica de negocio Global cubre Archivo + RRHH.
#
# ---------------------------------------------------------------------------
# CÓMO USARLA (para el abanico de 8 agentes que vienen detrás: docs.py,
# catalog.py, stats.py, retention.py, imports.py, users.py, trash.py, hr.py,
# archive.py, auth.py, files.py, ai.py)
# ---------------------------------------------------------------------------
#
# 1. Import:
#
#       from routes.admin.deps import require_session, require_role, require_admin_role
#
# 2. Para restringir un endpoint (o un router entero) a uno o más módulos:
#
#       @router.get("/algo-de-rrhh")
#       def ver_algo(usuario: str = Depends(require_session),
#                    _autorizado: str = Depends(require_role("RRHH"))):
#           ...
#
#    O aplicado a todo un router (igual que ya hace `backup.py` con
#    `require_session`):
#
#       router = APIRouter(dependencies=[Depends(require_session),
#                                        Depends(require_role("Global"))])
#
#    Varios módulos permitidos (cualquiera de ellos vale):
#
#       Depends(require_role("Archivo", "RRHH"))
#
# 3. Para las operaciones más sensibles dentro de un módulo (borrar usuarios,
#    restaurar backup, purgar retención), exige además `rol = 'Admin'` en ese
#    módulo con `require_admin_role`:
#
#       @router.delete("/usuarios/{id}")
#       def borrar_usuario(usuario: str = Depends(require_session),
#                          _autorizado: str = Depends(require_admin_role("Global"))):
#           ...
#
# 4. Ambas dependen de `require_session` internamente (`Depends(require_session)`
#    anidado dentro de la fábrica) — no hace falta declarar `require_session`
#    por separado sólo para que estas funcionen, aunque conviene dejarlo
#    explícito en la firma del endpoint para que quede documentado qué
#    devuelve `usuario` (útil para IN-133: el usuario sale de la sesión, nunca
#    del cuerpo de la petición).
#
# 5. Ambas lanzan 403 (nunca 401 — eso ya lo cubrió `require_session`) si:
#    - el usuario no aparece en `usuarios_sistema`,
#    - `is_active` es `FALSE`,
#    - el módulo no está en la lista de permitidos y tampoco es "Global",
#    - (sólo `require_admin_role`) el rol no es "Admin".
#
# 6. Para pruebas: usa el fixture `client_as` de `app/tests/conftest.py`
#    (SI-226) y mockea `db_query` del router bajo prueba con la fila de
#    `usuarios_sistema` que quieras simular. Ver
#    `app/tests/test_autorizacion_deps.py` para el patrón completo, y
#    `app/tests/test_autorizacion_fixture.py` para un ejemplo previo del
#    mismo estilo contra `serve_investigacion`.


def _fila_usuario(usuario: str) -> dict | None:
    """Consulta módulo, rol y estado del usuario. `usuario` es UNIQUE, así que
    esta consulta usa el índice y es barata; no hace falta caché."""
    return db_query(
        "SELECT modulo, rol, is_active FROM public.usuarios_sistema "
        "WHERE usuario = %s LIMIT 1",
        [usuario], fetch="one",
    )


def require_role(*modulos_permitidos: str):
    """Fábrica de dependencia: exige que el usuario de la sesión pertenezca a
    alguno de `modulos_permitidos` (o a "Global", que los cubre todos).

    Encadena con `require_session` (`Depends()` anidado): primero valida que
    haya sesión, y con ese usuario resuelto consulta su módulo/rol reales en
    `usuarios_sistema`. Lanza 403 si el módulo no coincide, si el usuario no
    existe o si está desactivado (`is_active = FALSE`).
    """

    def _dependencia(usuario: str = Depends(require_session)) -> str:
        fila = _fila_usuario(usuario)
        if not fila or not fila.get("is_active", True):
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="Usuario sin acceso o desactivado.",
            )
        modulo = fila.get("modulo")
        if modulo != "Global" and modulo not in modulos_permitidos:
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="Su módulo no tiene acceso a esta operación.",
            )
        return usuario

    return _dependencia


def require_admin_role(*modulos_permitidos: str):
    """Como `require_role`, y además exige `rol = 'Admin'` en el módulo del
    usuario. Para operaciones sensibles: borrar usuarios, restaurar backup,
    purgar retención, etc."""

    def _dependencia(usuario: str = Depends(require_session)) -> str:
        fila = _fila_usuario(usuario)
        if not fila or not fila.get("is_active", True):
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="Usuario sin acceso o desactivado.",
            )
        modulo = fila.get("modulo")
        if modulo != "Global" and modulo not in modulos_permitidos:
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="Su módulo no tiene acceso a esta operación.",
            )
        if fila.get("rol") != "Admin":
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="Esta operación requiere rol de administrador.",
            )
        return usuario

    return _dependencia
