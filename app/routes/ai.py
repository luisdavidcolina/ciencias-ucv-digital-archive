"""Asistente IA — endpoints.

EL PERFIL SALE DE LA SESIÓN FIRMADA, NUNCA DEL CUERPO
-----------------------------------------------------
El sistema ya tiene sesión criptográfica (`require_session`: token HMAC en la cookie
`ds_session`). El perfil y los módulos del usuario se resuelven desde ahí y desde
`usuarios_sistema`. Nada de lo que mande el navegador influye en qué puede hacer el
asistente: si el perfil viajara en el cuerpo, cambiar una palabra en el inspector daría
acceso a los expedientes.

`/api/ia/chat` es el único endpoint con sesión OPCIONAL, y a propósito: sin sesión responde
en modo `publico`, que no tiene ninguna herramienta de personal. Todo lo demás
—adjuntar, aprobar, configurar— exige sesión.

EL TOPE DE GASTO CORTA ANTES DE GASTAR
--------------------------------------
Se comprueba antes de llamar al modelo, no después: la idea es no hacer la llamada. El
riesgo del día uno de cualquier módulo de IA es el costo descontrolado.
"""
import io
import json
import os
import time

from fastapi import (APIRouter, Body, Cookie, Depends, File, Form, Header,
                     HTTPException, UploadFile)
from fastapi.responses import JSONResponse

import storage
from core import ai, ai_prompts, ai_proposals, ai_tools
from core.security import verify_session_token
from database import db_query, log_event, logger
from routes.admin.deps import require_session

router = APIRouter(prefix="/api/ia", tags=["ia"])


# =============================================================================
# PERFIL Y PERMISOS
# =============================================================================

_MODULOS = {
    "Archivo": {"archivo"},
    "RRHH": {"rrhh"},
    "Global": {"archivo", "rrhh"},
}


def _contexto(usuario: str | None, conversacion_id=None) -> dict:
    """Traduce un usuario autenticado a lo que el asistente puede hacer.

    Sin usuario, o con uno que ya no está activo, el perfil es `publico`. No se lanza 401:
    un chat que se cae al expirar la sesión es un chat que la gente deja de usar. Degradar
    a público es seguro porque ese perfil no tiene ninguna herramienta de personal.
    """
    base = {"perfil": "publico", "usuario": None, "nombre_usuario": None,
            "modulos": set(), "conversacion_id": conversacion_id}
    if not usuario:
        return base

    fila = db_query(
        "SELECT usuario, nombre_usuario, modulo, rol FROM public.usuarios_sistema "
        "WHERE usuario = %s AND COALESCE(is_active, TRUE) LIMIT 1",
        [usuario], fetch="one",
    )
    if not fila:
        return base

    base["usuario"] = fila["usuario"]
    base["nombre_usuario"] = fila["nombre_usuario"]
    base["modulos"] = _MODULOS.get(fila["modulo"] or "", set())
    # El rol Admin es lo que habilita PROPONER cambios. Un usuario Normal consulta y nada
    # más: es la misma división que ya usa el resto del sistema, no una regla nueva.
    base["perfil"] = "editor" if (fila["rol"] or "") == "Admin" else "consulta"
    return base


def _sesion_opcional(
    ds_session: str | None = Cookie(default=None),
    x_session_token: str | None = Header(default=None),
) -> str | None:
    """Igual que `require_session` pero sin lanzar 401. Devuelve el usuario o None."""
    return verify_session_token(ds_session or x_session_token)


# =============================================================================
# GASTO
# =============================================================================

def _gasto_de_hoy() -> float:
    fila = db_query(
        "SELECT COALESCE(SUM(costo), 0) AS total FROM public.ia_mensajes "
        "WHERE created_at::date = CURRENT_DATE",
        fetch="one",
    )
    return float(fila["total"]) if fila else 0.0


# =============================================================================
# CONVERSACIONES
# =============================================================================

def _abrir_conversacion(conv_id, meta: dict) -> int:
    if conv_id:
        try:
            existe = db_query("SELECT id FROM public.ia_conversaciones WHERE id = %s",
                              [int(conv_id)], fetch="one")
            if existe:
                return int(conv_id)
        except (TypeError, ValueError):
            pass

    fila = db_query("""
        INSERT INTO public.ia_conversaciones (canal, modo, usuario, titulo, modelo, ultima_at, created_at)
        VALUES (%s, %s, %s, %s, %s, NOW(), NOW())
        RETURNING id
    """, [
        meta.get("canal", "web"), meta.get("perfil", "publico"), meta.get("usuario"),
        (meta.get("titulo") or "")[:160], meta.get("modelo"),
    ], fetch="one", commit=True)
    return fila["id"]


def _guardar_turno(conv_id: int, rol: str, contenido: str, extra=None):
    extra = extra or {}
    db_query("""
        INSERT INTO public.ia_mensajes
            (conversacion_id, rol, contenido, herramientas, modelo, tokens, costo, ms, created_at)
        VALUES (%s, %s, %s, %s, %s, %s, %s, %s, NOW())
    """, [
        conv_id, rol, contenido,
        json.dumps(extra["herramientas"], ensure_ascii=False) if extra.get("herramientas") else None,
        extra.get("modelo"), extra.get("tokens") or 0,
        extra.get("costo") or 0, extra.get("ms"),
    ], fetch="none", commit=True)

    db_query("""
        UPDATE public.ia_conversaciones
           SET mensajes = mensajes + 1, tokens = tokens + %s,
               costo = costo + %s, ultima_at = NOW()
         WHERE id = %s
    """, [extra.get("tokens") or 0, extra.get("costo") or 0, conv_id],
        fetch="none", commit=True)


# =============================================================================
# CHAT
# =============================================================================

@router.get("/disponible")
def disponible(usuario: str | None = Depends(_sesion_opcional)):
    """Nunca revienta por configuración faltante: la pantalla muestra un aviso legible."""
    est = ai.estado()
    if not est["disponible"]:
        return est

    ctx = _contexto(usuario)
    est["perfil"] = ctx["perfil"]
    est["modulos"] = sorted(ctx["modulos"])
    est["puede_escribir"] = ctx["perfil"] == "editor"
    # El chat devuelve enlaces `/api/files/<key>`, y ese endpoint exige además `?u=<usuario>`.
    # Sin esto los enlaces del asistente darían 401 al abrirlos, que es la peor forma de
    # fallar: el documento existe, el enlace es correcto y aun así no abre.
    est["usuario"] = ctx["usuario"]
    try:
        est["gasto_hoy"] = round(_gasto_de_hoy(), 6)
        est["tope_diario"] = ai.tope_diario()
    except Exception:
        pass
    return est


@router.post("/chat")
def chat(payload: dict = Body(...), usuario: str | None = Depends(_sesion_opcional)):
    est = ai.estado()
    if not est["disponible"]:
        raise HTTPException(status_code=503, detail=est["motivo"])

    ctx = _contexto(usuario)
    mensajes = ai.sanear_historial(payload.get("mensajes"), ai.MAX_HISTORIAL[ctx["perfil"]])
    if not mensajes:
        raise HTTPException(status_code=422, detail="No hay ningún mensaje que enviar.")

    gasto, tope = _gasto_de_hoy(), ai.tope_diario()
    if gasto >= tope:
        logger.warning(f"IA: rechazado por tope diario (${gasto:.4f} / ${tope})")
        raise HTTPException(
            status_code=429,
            detail=f"Se alcanzó el tope de gasto diario del asistente "
                   f"(${gasto:.4f} de ${tope:.2f}). Se restablece mañana.",
        )

    ultimo = mensajes[-1]["content"]
    conv_id = _abrir_conversacion(payload.get("conversacion_id"), {
        "canal": "web", "perfil": ctx["perfil"], "usuario": ctx["usuario"],
        "titulo": ultimo, "modelo": ai.modelo_actual(),
    })
    # Las herramientas de adjuntos y propuestas se anclan a la conversación: sin esto, un
    # adjunto de otro hilo sería alcanzable adivinando un id.
    ctx["conversacion_id"] = conv_id

    _guardar_turno(conv_id, "user", ultimo)

    t0 = time.time()
    r = ai.conversar(ai_prompts.prompt(ctx), mensajes, ctx,
                     ai_tools.ejecutar, ai_tools.definiciones)
    ms = int((time.time() - t0) * 1000)

    if "error" in r:
        return JSONResponse(status_code=r.get("status", 502), content={"detail": r["error"]})

    _guardar_turno(conv_id, "assistant", r["respuesta"], {
        "herramientas": r.get("herramientas"), "modelo": r.get("modelo"),
        "tokens": r["uso"]["total_tokens"], "costo": r["uso"]["costo"], "ms": ms,
    })

    # La redirección la decide el navegador con una ruta de una lista cerrada que valida el
    # servidor, no un enlace que el modelo haya escrito en el texto.
    for h in r.get("herramientas") or []:
        if h["herramienta"] == "ir_a":
            ruta = (h.get("argumentos") or {}).get("ruta")
            if ruta in ai_tools._RUTAS_VALIDAS:
                r["navegar_a"] = ruta

    r["conversacion_id"] = conv_id
    r["perfil"] = ctx["perfil"]
    # Las propuestas que este turno dejó pendientes, para que el chat pinte los botones.
    r["propuestas"] = ai_proposals.listar(conversacion_id=conv_id, estado="pendiente")
    return r


# =============================================================================
# ADJUNTOS — el usuario sube, el asistente solo mira
# =============================================================================

@router.post("/adjuntar")
async def adjuntar(
    file: UploadFile = File(...),
    conversacion_id: int = Form(...),
    usuario: str = Depends(require_session),
):
    """Sube un archivo desde el chat y lo deja disponible para el asistente.

    El asistente NO puede subir nada por su cuenta y no existe herramienta para hacerlo: la
    subida siempre la inicia una persona. Un modelo que pudiera escribir en el bucket es un
    modelo que puede llenarlo, y el bucket lo paga la Facultad.

    El archivo va a R2 igual que cualquier digitalizado, con las mismas validaciones de
    extensión y tamaño. Todavía no queda enganchado a ningún documento: para eso hace falta
    una propuesta aprobada.
    """
    ctx = _contexto(usuario)
    if ctx["perfil"] != "editor":
        raise HTTPException(status_code=403,
                            detail="Solo un administrador de módulo puede subir archivos al chat.")

    if not storage.is_configured():
        raise HTTPException(status_code=503,
                            detail="Almacenamiento no configurado (faltan las variables R2_*).")

    ext = os.path.splitext(file.filename or "")[1].lower()
    if ext not in storage.ALLOWED_EXTENSIONS:
        raise HTTPException(
            status_code=400,
            detail=f"Extensión no permitida: {ext or '(sin extensión)'}. "
                   f"Permitidas: {', '.join(sorted(storage.ALLOWED_EXTENSIONS))}",
        )

    contenido = await file.read()
    if not contenido:
        raise HTTPException(status_code=400, detail="El archivo está vacío.")
    if len(contenido) > storage.MAX_FILE_SIZE:
        raise HTTPException(status_code=413, detail="El archivo excede el límite de 25 MB.")

    conv = db_query("SELECT usuario FROM public.ia_conversaciones WHERE id = %s",
                    [conversacion_id], fetch="one")
    if not conv:
        raise HTTPException(status_code=404, detail="Esa conversación no existe.")
    # Un adjunto solo entra en la conversación de quien la abrió. Si no, cualquiera con
    # sesión podría dejar archivos en el hilo de otro.
    if conv["usuario"] != usuario:
        raise HTTPException(status_code=403, detail="Esa conversación no es tuya.")

    key = storage.build_object_key("ia", file.filename or "adjunto")
    try:
        storage.upload_fileobj(io.BytesIO(contenido), key,
                               content_type=file.content_type or "application/octet-stream")
    except Exception as e:
        raise HTTPException(status_code=502, detail=f"Error subiendo a R2: {type(e).__name__}")

    file_url = f"/api/files/{key}"
    fila = db_query("""
        INSERT INTO public.ia_adjuntos
            (conversacion_id, usuario, nombre_archivo, file_url, tamano_bytes, created_at)
        VALUES (%s, %s, %s, %s, %s, NOW())
        RETURNING id
    """, [conversacion_id, usuario, (file.filename or "adjunto")[:200], file_url, len(contenido)],
        fetch="one", commit=True)

    log_event(usuario, "Adjuntó archivo en chat IA", "IA",
              f"conversacion={conversacion_id} key={key} ({len(contenido)} bytes)")

    return {"ok": True, "adjunto_id": fila["id"], "file_url": file_url,
            "nombre_archivo": file.filename}


# =============================================================================
# BANDEJA DE PROPUESTAS
# =============================================================================

@router.get("/propuestas")
def propuestas(conversacion_id: int | None = None, estado: str = "pendiente",
               usuario: str = Depends(require_session)):
    return {"propuestas": ai_proposals.listar(conversacion_id, estado)}


@router.post("/propuesta/{propuesta_id}/aprobar")
def aprobar_propuesta(propuesta_id: int, usuario: str = Depends(require_session)):
    ctx = _contexto(usuario)
    if ctx["perfil"] != "editor":
        raise HTTPException(status_code=403,
                            detail="Solo un administrador de módulo puede aprobar cambios.")
    try:
        return ai_proposals.aprobar(propuesta_id, usuario, ctx["modulos"])
    except ai_proposals.PropuestaError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        logger.error(f"IA: falló la ejecución de la propuesta {propuesta_id}: {e}")
        raise HTTPException(status_code=500,
                            detail="No se pudo aplicar el cambio. Revisa el log del servidor.")


@router.post("/propuesta/{propuesta_id}/rechazar")
def rechazar_propuesta(propuesta_id: int, usuario: str = Depends(require_session)):
    try:
        return ai_proposals.rechazar(propuesta_id, usuario)
    except ai_proposals.PropuestaError as e:
        raise HTTPException(status_code=400, detail=str(e))


# =============================================================================
# HISTORIAL Y ADMINISTRACIÓN
# =============================================================================

def _es_global(ctx: dict) -> bool:
    return ctx["modulos"] == {"archivo", "rrhh"}


def _puede_ver_conversacion(ctx: dict, cab: dict) -> bool:
    """Tu conversación es tuya. Solo un administrador Global ve las ajenas.

    Una conversación puede tener el nombre de un empleado, el contenido de un expediente o
    una consulta que alguien no querría que su jefe leyera. Que un admin de módulo pudiera
    abrir el hilo de un compañero convertiría el asistente en una herramienta de vigilancia,
    y eso haría que la gente dejara de usarlo — que es la peor forma de perder la función.

    El Global sí puede: es quien responde por el gasto y por lo que el asistente contesta, y
    sin poder leer los hilos donde falla no hay forma de mejorarlo.
    """
    return _es_global(ctx) or (cab.get("usuario") and cab["usuario"] == ctx["usuario"])


@router.get("/conversaciones")
def conversaciones(todas: bool = False, limite: int = 30,
                   usuario: str = Depends(require_session)):
    """Las conversaciones del usuario. `todas=true` solo lo honra un admin Global."""
    ctx = _contexto(usuario)
    ver_todas = todas and _es_global(ctx)

    filas = db_query("""
        SELECT id, titulo, modo AS perfil, modelo, mensajes, tokens, costo, usuario,
               TO_CHAR(ultima_at, 'YYYY-MM-DD HH24:MI') AS ultima
        FROM public.ia_conversaciones
        WHERE (%s OR usuario = %s)
        ORDER BY ultima_at DESC NULLS LAST
        LIMIT %s
    """, [ver_todas, usuario, max(1, min(limite, 100))], fetch="all")
    return {"conversaciones": filas, "viendo_todas": ver_todas}


@router.get("/conversacion/{conv_id}")
def conversacion(conv_id: int, usuario: str = Depends(require_session)):
    """Restaura una conversación completa, para retomarla desde donde quedó."""
    ctx = _contexto(usuario)
    cab = db_query("SELECT * FROM public.ia_conversaciones WHERE id = %s", [conv_id], fetch="one")
    if not cab:
        raise HTTPException(status_code=404, detail="Esa conversación no existe.")
    if not _puede_ver_conversacion(ctx, cab):
        # 404 y no 403: a quien no debe verla tampoco se le confirma que existe.
        raise HTTPException(status_code=404, detail="Esa conversación no existe.")

    msgs = db_query("""
        SELECT rol, contenido, herramientas, tokens, costo, ms,
               TO_CHAR(created_at, 'YYYY-MM-DD HH24:MI') AS hora
        FROM public.ia_mensajes WHERE conversacion_id = %s ORDER BY id
    """, [conv_id], fetch="all")

    # Las herramientas viajan como texto JSON en la base; se devuelven ya parseadas para
    # que el chat pueda pintar la traza al restaurar, igual que cuando llegó en vivo.
    for m in msgs:
        if m.get("herramientas"):
            try:
                m["herramientas"] = json.loads(m["herramientas"])
            except (ValueError, TypeError):
                m["herramientas"] = None

    return {
        "conversacion": cab,
        "mensajes": msgs,
        # Las propuestas que quedaron sin resolver siguen vivas al retomar el hilo.
        "propuestas": ai_proposals.listar(conversacion_id=conv_id, estado="pendiente"),
    }


@router.delete("/conversacion/{conv_id}")
def borrar_conversacion(conv_id: int, usuario: str = Depends(require_session)):
    """Borra la conversación y, en cascada, sus mensajes y adjuntos."""
    ctx = _contexto(usuario)
    cab = db_query("SELECT usuario FROM public.ia_conversaciones WHERE id = %s",
                   [conv_id], fetch="one")
    if not cab:
        raise HTTPException(status_code=404, detail="Esa conversación no existe.")
    if not _puede_ver_conversacion(ctx, cab):
        raise HTTPException(status_code=404, detail="Esa conversación no existe.")

    db_query("DELETE FROM public.ia_conversaciones WHERE id = %s", [conv_id],
             fetch="none", commit=True)
    log_event(usuario, "Borró conversación IA", "IA", f"conversacion_id={conv_id}")
    # Los objetos en R2 NO se borran: pueden estar ya enganchados a un documento por una
    # propuesta aprobada, y borrarlos dejaría la ficha apuntando al vacío. Limpiar los
    # huérfanos es una tarea aparte, no un efecto secundario de vaciar un chat.
    return {"ok": True}


@router.get("/gastos")
def gastos(dias: int = 30, usuario: str = Depends(require_session)):
    dias = max(1, min(dias, 365))
    por_dia = db_query("""
        SELECT TO_CHAR(created_at::date, 'YYYY-MM-DD') AS dia,
               COUNT(*) AS mensajes, SUM(tokens) AS tokens, ROUND(SUM(costo), 6) AS costo
        FROM public.ia_mensajes
        WHERE rol = 'assistant' AND created_at >= CURRENT_DATE - (%s || ' days')::interval
        GROUP BY created_at::date ORDER BY created_at::date DESC
    """, [dias], fetch="all")
    total = db_query("""
        SELECT COUNT(*) AS mensajes, COALESCE(SUM(tokens),0) AS tokens,
               ROUND(COALESCE(SUM(costo),0), 6) AS costo
        FROM public.ia_mensajes WHERE rol = 'assistant'
    """, fetch="one")
    return {"por_dia": por_dia, "historico": total,
            "hoy": round(_gasto_de_hoy(), 6), "tope_diario": ai.tope_diario()}


@router.get("/modelos")
def modelos(usuario: str = Depends(require_session)):
    """El catálogo real de OpenRouter, con el costo por mil mensajes ya calculado."""
    if not ai.api_key():
        raise HTTPException(status_code=503, detail="Falta OPENROUTER_API_KEY.")
    try:
        lista = ai.catalogo_modelos()
    except Exception as e:
        logger.error(f"IA: no se pudo traer el catálogo de OpenRouter: {e}")
        raise HTTPException(status_code=502, detail="No se pudo consultar el catálogo de modelos.")
    return {"actual": ai.modelo_actual(), "por_defecto": ai.MODELO_POR_DEFECTO,
            "total": len(lista), "modelos": lista, "supuesto": ai.TOKENS_MENSAJE_TIPICO}


_CLAVES_CONFIG = {"modelo", "nombre", "tono", "conocimiento", "reglas",
                  "tope_diario", "max_tokens"}


@router.get("/config")
def ver_config(usuario: str = Depends(require_session)):
    cfg = ai_prompts.config()
    cfg["modelo"] = ai.modelo_actual()
    cfg["tope_diario"] = ai.tope_diario()
    cfg["max_tokens"] = ai.max_tokens()
    return cfg


@router.post("/config")
def guardar_config(payload: dict = Body(...), usuario: str = Depends(require_session)):
    # El cerebro del asistente lo ve todo el mundo que lo use: cambiarlo es un acto de
    # administración global, no de módulo.
    ctx = _contexto(usuario)
    if ctx["modulos"] != {"archivo", "rrhh"}:
        raise HTTPException(status_code=403,
                            detail="Solo un administrador Global puede configurar el asistente.")

    # Se valida ANTES de guardar nada: si el modelo es inválido no se aplica ni el resto,
    # para que no quede una configuración a medias sin que nadie se entere.
    if "modelo" in payload:
        slug = str(payload["modelo"] or "").strip()
        if not slug:
            raise HTTPException(status_code=422, detail="El modelo no puede ir vacío.")
        if not ai.modelo_existe(slug):
            raise HTTPException(
                status_code=422,
                detail=f"'{slug}' no existe en OpenRouter. Los slugs llevan prefijo de "
                       f"proveedor y punto, por ejemplo 'anthropic/claude-haiku-4.5'.",
            )
        payload["modelo"] = slug

    if "tope_diario" in payload:
        try:
            tope = float(payload["tope_diario"])
        except (TypeError, ValueError):
            raise HTTPException(status_code=422, detail="El tope diario debe ser un número.")
        if not 0 <= tope <= 1000:
            raise HTTPException(status_code=422, detail="El tope diario debe estar entre 0 y 1000 dólares.")
        payload["tope_diario"] = f"{tope:.4f}"

    if "max_tokens" in payload:
        try:
            mt = int(float(payload["max_tokens"]))
        except (TypeError, ValueError):
            raise HTTPException(status_code=422, detail="El tope de tokens debe ser un número.")
        if not 100 <= mt <= 8000:
            raise HTTPException(status_code=422, detail="El tope de tokens debe estar entre 100 y 8000.")
        payload["max_tokens"] = str(mt)

    guardadas = []
    for clave in _CLAVES_CONFIG:
        if clave not in payload:
            continue
        db_query("""
            INSERT INTO public.ia_config (clave, valor, updated_at, updated_by)
            VALUES (%s, %s, NOW(), %s)
            ON CONFLICT (clave) DO UPDATE
               SET valor = EXCLUDED.valor, updated_at = NOW(), updated_by = EXCLUDED.updated_by
        """, [clave, str(payload[clave] or "")[:20000], usuario], fetch="none", commit=True)
        guardadas.append(clave)

    log_event(usuario, "Actualizó configuración de IA", "IA", ", ".join(guardadas))
    return {"ok": True, "guardadas": guardadas, "modelo": ai.modelo_actual()}
