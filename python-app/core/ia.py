"""Cliente de IA (OpenRouter) — el núcleo del Asistente del Archivo.

QUÉ ES Y QUÉ NO ES
------------------
Es la tubería que habla con el modelo. Nada de aquí conoce el Archivo ni RRHH: los datos
entran por `core.ia_tools`, que es lo único que toca la base.

LAS TRES REGLAS QUE SOSTIENEN ESTO
----------------------------------
1. La clave NUNCA sale del servidor. El navegador habla con /api/ia/*; es FastAPI quien
   llama a OpenRouter. Si la clave viajara al front, cualquiera la lee del bundle.
2. El prompt de sistema se arma AQUÍ y el historial del cliente se sanea. Solo se aceptan
   roles `user` y `assistant`: si dejáramos pasar un `system` desde el navegador, cualquiera
   reescribe la identidad del asistente desde la consola del inspector.
3. El modo decide QUÉ HERRAMIENTAS EXISTEN, y esa es la defensa real. El prompt es la
   primera línea y la más débil.

POR QUÉ OPENROUTER Y NO EL SDK NATIVO
-------------------------------------
OpenRouter es HTTP+JSON plano: se llama con `urllib` de la stdlib y no suma una dependencia
al bundle de Vercel (que tiene límite de tamaño). Además da acceso a todo el catálogo sin
cambiar código, y devuelve el COSTO REAL de cada llamada — no hay que estimarlo.
"""
import json
import logging
import os
import time
import urllib.error
import urllib.request

logger = logging.getLogger("app.ia")

# =============================================================================
# CONFIGURACIÓN
# =============================================================================

OPENROUTER_BASE = "https://openrouter.ai/api/v1/"

# Haiku 4.5 por defecto ($1/$5 por millón). El grueso del tráfico de un archivo son
# preguntas repetidas que se contestan con una búsqueda + un párrafo: para eso alcanza.
# Si hiciera falta más cabeza se sube desde el panel, sin tocar código.
MODELO_POR_DEFECTO = "anthropic/claude-haiku-4.5"

# Tope de vueltas del ciclo de herramientas. Cada vuelta es una llamada paga que reenvía
# toda la conversación: sin tope, un modelo que se obstine en buscar quema dinero sin fin.
MAX_VUELTAS = 5

# Tope de mensajes de historial que se reenvían. Sin esto la conversación se paga entera
# en cada mensaje y el costo crece al cuadrado.
#
# El público lleva menos: son consultas cortas, el volumen es mayor y no hace falta memoria
# larga. Y cuanto más corto el historial, menos superficie tiene una inyección para
# acumularse turno a turno. El editor lleva más porque encadena búsqueda → ficha → propuesta
# y perder el hilo a mitad de eso lo obliga a repetir consultas pagas.
MAX_HISTORIAL = {"publico": 10, "consulta": 20, "editor": 30}

# Supuesto para comparar modelos en el panel: cuánto sale UN mensaje típico.
TOKENS_MENSAJE_TIPICO = {"entrada": 2000, "salida": 250}


def _env(nombre: str, defecto: str = "") -> str:
    return os.environ.get(nombre, defecto)


def _de_bd(clave: str):
    """Lee un ajuste de `ia_config`. Devuelve None si no está o si la tabla aún no existe.

    Los ajustes operativos (modelo, tope de gasto, tope de tokens) viven en la base y NO en
    variables de entorno, y la variable queda solo como respaldo. El motivo es concreto: en
    Vercel cambiar una variable de entorno obliga a un redespliegue. Si el tope de gasto
    solo viviera ahí, subirlo un día de mucho uso sería un deploy — y nadie hace un deploy
    para eso, así que en la práctica el asistente se quedaría cortado hasta el día
    siguiente. En Diamond el mismo problema era peor (la config estaba cacheada y había que
    parchearla por SSH), y la solución fue la misma.
    """
    try:
        from database import db_query
        fila = db_query("SELECT valor FROM public.ia_config WHERE clave = %s LIMIT 1",
                        [clave], fetch="one")
        if fila and str(fila.get("valor") or "").strip():
            return fila["valor"].strip()
    except Exception:
        pass
    return None


def api_key() -> str:
    # La clave NO se lee de la base a propósito: un secreto no se guarda donde se guardan
    # los ajustes editables desde una pantalla.
    return _env("OPENROUTER_API_KEY").strip()


def habilitada() -> bool:
    return _env("IA_HABILITADA", "true").lower() not in ("false", "0", "no")


def max_tokens() -> int:
    """Tope de tokens por respuesta. La salida es el grueso del costo de un mensaje."""
    for valor in (_de_bd("max_tokens"), _env("IA_MAX_TOKENS", "1500")):
        try:
            n = int(float(valor))
            if 100 <= n <= 8000:
                return n
        except (TypeError, ValueError):
            continue
    return 1500


def tope_diario() -> float:
    """Tope de gasto por día, en dólares. El riesgo del día uno es el costo descontrolado."""
    for valor in (_de_bd("tope_diario"), _env("IA_TOPE_DIARIO", "1.00")):
        try:
            n = float(valor)
            if 0 <= n <= 1000:
                return n
        except (TypeError, ValueError):
            continue
    return 1.00


def estado() -> dict:
    """Nunca un 500 por configuración faltante: se degrada con un motivo legible."""
    if not habilitada():
        return {"disponible": False, "motivo": "El asistente está deshabilitado (IA_HABILITADA=false)."}
    if not api_key():
        return {"disponible": False, "motivo": "Falta OPENROUTER_API_KEY en las variables de entorno."}
    return {"disponible": True, "modelo": modelo_actual()}


def modelo_actual() -> str:
    """El modelo elegido desde el panel; si nadie eligió, el de la variable de entorno."""
    return _de_bd("modelo") or _env("OPENROUTER_MODEL", MODELO_POR_DEFECTO)


def modelo_existe(slug: str) -> bool:
    """Comprueba el slug contra el catálogo real antes de dejar guardarlo.

    Sin esto, un slug mal escrito (por ejemplo el id nativo `claude-haiku-4-5` en vez del de
    OpenRouter `anthropic/claude-haiku-4.5`) se guarda sin quejarse y el chat empieza a
    fallar con "modelo inexistente" en cada mensaje. El error aparecería lejos del lugar
    donde se cometió, que es la peor clase de error.

    Si el catálogo no se puede consultar, se deja pasar: es preferible confiar en quien
    administra a bloquear un cambio legítimo porque OpenRouter esté caído en ese momento.
    """
    try:
        return any(m["id"] == slug for m in catalogo_modelos())
    except Exception:
        logger.warning(f"IA: no se pudo validar el modelo '{slug}' contra el catálogo.")
        return True


# =============================================================================
# HTTP
# =============================================================================

def _post(cuerpo: dict) -> dict:
    datos = json.dumps(cuerpo).encode("utf-8")
    req = urllib.request.Request(
        OPENROUTER_BASE + "chat/completions",
        data=datos,
        headers={
            "Authorization": f"Bearer {api_key()}",
            "Content-Type": "application/json",
            # OpenRouter usa estos dos para atribuir el tráfico en su panel.
            "HTTP-Referer": _env("APP_URL", "https://ciencias-ucv-digital-archive.vercel.app"),
            "X-Title": "Archivo Ciencias UCV",
        },
        method="POST",
    )
    with urllib.request.urlopen(req, timeout=90) as r:
        return json.loads(r.read().decode("utf-8"))


_CATALOGO = {"datos": None, "ts": 0.0}
_CATALOGO_TTL = 6 * 3600  # el catálogo de OpenRouter cambia de semana en semana, no de minuto


def catalogo_modelos() -> list:
    """El catálogo real de OpenRouter, normalizado para el panel.

    No se adivinan slugs ni precios: se consultan. Los slugs de OpenRouter llevan prefijo de
    proveedor y punto (`anthropic/claude-haiku-4.5`), no son los ids nativos de Anthropic.

    Se cachea en memoria porque son ~350 modelos y lo consultan tanto la pantalla como la
    validación al guardar. En serverless el proceso se recicla y el caché se pierde solo:
    eso está bien, no hay nada que invalidar a mano.
    """
    ahora = time.time()
    if _CATALOGO["datos"] is not None and (ahora - _CATALOGO["ts"]) < _CATALOGO_TTL:
        return _CATALOGO["datos"]

    req = urllib.request.Request(
        OPENROUTER_BASE + "models",
        headers={"Authorization": f"Bearer {api_key()}"},
    )
    with urllib.request.urlopen(req, timeout=30) as r:
        data = json.loads(r.read().decode("utf-8")).get("data", [])

    mm = TOKENS_MENSAJE_TIPICO
    lista = []
    for m in data:
        if not m.get("id") or not m.get("pricing"):
            continue
        p = m["pricing"]
        nombre = m.get("name") or m["id"]
        partes = nombre.split(":", 1)
        empresa = partes[0].strip() if len(partes) == 2 else m["id"].split("/")[0].title()
        corto = partes[1].strip() if len(partes) == 2 else nombre

        def _mm(valor):
            try:
                return round(float(valor) * 1_000_000, 4)
            except (TypeError, ValueError):
                return None

        precio_in = _mm(p.get("prompt"))
        precio_out = _mm(p.get("completion"))
        precio_cache = _mm(p.get("input_cache_read"))
        params = m.get("supported_parameters") or []

        item = {
            "id": m["id"],
            "nombre": corto,
            "empresa": empresa,
            "descripcion": (m.get("description") or "")[:240],
            "contexto": int(m.get("context_length") or 0),
            "precio_in": precio_in,
            "precio_out": precio_out,
            # Sin `tools` el asistente NO puede consultar la base: queda reducido a conversar.
            # La pantalla tiene que gritarlo.
            "herramientas": "tools" in params,
            "cache": bool(precio_cache),
            "precio_cache": precio_cache,
            "entradas": (m.get("architecture") or {}).get("input_modalities") or ["text"],
            "gratis": precio_in == 0,
        }

        # EL NÚMERO QUE DE VERDAD COMPARA: cuánto sale MIL MENSAJES.
        # Los $/M engañan — un modelo con entrada baratísima y salida cara puede salir más
        # caro, porque en nuestro uso la salida pesa el grueso del costo. Y uno con caché le
        # gana a otro con precio de lista más bajo. Se usa el precio de caché como "efectivo"
        # porque, salvo la primera llamada, todas leen del caché.
        if precio_in is not None and precio_out is not None:
            efectivo = precio_cache if item["cache"] and precio_cache else precio_in
            por_mensaje = (mm["entrada"] * efectivo + mm["salida"] * precio_out) / 1_000_000
            item["costo_mensaje"] = round(por_mensaje, 8)
            item["costo_mil"] = round(por_mensaje * 1000, 4)
        else:
            item["costo_mensaje"] = None
            item["costo_mil"] = None

        lista.append(item)

    lista.sort(key=lambda x: (x["empresa"].lower(), x["nombre"].lower()))
    _CATALOGO["datos"] = lista
    _CATALOGO["ts"] = ahora
    return lista


# =============================================================================
# EL CICLO DE CONVERSACIÓN CON HERRAMIENTAS
# =============================================================================

def bloque_sistema(prompt: str) -> dict:
    """El turno de sistema, con CACHÉ DE PROMPT — la palanca de costo real.

    El prompt de sistema (identidad + catálogos + reglas) son ~2000 tokens que se reenvían
    ENTEROS en cada mensaje y no cambian nunca. Cacheados, esa parte cuesta ~10%: medido en
    otro proyecto, Sonnet CON caché salía más barato que Haiku SIN caché.

    Lo estable va SOLO aquí. Lo que cambia (la pregunta, los resultados de las herramientas)
    va después: si metiéramos la fecha de hoy en este bloque, el caché se invalidaría en cada
    mensaje y pagaríamos la escritura para nada.

    `cache_control` es de Anthropic; los modelos que no lo entienden ignoran el campo.
    """
    return {
        "role": "system",
        "content": [{"type": "text", "text": prompt, "cache_control": {"type": "ephemeral"}}],
    }


def sanear_historial(crudos, tope: int = 20) -> list:
    """Deja pasar solo turnos user/assistant y recorta a los últimos `tope`.

    Lo que llega del navegador es texto no confiable: aquí se decide la forma, no allá.
    """
    limpios = []
    for m in crudos or []:
        if not isinstance(m, dict):
            continue
        rol = m.get("rol") or m.get("role")
        contenido = (m.get("contenido") or m.get("content") or "").strip()
        if rol not in ("user", "assistant") or not contenido:
            continue
        limpios.append({"role": rol, "content": contenido[:8000]})
    return limpios[-tope:] if len(limpios) > tope else limpios


def conversar(prompt: str, mensajes: list, ctx: dict, ejecutar, definiciones) -> dict:
    """Un turno completo: pregunta → (herramientas)* → respuesta.

    El modelo puede pedir datos en vez de responder: contesta con `tool_calls`, se ejecuta
    la herramienta, se le devuelve el resultado y se le vuelve a preguntar, hasta que
    responda con texto o se agoten las vueltas.

    `ctx` es opaco aquí: este módulo no sabe qué es un perfil ni un módulo, solo se lo pasa
    a `definiciones` y `ejecutar`, que son quienes deciden los permisos.

    Devuelve siempre un dict; los errores llegan como {"error": ..., "status": ...}.
    """
    conversacion = [bloque_sistema(prompt)] + mensajes
    herramientas = definiciones(ctx)

    tokens = 0
    costo = 0.0
    usadas = []

    try:
        for _ in range(MAX_VUELTAS):
            datos = _post({
                "model": modelo_actual(),
                "messages": conversacion,
                "max_tokens": max_tokens(),
                "tools": herramientas,
                "usage": {"include": True},
            })

            uso = datos.get("usage") or {}
            tokens += int(uso.get("total_tokens") or 0)
            # Cada vuelta se paga: hay que sumarlas todas o el gasto queda subestimado.
            costo += float(uso.get("cost") or 0)

            mensaje = (datos.get("choices") or [{}])[0].get("message")
            if not mensaje:
                logger.warning(f"IA: respuesta sin mensaje: {datos}")
                return {"error": "El asistente respondió vacío. Intenta de nuevo.", "status": 502}

            if not mensaje.get("tool_calls"):
                texto = (mensaje.get("content") or "").strip()
                if not texto:
                    return {"error": "El asistente respondió vacío. Intenta de nuevo.", "status": 502}
                return {
                    "respuesta": texto,
                    "modelo": datos.get("model") or modelo_actual(),
                    "uso": {"total_tokens": tokens, "costo": round(costo, 6)},
                    "herramientas": usadas,
                }

            # El turno del asistente con sus tool_calls tiene que volver TAL CUAL: si no, el
            # modelo no reconoce a qué llamada corresponde cada resultado.
            conversacion.append(mensaje)

            for llamada in mensaje["tool_calls"]:
                fn = llamada.get("function") or {}
                nombre = fn.get("name") or ""
                # Los argumentos vienen como STRING JSON generado por el modelo: puede venir
                # malformado. Si no parsea, se le informa en vez de reventar.
                try:
                    args = json.loads(fn.get("arguments") or "{}")
                    if not isinstance(args, dict):
                        args = {}
                except (ValueError, TypeError):
                    args = {}

                resultado = ejecutar(nombre, args, ctx)
                usadas.append({"herramienta": nombre, "argumentos": args})
                logger.info(f"IA[{ctx.get('perfil')}/{ctx.get('usuario')}]: {nombre} {args}")

                conversacion.append({
                    "role": "tool",
                    "tool_call_id": llamada.get("id", ""),
                    "content": json.dumps(resultado, ensure_ascii=False, default=str),
                })

        logger.warning(f"IA: se agotaron las vueltas de herramientas ({usadas})")

        return {
            "respuesta": "Estuve consultando el archivo pero no logré cerrar la respuesta. "
                         "¿Podrías reformular la pregunta?",
            "modelo": modelo_actual(),
            "uso": {"total_tokens": tokens, "costo": round(costo, 6)},
            "herramientas": usadas,
        }

    except urllib.error.HTTPError as e:
        # El cuerpo del error de OpenRouter dice cosas útiles (clave inválida, sin crédito,
        # modelo inexistente). Se registra entero; al navegador va un mensaje corto, porque
        # el detalle puede tener rastros de la configuración.
        detalle = e.read().decode("utf-8", "replace")[:800]
        logger.error(f"IA: falló la llamada a OpenRouter ({e.code}): {detalle}")
        return {"error": "No se pudo contactar al asistente. Revisa el log del servidor.", "status": 502}
    except Exception as e:
        logger.error(f"IA: error inesperado: {e}")
        return {"error": "Error inesperado al consultar al asistente.", "status": 500}
