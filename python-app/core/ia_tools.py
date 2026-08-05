"""Las herramientas del asistente: lo único con lo que puede tocar datos reales.

LA REGLA QUE SOSTIENE TODO ESTO
-------------------------------
El modelo NUNCA escribe SQL ni recibe una conexión. Solo puede llamar a las funciones de
abajo, con los argumentos que declara el esquema. Todo lo que llega del modelo es texto no
confiable y se valida antes de tocar la base.

LECTURA LIBRE, ESCRITURA POR PROPUESTA
--------------------------------------
Ninguna herramienta modifica el archivo directamente. Las de escritura crean una PROPUESTA
en `ia_propuestas` que una persona tiene que aprobar desde el chat. El bot propone; un
humano ejecuta.

No es exceso de celo. Un modelo que se equivoca al leer da una respuesta errónea que el
usuario descarta; uno que se equivoca al escribir corrompe la ficha de un documento
institucional y nadie se entera hasta que alguien la busca. Además, la propuesta deja
escrito qué se pidió, quién lo aprobó y cuándo — que es exactamente lo que un archivo
necesita poder demostrar.

QUÉ HERRAMIENTA VE CADA PERFIL — LO MÁS IMPORTANTE DE ESTE ARCHIVO
-----------------------------------------------------------------
    publico   Sin sesión.               Solo el Archivo institucional, solo lectura.
    consulta  Sesión, rol Normal.       Lectura de SUS módulos.
    editor    Sesión, rol Admin.        Lectura + proponer cambios en SUS módulos.

Y encima del perfil hay un segundo filtro: el MÓDULO. Un administrador del Archivo no ve
las herramientas de RRHH aunque su perfil sea `editor`, porque su usuario no tiene ese
módulo. Los dos filtros son independientes y se aplican los dos.

Ésta es la defensa de verdad. El prompt es la primera línea y la más débil; lo que impide
que el chat público filtre un expediente es que la herramienta NO EXISTE para ese perfil.
"""
import json

from database import db_query

_LIMITE_MAX = 25

# El orden importa: `_perfil_alcanza` compara posiciones.
_ESCALA = {"publico": 0, "consulta": 1, "editor": 2}


def _perfil_alcanza(perfil, minimo):
    return _ESCALA.get(perfil, 0) >= _ESCALA.get(minimo, 0)


def _limite(args, defecto=8):
    try:
        n = int(args.get("limite") or defecto)
    except (TypeError, ValueError):
        n = defecto
    return max(1, min(n, _LIMITE_MAX))


def _texto(args, clave, largo=200):
    v = args.get(clave)
    return str(v).strip()[:largo] if v not in (None, "") else None


def _fn(nombre, descripcion, propiedades=None, requeridos=None):
    return {
        "type": "function",
        "function": {
            "name": nombre,
            "description": descripcion,
            "parameters": {
                "type": "object",
                "properties": propiedades or {},
                "required": requeridos or [],
            },
        },
    }


# =============================================================================
# REGISTRO DE HERRAMIENTAS
# =============================================================================
# Cada entrada declara, además del esquema, QUIÉN puede usarla:
#   perfil : el mínimo necesario ('publico' | 'consulta' | 'editor')
#   modulo : 'archivo', 'rrhh' o None (no depende de módulo)
#
# Una herramienta nueva sin estos dos campos no compila el registro. Es a propósito:
# el modo de fallo que hay que evitar es agregar una capacidad y olvidar restringirla.

_REGISTRO = []


def _registrar(perfil, modulo, esquema, manejador):
    _REGISTRO.append({
        "nombre": esquema["function"]["name"],
        "perfil": perfil,
        "modulo": modulo,
        "esquema": esquema,
        "manejador": manejador,
    })


def definiciones(ctx: dict) -> list:
    """El subconjunto de herramientas que existe para este usuario concreto."""
    perfil = ctx.get("perfil", "publico")
    modulos = ctx.get("modulos") or set()
    fuera = []
    for h in _REGISTRO:
        if not _perfil_alcanza(perfil, h["perfil"]):
            continue
        if h["modulo"] and h["modulo"] not in modulos:
            continue
        fuera.append(h["esquema"])
    return fuera


def ejecutar(nombre: str, args: dict, ctx: dict) -> dict:
    """Despacha la herramienta, revalidando el permiso.

    La comprobación se repite AQUÍ además de en `definiciones`. Es deliberado: si mañana
    alguien arma el esquema con un contexto equivocado, el modelo podría pedir una
    herramienta que no le toca y aquí igual se rechaza. Dos cerrojos para la misma puerta,
    porque uno se olvida.
    """
    entrada = next((h for h in _REGISTRO if h["nombre"] == nombre), None)
    if entrada is None:
        return {"error": f"La herramienta '{nombre}' no existe."}

    perfil = ctx.get("perfil", "publico")
    if not _perfil_alcanza(perfil, entrada["perfil"]):
        return {"error": "No tienes permiso para esa acción con tu tipo de usuario."}
    if entrada["modulo"] and entrada["modulo"] not in (ctx.get("modulos") or set()):
        return {"error": f"Tu usuario no tiene acceso al módulo {entrada['modulo']}."}

    try:
        return entrada["manejador"](args or {}, ctx)
    except Exception as e:
        return {"error": f"No se pudo completar la consulta: {e}"}


# =============================================================================
# LECTURA — ARCHIVO (público)
# =============================================================================

def _buscar_archivo(args, ctx):
    where = ["da.deleted_at IS NULL"]
    params = []

    termino = _texto(args, "termino")
    if termino:
        # FTS en español con fallback a ILIKE: el mismo patrón que la búsqueda pública del
        # sistema, para que el asistente y la pantalla no den resultados distintos.
        where.append("""(
            to_tsvector('spanish', COALESCE(da.titulo,'') || ' ' || COALESCE(da.abstract,'') || ' '
                        || COALESCE(da.autor,'') || ' ' || COALESCE(da.tesauro_primario,'') || ' '
                        || COALESCE(da.personas_relacionadas,'')) @@ plainto_tsquery('spanish', %s)
            OR da.titulo ILIKE %s OR da.abstract ILIKE %s OR da.autor ILIKE %s
        )""")
        params += [termino, f"%{termino}%", f"%{termino}%", f"%{termino}%"]

    tipo = _texto(args, "tipo_documento", 120)
    if tipo:
        where.append("(td.nombre ILIKE %s OR td.nombre_corto ILIKE %s OR da.tesauro_primario ILIKE %s)")
        params += [f"%{tipo}%", f"%{tipo}%", f"%{tipo}%"]

    autor = _texto(args, "autor", 120)
    if autor:
        where.append("da.autor ILIKE %s")
        params.append(f"%{autor}%")

    palabra = _texto(args, "palabra_clave", 120)
    if palabra:
        where.append("""EXISTS (
            SELECT 1 FROM public.archivo_descriptores ax
            JOIN public.descriptores_libres dx ON dx.id_descriptor = ax.id_descriptor
            WHERE ax.id_archivo = da.id_archivo AND dx.nombre ILIKE %s)""")
        params.append(f"%{palabra}%")

    for clave, op in (("anio_desde", ">="), ("anio_hasta", "<=")):
        if args.get(clave):
            try:
                where.append(f"EXTRACT(YEAR FROM da.fecha_documento) {op} %s")
                params.append(int(args[clave]))
            except (TypeError, ValueError):
                pass

    if args.get("solo_digitalizados"):
        where.append("da.file_url IS NOT NULL AND da.file_url <> ''")

    params.append(_limite(args))

    filas = db_query(f"""
        SELECT da.id_archivo AS id, da.titulo,
               COALESCE(da.autor,'')                     AS autor,
               TO_CHAR(da.fecha_documento,'YYYY-MM-DD')  AS fecha,
               COALESCE(td.nombre_corto, da.tesauro_primario, 'Sin tipo') AS tipo,
               COALESCE(da.tesauro_secundario,'')        AS clasificacion,
               COALESCE(da.ubicacion,'')                 AS ubicacion_fisica,
               COALESCE(da.file_url,'')                  AS enlace,
               LEFT(COALESCE(da.abstract,''), 300)       AS resumen
        FROM public.datos_archivo da
        LEFT JOIN public.tipo_documento td ON td.id = da.id_tipo_documento
        WHERE {' AND '.join(where)}
        ORDER BY da.fecha_documento DESC NULLS LAST, da.id_archivo DESC
        LIMIT %s
    """, params, fetch="all")

    return {
        "encontrados": len(filas),
        "documentos": filas,
        "nota": "'enlace' es la URL del documento digitalizado; si viene vacío, el documento "
                "solo existe en físico y hay que buscarlo en 'ubicacion_fisica'.",
    }


def _ver_documento(args, ctx):
    try:
        doc_id = int(args.get("id"))
    except (TypeError, ValueError):
        return {"error": "Falta un id numérico válido."}

    if (args.get("modulo") or "archivo").lower() == "rrhh":
        # La ficha de RRHH trae datos personales: se exige el módulo, no basta el perfil.
        if "rrhh" not in (ctx.get("modulos") or set()):
            return {"error": "Tu usuario no tiene acceso a los expedientes de RRHH."}
        fila = db_query("""
            SELECT dr.id_rrhh AS id, dr.titulo, COALESCE(dr.abstract,'') AS resumen,
                   COALESCE(dr.autor,'') AS autor,
                   TO_CHAR(dr.fecha_documento,'YYYY-MM-DD') AS fecha,
                   TO_CHAR(dr.fecha_vencimiento,'YYYY-MM-DD') AS vence,
                   COALESCE(td.nombre_corto,'') AS tipo, COALESCE(c.nombre,'') AS parte,
                   COALESCE(dr.ubicacion,'') AS ubicacion_fisica,
                   COALESCE(dr.file_url,'') AS enlace,
                   COALESCE(dr.numero_folio,'') AS folio, dr.numero_paginas AS paginas,
                   COALESCE(dr.notas,'') AS notas,
                   e.cedula, e.nombres || ' ' || e.apellidos AS empleado
            FROM public.datos_rrhh dr
            LEFT JOIN public.tipo_documento td ON td.id = dr.id_tipo_documento
            LEFT JOIN public.categoria c       ON c.id  = td.id_categoria
            LEFT JOIN public.empleados e       ON e.id  = dr.empleado_id
            WHERE dr.id_rrhh = %s
        """, [doc_id], fetch="one")
        return fila or {"error": "No existe un documento de RRHH con ese id."}

    fila = db_query("""
        SELECT da.id_archivo AS id, da.titulo, COALESCE(da.abstract,'') AS resumen,
               COALESCE(da.autor,'') AS autor,
               TO_CHAR(da.fecha_documento,'YYYY-MM-DD') AS fecha,
               TO_CHAR(da.fecha_vencimiento,'YYYY-MM-DD') AS vence,
               COALESCE(td.nombre,'') AS tipo,
               COALESCE(da.tesauro_secundario,'') AS clasificacion,
               COALESCE(da.personas_relacionadas,'') AS personas_relacionadas,
               COALESCE(da.ubicacion,'') AS ubicacion_fisica,
               COALESCE(da.file_url,'') AS enlace,
               COALESCE(da.numero_folio,'') AS folio, da.numero_paginas AS paginas,
               COALESCE(da.soporte,'') AS soporte, COALESCE(da.notas,'') AS notas,
               COALESCE(da.status,'') AS status,
               COALESCE(STRING_AGG(dl.nombre, ', '), '') AS palabras_clave
        FROM public.datos_archivo da
        LEFT JOIN public.tipo_documento td       ON td.id = da.id_tipo_documento
        LEFT JOIN public.archivo_descriptores ad ON ad.id_archivo = da.id_archivo
        LEFT JOIN public.descriptores_libres dl  ON dl.id_descriptor = ad.id_descriptor
        WHERE da.id_archivo = %s AND da.deleted_at IS NULL
        GROUP BY da.id_archivo, td.nombre
    """, [doc_id], fetch="one")
    return fila or {"error": "No existe un documento con ese id en el Archivo."}


def _listar_tipos(args, ctx):
    if (args.get("modulo") or "archivo").lower() == "rrhh":
        filas = db_query("""
            SELECT td.id, td.nombre_corto AS tipo, c.nombre AS parte, COUNT(dr.id_rrhh) AS documentos
            FROM public.tipo_documento td
            JOIN public.categoria c ON c.id = td.id_categoria
            LEFT JOIN public.datos_rrhh dr ON dr.id_tipo_documento = td.id
            WHERE c.slug <> 'archivo'
            GROUP BY td.id, td.nombre_corto, c.nombre
            ORDER BY c.nombre, td.nombre_corto
        """, fetch="all")
        return {"modulo": "rrhh", "tipos": filas}

    filas = db_query("""
        SELECT td.id, td.nombre_corto AS tipo, td.nombre AS nombre_completo,
               td.plazo_retencion_anios AS retencion_anios,
               COUNT(da.id_archivo) AS documentos
        FROM public.tipo_documento td
        JOIN public.categoria c ON c.id = td.id_categoria AND c.slug = 'archivo'
        LEFT JOIN public.datos_archivo da
               ON da.id_tipo_documento = td.id AND da.deleted_at IS NULL
        GROUP BY td.id, td.nombre_corto, td.nombre, td.plazo_retencion_anios
        ORDER BY documentos DESC, td.nombre_corto
    """, fetch="all")
    # El id viaja porque las herramientas de propuesta lo piden: sin él, el modelo tendría
    # que adivinar a qué tipo se refiere y adivinar es exactamente lo que no debe hacer.
    return {"modulo": "archivo", "tipos": filas,
            "nota": "Usa el 'id' del tipo al proponer o modificar un documento."}


def _listar_palabras_clave(args, ctx):
    filas = db_query("""
        SELECT dl.nombre AS palabra_clave, COUNT(ad.id_archivo) AS documentos
        FROM public.descriptores_libres dl
        LEFT JOIN public.archivo_descriptores ad ON ad.id_descriptor = dl.id_descriptor
        GROUP BY dl.nombre
        ORDER BY documentos DESC, dl.nombre
        LIMIT %s
    """, [_limite(args, 20)], fetch="all")
    return {"palabras_clave": filas}


def _estadisticas(args, ctx):
    fila = db_query("""
        SELECT
            (SELECT COUNT(*) FROM public.datos_archivo WHERE deleted_at IS NULL) AS documentos_archivo,
            (SELECT COUNT(*) FROM public.datos_archivo
              WHERE deleted_at IS NULL AND file_url IS NOT NULL AND file_url <> '') AS archivo_digitalizados,
            (SELECT COUNT(*) FROM public.datos_rrhh)   AS documentos_rrhh,
            (SELECT COUNT(*) FROM public.empleados)    AS empleados,
            (SELECT COUNT(*) FROM public.descriptores_libres) AS palabras_clave,
            (SELECT MIN(EXTRACT(YEAR FROM fecha_documento))::int FROM public.datos_archivo
              WHERE deleted_at IS NULL AND fecha_documento IS NOT NULL) AS anio_mas_antiguo,
            (SELECT MAX(EXTRACT(YEAR FROM fecha_documento))::int FROM public.datos_archivo
              WHERE deleted_at IS NULL AND fecha_documento IS NOT NULL) AS anio_mas_reciente
    """, fetch="one")
    return fila or {}


_RUTAS_VALIDAS = {
    "/archivo", "/rrhh", "/admin/archivo", "/admin/rrhh",
    "/admin/sistema", "/admin/ia", "/investigacion", "/ayuda",
}


def _ir_a(args, ctx):
    ruta = _texto(args, "ruta", 60) or ""
    if ruta not in _RUTAS_VALIDAS:
        return {"error": f"Ruta no válida. Disponibles: {', '.join(sorted(_RUTAS_VALIDAS))}."}
    # El frontend lee `navegar_a` de la respuesta y redirige. La ruta sale de una lista
    # cerrada que valida el servidor, no de un enlace que el modelo escriba en el texto.
    return {"navegar_a": ruta, "ok": True}


# =============================================================================
# LECTURA — RRHH (requiere sesión y módulo)
# =============================================================================

def _buscar_empleado(args, ctx):
    where = ["1=1"]
    params = []

    termino = _texto(args, "termino", 120)
    if termino:
        where.append("(v.persona_raw ILIKE %s OR v.cedula ILIKE %s)")
        params += [f"%{termino}%", f"%{termino}%"]

    depto = _texto(args, "departamento", 120)
    if depto:
        where.append("v.departamento ILIKE %s")
        params.append(f"%{depto}%")

    estado = _texto(args, "estado", 60)
    if estado:
        where.append("v.estado ILIKE %s")
        params.append(f"%{estado}%")

    params.append(_limite(args))

    filas = db_query(f"""
        SELECT v.cedula, v.persona_raw AS nombre, v.cargo, v.departamento, v.estado,
               v.fecha_ingreso, v.doc_count AS documentos_en_expediente
        FROM public.vw_rrhh_persona_index v
        WHERE {' AND '.join(where)}
        ORDER BY v.persona_raw
        LIMIT %s
    """, params, fetch="all")
    return {"encontrados": len(filas), "empleados": filas}


def _expediente_empleado(args, ctx):
    cedula = _texto(args, "cedula", 20)
    if not cedula:
        return {"error": "Falta la cédula."}

    persona = db_query("""
        SELECT cedula, persona_raw AS nombre, cargo, departamento, estado, fecha_ingreso
        FROM public.vw_rrhh_persona_index WHERE cedula = %s
    """, [cedula], fetch="one")
    if not persona:
        return {"error": f"No hay ningún empleado con cédula {cedula}."}

    docs = db_query("""
        SELECT dr.id_rrhh AS id, dr.titulo,
               COALESCE(c.nombre, 'Sin parte')          AS parte,
               COALESCE(td.nombre_corto,'')             AS tipo,
               TO_CHAR(dr.fecha_documento,'YYYY-MM-DD') AS fecha,
               TO_CHAR(dr.fecha_vencimiento,'YYYY-MM-DD') AS vence,
               COALESCE(dr.ubicacion,'')                AS ubicacion_fisica,
               COALESCE(dr.file_url,'')                 AS enlace
        FROM public.datos_rrhh dr
        JOIN public.empleados e            ON e.id  = dr.empleado_id
        LEFT JOIN public.tipo_documento td ON td.id = dr.id_tipo_documento
        LEFT JOIN public.categoria c       ON c.id  = td.id_categoria
        WHERE e.cedula = %s
        ORDER BY c.nombre, dr.fecha_documento DESC NULLS LAST
    """, [cedula], fetch="all")

    por_parte = {}
    for d in docs:
        por_parte.setdefault(d["parte"], []).append(d)

    return {"empleado": persona, "total_documentos": len(docs), "expediente": por_parte}


def _buscar_documento_rrhh(args, ctx):
    termino = _texto(args, "termino")
    where = ["1=1"]
    params = []
    if termino:
        where.append("(dr.titulo ILIKE %s OR dr.abstract ILIKE %s OR dr.notas ILIKE %s)")
        params += [f"%{termino}%"] * 3
    params.append(_limite(args))

    filas = db_query(f"""
        SELECT dr.id_rrhh AS id, dr.titulo,
               COALESCE(td.nombre_corto,'') AS tipo, COALESCE(c.nombre,'') AS parte,
               TO_CHAR(dr.fecha_documento,'YYYY-MM-DD') AS fecha,
               COALESCE(dr.file_url,'') AS enlace,
               e.cedula, e.nombres || ' ' || e.apellidos AS empleado
        FROM public.datos_rrhh dr
        LEFT JOIN public.tipo_documento td ON td.id = dr.id_tipo_documento
        LEFT JOIN public.categoria c       ON c.id  = td.id_categoria
        LEFT JOIN public.empleados e       ON e.id  = dr.empleado_id
        WHERE {' AND '.join(where)}
        ORDER BY dr.fecha_documento DESC NULLS LAST
        LIMIT %s
    """, params, fetch="all")
    return {"encontrados": len(filas), "documentos": filas}


def _documentos_por_vencer(args, ctx):
    try:
        dias = max(1, min(int(args.get("dias") or 90), 1825))
    except (TypeError, ValueError):
        dias = 90
    limite = _limite(args, 15)

    filas = db_query("""
        SELECT * FROM (
            SELECT 'Archivo' AS modulo, da.id_archivo AS id, da.titulo,
                   TO_CHAR(da.fecha_vencimiento,'YYYY-MM-DD') AS vence,
                   (da.fecha_vencimiento - CURRENT_DATE) AS dias_restantes,
                   '' AS empleado
            FROM public.datos_archivo da
            WHERE da.deleted_at IS NULL AND da.fecha_vencimiento IS NOT NULL
              AND da.fecha_vencimiento <= CURRENT_DATE + (%s || ' days')::interval
            UNION ALL
            SELECT 'RRHH', dr.id_rrhh, dr.titulo,
                   TO_CHAR(dr.fecha_vencimiento,'YYYY-MM-DD'),
                   (dr.fecha_vencimiento - CURRENT_DATE),
                   COALESCE(e.nombres || ' ' || e.apellidos, '')
            FROM public.datos_rrhh dr
            LEFT JOIN public.empleados e ON e.id = dr.empleado_id
            WHERE dr.fecha_vencimiento IS NOT NULL
              AND dr.fecha_vencimiento <= CURRENT_DATE + (%s || ' days')::interval
        ) t
        ORDER BY dias_restantes
        LIMIT %s
    """, [dias, dias, limite], fetch="all")

    return {
        "ventana_dias": dias,
        "encontrados": len(filas),
        "documentos": filas,
        "nota": "dias_restantes negativo significa que el documento YA venció.",
    }


# =============================================================================
# ADJUNTOS — lo que el usuario suelta en el chat
# =============================================================================

def _mis_adjuntos(args, ctx):
    """Los archivos que el usuario subió en esta conversación.

    El asistente NO puede subir nada por su cuenta: la subida la hace la persona desde el
    chat y queda en `ia_adjuntos`. Aquí solo los ve para poder proponer engancharlos a un
    documento. Un modelo que pudiera escribir en el bucket es un modelo que puede llenarlo.
    """
    filas = db_query("""
        SELECT id, nombre_archivo, file_url, tamano_bytes,
               TO_CHAR(created_at,'YYYY-MM-DD HH24:MI') AS subido
        FROM public.ia_adjuntos
        WHERE conversacion_id = %s
        ORDER BY id DESC LIMIT 20
    """, [ctx.get("conversacion_id")], fetch="all")
    return {"adjuntos": filas,
            "nota": "Usa el 'id' del adjunto en proponer_adjuntar_archivo."}


# =============================================================================
# ESCRITURA — todas crean una PROPUESTA, ninguna toca el archivo
# =============================================================================

# Columnas que una propuesta puede modificar, por módulo. Lista blanca, no negra: lo que
# no está aquí no se puede cambiar desde el chat, y agregar una columna es una decisión
# consciente. Fuera quedan a propósito `creado_por`, `created_at`, `deleted_at` y los ids.
_CAMPOS_ARCHIVO = {
    "titulo", "abstract", "autor", "id_tipo_documento", "fecha_documento",
    "tesauro_secundario", "personas_relacionadas", "ubicacion", "file_url",
    "soporte", "numero_folio", "numero_paginas", "idioma", "notas", "fecha_vencimiento",
}
_CAMPOS_RRHH = {
    "titulo", "abstract", "autor", "id_tipo_documento", "fecha_documento",
    "personas_relacionadas", "ubicacion", "file_url", "soporte",
    "numero_folio", "numero_paginas", "notas", "fecha_vencimiento",
}


def campos_permitidos(modulo):
    return _CAMPOS_ARCHIVO if modulo == "archivo" else _CAMPOS_RRHH


def _crear_propuesta(ctx, accion, modulo, objetivo_id, datos, resumen):
    """Deja la propuesta en la bandeja y devuelve al modelo algo que anunciar.

    El resumen se guarda porque quien aprueba tiene que poder juzgar sin releer toda la
    conversación: una orden suelta fuera de contexto no se puede evaluar.
    """
    fila = db_query("""
        INSERT INTO public.ia_propuestas
            (conversacion_id, usuario, accion, modulo, objetivo_id, datos, resumen,
             estado, created_at)
        VALUES (%s, %s, %s, %s, %s, %s, %s, 'pendiente', NOW())
        RETURNING id
    """, [
        ctx.get("conversacion_id"), ctx.get("usuario"), accion, modulo, objetivo_id,
        json.dumps(datos, ensure_ascii=False, default=str), resumen[:400],
    ], fetch="one", commit=True)

    return {
        "propuesta_id": fila["id"],
        "estado": "pendiente_de_aprobacion",
        "resumen": resumen,
        "instruccion_para_ti": "NO digas que ya está hecho. Di que dejaste la propuesta "
                               "lista y que hay que aprobarla con el botón que aparece abajo.",
    }


def _normalizar_campos(modulo, datos):
    """Filtra a la lista blanca y normaliza tipos. Lo que no pasa, se informa."""
    permitidos = campos_permitidos(modulo)
    limpios, rechazados = {}, []
    for k, v in (datos or {}).items():
        clave = str(k).strip().lower()
        if clave not in permitidos:
            rechazados.append(clave)
            continue
        if isinstance(v, str):
            v = v.strip()[:5000]
        limpios[clave] = v
    return limpios, rechazados


def _proponer_actualizacion(args, ctx):
    modulo = (args.get("modulo") or "archivo").lower()
    if modulo not in ("archivo", "rrhh"):
        return {"error": "modulo debe ser 'archivo' o 'rrhh'."}
    if modulo not in (ctx.get("modulos") or set()):
        return {"error": f"Tu usuario no puede modificar documentos de {modulo}."}

    try:
        objetivo = int(args.get("id"))
    except (TypeError, ValueError):
        return {"error": "Falta el id del documento a modificar."}

    campos, rechazados = _normalizar_campos(modulo, args.get("campos"))
    if not campos:
        return {"error": "No indicaste ningún campo modificable. "
                         f"Se pueden cambiar: {', '.join(sorted(campos_permitidos(modulo)))}."}

    # Se lee el estado ACTUAL y se guarda en la propuesta. Sin esto, quien aprueba ve
    # "titulo -> X" sin saber qué decía antes, que es justo lo que necesita para decidir.
    tabla, pk = ("datos_archivo", "id_archivo") if modulo == "archivo" else ("datos_rrhh", "id_rrhh")
    actual = db_query(
        f"SELECT * FROM public.{tabla} WHERE {pk} = %s", [objetivo], fetch="one"
    )
    if not actual:
        return {"error": f"No existe el documento {objetivo} en {modulo}."}

    antes = {k: actual.get(k) for k in campos}
    cambios = ", ".join(f"{k}: '{antes.get(k)}' → '{v}'" for k, v in campos.items())
    resumen = f"Modificar «{actual.get('titulo')}» (id {objetivo}, {modulo}). {cambios}"

    return _crear_propuesta(ctx, "actualizar", modulo, objetivo,
                            {"campos": campos, "antes": antes}, resumen) | (
        {"campos_ignorados": rechazados} if rechazados else {}
    )


def _proponer_documento(args, ctx):
    modulo = (args.get("modulo") or "archivo").lower()
    if modulo not in ("archivo", "rrhh"):
        return {"error": "modulo debe ser 'archivo' o 'rrhh'."}
    if modulo not in (ctx.get("modulos") or set()):
        return {"error": f"Tu usuario no puede crear documentos en {modulo}."}

    campos, rechazados = _normalizar_campos(modulo, args.get("campos"))
    if not campos.get("titulo"):
        return {"error": "Un documento necesita al menos un 'titulo'."}
    if not campos.get("ubicacion"):
        # `ubicacion` es NOT NULL en la tabla. Mejor pedirla que dejar que falle al aprobar.
        return {"error": "Falta 'ubicacion' (gaveta, estante o 'Digitalizado Exclusivo'). "
                         "Pregúntasela al usuario, no la inventes."}

    extra = {}
    if modulo == "rrhh":
        cedula = _texto(args, "cedula_empleado", 20)
        if not cedula:
            return {"error": "Un documento de RRHH necesita la cédula del empleado."}
        emp = db_query("SELECT id FROM public.empleados WHERE cedula = %s", [cedula], fetch="one")
        if not emp:
            return {"error": f"No hay ningún empleado con cédula {cedula}."}
        extra["empleado_id"] = emp["id"]

    resumen = f"Crear documento «{campos['titulo']}» en {modulo} (ubicación: {campos['ubicacion']})."
    return _crear_propuesta(ctx, "crear", modulo, None,
                            {"campos": campos, "extra": extra}, resumen) | (
        {"campos_ignorados": rechazados} if rechazados else {}
    )


def _proponer_adjuntar_archivo(args, ctx):
    """Engancha un archivo que el usuario subió al chat con un documento existente."""
    modulo = (args.get("modulo") or "archivo").lower()
    if modulo not in ("archivo", "rrhh"):
        return {"error": "modulo debe ser 'archivo' o 'rrhh'."}
    if modulo not in (ctx.get("modulos") or set()):
        return {"error": f"Tu usuario no puede modificar documentos de {modulo}."}

    try:
        adjunto_id = int(args.get("adjunto_id"))
        objetivo = int(args.get("id"))
    except (TypeError, ValueError):
        return {"error": "Faltan 'adjunto_id' y/o 'id' del documento."}

    # El adjunto se busca RESTRINGIDO a esta conversación. Si se buscara por id a secas,
    # bastaría con adivinar un número para enganchar el archivo de otra persona.
    adj = db_query("""
        SELECT id, nombre_archivo, file_url FROM public.ia_adjuntos
        WHERE id = %s AND conversacion_id = %s
    """, [adjunto_id, ctx.get("conversacion_id")], fetch="one")
    if not adj:
        return {"error": "Ese adjunto no existe en esta conversación. "
                         "Pide al usuario que suba el archivo con el clip del chat."}

    tabla, pk = ("datos_archivo", "id_archivo") if modulo == "archivo" else ("datos_rrhh", "id_rrhh")
    doc = db_query(f"SELECT titulo, file_url FROM public.{tabla} WHERE {pk} = %s",
                   [objetivo], fetch="one")
    if not doc:
        return {"error": f"No existe el documento {objetivo} en {modulo}."}

    aviso = ""
    if doc.get("file_url"):
        # Reemplazar un digitalizado ya existente es destructivo y quien aprueba tiene que
        # verlo escrito, no deducirlo.
        aviso = " ATENCIÓN: este documento YA tiene un archivo digitalizado y se reemplazará."

    resumen = (f"Adjuntar «{adj['nombre_archivo']}» al documento «{doc['titulo']}» "
               f"(id {objetivo}, {modulo}).{aviso}")

    return _crear_propuesta(
        ctx, "actualizar", modulo, objetivo,
        {"campos": {"file_url": adj["file_url"]},
         "antes": {"file_url": doc.get("file_url")}},
        resumen,
    )


def _proponer_palabras_clave(args, ctx):
    if "archivo" not in (ctx.get("modulos") or set()):
        return {"error": "Tu usuario no puede modificar el Archivo."}
    try:
        objetivo = int(args.get("id"))
    except (TypeError, ValueError):
        return {"error": "Falta el id del documento."}

    palabras = args.get("palabras")
    if isinstance(palabras, str):
        palabras = [p.strip() for p in palabras.split(",")]
    palabras = [str(p).strip()[:100] for p in (palabras or []) if str(p).strip()][:20]
    if not palabras:
        return {"error": "No indicaste ninguna palabra clave."}

    doc = db_query(
        "SELECT titulo FROM public.datos_archivo WHERE id_archivo = %s AND deleted_at IS NULL",
        [objetivo], fetch="one")
    if not doc:
        return {"error": f"No existe el documento {objetivo} en el Archivo."}

    resumen = (f"Agregar palabras clave a «{doc['titulo']}» (id {objetivo}): "
               f"{', '.join(palabras)}.")
    return _crear_propuesta(ctx, "palabras_clave", "archivo", objetivo,
                            {"palabras": palabras}, resumen)


# =============================================================================
# REGISTRO
# =============================================================================

_registrar("publico", None, _fn(
    "buscar_archivo",
    "Busca documentos en el Archivo Institucional por texto libre (título, resumen, autor, "
    "palabras clave). Devuelve el enlace al documento digitalizado cuando existe. Úsala para "
    "CUALQUIER pregunta sobre qué documentos hay, de qué tratan o dónde están.",
    {
        "termino": {"type": "string", "description": "Texto a buscar. Vacío para listar los más recientes."},
        "tipo_documento": {"type": "string", "description": "Filtrar por tipo (ej. 'Acta', 'Resolución'). Opcional."},
        "autor": {"type": "string", "description": "Ente o departamento que emitió el documento. Opcional."},
        "palabra_clave": {"type": "string", "description": "Filtrar por una palabra clave concreta. Opcional."},
        "anio_desde": {"type": "integer", "description": "Año mínimo. Opcional."},
        "anio_hasta": {"type": "integer", "description": "Año máximo. Opcional."},
        "solo_digitalizados": {"type": "boolean", "description": "True para devolver solo los que tienen enlace."},
        "limite": {"type": "integer", "description": "Cuántos resultados (1-25, por defecto 8)."},
    },
), _buscar_archivo)

_registrar("publico", None, _fn(
    "ver_documento",
    "Ficha completa de un documento: resumen, ubicación física, folio, páginas, palabras "
    "clave y enlace al digitalizado. Primero obtén su id con buscar_archivo.",
    {
        "id": {"type": "integer", "description": "id devuelto por una búsqueda."},
        "modulo": {"type": "string", "description": "'archivo' o 'rrhh'. Por defecto 'archivo'."},
    },
    ["id"],
), _ver_documento)

_registrar("publico", None, _fn(
    "listar_tipos_documento",
    "Lista los tipos de documento con su id y cuántos documentos tiene cada uno. Úsala "
    "cuando pregunten qué clases de documentos existen, o para obtener el id de un tipo "
    "antes de crear o modificar un documento.",
    {"modulo": {"type": "string", "description": "'archivo' o 'rrhh'. Por defecto 'archivo'."}},
), _listar_tipos)

_registrar("publico", None, _fn(
    "listar_palabras_clave",
    "Lista las palabras clave (descriptores) más usadas en el Archivo, con su frecuencia. "
    "Sirve para orientar una búsqueda cuando el usuario no sabe qué términos existen.",
    {"limite": {"type": "integer", "description": "Cuántas traer (1-25, por defecto 20)."}},
), _listar_palabras_clave)

_registrar("publico", None, _fn(
    "estadisticas",
    "Cifras generales: total de documentos del Archivo, documentos de RRHH, empleados, "
    "cuántos están digitalizados y el rango de años cubierto.",
), _estadisticas)

_registrar("publico", None, _fn(
    "ir_a",
    "Lleva al usuario a una pantalla del sistema. Úsala cuando pidan ir o ver una sección "
    "('llévame al archivo', 'abre el panel de RRHH').",
    {"ruta": {"type": "string", "description":
              "Una de: '/archivo', '/rrhh', '/admin/archivo', '/admin/rrhh', "
              "'/admin/sistema', '/admin/ia', '/investigacion', '/ayuda'."}},
    ["ruta"],
), _ir_a)

_registrar("consulta", "rrhh", _fn(
    "buscar_empleado",
    "Busca personal de la Facultad por nombre, apellido o cédula. Devuelve cargo, "
    "departamento, estado laboral y cuántos documentos tiene su expediente.",
    {
        "termino": {"type": "string", "description": "Nombre, apellido o cédula."},
        "departamento": {"type": "string", "description": "Filtrar por departamento. Opcional."},
        "estado": {"type": "string", "description": "Filtrar por estado laboral. Opcional."},
        "limite": {"type": "integer", "description": "Cuántos resultados (1-25, por defecto 8)."},
    },
), _buscar_empleado)

_registrar("consulta", "rrhh", _fn(
    "expediente_empleado",
    "Expediente completo de un empleado: sus documentos agrupados por Parte (I Ingreso, "
    "II Escalafón, III Permisos, IV Documentos Personales), con enlaces a los digitalizados. "
    "Primero obtén la cédula con buscar_empleado.",
    {"cedula": {"type": "string", "description": "Cédula del empleado, sin puntos."}},
    ["cedula"],
), _expediente_empleado)

_registrar("consulta", "rrhh", _fn(
    "buscar_documento_rrhh",
    "Busca documentos dentro de los expedientes de RRHH por texto libre, sin partir de un "
    "empleado concreto. Devuelve a qué empleado pertenece cada uno.",
    {
        "termino": {"type": "string", "description": "Texto a buscar en título, resumen o notas."},
        "limite": {"type": "integer", "description": "Cuántos resultados (1-25, por defecto 8)."},
    },
), _buscar_documento_rrhh)

_registrar("consulta", "rrhh", _fn(
    "documentos_por_vencer",
    "Documentos cuya fecha de vencimiento está próxima o ya pasó. Úsala cuando pregunten "
    "por vencimientos, alertas o documentos por renovar.",
    {
        "dias": {"type": "integer", "description": "Ventana en días hacia adelante (por defecto 90)."},
        "limite": {"type": "integer", "description": "Cuántos resultados (1-25, por defecto 15)."},
    },
), _documentos_por_vencer)

_registrar("editor", None, _fn(
    "mis_adjuntos",
    "Lista los archivos que el usuario subió con el clip en esta conversación. Úsala antes "
    "de proponer adjuntar un archivo a un documento, para obtener su 'adjunto_id'.",
), _mis_adjuntos)

_registrar("editor", None, _fn(
    "proponer_actualizacion",
    "Propone MODIFICAR un documento existente. NO lo modifica: crea una propuesta que el "
    "usuario tiene que aprobar con un botón. Úsala para corregir títulos, fechas, "
    "ubicaciones, resúmenes o cualquier dato mal cargado. Obtén el id con una búsqueda.",
    {
        "modulo": {"type": "string", "description": "'archivo' o 'rrhh'."},
        "id": {"type": "integer", "description": "id del documento a modificar."},
        "campos": {"type": "object", "description":
                   "Solo los campos a cambiar. Permitidos: titulo, abstract, autor, "
                   "id_tipo_documento, fecha_documento (AAAA-MM-DD), tesauro_secundario, "
                   "personas_relacionadas, ubicacion, soporte, numero_folio, numero_paginas, "
                   "notas, fecha_vencimiento (AAAA-MM-DD)."},
    },
    ["modulo", "id", "campos"],
), _proponer_actualizacion)

_registrar("editor", None, _fn(
    "proponer_documento",
    "Propone CREAR un documento nuevo. NO lo crea: deja una propuesta que el usuario "
    "aprueba con un botón. Pregunta los datos que falten; no inventes ubicación ni fecha.",
    {
        "modulo": {"type": "string", "description": "'archivo' o 'rrhh'."},
        "campos": {"type": "object", "description":
                   "Al menos 'titulo' y 'ubicacion'. Además: abstract, autor, "
                   "id_tipo_documento, fecha_documento (AAAA-MM-DD), numero_folio, "
                   "numero_paginas, notas, fecha_vencimiento."},
        "cedula_empleado": {"type": "string", "description":
                            "Obligatoria si modulo='rrhh': de quién es el expediente."},
    },
    ["modulo", "campos"],
), _proponer_documento)

_registrar("editor", None, _fn(
    "proponer_adjuntar_archivo",
    "Propone enganchar un archivo que el usuario subió al chat con un documento existente, "
    "para que quede como su versión digitalizada. Primero usa mis_adjuntos para el "
    "'adjunto_id' y una búsqueda para el 'id' del documento.",
    {
        "modulo": {"type": "string", "description": "'archivo' o 'rrhh'."},
        "id": {"type": "integer", "description": "id del documento que recibirá el archivo."},
        "adjunto_id": {"type": "integer", "description": "id devuelto por mis_adjuntos."},
    },
    ["modulo", "id", "adjunto_id"],
), _proponer_adjuntar_archivo)

_registrar("editor", "archivo", _fn(
    "proponer_palabras_clave",
    "Propone agregar palabras clave (descriptores) a un documento del Archivo. Las que no "
    "existan se crean al aprobar. Nunca digas 'tesauro': aquí se llaman Palabras Clave.",
    {
        "id": {"type": "integer", "description": "id del documento del Archivo."},
        "palabras": {"type": "array", "items": {"type": "string"},
                     "description": "Lista de palabras clave a agregar."},
    },
    ["id", "palabras"],
), _proponer_palabras_clave)
