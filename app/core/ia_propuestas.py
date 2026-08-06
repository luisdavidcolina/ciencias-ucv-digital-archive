"""La bandeja de aprobación: donde una propuesta del asistente se vuelve un cambio real.

POR QUÉ EXISTE ESTE ARCHIVO
---------------------------
El asistente no escribe en el archivo. Propone. Aquí es donde una persona convierte esa
propuesta en un UPDATE o un INSERT, y donde queda registrado quién lo aprobó.

La separación no es ceremonia: `ia_tools` no importa nada de aquí y este módulo no sabe que
existe un modelo de lenguaje. Una propuesta es una fila con una acción y unos datos; da lo
mismo si la escribió una IA o un formulario.

LAS DOS COSAS QUE NO SE PUEDEN AFLOJAR
--------------------------------------
1. **La lista blanca de columnas se revalida aquí.** Ya se filtró al crear la propuesta,
   pero entre crearla y aprobarla pasa tiempo y la fila es editable en la base. Si la lista
   blanca viviera solo en el momento de crear, bastaría con tocar el JSON para escribir en
   cualquier columna. El SQL se arma con nombres de columna que salen de un `set` del
   código, nunca del JSON.
2. **Quien aprueba tiene que poder.** Se comprueba su módulo contra el de la propuesta en
   el momento de aprobar, no en el de proponer: los permisos de un usuario pueden haber
   cambiado, y el que aprueba puede no ser el que pidió.
"""
import json

from database import db_query, log_event
from core.ia_tools import campos_permitidos

_TABLAS = {
    "archivo": ("datos_archivo", "id_archivo"),
    "rrhh": ("datos_rrhh", "id_rrhh"),
}


class PropuestaError(Exception):
    """Algo impide ejecutar la propuesta. El mensaje va al usuario tal cual."""


def _usuario_id(usuario: str):
    fila = db_query(
        "SELECT id FROM public.usuarios_sistema WHERE usuario = %s AND COALESCE(is_active, TRUE)",
        [usuario], fetch="one",
    )
    if not fila:
        raise PropuestaError("Tu usuario ya no está activo en el sistema.")
    return fila["id"]


def listar(conversacion_id=None, estado="pendiente", limite=50):
    where, params = [], []
    if conversacion_id is not None:
        where.append("conversacion_id = %s")
        params.append(conversacion_id)
    if estado:
        where.append("estado = %s")
        params.append(estado)
    params.append(max(1, min(limite, 200)))

    return db_query(f"""
        SELECT id, conversacion_id, usuario, accion, modulo, objetivo_id, resumen, estado,
               resuelto_por, TO_CHAR(created_at,'YYYY-MM-DD HH24:MI') AS creada,
               TO_CHAR(resuelto_at,'YYYY-MM-DD HH24:MI') AS resuelta
        FROM public.ia_propuestas
        {('WHERE ' + ' AND '.join(where)) if where else ''}
        ORDER BY id DESC LIMIT %s
    """, params, fetch="all")


def rechazar(propuesta_id: int, usuario: str) -> dict:
    fila = db_query("SELECT estado FROM public.ia_propuestas WHERE id = %s",
                    [propuesta_id], fetch="one")
    if not fila:
        raise PropuestaError("Esa propuesta no existe.")
    if fila["estado"] != "pendiente":
        raise PropuestaError(f"Esa propuesta ya está {fila['estado']}.")

    db_query("""
        UPDATE public.ia_propuestas
           SET estado = 'rechazada', resuelto_por = %s, resuelto_at = NOW()
         WHERE id = %s
    """, [usuario, propuesta_id], fetch="none", commit=True)
    log_event(usuario, "Rechazó propuesta IA", "IA", f"propuesta_id={propuesta_id}")
    return {"ok": True, "estado": "rechazada"}


def aprobar(propuesta_id: int, usuario: str, modulos: set) -> dict:
    """Ejecuta la propuesta de verdad. Es el único punto del módulo que escribe."""
    p = db_query("SELECT * FROM public.ia_propuestas WHERE id = %s", [propuesta_id], fetch="one")
    if not p:
        raise PropuestaError("Esa propuesta no existe.")
    if p["estado"] != "pendiente":
        raise PropuestaError(f"Esa propuesta ya está {p['estado']}.")
    if p["modulo"] not in modulos:
        raise PropuestaError(f"Tu usuario no puede aprobar cambios en {p['modulo']}.")

    try:
        datos = json.loads(p["datos"] or "{}")
    except (ValueError, TypeError):
        raise PropuestaError("La propuesta está corrupta y no se puede ejecutar.")

    accion = p["accion"]
    if accion == "actualizar":
        detalle = _actualizar(p, datos, usuario)
    elif accion == "crear":
        detalle = _crear(p, datos, usuario)
    elif accion == "palabras_clave":
        detalle = _palabras_clave(p, datos)
    else:
        raise PropuestaError(f"Acción desconocida: {accion}.")

    db_query("""
        UPDATE public.ia_propuestas
           SET estado = 'aprobada', resuelto_por = %s, resuelto_at = NOW()
         WHERE id = %s
    """, [usuario, propuesta_id], fetch="none", commit=True)

    log_event(usuario, f"Aprobó propuesta IA ({accion})", p["modulo"].upper(),
              f"propuesta_id={propuesta_id} — {p['resumen']}")

    return {"ok": True, "estado": "aprobada", "detalle": detalle}


# =============================================================================
# EJECUTORES
# =============================================================================

def _sql_campos(modulo, campos):
    """Convierte el dict en SET seguro. Los nombres salen del `set` del código.

    Ésta es la línea que impide una inyección por nombre de columna: `campos` viene de un
    JSON que un modelo escribió, así que sus CLAVES no pueden llegar al SQL. Solo llegan
    las que coinciden con la lista blanca, y esas son literales del código.
    """
    permitidos = campos_permitidos(modulo)
    trozos, valores = [], []
    for clave in sorted(permitidos):
        if clave in campos:
            trozos.append(f"{clave} = %s")
            valor = campos[clave]
            # Un string vacío en una fecha o un entero revienta el INSERT; NULL no.
            valores.append(None if valor == "" else valor)
    return trozos, valores


def _actualizar(p, datos, usuario):
    tabla, pk = _TABLAS[p["modulo"]]
    campos = datos.get("campos") or {}
    trozos, valores = _sql_campos(p["modulo"], campos)
    if not trozos:
        raise PropuestaError("La propuesta no tiene ningún campo modificable.")

    uid = _usuario_id(usuario)
    valores += [uid, p["objetivo_id"]]

    db_query(
        f"UPDATE public.{tabla} SET {', '.join(trozos)}, updated_at = NOW(), updated_by = %s "
        f"WHERE {pk} = %s",
        valores, fetch="none", commit=True,
    )
    return f"{len(trozos)} campo(s) actualizado(s) en {tabla} #{p['objetivo_id']}."


def _crear(p, datos, usuario):
    tabla, pk = _TABLAS[p["modulo"]]
    campos = dict(datos.get("campos") or {})
    extra = datos.get("extra") or {}

    permitidos = campos_permitidos(p["modulo"])
    columnas = [c for c in sorted(permitidos) if c in campos]
    valores = [None if campos[c] == "" else campos[c] for c in columnas]

    # `empleado_id` no está en la lista blanca de modificables a propósito (mover un
    # documento de expediente es una operación distinta), pero al CREAR es obligatorio y
    # ya fue resuelto contra la tabla de empleados al proponer.
    if p["modulo"] == "rrhh":
        if not extra.get("empleado_id"):
            raise PropuestaError("La propuesta no dice de qué empleado es el documento.")
        columnas.append("empleado_id")
        valores.append(extra["empleado_id"])

    columnas.append("creado_por")
    valores.append(_usuario_id(usuario))

    marcadores = ", ".join(["%s"] * len(valores))
    fila = db_query(
        f"INSERT INTO public.{tabla} ({', '.join(columnas)}) VALUES ({marcadores}) "
        f"RETURNING {pk} AS id",
        valores, fetch="one", commit=True,
    )
    return f"Documento creado en {tabla} con id {fila['id']}."


def _palabras_clave(p, datos):
    palabras = datos.get("palabras") or []
    if not palabras:
        raise PropuestaError("La propuesta no tiene palabras clave.")

    agregadas = 0
    for palabra in palabras:
        # Se crea si no existe y se recupera el id en cualquier caso. Un ON CONFLICT DO
        # NOTHING solo no devuelve la fila existente, y hace falta el id para el vínculo.
        db_query(
            "INSERT INTO public.descriptores_libres (nombre) VALUES (%s) "
            "ON CONFLICT (nombre) DO NOTHING",
            [palabra], fetch="none", commit=True,
        )
        d = db_query("SELECT id_descriptor FROM public.descriptores_libres WHERE nombre = %s",
                     [palabra], fetch="one")
        if not d:
            continue
        db_query(
            "INSERT INTO public.archivo_descriptores (id_archivo, id_descriptor) "
            "VALUES (%s, %s) ON CONFLICT DO NOTHING",
            [p["objetivo_id"], d["id_descriptor"]], fetch="none", commit=True,
        )
        agregadas += 1

    # El caché de choices sirve las palabras clave a los formularios: sin invalidarlo, la
    # pantalla seguiría mostrando la lista vieja hasta cinco minutos.
    try:
        from routes.lookups import invalidate_choices_cache
        invalidate_choices_cache()
    except Exception:
        pass

    return f"{agregadas} palabra(s) clave vinculada(s) al documento #{p['objetivo_id']}."
