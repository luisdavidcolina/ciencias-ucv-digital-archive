"""Categorías, palabras clave y audit log."""
from fastapi import APIRouter, Depends, HTTPException, Query
from typing import Optional

from database import db_query, log_event
from models import CategoryCreateRequest, KeywordRequest
from utils import generate_unique_slug
from ..lookups import invalidate_choices_cache
from .deps import require_role, require_admin_role, _fila_usuario

router = APIRouter()

# Catálogo compartido: tipos documentales, palabras clave y auditoría los usan
# tanto Archivo como RRHH (pestaña "Tipos" en ambos paneles, OA-123.../OR-163...).
# Ninguno de estos endpoints toma un parámetro de módulo fiable antes de
# ejecutarse (add_category recibe `scope` en el cuerpo, no filtra acceso), así
# que la autorización se hace por pertenencia a cualquiera de los dos módulos,
# no por adivinar el scope. Crear un tipo documental (add_category) exige
# además rol Admin: es más sensible porque se propaga a todos los desplegables
# y documentos (OA-123). Ver `deps.py` para el criterio completo.


@router.get("/keywords", dependencies=[Depends(require_role("Archivo", "RRHH"))])
def get_keywords(search: str = ""):
    # IN-096/IN-097: agregaba `archivo_descriptores` entero sin ningún límite.
    # `search` es opcional y no lo usa hoy el cliente (OA-129 lo dejó
    # pendiente en `admin-categories.js`, fuera de esta zona), así que no
    # cambia el comportamiento actual sin él; el `LIMIT` es sólo una cota de
    # seguridad muy por encima de cualquier volumen real hoy (no trunca la
    # lista existente), para que un fondo de cientos de miles de vínculos no
    # agote la memoria del lambda.
    where, params = "", None
    if search.strip():
        where = "WHERE unaccent(dl.nombre) ILIKE unaccent(%s)"
        params = [f"%{search.strip()}%"]
    rows = db_query(
        f"""SELECT dl.id_descriptor AS id, dl.nombre,
                  COUNT(DISTINCT ad.id_archivo) AS uso_archivo
           FROM public.descriptores_libres dl
           LEFT JOIN public.archivo_descriptores ad ON ad.id_descriptor = dl.id_descriptor
           {where}
           GROUP BY dl.id_descriptor, dl.nombre
           ORDER BY dl.nombre
           LIMIT 5000""",
        params, fetch="all",
    ) or []
    return [dict(r) for r in rows]


@router.post("/keywords")
def create_keyword(req: KeywordRequest, usuario: str = Depends(require_role("Archivo", "RRHH"))):
    nombre = (req.nombre or "").strip()
    if not nombre:
        raise HTTPException(400, "Nombre vacío")
    existing = db_query(
        "SELECT id_descriptor FROM public.descriptores_libres WHERE LOWER(nombre) = LOWER(%s)",
        (nombre,), fetch="one",
    )
    if existing:
        raise HTTPException(400, "Palabra clave ya existe")
    row = db_query(
        "INSERT INTO public.descriptores_libres (nombre) VALUES (%s) RETURNING id_descriptor AS id",
        (nombre,), fetch="one", commit=True,
    )
    # IN-083: la creación/renombrado/borrado de palabras clave no quedaba
    # registrado en `audit_log`. `usuario` sale de la sesión (require_role),
    # nunca del cuerpo de la petición (IN-133): `KeywordRequest` no lo trae.
    log_event(usuario, "Create Keyword", "Sistema", f"Nombre: {nombre}")
    return {"success": True, "id": row["id"]}


@router.put("/keywords/{kid}")
def update_keyword(kid: int, req: KeywordRequest, usuario: str = Depends(require_role("Archivo", "RRHH"))):
    nombre = (req.nombre or "").strip()
    if not nombre:
        raise HTTPException(400, "Nombre vacío")
    # OA-130: sin este chequeo, renombrar a un nombre ya existente fusionaba
    # dos descriptores sin avisar (o reventaba con un 500 genérico si hay
    # UNIQUE(nombre)). Aquí se detecta antes de escribir y se responde 409.
    colision = db_query(
        "SELECT id_descriptor FROM public.descriptores_libres "
        "WHERE LOWER(nombre) = LOWER(%s) AND id_descriptor != %s",
        (nombre, kid), fetch="one",
    )
    if colision:
        raise HTTPException(
            409,
            f"Ya existe una palabra clave '{nombre}'. Para unirlas, borre una "
            "de las dos usando force=true y reasigne los documentos.",
        )
    db_query(
        "UPDATE public.descriptores_libres SET nombre = %s WHERE id_descriptor = %s",
        (nombre, kid), fetch="none", commit=True,
    )
    log_event(usuario, "Rename Keyword", "Sistema", f"id={kid} → '{nombre}'")
    return {"success": True}


@router.delete("/keywords/{kid}")
def delete_keyword(kid: int, force: bool = False, merge_into: Optional[int] = None,
                    usuario: str = Depends(require_admin_role("Archivo", "RRHH"))):
    uso = db_query(
        "SELECT COUNT(*) AS cnt FROM public.archivo_descriptores WHERE id_descriptor = %s",
        (kid,), fetch="one",
    )
    uso_count = int((uso or {}).get("cnt", 0))

    # OA-130/OA-131: en vez de sólo borrar los enlaces (perdiendo la
    # asociación), "fusionar con..." reasigna los documentos al descriptor
    # destino antes de borrar el origen. Evita el escenario de OA-130 (dos
    # palabras clave que significan lo mismo y ninguna búsqueda las junta).
    if merge_into is not None:
        if merge_into == kid:
            raise HTTPException(400, "No se puede fusionar una palabra clave consigo misma")
        destino = db_query(
            "SELECT id_descriptor FROM public.descriptores_libres WHERE id_descriptor = %s",
            (merge_into,), fetch="one",
        )
        if not destino:
            raise HTTPException(404, "La palabra clave destino no existe")
        # Reasigna sólo los vínculos que el destino no tenga ya (evita choque
        # con la PK compuesta de archivo_descriptores).
        db_query(
            """UPDATE public.archivo_descriptores ad
               SET id_descriptor = %s
               WHERE ad.id_descriptor = %s
                 AND NOT EXISTS (
                     SELECT 1 FROM public.archivo_descriptores ad2
                     WHERE ad2.id_archivo = ad.id_archivo AND ad2.id_descriptor = %s
                 )""",
            (merge_into, kid, merge_into), fetch="none", commit=True,
        )
        db_query(
            "DELETE FROM public.archivo_descriptores WHERE id_descriptor = %s",
            (kid,), fetch="none", commit=True,
        )
        db_query(
            "DELETE FROM public.descriptores_libres WHERE id_descriptor = %s",
            (kid,), fetch="none", commit=True,
        )
        log_event(usuario, "Merge Keyword", "Sistema",
                  f"id={kid} fusionada en id={merge_into} ({uso_count} documento(s) reasignados)")
        return {"success": True, "merged_into": merge_into, "reassigned": uso_count}

    if uso_count > 0 and not force:
        raise HTTPException(
            400,
            f"La palabra clave está en uso en {uso_count} documento(s). "
            "Use force=true para eliminar de todas formas."
        )
    if uso_count > 0:
        db_query(
            "DELETE FROM public.archivo_descriptores WHERE id_descriptor = %s",
            (kid,), fetch="none", commit=True,
        )
    db_query(
        "DELETE FROM public.descriptores_libres WHERE id_descriptor = %s",
        (kid,), fetch="none", commit=True,
    )
    log_event(usuario, "Delete Keyword", "Sistema", f"id={kid}, desvinculada de {uso_count} documento(s)")
    return {"success": True, "removed_from_docs": uso_count}


@router.post("/add_category")
def add_category(req: CategoryCreateRequest, usuario: str = Depends(require_admin_role("Archivo", "RRHH"))):
    nombre = (req.name or "").strip()
    if not nombre:
        raise HTTPException(400, "Nombre vacío")

    if req.parte:
        cat_slug = req.parte
    elif req.scope == "Archivo":
        cat_slug = "archivo"
    else:
        cat_slug = "parte-i"

    cat = db_query("SELECT id FROM public.categoria WHERE slug = %s", (cat_slug,), fetch="one")
    if not cat:
        cat = db_query("SELECT id FROM public.categoria ORDER BY id LIMIT 1", fetch="one")
    if not cat:
        raise HTTPException(500, "No hay categorías en la BD")

    # OR-040: `tipo_documento.nombre` es UNIQUE global en el esquema (no por
    # (nombre, categoría) — cambiar esa restricción exige un DROP CONSTRAINT,
    # prohibido por las reglas de migración de este proyecto), así que el
    # choque es real e ineludible tal como está la base hoy. Lo que sí se
    # arregla aquí es la honestidad del mensaje: decir en qué categoría vive
    # ya el nombre, en vez de fingir éxito (OA-125).
    existing = db_query(
        """SELECT td.id, c.nombre AS categoria_nombre FROM public.tipo_documento td
           JOIN public.categoria c ON c.id = td.id_categoria
           WHERE LOWER(td.nombre) = LOWER(%s)""",
        (nombre,), fetch="one",
    )
    if existing:
        raise HTTPException(
            409,
            f"El tipo documental '{nombre}' ya existe en la categoría "
            f"'{existing['categoria_nombre']}' (id={existing['id']}).",
        )

    slug = generate_unique_slug(nombre, "tipo_documento")
    descripcion = (req.desc or "").strip() or None
    db_query(
        "INSERT INTO public.tipo_documento (nombre, nombre_corto, slug, id_categoria, descripcion) "
        "VALUES (%s, %s, %s, %s, %s)",
        (nombre, nombre, slug, cat["id"], descripcion),
        fetch="none", commit=True,
    )
    invalidate_choices_cache()
    # El log se hace tras el INSERT real, no antes: si la petición falla por
    # nombre vacío o duplicado, ya no se registraba "creado" algo que nunca
    # se creó. `usuario` sale de la sesión, no de `req.usuario` (IN-133).
    log_event(usuario, "Create Category", req.scope, f"Nueva Tipología: {nombre}")
    return {"success": True}


@router.put("/categories/{tid}")
def update_category(tid: int, req: CategoryCreateRequest,
                     usuario: str = Depends(require_admin_role("Archivo", "RRHH"))):
    """OA-123/OR-163: renombrar, editar descripción o mover de Parte un tipo
    documental existente. Reutiliza `CategoryCreateRequest` (ya trae name/desc/
    parte) para no depender de un modelo nuevo en `models.py`, fuera de esta
    zona."""
    actual = db_query("SELECT id FROM public.tipo_documento WHERE id = %s", (tid,), fetch="one")
    if not actual:
        raise HTTPException(404, "Tipo documental no encontrado")

    nombre = (req.name or "").strip()
    if not nombre:
        raise HTTPException(400, "Nombre vacío")

    colision = db_query(
        "SELECT id FROM public.tipo_documento WHERE LOWER(nombre) = LOWER(%s) AND id != %s",
        (nombre, tid), fetch="one",
    )
    if colision:
        raise HTTPException(409, f"El tipo documental '{nombre}' ya existe (id={colision['id']})")

    set_clauses = ["nombre = %s", "nombre_corto = %s", "descripcion = %s"]
    params = [nombre, nombre, (req.desc or "").strip() or None]

    if req.parte:
        cat = db_query("SELECT id FROM public.categoria WHERE slug = %s", (req.parte,), fetch="one")
        if not cat:
            raise HTTPException(400, f"Categoría '{req.parte}' no existe")
        set_clauses.append("id_categoria = %s")
        params.append(cat["id"])

    params.append(tid)
    db_query(
        f"UPDATE public.tipo_documento SET {', '.join(set_clauses)} WHERE id = %s",
        params, fetch="none", commit=True,
    )
    invalidate_choices_cache()
    log_event(usuario, "Update Category", req.scope, f"id={tid} → '{nombre}'")
    return {"success": True}


@router.delete("/categories/{tid}")
def delete_category(tid: int, force: bool = False, merge_into: Optional[int] = None,
                     usuario: str = Depends(require_admin_role("Archivo", "RRHH"))):
    """OA-123/OR-163: hoy un tipo documental sólo se puede crear, nunca
    retirar. `tipo_documento` tiene FK RESTRICT desde `datos_archivo` y
    `datos_rrhh` (schema.sql), así que un DELETE liso ya fallaba con un 500
    genérico en cuanto el tipo estaba en uso. Aquí se ofrece, en este orden:
    fusionar con otro tipo (reasigna los documentos), desactivar (`activo =
    false`, visible pero no ofrecible en altas nuevas) o borrar de verdad
    cuando no está en uso."""
    tipo = db_query("SELECT id, nombre FROM public.tipo_documento WHERE id = %s", (tid,), fetch="one")
    if not tipo:
        raise HTTPException(404, "Tipo documental no encontrado")

    uso_archivo = db_query(
        "SELECT COUNT(*) AS cnt FROM public.datos_archivo WHERE id_tipo_documento = %s",
        (tid,), fetch="one",
    )
    uso_rrhh = db_query(
        "SELECT COUNT(*) AS cnt FROM public.datos_rrhh WHERE id_tipo_documento = %s",
        (tid,), fetch="one",
    )
    uso_count = int((uso_archivo or {}).get("cnt", 0)) + int((uso_rrhh or {}).get("cnt", 0))

    if merge_into is not None:
        if merge_into == tid:
            raise HTTPException(400, "No se puede fusionar un tipo consigo mismo")
        destino = db_query("SELECT id FROM public.tipo_documento WHERE id = %s", (merge_into,), fetch="one")
        if not destino:
            raise HTTPException(404, "El tipo destino no existe")
        db_query(
            "UPDATE public.datos_archivo SET id_tipo_documento = %s WHERE id_tipo_documento = %s",
            (merge_into, tid), fetch="none", commit=True,
        )
        db_query(
            "UPDATE public.datos_rrhh SET id_tipo_documento = %s WHERE id_tipo_documento = %s",
            (merge_into, tid), fetch="none", commit=True,
        )
        db_query("DELETE FROM public.tipo_documento WHERE id = %s", (tid,), fetch="none", commit=True)
        invalidate_choices_cache()
        log_event(usuario, "Merge Category", "Sistema",
                  f"'{tipo['nombre']}' (id={tid}) fusionado en id={merge_into} "
                  f"({uso_count} documento(s) reasignados)")
        return {"success": True, "merged_into": merge_into, "reassigned": uso_count}

    if uso_count > 0 and not force:
        raise HTTPException(
            400,
            f"El tipo '{tipo['nombre']}' está en uso en {uso_count} documento(s). "
            "Use force=true para desactivarlo, o merge_into=<id> para fusionarlo con otro.",
        )

    if uso_count > 0:
        # En uso: no se puede DELETE (FK RESTRICT) ni tiene sentido perder el
        # historial. Se desactiva en vez de borrar.
        db_query(
            "UPDATE public.tipo_documento SET activo = FALSE WHERE id = %s",
            (tid,), fetch="none", commit=True,
        )
        invalidate_choices_cache()
        log_event(usuario, "Deactivate Category", "Sistema",
                  f"'{tipo['nombre']}' (id={tid}), en uso en {uso_count} documento(s)")
        return {"success": True, "deactivated": True, "in_use": uso_count}

    db_query("DELETE FROM public.tipo_documento WHERE id = %s", (tid,), fetch="none", commit=True)
    invalidate_choices_cache()
    log_event(usuario, "Delete Category", "Sistema", f"'{tipo['nombre']}' (id={tid})")
    return {"success": True, "deactivated": False}


@router.get("/audit_log")
def get_audit_log(page: int = 1, per_page: int = 50, search: str = "",
                   modulo: str = "", resultado: str = "",
                   desde: str = "", hasta: str = "",
                   usuario: str = Depends(require_admin_role("Archivo", "RRHH"))):
    """Retorna el log de auditoría con paginación y búsqueda opcional.

    OA-152: un admin de Archivo veía también las filas de RRHH (y viceversa),
    incluyendo nombres de empleados en `detalle` — dato personal fuera de su
    módulo. Sólo un admin "Global" ve todo; el resto queda restringido a su
    propio módulo, sin excepción y sin parámetro que lo evada.

    OA-151: además de la caja de texto original (`search` sobre accion/
    usuario), acepta filtro por módulo (dentro de lo que el rol ya permite
    ver), por resultado y por rango de fechas — combinables. La UI de estos
    filtros queda pendiente en `admin-ui.js`, fuera de esta zona.
    """
    page     = max(1, page)
    per_page = max(1, min(per_page, 100))
    offset   = (page - 1) * per_page

    # Reutiliza la misma consulta que ya hace `require_admin_role` (vía
    # `deps.db_query`, no `catalog.db_query`): así no consume una posición
    # extra en los mocks de `db_query` de este router que ya usan los tests
    # existentes por orden de llamada.
    fila = _fila_usuario(usuario)
    modulo_sesion = (fila or {}).get("modulo")

    conditions, params = [], []
    if search:
        conditions.append(
            "(unaccent(accion) ILIKE unaccent(%s) OR unaccent(usuario) ILIKE unaccent(%s))"
        )
        params.extend([f"%{search}%", f"%{search}%"])
    if modulo_sesion != "Global":
        # No es un filtro opcional: es el límite de lo que este usuario puede
        # ver, así que ignora cualquier `modulo` pedido que no sea el suyo.
        conditions.append("modulo = %s")
        params.append(modulo_sesion)
    elif modulo:
        conditions.append("modulo = %s")
        params.append(modulo)
    if resultado:
        conditions.append("status = %s")
        params.append(resultado)
    if desde:
        conditions.append("timestamp >= %s")
        params.append(desde)
    if hasta:
        conditions.append("timestamp < (%s::date + INTERVAL '1 day')")
        params.append(hasta)

    where = ("WHERE " + " AND ".join(conditions)) if conditions else ""

    count_row = db_query(
        f"SELECT COUNT(*) AS total FROM public.audit_log {where}",
        params or None, fetch="one",
    )
    total = int(count_row["total"]) if count_row else 0

    rows = db_query(
        f"""SELECT id, usuario, accion AS evento, modulo, detalle, status AS resultado,
                   TO_CHAR(timestamp, 'YYYY-MM-DD HH24:MI:SS') AS timestamp
            FROM public.audit_log
            {where}
            ORDER BY timestamp DESC
            LIMIT %s OFFSET %s""",
        (params + [per_page, offset]) if params else [per_page, offset],
        fetch="all",
    ) or []

    return {"total": total, "page": page, "per_page": per_page, "records": [dict(r) for r in rows]}


@router.get("/notifications", dependencies=[Depends(require_role("Archivo", "RRHH"))])
def get_notifications(modulo: Optional[str] = Query(default="")):
    """
    Devuelve un resumen de pendientes para el panel de notificaciones.
    Incluye docs en revisión y borradores del módulo solicitado.
    """
    items = []
    total = 0

    # Archivo pendientes
    if not modulo or modulo in ("Archivo", "Global"):
        arch_rows = db_query(
            """SELECT da.id_archivo AS id, da.titulo AS label, da.status, 'Archivo' AS modulo,
                      TO_CHAR(da.updated_at, 'YYYY-MM-DD HH24:MI') AS ts
               FROM public.datos_archivo da
               WHERE da.status IN ('revision','draft') AND da.deleted_at IS NULL
               ORDER BY da.updated_at DESC LIMIT 20""",
            fetch="all",
        ) or []
        for r in arch_rows:
            items.append(dict(r))
        total += len(arch_rows)

    # RRHH pendientes
    if not modulo or modulo in ("RRHH", "Global"):
        rrhh_rows = db_query(
            """SELECT dr.id_rrhh AS id, COALESCE(e.nombres||' '||e.apellidos, 'Sin nombre') AS label,
                      dr.status, 'RRHH' AS modulo,
                      TO_CHAR(dr.updated_at, 'YYYY-MM-DD HH24:MI') AS ts
               FROM public.datos_rrhh dr
               LEFT JOIN public.empleados e ON e.id = dr.empleado_id
               WHERE dr.status IN ('revision','draft') AND dr.deleted_at IS NULL
               ORDER BY dr.updated_at DESC LIMIT 20""",
            fetch="all",
        ) or []
        for r in rrhh_rows:
            items.append(dict(r))
        total += len(rrhh_rows)

    # Contar por tipo de estado
    counts = {"revision": 0, "draft": 0}
    for it in items:
        st = it.get("status", "")
        if st in counts:
            counts[st] += 1

    return {"total": total, "counts": counts, "items": items[:30]}
