import multiprocessing
import platform
from datetime import datetime
from typing import Optional

from fastapi import APIRouter, HTTPException
import pandas as pd

from database import db_query, log_event, logger, hash_password
from models import (
    DocumentSubmitRequest,
    StatsRequest,
    CategoryCreateRequest,
    KeywordRequest,
    UserCreateRequest,
    DocumentUpdateRequest,
    EmpleadoUpdateRequest,
    PasswordChangeRequest,
)
from utils import generate_unique_slug
from .archivo import fetch_archivo_dataframe
from .rrhh import fetch_rrhh_dataframe
from .choices import invalidate_choices_cache

router = APIRouter(prefix="/api/admin", tags=["admin"])


# =============================================================================
# HELPERS PRIVADOS
# =============================================================================

def _resolve_or_create_lookup(table: str, nombre: str, default_name: str = "Por Asignar") -> int:
    """Busca o crea un registro en una tabla de catálogo y retorna su id."""
    nombre = (nombre or "").strip() or default_name
    col = "estados" if table == "estados_laborales" else "nombre"
    row = db_query(f"SELECT id FROM public.{table} WHERE {col} = %s", (nombre,), fetch="one")
    if row:
        return row["id"]
    new_row = db_query(
        f"INSERT INTO public.{table} ({col}) VALUES (%s) RETURNING id",
        (nombre,),
        fetch="one",
        commit=True,
    )
    return new_row["id"]


def _resolve_or_create_tipo_documento(nombre: str, cat_slug: str = None) -> int:
    """Busca o crea un tipo de documento y retorna su id."""
    nombre = (nombre or "").strip()
    if not nombre:
        raise HTTPException(status_code=400, detail="doc_type vacío")
    row = db_query(
        "SELECT id FROM public.tipo_documento WHERE LOWER(nombre) = LOWER(%s)",
        (nombre,),
        fetch="one",
    )
    if row:
        return row["id"]
    # Obtener categoría correcta
    if cat_slug:
        cat = db_query("SELECT id FROM public.categoria WHERE slug = %s", (cat_slug,), fetch="one")
    else:
        cat = None
    if not cat:
        cat = db_query("SELECT id FROM public.categoria ORDER BY id LIMIT 1", fetch="one")
    if not cat:
        raise HTTPException(status_code=500, detail="No existen categorías en la BD")
    slug = generate_unique_slug(nombre, "tipo_documento")
    new_row = db_query(
        "INSERT INTO public.tipo_documento (nombre, nombre_corto, slug, id_categoria) VALUES (%s, %s, %s, %s) RETURNING id",
        (nombre, nombre, slug, cat["id"]),
        fetch="one",
        commit=True,
    )
    return new_row["id"]


def _resolve_user_id(usuario: str) -> int:
    """Retorna el id del usuario; usa el primer usuario como fallback."""
    row = db_query(
        "SELECT id FROM public.usuarios_sistema WHERE TRIM(usuario) = %s",
        (usuario.strip(),),
        fetch="one",
    )
    if row:
        return row["id"]
    fallback = db_query("SELECT id FROM public.usuarios_sistema ORDER BY id LIMIT 1", fetch="one")
    return fallback["id"] if fallback else 1


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.post("/stats")
def get_admin_stats(req: StatsRequest):
    df = fetch_archivo_dataframe() if req.modulo == "Archivo" else fetch_rrhh_dataframe()
    if df.empty:
        return {"total_docs": 0, "categories_count": 0, "by_type": [], "timeline": []}

    fecha_col = "fecha" if req.modulo == "Archivo" else "fecha_ingreso"

    if req.date_start and req.date_end:
        df = df[(df[fecha_col] >= req.date_start) & (df[fecha_col] <= req.date_end)]
    if req.doc_types:
        df = df[df["doc_type"].isin(req.doc_types)]
    if req.modulo == "RRHH":
        if req.status:
            df = df[df["estado"] == req.status]
        if req.dept:
            df = df[df["departamento"] == req.dept]
    else:
        if req.author:
            df = df[df["autor"] == req.author]
        if req.only_recent:
            cutoff = datetime.now().year - 2
            df = df[pd.to_datetime(df["fecha"], errors="coerce").dt.year >= cutoff]

    total_docs = len(df)
    categories_count = len(df["doc_type"].dropna().unique())

    by_type = []
    if total_docs > 0:
        for t_name, count in df["doc_type"].value_counts().items():
            by_type.append({
                "type":  t_name,
                "count": int(count),
                "pct":   int(round(count / total_docs * 100)),
            })

    timeline = []
    if total_docs > 0:
        years = pd.to_datetime(df[fecha_col], errors="coerce").dt.year.dropna().astype(int)
        if not years.empty:
            conteos = years.value_counts().sort_index()
            max_c = int(conteos.max())
            for y_val, count in conteos.items():
                timeline.append({
                    "year":      str(y_val),
                    "count":     int(count),
                    "pct_width": int(round(count / max_c * 100)) if max_c > 0 else 0,
                })

    return {
        "total_docs":       total_docs,
        "categories_count": categories_count,
        "by_type":          by_type,
        "timeline":         timeline,
        "system": {
            "status": "Operativo" if total_docs > 0 else "Sin Datos",
            "ram":    f"{round(30 + total_docs * 0.005, 2)} MB",
            "cpu":    multiprocessing.cpu_count(),
            "os":     f"{platform.system()} {platform.release()}",
        },
    }


@router.post("/submit")
def admin_submit(req: DocumentSubmitRequest):
    log_event(req.usuario, "Create Document", req.modulo, f"Tipo: {req.doc_type}, Ubicacion: {req.ubicacion}")
    creado_por = _resolve_user_id(req.usuario)
    fecha_doc  = req.fecha or datetime.now().strftime("%Y-%m-%d")

    if req.modulo == "Archivo":
        tipo_id = _resolve_or_create_tipo_documento(req.doc_type, cat_slug="archivo")
        new_row = db_query(
            """
            INSERT INTO public.datos_archivo
                (titulo, abstract, autor,
                 fecha_documento, ubicacion, creado_por, tesauro_primario,
                 tesauro_secundario, id_tipo_documento)
            VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s)
            RETURNING id_archivo
            """,
            (
                req.titulo or "Sin título",
                req.resumen or "",
                req.autor or "Anónimo",
                fecha_doc, req.ubicacion, creado_por, req.doc_type,
                req.tesauro_secundario or "", tipo_id,
            ),
            fetch="one",
            commit=True,
        )

        # Guardar palabras clave relacionales (descriptores libres)
        if req.descriptores_libres:
            raw_descs = req.descriptores_libres.replace(";", ",").split(",")
            descriptores = [d.strip() for d in raw_descs if d.strip()]
            for desc in descriptores:
                desc_row = db_query(
                    """
                    INSERT INTO public.descriptores_libres (nombre)
                    VALUES (%s)
                    ON CONFLICT (nombre) DO UPDATE SET nombre = EXCLUDED.nombre
                    RETURNING id_descriptor
                    """,
                    (desc,),
                    fetch="one",
                    commit=True,
                )
                db_query(
                    """
                    INSERT INTO public.archivo_descriptores (id_archivo, id_descriptor)
                    VALUES (%s, %s)
                    ON CONFLICT DO NOTHING
                    """,
                    (new_row["id_archivo"], desc_row["id_descriptor"]),
                    fetch="none",
                    commit=True,
                )

        invalidate_choices_cache()
        return {"success": True, "id": str(new_row["id_archivo"])}

    # ── Módulo RRHH ──────────────────────────────────────────────────────────
    tipo_id = _resolve_or_create_tipo_documento(req.doc_type)
    cedula = (req.cedula or "").strip()
    if not cedula:
        raise HTTPException(status_code=400, detail="Cédula es requerida para RRHH")

    emp_row = db_query("SELECT id FROM public.empleados WHERE cedula = %s", (cedula,), fetch="one")
    if not emp_row:
        cargo_id   = _resolve_or_create_lookup("cargos",            req.cargo,        "Por Asignar")
        dept_id    = _resolve_or_create_lookup("departamentos",     req.departamento, "Por Asignar")
        estado_id  = _resolve_or_create_lookup("estados_laborales", req.estado,       "Pendiente de Registro")
        emp_row = db_query(
            """
            INSERT INTO public.empleados
                (cedula, nombres, apellidos, rif, cargo_id, departamento_id,
                 estado_id, fecha_ingreso, fecha_jubilacion, fecha_pension, foto_url)
            VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
            RETURNING id
            """,
            (
                cedula,
                (req.nombres or req.empleado or cedula).strip(),
                (req.apellidos or "").strip(),
                req.rif or None,
                cargo_id, dept_id, estado_id, fecha_doc,
                req.fecha_jubilacion or None, req.fecha_pension or None, req.foto_url or None,
            ),
            fetch="one",
            commit=True,
        )

    new_row = db_query(
        """
        INSERT INTO public.datos_rrhh
            (titulo, autor, id_tipo_documento,
             empleado_id, fecha_documento, ubicacion, creado_por, 
             tesauro_primario, tesauro_secundario, abstract)
        VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
        RETURNING id_rrhh
        """,
        (
            f"{req.doc_type} de {req.personas_relacionadas or req.empleado}",
            "Recursos Humanos",
            tipo_id, emp_row["id"], fecha_doc, req.ubicacion, creado_por, 
            req.doc_type, req.tesauro_secundario or "", req.resumen or "",
        ),
        fetch="one",
        commit=True,
    )

    # Guardar palabras clave relacionales (descriptores libres) para RRHH
    if req.descriptores_libres:
        raw_descs = req.descriptores_libres.replace(";", ",").split(",")
        descriptores = [d.strip() for d in raw_descs if d.strip()]
        for desc in descriptores:
            desc_row = db_query(
                """
                INSERT INTO public.descriptores_libres (nombre)
                VALUES (%s)
                ON CONFLICT (nombre) DO UPDATE SET nombre = EXCLUDED.nombre
                RETURNING id_descriptor
                """,
                (desc,),
                fetch="one",
                commit=True,
            )
            db_query(
                """
                INSERT INTO public.rrhh_descriptores (id_rrhh, id_descriptor)
                VALUES (%s, %s)
                ON CONFLICT DO NOTHING
                """,
                (new_row["id_rrhh"], desc_row["id_descriptor"]),
                fetch="none",
                commit=True,
            )

    invalidate_choices_cache()
    return {"success": True, "id": str(new_row["id_rrhh"])}


@router.get("/list_all")
def list_all_files(
    modulo: str,
    search: Optional[str] = "",
    type_filter: Optional[str] = "",
    person_filter: Optional[str] = "",
    page: int = 1,
    per_page: int = 25,
):
    page     = max(1, page)
    per_page = max(1, min(per_page, 100))
    offset   = (page - 1) * per_page

    if modulo == "Archivo":
        conditions, params = [], []
        if search:
            conditions.append(
                "(unaccent(da.titulo) ILIKE unaccent(%s) OR unaccent(da.autor) ILIKE unaccent(%s))"
            )
            params.extend([f"%{search}%", f"%{search}%"])
        if type_filter:
            conditions.append("da.tesauro_primario = %s")
            params.append(type_filter)
        if person_filter:
            conditions.append("unaccent(COALESCE(da.autor,'')) ILIKE unaccent(%s)")
            params.append(f"%{person_filter}%")

        where = ("WHERE " + " AND ".join(conditions)) if conditions else ""

        count_row = db_query(
            f"SELECT COUNT(*) AS total FROM public.datos_archivo da {where}",
            params or None, fetch="one",
        )
        total = int(count_row["total"]) if count_row else 0

        rows = db_query(
            f"""
            SELECT
                da.id_archivo AS id,
                da.titulo,
                COALESCE(da.autor, '') AS autor,
                TO_CHAR(da.fecha_documento, 'YYYY-MM-DD') AS fecha,
                COALESCE(da.tesauro_primario, '') AS doc_type,
                COALESCE(da.tesauro_secundario, '') AS tesauro_secundario,
                COALESCE(da.ubicacion, '') AS ubicacion,
                COALESCE(da.abstract, '') AS resumen,
                COALESCE(da.file_url, '') AS file_url
            FROM public.datos_archivo da
            {where}
            ORDER BY da.fecha_documento DESC NULLS LAST
            LIMIT %s OFFSET %s
            """,
            (params + [per_page, offset]) if params else [per_page, offset],
            fetch="all",
        ) or []

        records = [dict(r) for r in rows]
        for idx, r in enumerate(records):
            r["__idx"] = offset + idx + 1

    else:
        conditions, params = [], []
        if search:
            conditions.append(
                """(unaccent(e.nombres || ' ' || e.apellidos) ILIKE unaccent(%s)
                   OR e.cedula ILIKE %s)"""
            )
            params.extend([f"%{search}%", f"%{search}%"])
        if type_filter:
            conditions.append("COALESCE(td.nombre_corto, td.nombre) = %s")
            params.append(type_filter)
        if person_filter:
            conditions.append("unaccent(e.nombres || ' ' || e.apellidos) ILIKE unaccent(%s)")
            params.append(f"%{person_filter}%")

        join = """
            FROM public.empleados e
            LEFT JOIN public.cargos c ON e.cargo_id = c.id
            LEFT JOIN public.departamentos d ON e.departamento_id = d.id
            LEFT JOIN public.estados_laborales el ON e.estado_id = el.id
            LEFT JOIN public.datos_rrhh dr ON dr.empleado_id = e.id
            LEFT JOIN public.tipo_documento td ON dr.id_tipo_documento = td.id
        """
        where = ("WHERE " + " AND ".join(conditions)) if conditions else ""

        count_row = db_query(
            f"SELECT COUNT(DISTINCT e.id) AS total {join} {where}",
            params or None, fetch="one",
        )
        total = int(count_row["total"]) if count_row else 0

        rows = db_query(
            f"""
            SELECT DISTINCT
                e.id AS empleado_id,
                e.cedula,
                e.nombres || ' ' || e.apellidos AS empleado,
                COALESCE(d.nombre, '') AS departamento,
                COALESCE(el.estados, '') AS estado,
                COALESCE(c.nombre, '') AS cargo,
                TO_CHAR(e.fecha_ingreso, 'YYYY-MM-DD') AS fecha_ingreso,
                COALESCE(td.nombre_corto, td.nombre, '') AS doc_type,
                COALESCE(dr.ubicacion, '') AS ubicacion
            {join}
            {where}
            ORDER BY e.nombres ASC
            LIMIT %s OFFSET %s
            """,
            (params + [per_page, offset]) if params else [per_page, offset],
            fetch="all",
        ) or []

        records = [dict(r) for r in rows]
        for idx, r in enumerate(records):
            r["__idx"] = offset + idx + 1

    return {
        "total":    total,
        "page":     page,
        "per_page": per_page,
        "records":  records,
    }


@router.post("/add_category")
def add_category(req: CategoryCreateRequest):
    log_event(req.usuario, "Create Category", req.scope, f"Nueva Tipología: {req.name}")
    nombre = (req.name or "").strip()
    if not nombre:
        raise HTTPException(400, "Nombre vacío")

    # Determinar categoría destino
    if req.parte:
        cat_slug = req.parte  # "parte-i", "parte-ii", "archivo", etc.
    elif req.scope == "Archivo":
        cat_slug = "archivo"
    else:
        cat_slug = "parte-i"  # fallback RRHH

    cat = db_query("SELECT id FROM public.categoria WHERE slug = %s", (cat_slug,), fetch="one")
    if not cat:
        cat = db_query("SELECT id FROM public.categoria ORDER BY id LIMIT 1", fetch="one")
    if not cat:
        raise HTTPException(500, "No hay categorías en la BD")

    existing = db_query("SELECT id FROM public.tipo_documento WHERE LOWER(nombre) = LOWER(%s)", (nombre,), fetch="one")
    if existing:
        return {"success": True, "detail": "Ya existe"}

    slug = generate_unique_slug(nombre, "tipo_documento")
    db_query(
        "INSERT INTO public.tipo_documento (nombre, nombre_corto, slug, id_categoria) VALUES (%s, %s, %s, %s)",
        (nombre, nombre, slug, cat["id"]),
        fetch="none", commit=True,
    )
    invalidate_choices_cache()
    return {"success": True}


@router.get("/keywords")
def get_keywords():
    rows = db_query(
        """SELECT dl.id_descriptor AS id, dl.nombre,
                  COUNT(DISTINCT ad.id_archivo) AS uso_archivo
           FROM public.descriptores_libres dl
           LEFT JOIN public.archivo_descriptores ad ON ad.id_descriptor = dl.id_descriptor
           GROUP BY dl.id_descriptor, dl.nombre
           ORDER BY dl.nombre""",
        fetch="all",
    ) or []
    return [dict(r) for r in rows]


@router.post("/keywords")
def create_keyword(req: KeywordRequest):
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
    return {"success": True, "id": row["id"]}


@router.put("/keywords/{kid}")
def update_keyword(kid: int, req: KeywordRequest):
    nombre = (req.nombre or "").strip()
    if not nombre:
        raise HTTPException(400, "Nombre vacío")
    db_query(
        "UPDATE public.descriptores_libres SET nombre = %s WHERE id_descriptor = %s",
        (nombre, kid), fetch="none", commit=True,
    )
    return {"success": True}


@router.delete("/keywords/{kid}")
def delete_keyword(kid: int):
    db_query(
        "DELETE FROM public.descriptores_libres WHERE id_descriptor = %s",
        (kid,), fetch="none", commit=True,
    )
    return {"success": True}


@router.get("/users")
def get_users_list():
    rows = db_query(
        "SELECT id, usuario, nombre_usuario, modulo, rol "
        "FROM public.usuarios_sistema ORDER BY usuario",
        fetch="all",
    ) or []
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


# =============================================================================
# EDICIÓN Y ELIMINACIÓN DE DOCUMENTOS
# =============================================================================

@router.put("/documento/{doc_id}")
def update_documento(doc_id: int, req: DocumentUpdateRequest):
    updated_by = _resolve_user_id(req.usuario)
    updated_at = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    if req.modulo == "Archivo":
        set_clauses, params = [], []

        if req.titulo is not None:
            set_clauses.append("titulo = %s"); params.append(req.titulo)
        if req.autor is not None:
            set_clauses.append("autor = %s"); params.append(req.autor)
        if req.resumen is not None:
            set_clauses.append("abstract = %s"); params.append(req.resumen)
        if req.fecha is not None:
            set_clauses.append("fecha_documento = %s"); params.append(req.fecha)
        if req.ubicacion is not None:
            set_clauses.append("ubicacion = %s"); params.append(req.ubicacion)
        if req.tesauro_secundario is not None:
            set_clauses.append("tesauro_secundario = %s"); params.append(req.tesauro_secundario)

        if req.doc_type is not None:
            tipo_id = _resolve_or_create_tipo_documento(req.doc_type, cat_slug="archivo")
            set_clauses.append("id_tipo_documento = %s"); params.append(tipo_id)
            set_clauses.append("tesauro_primario = %s"); params.append(req.doc_type)

        set_clauses.append("updated_at = %s"); params.append(updated_at)
        set_clauses.append("updated_by = %s"); params.append(updated_by)

        if set_clauses:
            params.append(doc_id)
            db_query(
                f"UPDATE public.datos_archivo SET {', '.join(set_clauses)} WHERE id_archivo = %s",
                params, fetch="none", commit=True,
            )

        if req.palabras_clave is not None:
            db_query(
                "DELETE FROM public.archivo_descriptores WHERE id_archivo = %s",
                (doc_id,), fetch="none", commit=True,
            )
            raw_descs = req.palabras_clave.replace(";", ",").split(",")
            descriptores = [d.strip() for d in raw_descs if d.strip()]
            for desc in descriptores:
                desc_row = db_query(
                    "INSERT INTO public.descriptores_libres (nombre) VALUES (%s) "
                    "ON CONFLICT (nombre) DO UPDATE SET nombre = EXCLUDED.nombre RETURNING id_descriptor",
                    (desc,), fetch="one", commit=True,
                )
                db_query(
                    "INSERT INTO public.archivo_descriptores (id_archivo, id_descriptor) VALUES (%s, %s) ON CONFLICT DO NOTHING",
                    (doc_id, desc_row["id_descriptor"]), fetch="none", commit=True,
                )

        invalidate_choices_cache()
        log_event(req.usuario, "Update Document", "Archivo", f"ID: {doc_id}")
        return {"success": True}

    else:  # RRHH
        set_clauses, params = [], []

        if req.titulo is not None:
            set_clauses.append("titulo = %s"); params.append(req.titulo)
        if req.autor is not None:
            set_clauses.append("autor = %s"); params.append(req.autor)
        if req.resumen is not None:
            set_clauses.append("abstract = %s"); params.append(req.resumen)
        if req.fecha is not None:
            set_clauses.append("fecha_documento = %s"); params.append(req.fecha)
        if req.ubicacion is not None:
            set_clauses.append("ubicacion = %s"); params.append(req.ubicacion)
        if req.tesauro_secundario is not None:
            set_clauses.append("tesauro_secundario = %s"); params.append(req.tesauro_secundario)
        if req.doc_type is not None:
            set_clauses.append("tesauro_primario = %s"); params.append(req.doc_type)

        set_clauses.append("updated_at = %s"); params.append(updated_at)
        set_clauses.append("updated_by = %s"); params.append(updated_by)

        if set_clauses:
            params.append(doc_id)
            db_query(
                f"UPDATE public.datos_rrhh SET {', '.join(set_clauses)} WHERE id_rrhh = %s",
                params, fetch="none", commit=True,
            )

        invalidate_choices_cache()
        log_event(req.usuario, "Update Document", "RRHH", f"ID: {doc_id}")
        return {"success": True}


@router.delete("/documento/{doc_id}")
def delete_documento(doc_id: int, modulo: str, usuario: str):
    if modulo == "Archivo":
        db_query(
            "DELETE FROM public.archivo_descriptores WHERE id_archivo = %s",
            (doc_id,), fetch="none", commit=True,
        )
        db_query(
            "DELETE FROM public.datos_archivo WHERE id_archivo = %s",
            (doc_id,), fetch="none", commit=True,
        )
    else:
        db_query(
            "DELETE FROM public.rrhh_descriptores WHERE id_rrhh = %s",
            (doc_id,), fetch="none", commit=True,
        )
        db_query(
            "DELETE FROM public.datos_rrhh WHERE id_rrhh = %s",
            (doc_id,), fetch="none", commit=True,
        )

    invalidate_choices_cache()
    log_event(usuario, "Delete Document", modulo, f"ID: {doc_id}")
    return {"success": True}


@router.put("/empleado/{emp_id}")
def update_empleado(emp_id: int, req: EmpleadoUpdateRequest):
    set_clauses, params = [], []

    if req.nombres is not None:
        set_clauses.append("nombres = %s"); params.append(req.nombres)
    if req.apellidos is not None:
        set_clauses.append("apellidos = %s"); params.append(req.apellidos)
    if req.rif is not None:
        set_clauses.append("rif = %s"); params.append(req.rif)
    if req.foto_url is not None:
        set_clauses.append("foto_url = %s"); params.append(req.foto_url)
    if req.fecha_jubilacion is not None:
        set_clauses.append("fecha_jubilacion = %s")
        params.append(req.fecha_jubilacion if req.fecha_jubilacion else None)
    if req.fecha_pension is not None:
        set_clauses.append("fecha_pension = %s")
        params.append(req.fecha_pension if req.fecha_pension else None)

    if req.cargo is not None:
        cargo_id = _resolve_or_create_lookup("cargos", req.cargo)
        set_clauses.append("cargo_id = %s"); params.append(cargo_id)
    if req.departamento is not None:
        dept_id = _resolve_or_create_lookup("departamentos", req.departamento)
        set_clauses.append("departamento_id = %s"); params.append(dept_id)
    if req.estado is not None:
        estado_id = _resolve_or_create_lookup("estados_laborales", req.estado)
        set_clauses.append("estado_id = %s"); params.append(estado_id)

    if set_clauses:
        params.append(emp_id)
        db_query(
            f"UPDATE public.empleados SET {', '.join(set_clauses)} WHERE id = %s",
            params, fetch="none", commit=True,
        )

    log_event(req.usuario, "Update Empleado", "RRHH", f"ID: {emp_id}")
    return {"success": True}


@router.delete("/empleado/{emp_id}")
def delete_empleado(emp_id: int, usuario: str):
    db_query(
        """DELETE FROM public.rrhh_descriptores
           WHERE id_rrhh IN (SELECT id_rrhh FROM public.datos_rrhh WHERE empleado_id = %s)""",
        (emp_id,), fetch="none", commit=True,
    )
    db_query(
        "DELETE FROM public.datos_rrhh WHERE empleado_id = %s",
        (emp_id,), fetch="none", commit=True,
    )
    db_query(
        "DELETE FROM public.empleados WHERE id = %s",
        (emp_id,), fetch="none", commit=True,
    )
    log_event(usuario, "Delete Empleado", "RRHH", f"ID: {emp_id}")
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
