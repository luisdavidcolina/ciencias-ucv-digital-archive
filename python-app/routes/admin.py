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
    UserCreateRequest,
)
from utils import generate_unique_slug
from .archivo import fetch_archivo_dataframe
from .rrhh import fetch_rrhh_dataframe

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


def _resolve_or_create_tipo_documento(nombre: str) -> int:
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
    tipo_id    = _resolve_or_create_tipo_documento(req.doc_type)
    fecha_doc  = req.fecha or datetime.now().strftime("%Y-%m-%d")

    if req.modulo == "Archivo":
        new_row = db_query(
            """
            INSERT INTO public.datos_archivo
                (titulo, abstract, autor,
                 fecha_documento, ubicacion, creado_por, tesauro_primario,
                 tesauro_secundario)
            VALUES (%s, %s, %s, %s, %s, %s, %s, %s)
            RETURNING id_archivo
            """,
            (
                req.titulo or "Sin título",
                req.resumen or "",
                req.autor or "Anónimo",
                fecha_doc, req.ubicacion, creado_por, req.doc_type,
                req.tesauro_secundario or "",
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

        return {"success": True, "id": str(new_row["id_archivo"])}

    # ── Módulo RRHH ──────────────────────────────────────────────────────────
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
                req.empleado or cedula,  # nombres (temporalmente usando empleado)
                "",                        # apellidos (vacío por ahora)
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

    return {"success": True, "id": str(new_row["id_rrhh"])}


@router.get("/list_all")
def list_all_files(modulo: str, search: Optional[str] = "", type_filter: Optional[str] = ""):
    if modulo == "Archivo":
        df = fetch_archivo_dataframe()
        if search:
            s = search.lower().strip()
            df = df[
                df["titulo"].astype(str).str.lower().str.contains(s, na=False)
                | df["autor"].astype(str).str.lower().str.contains(s, na=False)
            ]
        if type_filter:
            df = df[df["doc_type"] == type_filter]
    else:
        df = fetch_rrhh_dataframe()
        if search:
            s = search.lower().strip()
            df = df[
                df["empleado"].astype(str).str.lower().str.contains(s, na=False)
                | df["cedula"].astype(str).str.lower().str.contains(s, na=False)
            ]
        if type_filter:
            df = df[df["doc_type"] == type_filter]

    records = df.to_dict(orient="records")
    for idx, r in enumerate(records):
        r["__idx"] = idx + 1
    return records


@router.post("/add_category")
def add_category(req: CategoryCreateRequest):
    log_event(req.usuario, "Create Category", req.scope, f"Nueva Tipología: {req.name}")
    try:
        _resolve_or_create_tipo_documento(req.name)
    except Exception as e:
        logger.error(f"Error creando tipo_documento: {e}")
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
