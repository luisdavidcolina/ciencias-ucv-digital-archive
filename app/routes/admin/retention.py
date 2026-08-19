"""
Gestión de plazos de retención documental.

Estándar de referencia: ISO 15489-1:2016 §8 (Procesos y controles de gestión de documentos)
y Ley de Archivos Nacionales de Venezuela (1945, vigente con reformas).

El plazo de retención establece cuántos años debe conservarse un tipo de documento
antes de poder ser eliminado o transferido al archivo permanente.
"""
from typing import Optional

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, field_validator

from database import db_query, log_event
from ..lookups import invalidate_choices_cache

router = APIRouter()


class RetencionUpdate(BaseModel):
    plazo_retencion_anios: int
    requester: Optional[str] = ""

    @field_validator("plazo_retencion_anios")
    @classmethod
    def plazo_must_be_positive(cls, v):
        if v < 1 or v > 100:
            raise ValueError("El plazo de retención debe estar entre 1 y 100 años")
        return v


@router.get("/retencion/tipos")
def list_retention_types(scope: str = Query(default="")):
    """
    Lista todos los tipos de documento con su plazo de retención configurado.
    Filtra por scope ('archivo' | 'rrhh' | '' para todos).
    """
    params = []
    where = ""
    if scope:
        where = "WHERE LOWER(c.slug) LIKE %s"
        params.append(f"%{scope.lower()}%")

    rows = db_query(f"""
        SELECT
            td.id,
            td.nombre,
            td.nombre_corto,
            COALESCE(td.plazo_retencion_anios, 5)  AS plazo_retencion_anios,
            c.nombre                                AS categoria,
            c.slug                                  AS categoria_slug,
            COUNT(DISTINCT da.id_archivo)           AS uso_archivo,
            COUNT(DISTINCT dr.id_rrhh)              AS uso_rrhh
        FROM public.tipo_documento td
        JOIN public.categoria c ON td.id_categoria = c.id
        LEFT JOIN public.datos_archivo da ON da.id_tipo_documento = td.id
        LEFT JOIN public.datos_rrhh    dr ON dr.id_tipo_documento = td.id
        {where}
        GROUP BY td.id, td.nombre, td.nombre_corto, td.plazo_retencion_anios, c.id, c.nombre, c.slug
        ORDER BY c.id, td.nombre
    """, params or None, fetch="all") or []

    return {"tipos": [dict(r) for r in rows]}


@router.patch("/retencion/tipos/{tipo_id}")
def update_retention(tipo_id: int, data: RetencionUpdate):
    """Actualiza el plazo de retención de un tipo de documento."""
    existing = db_query(
        "SELECT id, nombre FROM public.tipo_documento WHERE id = %s", [tipo_id], fetch="one"
    )
    if not existing:
        raise HTTPException(status_code=404, detail="Tipo de documento no encontrado")

    db_query(
        "UPDATE public.tipo_documento SET plazo_retencion_anios = %s WHERE id = %s",
        [data.plazo_retencion_anios, tipo_id], fetch="none", commit=True
    )
    invalidate_choices_cache()
    log_event(
        data.requester or "sistema",
        "Retención Actualizada",
        "Admin",
        f"tipo_id={tipo_id} nombre='{existing['nombre']}' plazo={data.plazo_retencion_anios} años",
    )
    return {
        "success": True,
        "tipo_id": tipo_id,
        "nombre": existing["nombre"],
        "plazo_retencion_anios": data.plazo_retencion_anios,
    }


@router.get("/retencion/vencimientos")
def get_expired_docs(limite: int = Query(default=50, ge=1, le=500)):
    """
    Documentos del módulo Archivo cuyo plazo de retención ha expirado.
    Ordenados por antigüedad de vencimiento (ISO 15489-1:2016 §8.5 – Disposición).
    """
    rows = db_query("""
        SELECT
            da.id_archivo,
            da.titulo,
            COALESCE(da.autor, '—')                     AS autor,
            TO_CHAR(da.fecha_documento, 'YYYY-MM-DD')   AS fecha_documento,
            COALESCE(da.ubicacion, '—')                 AS ubicacion,
            COALESCE(da.soporte, 'Físico')              AS soporte,
            COALESCE(td.nombre_corto, '—')              AS tipo_documento,
            COALESCE(td.plazo_retencion_anios, 5)       AS plazo_anios,
            TO_CHAR(
                (da.fecha_documento + (COALESCE(td.plazo_retencion_anios,5) || ' years')::INTERVAL)::DATE,
                'YYYY-MM-DD'
            )                                           AS fecha_vencimiento,
            (CURRENT_DATE - (da.fecha_documento + (COALESCE(td.plazo_retencion_anios,5) || ' years')::INTERVAL)::DATE
            )                                           AS dias_vencido
        FROM public.datos_archivo da
        LEFT JOIN public.tipo_documento td ON da.id_tipo_documento = td.id
        WHERE da.fecha_documento IS NOT NULL
          AND (da.fecha_documento + (COALESCE(td.plazo_retencion_anios, 5) || ' years')::INTERVAL)::DATE < CURRENT_DATE
          AND COALESCE(da.status, 'aprobado') = 'aprobado'
          AND da.disposicion IS NULL
          AND da.deleted_at IS NULL
        ORDER BY dias_vencido DESC
        LIMIT %s
    """, [limite], fetch="all") or []

    return {"total": len(rows), "vencimientos": [dict(r) for r in rows]}


# =============================================================================
# DISPOSICIÓN DOCUMENTAL (ISO 15489-1:2016 §8.5)
# =============================================================================
# El sistema avisaba de los documentos con el plazo vencido pero no ofrecía
# ninguna acción, así que la decisión archivística no quedaba en ninguna parte.
#
# Disponer NO borra: registra qué se decidió, quién lo decidió, cuándo y con qué
# acta. Eso es lo que un archivo tiene que poder demostrar años después. Para
# retirar el documento de la vista está la papelera, que es otra cosa.

DISPOSICIONES = {
    "conservar":   "Conservación permanente",
    "transferido": "Transferido al archivo histórico",
    "eliminado":   "Eliminado por expurgo",
}


class DisposicionIn(BaseModel):
    disposicion: str
    acta: Optional[str] = ""
    requester: Optional[str] = ""

    @field_validator("disposicion")
    @classmethod
    def debe_ser_conocida(cls, v):
        if v not in DISPOSICIONES:
            raise ValueError(
                "disposicion debe ser una de: " + ", ".join(sorted(DISPOSICIONES))
            )
        return v


@router.post("/retencion/disponer/{doc_id}")
def registrar_disposicion(doc_id: int, data: DisposicionIn):
    """Deja constancia de la decisión de disposición sobre un documento."""
    fila = db_query(
        """SELECT id_archivo, titulo, disposicion
           FROM public.datos_archivo
           WHERE id_archivo = %s AND deleted_at IS NULL""",
        [doc_id], fetch="one",
    )
    if not fila:
        raise HTTPException(404, "El documento no existe o está en la papelera")
    if fila["disposicion"]:
        raise HTTPException(
            409,
            f"El documento ya tiene una disposición registrada "
            f"({DISPOSICIONES.get(fila['disposicion'], fila['disposicion'])}). "
            "Rectificarla exige un acta nueva."
        )

    db_query(
        """UPDATE public.datos_archivo
           SET disposicion = %s, disposicion_fecha = CURRENT_DATE,
               disposicion_acta = %s, disposicion_por = %s
           WHERE id_archivo = %s""",
        [data.disposicion, (data.acta or "").strip() or None,
         (data.requester or "sistema").strip(), doc_id],
        fetch="none", commit=True,
    )
    log_event(data.requester or "sistema", "Disposición Documental", "Archivo",
              f"doc_id={doc_id}, {DISPOSICIONES[data.disposicion]}, "
              f"acta: {(data.acta or '—')[:60]}")
    return {"success": True, "doc_id": doc_id,
            "disposicion": data.disposicion,
            "etiqueta": DISPOSICIONES[data.disposicion]}


@router.get("/retencion/disposiciones")
def listar_disposiciones(limite: int = Query(default=100, ge=1, le=500)):
    """Historial de disposiciones: es el registro que un archivo debe conservar."""
    filas = db_query(
        """SELECT da.id_archivo, da.titulo, da.disposicion,
                  TO_CHAR(da.disposicion_fecha, 'YYYY-MM-DD') AS fecha,
                  da.disposicion_acta AS acta, da.disposicion_por AS responsable,
                  COALESCE(td.nombre_corto, td.nombre, '—')   AS tipo
           FROM public.datos_archivo da
           LEFT JOIN public.tipo_documento td ON da.id_tipo_documento = td.id
           WHERE da.disposicion IS NOT NULL
           ORDER BY da.disposicion_fecha DESC NULLS LAST, da.id_archivo DESC
           LIMIT %s""",
        [limite], fetch="all",
    ) or []
    return {
        "total": len(filas),
        "disposiciones": [
            dict(f, etiqueta=DISPOSICIONES.get(f["disposicion"], f["disposicion"]))
            for f in filas
        ],
    }
