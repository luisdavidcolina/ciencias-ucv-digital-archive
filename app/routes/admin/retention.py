"""
GestiÃ³n de plazos de retenciÃ³n documental.

EstÃ¡ndar de referencia: ISO 15489-1:2016 Â§8 (Procesos y controles de gestiÃ³n de documentos)
y Ley de Archivos Nacionales de Venezuela (1945, vigente con reformas).

El plazo de retenciÃ³n establece cuÃ¡ntos aÃ±os debe conservarse un tipo de documento
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
            raise ValueError("El plazo de retenciÃ³n debe estar entre 1 y 100 aÃ±os")
        return v


@router.get("/retencion/tipos")
def list_retention_types(scope: str = Query(default="")):
    """
    Lista todos los tipos de documento con su plazo de retenciÃ³n configurado.
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
            COUNT(da.id_archivo)                    AS uso_archivo,
            COUNT(dr.id_rrhh)                       AS uso_rrhh
        FROM public.tipo_documento td
        JOIN public.categoria c ON td.id_categoria = c.id
        LEFT JOIN public.datos_archivo da ON da.id_tipo_documento = td.id
        LEFT JOIN public.datos_rrhh    dr ON dr.id_tipo_documento = td.id
        {where}
        GROUP BY td.id, td.nombre, td.nombre_corto, td.plazo_retencion_anios, c.nombre, c.slug
        ORDER BY c.id, td.nombre
    """, params or None, fetch="all") or []

    return {"tipos": [dict(r) for r in rows]}


@router.patch("/retencion/tipos/{tipo_id}")
def update_retention(tipo_id: int, data: RetencionUpdate):
    """Actualiza el plazo de retenciÃ³n de un tipo de documento."""
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
        "RetenciÃ³n Actualizada",
        "Admin",
        f"tipo_id={tipo_id} nombre='{existing['nombre']}' plazo={data.plazo_retencion_anios} aÃ±os",
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
    Documentos del mÃ³dulo Archivo cuyo plazo de retenciÃ³n ha expirado.
    Ordenados por antigÃ¼edad de vencimiento (ISO 15489-1:2016 Â§8.5 â€“ DisposiciÃ³n).
    """
    rows = db_query("""
        SELECT
            da.id_archivo,
            da.titulo,
            COALESCE(da.autor, 'â€”')                     AS autor,
            TO_CHAR(da.fecha_documento, 'YYYY-MM-DD')   AS fecha_documento,
            COALESCE(da.ubicacion, 'â€”')                 AS ubicacion,
            COALESCE(da.soporte, 'FÃ­sico')              AS soporte,
            COALESCE(td.nombre_corto, 'â€”')              AS tipo_documento,
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
        ORDER BY dias_vencido DESC
        LIMIT %s
    """, [limite], fetch="all") or []

    return {"total": len(rows), "vencimientos": [dict(r) for r in rows]}
