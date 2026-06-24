"""
Alertas operativas de RRHH y gestión de historial de cargos.

Estándares aplicados:
  - LOTTT (Venezuela) Art. 147: derecho a información del expediente
  - ISO 30300:2011: sistemas de gestión para documentos de RRHH
"""
from datetime import date
from typing import Optional

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, validator

from database import db_query, log_event

router = APIRouter(prefix="/api/rrhh", tags=["rrhh-alertas"])


# =============================================================================
# ALERTAS DE JUBILACIÓN / VENCIMIENTOS
# =============================================================================

@router.get("/alertas/jubilaciones")
def get_alertas_jubilaciones(horizonte_dias: int = Query(default=365, ge=30, le=730)):
    """
    Empleados cuya fecha_jubilacion o fecha_pension cae dentro del horizonte
    indicado (default: 365 días). Útil para planificación de recursos.
    """
    rows = db_query("""
        SELECT
            e.id                                         AS empleado_id,
            e.cedula,
            e.nombres || ' ' || e.apellidos              AS nombre_completo,
            COALESCE(c.nombre, 'Sin cargo')              AS cargo,
            COALESCE(d.nombre, 'Sin departamento')       AS departamento,
            COALESCE(el.estados, 'Sin estado')           AS estado,
            TO_CHAR(e.fecha_jubilacion, 'YYYY-MM-DD')    AS fecha_jubilacion,
            TO_CHAR(e.fecha_pension,    'YYYY-MM-DD')    AS fecha_pension,
            TO_CHAR(e.fecha_nacimiento, 'YYYY-MM-DD')    AS fecha_nacimiento,
            CASE
                WHEN e.fecha_jubilacion IS NOT NULL
                     AND e.fecha_jubilacion BETWEEN CURRENT_DATE AND CURRENT_DATE + (%s || ' days')::INTERVAL
                     THEN 'Próxima Jubilación'
                WHEN e.fecha_pension IS NOT NULL
                     AND e.fecha_pension BETWEEN CURRENT_DATE AND CURRENT_DATE + (%s || ' days')::INTERVAL
                     THEN 'Próxima Pensión'
                WHEN e.fecha_jubilacion < CURRENT_DATE
                     THEN 'Jubilación Vencida (no procesada)'
                ELSE 'Alerta'
            END                                          AS tipo_alerta,
            CASE
                WHEN e.fecha_jubilacion IS NOT NULL AND e.fecha_jubilacion >= CURRENT_DATE
                     THEN (e.fecha_jubilacion - CURRENT_DATE)
                WHEN e.fecha_pension IS NOT NULL AND e.fecha_pension >= CURRENT_DATE
                     THEN (e.fecha_pension - CURRENT_DATE)
                ELSE 0
            END                                          AS dias_restantes
        FROM public.empleados e
        LEFT JOIN public.cargos            c  ON e.cargo_id        = c.id
        LEFT JOIN public.departamentos     d  ON e.departamento_id = d.id
        LEFT JOIN public.estados_laborales el ON e.estado_id       = el.id
        WHERE
            (
                e.fecha_jubilacion IS NOT NULL
                AND e.fecha_jubilacion BETWEEN CURRENT_DATE - INTERVAL '30 days'
                                           AND CURRENT_DATE + (%s || ' days')::INTERVAL
            )
            OR
            (
                e.fecha_pension IS NOT NULL
                AND e.fecha_pension BETWEEN CURRENT_DATE AND CURRENT_DATE + (%s || ' days')::INTERVAL
            )
        ORDER BY
            LEAST(
                COALESCE(e.fecha_jubilacion, '9999-12-31'::DATE),
                COALESCE(e.fecha_pension,    '9999-12-31'::DATE)
            ) ASC
    """, [horizonte_dias, horizonte_dias, horizonte_dias, horizonte_dias], fetch="all") or []

    return {
        "horizonte_dias": horizonte_dias,
        "total": len(rows),
        "alertas": [dict(r) for r in rows],
    }


@router.get("/alertas/documentos_vencidos")
def get_documentos_vencidos(limite: int = Query(default=50, ge=1, le=200)):
    """
    Documentos cuyo plazo de retención (tipo_documento.plazo_retencion_anios)
    ya expiró según la fecha del documento. Basado en ISO 15489-1:2016 §8.
    """
    rows = db_query("""
        SELECT
            da.id_archivo,
            da.titulo,
            da.autor,
            TO_CHAR(da.fecha_documento, 'YYYY-MM-DD') AS fecha_documento,
            da.ubicacion,
            da.soporte,
            td.nombre_corto                             AS tipo_documento,
            COALESCE(td.plazo_retencion_anios, 5)       AS plazo_anios,
            (da.fecha_documento + (COALESCE(td.plazo_retencion_anios,5) || ' years')::INTERVAL)::DATE
                                                        AS fecha_vencimiento,
            CURRENT_DATE - (da.fecha_documento + (COALESCE(td.plazo_retencion_anios,5) || ' years')::INTERVAL)::DATE
                                                        AS dias_vencido
        FROM public.datos_archivo da
        LEFT JOIN public.tipo_documento td ON da.id_tipo_documento = td.id
        WHERE da.fecha_documento IS NOT NULL
          AND (da.fecha_documento + (COALESCE(td.plazo_retencion_anios,5) || ' years')::INTERVAL)::DATE < CURRENT_DATE
          AND COALESCE(da.status, 'aprobado') = 'aprobado'
        ORDER BY dias_vencido DESC
        LIMIT %s
    """, [limite], fetch="all") or []

    return {
        "total": len(rows),
        "documentos": [dict(r) for r in rows],
    }


# =============================================================================
# HISTORIAL DE CARGOS (LOTTT Art. 147 – derecho al expediente)
# =============================================================================

class HistorialCargoIn(BaseModel):
    cargo_nombre: str
    fecha_inicio: date
    fecha_fin: Optional[date] = None
    motivo: Optional[str] = None
    registrado_por: Optional[str] = ""

    @validator("cargo_nombre")
    def cargo_not_empty(cls, v):
        v = (v or "").strip()
        if not v:
            raise ValueError("cargo_nombre no puede estar vacío")
        return v

    @validator("fecha_fin")
    def fin_after_inicio(cls, v, values):
        if v and "fecha_inicio" in values and v < values["fecha_inicio"]:
            raise ValueError("fecha_fin no puede ser anterior a fecha_inicio")
        return v


@router.get("/empleado/{empleado_id}/historial_cargos")
def get_historial_cargos(empleado_id: int):
    """Lista el historial de cargos de un empleado, del más reciente al más antiguo."""
    emp = db_query("SELECT id FROM public.empleados WHERE id = %s", [empleado_id], fetch="one")
    if not emp:
        raise HTTPException(status_code=404, detail="Empleado no encontrado")

    rows = db_query("""
        SELECT
            hc.id,
            c.nombre                                 AS cargo,
            TO_CHAR(hc.fecha_inicio, 'YYYY-MM-DD')  AS fecha_inicio,
            TO_CHAR(hc.fecha_fin,    'YYYY-MM-DD')  AS fecha_fin,
            hc.motivo,
            hc.registrado_por,
            TO_CHAR(hc.created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS"Z"') AS created_at
        FROM public.historial_cargos hc
        JOIN public.cargos c ON hc.cargo_id = c.id
        WHERE hc.empleado_id = %s
        ORDER BY hc.fecha_inicio DESC, hc.id DESC
    """, [empleado_id], fetch="all") or []

    return {"empleado_id": empleado_id, "historial": [dict(r) for r in rows]}


@router.post("/empleado/{empleado_id}/historial_cargos")
def add_historial_cargo(empleado_id: int, data: HistorialCargoIn):
    """
    Registra un movimiento de cargo en el expediente del empleado.
    Si el cargo no existe en el catálogo, se crea automáticamente.
    """
    emp = db_query("SELECT id FROM public.empleados WHERE id = %s", [empleado_id], fetch="one")
    if not emp:
        raise HTTPException(status_code=404, detail="Empleado no encontrado")

    # Resolver o crear el cargo en el catálogo
    cargo_row = db_query(
        "SELECT id FROM public.cargos WHERE LOWER(nombre) = LOWER(%s)",
        [data.cargo_nombre], fetch="one"
    )
    if not cargo_row:
        cargo_row = db_query(
            "INSERT INTO public.cargos (nombre) VALUES (%s) RETURNING id",
            [data.cargo_nombre], fetch="one", commit=True
        )
    cargo_id = cargo_row["id"]

    # Cerrar el registro abierto anterior (si fecha_fin no está seteada)
    if data.fecha_inicio:
        db_query("""
            UPDATE public.historial_cargos
               SET fecha_fin = %s
             WHERE empleado_id = %s AND fecha_fin IS NULL AND fecha_inicio < %s
        """, [data.fecha_inicio, empleado_id, data.fecha_inicio], fetch="none", commit=True)

    new_row = db_query("""
        INSERT INTO public.historial_cargos
            (empleado_id, cargo_id, fecha_inicio, fecha_fin, motivo, registrado_por)
        VALUES (%s, %s, %s, %s, %s, %s)
        RETURNING id
    """, [
        empleado_id, cargo_id,
        data.fecha_inicio, data.fecha_fin,
        data.motivo, data.registrado_por,
    ], fetch="one", commit=True)

    log_event(
        data.registrado_por or "sistema",
        "Historial Cargo Registrado",
        "RRHH",
        f"empleado_id={empleado_id} cargo={data.cargo_nombre} desde={data.fecha_inicio}",
    )

    return {"success": True, "id": new_row["id"], "cargo": data.cargo_nombre}


@router.delete("/empleado/{empleado_id}/historial_cargos/{historial_id}")
def delete_historial_cargo(empleado_id: int, historial_id: int, requester: str = Query(default="")):
    """Elimina una entrada del historial de cargos (solo para correcciones)."""
    row = db_query(
        "SELECT id FROM public.historial_cargos WHERE id = %s AND empleado_id = %s",
        [historial_id, empleado_id], fetch="one"
    )
    if not row:
        raise HTTPException(status_code=404, detail="Registro de historial no encontrado")

    db_query(
        "DELETE FROM public.historial_cargos WHERE id = %s",
        [historial_id], fetch="none", commit=True
    )
    log_event(requester or "sistema", "Historial Cargo Eliminado", "RRHH",
              f"historial_id={historial_id} empleado_id={empleado_id}")
    return {"success": True}
