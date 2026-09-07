"""
Alertas operativas de RRHH y gestión de historial de cargos.

Estándares aplicados:
  - LOTTT (Venezuela) Art. 147: derecho a información del expediente
  - ISO 30300:2011: sistemas de gestión para documentos de RRHH
"""
from datetime import date
from typing import Optional

from fastapi import APIRouter, Depends, HTTPException, Query
from pydantic import BaseModel, field_validator

from database import db_query, db_transaction, log_event
from routes.admin.deps import require_session, require_role, require_admin_role

# BR-001 / BR-002: mismo criterio que hr.py — sesion + modulo RRHH en todo el
# router (alertas de jubilacion/pension, vencimientos y historial de cargos).
router = APIRouter(
    prefix="/api/rrhh",
    tags=["rrhh-alertas"],
    dependencies=[Depends(require_session), Depends(require_role("RRHH"))],
)


# =============================================================================
# ALERTAS DE JUBILACIÓN / VENCIMIENTOS
# =============================================================================

@router.get("/alertas/jubilaciones")
def get_retirement_alerts(horizonte_dias: int = Query(default=365, ge=30, le=730)):
    """
    Empleados cuya fecha_jubilacion o fecha_pension cae dentro del horizonte
    indicado (default: 365 días). Útil para planificación de recursos.
    """
    # OR-022: sin corte inferior para lo vencido y no procesado — antes el
    # WHERE recortaba a 30 dias hacia atras y un caso sin procesar desde hace
    # meses (el que mas urge perseguir) desaparecia de la lista sin que nadie
    # lo notara.
    # OR-023: `e.deleted_at IS NULL` en el WHERE — un expediente en la
    # papelera seguia apareciendo en el banner de jubilaciones aunque el KPI
    # de stats.py (que si filtra) ya no lo contara.
    # BR-068 / OR-021: `tipo_alerta` cubre tambien la pension vencida (antes
    # caia en un ELSE 'Alerta' que no decia que habia que hacer), y
    # `dias_restantes` sale con signo (BR-069): positivo si falta, negativo si
    # ya vencio. Mezclar "vence hoy" con "vencido hace anios" en el mismo 0
    # hacia imposible ordenar por urgencia real.
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
                WHEN e.fecha_jubilacion IS NOT NULL AND e.fecha_jubilacion < CURRENT_DATE
                     THEN 'Jubilación Vencida (no procesada)'
                WHEN e.fecha_pension IS NOT NULL AND e.fecha_pension < CURRENT_DATE
                     THEN 'Pensión Vencida (no procesada)'
                ELSE 'Alerta'
            END                                          AS tipo_alerta,
            CASE
                WHEN e.fecha_jubilacion IS NOT NULL AND e.fecha_pension IS NOT NULL
                     THEN (LEAST(e.fecha_jubilacion, e.fecha_pension) - CURRENT_DATE)
                WHEN e.fecha_jubilacion IS NOT NULL
                     THEN (e.fecha_jubilacion - CURRENT_DATE)
                WHEN e.fecha_pension IS NOT NULL
                     THEN (e.fecha_pension - CURRENT_DATE)
                ELSE NULL
            END                                          AS dias_restantes
        FROM public.empleados e
        LEFT JOIN public.cargos            c  ON e.cargo_id        = c.id
        LEFT JOIN public.departamentos     d  ON e.departamento_id = d.id
        LEFT JOIN public.estados_laborales el ON e.estado_id       = el.id
        WHERE e.deleted_at IS NULL
            AND (
                (
                    e.fecha_jubilacion IS NOT NULL
                    AND e.fecha_jubilacion <= CURRENT_DATE + (%s || ' days')::INTERVAL
                )
                OR
                (
                    e.fecha_pension IS NOT NULL
                    AND e.fecha_pension <= CURRENT_DATE + (%s || ' days')::INTERVAL
                )
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


# BR-063: existía aquí un `GET /alertas/documentos_vencidos` que consultaba
# `public.datos_archivo` — retención documental del módulo Archivo, colgada
# bajo el prefijo `/api/rrhh` y protegida solo por `require_role("RRHH")`. Un
# usuario del módulo RRHH podía leer datos de Archivo por esta puerta trasera.
# Sin ningún consumidor en el frontend (`grep` de `documentos_vencidos` no
# encontró llamadas desde `app/static/`), se retira en vez de moverse: no hay
# nada que romper y así no queda la fuga viva mientras se decide su destino
# definitivo (`routes/admin/retention.py` o `archive.py`, ambos fuera de esta
# zona — ver `_BUZON.md`).


# =============================================================================
# HISTORIAL DE CARGOS (LOTTT Art. 147 – derecho al expediente)
# =============================================================================

class HistorialCargoIn(BaseModel):
    cargo_nombre: str
    fecha_inicio: date
    fecha_fin: Optional[date] = None
    motivo: Optional[str] = None
    # BR-064/OR-037/OR-044: se acepta por compatibilidad con clientes viejos
    # pero se ignora para la auditoría — el actor real sale de la sesión
    # verificada (`require_session`), nunca de lo que declare quien llama.
    registrado_por: Optional[str] = ""

    @field_validator("cargo_nombre")
    @classmethod
    def cargo_not_empty(cls, v):
        v = (v or "").strip()
        if not v:
            raise ValueError("cargo_nombre no puede estar vacío")
        return v

    @field_validator("fecha_fin")
    @classmethod
    def fin_after_inicio(cls, v, info):
        if v and "fecha_inicio" in info.data and v < info.data["fecha_inicio"]:
            raise ValueError("fecha_fin no puede ser anterior a fecha_inicio")
        return v


@router.get("/empleado/{empleado_id}/historial_cargos")
def get_position_history(empleado_id: int):
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
def add_position_history(
    empleado_id: int,
    data: HistorialCargoIn,
    usuario_sesion: str = Depends(require_session),
):
    """
    Registra un movimiento de cargo en el expediente del empleado.
    Si el cargo no existe en el catálogo, se crea automáticamente.

    BR-067/OR-038: cerrar el tramo anterior, insertar el nuevo y sincronizar
    `empleados.cargo_id` (OR-013, parcial) van en una sola `db_transaction()`
    — antes eran escrituras sueltas y si la segunda fallaba el empleado
    quedaba con el cargo anterior cerrado y ninguno abierto. El cierre resta
    un día a `fecha_fin` (OR-038): con `fecha_fin = fecha_inicio` del nuevo
    tramo, un mismo día contaba en los dos cargos a la vez.

    OR-155 (parcial, solo la mitad de servidor — la mitad de cliente en
    `admin-edit-hr.js` queda fuera de esta zona, ver `_BUZON.md`): se
    rechaza una fecha de inicio anterior al ingreso del empleado, una fecha
    futura, o una que solape con un tramo ya cerrado del historial. Antes
    solo se exigía que cargo y fecha no estuvieran vacíos, así que un dedo
    en el año (p. ej. 3016) quedaba como cargo "actual" del expediente.
    """
    emp = db_query(
        "SELECT id, fecha_ingreso FROM public.empleados WHERE id = %s",
        [empleado_id], fetch="one"
    )
    if not emp:
        raise HTTPException(status_code=404, detail="Empleado no encontrado")

    if data.fecha_inicio:
        if emp["fecha_ingreso"] and data.fecha_inicio < emp["fecha_ingreso"]:
            raise HTTPException(
                status_code=400,
                detail="fecha_inicio no puede ser anterior a la fecha de ingreso del empleado",
            )
        if data.fecha_inicio > date.today():
            raise HTTPException(
                status_code=400,
                detail="fecha_inicio no puede ser una fecha futura",
            )
        # No se marca como solape el tramo abierto que esta insercion va a
        # cerrar (fecha_inicio anterior, fecha_fin NULL): eso es la sucesion
        # normal de cargos, no un error. Sólo se rechaza una fecha_inicio
        # duplicada o que caiga dentro de un tramo ya cerrado.
        overlap = db_query("""
            SELECT id FROM public.historial_cargos
             WHERE empleado_id = %s
               AND (
                   fecha_inicio = %s
                   OR (fecha_fin IS NOT NULL AND %s BETWEEN fecha_inicio AND fecha_fin)
               )
             LIMIT 1
        """, [empleado_id, data.fecha_inicio, data.fecha_inicio], fetch="one")
        if overlap:
            raise HTTPException(
                status_code=400,
                detail="fecha_inicio solapa con un tramo existente del historial",
            )

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

    with db_transaction() as execute:
        # Cerrar el registro abierto anterior (si fecha_fin no está seteada),
        # el día antes de que empiece el nuevo para que no se solapen.
        if data.fecha_inicio:
            execute("""
                UPDATE public.historial_cargos
                   SET fecha_fin = %s - INTERVAL '1 day'
                 WHERE empleado_id = %s AND fecha_fin IS NULL AND fecha_inicio < %s
            """, [data.fecha_inicio, empleado_id, data.fecha_inicio])

        new_id = execute("""
            INSERT INTO public.historial_cargos
                (empleado_id, cargo_id, fecha_inicio, fecha_fin, motivo, registrado_por)
            VALUES (%s, %s, %s, %s, %s, %s)
            RETURNING id
        """, [
            empleado_id, cargo_id,
            data.fecha_inicio, data.fecha_fin,
            data.motivo, usuario_sesion,
        ], fetch="one")["id"]

        # OR-013 (parcial): si el tramo que se registra queda abierto, es el
        # cargo vigente — se refleja en `empleados.cargo_id` en la misma
        # transacción para que el dossier no muestre dos verdades distintas.
        # Sincronizar la edición manual del campo "Cargo" para que abra un
        # movimiento en vez de escribir la columna directo excede esta zona
        # (toca `admin/docs.py` y `admin-edit-hr.js`, ver `_BUZON.md`).
        if data.fecha_fin is None:
            execute(
                "UPDATE public.empleados SET cargo_id = %s WHERE id = %s",
                [cargo_id, empleado_id],
            )

    log_event(
        usuario_sesion,
        "Historial Cargo Registrado",
        "RRHH",
        f"empleado_id={empleado_id} cargo={data.cargo_nombre} desde={data.fecha_inicio}",
    )

    return {"success": True, "id": new_id, "cargo": data.cargo_nombre}


@router.delete("/empleado/{empleado_id}/historial_cargos/{historial_id}")
def delete_position_history(
    empleado_id: int,
    historial_id: int,
    usuario_sesion: str = Depends(require_admin_role("RRHH")),
):
    """Elimina una entrada del historial de cargos (solo para correcciones).

    BR-065 (parcial): el expediente laboral es del ámbito del Art. 147 LOTTT,
    así que este borrado ya exige `require_admin_role` en vez de bastar con
    pertenecer al módulo — antes cualquier sesión de RRHH podía hacerlo. El
    borrado sigue siendo físico: pasarlo a lógico (`deleted_at` + papelera)
    exige una migración en `app/main.py`, fuera de esta zona (ver `_BUZON.md`).

    OR-014: si el tramo borrado era el que cerraba al anterior, se reabre ese
    anterior (`fecha_fin = NULL`) en la misma transacción — si no, el
    empleado se queda sin ningún cargo marcado como vigente.
    """
    row = db_query(
        "SELECT empleado_id, fecha_inicio FROM public.historial_cargos WHERE id = %s AND empleado_id = %s",
        [historial_id, empleado_id], fetch="one"
    )
    if not row:
        raise HTTPException(status_code=404, detail="Registro de historial no encontrado")

    with db_transaction() as execute:
        execute("DELETE FROM public.historial_cargos WHERE id = %s", [historial_id])
        execute("""
            UPDATE public.historial_cargos
               SET fecha_fin = NULL
             WHERE id = (
                 SELECT id FROM public.historial_cargos
                  WHERE empleado_id = %s AND fecha_fin IS NOT NULL AND fecha_inicio < %s
                  ORDER BY fecha_inicio DESC
                  LIMIT 1
             )
        """, [empleado_id, row["fecha_inicio"]])

    log_event(usuario_sesion, "Historial Cargo Eliminado", "RRHH",
              f"historial_id={historial_id} empleado_id={empleado_id}")
    return {"success": True}
