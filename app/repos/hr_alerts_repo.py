"""Consultas SQL para `routes/hr_alerts.py` (alertas RRHH + historial de cargos).

IN-042 paso 4: funciones de consulta movidas tal cual desde `routes/hr_alerts.py`,
sin cambiar SQL ni comportamiento. `routes/hr_alerts.py` las importa y las usa;
`db_query` sigue siendo el único ejecutor, ahora invocado desde este módulo.

Los bloques `db_transaction()` (escrituras multi-sentencia con lógica
condicional en `add_position_history`/`delete_position_history`) se quedan en
`routes/hr_alerts.py`: mover el callback `execute` fuera del `with` cambiaría
la forma del código, no solo su ubicación.
"""

from database import db_query


def alertas_jubilacion_rows(horizonte_dias: int) -> list[dict]:
    return db_query("""
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


def empleado_por_id(empleado_id: int) -> dict | None:
    """Fila `{id}` de `empleados`, o None. Usada por `get_position_history`."""
    return db_query("SELECT id FROM public.empleados WHERE id = %s", [empleado_id], fetch="one")


def historial_cargos_rows(empleado_id: int) -> list[dict]:
    return db_query("""
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


def empleado_con_ingreso(empleado_id: int) -> dict | None:
    """Fila `{id, fecha_ingreso}` de `empleados`, o None. Usada por `add_position_history`."""
    return db_query(
        "SELECT id, fecha_ingreso FROM public.empleados WHERE id = %s",
        [empleado_id], fetch="one",
    )


def historial_solapado(empleado_id: int, fecha_inicio) -> dict | None:
    return db_query("""
        SELECT id FROM public.historial_cargos
         WHERE empleado_id = %s
           AND (
               fecha_inicio = %s
               OR (fecha_fin IS NOT NULL AND %s BETWEEN fecha_inicio AND fecha_fin)
           )
         LIMIT 1
    """, [empleado_id, fecha_inicio, fecha_inicio], fetch="one")


def cargo_por_nombre(cargo_nombre: str) -> dict | None:
    return db_query(
        "SELECT id FROM public.cargos WHERE LOWER(nombre) = LOWER(%s)",
        [cargo_nombre], fetch="one",
    )


def crear_cargo(cargo_nombre: str) -> dict:
    return db_query(
        "INSERT INTO public.cargos (nombre) VALUES (%s) RETURNING id",
        [cargo_nombre], fetch="one", commit=True,
    )


def historial_por_id(historial_id: int, empleado_id: int) -> dict | None:
    return db_query(
        "SELECT empleado_id, fecha_inicio FROM public.historial_cargos WHERE id = %s AND empleado_id = %s",
        [historial_id, empleado_id], fetch="one",
    )
