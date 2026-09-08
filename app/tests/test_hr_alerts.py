"""Pruebas funcionales de `app/routes/hr_alerts.py` (candidato de la 3a ronda de
auditoria, RECORRIDO-visual-funcional-2).

`test_autorizacion_hr.py` ya cubre que estos endpoints exigen sesion + modulo
RRHH, pero ningun test ejercitaba la logica de negocio propia del archivo:

- OR-155: `add_position_history` rechaza `fecha_inicio` anterior al ingreso
  del empleado, futura, o que solape con un tramo ya cerrado del historial.
- BR-067/OR-038: al insertar un tramo abierto se cierra el tramo anterior
  (`fecha_fin = nueva.fecha_inicio - 1 dia`) y se sincroniza
  `empleados.cargo_id` en la misma transaccion.
- OR-014: `delete_position_history` reabre el tramo anterior si el que se
  borra era el que lo habia cerrado, para que el empleado no quede sin ningun
  cargo vigente.
- OR-022/OR-023/BR-069: `get_retirement_alerts` no recorta lo vencido con un
  piso de dias (a diferencia de una version anterior) y filtra
  `deleted_at IS NULL`.

Mismo patron que `test_hr.py`: `client_as` + mock de
`routes.admin.deps.db_query` para la fila de rol, y de `repos.hr_alerts_repo.db_query`
/ `routes.hr_alerts.db_transaction` para los datos de negocio.
"""
from datetime import date
from unittest.mock import MagicMock, patch


def _fila(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.get = lambda k, default=None: data.get(k, default)
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


ROL_RRHH_NORMAL = _fila(modulo="RRHH", rol="Normal", is_active=True)
ROL_RRHH_ADMIN = _fila(modulo="RRHH", rol="Admin", is_active=True)


def _emp_row(**kwargs):
    base = {"id": 1, "fecha_ingreso": date(2015, 6, 20)}
    base.update(kwargs)
    row = MagicMock()
    row.__getitem__ = lambda self, k: base[k]
    row.get = lambda k, default=None: base.get(k, default)
    row.keys = lambda: base.keys()
    row.__iter__ = lambda self: iter(base)
    return row


def _mock_transaction():
    """Simula `with db_transaction() as execute:` devolviendo una fila real
    con `["id"]` en el INSERT final, como espera `add_position_history`."""
    execute = MagicMock(side_effect=lambda *a, **k: {"id": 99})
    ctx = MagicMock()
    ctx.__enter__.return_value = execute
    ctx.__exit__.return_value = False
    return ctx, execute


# =============================================================================
# OR-155 — validacion de fecha_inicio al registrar un movimiento de cargo
# =============================================================================

class TestValidacionFechaInicio:

    def test_fecha_inicio_anterior_al_ingreso_se_rechaza(self, client_as):
        c = client_as("rrhh_admin")
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("repos.hr_alerts_repo.db_query", return_value=_emp_row()),
        ):
            res = c.post(
                "/api/rrhh/empleado/1/historial_cargos",
                json={"cargo_nombre": "Profesor", "fecha_inicio": "2010-01-01"},
            )
        assert res.status_code == 400
        assert "anterior a la fecha de ingreso" in res.text

    def test_fecha_inicio_futura_se_rechaza(self, client_as):
        c = client_as("rrhh_admin")
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("repos.hr_alerts_repo.db_query", return_value=_emp_row()),
        ):
            res = c.post(
                "/api/rrhh/empleado/1/historial_cargos",
                json={"cargo_nombre": "Profesor", "fecha_inicio": "3016-01-01"},
            )
        assert res.status_code == 400
        assert "fecha futura" in res.text

    def test_fecha_inicio_que_solapa_tramo_cerrado_se_rechaza(self, client_as):
        c = client_as("rrhh_admin")
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("repos.hr_alerts_repo.db_query") as mock_db,
        ):
            # 1a llamada: SELECT empleado (fecha_ingreso). 2a: overlap SELECT.
            mock_db.side_effect = [_emp_row(), _fila(id=5)]
            res = c.post(
                "/api/rrhh/empleado/1/historial_cargos",
                json={"cargo_nombre": "Profesor", "fecha_inicio": "2020-05-01"},
            )
        assert res.status_code == 400
        assert "solapa" in res.text

    def test_empleado_inexistente_da_404(self, client_as):
        c = client_as("rrhh_admin")
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("repos.hr_alerts_repo.db_query", return_value=None),
        ):
            res = c.post(
                "/api/rrhh/empleado/999/historial_cargos",
                json={"cargo_nombre": "Profesor", "fecha_inicio": "2020-05-01"},
            )
        assert res.status_code == 404


# =============================================================================
# BR-067/OR-038/OR-013 — cierre del tramo anterior + sincronizacion de cargo_id
# =============================================================================

class TestRegistroDeCargoExitoso:

    def test_movimiento_abierto_cierra_el_tramo_anterior_y_sincroniza_cargo(self, client_as):
        c = client_as("rrhh_admin")
        ctx, execute = _mock_transaction()
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("repos.hr_alerts_repo.db_query") as mock_db,
            patch("routes.hr_alerts.db_transaction", return_value=ctx),
            patch("routes.hr_alerts.log_event"),
        ):
            # SELECT empleado, SELECT overlap (ninguno), SELECT cargo (no existe), INSERT cargo
            mock_db.side_effect = [
                _emp_row(),          # empleado
                None,                # sin solape
                None,                # cargo no existe en catalogo
                {"id": 7},           # INSERT cargo -> id
            ]
            res = c.post(
                "/api/rrhh/empleado/1/historial_cargos",
                json={"cargo_nombre": "Profesor Titular", "fecha_inicio": "2020-05-01"},
            )
        assert res.status_code == 200
        assert res.json()["success"] is True

        sqls = [call.args[0] for call in execute.call_args_list]
        # Se cierra el tramo abierto anterior antes de insertar el nuevo.
        assert any("SET fecha_fin" in s and "fecha_fin IS NULL" in s for s in sqls)
        # Al quedar el nuevo tramo abierto (sin fecha_fin), se sincroniza
        # empleados.cargo_id en la misma transaccion (OR-013).
        assert any("UPDATE public.empleados SET cargo_id" in s for s in sqls)

    def test_movimiento_con_fecha_fin_no_sincroniza_cargo_actual(self, client_as):
        """Un tramo que ya nace cerrado (fecha_fin != None) no es el cargo
        vigente, así que no debe tocar `empleados.cargo_id`."""
        c = client_as("rrhh_admin")
        ctx, execute = _mock_transaction()
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("repos.hr_alerts_repo.db_query") as mock_db,
            patch("routes.hr_alerts.db_transaction", return_value=ctx),
            patch("routes.hr_alerts.log_event"),
        ):
            mock_db.side_effect = [_emp_row(), None, _fila(id=3)]
            res = c.post(
                "/api/rrhh/empleado/1/historial_cargos",
                json={
                    "cargo_nombre": "Profesor",
                    "fecha_inicio": "2018-01-01",
                    "fecha_fin": "2019-01-01",
                },
            )
        assert res.status_code == 200
        sqls = [call.args[0] for call in execute.call_args_list]
        assert not any("UPDATE public.empleados SET cargo_id" in s for s in sqls)


# =============================================================================
# OR-014 — borrar un tramo reabre el que cerró, si lo había
# =============================================================================

class TestBorrarHistorialReabreElAnterior:

    def test_borrar_tramo_reabre_el_tramo_previo(self, client_as):
        c = client_as("rrhh_admin")
        ctx, execute = _mock_transaction()
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_ADMIN),
            patch(
                "repos.hr_alerts_repo.db_query",
                return_value=_fila(empleado_id=1, fecha_inicio="2020-05-01"),
            ),
            patch("routes.hr_alerts.db_transaction", return_value=ctx),
            patch("routes.hr_alerts.log_event"),
        ):
            res = c.delete("/api/rrhh/empleado/1/historial_cargos/10")
        assert res.status_code == 200

        sqls = [call.args[0] for call in execute.call_args_list]
        assert any(s.strip().startswith("DELETE FROM public.historial_cargos") for s in sqls)
        # La segunda sentencia reabre el tramo cerrado inmediatamente anterior.
        assert any("SET fecha_fin = NULL" in s for s in sqls)

    def test_borrar_historial_inexistente_da_404(self, client_as):
        c = client_as("rrhh_admin")
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_ADMIN),
            patch("repos.hr_alerts_repo.db_query", return_value=None),
        ):
            res = c.delete("/api/rrhh/empleado/1/historial_cargos/999")
        assert res.status_code == 404

    def test_borrar_historial_exige_admin_no_basta_normal(self, client_as):
        """BR-065: a diferencia de crear un movimiento, borrarlo exige
        `require_admin_role`, no solo pertenecer al modulo RRHH."""
        c = client_as("rrhh_normal")
        with patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL):
            res = c.delete("/api/rrhh/empleado/1/historial_cargos/10")
        assert res.status_code == 403


# =============================================================================
# OR-022/OR-023/BR-069 — alertas de jubilacion/pension
# =============================================================================

class TestAlertasJubilacion:

    def test_filtra_empleados_en_papelera(self, client_as):
        c = client_as("rrhh_normal")
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("repos.hr_alerts_repo.db_query", return_value=[]) as mock_db,
        ):
            res = c.get("/api/rrhh/alertas/jubilaciones")
        assert res.status_code == 200
        sql = mock_db.call_args.args[0]
        assert "e.deleted_at IS NULL" in sql

    def test_no_recorta_lo_vencido_con_piso_de_dias(self, client_as):
        """OR-022: una jubilación vencida hace meses, sin procesar, sigue
        cayendo dentro del WHERE — no hay corte inferior de 30 días."""
        c = client_as("rrhh_normal")
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("repos.hr_alerts_repo.db_query", return_value=[]) as mock_db,
        ):
            c.get("/api/rrhh/alertas/jubilaciones")
        sql = mock_db.call_args.args[0]
        # No debe existir ningún BETWEEN de piso inferior en el WHERE final.
        where_clause = sql.split("WHERE e.deleted_at")[1]
        assert "BETWEEN CURRENT_DATE" not in where_clause

    def test_respuesta_trae_total_y_horizonte(self, client_as):
        c = client_as("rrhh_normal")
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("repos.hr_alerts_repo.db_query", return_value=[_fila(empleado_id=1)]),
        ):
            res = c.get("/api/rrhh/alertas/jubilaciones?horizonte_dias=90")
        body = res.json()
        assert body["horizonte_dias"] == 90
        assert body["total"] == 1
