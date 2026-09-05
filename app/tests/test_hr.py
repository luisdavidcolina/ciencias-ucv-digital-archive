"""Pruebas funcionales del módulo RRHH (BR-057).

`hr.py` es el único router sin `test_hr.py` propio pese a manejar los datos
más sensibles del sistema (cédula, RIF, fecha de nacimiento). Cubre lo que
`test_autorizacion_hr.py` no cubre: escapado del reporte imprimible (BR-003),
que el reporte y el dossier no muestren documentos en papelera (BR-015,
BR-005), y el 404 del perfil.

Usa el mismo patrón que `test_autorizacion_hr.py`: `client_as` + mock de
`routes.admin.deps.db_query` para la fila de rol, y de `routes.hr.db_query`
para los datos de negocio.
"""
from unittest.mock import MagicMock, patch


def _fila(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.get = lambda k, default=None: data.get(k, default)
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


ROL_RRHH_NORMAL = _fila(modulo="RRHH", rol="Normal", is_active=True)


def _emp_row(**kwargs):
    base = {
        "id": 1, "cedula": "V-12345678", "nombres": "Carlos", "apellidos": "Gómez",
        "rif": "J-12345678-0", "fecha_jubilacion": None, "fecha_pension": None,
        "fecha_nacimiento": None, "nivel_educativo": "Universitario", "sexo": "M",
        "fecha_ingreso": "2015-06-20", "cargo": "Director", "departamento": "Decanato",
        "estado": "Activo",
    }
    base.update(kwargs)
    row = MagicMock()
    row.__getitem__ = lambda self, k: base[k]
    row.get = lambda k, default=None: base.get(k, default)
    row.keys = lambda: base.keys()
    row.__iter__ = lambda self: iter(base)
    return row


def _doc_row(**kwargs):
    base = {
        "id_rrhh": 1, "fecha_documento": "2024-01-10", "notas": "Sin novedad",
        "ubicacion": "Archivo Central", "file_url": "",
        "tipo_nombre": "Contrato", "parte_nombre": "Parte I", "parte_orden": 1,
    }
    base.update(kwargs)
    row = MagicMock()
    row.__getitem__ = lambda self, k: base[k]
    row.get = lambda k, default=None: base.get(k, default)
    row.keys = lambda: base.keys()
    row.__iter__ = lambda self: iter(base)
    return row


# =============================================================================
# BR-003 — el reporte imprimible escapa todo lo que viene de la base
# =============================================================================

class TestReporteEscapaHtml:

    def test_notas_con_script_no_sale_literal(self, client_as):
        c = client_as("rrhh_normal")
        payload_xss = '<img src=x onerror="fetch(1)">'
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("routes.hr.db_query") as mock_db,
        ):
            mock_db.side_effect = [
                _emp_row(),
                [_doc_row(notas=payload_xss)],
                [],  # historial_cargos
            ]
            res = c.get("/api/rrhh/report/1")
        assert res.status_code == 200
        assert payload_xss not in res.text
        assert "&lt;img" in res.text

    def test_estado_con_comillas_no_rompe_el_atributo_class(self, client_as):
        c = client_as("rrhh_normal")
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("routes.hr.db_query") as mock_db,
        ):
            mock_db.side_effect = [
                _emp_row(estado='Activo"><script>alert(1)</script>'),
                [],
                [],
            ]
            res = c.get("/api/rrhh/report/1")
        assert res.status_code == 200
        assert "<script>alert(1)</script>" not in res.text


# =============================================================================
# BR-015 / BR-005 — documentos y personal en papelera no salen en RRHH
# =============================================================================

class TestBorradoLogicoSeRespeta:

    def test_reporte_filtra_documentos_borrados(self, client_as):
        c = client_as("rrhh_normal")
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("routes.hr.db_query") as mock_db,
        ):
            mock_db.side_effect = [_emp_row(), [], []]
            c.get("/api/rrhh/report/1")
            docs_call = mock_db.call_args_list[1]
        assert "deleted_at IS NULL" in docs_call.args[0]

    def test_dossier_filtra_empleados_y_documentos_borrados(self, client_as):
        c = client_as("rrhh_normal")
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("routes.hr.db_query", return_value=[]) as mock_db,
        ):
            res = c.post("/api/rrhh/person/profile", json={"persona": "Carlos Gómez"})
        assert res.status_code == 404
        sql = mock_db.call_args.args[0]
        assert "e.deleted_at IS NULL" in sql
        assert "dr.deleted_at IS NULL" in sql


# =============================================================================
# 404 del perfil (persona no encontrada en expedientes)
# =============================================================================

class TestPerfilNoEncontrado:

    def test_persona_inexistente_da_404(self, client_as):
        c = client_as("rrhh_normal")
        with (
            patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL),
            patch("routes.hr.db_query", return_value=[]),
        ):
            res = c.post("/api/rrhh/person/profile", json={"persona": "Nadie Existe"})
        assert res.status_code == 404
