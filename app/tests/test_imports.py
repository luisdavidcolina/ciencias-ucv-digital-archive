"""Pruebas de `app/routes/admin/imports.py` (carril C5-importaciones).

Reproduce los tres hallazgos más graves del barrido de RRHH:

- OR-002: `POST /api/admin/import/empleados` nunca insertaba una fila porque
  el INSERT omitía `empleados.fecha_ingreso` (DATE NOT NULL sin DEFAULT).
  Cada fila lanzaba NotNullViolation, el bucle la capturaba, y la respuesta
  era HTTP 200 con `inserted: 0` disfrazado de éxito.
- OR-003: la rama RRHH de `POST /api/admin/import/documentos` nunca
  insertaba una fila por la misma razón, con `datos_rrhh.titulo` (TEXT NOT
  NULL).
- OR-004: cuando la cédula ya existía, el UPDATE fijaba SIEMPRE `nombres`,
  `apellidos`, `rif`, `fecha_jubilacion` y `fecha_pension`, así vinieran
  vacíos en el CSV o no — subir `cedula,departamento` de una persona le
  borraba el nombre.

Los tests mockean `db_query`, `db_transaction` y los resolvers de catálogo:
no hay conexión real a la base.
"""
import io
from contextlib import contextmanager
from unittest.mock import patch, MagicMock


def _fila(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.get = lambda k, default=None: data.get(k, default)
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


def _fake_db_transaction(calls):
    """Sustituto de `database.db_transaction`: registra cada `execute()` en
    `calls` y no toca ninguna base real. No simula fallos: eso lo cubre
    `test_database.py` en el carril O2, dueño de `db_transaction` en sí."""

    @contextmanager
    def _fake(*a, **kw):
        def execute(sql, params=None, fetch="none"):
            calls.append((sql, params or []))
            return None
        yield execute

    return _fake


def _post_csv(client, path, filename, content, **params):
    return client.post(
        path,
        files={"file": (filename, io.BytesIO(content.encode("utf-8")), "text/csv")},
        params=params,
    )


class TestImportEmpleadosInsertaFilas:
    """OR-002: el CSV mínimo documentado por la propia barra debe insertar,
    no reportar 0 filas disfrazadas de éxito."""

    def test_csv_minimo_sin_fecha_ingreso_inserta_y_avisa_el_valor_por_defecto(self, client):
        calls = []
        csv_body = (
            "cedula,nombres,apellidos,cargo,departamento,estado\n"
            "V-11111111,Juan,Perez,Analista,Compras,Activo\n"
        )
        with patch("routes.admin.imports.db_query", return_value=None), \
             patch("routes.admin.imports._resolve_or_create_lookup", return_value=7), \
             patch("routes.admin.imports.db_transaction", _fake_db_transaction(calls)):
            res = _post_csv(client, "/api/admin/import/empleados", "empleados.csv", csv_body)

        assert res.status_code == 200
        body = res.json()
        assert body["inserted"] == 1, body
        assert body["errors"] == []
        assert body["success"] is True
        assert "V-11111111" in body["fecha_ingreso_por_defecto"]

        # El INSERT real debe declarar fecha_ingreso con un valor no nulo.
        insert_calls = [c for c in calls if "INSERT INTO public.empleados" in c[0]]
        assert len(insert_calls) == 1
        sql, params = insert_calls[0]
        assert "fecha_ingreso" in sql
        cols = sql[sql.index("(") + 1: sql.index(")")].replace("\n", " ")
        cols = [c.strip() for c in cols.split(",")]
        idx = cols.index("fecha_ingreso")
        assert params[idx] is not None

    def test_csv_con_fecha_ingreso_no_queda_listado_como_valor_por_defecto(self, client):
        calls = []
        csv_body = (
            "cedula,nombres,apellidos,fecha_ingreso\n"
            "V-22222222,Ana,Gomez,2020-05-01\n"
        )
        with patch("routes.admin.imports.db_query", return_value=None), \
             patch("routes.admin.imports._resolve_or_create_lookup", return_value=7), \
             patch("routes.admin.imports.db_transaction", _fake_db_transaction(calls)):
            res = _post_csv(client, "/api/admin/import/empleados", "empleados.csv", csv_body)

        body = res.json()
        assert body["inserted"] == 1
        assert body["fecha_ingreso_por_defecto"] == []


class TestImportDocumentosRRHHInsertaFilas:
    """OR-003: la rama RRHH del importador de documentos debe insertar, con
    un `titulo` no nulo aunque el CSV no lo traiga."""

    def test_csv_sin_titulo_deriva_uno_e_inserta(self, client):
        def _db_query_side_effect(sql, params=None, fetch=None, commit=False):
            if "FROM public.empleados" in sql:
                return _fila(id=1)
            return None

        with patch("routes.admin.imports.db_query", side_effect=_db_query_side_effect) as mock_dq, \
             patch("routes.admin.imports._resolve_or_create_tipo_documento", return_value=9), \
             patch("routes.admin.imports._resolve_user_id", return_value=1):
            csv_body = "cedula_empleado,tipo_documento\nV-11111111,Contrato\n"
            res = _post_csv(
                client, "/api/admin/import/documentos", "docs.csv", csv_body, modulo="RRHH",
            )

        assert res.status_code == 200
        body = res.json()
        assert body["inserted"] == 1, body
        assert body["errors"] == []
        assert body["success"] is True
        assert body["titulo_por_defecto"], "debe avisar que el titulo se derivo"

        insert_calls = [
            c for c in mock_dq.call_args_list
            if c.args and "INSERT INTO public.datos_rrhh" in c.args[0]
        ]
        assert len(insert_calls) == 1
        sql, params = insert_calls[0].args[0], insert_calls[0].args[1]
        cols = sql[sql.index("(") + 1: sql.index(")")].replace("\n", " ")
        cols = [c.strip() for c in cols.split(",")]
        idx = cols.index("titulo")
        assert params[idx]  # no vacío / no None


class TestImportEmpleadosActualizacionParcial:
    """OR-004: subir `cedula,departamento` de alguien que ya existe no debe
    borrarle nombres, apellidos, rif ni fechas de jubilación/pensión."""

    def test_csv_de_dos_columnas_no_toca_nombre_ni_apellido_ni_rif(self, client):
        calls = []
        csv_body = "cedula,departamento\nV-33333333,Nomina\n"
        with patch("routes.admin.imports.db_query", return_value=_fila(id=5)), \
             patch("routes.admin.imports._resolve_or_create_lookup", return_value=42), \
             patch("routes.admin.imports.db_transaction", _fake_db_transaction(calls)):
            res = _post_csv(client, "/api/admin/import/empleados", "empleados.csv", csv_body)

        assert res.status_code == 200
        body = res.json()
        assert body["updated"] == 1, body
        assert body["errors"] == []

        update_calls = [c for c in calls if "UPDATE public.empleados" in c[0]]
        assert len(update_calls) == 1
        sql, params = update_calls[0]
        for columna_intocable in ("nombres=", "apellidos=", "rif=", "fecha_jubilacion=", "fecha_pension="):
            assert columna_intocable not in sql, f"OR-004: {columna_intocable} no debe tocarse sin dato en el CSV"
        assert "departamento_id=" in sql
        # Ningún valor NULL/None viaja en la actualización parcial.
        assert all(p is not None for p in params)

    def test_csv_completo_si_actualiza_nombres_y_apellidos(self, client):
        """Control: cuando el CSV SÍ trae nombres/apellidos, deben actualizarse
        (la corrección de OR-004 es condicional, no una prohibición total)."""
        calls = []
        csv_body = "cedula,nombres,apellidos\nV-33333333,Nuevo,Nombre\n"
        with patch("routes.admin.imports.db_query", return_value=_fila(id=5)), \
             patch("routes.admin.imports._resolve_or_create_lookup", return_value=42), \
             patch("routes.admin.imports.db_transaction", _fake_db_transaction(calls)):
            res = _post_csv(client, "/api/admin/import/empleados", "empleados.csv", csv_body)

        assert res.json()["updated"] == 1
        update_calls = [c for c in calls if "UPDATE public.empleados" in c[0]]
        sql, params = update_calls[0]
        assert "nombres=" in sql and "apellidos=" in sql
        assert "Nuevo" in params and "Nombre" in params


class TestReporteHonesto:
    """El mensaje de éxito sólo debe sonar a éxito si de verdad insertó o
    actualizó algo (causa raíz de que '0 insertados' pareciera un éxito)."""

    def test_csv_solo_con_filas_invalidas_no_reporta_exito(self, client):
        csv_body = "cedula,nombres\n,SinCedula\n"
        res = _post_csv(client, "/api/admin/import/empleados", "empleados.csv", csv_body)
        body = res.json()
        assert body["inserted"] == 0
        assert body["updated"] == 0
        assert body["success"] is False
        assert "sin cambios" in body["message"].lower()
