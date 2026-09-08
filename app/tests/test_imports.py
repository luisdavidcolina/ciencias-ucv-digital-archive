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


class TestPrefetchCedulasFallaCaeAConsultaPorFila:
    """IN-217 paso 2 (ronda74): `import_empleados_csv` resuelve todas las
    cédulas del CSV de una vez con `WHERE cedula = ANY(%s)` antes del bucle.
    Si esa consulta en lote falla (`except Exception` alrededor del
    prefetch), el código debe caer de vuelta al `SELECT` por fila de
    siempre, no perder filas ni reventar con un 500 — pero ninguna prueba
    forzaba jamás esa rama desde que se escribió."""

    def test_prefetch_en_lote_falla_pero_la_fila_se_procesa_por_separado(self, client):
        calls = []

        def _db_query_side_effect(sql, params=None, fetch=None, commit=False):
            if "cedula = ANY(%s)" in sql:
                raise Exception("boom: fallo simulado del prefetch en lote")
            if "FROM public.empleados WHERE cedula=%s" in sql:
                return None  # la fila cae al SELECT individual: cédula nueva
            return None

        csv_body = (
            "cedula,nombres,apellidos,cargo,departamento,estado\n"
            "V-44444444,Maria,Rodriguez,Analista,Compras,Activo\n"
        )
        with patch("routes.admin.imports.db_query", side_effect=_db_query_side_effect), \
             patch("routes.admin.imports._resolve_or_create_lookup", return_value=7), \
             patch("routes.admin.imports.db_transaction", _fake_db_transaction(calls)):
            res = _post_csv(client, "/api/admin/import/empleados", "empleados.csv", csv_body)

        assert res.status_code == 200
        body = res.json()
        # El fallo del prefetch no debe aparecer como error de fila ni tumbar
        # la importación: la fila se resuelve igual, sólo que por SELECT
        # individual en vez del lote.
        assert body["errors"] == [], body
        assert body["inserted"] == 1, body

        insert_calls = [c for c in calls if "INSERT INTO public.empleados" in c[0]]
        assert len(insert_calls) == 1


class TestPrefetchEmpleadosDocumentosRRHH:
    """IN-217 paso 3 (ronda88): la rama RRHH de `import_documentos_csv`
    resolvía `emp` con un SELECT por fila (`WHERE cedula=%s`), exactamente
    el mismo patrón que el paso 2 ya optimizó en `import_empleados_csv`. Se
    replica aquí: prefetch en lote con `cedula = ANY(%s)` antes del bucle,
    con la misma rama de repliegue si el lote falla. Es lectura pura (no
    escribe, no decide nada condicional) así que no toca el aislamiento de
    errores por fila que el resto de la función sigue necesitando."""

    def test_csv_grande_con_duplicados_y_una_cedula_inexistente(self, client):
        """CSV de 60 filas: cédulas repetidas (deben reutilizar el prefetch,
        no repetir consulta) y una cédula que no existe (debe reportarse
        como error de esa fila sin afectar a las demás — aislamiento de
        errores por fila intacto)."""
        calls = []

        def _db_query_side_effect(sql, params=None, fetch=None, commit=False):
            if "cedula = ANY(%s)" in sql:
                calls.append(("PREFETCH", params))
                cedulas = params[0]
                return [
                    {"id": idx + 1, "cedula": c}
                    for idx, c in enumerate(cedulas) if c != "V-99999999"
                ]
            if "INSERT INTO public.datos_rrhh" in sql:
                calls.append(("INSERT", params))
                return None
            return None

        filas_csv = ["cedula_empleado,tipo_documento"]
        # 58 filas válidas repartidas entre 5 cédulas distintas (duplicados
        # reales dentro del propio CSV) + 2 filas con la cédula inexistente.
        cedulas_validas = ["V-10000001", "V-10000002", "V-10000003", "V-10000004", "V-10000005"]
        for i in range(58):
            filas_csv.append(f"{cedulas_validas[i % 5]},Contrato")
        filas_csv.append("V-99999999,Contrato")
        filas_csv.append("V-99999999,Constancia")
        csv_body = "\n".join(filas_csv) + "\n"

        with patch("routes.admin.imports.db_query", side_effect=_db_query_side_effect), \
             patch("routes.admin.imports._resolve_or_create_tipo_documento", return_value=9), \
             patch("routes.admin.imports._resolve_user_id", return_value=1):
            res = _post_csv(
                client, "/api/admin/import/documentos", "docs_grande.csv", csv_body, modulo="RRHH",
            )

        assert res.status_code == 200
        body = res.json()
        assert body["inserted"] == 58, body
        assert len(body["errors"]) == 2, body
        assert all("V-99999999" in e for e in body["errors"])

        prefetch_calls = [c for c in calls if c[0] == "PREFETCH"]
        assert len(prefetch_calls) == 1, "el prefetch debe correr una sola vez, no por fila"
        assert sorted(prefetch_calls[0][1][0]) == sorted(cedulas_validas + ["V-99999999"])

        insert_calls = [c for c in calls if c[0] == "INSERT"]
        assert len(insert_calls) == 58

    def test_prefetch_de_empleados_falla_cae_a_consulta_por_fila(self, client):
        """Igual que TestPrefetchCedulasFallaCaeAConsultaPorFila pero para el
        prefetch de empleados de esta función: si el lote falla, cada fila
        cae de vuelta a su SELECT individual y la importación no se cae."""
        def _db_query_side_effect(sql, params=None, fetch=None, commit=False):
            if "cedula = ANY(%s)" in sql:
                raise Exception("boom: fallo simulado del prefetch en lote")
            if "FROM public.empleados WHERE cedula=%s" in sql:
                return _fila(id=1)
            return None

        csv_body = "cedula_empleado,tipo_documento\nV-11111111,Contrato\n"
        with patch("routes.admin.imports.db_query", side_effect=_db_query_side_effect), \
             patch("routes.admin.imports._resolve_or_create_tipo_documento", return_value=9), \
             patch("routes.admin.imports._resolve_user_id", return_value=1):
            res = _post_csv(
                client, "/api/admin/import/documentos", "docs.csv", csv_body, modulo="RRHH",
            )

        assert res.status_code == 200
        body = res.json()
        assert body["errors"] == [], body
        assert body["inserted"] == 1, body


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


class TestIN133IgnoraRequesterDeclaradoPorElCliente:
    """IN-133: la identidad que queda en auditoría (`log_event`) y en
    `creado_por`/`updated_by` debe salir de la sesión verificada por
    `require_session`, nunca de un parámetro que declare quien llama —
    antes ambos endpoints aceptaban `requester` en la query string y lo
    usaban tal cual, permitiendo a cualquiera atribuir su importación a
    otro usuario."""

    def test_import_empleados_usa_la_sesion_no_el_query_string(self, client_as):
        c = client_as("empleado_real")
        csv_body = "cedula,nombres,apellidos\nV-11111111,Ana,Perez\n"
        with patch("routes.admin.imports.log_event") as mock_log:
            res = c.post(
                "/api/admin/import/empleados",
                files={"file": ("empleados.csv", io.BytesIO(csv_body.encode("utf-8")), "text/csv")},
                params={"requester": "otro_usuario_falsificado"},
            )
        assert res.status_code == 200
        assert mock_log.call_args.args[0] == "empleado_real"

    def test_import_documentos_usa_la_sesion_no_el_query_string(self, client_as):
        c = client_as("empleado_real")

        def _db_query_side_effect(sql, params=None, fetch=None, commit=False):
            if "FROM public.empleados" in sql:
                return _fila(id=1)
            return None

        with patch("routes.admin.imports.db_query", side_effect=_db_query_side_effect), \
             patch("routes.admin.imports._resolve_or_create_tipo_documento", return_value=9), \
             patch("routes.admin.imports._resolve_user_id", return_value=1) as mock_uid, \
             patch("routes.admin.imports.log_event") as mock_log:
            csv_body = "cedula_empleado,tipo_documento\nV-11111111,Contrato\n"
            res = c.post(
                "/api/admin/import/documentos",
                files={"file": ("docs.csv", io.BytesIO(csv_body.encode("utf-8")), "text/csv")},
                params={"modulo": "RRHH", "requester": "otro_usuario_falsificado"},
            )
        assert res.status_code == 200
        mock_uid.assert_called_with("empleado_real")
        assert mock_log.call_args.args[0] == "empleado_real"
