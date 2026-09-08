"""Tests para el módulo de backup: sanitización de columnas y exportación."""
import json
import pytest
from unittest.mock import patch, MagicMock


def _mock_row(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    row.items = lambda: data.items()
    return row


# IN-131/SI-002: desde que /api/admin/backup/{export,restore} exigen
# require_role("Global"), el fixture `client` (usuario "test_user" sin fila
# real en usuarios_sistema) necesita que `routes.admin.deps.db_query` devuelva
# una fila de administrador Global para que las pruebas de sanitización de
# columnas -que no son sobre autorización- sigan pudiendo ejercitar el
# endpoint. La autorización en sí (403 sin ese módulo) se prueba aparte en
# app/tests/test_autorizacion_deps.py.
# Diccionario simple (no MagicMock): `require_role` llama a `.get()` sobre la
# fila y un MagicMock sin `.get` explícito devolvería otro Mock "truthy" en
# vez del valor real, dejando pasar la comparación de módulo por accidente.
_FILA_ADMIN_GLOBAL = {"modulo": "Global", "rol": "Admin", "is_active": True}


class _FakeTransaction:
    """Sustituto de `db_transaction()` que registra el SQL ejecutado y deja
    elegir, por SQL, si una sentencia debe fallar — para probar que el
    rollback deshace la tabla entera (IN-022) sin necesitar una base real.
    """

    def __init__(self, call_log, fail_when=None):
        self.call_log = call_log
        self.fail_when = fail_when or (lambda sql: False)
        self.rolled_back = False

    def __enter__(self):
        def execute(sql, params=None, fetch="none"):
            self.call_log.append(sql)
            if self.fail_when(sql):
                raise Exception("fallo simulado")
            return None
        return execute

    def __exit__(self, exc_type, exc, tb):
        if exc_type is not None:
            self.rolled_back = True
        return False  # nunca traga la excepción, igual que db_transaction real


class TestColumnSanitization:
    """Verifica que restore rechaza nombres de columna con caracteres peligrosos."""

    def test_columna_valida_insertada(self, client):
        backup = {
            "_metadata": {"version": "1.0", "tables": ["categoria"], "partial": False},
            "categoria": [{"id": 99, "nombre": "Test", "slug": "test"}],
        }
        content = json.dumps(backup).encode()

        call_log = []
        tx = _FakeTransaction(call_log)

        with patch("routes.backup.db_transaction", return_value=tx), \
             patch("routes.backup.db_query", return_value=None), \
             patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            import io
            res = client.post(
                "/api/admin/backup/restore?mode=merge&requester=admin",
                files={"file": ("backup.json", io.BytesIO(content), "application/json")},
            )

        assert res.status_code == 200
        inserts = [s for s in call_log if "INSERT" in s]
        assert any("id" in s and "nombre" in s and "slug" in s for s in inserts)
        body = res.json()
        assert body["results"]["categoria"]["inserted"] == 1

    def test_columna_con_inyeccion_descartada(self, client):
        malicious_col = "id); DROP TABLE empleados;--"
        backup = {
            "_metadata": {"version": "1.0", "tables": ["categoria"], "partial": False},
            "categoria": [{malicious_col: 1, "nombre": "Injected"}],
        }
        content = json.dumps(backup).encode()

        call_log = []
        tx = _FakeTransaction(call_log)

        with patch("routes.backup.db_transaction", return_value=tx), \
             patch("routes.backup.db_query", return_value=None), \
             patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            import io
            res = client.post(
                "/api/admin/backup/restore?mode=merge&requester=admin",
                files={"file": ("backup.json", io.BytesIO(content), "application/json")},
            )

        assert res.status_code == 200
        inserted_sqls = [s for s in call_log if "INSERT" in s]
        for sql in inserted_sqls:
            assert "DROP" not in sql
            assert malicious_col not in sql

    def test_fila_sin_columnas_validas_se_omite(self, client):
        backup = {
            "_metadata": {"version": "1.0", "tables": ["categoria"], "partial": False},
            "categoria": [{"123badname": "x", "spaces here": "y"}],
        }
        content = json.dumps(backup).encode()

        call_log = []
        tx = _FakeTransaction(call_log)

        with patch("routes.backup.db_transaction", return_value=tx), \
             patch("routes.backup.db_query", return_value=None), \
             patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            import io
            res = client.post(
                "/api/admin/backup/restore?mode=merge&requester=admin",
                files={"file": ("backup.json", io.BytesIO(content), "application/json")},
            )

        assert res.status_code == 200
        data_inserts = [s for s in call_log if "INSERT" in s]
        assert len(data_inserts) == 0

    def test_json_invalido_retorna_400(self, client):
        import io
        with patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            res = client.post(
                "/api/admin/backup/restore?mode=merge",
                files={"file": ("bad.json", io.BytesIO(b"not json at all"), "application/json")},
            )
        assert res.status_code == 400

    def test_sin_metadata_retorna_400(self, client):
        import io
        content = json.dumps({"categoria": []}).encode()
        with patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            res = client.post(
                "/api/admin/backup/restore?mode=merge",
                files={"file": ("bad.json", io.BytesIO(content), "application/json")},
            )
        assert res.status_code == 400

    def test_mode_invalido_retorna_400(self, client):
        import io
        with patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            res = client.post(
                "/api/admin/backup/restore?mode=drop_all",
                files={"file": ("x.json", io.BytesIO(b"{}"), "application/json")},
            )
        assert res.status_code == 400


class TestRestoreValidacionYTransaccion:
    """IN-021/IN-022/DG-149: pruebas nuevas de este carril (SI-233 pide más
    cobertura para backup, y aquí se cubre lo que se tocó)."""

    def test_tabla_con_forma_invalida_retorna_400(self, client):
        # `categoria` como string en vez de lista de filas: no debe llegar a
        # construir SQL con lo que traiga dentro.
        backup = {
            "_metadata": {"version": "1.1", "tables": ["categoria"], "partial": False},
            "categoria": "no soy una lista de filas",
        }
        content = json.dumps(backup).encode()
        with patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            import io
            res = client.post(
                "/api/admin/backup/restore?mode=merge",
                files={"file": ("backup.json", io.BytesIO(content), "application/json")},
            )
        assert res.status_code == 400

    def test_archivo_demasiado_grande_retorna_413(self, client):
        import io
        import routes.backup as backup_mod
        contenido = b"x" * 100  # de sobra con un límite bajado para la prueba
        with patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL), \
             patch.object(backup_mod, "_MAX_RESTORE_BYTES", 10):
            res = client.post(
                "/api/admin/backup/restore?mode=merge",
                files={"file": ("grande.json", io.BytesIO(contenido), "application/json")},
            )
        assert res.status_code == 413

    def test_fallo_a_mitad_de_tabla_no_deja_insercion_parcial(self, client):
        """IN-022: si una fila falla dentro de la tabla, el resultado reporta
        error para esa tabla y no un `inserted` parcial — la transacción
        deshace lo que llevaba hecho esa tabla (incluido el DELETE del modo
        overwrite), que es justo lo que `db_transaction()` garantiza."""
        backup = {
            "_metadata": {"version": "1.1", "tables": ["categoria"], "partial": False},
            "categoria": [
                {"id": 1, "nombre": "Uno", "slug": "uno"},
                {"id": 2, "nombre": "Dos", "slug": "dos"},
            ],
        }
        content = json.dumps(backup).encode()

        call_log = []
        # La segunda sentencia INSERT falla.
        vistos = {"n": 0}

        def fail_when(sql):
            if "INSERT" in sql:
                vistos["n"] += 1
                return vistos["n"] == 2
            return False

        tx = _FakeTransaction(call_log, fail_when=fail_when)

        with patch("routes.backup.db_transaction", return_value=tx), \
             patch("routes.backup.db_query", return_value=None), \
             patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            import io
            res = client.post(
                "/api/admin/backup/restore?mode=overwrite&requester=admin",
                files={"file": ("backup.json", io.BytesIO(content), "application/json")},
            )

        assert res.status_code == 200
        body = res.json()
        assert "error" in body["results"]["categoria"]
        assert "inserted" not in body["results"]["categoria"]
        assert body["success"] is False
        assert tx.rolled_back is True

    def test_secuencia_se_reajusta_tras_insertar_con_id_explicito(self, client):
        backup = {
            "_metadata": {"version": "1.1", "tables": ["categoria"], "partial": False},
            "categoria": [{"id": 50, "nombre": "Test", "slug": "test"}],
        }
        content = json.dumps(backup).encode()

        call_log = []
        tx = _FakeTransaction(call_log)

        with patch("routes.backup.db_transaction", return_value=tx), \
             patch("routes.backup.db_query", return_value=None), \
             patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            import io
            res = client.post(
                "/api/admin/backup/restore?mode=merge&requester=admin",
                files={"file": ("backup.json", io.BytesIO(content), "application/json")},
            )

        assert res.status_code == 200
        assert any("setval" in s and "pg_get_serial_sequence" in s for s in call_log)


class TestExportManifiestoR2:
    """DG-149: el export deja constancia de qué claves de R2 referencian los
    documentos exportados, aunque no copie los ficheros."""

    def test_export_incluye_claves_r2_referenciadas(self, client):
        filas_archivo = [
            _mock_row(id_archivo=1, titulo="A", file_url="/api/files/archivo/uno.pdf"),
            _mock_row(id_archivo=2, titulo="B", file_url="/api/files/archivo/dos.pdf"),
            _mock_row(id_archivo=3, titulo="C", file_url=""),
        ]

        def mock_query(sql, params=None, fetch="all"):
            if "datos_archivo" in sql:
                return filas_archivo
            return []

        with patch("routes.backup.db_query", side_effect=mock_query), \
             patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            res = client.get(
                "/api/admin/backup/export?tables=datos_archivo&requester=admin"
            )

        assert res.status_code == 200
        body = json.loads(res.content)
        claves = body["_metadata"]["r2_keys_referenced"]
        assert "archivo/uno.pdf" in claves
        assert "archivo/dos.pdf" in claves
        assert body["_metadata"]["r2_keys_count"] == 2

    def test_export_registra_la_sesion_no_el_query_string(self, client_as):
        """IN-133: `backup_history.usuario` debe salir de la sesión
        verificada, no de un parámetro `requester` que declare quien llama."""
        c = client_as("admin_real")
        with patch("routes.backup.db_query", return_value=[]) as mock_dq, \
             patch("routes.admin.deps.db_query", return_value=_FILA_ADMIN_GLOBAL):
            res = c.get(
                "/api/admin/backup/export?tables=categoria&requester=otro_usuario_falsificado"
            )
        assert res.status_code == 200
        insert_calls = [
            call for call in mock_dq.call_args_list
            if call.args and "INSERT INTO public.backup_history" in call.args[0]
        ]
        assert len(insert_calls) == 1
        assert insert_calls[0].args[1][0] == "admin_real"

    def test_extraer_claves_r2_ignora_urls_sin_prefijo_esperado(self):
        from routes.backup import _extraer_claves_r2

        backup = {
            "datos_archivo": [
                {"file_url": "/api/files/archivo/valido.pdf"},
                {"file_url": "https://otro-dominio.example/x.pdf"},
                {"file_url": ""},
                {"file_url": None},
            ],
            "datos_rrhh": [
                {"file_url": "/api/files/rrhh/uno.pdf"},
            ],
        }
        claves = _extraer_claves_r2(backup)
        assert claves == ["archivo/valido.pdf", "rrhh/uno.pdf"]
