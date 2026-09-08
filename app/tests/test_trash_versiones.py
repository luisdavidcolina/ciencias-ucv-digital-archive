"""Cobertura de comportamiento real de `app/routes/trash.py` (versiones de
archivos digitales), zona TEST-trash-versiones-cobertura de `_RESERVAS.md`.

`test_autorizacion_papelera.py` sólo cubre 401/403/409/200 de autorización
para `/papelera` y `/papelera/*/purgar`. Ningún test ejercitaba la lógica de
negocio de `/documento/{id}/versiones*` (add_version, restore_version,
delete_version, restore_document):

- IN-149: `add_version` rechaza un `file_url` con `..` (recorrido de ruta),
  no sólo el esquema (`/` o `http(s)://`).
- OA-008: `restore_version` archiva la versión vigente como versión nueva
  ANTES de sobrescribirla -- restaurar v1 sobre una v3 buena no debía hacer
  desaparecer v3. Antes de este arreglo sobreescribía sin archivar.
- `restore_version` NO debe crear una versión "fantasma" cuando la URL
  vigente ya es idéntica a la versión que se restaura.
- `restore_document`/`delete_version` con IDs inexistentes deben responder
  404 sin tocar nada.

Sigue el patrón de `test_autorizacion_papelera.py`: fixtures `client_as`
(SI-226) con `routes.admin.deps.db_query` mockeado para la fila de sesión, y
`routes.trash.db_query`/`db_transaction` mockeados para simular la secuencia
real de lecturas/escrituras sin tocar una base de datos real.
"""
from unittest.mock import MagicMock, patch


def _fila(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.get = lambda k, default=None: data.get(k, default)
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


def _sesion_archivo_normal():
    return _fila(modulo="Archivo", rol="Normal", is_active=True)


class TestAddVersionRutaSegura:
    """`POST /api/admin/documento/{id}/versiones` -- IN-149."""

    def test_rechaza_file_url_con_recorrido_de_ruta(self, client_as):
        c = client_as("archivo_normal")
        with patch("routes.admin.deps.db_query", return_value=_sesion_archivo_normal()):
            res = c.post(
                "/api/admin/documento/1/versiones",
                params={
                    "modulo": "Archivo",
                    "file_url": "/api/files/../../otra-cosa",
                    "usuario": "archivo_normal",
                },
            )
        assert res.status_code == 400

    def test_rechaza_file_url_con_esquema_invalido(self, client_as):
        c = client_as("archivo_normal")
        with patch("routes.admin.deps.db_query", return_value=_sesion_archivo_normal()):
            res = c.post(
                "/api/admin/documento/1/versiones",
                params={
                    "modulo": "Archivo",
                    "file_url": "javascript:alert(1)",
                    "usuario": "archivo_normal",
                },
            )
        assert res.status_code == 400

    def test_404_si_el_documento_no_existe_o_esta_en_papelera(self, client_as):
        c = client_as("archivo_normal")
        with patch("routes.admin.deps.db_query", return_value=_sesion_archivo_normal()), \
             patch("routes.trash.db_query", return_value=None):
            res = c.post(
                "/api/admin/documento/1/versiones",
                params={
                    "modulo": "Archivo",
                    "file_url": "/api/files/nuevo.pdf",
                    "usuario": "archivo_normal",
                },
            )
        assert res.status_code == 404

    def test_archiva_la_url_vigente_antes_de_reemplazarla(self, client_as):
        """El flujo normal: la URL actual se guarda como versión histórica
        antes de que `file_url` apunte a la nueva."""
        c = client_as("archivo_normal")
        current = _fila(file_url="/api/files/viejo.pdf")
        last_ver = _fila(vn=2)
        insert_calls = []
        update_calls = []

        def side_effect(sql, params=None, **kwargs):
            if sql.strip().startswith("SELECT file_url FROM public.datos_archivo"):
                return current
            if "COALESCE(MAX(version_num)" in sql:
                return last_ver
            if sql.strip().startswith("INSERT INTO public.documento_versiones"):
                insert_calls.append(params)
                return None
            if sql.strip().startswith("UPDATE public.datos_archivo"):
                update_calls.append(params)
                return None
            raise AssertionError(f"consulta inesperada: {sql}")

        with patch("routes.admin.deps.db_query", return_value=_sesion_archivo_normal()), \
             patch("routes.trash.db_query", side_effect=side_effect), \
             patch("routes.trash.log_event"):
            res = c.post(
                "/api/admin/documento/1/versiones",
                params={
                    "modulo": "Archivo",
                    "file_url": "/api/files/nuevo.pdf",
                    "usuario": "archivo_normal",
                },
            )
        assert res.status_code == 200
        assert res.json()["version_num"] == 3
        # La versión archivada es la URL VIEJA, no la nueva.
        assert insert_calls[0][3] == "/api/files/viejo.pdf"
        # file_url del documento pasa a apuntar a la nueva.
        assert update_calls[0][0] == "/api/files/nuevo.pdf"


class TestRestoreVersion:
    """`POST /api/admin/documento/{id}/versiones/{ver_id}/restaurar` -- OA-008."""

    def test_404_si_la_version_no_existe(self, client_as):
        c = client_as("archivo_normal")
        with patch("routes.admin.deps.db_query", return_value=_sesion_archivo_normal()), \
             patch("routes.trash.db_query", return_value=None):
            res = c.post("/api/admin/documento/1/versiones/99/restaurar?modulo=Archivo")
        assert res.status_code == 404

    def test_archiva_la_version_vigente_antes_de_restaurar_una_anterior(self, client_as):
        """OA-008: restaurar v1 sobre una v3 distinta no debe hacer
        desaparecer v3 -- se archiva como versión nueva dentro de la misma
        transacción antes de sobreescribir `file_url`."""
        c = client_as("archivo_normal")
        version_pedida = _fila(file_url="/api/files/v1.pdf")
        current = _fila(file_url="/api/files/v3-vigente.pdf")
        last_ver = _fila(vn=3)

        mock_execute = MagicMock(side_effect=[last_ver, None, None])

        def trash_db_query_side_effect(sql, params=None, **kwargs):
            if sql.strip().startswith("SELECT file_url FROM public.documento_versiones"):
                return version_pedida
            if sql.strip().startswith("SELECT file_url FROM public.datos_archivo"):
                return current
            raise AssertionError(f"consulta inesperada fuera de la transacción: {sql}")

        with patch("routes.admin.deps.db_query", return_value=_sesion_archivo_normal()), \
             patch("routes.trash.db_query", side_effect=trash_db_query_side_effect), \
             patch("routes.trash.db_transaction") as mock_tx, \
             patch("routes.trash.log_event"):
            mock_tx.return_value.__enter__.return_value = mock_execute
            res = c.post(
                "/api/admin/documento/1/versiones/5/restaurar",
                params={"modulo": "Archivo", "usuario": "archivo_normal"},
            )
        assert res.status_code == 200
        # Primera llamada dentro de la transacción: MAX(version_num).
        # Segunda: INSERT que archiva la URL VIGENTE (v3), no la que se restaura.
        insert_call_args = mock_execute.call_args_list[1]
        assert insert_call_args[0][1][3] == "/api/files/v3-vigente.pdf"
        # Tercera: UPDATE que fija file_url a la versión restaurada (v1).
        update_call_args = mock_execute.call_args_list[2]
        assert update_call_args[0][1][0] == "/api/files/v1.pdf"

    def test_no_archiva_version_fantasma_si_la_vigente_ya_es_la_pedida(self, client_as):
        """Si la URL vigente ya coincide con la versión pedida (restaurar la
        misma versión dos veces, o ya estaba activa), no se crea un
        duplicado en el historial."""
        c = client_as("archivo_normal")
        version_pedida = _fila(file_url="/api/files/v1.pdf")
        current = _fila(file_url="/api/files/v1.pdf")
        mock_execute = MagicMock(return_value=None)

        def db_query_side_effect(sql, params=None, **kwargs):
            if sql.strip().startswith("SELECT file_url FROM public.documento_versiones"):
                return version_pedida
            if sql.strip().startswith("SELECT file_url FROM public.datos_archivo"):
                return current
            raise AssertionError(f"consulta inesperada: {sql}")

        with patch("routes.admin.deps.db_query", return_value=_sesion_archivo_normal()), \
             patch("routes.trash.db_query", side_effect=db_query_side_effect), \
             patch("routes.trash.db_transaction") as mock_tx, \
             patch("routes.trash.log_event"):
            mock_tx.return_value.__enter__.return_value = mock_execute
            res = c.post(
                "/api/admin/documento/1/versiones/5/restaurar",
                params={"modulo": "Archivo", "usuario": "archivo_normal"},
            )
        assert res.status_code == 200
        # Sólo el UPDATE final -- ningún INSERT de archivado.
        assert mock_execute.call_count == 1
        assert mock_execute.call_args[0][0].strip().startswith("UPDATE public.datos_archivo")


class TestDeleteVersion:
    """`DELETE /api/admin/documento/{id}/versiones/{ver_id}`."""

    def test_404_si_la_version_no_existe(self, client_as):
        c = client_as("archivo_normal")
        with patch("routes.admin.deps.db_query", return_value=_sesion_archivo_normal()), \
             patch("routes.trash.db_query", return_value=None):
            res = c.delete("/api/admin/documento/1/versiones/99?modulo=Archivo")
        assert res.status_code == 404

    def test_borra_la_version_correcta(self, client_as):
        c = client_as("archivo_normal")
        with patch("routes.admin.deps.db_query", return_value=_sesion_archivo_normal()), \
             patch("routes.trash.db_query", return_value=_fila(id=5)) as mock_dbq, \
             patch("routes.trash.log_event"):
            res = c.delete("/api/admin/documento/1/versiones/5?modulo=Archivo")
        assert res.status_code == 200
        sql, params = mock_dbq.call_args[0][0], mock_dbq.call_args[0][1]
        assert params == [5, "datos_archivo", 1]


class TestRestoreDocument:
    """`POST /api/admin/papelera/{doc_id}/restaurar`."""

    def test_404_si_no_esta_en_papelera(self, client_as):
        c = client_as("archivo_normal")
        with patch("routes.admin.deps.db_query", return_value=_sesion_archivo_normal()), \
             patch("routes.trash.db_query", return_value=None):
            res = c.post("/api/admin/papelera/1/restaurar?modulo=Archivo&usuario=archivo_normal")
        assert res.status_code == 404

    def test_restaura_documento_de_archivo(self, client_as):
        c = client_as("archivo_normal")
        with patch("routes.admin.deps.db_query", return_value=_sesion_archivo_normal()), \
             patch("routes.trash.db_query", return_value=_fila(id_archivo=1)) as mock_dbq, \
             patch("routes.trash.log_event"):
            res = c.post("/api/admin/papelera/1/restaurar?modulo=Archivo&usuario=archivo_normal")
        assert res.status_code == 200
        assert res.json() == {"success": True}
        sql = mock_dbq.call_args[0][0]
        assert "deleted_at=NULL" in sql
        assert "datos_archivo" in sql
