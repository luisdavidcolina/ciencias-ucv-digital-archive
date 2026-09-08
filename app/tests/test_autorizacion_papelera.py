"""Pruebas de autorización de `app/routes/trash.py` (papelera y versiones).

Contexto: `C7-papelera` aplica `require_role`/`require_admin_role` (O1,
`routes/admin/deps.py`) a los endpoints de papelera. Antes de esto, cualquier
sesión válida —sin mirar módulo ni rol— podía listar, restaurar o purgar
documentos y empleados. La purga (`DELETE .../purgar`) es irreversible y por
eso exige además `rol = 'Admin'` en el módulo, no sólo pertenecer a él.

Sigue el patrón de `test_autorizacion_deps.py`: fixtures `anon_client` y
`client_as` (SI-226), con `routes.admin.deps.db_query` mockeado para simular
la fila de `usuarios_sistema`, y `repos.trash_repo.db_query` (IN-042 paso 7:
`list_trash`/`list_trash_employees`/`list_versions` se movieron ahí) mockeado
sólo cuando el escenario debe llegar a ejecutar la consulta real (caso 200).
"""
from unittest.mock import MagicMock, patch


def _fila(**data):
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.get = lambda k, default=None: data.get(k, default)
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


class TestListarPapelera:
    """`GET /api/admin/papelera` lleva `require_role("Archivo", "RRHH")`."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.get("/api/admin/papelera?modulo=Archivo")
        assert res.status_code == 401

    def test_usuario_sin_modulo_asignado_recibe_403(self, client_as):
        c = client_as("usuario_fantasma")
        with patch("routes.admin.deps.db_query", return_value=None):
            res = c.get("/api/admin/papelera?modulo=Archivo")
        assert res.status_code == 403

    def test_usuario_de_archivo_si_puede_listar(self, client_as):
        c = client_as("archivo_normal")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=True)
        count_row = _fila(total=0)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("repos.trash_repo.db_query", side_effect=[count_row, []]):
            res = c.get("/api/admin/papelera?modulo=Archivo")
        assert res.status_code == 200


class TestPurgarDocumento:
    """`DELETE /api/admin/papelera/{doc_id}/purgar` es irreversible: exige
    `require_admin_role`, no basta con pertenecer al módulo."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.delete("/api/admin/papelera/1/purgar?modulo=Archivo&usuario=x")
        assert res.status_code == 401

    def test_usuario_normal_de_archivo_no_puede_purgar(self, client_as):
        """OR-009/OR-011/OR-012: un usuario Normal del módulo correcto sigue
        sin poder ejecutar el borrado irreversible."""
        c = client_as("archivo_normal")
        fila = _fila(modulo="Archivo", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.delete("/api/admin/papelera/1/purgar?modulo=Archivo&usuario=archivo_normal")
        assert res.status_code == 403

    # NOTA (IN-008, ya en la lista de este carril pero fuera de esta tarea:
    # su arreglo toca `routes/admin/deps.py` [CHOCA], que esta tarea tiene
    # prohibido tocar): `require_admin_role("Archivo", "RRHH")` sólo exige
    # que el usuario sea Admin en *alguno* de los dos módulos, no
    # específicamente en el `modulo` que llega por query string. Un Admin de
    # RRHH que llame `.../purgar?modulo=Archivo` pasa la dependencia igual
    # que en `docs.py` (mismo patrón ya usado por el carril hermano
    # C1-docs-backend) y sólo lo para el 404 porque el `doc_id` no existe en
    # esa tabla. Documentado en `docs/auditoria/_BUZON.md`.

    def test_admin_del_modulo_si_puede_purgar(self, client_as):
        c = client_as("archivo_admin")
        fila = _fila(modulo="Archivo", rol="Admin", is_active=True)
        # OA-006: purge_document ahora hace dos lecturas antes de la
        # transacción (el documento, luego sus versiones) para poder borrar
        # también los objetos de R2 -- side_effect refleja esa secuencia real,
        # en vez de un return_value único (que al iterarse como lista de
        # versiones daba sus claves de dict, no filas).
        existing = _fila(id_archivo=1, file_url=None)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.trash.db_query", side_effect=[existing, []]), \
             patch("routes.trash.db_transaction") as mock_tx, \
             patch("routes.trash.log_event"):
            mock_tx.return_value.__enter__.return_value = MagicMock()
            res = c.delete("/api/admin/papelera/1/purgar?modulo=Archivo&usuario=archivo_admin")
        assert res.status_code == 200

    def test_admin_global_si_puede_purgar(self, client_as):
        c = client_as("admin_global")
        fila = _fila(modulo="Global", rol="Admin", is_active=True)
        existing = _fila(id_rrhh=2, file_url=None)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.trash.db_query", side_effect=[existing, []]), \
             patch("routes.trash.db_transaction") as mock_tx, \
             patch("routes.trash.log_event"):
            mock_tx.return_value.__enter__.return_value = MagicMock()
            res = c.delete("/api/admin/papelera/2/purgar?modulo=RRHH&usuario=admin_global")
        assert res.status_code == 200


class TestPurgarEmpleado:
    """`DELETE /api/admin/papelera/empleados/{emp_id}/purgar` también es
    irreversible (borra empleado, documentos e historial): exige
    `require_admin_role("RRHH")`."""

    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.delete("/api/admin/papelera/empleados/1/purgar?usuario=x")
        assert res.status_code == 401

    def test_usuario_normal_de_rrhh_no_puede_purgar_empleado(self, client_as):
        c = client_as("rrhh_normal")
        fila = _fila(modulo="RRHH", rol="Normal", is_active=True)
        with patch("routes.admin.deps.db_query", return_value=fila):
            res = c.delete("/api/admin/papelera/empleados/1/purgar?usuario=rrhh_normal")
        assert res.status_code == 403

    def test_admin_de_rrhh_si_puede_purgar_empleado(self, client_as):
        c = client_as("rrhh_admin")
        fila = _fila(modulo="RRHH", rol="Admin", is_active=True)
        # OA-007: purge_employee lee al empleado, luego sus documentos (para
        # poder borrar también versiones y objetos de R2) antes de la
        # transacción -- side_effect refleja esa secuencia real de lecturas.
        # OR-012: entre medias, ahora cuenta documentos vivos (no en
        # papelera) del empleado y rechaza la purga si hay alguno.
        existing = _fila(id=1)
        vivos = _fila(total=0)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.trash.db_query", side_effect=[existing, vivos, []]), \
             patch("routes.trash.db_transaction") as mock_tx, \
             patch("routes.trash.log_event"):
            mock_tx.return_value.__enter__.return_value = MagicMock()
            res = c.delete("/api/admin/papelera/empleados/1/purgar?usuario=rrhh_admin")
        assert res.status_code == 200

    def test_no_purga_si_el_empleado_tiene_documentos_vivos(self, client_as):
        """OR-012: `datos_rrhh.empleado_id` tiene `ON DELETE CASCADE` hacia
        `empleados` -- si se dejara purgar con documentos vivos, borrar la
        fila de `empleados` al final de la transacción los arrastraría en
        cascada aunque nunca pasaron por la papelera."""
        c = client_as("rrhh_admin")
        fila = _fila(modulo="RRHH", rol="Admin", is_active=True)
        existing = _fila(id=1)
        vivos = _fila(total=3)
        with patch("routes.admin.deps.db_query", return_value=fila), \
             patch("routes.trash.db_query", side_effect=[existing, vivos]):
            res = c.delete("/api/admin/papelera/empleados/1/purgar?usuario=rrhh_admin")
        assert res.status_code == 409
