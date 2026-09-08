"""R49: el `exception_handler(RequestValidationError)` de R48 (app/main.py)
solo se probó con un body JSON inválido. `RequestValidationError` también la
lanza FastAPI para query params, path params y multipart/form-data mal
tipados -- casos donde `loc` no empieza con "body" (p. ej. `("query", "page")`
o `("path", "emp_id")`), y para el borde en que tras filtrar "body" de `loc`
no queda ningún segmento (el handler cae a "desconocido").

Este archivo confirma que el handler es robusto en esos ángulos, no solo en
el caso feliz que probó R48.
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
ROL_ARCHIVO_NORMAL = _fila(modulo="Archivo", rol="Normal", is_active=True)


class TestValidationErrorOtrosTipos:
    def test_path_param_invalido_da_mensaje_legible(self, client_as):
        """`/api/rrhh/empleado/{emp_id}/documentos` tipa `emp_id: int` en la
        ruta. Un id no numérico dispara `RequestValidationError` con
        `loc=("path", "emp_id")`, no `("body", ...)`.
        """
        c = client_as("r49_path")
        with patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL):
            res = c.get("/api/rrhh/empleado/abc/documentos")
        assert res.status_code == 422
        detail = res.json()["detail"]
        assert isinstance(detail, str)
        assert "emp_id" in detail

    def test_query_param_invalido_da_mensaje_legible(self, client_as):
        """El mismo endpoint tipa `page: int = 1` como query param."""
        c = client_as("r49_query")
        with patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL):
            res = c.get("/api/rrhh/empleado/1/documentos?page=abc")
        assert res.status_code == 422
        detail = res.json()["detail"]
        assert isinstance(detail, str)
        assert "page" in detail

    def test_multipart_sin_archivo_requerido_da_mensaje_legible(self, client_as):
        """`/api/admin/import/empleados` exige `file: UploadFile = File(...)`.
        Sin el campo, FastAPI reporta el error de multipart/form-data, no de
        JSON -- confirma que el handler también lo cubre.
        """
        c = client_as("r49_multipart")
        res = c.post("/api/admin/import/empleados")
        assert res.status_code == 422
        detail = res.json()["detail"]
        assert isinstance(detail, str)
        assert "file" in detail

    def test_dos_campos_invalidos_da_mensaje_plural(self, client_as):
        """Path Y query inválidos a la vez: confirma la rama plural
        ("Hay N campos...") con `loc` que no es de body en ninguno de los dos.
        """
        c = client_as("r49_plural")
        with patch("routes.admin.deps.db_query", return_value=ROL_RRHH_NORMAL):
            res = c.get("/api/rrhh/empleado/abc/documentos?page=xyz")
        assert res.status_code == 422
        detail = res.json()["detail"]
        assert isinstance(detail, str)
        assert detail.startswith("Hay 2 campos")
        assert "emp_id" in detail
        assert "page" in detail

    def test_loc_solo_body_cae_a_desconocido_sin_romper(self, client_as):
        """Si `loc` es solo `("body",)` (error a nivel raíz del body, p. ej.
        mandar una lista donde se esperaba un objeto), tras filtrar "body" no
        queda ningún segmento. El handler debe caer a "desconocido" sin
        `IndexError`/`KeyError` ni dejar un mensaje vacío o mal formado.
        """
        c = client_as("r49_raiz")
        with patch("routes.admin.deps.db_query", return_value=ROL_ARCHIVO_NORMAL):
            res = c.post(
                "/api/admin/keywords",
                content=b"[1,2,3]",
                headers={"Content-Type": "application/json"},
            )
        assert res.status_code == 422
        detail = res.json()["detail"]
        assert isinstance(detail, str)
        assert detail == "Revisa el campo 'desconocido': los datos enviados no son válidos."
