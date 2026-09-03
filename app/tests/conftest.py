"""
Fixtures compartidas para los tests del backend.

Los tests mockean db_query para no requerir una conexión real a Neon/PostgreSQL.
"""
import pytest
from unittest.mock import patch, MagicMock
from fastapi.testclient import TestClient


@pytest.fixture(scope="session")
def app():
    """Importa la app con startup hooks desactivados (sin conexión a BD)."""
    # Se parchean los nombres TAL COMO QUEDAN LIGADOS EN `main`, no en su modulo
    # de origen: main hace `from database import ensure_audit_table`, asi que
    # parchear `database.ensure_audit_table` no le llega si main ya fue
    # importado. Dependia del orden de importacion — y en cuanto otro test
    # importo main durante la recoleccion, el arranque intento conectar de
    # verdad a Neon y la suite se quedo colgada.
    import main  # noqa: F401  (asegura que el modulo exista antes de parchearlo)

    with (
        patch("main.ensure_audit_table", return_value=None),
        patch("main.populate_missing_slugs", return_value=None),
        patch("main.run_migrations", return_value=None),
        patch("main._backfill_rrhh_tipo_fk", return_value=None),
        patch("database.ensure_audit_table", return_value=None),
        patch("utils.populate_missing_slugs", return_value=None),
        patch("database.log_event", return_value=None),
    ):
        from main import app as _app
        yield _app


@pytest.fixture
def client(app):
    """Cliente de tests con require_session desactivado (retorna usuario ficticio).

    Por defecto simula "hay sesión válida de test_user", para no romper las
    pruebas existentes que asumen sesión y no les importa quién es el usuario.
    """
    from routes.admin.deps import require_session
    app.dependency_overrides[require_session] = lambda: "test_user"
    with TestClient(app) as c:
        yield c
    app.dependency_overrides.pop(require_session, None)


@pytest.fixture
def anon_client(app):
    """Cliente SIN ninguna sobrescritura de require_session.

    A diferencia de `client`, aquí `require_session` corre de verdad: sin
    cookie ni cabecera X-Session-Token, la petición debe devolver 401. Úsalo
    para probar "sin sesión, 401" en cualquier ruta admin.

    Nota SI-226: antes de este fixture, `client` aplanaba require_session a
    `lambda: "test_user"` de forma incondicional en TODA la suite, así que
    ningún test podía detectar una ruta que se hubiera quedado sin protección.
    """
    from routes.admin.deps import require_session
    previous = app.dependency_overrides.pop(require_session, None)
    with TestClient(app) as c:
        yield c
    # Restaura el estado anterior (el override por defecto de `client`, si lo
    # hubiera) para no filtrar este cambio a otros tests que compartan `app`.
    if previous is not None:
        app.dependency_overrides[require_session] = previous
    else:
        app.dependency_overrides.pop(require_session, None)


@pytest.fixture
def client_as(app):
    """Factory de clientes con sesión de un usuario concreto: `client_as("ana")`.

    Pensado para que otros carriles (bloque O1 y siguientes, cuando exista una
    comprobación real de rol/módulo aguas abajo de require_session) puedan
    escribir tests de autorización por rol sin reinventar el mecanismo de
    override: basta con pedir `client_as("usuario_x")` y, si la ruta consulta
    la base para conocer el rol/módulo de ese usuario, mockear `db_query` (o
    lo que corresponda) para que devuelva la fila de usuario que se quiere
    probar — así se ejercita la lógica real de autorización, no una que
    siempre da "true".

    Ejemplo de uso futuro:

        def test_solo_admin_global_puede_X(client_as):
            c = client_as("usuario_normal")
            with patch("routes.admin.X.db_query", return_value=[fila_rol_normal]):
                res = c.get("/api/admin/solo-global")
            assert res.status_code == 403
    """
    from routes.admin.deps import require_session
    previous = app.dependency_overrides.get(require_session)
    clientes = []

    def _factory(usuario="test_user"):
        app.dependency_overrides[require_session] = lambda: usuario
        c = TestClient(app)
        clientes.append(c)
        return c

    yield _factory

    for c in clientes:
        c.close()
    if previous is not None:
        app.dependency_overrides[require_session] = previous
    else:
        app.dependency_overrides.pop(require_session, None)


# ─── Filas de prueba reutilizables ────────────────────────────────────────────

def _archivo_row(**kwargs):
    base = {
        "id": 1, "titulo": "Informe de Prueba", "autor": "Test",
        "fecha": "2024-01-10", "doc_type": "Informe",
        "categoria": "Parte I", "ubicacion": "Digitalizado Exclusivo",
        "tesauro_primario": "Informe", "tesauro_secundario": "Parte I",
        "descriptores_libres": "prueba; test", "resumen": "Resumen de prueba",
        "file_url": "",
    }
    base.update(kwargs)
    row = MagicMock()
    row.__getitem__ = lambda self, k: base[k]
    row.keys = lambda: base.keys()
    # Permitir dict(row)
    row.__iter__ = lambda self: iter(base)
    return row


def _rrhh_view_row(**kwargs):
    base = {
        "empleado_id": 1, "cedula": "V-12345678", "rif": "J-12345678-0",
        "persona_raw": "Carlos Alberto Gomez Perez", "cargo": "Director General",
        "departamento": "Decanato", "estado": "Activo",
        "fecha_ingreso": "2015-06-20", "fecha_jubilacion": None,
        "fecha_pension": None, "foto_url": "", "doc_count": 3,
        "tipos": "Contrato; Cédula",
    }
    base.update(kwargs)
    row = MagicMock()
    row.__getitem__ = lambda self, k: base[k]
    row.keys = lambda: base.keys()
    row.__iter__ = lambda self: iter(base)
    return row


def _user_row(**kwargs):
    import bcrypt
    hashed = bcrypt.hashpw(b"test1234", bcrypt.gensalt()).decode()
    base = {
        "id": 1, "usuario": "test_user", "nombre_usuario": "Test User",
        "contrasena": hashed, "modulo": "Archivo", "rol": "Admin",
    }
    base.update(kwargs)
    row = MagicMock()
    row.__getitem__ = lambda self, k: base[k]
    row.keys = lambda: base.keys()
    row.__iter__ = lambda self: iter(base)
    return row
