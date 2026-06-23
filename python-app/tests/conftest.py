"""
Fixtures compartidas para los tests del backend.

Los tests mockean db_query para no requerir una conexión real a Neon/PostgreSQL.
"""
import pytest
from unittest.mock import patch, MagicMock
from fastapi.testclient import TestClient


@pytest.fixture(scope="session")
def app():
    """Importa la app con startup hooks desactivados."""
    with (
        patch("database.ensure_audit_table", return_value=None),
        patch("utils.populate_missing_slugs", return_value=None),
    ):
        from main import app as _app
        yield _app


@pytest.fixture
def client(app):
    with TestClient(app) as c:
        yield c


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
