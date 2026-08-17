"""Tests para endpoints de categorías, palabras clave, retención y papelera."""
import pytest
from unittest.mock import patch, MagicMock


def _row(**data):
    r = MagicMock()
    r.__getitem__ = lambda self, k: data[k]
    r.keys = lambda: data.keys()
    r.__iter__ = lambda self: iter(data)
    r.get = lambda k, d=None: data.get(k, d)
    return r


# =============================================================================
# Palabras clave
# =============================================================================

class TestKeywords:
    def test_list_keywords(self, client):
        kw = _row(id=1, nombre="gestión", uso_archivo=3)
        with patch("routes.admin.catalog.db_query", return_value=[kw]):
            res = client.get("/api/admin/keywords")
        assert res.status_code == 200
        body = res.json()
        assert isinstance(body, list)
        assert body[0]["nombre"] == "gestión"

    def test_create_keyword_ok(self, client):
        new_kw = _row(id=5)
        call_n = [0]
        def mock_q(sql, params=None, fetch="all", commit=False):
            call_n[0] += 1
            if call_n[0] == 1:  # SELECT exists check
                return None
            return new_kw  # INSERT RETURNING
        with patch("routes.admin.catalog.db_query", side_effect=mock_q):
            res = client.post("/api/admin/keywords", json={"nombre": "nueva"})
        assert res.status_code == 200
        assert res.json()["success"] is True

    def test_create_keyword_duplicado_retorna_400(self, client):
        existing = _row(id_descriptor=1)
        call_n = [0]
        def mock_q(sql, params=None, fetch="all", commit=False):
            call_n[0] += 1
            return existing if call_n[0] == 1 else None
        with patch("routes.admin.catalog.db_query", side_effect=mock_q):
            res = client.post("/api/admin/keywords", json={"nombre": "duplicado"})
        assert res.status_code == 400

    def test_create_keyword_vacio_retorna_400(self, client):
        with patch("routes.admin.catalog.db_query", return_value=None):
            res = client.post("/api/admin/keywords", json={"nombre": "   "})
        assert res.status_code == 400

    def test_create_keyword_muy_larga_retorna_422(self, client):
        res = client.post("/api/admin/keywords", json={"nombre": "k" * 201})
        assert res.status_code == 422

    def test_delete_keyword_sin_uso(self, client):
        uso = _row(cnt=0)
        call_n = [0]
        def mock_q(sql, params=None, fetch="all", commit=False):
            call_n[0] += 1
            return uso if call_n[0] == 1 else None
        with patch("routes.admin.catalog.db_query", side_effect=mock_q):
            res = client.delete("/api/admin/keywords/1")
        assert res.status_code == 200
        assert res.json()["success"] is True

    def test_delete_keyword_en_uso_sin_force_retorna_400(self, client):
        uso = _row(cnt=3)
        with patch("routes.admin.catalog.db_query", return_value=uso):
            res = client.delete("/api/admin/keywords/1")
        assert res.status_code == 400
        assert "uso" in res.json()["detail"].lower()

    def test_delete_keyword_en_uso_con_force(self, client):
        uso = _row(cnt=3)
        call_n = [0]
        def mock_q(sql, params=None, fetch="all", commit=False):
            call_n[0] += 1
            return uso if call_n[0] == 1 else None
        with patch("routes.admin.catalog.db_query", side_effect=mock_q):
            res = client.delete("/api/admin/keywords/1?force=true")
        assert res.status_code == 200
        assert res.json()["removed_from_docs"] == 3


# =============================================================================
# Categorías (tipologías)
# =============================================================================

class TestCategories:
    def test_add_category_ok(self, client):
        cat = _row(id=2)
        call_n = [0]
        def mock_q(sql, params=None, fetch="all", commit=False):
            call_n[0] += 1
            if "slug" in sql and call_n[0] == 1:  # busca categoría
                return cat
            if "LOWER(nombre)" in sql:             # busca existente
                return None
            return None
        with (
            patch("routes.admin.catalog.db_query", side_effect=mock_q),
            patch("routes.admin.catalog.generate_unique_slug", return_value="nueva-tipo"),
            patch("routes.admin.catalog.invalidate_choices_cache"),
            patch("routes.admin.catalog.log_event"),
        ):
            res = client.post("/api/admin/add_category", json={
                "name": "Nueva Tipología", "desc": "Desc", "scope": "Archivo",
                "usuario": "admin", "parte": "",
            })
        assert res.status_code == 200
        assert res.json()["success"] is True

    def test_add_category_nombre_muy_largo_retorna_422(self, client):
        res = client.post("/api/admin/add_category", json={
            "name": "x" * 201, "desc": "Desc", "scope": "Archivo",
            "usuario": "admin", "parte": "",
        })
        assert res.status_code == 422


# =============================================================================
# Retención
# =============================================================================

class TestRetencion:
    def test_list_tipos_retencion(self, client):
        tipo = _row(id=1, nombre="Acta", nombre_corto="Acta",
                    plazo_retencion_anios=5, categoria="Archivo",
                    categoria_slug="archivo", uso_archivo=0, uso_rrhh=0)
        with patch("routes.admin.retention.db_query", return_value=[tipo]):
            res = client.get("/api/admin/retencion/tipos")
        assert res.status_code == 200
        assert res.json()["tipos"][0]["plazo_retencion_anios"] == 5

    def test_update_retencion_ok(self, client):
        tipo = _row(id=1, nombre="Acta")
        call_n = [0]
        def mock_q(sql, params=None, fetch="all", commit=False):
            call_n[0] += 1
            return tipo if call_n[0] == 1 else None
        with (
            patch("routes.admin.retention.db_query", side_effect=mock_q),
            patch("routes.admin.retention.invalidate_choices_cache"),
            patch("routes.admin.retention.log_event"),
        ):
            res = client.patch("/api/admin/retencion/tipos/1",
                               json={"plazo_retencion_anios": 10, "requester": "admin"})
        assert res.status_code == 200
        assert res.json()["plazo_retencion_anios"] == 10

    def test_update_retencion_plazo_cero_rechazado(self, client):
        res = client.patch("/api/admin/retencion/tipos/1",
                           json={"plazo_retencion_anios": 0})
        assert res.status_code == 422

    def test_update_retencion_plazo_negativo_rechazado(self, client):
        res = client.patch("/api/admin/retencion/tipos/1",
                           json={"plazo_retencion_anios": -5})
        assert res.status_code == 422

    def test_update_retencion_mas_de_100_rechazado(self, client):
        res = client.patch("/api/admin/retencion/tipos/1",
                           json={"plazo_retencion_anios": 101})
        assert res.status_code == 422

    def test_update_retencion_tipo_inexistente_retorna_404(self, client):
        with patch("routes.admin.retention.db_query", return_value=None):
            res = client.patch("/api/admin/retencion/tipos/999",
                               json={"plazo_retencion_anios": 5})
        assert res.status_code == 404

    def test_get_vencimientos(self, client):
        v = _row(id_archivo=1, titulo="Doc viejo", autor="A",
                 fecha_documento="2010-01-01", ubicacion="Estante 1",
                 soporte="Físico", tipo_documento="Acta",
                 plazo_anios=5, fecha_vencimiento="2015-01-01", dias_vencido=3650)
        with patch("routes.admin.retention.db_query", return_value=[v]):
            res = client.get("/api/admin/retencion/vencimientos")
        assert res.status_code == 200
        body = res.json()
        assert body["total"] == 1
        assert body["vencimientos"][0]["titulo"] == "Doc viejo"


# =============================================================================
# Papelera
# =============================================================================

class TestPapelera:
    def test_list_papelera_archivo(self, client):
        count = _row(total=1)
        doc   = _row(id=10, titulo="Borrado", autor="A", doc_type="Acta",
                     fecha="2020-01-01", deleted_at="2024-06-01 10:00", deleted_by="admin")
        call_n = [0]
        def mock_q(sql, params=None, fetch="all", commit=False):
            call_n[0] += 1
            return count if call_n[0] == 1 else [doc]
        with patch("routes.trash.db_query", side_effect=mock_q):
            res = client.get("/api/admin/papelera?modulo=Archivo")
        assert res.status_code == 200
        body = res.json()
        assert body["total"] == 1
        assert body["records"][0]["titulo"] == "Borrado"

    def test_list_papelera_modulo_invalido(self, client):
        res = client.get("/api/admin/papelera?modulo=Invalido")
        assert res.status_code == 400

    def test_list_papelera_paginacion(self, client):
        count = _row(total=50)
        def mock_q(sql, params=None, fetch="all", commit=False):
            return count if "COUNT" in sql else []
        with patch("routes.trash.db_query", side_effect=mock_q):
            res = client.get("/api/admin/papelera?modulo=Archivo&page=2&per_page=10")
        assert res.status_code == 200
        body = res.json()
        assert body["page"] == 2
        assert body["per_page"] == 10


# =============================================================================
# Audit log
# =============================================================================

class TestAuditLog:
    def test_audit_log_paginado(self, client):
        count = _row(total=2)
        evt   = _row(id=1, usuario="admin", evento="Login", modulo="Archivo",
                     detalle="OK", resultado="Success", timestamp="2024-01-01 10:00:00")
        call_n = [0]
        def mock_q(sql, params=None, fetch="all", commit=False):
            call_n[0] += 1
            return count if call_n[0] == 1 else [evt, evt]
        with patch("routes.admin.catalog.db_query", side_effect=mock_q):
            res = client.get("/api/admin/audit_log?page=1&per_page=50")
        assert res.status_code == 200
        body = res.json()
        assert body["total"] == 2
        assert len(body["records"]) == 2

    def test_audit_log_busqueda(self, client):
        count = _row(total=0)
        def mock_q(sql, params=None, fetch="all", commit=False):
            return count if "COUNT" in sql else []
        with patch("routes.admin.catalog.db_query", side_effect=mock_q):
            res = client.get("/api/admin/audit_log?search=admin")
        assert res.status_code == 200
        assert res.json()["total"] == 0


# =============================================================================
# Notificaciones
# =============================================================================

class TestNotifications:
    def test_notifications_archivo(self, client):
        notif = _row(id=1, label="Doc en revisión", status="revision",
                     modulo="Archivo", ts="2024-01-01 10:00")
        with patch("routes.admin.catalog.db_query", return_value=[notif]):
            res = client.get("/api/admin/notifications?modulo=Archivo")
        assert res.status_code == 200
        body = res.json()
        assert body["total"] >= 1
        assert "revision" in body["counts"]

    def test_notifications_sin_modulo(self, client):
        notif = _row(id=1, label="Doc", status="draft", modulo="Archivo", ts="2024-01-01 10:00")
        with patch("routes.admin.catalog.db_query", return_value=[notif]):
            res = client.get("/api/admin/notifications")
        assert res.status_code == 200
