"""Tests para /api/choices (BA-101: exige sesión y segmenta por módulo;
BA-145: `?scope=` filtra el payload; caché en memoria con TTL)."""
from unittest.mock import patch


def _fila_modulo(modulo):
    return [{"modulo": modulo}]


class TestChoices:
    def test_sin_sesion_recibe_401(self, anon_client):
        res = anon_client.get("/api/choices")
        assert res.status_code == 401

    def test_usuario_de_archivo_no_recibe_datos_de_rrhh(self, client_as):
        c = client_as("archivo_normal")
        with patch("repos.lookups_repo.db_query") as mock_db:
            mock_db.side_effect = lambda sql, *a, **k: (
                _fila_modulo("Archivo") if "usuarios_sistema" in sql else []
            )
            import routes.lookups as ch
            ch.invalidate_choices_cache()
            res = c.get("/api/choices")

        assert res.status_code == 200
        body = res.json()
        assert "archivo" in body
        assert "rrhh" not in body

    def test_usuario_de_rrhh_no_recibe_datos_de_archivo(self, client_as):
        c = client_as("rrhh_normal")
        with patch("repos.lookups_repo.db_query") as mock_db:
            mock_db.side_effect = lambda sql, *a, **k: (
                _fila_modulo("RRHH") if "usuarios_sistema" in sql else []
            )
            import routes.lookups as ch
            ch.invalidate_choices_cache()
            res = c.get("/api/choices")

        assert res.status_code == 200
        body = res.json()
        assert "rrhh" in body
        assert "archivo" not in body

    def test_usuario_global_recibe_ambos_modulos(self, client_as):
        c = client_as("global_admin")
        with patch("repos.lookups_repo.db_query") as mock_db:
            mock_db.side_effect = lambda sql, *a, **k: (
                _fila_modulo("Global") if "usuarios_sistema" in sql else []
            )
            import routes.lookups as ch
            ch.invalidate_choices_cache()
            res = c.get("/api/choices")

        assert res.status_code == 200
        body = res.json()
        assert "archivo" in body
        assert "rrhh"    in body
        assert "doc_types" in body["archivo"]
        assert "tesauro"   in body["archivo"]
        assert "min_date"  in body["archivo"]
        assert "max_date"  in body["archivo"]
        assert "doc_types" in body["rrhh"]
        assert "estados"   in body["rrhh"]
        assert "people"    in body["rrhh"]

    def test_scope_archivo_omite_rrhh_aunque_el_usuario_tenga_ambos_modulos(self, client_as):
        c = client_as("global_admin")
        with patch("repos.lookups_repo.db_query") as mock_db:
            mock_db.side_effect = lambda sql, *a, **k: (
                _fila_modulo("Global") if "usuarios_sistema" in sql else []
            )
            import routes.lookups as ch
            ch.invalidate_choices_cache()
            res = c.get("/api/choices?scope=archivo")

        assert res.status_code == 200
        body = res.json()
        assert "archivo" in body
        assert "rrhh" not in body

    def test_cache_activo_no_repite_las_consultas_de_datos(self, client_as):
        """Segunda llamada dentro del TTL no vuelve a construir el payload."""
        c = client_as("global_admin")
        with patch("repos.lookups_repo.db_query") as mock_db:
            mock_db.side_effect = lambda sql, *a, **k: (
                _fila_modulo("Global") if "usuarios_sistema" in sql else []
            )
            import routes.lookups as ch
            ch.invalidate_choices_cache()

            res1 = c.get("/api/choices")
            calls_after_first = mock_db.call_count
            res2 = c.get("/api/choices")

        assert res1.status_code == 200
        assert res2.status_code == 200
        # La segunda llamada sólo consulta el módulo del usuario, no reconstruye
        # el payload completo desde la base.
        assert mock_db.call_count < calls_after_first * 2

        ch.invalidate_choices_cache()
