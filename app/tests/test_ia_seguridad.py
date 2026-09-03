"""Pruebas de seguridad del carril D1-ia-backend (`docs/auditoria/sistema-ia-paginas.md`).

Cubren, sobre todo, las dos fichas urgentes del encargo:

- SI-004: `_open_conversation()` aceptaba cualquier `conversacion_id` que
  EXISTIERA, sin comprobar que fuera del usuario de la sesión. Como
  `ctx["conversacion_id"]` se ancla ahí y `mis_adjuntos` filtra precisamente
  por ese id, un usuario con sesión podía leer los adjuntos —nombre y
  `file_url`— de la conversación de otra persona con solo mandar su id.
- SI-005: `current_model()` descartaba en silencio cualquier slug que no
  empezara por "mistralai/", así que el panel guardaba el modelo elegido,
  decía "Guardado", y el chat seguía hablando con el barato por defecto.

Y, de paso, las correcciones menores que se hicieron en el mismo carril:
SI-009 (rechazar propuesta sin permiso), SI-010 (bandeja de propuestas sin
acotar por módulo/dueño), SI-011 (config/gastos/modelos sin exigir Global),
SI-022 (aprobar dos veces en una carrera) y SI-023 (aprobar sobre un
documento borrado).

Estas pruebas mockean `db_query` allí donde cada módulo lo importa — no hay
conexión real a Postgres.
"""
from unittest.mock import MagicMock, patch

import pytest


def _fila(**data):
    """Simula una fila devuelta por `db_query` (soporta `[]`, `.get()`, iteración)."""
    row = MagicMock()
    row.__getitem__ = lambda self, k: data[k]
    row.get = lambda k, default=None: data.get(k, default)
    row.keys = lambda: data.keys()
    row.__iter__ = lambda self: iter(data)
    return row


# =============================================================================
# SI-004 — la conversación reabierta tiene que ser del usuario de la sesión
# =============================================================================

class TestSI004ConversacionAjena:
    def test_conv_id_de_otro_usuario_no_se_reutiliza(self):
        """Bob manda el id de la conversación de Ana: se abre una NUEVA, no la
        de Ana — que es justo lo que hacía posible leer sus adjuntos."""
        from routes import ai as ai_routes

        with patch.object(ai_routes, "db_query") as mock_db:
            # La consulta con "AND usuario = %s" no encuentra fila (la 5 es de
            # Ana, no de Bob) -> primera llamada None; la segunda es el INSERT
            # que crea la conversación nueva.
            mock_db.side_effect = [None, {"id": 999}]
            resultado = ai_routes._open_conversation(
                5, {"canal": "web", "perfil": "consulta", "usuario": "bob",
                    "titulo": "hola", "modelo": "x"})

        assert resultado == 999
        assert resultado != 5
        # La primera consulta comprobó dueño, no solo existencia.
        primera_llamada = mock_db.call_args_list[0]
        assert "usuario = %s" in primera_llamada.args[0]
        assert primera_llamada.args[1] == [5, "bob"]

    def test_conv_id_propio_si_se_reutiliza(self):
        from routes import ai as ai_routes

        with patch.object(ai_routes, "db_query", return_value={"id": 5}) as mock_db:
            resultado = ai_routes._open_conversation(
                5, {"usuario": "ana"})

        assert resultado == 5
        mock_db.assert_called_once()

    def test_sin_sesion_nunca_reutiliza_aunque_el_id_exista(self):
        """Perfil público (sin usuario): jamás debe intentar leer la conversación
        de otro con solo mandar un id — se crea una nueva siempre."""
        from routes import ai as ai_routes

        with patch.object(ai_routes, "db_query") as mock_db:
            mock_db.return_value = {"id": 777}  # la INSERT
            resultado = ai_routes._open_conversation(
                5, {"canal": "web", "perfil": "publico", "usuario": None,
                    "titulo": "hola", "modelo": "x"})

        assert resultado == 777
        # Solo se llamó una vez: el INSERT. Nunca se intentó leer el id ajeno.
        assert mock_db.call_count == 1
        assert "INSERT INTO public.ia_conversaciones" in mock_db.call_args.args[0]

    def test_mis_adjuntos_no_alcanzables_via_conversacion_ajena(self):
        """Fin a fin: aunque Bob intente forzar `ctx['conversacion_id']` al hilo
        de Ana, `_open_conversation` ya se lo impidió, así que la herramienta
        `mis_adjuntos` -que filtra por ese id- nunca ve los adjuntos de Ana."""
        from core import ai_tools

        with patch.object(ai_tools, "db_query", return_value=[
            {"id": 1, "nombre_archivo": "reposo_medico_ana.pdf",
             "file_url": "/api/files/ia/x", "tamano_bytes": 100, "subido": "2026-01-01"},
        ]) as mock_db:
            # conversacion_id=999 es la que _open_conversation abrió de VERDAD
            # para Bob (nunca la 5 de Ana), así que el filtro por conversación
            # ya hace su trabajo: esto solo confirma que sigue filtrando por id.
            ai_tools._my_attachments({}, {"conversacion_id": 999})

        assert "WHERE conversacion_id = %s" in mock_db.call_args.args[0]
        assert mock_db.call_args.args[1] == [999]


# =============================================================================
# SI-005 — el modelo que se guarda es el que se usa, sin filtro silencioso
# =============================================================================

class TestSI005ModeloGuardadoEsElUsado:
    def test_modelo_no_mistralai_ya_no_se_descarta(self):
        from core import ai as ai_core

        with patch.object(ai_core, "_de_bd", return_value="anthropic/claude-haiku-4.5"):
            assert ai_core.current_model() == "anthropic/claude-haiku-4.5"

    def test_modelo_mistralai_sigue_funcionando(self):
        from core import ai as ai_core

        with patch.object(ai_core, "_de_bd", return_value="mistralai/ministral-3b-2512"):
            assert ai_core.current_model() == "mistralai/ministral-3b-2512"

    def test_sin_configuracion_usa_env_o_defecto(self):
        from core import ai as ai_core

        with patch.object(ai_core, "_de_bd", return_value=None), \
             patch.object(ai_core, "_env", return_value=ai_core.MODELO_POR_DEFECTO):
            assert ai_core.current_model() == ai_core.MODELO_POR_DEFECTO

    def test_converse_llama_al_modelo_configurado_no_al_defecto(self):
        """El ciclo de conversación pide `current_model()` en cada vuelta: si el
        administrador guardó un modelo caro-pero-elegido, `_post` debe recibir
        ESE slug, no el barato por defecto."""
        from core import ai as ai_core

        modelos_pedidos = []

        def _post_falso(cuerpo):
            modelos_pedidos.append(cuerpo["model"])
            return {
                "usage": {"total_tokens": 10, "cost": 0.001},
                "model": cuerpo["model"],
                "choices": [{"message": {"content": "hola", "tool_calls": None}}],
            }

        with patch.object(ai_core, "_de_bd", return_value="anthropic/claude-haiku-4.5"), \
             patch.object(ai_core, "_post", side_effect=_post_falso):
            r = ai_core.converse("system", [{"role": "user", "content": "hola"}],
                                 {"perfil": "publico"}, lambda *a: {}, lambda ctx: [])

        assert modelos_pedidos == ["anthropic/claude-haiku-4.5"]
        assert r["modelo"] == "anthropic/claude-haiku-4.5"


# =============================================================================
# SI-009 — rechazar una propuesta exige el mismo cerrojo que aprobarla
# =============================================================================

class TestSI009Rechazar:
    def test_reject_exige_modulo_de_la_propuesta(self):
        from core import ai_proposals

        with patch.object(ai_proposals, "db_query",
                          return_value=_fila(estado="pendiente", modulo="rrhh")):
            with pytest.raises(ai_proposals.PropuestaError):
                ai_proposals.reject(1, "usuario_x", modulos={"archivo"})

    def test_reject_funciona_con_el_modulo_correcto(self):
        from core import ai_proposals

        filas = [_fila(estado="pendiente", modulo="archivo"), _fila(id=1)]
        with patch.object(ai_proposals, "db_query", side_effect=filas), \
             patch.object(ai_proposals, "log_event"):
            r = ai_proposals.reject(1, "usuario_x", modulos={"archivo"})
        assert r["estado"] == "rechazada"

    def test_endpoint_rechazar_exige_perfil_editor(self, client_as):
        from routes import ai as ai_routes

        c = client_as("normal_archivo")
        with patch.object(ai_routes, "db_query",
                          return_value=_fila(usuario="normal_archivo",
                                             nombre_usuario="N", modulo="Archivo", rol="Normal")):
            res = c.post("/api/ia/propuesta/1/rechazar")
        assert res.status_code == 403


# =============================================================================
# SI-010 — la bandeja de propuestas se acota a módulo (y dueño, salvo Global)
# =============================================================================

class TestSI010BandejaAcotada:
    def test_modulos_vacio_no_consulta_la_base(self):
        from core import ai_proposals

        with patch.object(ai_proposals, "db_query") as mock_db:
            r = ai_proposals.list_proposals(estado="aprobada", modulos=set())
        assert r == []
        mock_db.assert_not_called()

    def test_filtra_por_modulo_y_por_usuario_si_no_es_global(self):
        from core import ai_proposals

        with patch.object(ai_proposals, "db_query", return_value=[]) as mock_db:
            ai_proposals.list_proposals(estado="aprobada", modulos={"archivo"},
                                        usuario="ana")
        sql, params = mock_db.call_args.args[0], mock_db.call_args.args[1]
        assert "modulo IN" in sql
        assert "usuario = %s" in sql
        assert "archivo" in params and "ana" in params


# =============================================================================
# SI-011 — config/gastos/modelos exigen Global, no solo sesión
# =============================================================================

class TestSI011EndpointsGlobal:
    def test_gastos_rechaza_a_usuario_no_global(self, client_as):
        from routes import ai as ai_routes

        c = client_as("normal_archivo")
        with patch.object(ai_routes, "db_query",
                          return_value=_fila(usuario="normal_archivo",
                                             nombre_usuario="N", modulo="Archivo", rol="Admin")):
            res = c.get("/api/ia/gastos")
        assert res.status_code == 403

    def test_config_rechaza_a_usuario_no_global(self, client_as):
        from routes import ai as ai_routes

        c = client_as("normal_archivo")
        with patch.object(ai_routes, "db_query",
                          return_value=_fila(usuario="normal_archivo",
                                             nombre_usuario="N", modulo="Archivo", rol="Admin")):
            res = c.get("/api/ia/config")
        assert res.status_code == 403


# =============================================================================
# SI-022 — aprobar dos veces en una carrera no ejecuta el cambio dos veces
# =============================================================================

class TestSI022CarreraAlAprobar:
    def test_segunda_aprobacion_simultanea_no_ejecuta_nada(self):
        from core import ai_proposals

        p = _fila(id=1, estado="pendiente", modulo="archivo", accion="palabras_clave",
                  datos="{}", resumen="x")
        # 1) SELECT inicial -> pendiente y con módulo válido.
        # 2) UPDATE ... WHERE estado='pendiente' RETURNING id -> None: ya la
        #    reclamó la otra petición justo antes.
        with patch.object(ai_proposals, "db_query", side_effect=[p, None]):
            with pytest.raises(ai_proposals.PropuestaError):
                ai_proposals.approve(1, "usuario_x", modulos={"archivo"})


# =============================================================================
# SI-023 — aprobar no escribe sobre un documento borrado
# =============================================================================

class TestSI023DocumentoBorrado:
    def test_actualizar_sobre_documento_borrado_falla_y_no_revienta(self):
        from core import ai_proposals

        p = _fila(objetivo_id=42, modulo="archivo")
        with patch.object(ai_proposals, "allowed_fields", return_value={"titulo"}), \
             patch.object(ai_proposals, "_usuario_id", return_value=1), \
             patch.object(ai_proposals, "db_query", return_value=None) as mock_db:
            with pytest.raises(ai_proposals.PropuestaError):
                ai_proposals._actualizar(p, {"campos": {"titulo": "nuevo"}}, "usuario_x")
        assert "deleted_at IS NULL" in mock_db.call_args.args[0]

    def test_approve_completo_revierte_a_pendiente_si_el_documento_esta_borrado(self):
        """La propuesta se reclama, falla al ejecutar (documento borrado) y
        vuelve a quedar 'pendiente' en vez de marcarse 'aprobada' sin haber
        tocado nada."""
        from core import ai_proposals
        import json as _json

        p = _fila(id=1, estado="pendiente", modulo="archivo", accion="actualizar",
                  objetivo_id=42,
                  datos=_json.dumps({"campos": {"titulo": "nuevo"}}), resumen="x")

        llamadas = []

        def _db(sql, params=None, **kw):
            llamadas.append((sql, params))
            if sql.startswith("SELECT * FROM public.ia_propuestas"):
                return p
            if "SET estado = 'aprobada'" in sql:
                return {"id": 1}
            if sql.strip().startswith("UPDATE public.datos_archivo"):
                return None  # documento borrado: RETURNING no da fila
            if "SET estado = 'pendiente'" in sql:
                return None
            return None

        with patch.object(ai_proposals, "db_query", side_effect=_db), \
             patch.object(ai_proposals, "allowed_fields", return_value={"titulo"}), \
             patch.object(ai_proposals, "_usuario_id", return_value=1), \
             patch.object(ai_proposals, "log_event"):
            with pytest.raises(ai_proposals.PropuestaError):
                ai_proposals.approve(1, "usuario_x", modulos={"archivo"})

        # Se intentó revertir a pendiente tras el fallo.
        assert any("SET estado = 'pendiente'" in sql for sql, _ in llamadas)
