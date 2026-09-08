"""Pruebas del carril SI-228-230-231-test-ia: ciclo de propuestas y herramientas de IA.

Cubren los bugs reales arreglados hoy en el barrido de terceros pases sobre el módulo de
IA (`app/core/ai.py`, `app/core/ai_tools.py`, `app/core/ai_prompts.py`,
`app/core/ai_proposals.py`, `app/routes/ai.py`) que quedaron documentados como sin test
dedicado:

- El bug de integridad de auditoría en `ai_proposals.approve()`: antes solo se revertía a
  "pendiente" cuando la ejecución lanzaba `PropuestaError`; cualquier otro fallo (corte de
  conexión, dato con forma inesperada) dejaba la fila marcada "aprobada" -con
  `resuelto_por`/`resuelto_at` ya escritos- sin que el cambio se hubiera aplicado de verdad.
  Se amplió el `except` a `Exception` (commit 9562873).
- SI-022 (ya resuelto, sin test dedicado hasta ahora): aprobar dos veces la misma propuesta
  no la ejecuta dos veces, porque el `UPDATE ... WHERE estado='pendiente' RETURNING` la
  reclama antes de ejecutar cualquier cambio real.
- SI-018 (`ai_tools.execute`): un resultado exitoso se marca con `_advertencia` para que el
  contenido de un documento no se confunda con una instrucción de sistema; un resultado con
  `error` no lleva esa marca (commit 4b2c62f).
- SI-121 (`routes/ai.py POST /chat`): la respuesta del chat trae `gasto_hoy`/`tope_diario`
  en cada turno, no solo al cargar la página (commit eb0450b).

Sigue el mismo patrón de mocking que `test_ia_seguridad.py`: se mockea `db_query` allí
donde cada módulo lo importa, sin conexión real a Postgres.
"""
import json
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
# ai_proposals.approve() — revertir a 'pendiente' ante CUALQUIER excepción
# =============================================================================

class TestApproveRevierteAntesCualquierExcepcion:
    def test_fallo_generico_durante_la_ejecucion_revierte_a_pendiente(self):
        """Regresión del bug de integridad de auditoría arreglado hoy (commit 9562873):
        antes `approve()` solo capturaba `PropuestaError` en el `except` que revierte a
        'pendiente'. Un fallo de otro tipo (aquí, un `RuntimeError` simulando un corte de
        conexión a mitad del INSERT) dejaba la propuesta marcada 'aprobada', con
        resuelto_por/resuelto_at ya escritos, sin que el cambio se hubiera aplicado."""
        from core import ai_proposals

        p = _fila(id=1, estado="pendiente", modulo="archivo", accion="palabras_clave",
                  objetivo_id=42, datos=json.dumps({"palabras": ["urgente"]}), resumen="x")

        llamadas = []

        def _db(sql, params=None, **kw):
            llamadas.append((sql, params))
            if sql.strip().startswith("SELECT * FROM public.ia_propuestas"):
                return p
            if "SET estado = 'aprobada'" in sql:
                return {"id": 1}
            if sql.strip().startswith("INSERT INTO public.descriptores_libres"):
                # Simula un fallo que NO es un PropuestaError: un corte de conexión
                # a mitad de la operación real de escritura.
                raise RuntimeError("conexión perdida a mitad del INSERT")
            if "SET estado = 'pendiente'" in sql:
                return None
            return None

        with patch.object(ai_proposals, "db_query", side_effect=_db):
            # Antes del fix esto NO lanzaba RuntimeError hacia afuera de forma
            # correcta con la fila revertida; lo importante es que la reversión
            # ocurra igual que con un PropuestaError.
            with pytest.raises(RuntimeError):
                ai_proposals.approve(1, "usuario_x", modulos={"archivo"})

        # La propuesta se intentó revertir a 'pendiente' pese a que el fallo no fue
        # un PropuestaError -- éste es el corazón de la regresión.
        reversiones = [sql for sql, _ in llamadas if "SET estado = 'pendiente'" in sql]
        assert len(reversiones) == 1
        # Y la reversión limpia resuelto_por/resuelto_at, no solo el estado.
        assert "resuelto_por = NULL" in reversiones[0]
        assert "resuelto_at = NULL" in reversiones[0]

    def test_propuesta_error_tambien_revierte_a_pendiente(self):
        """El caso ya cubierto antes del fix (SI-023) sigue funcionando: un
        `PropuestaError` explícito durante la ejecución también revierte."""
        from core import ai_proposals

        p = _fila(id=1, estado="pendiente", modulo="archivo", accion="actualizar",
                  objetivo_id=42, datos=json.dumps({"campos": {"titulo": "nuevo"}}),
                  resumen="x")

        llamadas = []

        def _db(sql, params=None, **kw):
            llamadas.append((sql, params))
            if sql.strip().startswith("SELECT * FROM public.ia_propuestas"):
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
             patch.object(ai_proposals, "_usuario_id", return_value=1):
            with pytest.raises(ai_proposals.PropuestaError):
                ai_proposals.approve(1, "usuario_x", modulos={"archivo"})

        assert any("SET estado = 'pendiente'" in sql for sql, _ in llamadas)

    def test_ejecucion_exitosa_no_revierte_nada(self):
        """Contraprueba: si la ejecución SÍ funciona, no se llama a la reversión y la
        propuesta queda 'aprobada' de verdad."""
        from core import ai_proposals

        p = _fila(id=1, estado="pendiente", modulo="archivo", accion="palabras_clave",
                  objetivo_id=42, datos=json.dumps({"palabras": ["urgente"]}), resumen="x")

        llamadas = []

        def _db(sql, params=None, **kw):
            llamadas.append((sql, params))
            if sql.strip().startswith("SELECT * FROM public.ia_propuestas"):
                return p
            if "SET estado = 'aprobada'" in sql:
                return {"id": 1}
            if sql.strip().startswith("INSERT INTO public.descriptores_libres"):
                return None
            if sql.strip().startswith("SELECT id_descriptor"):
                return {"id_descriptor": 5}
            if sql.strip().startswith("INSERT INTO public.archivo_descriptores"):
                return None
            return None

        with patch.object(ai_proposals, "db_query", side_effect=_db), \
             patch.object(ai_proposals, "log_event"):
            r = ai_proposals.approve(1, "usuario_x", modulos={"archivo"})

        assert r["estado"] == "aprobada"
        assert not any("SET estado = 'pendiente'" in sql for sql, _ in llamadas)


# =============================================================================
# SI-022 — aprobar dos veces la misma propuesta no la ejecuta dos veces
# =============================================================================

class TestAprobarDosVecesNoEjecutaDosVeces:
    def test_segunda_aprobacion_no_repite_la_escritura_real(self):
        """El UPDATE ... WHERE estado='pendiente' RETURNING la reclama antes de
        ejecutar: una segunda llamada a approve() sobre la misma propuesta (ya
        'aprobada' en la base) no debe volver a escribir el cambio real."""
        from core import ai_proposals

        p = _fila(id=1, estado="pendiente", modulo="archivo", accion="palabras_clave",
                  objetivo_id=42, datos=json.dumps({"palabras": ["urgente"]}), resumen="x")
        p_ya_aprobada = _fila(id=1, estado="aprobada", modulo="archivo",
                              accion="palabras_clave", objetivo_id=42,
                              datos=json.dumps({"palabras": ["urgente"]}), resumen="x")

        ejecuciones_reales = []

        def _db(sql, params=None, **kw):
            if sql.strip().startswith("SELECT * FROM public.ia_propuestas"):
                # Primera llamada: pendiente. Segunda: ya aprobada (así habría
                # quedado tras la primera aprobación real).
                return p if not ejecuciones_reales else p_ya_aprobada
            if "SET estado = 'aprobada'" in sql:
                return {"id": 1}
            if sql.strip().startswith("INSERT INTO public.descriptores_libres"):
                ejecuciones_reales.append(sql)
                return None
            if sql.strip().startswith("SELECT id_descriptor"):
                return {"id_descriptor": 5}
            if sql.strip().startswith("INSERT INTO public.archivo_descriptores"):
                return None
            return None

        with patch.object(ai_proposals, "db_query", side_effect=_db), \
             patch.object(ai_proposals, "log_event"):
            ai_proposals.approve(1, "usuario_x", modulos={"archivo"})
            # Segunda aprobación: el SELECT ya devuelve 'aprobada', así que ni
            # siquiera llega a reclamarla -- falla en el chequeo de estado.
            with pytest.raises(ai_proposals.PropuestaError):
                ai_proposals.approve(1, "usuario_x", modulos={"archivo"})

        assert len(ejecuciones_reales) == 1

    def test_carrera_simultanea_el_returning_vacio_impide_la_segunda_ejecucion(self):
        """La otra forma de la misma carrera (ya cubierta también en
        test_ia_seguridad.py, SI-022): dos peticiones leen 'pendiente' casi a la
        vez, pero solo una gana el UPDATE ... RETURNING; la otra nunca llega a
        ejecutar nada."""
        from core import ai_proposals

        p = _fila(id=1, estado="pendiente", modulo="archivo", accion="palabras_clave",
                  datos="{}", resumen="x")
        # 1) SELECT inicial -> pendiente. 2) UPDATE ... RETURNING -> None: la otra
        #    petición ya la reclamó justo antes.
        with patch.object(ai_proposals, "db_query", side_effect=[p, None]):
            with pytest.raises(ai_proposals.PropuestaError):
                ai_proposals.approve(1, "usuario_x", modulos={"archivo"})


# =============================================================================
# SI-018 — execute() marca los resultados exitosos como datos, no instrucciones
# =============================================================================

class TestSI018AdvertenciaEnResultados:
    def test_resultado_exitoso_lleva_advertencia(self):
        from core import ai_tools

        with patch.object(ai_tools, "db_query", return_value={
                "documentos_archivo": 10, "archivo_digitalizados": 5, "documentos_rrhh": 2,
                "empleados": 3, "palabras_clave": 7, "anio_mas_antiguo": 1990,
                "anio_mas_reciente": 2026}):
            resultado = ai_tools.execute("estadisticas", {}, {"perfil": "publico", "modulos": set()})

        assert "_advertencia" in resultado
        assert "DATOS" in resultado["_advertencia"]

    def test_resultado_con_error_no_lleva_advertencia(self):
        from core import ai_tools

        # ver_documento sin un id numérico válido devuelve {"error": ...} sin
        # siquiera tocar la base.
        resultado = ai_tools.execute("ver_documento", {"id": "no-es-un-numero"},
                                     {"perfil": "publico", "modulos": set()})

        assert "error" in resultado
        assert "_advertencia" not in resultado

    def test_advertencia_no_pisa_una_clave_que_ya_trae_el_resultado(self):
        """`setdefault`: si alguna herramienta alguna vez trajera su propia
        `_advertencia`, execute() no debe sobreescribirla."""
        from core import ai_tools

        handler_falso = MagicMock(return_value={"ok": True, "_advertencia": "propia"})
        entrada_falsa = {"nombre": "herramienta_de_prueba", "perfil": "publico",
                         "modulo": None, "esquema": {}, "manejador": handler_falso}

        with patch.object(ai_tools, "_REGISTRO", ai_tools._REGISTRO + [entrada_falsa]):
            resultado = ai_tools.execute("herramienta_de_prueba", {},
                                         {"perfil": "publico", "modulos": set()})

        assert resultado["_advertencia"] == "propia"


# =============================================================================
# SI-121 — POST /api/ia/chat devuelve gasto_hoy/tope_diario en cada respuesta
# =============================================================================

class TestSI121GastoEnCadaRespuestaDeChat:
    def test_chat_devuelve_gasto_hoy_y_tope_diario(self):
        from routes import ai as ai_routes

        def _db(sql, params=None, **kw):
            if "SUM(costo)" in sql:
                return {"total": 2.5}
            if sql.strip().startswith("INSERT INTO public.ia_conversaciones"):
                return {"id": 1}
            return None  # INSERT/UPDATE de ia_mensajes y ia_conversaciones (fetch=none)

        with patch.object(ai_routes, "db_query", side_effect=_db), \
             patch.object(ai_routes.ai, "status", return_value={"disponible": True}), \
             patch.object(ai_routes.ai, "sanitize_history",
                          return_value=[{"role": "user", "content": "hola"}]), \
             patch.object(ai_routes.ai, "MAX_HISTORIAL",
                          {"publico": 20, "consulta": 20, "editor": 20}), \
             patch.object(ai_routes.ai, "daily_limit", return_value=5.0), \
             patch.object(ai_routes.ai, "current_model", return_value="modelo-x"), \
             patch.object(ai_routes.ai, "converse", return_value={
                 "respuesta": "hola de vuelta",
                 "uso": {"total_tokens": 12, "costo": 0.002},
                 "modelo": "modelo-x", "herramientas": [],
             }), \
             patch.object(ai_routes.ai_proposals, "list_proposals", return_value=[]):
            respuesta = ai_routes.chat({"mensajes": [{"role": "user", "content": "hola"}]},
                                       usuario=None)

        assert respuesta["gasto_hoy"] == 2.5
        assert respuesta["tope_diario"] == 5.0

    def test_chat_no_revienta_si_falla_el_calculo_de_gasto_al_final(self):
        """`gasto_hoy`/`tope_diario` van en un try/except a propósito: un fallo al
        recalcular el gasto DESPUÉS de haber respondido no debe tirar abajo una
        respuesta que ya se generó bien."""
        from routes import ai as ai_routes

        llamadas_gasto = {"n": 0}

        def _db(sql, params=None, **kw):
            if "SUM(costo)" in sql:
                llamadas_gasto["n"] += 1
                if llamadas_gasto["n"] == 1:
                    return {"total": 1.0}  # chequeo de tope al inicio: pasa
                raise RuntimeError("la base no responde")  # cálculo final: falla
            if sql.strip().startswith("INSERT INTO public.ia_conversaciones"):
                return {"id": 1}
            return None

        with patch.object(ai_routes, "db_query", side_effect=_db), \
             patch.object(ai_routes.ai, "status", return_value={"disponible": True}), \
             patch.object(ai_routes.ai, "sanitize_history",
                          return_value=[{"role": "user", "content": "hola"}]), \
             patch.object(ai_routes.ai, "MAX_HISTORIAL",
                          {"publico": 20, "consulta": 20, "editor": 20}), \
             patch.object(ai_routes.ai, "daily_limit", return_value=5.0), \
             patch.object(ai_routes.ai, "current_model", return_value="modelo-x"), \
             patch.object(ai_routes.ai, "converse", return_value={
                 "respuesta": "hola de vuelta",
                 "uso": {"total_tokens": 12, "costo": 0.002},
                 "modelo": "modelo-x", "herramientas": [],
             }), \
             patch.object(ai_routes.ai_proposals, "list_proposals", return_value=[]):
            respuesta = ai_routes.chat({"mensajes": [{"role": "user", "content": "hola"}]},
                                       usuario=None)

        assert respuesta["respuesta"] == "hola de vuelta"
        assert "gasto_hoy" not in respuesta


# =============================================================================
# _crear() — la acción "crear" de una propuesta de IA (INSERT), sin test hasta
# ahora: ni el caso Archivo, ni el caso RRHH (con y sin empleado_id), ni su
# paso completo por approve().
# =============================================================================

class TestCrearPropuesta:
    def test_crear_en_archivo_inserta_con_columnas_permitidas(self):
        """El caso simple: sólo columnas de la lista blanca + creado_por, sin la
        rama de empleado_id que es exclusiva de RRHH."""
        from core import ai_proposals

        p = _fila(modulo="archivo", objetivo_id=None)
        datos = {"campos": {"titulo": "Acta 2026", "autor": "Comisión"}}

        capturado = {}

        def _db(sql, params=None, **kw):
            if sql.strip().startswith("SELECT id FROM public.usuarios_sistema"):
                return _fila(id=7)
            if sql.strip().startswith("INSERT INTO public.datos_archivo"):
                capturado["sql"] = sql
                capturado["params"] = params
                return {"id": 99}
            return None

        with patch.object(ai_proposals, "db_query", side_effect=_db), \
             patch.object(ai_proposals, "allowed_fields", return_value={"titulo", "autor"}):
            resultado = ai_proposals._crear(p, datos, "usuario_x")

        assert "id 99" in resultado
        # columnas ordenadas alfabéticamente: autor, titulo, creado_por al final
        assert "autor, titulo, creado_por" in capturado["sql"]
        assert capturado["params"] == ["Comisión", "Acta 2026", 7]

    def test_crear_en_rrhh_exige_empleado_id(self):
        """RRHH sin `extra.empleado_id` no debe insertar nada: no sabe de quién
        es el documento."""
        from core import ai_proposals

        p = _fila(modulo="rrhh", objetivo_id=None)
        datos = {"campos": {"notas": "Consignación"}, "extra": {}}

        with patch.object(ai_proposals, "db_query") as mock_db, \
             patch.object(ai_proposals, "allowed_fields", return_value={"notas"}):
            with pytest.raises(ai_proposals.PropuestaError):
                ai_proposals._crear(p, datos, "usuario_x")
            mock_db.assert_not_called()

    def test_crear_en_rrhh_con_empleado_id_lo_agrega_al_insert(self):
        """`empleado_id` no está en la lista blanca de modificables (no se
        mueve un documento entre expedientes editando), pero al crear es
        obligatorio y se añade aparte, ya resuelto contra la tabla de
        empleados en el momento de proponer."""
        from core import ai_proposals

        p = _fila(modulo="rrhh", objetivo_id=None)
        datos = {"campos": {"notas": "Consignación"}, "extra": {"empleado_id": 55}}

        capturado = {}

        def _db(sql, params=None, **kw):
            if sql.strip().startswith("SELECT id FROM public.usuarios_sistema"):
                return _fila(id=3)
            if sql.strip().startswith("INSERT INTO public.datos_rrhh"):
                capturado["sql"] = sql
                capturado["params"] = params
                return {"id": 12}
            return None

        with patch.object(ai_proposals, "db_query", side_effect=_db), \
             patch.object(ai_proposals, "allowed_fields", return_value={"notas"}):
            resultado = ai_proposals._crear(p, datos, "usuario_x")

        assert "id 12" in resultado
        assert "notas, empleado_id, creado_por" in capturado["sql"]
        assert capturado["params"] == ["Consignación", 55, 3]

    def test_campo_vacio_se_guarda_como_null(self):
        """Un string vacío en un campo revienta un INSERT tipado (fecha, entero);
        NULL no. `_crear` lo normaliza igual que `_sql_campos` lo hace para
        `_actualizar`."""
        from core import ai_proposals

        p = _fila(modulo="archivo", objetivo_id=None)
        datos = {"campos": {"titulo": "Acta", "fecha_documento": ""}}

        capturado = {}

        def _db(sql, params=None, **kw):
            if sql.strip().startswith("SELECT id FROM public.usuarios_sistema"):
                return _fila(id=1)
            if sql.strip().startswith("INSERT INTO public.datos_archivo"):
                capturado["params"] = params
                return {"id": 1}
            return None

        with patch.object(ai_proposals, "db_query", side_effect=_db), \
             patch.object(ai_proposals, "allowed_fields",
                          return_value={"titulo", "fecha_documento"}):
            ai_proposals._crear(p, datos, "usuario_x")

        # columnas ordenadas alfabéticamente: fecha_documento, titulo, creado_por
        assert capturado["params"] == [None, "Acta", 1]

    def test_approve_con_accion_crear_llega_hasta_el_insert(self):
        """El recorrido completo desde `approve()`: reclama la propuesta,
        despacha a `_crear()` y marca 'aprobada' de verdad."""
        from core import ai_proposals

        p = _fila(id=9, estado="pendiente", modulo="archivo", accion="crear",
                  objetivo_id=None, datos=json.dumps({"campos": {"titulo": "Nueva"}}),
                  resumen="crear documento")

        def _db(sql, params=None, **kw):
            if sql.strip().startswith("SELECT * FROM public.ia_propuestas"):
                return p
            if "SET estado = 'aprobada'" in sql:
                return {"id": 9}
            if sql.strip().startswith("SELECT id FROM public.usuarios_sistema"):
                return _fila(id=2)
            if sql.strip().startswith("INSERT INTO public.datos_archivo"):
                return {"id": 123}
            return None

        with patch.object(ai_proposals, "db_query", side_effect=_db), \
             patch.object(ai_proposals, "allowed_fields", return_value={"titulo"}), \
             patch.object(ai_proposals, "log_event"):
            r = ai_proposals.approve(9, "usuario_x", modulos={"archivo"})

        assert r["estado"] == "aprobada"
        assert "id 123" in r["detalle"]
