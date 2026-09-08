"""
Disposición documental (ISO 15489-1:2016 §8.5).

Motivación: el sistema avisaba de los documentos con el plazo de retención
vencido pero no ofrecía ninguna acción, así que la decisión archivística no
quedaba registrada en ninguna parte.

Lo importante de estas pruebas: disponer **no borra**. Registra qué se decidió,
quién y con qué acta — que es lo que un archivo tiene que poder demostrar años
después. Y una disposición ya registrada no se pisa en silencio.
"""
import sys
from pathlib import Path
from unittest.mock import patch

import pytest
from fastapi import HTTPException
from pydantic import ValidationError

APP = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(APP))

import routes.admin.retention as ret  # noqa: E402


def _doc(disposicion=None):
    return {"id_archivo": 7, "titulo": "Oficio DA-001", "disposicion": disposicion}


def test_solo_se_aceptan_disposiciones_conocidas():
    for valida in ("conservar", "transferido", "eliminado"):
        assert ret.DisposicionIn(disposicion=valida).disposicion == valida
    with pytest.raises(ValidationError):
        ret.DisposicionIn(disposicion="destruir_todo")


def test_registra_la_decision_sin_borrar_el_documento():
    ejecutadas = []

    def falso(sql, params=None, **k):
        ejecutadas.append(sql)
        return _doc() if "SELECT" in sql else None

    with patch.object(ret, "db_query", side_effect=falso), \
         patch.object(ret, "log_event"):
        r = ret.registrar_disposicion(7, ret.DisposicionIn(
            disposicion="transferido", acta="Acta 12/2026", requester="admin"),
            usuario_sesion="admin.global")

    assert r["success"] is True
    assert r["etiqueta"] == "Transferido al archivo histórico"
    escritura = [s for s in ejecutadas if "UPDATE" in s]
    assert len(escritura) == 1, "debe haber exactamente una escritura"
    assert "DELETE" not in " ".join(ejecutadas), "disponer nunca borra"
    assert "deleted_at" not in escritura[0], "disponer no es enviar a la papelera"


def test_no_se_puede_disponer_dos_veces():
    """Rectificar una disposición exige un acta nueva, no pisarla en silencio."""
    with patch.object(ret, "db_query", return_value=_doc("eliminado")), \
         patch.object(ret, "log_event"):
        with pytest.raises(HTTPException) as e:
            ret.registrar_disposicion(7, ret.DisposicionIn(disposicion="conservar"),
                                       usuario_sesion="admin.global")
        assert e.value.status_code == 409
        assert "Eliminado por expurgo" in e.value.detail


def test_documento_inexistente():
    with patch.object(ret, "db_query", return_value=None), \
         patch.object(ret, "log_event"):
        with pytest.raises(HTTPException) as e:
            ret.registrar_disposicion(999, ret.DisposicionIn(disposicion="conservar"),
                                       usuario_sesion="admin.global")
        assert e.value.status_code == 404


def test_la_decision_queda_en_auditoria():
    """Sin rastro no hay disposición que valga: es el punto de todo esto."""
    with patch.object(ret, "db_query", side_effect=lambda s, *a, **k: _doc() if "SELECT" in s else None), \
         patch.object(ret, "log_event") as reg:
        ret.registrar_disposicion(7, ret.DisposicionIn(
            disposicion="eliminado", acta="Acta 3/2026", requester="admin"),
            usuario_sesion="admin.global")
    reg.assert_called_once()
    assert "Disposición" in reg.call_args[0][1]
    assert "Acta 3/2026" in reg.call_args[0][3]


def test_ignora_el_requester_declarado_por_el_cliente():
    """IN-133: `requester` lo declara el cliente y no prueba nada -- la
    identidad real para `disposicion_por` y la auditoría sale de la sesión
    verificada (`usuario_sesion`), no del payload. Antes de este fix
    `requester="quien-sea"` habría quedado escrito tal cual en la base y en
    el log."""
    ejecutadas = []

    def falso(sql, params=None, **k):
        ejecutadas.append((sql, params))
        return _doc() if "SELECT" in sql else None

    with patch.object(ret, "db_query", side_effect=falso), \
         patch.object(ret, "log_event") as reg:
        ret.registrar_disposicion(7, ret.DisposicionIn(
            disposicion="conservar", requester="quien-sea"),
            usuario_sesion="admin.real")

    update_sql, update_params = next(s for s in ejecutadas if "UPDATE" in s[0])
    assert "admin.real" in update_params
    assert "quien-sea" not in update_params
    reg.assert_called_once()
    assert reg.call_args[0][0] == "admin.real"


def test_los_vencimientos_dejan_de_listar_lo_ya_dispuesto():
    """Si siguieran apareciendo, la lista no bajaría nunca y perdería sentido."""
    import inspect
    sql = inspect.getsource(ret.get_expired_docs)
    assert "da.disposicion IS NULL" in sql
    assert "da.deleted_at IS NULL" in sql


def test_vencimiento_respeta_la_fecha_explicita_si_existe():
    """OA-002: una fecha de vencimiento manual sobreescribe el plazo del tipo,
    tanto en el filtro (WHERE) como en lo que se muestra. Antes el cálculo
    ignoraba `fecha_vencimiento` por completo, así que el campo del formulario
    de alta/edición no tenía ningún efecto sobre qué se consideraba vencido —
    exactamente el bug que describía OA-002, y que dejaba al KPI de stats.py
    (ya corregido) diciendo una cifra distinta de la que esta tabla mostraba
    (OA-003)."""
    import inspect
    sql_archivo = inspect.getsource(ret.get_expired_docs)
    sql_rrhh = inspect.getsource(ret.get_expired_docs_rrhh)
    for sql, col in ((sql_archivo, "da"), (sql_rrhh, "dr")):
        # el COALESCE(fecha_vencimiento, calculado) aparece en el SELECT
        # mostrado y en el WHERE del filtro: al menos dos usos reales.
        assert sql.count(f"{col}.fecha_vencimiento") >= 2
        assert f"COALESCE(\n" in sql


def test_vencimientos_rrhh_usa_datos_rrhh_no_datos_archivo():
    """OR-183/OR-184: la pestaña de Retención de RRHH necesitaba su propio
    endpoint sobre `datos_rrhh` — antes reusaba el de Archivo (mostrando
    documentos institucionales bajo el encabezado de RRHH) o, en hr_alerts.py,
    un segundo endpoint que también apuntaba a `datos_archivo` por error
    (ya retirado). Este endpoint nuevo consulta la tabla correcta."""
    llamadas = []

    def falso(sql, params=None, **k):
        llamadas.append(sql)
        return []

    with patch.object(ret, "db_query", side_effect=falso):
        r = ret.get_expired_docs_rrhh(limite=10)

    assert r == {"total": 0, "vencimientos": []}
    assert len(llamadas) == 1
    assert "public.datos_rrhh" in llamadas[0]
    assert "public.datos_archivo" not in llamadas[0]
    assert "public.empleados" in llamadas[0]


def test_vencimientos_rrhh_filtra_deleted_at_y_status():
    import inspect
    sql = inspect.getsource(ret.get_expired_docs_rrhh)
    assert "dr.deleted_at IS NULL" in sql
    assert "dr.status" in sql
