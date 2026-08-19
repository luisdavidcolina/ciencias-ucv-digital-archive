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
            disposicion="transferido", acta="Acta 12/2026", requester="admin"))

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
            ret.registrar_disposicion(7, ret.DisposicionIn(disposicion="conservar"))
        assert e.value.status_code == 409
        assert "Eliminado por expurgo" in e.value.detail


def test_documento_inexistente():
    with patch.object(ret, "db_query", return_value=None), \
         patch.object(ret, "log_event"):
        with pytest.raises(HTTPException) as e:
            ret.registrar_disposicion(999, ret.DisposicionIn(disposicion="conservar"))
        assert e.value.status_code == 404


def test_la_decision_queda_en_auditoria():
    """Sin rastro no hay disposición que valga: es el punto de todo esto."""
    with patch.object(ret, "db_query", side_effect=lambda s, *a, **k: _doc() if "SELECT" in s else None), \
         patch.object(ret, "log_event") as reg:
        ret.registrar_disposicion(7, ret.DisposicionIn(
            disposicion="eliminado", acta="Acta 3/2026", requester="admin"))
    reg.assert_called_once()
    assert "Disposición" in reg.call_args[0][1]
    assert "Acta 3/2026" in reg.call_args[0][3]


def test_los_vencimientos_dejan_de_listar_lo_ya_dispuesto():
    """Si siguieran apareciendo, la lista no bajaría nunca y perdería sentido."""
    import inspect
    sql = inspect.getsource(ret.get_expired_docs)
    assert "da.disposicion IS NULL" in sql
    assert "da.deleted_at IS NULL" in sql
