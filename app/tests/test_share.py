"""
Enlaces de compartición externa.

Lo que importa aquí es lo que el enlace NO debe permitir: sobrevivir a su
caducidad, ser manipulado, apuntar a un módulo inventado, o seguir sirviendo un
documento que ya está en la papelera.
"""
import sys
import time
from pathlib import Path
from unittest.mock import patch

import pytest

APP = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(APP))

from core.security import (  # noqa: E402
    generate_share_token,
    share_token_jti,
    verify_share_token,
)


@pytest.fixture(autouse=True)
def _no_revocados():
    """`verify_share_token` consulta `enlaces_revocados` (IN-146) antes de
    aceptar un token. Estos tests no tienen base de datos: se simula "nada
    revocado" salvo que un test concreto quiera lo contrario."""
    with patch("database.db_query", return_value=None) as m:
        yield m


# --- el token ---------------------------------------------------------------

def test_ida_y_vuelta():
    t = generate_share_token("Archivo", 42, horas=1)
    assert verify_share_token(t) == ("Archivo", 42)


def test_jti_es_unico_por_token():
    a = generate_share_token("Archivo", 1, horas=1)
    b = generate_share_token("Archivo", 1, horas=1)
    assert share_token_jti(a) != share_token_jti(b)


def test_token_revocado_deja_de_servir(_no_revocados):
    """Un jti presente en `enlaces_revocados` invalida el enlace aunque la
    firma y la caducidad sigan siendo correctas — el mecanismo de IN-146."""
    t = generate_share_token("Archivo", 42, horas=1)
    assert verify_share_token(t) == ("Archivo", 42)  # antes de revocar, sirve
    _no_revocados.return_value = {"1": 1}            # simula fila encontrada
    assert verify_share_token(t) is None


def test_revocar_un_token_no_afecta_a_otro(_no_revocados):
    a = generate_share_token("Archivo", 1, horas=1)
    b = generate_share_token("Archivo", 2, horas=1)
    jti_a = share_token_jti(a)

    def fake_query(sql, params=None, fetch="all"):
        return {"jti": jti_a} if params and params[0] == jti_a else None

    _no_revocados.side_effect = fake_query
    assert verify_share_token(a) is None
    assert verify_share_token(b) == ("Archivo", 2)


def test_rechaza_token_manipulado():
    t = generate_share_token("Archivo", 42, horas=1)
    assert verify_share_token(t[:-6] + "AAAAAA") is None


def test_rechaza_token_caducado():
    assert verify_share_token(generate_share_token("Archivo", 42, horas=-1)) is None


def test_rechaza_modulo_desconocido():
    """Aunque la firma sea válida: el módulo decide qué tabla se consulta."""
    assert verify_share_token(generate_share_token("otra_tabla", 1, horas=1)) is None


def test_rechaza_vacio_y_basura():
    for malo in (None, "", "xxx", "!!!!", "a" * 200):
        assert verify_share_token(malo) is None


def test_un_token_no_sirve_para_otro_documento():
    a = generate_share_token("Archivo", 1, horas=1)
    b = generate_share_token("Archivo", 2, horas=1)
    assert a != b
    assert verify_share_token(a)[1] == 1
    assert verify_share_token(b)[1] == 2


def test_la_firma_depende_de_la_clave():
    """Con otra SECRET_KEY el token deja de valer: no es sólo codificación."""
    t = generate_share_token("Archivo", 42, horas=1)
    from core import security
    with patch.object(security.settings, "secret_key", "otra-clave-distinta"):
        assert verify_share_token(t) is None


def test_caducidad_futura_razonable():
    t = generate_share_token("Archivo", 7, horas=72)
    from core import security
    import base64
    relleno = "=" * (-len(t) % 4)
    crudo = base64.urlsafe_b64decode((t + relleno).encode()).decode()
    expira = int(crudo.rsplit(":", 4)[2])
    restante = expira - time.time()
    assert 71 * 3600 < restante <= 72 * 3600


# --- los endpoints ----------------------------------------------------------

@pytest.fixture
def share_mod():
    import routes.share as m
    return m


def test_no_comparte_un_documento_inexistente(share_mod):
    from fastapi import HTTPException
    with patch.object(share_mod, "_leer_documento", return_value=None), \
         patch.object(share_mod, "log_event"):
        with pytest.raises(HTTPException) as e:
            share_mod.create_link(modulo="Archivo", doc_id=999, horas=1, usuario="admin")
        assert e.value.status_code == 404


def test_no_comparte_un_modulo_invalido(share_mod):
    from fastapi import HTTPException
    with pytest.raises(HTTPException) as e:
        share_mod.create_link(modulo="Inventado", doc_id=1, horas=1, usuario="admin")
    assert e.value.status_code == 400


def test_el_enlace_no_expone_la_ruta_interna_del_archivo(share_mod):
    """El file_url es la llave real; sólo debe servirse por el propio enlace."""
    doc = {"id": 1, "titulo": "X", "autor": "Y", "file_url": "/api/files/secreto.pdf",
           "ubicacion": "", "fecha": "2026-01-01", "tipo": "Oficio", "soporte": "Físico"}
    token = generate_share_token("Archivo", 1, horas=1)
    with patch.object(share_mod, "_leer_documento", return_value=doc), \
         patch.object(share_mod, "log_event"):
        r = share_mod.leer_compartido(token)
    assert "file_url" not in r["documento"]
    assert r["documento"]["tiene_archivo"] is True


def test_un_documento_en_papelera_deja_de_verse(share_mod):
    """_leer_documento filtra deleted_at; el enlace vigente no debe rescatarlo."""
    from fastapi import HTTPException
    token = generate_share_token("Archivo", 1, horas=1)
    with patch.object(share_mod, "_leer_documento", return_value=None), \
         patch.object(share_mod, "log_event"):
        with pytest.raises(HTTPException) as e:
            share_mod.leer_compartido(token)
        assert e.value.status_code == 404


def test_la_consulta_queda_en_auditoria(share_mod):
    doc = {"id": 1, "titulo": "X", "autor": "", "file_url": None, "ubicacion": "",
           "fecha": "", "tipo": "", "soporte": ""}
    token = generate_share_token("Archivo", 1, horas=1)
    with patch.object(share_mod, "_leer_documento", return_value=doc), \
         patch.object(share_mod, "log_event") as reg:
        share_mod.leer_compartido(token)
    reg.assert_called_once()
    assert "Enlace" in reg.call_args[0][1]
