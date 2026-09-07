"""
Backup programado.

Motivación: la única copia era la que alguien se acordara de descargar a mano
desde el panel. El export no se guardaba en ningún sitio — se enviaba al
navegador y ya —, así que si se perdía la base no había nada.

Lo delicado de este endpoint es que devuelve la base entera. Si queda abierto,
es una fuga completa. Por eso las pruebas se centran en que falle CERRADO:
sin `CRON_SECRET` configurado, o con una credencial equivocada, no responde.
"""
import sys
from pathlib import Path
from unittest.mock import patch

import pytest
from fastapi import HTTPException

APP = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(APP))

import routes.backup as bk  # noqa: E402

# Valor claramente de relleno: la guarda de test_secrets.py revisa tambien
# los tests, y hace bien en sospechar de cualquier cadena que parezca real.
SECRETO = "clave-solo-para-pruebas"


def _sin_efectos():
    """Parchea todo lo que sale del proceso: base, almacenamiento y registro."""
    return (
        patch.object(bk, "_construir_backup", return_value=({"_metadata": {}}, 42)),
        patch.object(bk, "_registrar_backup"),
        patch.object(bk.storage, "is_configured", return_value=True),
        patch.object(bk.storage, "upload_fileobj"),
    )


def test_sin_cron_secret_no_responde():
    """Fallar cerrado: sin la variable, el endpoint queda deshabilitado."""
    with patch.dict("os.environ", {}, clear=False):
        import os
        os.environ.pop("CRON_SECRET", None)
        with pytest.raises(HTTPException) as e:
            bk.backup_programado(authorization="Bearer loquesea")
        assert e.value.status_code == 503


@pytest.mark.parametrize("cabecera", ["", "Bearer equivocado", SECRETO,
                                      "Basic secreto-de-prueba"])
def test_credencial_incorrecta_rechazada(cabecera):
    """Incluye el caso sutil: el secreto correcto pero sin el prefijo Bearer."""
    with patch.dict("os.environ", {"CRON_SECRET": SECRETO}):
        with pytest.raises(HTTPException) as e:
            bk.backup_programado(authorization=cabecera)
        assert e.value.status_code == 401


def test_sin_almacenamiento_configurado_no_finge_exito():
    with patch.dict("os.environ", {"CRON_SECRET": SECRETO}), \
         patch.object(bk.storage, "is_configured", return_value=False):
        with pytest.raises(HTTPException) as e:
            bk.backup_programado(authorization=f"Bearer {SECRETO}")
        assert e.value.status_code == 503


def test_copia_correcta():
    p1, p2, p3, p4 = _sin_efectos()
    with patch.dict("os.environ", {"CRON_SECRET": SECRETO}), p1, p2 as reg, p3, p4 as subir:
        r = bk.backup_programado(authorization=f"Bearer {SECRETO}")
    assert r["ok"] is True
    assert r["filas"] == 42
    assert r["clave"].startswith("backups/") and r["clave"].endswith(".json")
    subir.assert_called_once()
    reg.assert_called_once()


def test_un_fallo_al_subir_no_se_reporta_como_exito():
    """Si R2 falla, el endpoint debe romper: un backup que no se guardó no vale."""
    p1, p2, p3, _ = _sin_efectos()
    with patch.dict("os.environ", {"CRON_SECRET": SECRETO}), p1, p2 as reg, p3, \
         patch.object(bk.storage, "upload_fileobj", side_effect=RuntimeError("R2 caído")):
        with pytest.raises(HTTPException) as e:
            bk.backup_programado(authorization=f"Bearer {SECRETO}")
        assert e.value.status_code == 502
    # y el intento fallido queda registrado, que es cuando más falta hace
    reg.assert_called_once()
    assert "FALLO" in reg.call_args[0][3]


def test_todas_las_tablas_fallan_no_sube_ni_finge_exito():
    """IN-176: si ninguna tabla pudo exportarse, no es una copia parcial
    aprovechable — no debe subirse a R2 ni registrarse como si lo fuera."""
    backup_roto = {"_metadata": {}}
    backup_roto.update({f"_error_{t}": "boom" for t in bk.EXPORTABLE_TABLES})
    with patch.dict("os.environ", {"CRON_SECRET": SECRETO}), \
         patch.object(bk, "_construir_backup", return_value=(backup_roto, 0)), \
         patch.object(bk, "_registrar_backup") as reg, \
         patch.object(bk.storage, "is_configured", return_value=True), \
         patch.object(bk.storage, "upload_fileobj") as subir:
        with pytest.raises(HTTPException) as e:
            bk.backup_programado(authorization=f"Bearer {SECRETO}")
        assert e.value.status_code == 502
    subir.assert_not_called()
    reg.assert_called_once()
    assert "FALLO" in reg.call_args[0][3]


def test_tabla_con_error_parcial_se_sube_pero_queda_anotado():
    """Una tabla rota entre trece no debe perder las otras doce, pero el
    historial debe dejar constancia — no marcarse como copia limpia."""
    backup_parcial = {"_metadata": {}, "_error_categoria": "boom"}
    with patch.dict("os.environ", {"CRON_SECRET": SECRETO}), \
         patch.object(bk, "_construir_backup", return_value=(backup_parcial, 10)), \
         patch.object(bk, "_registrar_backup") as reg, \
         patch.object(bk.storage, "is_configured", return_value=True), \
         patch.object(bk.storage, "upload_fileobj") as subir, \
         patch.object(bk, "db_query", return_value=None):
        r = bk.backup_programado(authorization=f"Bearer {SECRETO}")
    assert r["ok"] is True
    assert r["tablas_con_error"] == ["categoria"]
    subir.assert_called_once()
    reg.assert_called_once()
    assert "categoria" in reg.call_args[0][3]


def test_caida_brusca_de_filas_queda_anotada_sin_bloquear():
    """IN-176: comparar contra la copia anterior avisa de una caída fuerte de
    filas sin impedir que la copia de hoy (ya a salvo en R2) se registre."""
    p1, p2, p3, p4 = _sin_efectos()
    with patch.dict("os.environ", {"CRON_SECRET": SECRETO}), p1, p2 as reg, p3, p4, \
         patch.object(bk, "db_query", return_value={"total_rows": 1000}):
        r = bk.backup_programado(authorization=f"Bearer {SECRETO}")
    assert r["ok"] is True  # 42 filas frente a 1000 no impide la copia
    reg.assert_called_once()
    assert "AVISO" in reg.call_args[0][3] and "caída" in reg.call_args[0][3]


def test_fallo_al_comparar_con_historial_no_tumba_el_backup():
    """La comparación con la copia anterior es extra, no el backup en sí."""
    p1, p2, p3, p4 = _sin_efectos()
    with patch.dict("os.environ", {"CRON_SECRET": SECRETO}), p1, p2 as reg, p3, p4, \
         patch.object(bk, "db_query", side_effect=RuntimeError("sin conexión")):
        r = bk.backup_programado(authorization=f"Bearer {SECRETO}")
    assert r["ok"] is True
    reg.assert_called_once()


def test_el_historial_no_puede_tumbar_el_backup():
    """Si backup_history falla, la copia ya está en R2: no debe propagarse."""
    with patch.object(bk, "db_query", side_effect=RuntimeError("sin conexión")), \
         patch.object(bk, "logger"):
        bk._registrar_backup("cron", 1, 1, "nota")     # no debe lanzar


def test_esta_programado_en_vercel():
    """El endpoint sin cron que lo llame no sirve de nada."""
    import json
    cfg = json.loads((APP.parent / "vercel.json").read_text(encoding="utf-8"))
    rutas = [c.get("path") for c in cfg.get("crons", [])]
    assert "/api/admin/backup/programado" in rutas, (
        "el backup programado no está declarado en vercel.json"
    )


def test_el_cron_no_exige_sesion_de_navegador():
    """Va en su propio router: el principal exige cookie y el cron no la tiene."""
    rutas = {r.path for r in bk.router_cron.routes}
    assert "/programado" in rutas
    assert not bk.router_cron.dependencies
