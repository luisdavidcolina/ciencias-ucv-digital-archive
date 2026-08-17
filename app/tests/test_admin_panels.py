"""
Coherencia estructural de los paneles de administración.

Los paneles de Archivo y RRHH son dos HTML gemelos con la misma barra de
pestañas y los mismos identificadores salvo el sufijo de módulo. Es fácil mover
una tarjeta de sitio y dejar una pestaña apuntando a un panel inexistente, un
panel huérfano sin pestaña, o una pestaña sin rama en `loadAdminTab`. Nada de
eso rompe la carga de la página: simplemente deja un área en blanco, que es
justo el tipo de fallo que nadie reporta.
"""
import re
from pathlib import Path

import pytest

STATIC = Path(__file__).resolve().parents[1] / "static"

PANELS = {
    "archivo": STATIC / "admin_archive.html",
    "rrhh": STATIC / "admin_hr.html",
}


def _read(suf):
    return PANELS[suf].read_text(encoding="utf-8")


def _pills(html, suf):
    """Pestañas declaradas: (id de tab, destino del href, argumento de loadAdminTab)."""
    out = []
    for m in re.finditer(
        r'<a class="nav-link[^"]*" id="tab-admin-%s-([\w-]+)"[^>]*?'
        r'href="#(pane-admin-%s-[\w-]+)"[^>]*?'
        r"onclick=\"loadAdminTab\('([\w-]+)'\)\"" % (suf, suf),
        html,
    ):
        out.append(m.groups())
    return out


def _panes(html, suf):
    return set(
        re.findall(r'<div class="tab-pane[^"]*" id="(pane-admin-%s-[\w-]+)"' % suf, html)
    )


@pytest.mark.parametrize("suf", sorted(PANELS))
def test_cada_pestana_tiene_panel(suf):
    html = _read(suf)
    pills = _pills(html, suf)
    assert pills, f"no se detectó ninguna pestaña en el panel {suf}"
    panes = _panes(html, suf)
    faltan = [href for _, href, _ in pills if href not in panes]
    assert not faltan, f"pestañas que apuntan a un panel inexistente: {faltan}"


@pytest.mark.parametrize("suf", sorted(PANELS))
def test_cada_panel_tiene_pestana(suf):
    html = _read(suf)
    destinos = {href for _, href, _ in _pills(html, suf)}
    huerfanos = sorted(_panes(html, suf) - destinos)
    assert not huerfanos, f"paneles sin pestaña que los abra: {huerfanos}"


@pytest.mark.parametrize("suf", sorted(PANELS))
def test_id_href_y_handler_concuerdan(suf):
    """El sufijo del id, el del href y el argumento de loadAdminTab deben coincidir."""
    problemas = []
    for tab_id, href, handler in _pills(_read(suf), suf):
        if tab_id != handler or href != f"pane-admin-{suf}-{handler}":
            problemas.append((tab_id, href, handler))
    assert not problemas, f"pestañas descuadradas (id, href, handler): {problemas}"


def test_ambos_paneles_ofrecen_las_mismas_pestanas():
    """Archivo y RRHH deben mantenerse simétricos: misma navegación en ambos."""
    por_modulo = {
        suf: [h for _, _, h in _pills(_read(suf), suf)] for suf in PANELS
    }
    assert por_modulo["archivo"] == por_modulo["rrhh"], (
        f"la navegación difiere entre módulos: {por_modulo}"
    )


def test_loadadmintab_maneja_todas_las_pestanas():
    """Cada pestaña necesita su rama de carga, o abre un panel que nunca se llena."""
    admin_js = (STATIC / "admin.js").read_text(encoding="utf-8")
    cuerpo = admin_js[admin_js.index("function loadAdminTab"):]
    cuerpo = cuerpo[: cuerpo.index("\n}")]

    manejadas = set(re.findall(r'adminTabId === "([\w-]+)"', cuerpo))
    # "stats" es el panel activo por defecto y se maneja en la primera rama.
    declaradas = {h for _, _, h in _pills(_read("archivo"), "archivo")}

    sin_rama = sorted(declaradas - manejadas)
    assert not sin_rama, (
        f"pestañas sin rama en loadAdminTab (abren un panel vacío): {sin_rama}"
    )
