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
    """Paneles del HTML MÁS los que inyecta admin-ui.js.

    "Acceso" y "Auditoría" eran el mismo marcado en los dos módulos, así que
    viven en admin-ui.js. Para esta comprobación cuentan igual: lo que importa
    es que la pestaña tenga a dónde apuntar, no en qué archivo esté escrito.
    """
    del_html = set(
        re.findall(r'<div class="tab-pane[^"]*" id="(pane-admin-%s-[\w-]+)"' % suf, html)
    )
    js = (STATIC / "admin-ui.js").read_text(encoding="utf-8")
    inyectados = {
        f"pane-admin-{suf}-{tab}"
        for tab in re.findall(r'id="pane-admin-\$\{suf\}-([\w-]+)"', js)
    }
    return del_html | inyectados


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


# ---------------------------------------------------------------------------
# Tabla del monitor
# ---------------------------------------------------------------------------
# El monitor de RRHH emitia siete celdas bajo un encabezado de seis columnas:
# la tabla entera salia corrida y cada dato aparecia bajo el titulo equivocado.
# Las etiquetas dobles ("Titulo / Empleado") venian de cuando ambos modulos
# compartian una sola pagina.

MONITOR_HEAD = re.compile(
    r'<tbody id="admin_control_table-(\w+)"', re.S)


def _monitor_headers(html, suf):
    """Columnas <th> de la tabla del monitor de ese módulo."""
    i = html.index(f'admin_control_table-{suf}')
    head = html.rindex("<thead", 0, i)
    seg = html[head:i]
    return re.findall(r"<th(?=[\s>])[^>]*>(.*?)</th>", seg, re.S)


def _row_template_cells(js, modulo):
    """Celdas <td> de la plantilla de fila correspondiente al módulo."""
    marker = 'return `<tr class="ds-monitor-row">' if modulo == "archivo" \
        else 'return `\n        <tr class="ds-monitor-row">'
    i = js.index(marker)
    j = js.index("</tr>", i)
    return re.findall(r"<td[^>]*>", js[i:j])


@pytest.mark.parametrize("suf", sorted(PANELS))
def test_monitor_columnas_y_celdas_cuadran(suf):
    html = _read(suf)
    js = (STATIC / "admin-monitor.js").read_text(encoding="utf-8")
    cabeceras = _monitor_headers(html, suf)
    celdas = _row_template_cells(js, suf)
    assert len(cabeceras) == len(celdas), (
        f"monitor {suf}: {len(cabeceras)} columnas en el <thead> pero "
        f"{len(celdas)} celdas por fila — la tabla sale corrida"
    )


@pytest.mark.parametrize("suf", sorted(PANELS))
def test_monitor_ocultamiento_responsive_coherente(suf):
    """Si el <th> se oculta en móvil, su <td> también: si no, la fila se desplaza."""
    html = _read(suf)
    js = (STATIC / "admin-monitor.js").read_text(encoding="utf-8")

    def clases(tag):
        m = re.search(r'class="([^"]*)"', tag)
        return {c for c in (m.group(1).split() if m else []) if c.startswith("ds-hide-")}

    i = html.index(f"admin_control_table-{suf}")
    head = html.rindex("<thead", 0, i)
    # `<th[^>]*>` tambien casaria con `<thead>` y desplazaria la lista.
    th_tags = re.findall(r"<th(?=[\s>])[^>]*>", html[head:i])
    td_tags = _row_template_cells(js, suf)

    descuadres = [
        (n, clases(th), clases(td))
        for n, (th, td) in enumerate(zip(th_tags, td_tags), 1)
        if clases(th) != clases(td)
    ]
    assert not descuadres, (
        f"monitor {suf}: columnas con ocultamiento distinto entre <th> y <td>: "
        f"{descuadres}"
    )


# ---------------------------------------------------------------------------
# Cáscara compartida
# ---------------------------------------------------------------------------
# El menú lateral estaba copiado en cinco páginas. Eran idénticos salvo por cuál
# enlace llevaba `active`, y aun así habían derivado: el enlace del asistente
# existía solo en el panel de Sistema. Ahora se renderiza desde app-shell.js.

PAGINAS_CON_CASCARA = [
    "admin_archive.html", "admin_hr.html", "admin_system.html",
    "archive.html", "hr.html",
]


@pytest.mark.parametrize("nombre", PAGINAS_CON_CASCARA)
def test_barra_superior_no_esta_duplicada(nombre):
    html = (STATIC / nombre).read_text(encoding="utf-8")
    assert '<nav class="main-header' not in html, (
        f"{nombre} vuelve a traer la barra escrita a mano; debe usar el hueco "
        "#app-shell-navbar"
    )
    assert 'id="app-shell-navbar"' in html, f"{nombre} no tiene el hueco de la barra"


@pytest.mark.parametrize("nombre", PAGINAS_CON_CASCARA)
def test_menu_lateral_no_esta_duplicado(nombre):
    html = (STATIC / nombre).read_text(encoding="utf-8")
    assert "<aside id=\"app-sidebar\"" not in html, (
        f"{nombre} vuelve a traer el menú escrito a mano; debe usar el hueco "
        "#app-shell-sidebar que rellena app-shell.js"
    )
    assert 'id="app-shell-sidebar"' in html, f"{nombre} no tiene el hueco del menú"
    assert "/static/app-shell.js" in html, f"{nombre} no carga app-shell.js"


def test_el_hueco_se_rellena_antes_de_usarse():
    """app-shell.js debe cargarse en el <body>, no al final: configureSidebarVisibilities()
    busca los enlaces por id y necesita que ya existan."""
    for nombre in PAGINAS_CON_CASCARA:
        html = (STATIC / nombre).read_text(encoding="utf-8")
        barra  = html.index('id="app-shell-navbar"')
        menu   = html.index('id="app-shell-sidebar"')
        shell  = html.index("/static/app-shell.js")
        app_js = html.index("/static/app.js")
        assert barra < menu < shell < app_js, (
            f"{nombre}: el orden debe ser huecos → app-shell.js → app.js"
        )


def test_todas_las_paginas_declaran_su_data_page():
    """El enlace activo del menú sale de data-page; sin él no se marca ninguno."""
    faltan = [
        n for n in PAGINAS_CON_CASCARA
        if "data-page=" not in (STATIC / n).read_text(encoding="utf-8")
    ]
    assert not faltan, f"páginas sin data-page: {faltan}"


# ---------------------------------------------------------------------------
# Modales de admin-ui.js
# ---------------------------------------------------------------------------
# El marcado de los modales de confirmación y de texto estaba copiado en las
# tres páginas de administración. Es infraestructura de esos helpers, no de las
# páginas: si confirmModal() vive en admin-ui.js, su modal también.

PAGINAS_ADMIN = ["admin_archive.html", "admin_hr.html", "admin_system.html"]


@pytest.mark.parametrize("nombre", PAGINAS_ADMIN)
def test_los_modales_de_ui_no_estan_pegados_en_el_html(nombre):
    html = (STATIC / nombre).read_text(encoding="utf-8")
    for mid in ("ds-confirm-modal", "ds-prompt-modal"):
        assert f'id="{mid}"' not in html, (
            f"{nombre} vuelve a traer #{mid} escrito a mano; lo inyecta admin-ui.js"
        )


def test_admin_ui_define_sus_modales():
    js = (STATIC / "admin-ui.js").read_text(encoding="utf-8")
    for mid in ("ds-confirm-modal", "ds-prompt-modal"):
        assert f'id="{mid}"' in js, f"admin-ui.js ya no define #{mid}"
    assert "_asegurarModalesUI" in js


@pytest.mark.parametrize("nombre", PAGINAS_ADMIN)
def test_las_paginas_que_usan_los_modales_cargan_admin_ui(nombre):
    html = (STATIC / nombre).read_text(encoding="utf-8")
    assert "/static/admin-ui.js" in html, f"{nombre} no carga admin-ui.js"
