"""
Pruebas visuales — SD-224.

Motivación: 361 pruebas de servidor y de análisis estático y ninguna
renderiza. Todos los hallazgos de `sistema-diseno.md` (tabla corrida, panel
que abre vacío, desborde horizontal) sobrevivieron a esa suite porque ninguna
prueba abre un navegador.

Alcance de esta primera entrega, anotado también en `_BUZON.md`: capturar
**y comparar contra una referencia** las 235 combinaciones de componente ×
tema × modo × ancho que pide la ficha completa exige la galería de SD-223
(`app/static/sistema.html`, carril LG, sin resolver todavía) y una política de
cuándo una referencia PNG se actualiza a mano — dos decisiones de producto que
no le tocan a este carril. Lo que SÍ se puede automatizar ya, sin esa galería
y sin base de datos real, es lo que de hecho decidió estos hallazgos:
renderizar cada página pública a 390/768/1440px, en claro y oscuro, y fallar
si algo desborda horizontalmente o si la consola tira un error de JS — la
misma inspección manual que pide "Antes de dar algo por terminado" en
CLAUDE.md, hecha por CI. Cuando exista la galería de SD-223, estas mismas
utilidades (`_arrancar_servidor`, `_pagina`) sirven para el pixel-diff
completo.
"""
import socket
import threading
import time
from contextlib import closing
from pathlib import Path

import pytest

APP = Path(__file__).resolve().parents[1]

playwright_sync = pytest.importorskip(
    "playwright.sync_api", reason="playwright es opcional (ver requirements-dev.txt)"
)

try:
    import uvicorn
except ImportError:  # pragma: no cover
    uvicorn = None

ANCHOS = [390, 768, 1440]

# Páginas públicas, sin sesión ni datos reales: lo que verifica esta prueba es
# la cáscara (barra, menú, tarjetas, tipografía, tema), no el contenido de una
# búsqueda concreta. Se excluyen /investigacion (exige sesión en el servidor,
# redirige) y /compartido/<token> (exige un token real).
PAGINAS_PUBLICAS = [
    "/login", "/archivo", "/rrhh",
    "/admin/archivo", "/admin/rrhh", "/admin/sistema", "/admin/ia",
    "/ayuda", "/sistema",
]


def _puerto_libre() -> int:
    with closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


@pytest.fixture(scope="module")
def servidor_vivo(app):
    """Levanta la app real (misma `app` fixture que mockea startup/BD que usa
    el resto de la suite) en un puerto local, para que Playwright pueda
    navegar a una URL de verdad — un TestClient de httpx no sirve, no habla
    HTTP con un navegador."""
    if uvicorn is None:
        pytest.skip("uvicorn no está instalado")

    puerto = _puerto_libre()
    config = uvicorn.Config(app, host="127.0.0.1", port=puerto, log_level="error")
    servidor = uvicorn.Server(config)
    hilo = threading.Thread(target=servidor.run, daemon=True)
    hilo.start()

    limite = time.time() + 10
    while not getattr(servidor, "started", False) and time.time() < limite:
        time.sleep(0.05)
    assert servidor.started, "el servidor de pruebas no arrancó a tiempo"

    yield f"http://127.0.0.1:{puerto}"

    servidor.should_exit = True
    hilo.join(timeout=5)


@pytest.fixture(scope="module")
def navegador():
    with playwright_sync.sync_playwright() as p:
        try:
            b = p.chromium.launch()
        except Exception as e:  # pragma: no cover
            pytest.skip(f"chromium no disponible para Playwright: {e}")
        yield b
        b.close()


def _errores_de_consola(pagina) -> list[str]:
    errores = []
    pagina.on(
        "console",
        lambda msg: errores.append(msg.text) if msg.type == "error" else None,
    )
    pagina.on("pageerror", lambda exc: errores.append(str(exc)))
    return errores


def _desborde_horizontal(pagina) -> bool:
    return pagina.evaluate(
        "document.documentElement.scrollWidth > document.documentElement.clientWidth + 1"
    )


CASOS = [
    (ruta, ancho, oscuro)
    for ruta in PAGINAS_PUBLICAS
    for ancho in ANCHOS
    for oscuro in (False, True)
]


@pytest.mark.parametrize(
    "ruta,ancho,oscuro", CASOS,
    ids=[f"{r.strip('/')}-{a}px-{'oscuro' if o else 'claro'}" for r, a, o in CASOS],
)
def test_pagina_publica_sin_desborde_ni_errores(servidor_vivo, navegador, ruta, ancho, oscuro):
    contexto = navegador.new_context(viewport={"width": ancho, "height": 900})
    pagina = contexto.new_page()
    errores = _errores_de_consola(pagina)

    pagina.goto(f"{servidor_vivo}{ruta}", wait_until="networkidle")
    if oscuro:
        pagina.evaluate("document.body.classList.add('dark-mode')")
        pagina.wait_for_timeout(150)  # transición CSS (--duration-*)

    desborde = _desborde_horizontal(pagina)
    contexto.close()

    ignorables = (
        "favicon", "net::ERR_ABORTED", "Failed to load resource",
        "Transition was skipped",  # ruido del navegador sin frame visible, no es un bug propio
    )
    errores_reales = [e for e in errores if not any(i in e for i in ignorables)]

    assert not desborde, (
        f"{ruta} a {ancho}px ({'oscuro' if oscuro else 'claro'}) desborda "
        "horizontalmente (scrollWidth > clientWidth): la clase de fallo que "
        "ninguna de las 361 pruebas de servidor puede ver"
    )
    assert not errores_reales, (
        f"{ruta} a {ancho}px ({'oscuro' if oscuro else 'claro'}) tira errores "
        f"de consola: {errores_reales}"
    )


