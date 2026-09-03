"""Capturas de estados que no se alcanzan cargando una URL: modales, menú
lateral abierto, panel de personalización, burbuja del asistente, enlace
compartido válido y estados de pasar el ratón.

    python docs/auditoria/capturas/_harness/estados.py
"""
import json
import os
import sys
import time

from playwright.sync_api import sync_playwright

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", ".."))
sys.path.insert(0, os.path.join(ROOT, "app"))
os.environ.setdefault("SECRET_KEY", "auditoria-visual-key")
from core.security import generate_session_token, generate_share_token  # noqa: E402

OUT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
BASE = "http://127.0.0.1:8099"
S = {"username": "admin.global", "modules": ["Archivo", "RRHH"],
     "roles": {"Archivo": "Admin", "RRHH": "Admin"}, "modulo": "Archivo", "rol": "Admin"}

informe = []


def ctx_nuevo(browser, ancho=1440, alto=900, oscuro=False):
    ctx = browser.new_context(viewport={"width": ancho, "height": alto}, locale="es-VE")
    ctx.add_init_script("(() => { const p = %s; for (const k in p) localStorage.setItem(k, p[k]); })()" % json.dumps({
        "archive_session": json.dumps({**S, "ts": int(time.time() * 1000)}),
        "ds_dark_mode": "dark" if oscuro else "light"}))
    ctx.add_cookies([{"name": "ds_session", "value": generate_session_token("admin.global"), "url": BASE}])
    return ctx


def shot(page, nombre, full=False):
    page.screenshot(path=os.path.join(OUT, nombre + ".png"), full_page=full)
    print("   ", nombre)


def main():
    with sync_playwright() as pw:
        b = pw.chromium.launch()

        # 1. Enlace compartido válido
        for oscuro in (False, True):
            ctx = ctx_nuevo(b, oscuro=oscuro)
            p = ctx.new_page()
            p.goto(BASE + "/compartido/" + generate_share_token("Archivo", 1, 72))
            p.wait_for_timeout(2000)
            shot(p, "compartido-valido-1440-%s" % ("oscuro" if oscuro else "claro"), full=True)
            ctx.close()

        # 2. Menú lateral abierto + panel de personalización + asistente
        for oscuro in (False, True):
            suf = "oscuro" if oscuro else "claro"
            ctx = ctx_nuevo(b, oscuro=oscuro)
            p = ctx.new_page()
            p.goto(BASE + "/archivo"); p.wait_for_timeout(2500)
            try:
                p.evaluate("openSidebar()"); p.wait_for_timeout(700)
                shot(p, f"archivo-menu-abierto-1440-{suf}")
                p.evaluate("closeSidebar()")
            except Exception as e:
                print("menu:", e)
            try:
                p.evaluate("openThemePanel()"); p.wait_for_timeout(900)
                shot(p, f"archivo-panel-temas-1440-{suf}")
                p.keyboard.press("Escape"); p.wait_for_timeout(400)
            except Exception as e:
                print("temas:", e)
            try:
                p.click("#ai-widget-bubble, .ai-bubble, [id*=ai-] button", timeout=3000)
                p.wait_for_timeout(1200)
                shot(p, f"archivo-asistente-1440-{suf}")
            except Exception as e:
                print("asistente:", str(e)[:80])
            try:
                p.evaluate("showToast('Documento guardado correctamente','success'); "
                           "setTimeout(()=>showToast('No se pudo conectar con el servidor','error'),150)")
                p.wait_for_timeout(900)
                shot(p, f"archivo-toast-1440-{suf}")
            except Exception as e:
                print("toast:", str(e)[:80])
            ctx.close()

        # 3. Modal de detalle en la búsqueda y modal de edición del backoffice
        for oscuro in (False, True):
            suf = "oscuro" if oscuro else "claro"
            for ancho in (390, 1440):
                ctx = ctx_nuevo(b, ancho=ancho, oscuro=oscuro)
                p = ctx.new_page()
                p.goto(BASE + "/archivo"); p.wait_for_timeout(2500)
                try:
                    p.click(".ds-item-actions a, .ds-item-actions button", timeout=4000)
                    p.wait_for_timeout(1800)
                    shot(p, f"archivo-modal-detalle-{ancho}-{suf}", full=True)
                except Exception as e:
                    print("modal detalle:", str(e)[:90])
                ctx.close()

                ctx = ctx_nuevo(b, ancho=ancho, oscuro=oscuro)
                p = ctx.new_page()
                p.goto(BASE + "/admin/archivo"); p.wait_for_timeout(2800)
                try:
                    p.evaluate("openEditDocModal(1)")
                    p.wait_for_timeout(2200)
                    shot(p, f"admin_archivo-modal-edicion-{ancho}-{suf}", full=True)
                except Exception as e:
                    print("modal edicion:", str(e)[:90])
                ctx.close()

        # 4. Modal de expediente de RRHH (dossier)
        for ancho in (390, 1440):
            ctx = ctx_nuevo(b, ancho=ancho)
            p = ctx.new_page()
            p.goto(BASE + "/admin/rrhh"); p.wait_for_timeout(2800)
            try:
                p.evaluate("openEditEmpleadoModal(1)")
                p.wait_for_timeout(2500)
                shot(p, f"admin_rrhh-modal-expediente-{ancho}-claro", full=True)
            except Exception as e:
                print("modal empleado:", str(e)[:90])
            ctx.close()

        # 5. Estado vacío y estado de error de la búsqueda
        ctx = ctx_nuevo(b)
        p = ctx.new_page()
        p.route("**/api/archivo/buscar", lambda r: r.fulfill(
            status=200, content_type="application/json",
            body=json.dumps({"records": [], "total": 0, "page": 1, "per_page": 10})))
        p.goto(BASE + "/archivo"); p.wait_for_timeout(2500)
        shot(p, "archivo-vacio-1440-claro", full=True)
        ctx.close()

        ctx = ctx_nuevo(b)
        p = ctx.new_page()
        p.route("**/api/archivo/buscar", lambda r: r.fulfill(status=500, body="boom"))
        p.goto(BASE + "/archivo"); p.wait_for_timeout(3500)
        shot(p, "archivo-error-1440-claro", full=True)
        ctx.close()

        # 6. Pasar el ratón por una tarjeta de resultado y por una fila del monitor
        ctx = ctx_nuevo(b)
        p = ctx.new_page()
        p.goto(BASE + "/archivo"); p.wait_for_timeout(2500)
        try:
            p.hover(".ds-item-card"); p.wait_for_timeout(500)
            shot(p, "archivo-hover-tarjeta-1440-claro")
        except Exception as e:
            print("hover:", str(e)[:80])
        ctx.close()

        b.close()


if __name__ == "__main__":
    main()
