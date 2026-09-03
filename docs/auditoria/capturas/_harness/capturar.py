"""Recorrido visual: renderiza las diez páginas con Chromium y captura.

Requiere el servidor de `fakedb.py` en http://127.0.0.1:8099.

    python docs/auditoria/capturas/_harness/capturar.py [filtro]

Deja los PNG en docs/auditoria/capturas/ y un informe JSON con los
diagnósticos automáticos (consola, peticiones fallidas, desborde horizontal,
objetivos táctiles pequeños, foco de teclado) en `_diagnostico.json`.
"""
import json
import os
import sys
import time

from playwright.sync_api import sync_playwright

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", ".."))
sys.path.insert(0, os.path.join(ROOT, "app"))
os.environ.setdefault("SECRET_KEY", "auditoria-visual-key")
from core.security import generate_session_token  # noqa: E402

OUT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
BASE = os.environ.get("BASE", "http://127.0.0.1:8099")
TOKEN = generate_session_token("admin.global")

SESION = {
    "username": "admin.global",
    "modules": ["Archivo", "RRHH"],
    "roles": {"Archivo": "Admin", "RRHH": "Admin"},
    "modulo": "Archivo",
    "rol": "Admin",
}

TABS_ADMIN = ["stats", "new", "monitor", "categories", "papelera", "retencion", "audit", "users", "export"]
TABS_SISTEMA = ["pane-sistema", "pane-backup", "pane-auditoria", "pane-alertas", "pane-retencion"]

PAGINAS = [
    ("login", "/login", None),
    ("archivo", "/archivo", None),
    ("rrhh", "/rrhh", None),
    ("admin_archivo", "/admin/archivo", "admin"),
    ("admin_rrhh", "/admin/rrhh", "admin"),
    ("admin_sistema", "/admin/sistema", "sistema"),
    ("admin_ia", "/admin/ia", None),
    ("ayuda", "/ayuda", None),
    ("investigacion", "/investigacion", None),
    ("compartido", "/compartido/tokendeprueba", None),
]

ANCHOS = [(390, 844), (768, 1024), (1440, 900)]

DIAG_JS = r"""
() => {
  const vw = window.innerWidth;
  const overflow = [];
  const de = document.documentElement;
  document.querySelectorAll('body *').forEach(el => {
    const r = el.getBoundingClientRect();
    if (r.width === 0 || r.height === 0) return;
    if (r.right > vw + 2 && getComputedStyle(el).position !== 'fixed') {
      const p = el.parentElement;
      if (p) { const pr = p.getBoundingClientRect(); if (pr.right > vw + 2) return; }
      overflow.push({sel: el.tagName.toLowerCase() + (el.id ? '#'+el.id : '') + (el.className && typeof el.className === 'string' ? '.'+el.className.trim().split(/\s+/).slice(0,3).join('.') : ''), right: Math.round(r.right)});
    }
  });
  const chicos = [];
  document.querySelectorAll('a,button,input[type=checkbox],input[type=radio],select,[role=tab],[onclick]').forEach(el => {
    const r = el.getBoundingClientRect();
    if (r.width === 0 || r.height === 0) return;
    if (r.height < 44 || r.width < 44) {
      const t = (el.innerText || el.getAttribute('aria-label') || el.getAttribute('title') || '').trim().slice(0, 30);
      chicos.push({sel: el.tagName.toLowerCase() + (el.id ? '#'+el.id : ''), w: Math.round(r.width), h: Math.round(r.height), t});
    }
  });
  const tablas = [];
  document.querySelectorAll('table').forEach(t => {
    if (!t.offsetParent) return;
    const ths = t.querySelectorAll('thead tr:last-child th, thead tr:last-child td').length;
    const filas = [...t.querySelectorAll('tbody tr')].map(tr => tr.querySelectorAll('td,th').length);
    const distintas = [...new Set(filas)];
    const wrap = t.closest('.table-responsive');
    tablas.push({id: t.id || t.className.slice(0,40), ths, filas: distintas, envuelta: !!wrap,
                 desborda: t.scrollWidth > (wrap ? wrap.clientWidth : t.parentElement.clientWidth) + 2,
                 ancho: Math.round(t.getBoundingClientRect().width)});
  });
  const vacios = [];
  document.querySelectorAll('.tab-pane.active, [id^=pane-]').forEach(p => {
    if (!p.offsetParent) return;
    if ((p.innerText || '').trim().length < 3) vacios.push(p.id);
  });
  const imgs = [...document.querySelectorAll('img')].filter(i => i.complete && i.naturalWidth === 0)
        .map(i => i.getAttribute('src'));
  // iconos FontAwesome que no resolvieron glifo (ancho 0)
  const iconos = [...document.querySelectorAll('i.fas,i.far,i.fab,i.fa')].filter(i => {
      const r = i.getBoundingClientRect(); return i.offsetParent && r.width < 3; })
      .map(i => i.className).slice(0, 20);
  return {
    scrollAncho: de.scrollWidth, vw,
    desbordeHorizontal: de.scrollWidth > vw + 1,
    overflow: overflow.slice(0, 25), chicos: chicos.slice(0, 40), tablas,
    panelesVacios: vacios, imgsRotas: imgs, iconosSinGlifo: iconos,
    altura: de.scrollHeight
  };
}
"""

FOCO_JS = r"""
() => {
  const el = document.activeElement;
  if (!el || el === document.body) return null;
  const r = el.getBoundingClientRect();
  const cs = getComputedStyle(el);
  return {sel: el.tagName.toLowerCase() + (el.id ? '#'+el.id : '') + (el.className && typeof el.className==='string' ? '.'+el.className.trim().split(/\s+/)[0] : ''),
          texto: (el.innerText||el.value||el.getAttribute('aria-label')||'').trim().slice(0,40),
          visible: r.width>0 && r.height>0 && r.bottom>0 && r.top < window.innerHeight,
          outline: cs.outlineStyle + ' ' + cs.outlineWidth + ' ' + cs.outlineColor,
          boxShadow: cs.boxShadow.slice(0,60),
          y: Math.round(r.top)};
}
"""

informe = []


def nuevo_contexto(pw, browser, ancho, alto, oscuro, extra=None):
    ctx = browser.new_context(viewport={"width": ancho, "height": alto},
                              device_scale_factor=1, locale="es-VE",
                              reduced_motion="no-preference")
    prefs = {
        "archive_session": json.dumps({**SESION, "ts": int(time.time() * 1000)}),
        "ds_dark_mode": "dark" if oscuro else "light",
        "ds_density": (extra or {}).get("densidad", "comfortable"),
        "ds_anim": (extra or {}).get("anim", "on"),
    }
    ctx.add_init_script("(() => { const p = %s; for (const k in p) localStorage.setItem(k, p[k]); })()"
                        % json.dumps(prefs))
    ctx.add_cookies([{"name": "ds_session", "value": TOKEN, "url": BASE}])
    return ctx


def capturar(page, nombre, diagnostico=True, foco=False):
    ruta = os.path.join(OUT, nombre + ".png")
    try:
        page.screenshot(path=ruta, full_page=True, timeout=20000)
    except Exception as e:
        page.screenshot(path=ruta, full_page=False)
    d = {}
    if diagnostico:
        try:
            d = page.evaluate(DIAG_JS)
        except Exception as e:
            d = {"error": str(e)[:200]}
    return ruta, d


def recorrer_foco(page, n=25):
    pasos = []
    for _ in range(n):
        page.keyboard.press("Tab")
        try:
            f = page.evaluate(FOCO_JS)
        except Exception:
            break
        pasos.append(f)
    return pasos


def main():
    filtro = sys.argv[1] if len(sys.argv) > 1 else ""
    os.makedirs(OUT, exist_ok=True)
    with sync_playwright() as pw:
        browser = pw.chromium.launch()
        for nombre, ruta, tipo in PAGINAS:
            if filtro and filtro not in nombre:
                continue
            for ancho, alto in ANCHOS:
                for oscuro in (False, True):
                    tema = "oscuro" if oscuro else "claro"
                    ctx = nuevo_contexto(pw, browser, ancho, alto, oscuro)
                    page = ctx.new_page()
                    errores, fallos = [], []
                    page.on("console", lambda m: errores.append(f"{m.type}: {m.text[:200]}")
                            if m.type in ("error", "warning") else None)
                    page.on("requestfailed", lambda r: fallos.append(f"{r.method} {r.url[:120]} {r.failure}"))
                    page.on("response", lambda r: fallos.append(f"HTTP {r.status} {r.url[:120]}")
                            if r.status >= 400 else None)
                    try:
                        page.goto(BASE + ruta, wait_until="load", timeout=30000)
                        page.wait_for_timeout(2500)
                    except Exception as e:
                        informe.append({"pagina": nombre, "cond": f"{ancho}-{tema}", "error": str(e)[:200]})
                        ctx.close(); continue
                    base = f"{nombre}-{ancho}-{tema}"
                    _, d = capturar(page, base)
                    informe.append({"pagina": nombre, "cond": f"{ancho}-{tema}", "captura": base + ".png",
                                    "url_final": page.url, "consola": errores[:12], "red": fallos[:12], **d})

                    # pestañas de los paneles de administración
                    if tipo == "admin":
                        suf = "archivo" if "archivo" in nombre else "rrhh"
                        for t in TABS_ADMIN:
                            try:
                                page.click(f"#tab-admin-{suf}-{t}", timeout=5000)
                                page.wait_for_timeout(1800)
                            except Exception as e:
                                informe.append({"pagina": nombre, "tab": t, "cond": f"{ancho}-{tema}",
                                                "error_click": str(e)[:120]})
                                continue
                            b2 = f"{nombre}-{t}-{ancho}-{tema}"
                            _, d2 = capturar(page, b2)
                            informe.append({"pagina": nombre, "tab": t, "cond": f"{ancho}-{tema}",
                                            "captura": b2 + ".png", "consola": errores[-6:],
                                            "red": fallos[-6:], **d2})
                    if tipo == "sistema":
                        for t in TABS_SISTEMA:
                            try:
                                page.click(f"a[href='#{t}']", timeout=5000)
                                page.wait_for_timeout(1800)
                            except Exception as e:
                                informe.append({"pagina": nombre, "tab": t, "cond": f"{ancho}-{tema}",
                                                "error_click": str(e)[:120]})
                                continue
                            b2 = f"{nombre}-{t.replace('pane-','')}-{ancho}-{tema}"
                            _, d2 = capturar(page, b2)
                            informe.append({"pagina": nombre, "tab": t, "cond": f"{ancho}-{tema}",
                                            "captura": b2 + ".png", "consola": errores[-6:],
                                            "red": fallos[-6:], **d2})
                    ctx.close()

            # condiciones extra a 1440 claro: densidad compacta, animaciones apagadas, foco
            for etiqueta, extra in (("compacto", {"densidad": "compact"}),
                                    ("sinanim", {"anim": "off"})):
                ctx = nuevo_contexto(pw, browser, 1440, 900, False, extra)
                page = ctx.new_page()
                try:
                    page.goto(BASE + ruta, wait_until="load", timeout=30000)
                    page.wait_for_timeout(2200)
                    _, d = capturar(page, f"{nombre}-1440-{etiqueta}")
                    informe.append({"pagina": nombre, "cond": f"1440-{etiqueta}",
                                    "captura": f"{nombre}-1440-{etiqueta}.png", **d})
                except Exception as e:
                    informe.append({"pagina": nombre, "cond": etiqueta, "error": str(e)[:160]})
                ctx.close()

            # foco de teclado
            for ancho_f in (390, 1440):
                ctx = nuevo_contexto(pw, browser, ancho_f, 900, False)
                page = ctx.new_page()
                try:
                    page.goto(BASE + ruta, wait_until="load", timeout=30000)
                    page.wait_for_timeout(2000)
                    pasos = recorrer_foco(page, 22)
                    page.screenshot(path=os.path.join(OUT, f"{nombre}-{ancho_f}-foco.png"), full_page=False)
                    informe.append({"pagina": nombre, "cond": f"{ancho_f}-foco",
                                    "captura": f"{nombre}-{ancho_f}-foco.png", "tabulacion": pasos})
                except Exception as e:
                    informe.append({"pagina": nombre, "cond": f"{ancho_f}-foco", "error": str(e)[:160]})
                ctx.close()
            print("hecho:", nombre, flush=True)
        browser.close()
    with open(os.path.join(OUT, "_diagnostico.json"), "a", encoding="utf-8") as f:
        for r in informe:
            f.write(json.dumps(r, ensure_ascii=False) + "\n")


if __name__ == "__main__":
    main()
