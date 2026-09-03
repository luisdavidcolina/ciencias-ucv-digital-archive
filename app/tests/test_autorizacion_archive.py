"""Pruebas de autorización para `app/routes/archive.py` (carril A2-archivo-backend).

Contexto (docs/auditoria/buscador-archivo.md, BA-050/BA-051): la pantalla de
Archivo se documenta como "búsqueda pública", pero `checkPersistedSession()`
en el frontend expulsa al usuario anónimo — mientras que el endpoint real
`POST /api/archivo/buscar` **sí** es anónimo hoy (no lleva `require_session`).
Esa contradicción es una decisión de producto (¿el catálogo es público o no?)
que no le toca resolver a este carril: se anota en el buzón y se deja la
lectura tal cual está.

Lo que sí es responsabilidad de este carril es que **cualquier operación de
escritura** (crear, editar, borrar documento de Archivo) exija
`Depends(require_role("Archivo"))` o `require_admin_role("Archivo")`, igual
que ya demuestra `app/routes/backup.py` con `require_role("Global")`.

Al auditar `app/routes/archive.py` completo, hoy expone únicamente dos
endpoints, ambos de sólo lectura:

  - `POST /api/archivo/buscar`          (búsqueda full-text paginada)
  - `GET  /api/archivo/documentos/buscar` (autocompletar de tipos/descriptores)

No hay ningún endpoint de creación, edición ni borrado de documentos en este
archivo: ese CRUD vive en `app/routes/admin/docs.py`, que es el carril
`C1-docs-backend` (ver `docs/auditoria/PLAN-PARALELO.md`), no éste.

Estas pruebas dejan constancia de dos cosas:

1. Que la superficie de escritura de `archive.py` sigue siendo cero — si
   algún día se añade un endpoint de mutación aquí, esta prueba de inventario
   debe romperse y forzar a aplicarle `require_role`/`require_admin_role`
   antes de mergear (ver BA-003 · BR-001 en el buzón: BR-001 documenta el
   mismo patrón de escritura sin protección, pero sobre `docs.py`).
2. Que los endpoints de lectura existentes conservan su comportamiento
   anónimo actual (BA-051): esta suite NO los cierra ni los abre, sólo
   documenta el estado presente para que un cambio de ese estado sea
   deliberado y visible en el diff, no accidental.
"""
from fastapi.routing import APIRoute

from routes.archive import router as archive_router


def _rutas_por_metodo():
    """Mapa método HTTP -> lista de paths declarados en el router de archive.py."""
    mapa: dict[str, list[str]] = {}
    for route in archive_router.routes:
        if not isinstance(route, APIRoute):
            continue
        for metodo in route.methods:
            mapa.setdefault(metodo, []).append(route.path)
    return mapa


class TestInventarioDeEscrituraEnArchivePy:
    """`archive.py` no declara hoy ningún endpoint de mutación."""

    def test_no_hay_endpoints_put_patch_delete(self):
        mapa = _rutas_por_metodo()
        for metodo in ("PUT", "PATCH", "DELETE"):
            assert metodo not in mapa, (
                f"Apareció un endpoint {metodo} nuevo en archive.py "
                f"({mapa[metodo]}): debe llevar Depends(require_role(\"Archivo\")) "
                f"o require_admin_role(\"Archivo\") antes de mergear (ver "
                f"docs/auditoria/PLAN-PARALELO.md, carril A2-archivo-backend)."
            )

    def test_unico_post_es_la_busqueda_no_una_mutacion(self):
        mapa = _rutas_por_metodo()
        assert mapa.get("POST") == ["/api/archivo/buscar"], (
            "Si aparece un POST distinto de /api/archivo/buscar en archive.py, "
            "confirmar si es una mutación (crear documento): en ese caso exige "
            "require_role(\"Archivo\")."
        )

    def test_get_son_solo_los_de_busqueda_conocidos(self):
        mapa = _rutas_por_metodo()
        assert sorted(mapa.get("GET", [])) == ["/api/archivo/documentos/buscar"]


class TestLecturaSigueSinExigirSesionHoy:
    """BA-051: documenta el estado actual (anónimo) sin decidir el cambio de
    producto. No convertir en 401/403 sin que alguien resuelva antes BA-050."""

    def test_buscar_no_exige_sesion_hoy(self, anon_client):
        res = anon_client.post(
            "/api/archivo/buscar",
            json={"page": 1, "per_page": 1},
        )
        assert res.status_code != 401, (
            "El comportamiento de /api/archivo/buscar cambió de anónimo a "
            "protegido. Eso es la decisión de producto de BA-050/BA-051 — "
            "está bien que se tome, pero no como efecto colateral de este "
            "carril. Actualiza este test a propósito si fue deliberado."
        )

    def test_lookup_documentos_no_exige_sesion_hoy(self, anon_client):
        res = anon_client.get("/api/archivo/documentos/buscar", params={"q": "a"})
        assert res.status_code != 401
