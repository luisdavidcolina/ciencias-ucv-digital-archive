"""Prueba de ejemplo para el fixture `client_as` (SI-226).

Objetivo: demostrar, contra un endpoint que YA comprueba autorización de
verdad hoy, que `client_as` permite simular un usuario concreto (con el
rol/módulo que tenga en base de datos) en vez de aplanar toda la suite a
"hay sesión válida" como hacía el fixture `client` original.

El endpoint elegido es GET /investigacion (routes/pages.py:
serve_investigacion), que exige sesión y además exige que el usuario
pertenezca al módulo "Global"; cualquier otro módulo recibe 403. Es el mismo
criterio documentado en SI-156 y es justo el tipo de comprobación que el
fixture `client` (con su override incondicional) no dejaba poner a prueba.

No se añade cobertura de endpoints que todavía no comprueban nada — eso es
alcance de la ola O1, según la ficha SI-226.
"""
from unittest.mock import patch


def test_investigacion_usuario_global_ve_la_pagina(client_as):
    """Un usuario del módulo Global sí puede ver /investigacion (200)."""
    c = client_as("admin_global")
    fila = {"modulo": "Global"}
    with patch("routes.pages.db_query", return_value=fila):
        res = c.get("/investigacion")
    assert res.status_code == 200


def test_investigacion_usuario_no_global_recibe_403(client_as):
    """Un usuario de otro módulo (p. ej. Archivo) recibe 403, no 200.

    Esto es exactamente lo que el fixture `client` original no podía
    ejercitar: con `require_session` aplanado a "test_user" siempre
    autorizado, no había forma de simular un usuario real con un rol/módulo
    equivocado para comprobar que el endpoint lo rechaza.
    """
    c = client_as("usuario_archivo")
    fila = {"modulo": "Archivo"}
    with patch("routes.pages.db_query", return_value=fila):
        res = c.get("/investigacion")
    assert res.status_code == 403


def test_investigacion_sin_fila_de_usuario_recibe_403(client_as):
    """Si el usuario no aparece en usuarios_sistema (o está inactivo), 403."""
    c = client_as("usuario_fantasma")
    with patch("routes.pages.db_query", return_value=None):
        res = c.get("/investigacion")
    assert res.status_code == 403


def test_investigacion_sin_sesion_recibe_401(anon_client):
    """Sin cookie/token de sesión, 401 antes de llegar a comprobar el módulo."""
    res = anon_client.get("/investigacion")
    assert res.status_code == 401
