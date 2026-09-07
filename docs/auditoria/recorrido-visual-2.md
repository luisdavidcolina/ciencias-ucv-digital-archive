# Segundo recorrido visual/funcional — 2026-09-07

Carril `RECORRIDO-visual-funcional-2`. A diferencia del primer recorrido (VI-001 a VI-080, todos
cerrados), este no se limita a renderizar cada página con datos falsos: interactúa de verdad
—abre el menú, entra a pestañas del backoffice, abre modales, rellena formularios, prueba el
buscador, la burbuja de IA, el interruptor de tema— sobre el arnés de `docs/auditoria/capturas/_harness/`
(`fakedb.py`/`capturar.py`/`estados.py`), a 390px y 1440px, claro y oscuro.

Este documento es sólo de investigación: **ningún archivo de código se tocó** en este carril. Las
capturas de cada paso quedan en `docs/auditoria/capturas/recorrido2-*.png`, con los logs de consola en
`recorrido2-log.txt`/`recorrido2-log2.txt`.

## Nota metodológica importante

El primer hallazgo que reportó este recorrido (varios 403 al entrar a pestañas admin y al ciclo de
propuestas de IA) se investigó antes de darlo por bueno, porque parecía demasiado central para ser un
bug real no visto en toda la sesión de hoy. Resultado: **es un hueco de fidelidad del arnés, no un bug
de producción**. `require_role`/`require_admin_role` (`app/routes/admin/deps.py`) hacen una consulta
real a `usuarios_sistema` por cada llamada, y `fakedb.py` genera filas genéricas sin fijar
`rol='Admin'`/`is_active=true` para el usuario de prueba `admin.global` — así que la autorización puede
fallar al azar en este montaje concreto. Eso explica por qué no se pudo probar en vivo la pestaña de
Retención de RRHH ni el ciclo de propuestas de `/admin/ia`: no están confirmados como rotos, están sin
probar todavía. Recomendado antes de repetir ese tramo: fijar la fila de `admin.global` en el arnés.

Ese mismo hallazgo sí destapó un bug de código real e independiente del arnés (#1 abajo).

## Hallazgos

### CRÍTICO

**1. `apiFetch` trata un 403 igual que un 401 y cierra la sesión entera**
`app/static/js/core/app.js:480-485`. Un 403 (acción prohibida para ese rol concreto, sesión válida)
dispara el mismo flujo que un 401 (sesión caducada): `logout()` ~1.8s después, sacando a la persona de
una sesión que seguía siendo válida. Es un bug real de código, verificado por lectura directa, no sólo
por lo que produjo el arnés — un 403 legítimo de cualquier endpoint (no sólo los del hueco de arriba)
tumbaría igual la sesión. Arreglo: sólo `logout()` en 401; un 403 debería mostrar un toast y quedarse
donde estaba.

### IMPORTANTE

**2. El panel de Personalización tapa el menú sin cerrar solo, a 390px**
Repro: 390×844, abrir el menú lateral, tocar "Personalización" (`.ds-sidebar-theme-btn` →
`#ds-theme-panel`). El panel se abre sin fondo oscuro (`backdrop`) y su subárbol intercepta los clics
sobre los enlaces del menú que quedan detrás — hay que cerrarlo a mano antes de poder navegar.
Capturas: `recorrido2-sidebar-390-*-themepanel.png`.

**3. Los gráficos de Chart.js truenan al re-renderizarse**
`app/static/js/admin/charts.js:319` y `:543`. Consola: `"Canvas is already in use... must be
destroyed"`, más un choque en `generateLabel` leyendo `.length` de `undefined`. Reproducible en cada
re-render del gráfico de Resumen (volver a la pestaña, cambiar de tema) tanto en `/admin/archivo` como
en `/admin/rrhh` — la instancia anterior de `Chart` nunca se destruye antes de crear una nueva sobre el
mismo `<canvas>`.

**4. Medidor de fortaleza de contraseña sin probar en vivo**
`admin/users.js:213-291` se ve bien construido por lectura de código (tres bandas de fortaleza,
`aria-live="polite"`), pero el hueco del arnés (#1 de la nota metodológica) impidió llegar a probarlo
de verdad con una fila de usuario real. Pendiente de un pase de seguimiento una vez arreglado el arnés.

### MENOR

**5. Accesibilidad: letra de atajo del menú sin separador semántico**
Las letras "A"/"R" del sidebar van concatenadas al texto del enlace en el mismo nodo de texto, sin
ningún separador — un lector de pantalla leería "Panel Archivo A" sin contexto de qué es esa letra
suelta.

**6. Botón "Cambiar estado" del monitor sin confirmar**
En la tabla de monitor de documentos, el botón no produjo ningún modal ni cambio visible al hacer
clic. No confirmado como roto de verdad (puede ser un selector que falló al probarlo) — apuntado para
revisar de cerca en un pase dedicado, no para arreglar a ciegas.

**7. `pageerror: "Unexpected end of input"` recurrente al cambiar de pestaña en admin/archivo**
En monitor y papelera. Probablemente un `JSON.parse` sobre una respuesta vacía o truncada —
posiblemente correlacionado con el mismo hueco de autorización del arnés que el hallazgo #1, no
necesariamente un bug de producción por sí solo.

## Confirmado funcionando bien

**8.** La burbuja del asistente de IA abre, acepta texto, y se degrada con gracia sin backend/API key
real (mensaje de error claro en línea, sin que JS truene).

**9.** El buscador público (`/archivo`, `/rrhh`) con términos reales y paginación funcionó
correctamente en los dos módulos — sin hallazgos nuevos más allá del ya documentado VI-058 (doble
paginador).

## Siguiente paso recomendado

Arreglar #1 (403≠401) y #3 (Chart.js) de inmediato — bien acotados, riesgo bajo, reproducidos por
lectura de código además de por interacción. #2 es de UI, también acotado. Antes de dar por buenos o
rotos #4/#6/#7, fijar la fila de `admin.global` en `fakedb.py` y repetir esa parte del recorrido.
