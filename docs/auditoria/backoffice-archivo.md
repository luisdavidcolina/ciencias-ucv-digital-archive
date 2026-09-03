# Auditoría del backoffice del módulo ARCHIVO

Revisión exhaustiva de `app/static/admin_archive.html` y de las ramas «archivo» de
`admin*.js`, más el backend en `app/routes/admin/` y `app/routes/trash.py`.

**Alcance recorrido**: las nueve pestañas (Resumen · Ingresar · Documentos · Tipos ·
Papelera · Retención · Auditoría · Acceso · Exportar), cada control, tabla, gráfico,
modal y estado, para los tres roles (usuario Archivo, admin Archivo, admin Global).

**Convenciones de este documento**

- `[CHOCA]` marca los pendientes que tocan `styles.css`, `app.js`, `app-core.js`,
  `app-shell.js`, `app-theme.js`, `admin.js`, `admin-ui.js`, `admin-charts.js`,
  `admin-submit.js`, `admin-monitor.js`, `admin-categories.js`, `admin-users.js`,
  `models.py`, `docs.py`, `stats.py`, `imports.py` o `trash.py` — todos compartidos
  con el módulo RRHH. Cualquier cambio ahí hay que verificarlo también en
  `admin_hr.html`.
- Esfuerzo: **S** ≤ media jornada · **M** 1–3 jornadas · **L** > 3 jornadas.
- Nada de lo que sigue propone deshacer decisiones ya tomadas: AdminLTE sigue sin
  cargarse, las nueve pestañas siguen agrupadas por verbo, y en ningún sitio se
  escribe «Tesauro».
- Total: **212 pendientes**.

---

## Índice

| Sección | Rango | Nº |
|---|---|---|
| A. Fallos reales con escenario de fallo | OA-001 … OA-034 | 34 |
| B. Seguridad y autorización | OA-035 … OA-050 | 16 |
| C. Organización, coherencia y nomenclatura | OA-051 … OA-064 | 14 |
| D. Pestaña Resumen | OA-065 … OA-081 | 17 |
| E. Pestaña Ingresar | OA-082 … OA-102 | 21 |
| F. Pestaña Documentos | OA-103 … OA-122 | 20 |
| G. Pestaña Tipos | OA-123 … OA-131 | 9 |
| H. Pestaña Papelera | OA-132 … OA-140 | 9 |
| I. Pestaña Retención | OA-141 … OA-150 | 10 |
| J. Pestaña Auditoría | OA-151 … OA-158 | 8 |
| K. Pestaña Acceso | OA-159 … OA-166 | 8 |
| L. Pestaña Exportar | OA-167 … OA-171 | 5 |
| M. Modales, foco y teclado | OA-172 … OA-182 | 11 |
| N. Estética y sistema visual | OA-183 … OA-194 | 12 |
| O. Responsive, modo oscuro y densidad | OA-195 … OA-201 | 7 |
| P. Funcionalidad ausente frente a AtoM / Alfresco / SharePoint / Archivematica | OA-202 … OA-206 | 5 |
| Q. Rendimiento, concurrencia y deuda técnica | OA-207 … OA-212 | 6 |

---

## A. Fallos reales con escenario de fallo

Ordenados por impacto. Todos tienen un camino concreto para reproducirlos.

### OA-001 · La retención de RRHH abre siempre vacía
`app/routes/admin/retention.py:42-43`
**Hoy**: el filtro por scope es `WHERE LOWER(c.slug) LIKE '%rrhh%'`. Los slugs de las
categorías de RRHH son `parte-i` … `parte-iv`; ninguno contiene «rrhh». La pestaña
Retención de `admin_hr.html` lista cero tipos y el usuario concluye que no hay nada
configurado. En Archivo funciona por casualidad: el slug es literalmente `archivo`.
**Debe**: filtrar por pertenencia explícita — `archivo` → `slug = 'archivo'`,
`rrhh` → `slug LIKE 'parte-%%'` — o añadir una columna `scope` a `categoria`.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/retention.py`, `app/tests/test_admin.py`.

### OA-002 · La fecha de vencimiento explícita no hace nada
`app/routes/admin/retention.py:115-120`, `app/static/admin_archive.html:671-673`
**Hoy**: el modal de edición ofrece «Vencimiento» con el `title` «sobreescribe el plazo
del tipo», y el formulario de alta ofrece lo mismo (`admin-submit.js:70-72`). El valor
se guarda en `datos_archivo.fecha_vencimiento`, pero la consulta de vencimientos calcula
siempre `fecha_documento + plazo_del_tipo`: la columna nunca se lee. Escenario: un acta
con vencimiento pactado a 2030 sigue apareciendo como vencida.
**Debe**: `COALESCE(da.fecha_vencimiento, da.fecha_documento + plazo)` en el cálculo, y
lo mismo en el KPI de `stats.py:129-134`.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/retention.py`, `app/routes/admin/stats.py` `[CHOCA]`, `app/tests/test_disposicion.py`.

### OA-003 · El KPI «Retención vencida» y la tabla de la pestaña Retención dan cifras distintas
`app/routes/admin/stats.py:129-134` vs `app/routes/admin/retention.py:119-123`
**Hoy**: el KPI cuenta cualquier documento con el plazo cumplido; la tabla excluye los
que no están `aprobado` y los que ya tienen `disposicion`. Escenario: se dispone de los
12 vencidos, la tabla dice «Sin vencimientos pendientes» y el KPI sigue marcando 12 en
rojo con la tarjeta en estado de alerta.
**Debe**: una sola definición de «vencido», idealmente una vista SQL que consuman ambos.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`, `app/routes/admin/retention.py`, `app/main.py`.

### OA-004 · Los contadores de estado del monitor incluyen la papelera
`app/routes/admin/docs.py:612-618`
**Hoy**: `get_status_counts` no filtra `deleted_at IS NULL`. Escenario: se mandan 5
borradores a la papelera; el badge «Borrador: 5» sigue en la cabecera del monitor, se
pulsa, y la tabla —que sí filtra— sale vacía.
**Debe**: añadir `WHERE deleted_at IS NULL` en ambas ramas.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/tests/test_admin.py`.

### OA-005 · Se puede cambiar el estado de un documento que está en la papelera
`app/routes/admin/docs.py:468-471`
**Hoy**: `update_documento_status` actualiza por PK sin comprobar `deleted_at`. Combinado
con OA-004, el badge lleva a un documento borrado y el cambio se aplica igual.
**Debe**: `AND deleted_at IS NULL` en el `UPDATE`, y 409 si está en papelera.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`.

### OA-006 · Purgar un documento deja su archivo digital en R2 para siempre
`app/routes/trash.py:107-118`
**Hoy**: el purgado borra descriptores, versiones y la fila, pero nunca el objeto de R2
ni los objetos de las versiones históricas. `docs/funcionalidades.md` §6 promete
«borrado físico, irreversible y en cascada … destruyendo archivos». No es cierto:
el fondo digital crece indefinidamente con huérfanos que nadie puede enumerar.
**Debe**: recoger los `file_url` de la fila y de `documento_versiones`, y llamar a
`storage.delete_object()` por cada clave `/api/files/…` antes de borrar las filas;
registrar en auditoría los objetos borrados.
**Esfuerzo**: M. **Archivos**: `app/routes/trash.py` `[CHOCA]`, `app/storage.py`, `app/tests/test_admin.py`.

### OA-007 · Purgar un empleado no borra las versiones de sus documentos
`app/routes/trash.py:167-169`
**Hoy**: se borran `historial_cargos`, `datos_rrhh` y `empleados`, pero no las filas de
`documento_versiones` que apuntaban a esos `id_rrhh`. Quedan versiones colgando de
documentos inexistentes, y el purgado de documentos sí las limpia (`trash.py:117`), así
que la incoherencia es interna.
**Debe**: borrar `documento_versiones` de los `id_rrhh` afectados antes de la cascada.
**Esfuerzo**: S. **Archivos**: `app/routes/trash.py` `[CHOCA]`.

### OA-008 · Restaurar una versión anterior destruye la versión actual
`app/routes/trash.py:238-254`
**Hoy**: `restore_version` sobrescribe `file_url` con el de la versión elegida sin
archivar antes el archivo que estaba vigente. Escenario: v3 es la buena, se restaura v1
por error, y v3 ya no está en ninguna parte. El botón se llama «Restaurar esta versión»
y no advierte de nada (`admin-edit.js:387`).
**Debe**: crear una versión nueva con el `file_url` actual antes de sustituirlo — el
historial de versiones sólo tiene sentido si es aditivo.
**Esfuerzo**: S. **Archivos**: `app/routes/trash.py` `[CHOCA]`, `app/tests/test_admin.py`.

### OA-009 · Subir un archivo nuevo en el modal de edición pierde el anterior en silencio
`app/static/admin-edit.js:135-165`, `app/static/admin_archive.html:737-739`
**Hoy**: al soltar un archivo en la zona del modal se sube y se escribe la URL nueva en
el campo; el `file_url` viejo se pierde en cuanto se pulsa Guardar. Existe un botón
«Guardar versión» pero es manual, está a la izquierda del pie y su `title` sólo se ve al
pasar el ratón. Escenario cotidiano: se sustituye un escaneo por otro mejor y el escaneo
original desaparece del sistema.
**Debe**: versionar automáticamente al reemplazar (el endpoint `POST /versiones` ya hace
exactamente eso), y dejar «Guardar versión» sólo como acción explícita adicional.
**Esfuerzo**: M. **Archivos**: `app/static/admin-edit.js`, `app/routes/trash.py` `[CHOCA]`.

### OA-010 · Un archivo subido y luego cancelado queda huérfano en R2
`app/static/admin-edit.js:154-158`
**Hoy**: la subida ocurre al soltar el archivo, no al guardar. Si se cierra el modal con
Cancelar o Escape, el objeto ya está en R2 y no lo referencia nadie ni lo borrará nada.
**Debe**: subir a una clave temporal y confirmarla al guardar, o registrar la clave en
una tabla de pendientes que barra el cron de respaldo.
**Esfuerzo**: M. **Archivos**: `app/static/admin-edit.js`, `app/routes/files.py`, `app/storage.py`.

### OA-011 · El límite de tamaño que anuncia la pantalla es ocho veces el real
`app/static/admin_archive.html:291`, `:314`, `app/routes/files.py:327-328`
**Hoy**: la zona de carga del alta dice «PDF, DOC, ZIP, PNG — máx 200 MB» y la tarjeta
oculta repite «Peso Máximo: 200MB». El backend rechaza por encima de 25 MB con 413, y el
modal de edición sí dice 25 MB (`admin_archive.html:686`). Escenario: se arrastra un PDF
de 60 MB, se rellena el formulario entero y sólo al guardar salta el error.
**Debe**: 25 MB en los tres sitios, y validar el tamaño en el cliente al seleccionar el
archivo, antes de rellenar nada.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-monitor.js` `[CHOCA]`.

### OA-012 · Las extensiones que anuncia la zona de carga no son las que acepta el servidor
`app/static/admin_archive.html:291` vs `app/storage.py` (`ALLOWED_EXTENSIONS`)
**Hoy**: se anuncian DOC y ZIP; el modal de edición acepta `.pdf,.png,.jpg,.jpeg,.tiff,.tif,.webp`
y el validador de `_uploadEditDocFile` (`admin-edit.js:136`) la misma lista. Un `.docx`
arrastrado en el alta se rechaza con 400 tras rellenar el formulario.
**Debe**: una única lista, servida desde `/api/choices` o desde una constante compartida,
y `accept=` en el `<input type="file">` del alta (que hoy no lo tiene, `:287`).
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/storage.py`, `app/routes/lookups.py`.

### OA-013 · La tabla de vencimientos se desalinea al cargar y al fallar
`app/static/admin_archive.html:471`
**Hoy**: el `<thead>` de la línea 468 declara **8** columnas (`# · Título · Tipo ·
Fecha Doc. · Plazo · Vencido · Ubicación · Disposición`) y la fila de carga usa
`colspan="7"`. `admin.js:126` y `:132` sí usan 8. Resultado: mientras carga, «Cargando…»
no queda centrado y la tabla salta al llegar los datos.
**Debe**: `colspan="8"`.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`.

### OA-014 · El nombre del CSV elegido nunca aparece
`app/static/admin_archive.html:263`, `app/static/admin-charts.js:435-439`
**Hoy**: la etiqueta lleva `<span id="csv-label-docs-archivo">Elegir archivo…</span>`, pero
el único listener de `change` que actualiza etiquetas busca `.custom-file-input`, que no
existe en esta página. Se elige el CSV, la barra sigue diciendo «Elegir archivo…», y no
hay forma de saber si quedó seleccionado.
**Debe**: actualizar el `<span>` por id desde el `change` del propio input, con el nombre
truncado y el tamaño.
**Esfuerzo**: S. **Archivos**: `app/static/admin-charts.js` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-015 · El enlace de compartición sale como código HTML cuando no hay portapapeles
`app/static/admin-monitor.js:336-341`, `app/static/admin-ui.js:78`
**Hoy**: el respaldo sin `navigator.clipboard` pasa marcado HTML a `confirmModal()`, que
lo inserta con `textContent`. El usuario ve literalmente
`<p class="small text-muted mb-2">Caduca en 72 horas.</p><input …>`.
Ocurre siempre en HTTP y en navegadores que niegan el permiso.
**Debe**: un modal propio para el enlace, con un `<input readonly>` real y un botón
«Copiar»; o un parámetro `html:true` explícito en `confirmModal`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`.

### OA-016 · El botón de confirmación de «Eliminar usuario» sale sin estilo
`app/static/admin-users.js:594`
**Hoy**: se pasa `"danger"` como clase, no `"btn-danger"`. `confirmModal` compone
`btn danger ds-cm-ok`: un botón blanco sin color que no se lee como destructivo. El resto
de llamadas del módulo sí pasan `btn-danger` (`admin-edit.js:225`, `:337`, `:355`).
**Debe**: `"btn-danger"`, y normalizar en `confirmModal` (`if (!btnClass.startsWith("btn-"))`).
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`.

### OA-017 · El backup por módulo se registra siempre como anónimo
`app/static/admin.js:102-104`
**Hoy**: usa `state.user?.usuario`. El objeto de sesión que devuelve `/api/auth/login`
expone `username`, nunca `usuario` (`app/routes/auth.py:45`). Se envía
`requester=` vacío y `X-User: ""`; el `backup_history` y la auditoría no saben quién
descargó el fondo completo. Es exactamente el evento que más falta hace rastrear.
**Debe**: `state.user?.username`. Conviene además una prueba que recorra los `.js` en
busca de `state.user?.usuario`.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/tests/test_static_analysis.py`.

### OA-018 · Los nombres de tipología se inyectan sin escapar
`app/static/admin-categories.js:360`, `:375`, `:386`
**Hoy**: `<h6 …>${t}</h6>` con `t` viniendo de `tipo_documento.nombre`, que cualquier
usuario con sesión puede crear vía `POST /add_category` (OA-035). Una tipología llamada
`<img src=x onerror=…>` se ejecuta en el panel de todos los administradores. El resto del
archivo sí usa `escHtml` (`:429`), así que es un olvido puntual.
**Debe**: `escHtml(t)` en los tres puntos, y una guarda de análisis estático que exija
`escHtml` en cualquier interpolación dentro de `innerHTML`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-categories.js` `[CHOCA]`, `app/tests/test_static_analysis.py`.

### OA-019 · La vista previa del modal de edición admite inyección por atributo
`app/static/admin-edit.js:26`, `:28`, `:30`, `:125-129`
**Hoy**: la URL se interpola cruda dentro de `src="…"` / `href="…"`. El campo
«/api/files/… o URL externa» (`admin_archive.html:691`) es editable a mano, así que basta
guardar `x" onload="…` para inyectar atributos. `_secureFileUrl` sólo comprueba el esquema.
**Debe**: `escHtml(url)` en los cuatro puntos, o construir los nodos con `createElement` y
`setAttribute`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit.js`.

### OA-020 · El formulario de alta no puede crear un borrador
`app/routes/admin/docs.py:253-277`, `app/models.py:97`
**Hoy**: `DocumentSubmitRequest` declara `status`, pero el `INSERT` de Archivo no lo
incluye: todo lo que se ingresa nace con el valor por defecto de la columna. El flujo
`draft → revision → aprobado` que documenta `funcionalidades.md` §4 sólo se puede
recorrer editando después. La pantalla de alta ni siquiera muestra el selector de estado
que sí tiene el modal de edición (`admin_archive.html:727`).
**Debe**: incluir `status` (validado contra `VALID_STATUS`) en el `INSERT` y exponer el
selector en el alta, con «Borrador» por defecto para el rol Normal.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/static/admin-submit.js` `[CHOCA]`, `app/tests/test_sql_inserts.py`.

### OA-021 · La auditoría registra la creación antes de intentarla
`app/routes/admin/docs.py:247`, `app/routes/admin/catalog.py:327`
**Hoy**: `log_event` se llama en la primera línea del handler. Si el `INSERT` revienta
—tipología inexistente, fecha inválida, caída de Neon— el registro de auditoría dice que
el documento se creó. Un archivo cuya traza miente sobre lo que pasó no sirve para nada.
**Debe**: registrar después del commit, con el id resultante en el detalle; y registrar el
fallo con `status="Failure"` en el `except`.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/routes/admin/catalog.py` `[CHOCA]`.

### OA-022 · Las importaciones CSV no son transaccionales ni idempotentes
`app/routes/admin/imports.py:369-438`
**Hoy**: cada fila hace su propio `commit`, y el `except` por fila continúa. Un CSV de 500
líneas que falla en la 300 deja 299 documentos creados y ninguna forma de deshacerlo salvo
borrarlos a mano uno a uno desde el monitor. Reimportar el mismo archivo duplica todo: no
hay clave natural ni detección de duplicados.
**Debe**: una transacción por lote con opción «todo o nada»; una previsualización en seco
(`dry_run=true`) que informe filas válidas, filas con error y duplicados antes de escribir;
y una clave de deduplicación (título + fecha + tipo, o una columna `id_externo`).
**Esfuerzo**: L. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`, `app/database.py`, `app/static/admin-charts.js` `[CHOCA]`, `app/tests/test_sql_inserts.py`.

### OA-023 · La importación CSV no admite media docena de campos que sí tiene el alta
`app/routes/admin/imports.py:381-393`
**Hoy**: se importan título, autor, fecha, tipo, abstract, ubicación, folio, soporte,
páginas y palabras clave. Quedan fuera `tesauro_secundario` (la Clasificación, que en el
alta es obligatoria), `personas_relacionadas`, `idioma`, `fecha_vencimiento` y `status`.
Un fondo cargado por CSV nace sin clasificación y hay que editarlo documento a documento.
**Debe**: aceptar las mismas columnas que el formulario, documentadas en el `title` de la
ayuda (`admin_archive.html:268-269`) y en una plantilla descargable.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-024 · El número de versión se calcula con una carrera abierta
`app/routes/trash.py:214-227`
**Hoy**: `SELECT MAX(version_num)+1` y luego `INSERT`, sin bloqueo ni restricción única.
Dos administradores versionando el mismo documento a la vez generan dos filas `v3`; el
historial muestra dos entradas indistinguibles y `ORDER BY version_num DESC` las ordena al
azar.
**Debe**: `UNIQUE (tabla, documento_id, version_num)` y calcular con
`INSERT … SELECT COALESCE(MAX(version_num),0)+1 …` en una sola sentencia, reintentando al
conflicto.
**Esfuerzo**: S. **Archivos**: `app/routes/trash.py` `[CHOCA]`, `app/main.py` (migración).

### OA-025 · Guardar en el modal de edición pisa los cambios de otro sin avisar
`app/static/admin-edit.js:181-218`, `app/routes/admin/docs.py:346-412`
**Hoy**: el `PUT` no envía ni comprueba `updated_at`. Dos personas con el mismo documento
abierto: la última en pulsar Guardar se lleva el registro entero, incluidas las palabras
clave, que se borran y se reinsertan (`docs.py:402-408`). El otro no se entera.
**Debe**: enviar el `updated_at` leído al abrir y responder 409 si no coincide, con un
aviso que ofrezca recargar; el dato ya viaja en `GET /documento/{id}` (`docs.py:210`).
**Esfuerzo**: M. **Archivos**: `app/static/admin-edit.js`, `app/routes/admin/docs.py` `[CHOCA]`, `app/models.py` `[CHOCA]`.

### OA-026 · Guardar deja el modal abierto contando el fallo como éxito visual
`app/static/admin-edit.js:216-218`
**Hoy**: el `catch` es ciego (`catch {}`) y muestra «Error al actualizar el documento»
siempre igual, perdiendo el `detail` del servidor. Un 422 de Pydantic (páginas negativas,
fecha mal formada) llega como el mismo mensaje genérico que una caída de red, y el usuario
no sabe qué campo corregir.
**Debe**: mostrar `e.message` y, si el detalle identifica un campo, marcarlo con
`is-invalid` y su `.invalid-feedback`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit.js`.

### OA-027 · Guardar cierra el modal sin refrescar la fila cuando falla el monitor
`app/static/admin-edit.js:213-215`
**Hoy**: se cierra el modal y se llama a `loadMonitorTable()`, que si falla sólo hace
`console.error` (`admin-monitor.js:92-94`). La tabla se queda con los datos viejos y sin
ningún aviso: el título que se acaba de cambiar sigue mostrando el anterior.
**Debe**: estado de error visible en la tabla del monitor y reintento; y actualización
optimista de la fila editada.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin-edit.js`.

### OA-028 · El filtro «Persona» del monitor de Archivo sólo conoce la página actual
`app/static/admin-monitor.js:78-88`
**Hoy**: para Archivo, la lista de personas se deduce de `data.records`, es decir de los
25 documentos visibles, y sólo en la primera carga (`options.length <= 1`). Con 4.000
documentos, el desplegable ofrece los autores de la primera página y nada más. Además el
filtro se llama «Persona» pero busca en `autor` (`docs.py:62`).
**Debe**: servir los autores distintos desde `/api/choices` o desde un endpoint propio, y
renombrar el control a «Autor / Ente emisor», que es el campo real.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/routes/lookups.py`, `app/static/admin_archive.html`.

### OA-029 · El desplegable de tipología se rellena con el primer texto y ya no cambia
`app/static/admin-monitor.js:70-75`
**Hoy**: la opción inicial del HTML es `Tipo...` (`admin_archive.html:342`) y el JS la
sustituye por `Filtrar por Tipología...`. Dos etiquetas para el mismo control según el
momento; y como sólo se rellena si `options.length <= 1`, crear una tipología nueva no la
añade al filtro hasta recargar la página entera.
**Debe**: una sola etiqueta y repoblar tras `loadDynamicChoices()`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-030 · Las palabras clave se cuelgan del panel equivocado al cambiar de módulo
`app/static/admin-categories.js:398-407`
**Hoy**: la sección se busca por el id global `admin-keywords-section`, pero se inserta
dentro del pane del módulo activo. En un admin Global que navega entre paneles, la sección
existente se reutiliza aunque cuelgue del pane del otro módulo, y las palabras clave dejan
de verse en el panel donde se está.
**Debe**: sufijar el id por módulo (`admin-keywords-section-${suf}`), como el resto.
**Esfuerzo**: S. **Archivos**: `app/static/admin-categories.js` `[CHOCA]`.

### OA-031 · La búsqueda del monitor dispara una petición por tecla
`app/static/app.js:294-295`
**Hoy**: el `input` llama directamente a `loadMonitorTable()`. Escribir «resolución» son
diez peticiones a Neon, y como las respuestas no se cancelan ni se ordenan, una lenta
puede pisar a una posterior y dejar en pantalla resultados de un prefijo. Existe
`debounce()` en `admin-ui.js:7` y las búsquedas públicas sí lo usan (420 ms).
**Debe**: `debounce(loadMonitorTable, 350)` y descartar respuestas obsoletas con un
contador de petición o `AbortController`.
**Esfuerzo**: S. **Archivos**: `app/static/app.js` `[CHOCA]`, `app/static/admin-monitor.js` `[CHOCA]`.

### OA-032 · Lo mismo en el buscador de auditoría
`app/static/app.js:288-291`
**Hoy**: cada pulsación lanza `loadAuditTab()` sobre un `audit_log` que puede tener
cientos de miles de filas, con `ILIKE` sin índice sobre dos columnas (`catalog.py:369`).
**Debe**: `debounce` de 350 ms más un índice trigram sobre `accion` y `usuario`.
**Esfuerzo**: S. **Archivos**: `app/static/app.js` `[CHOCA]`, `app/routes/admin/catalog.py` `[CHOCA]`, `app/main.py`.

### OA-033 · Los KPIs de la cabecera mezclan cifras filtradas y sin filtrar
`app/static/admin-stats.js:16-32` vs `app/static/admin-charts.js:129-133`
**Hoy**: «Documentos» y «Tipologías» vienen de `POST /stats`, que aplica el rango de
fechas del filtro analítico. Las otras seis tarjetas vienen de `GET /charts`, que ignora
ese rango. Escenario: se filtra 2024, «Documentos: 120» y «Digitalizados: 3.400» conviven
en la misma fila, y el porcentaje del subtítulo se calcula contra el total sin filtrar
(`admin-charts.js:137`), dando cifras imposibles.
**Debe**: que `/charts` acepte el mismo rango y sea la única fuente de la fila de KPIs; o
etiquetar visiblemente qué tarjetas responden al filtro.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`, `app/static/admin-stats.js` `[CHOCA]`, `app/static/admin-charts.js` `[CHOCA]`.

### OA-034 · El filtro analítico acepta rangos invertidos y no valida nada
`app/static/admin_archive.html:206-208`, `app/routes/admin/stats.py:29-30`
**Hoy**: con «desde» posterior a «hasta» el filtro `df[…] & df[…]` devuelve vacío y el
panel muestra ceros como si el archivo estuviese vacío. Tampoco hay `min`/`max` en los
`<input type="date">` ni comprobación en el borde.
**Debe**: validar en el cliente (intercambiar o avisar), acotar con `max` al día de hoy, y
devolver 400 desde el servidor si el rango es imposible.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/routes/admin/stats.py` `[CHOCA]`.

---

## B. Seguridad y autorización

### OA-035 · Todo `/api/admin` sólo exige sesión, nunca rol ni módulo
`app/routes/admin/__init__.py:12-16`
**Hoy**: el único `Depends` es `require_session`. Un usuario **Normal** del módulo
**RRHH** puede, con `curl` y su propia cookie: crear usuarios Admin
(`POST /api/admin/users/create`), borrar palabras clave del Archivo, cambiar plazos de
retención, purgar la papelera y descargar el fondo entero. El control de acceso vive sólo
en `configureSidebarVisibilities()` (`app.js:139-158`), que es JavaScript en el navegador.
**Debe**: una dependencia `require_admin(modulo)` que resuelva el rol y el módulo del
usuario de la sesión contra `usuarios_sistema` y devuelva 403; el `modulo` de la petición
no puede seguir siendo un parámetro que el cliente elige.
**Esfuerzo**: L. **Archivos**: `app/routes/admin/deps.py` `[CHOCA]`, `app/routes/admin/__init__.py` `[CHOCA]`, `app/routes/trash.py` `[CHOCA]`, `app/routes/files.py`, `app/tests/test_admin.py`.

### OA-036 · La identidad de quien actúa la pone el cliente
`app/routes/admin/docs.py:436`, `:459`, `app/routes/trash.py:76`, `:98`
**Hoy**: `usuario` y `requester` son parámetros de consulta. Cualquiera con sesión puede
purgar un documento firmando el evento con el nombre de otro. La auditoría —el motivo por
el que existe un archivo institucional— es falsificable por diseño.
**Debe**: derivar el actor de `require_session` y eliminar el parámetro; conservar el
parámetro sólo como comprobación de coherencia durante la transición.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/deps.py` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`, `app/routes/trash.py` `[CHOCA]`, `app/routes/admin/retention.py`, `app/routes/admin/users.py` `[CHOCA]`.

### OA-037 · Un usuario puede quitarse el propio rol o borrar al último administrador
`app/routes/admin/users.py:221-243`
**Hoy**: ni `toggle_user_active` ni `delete_user` comprueban que quede al menos un Admin
activo por módulo, ni impiden actuar sobre uno mismo. Escenario: el único admin de Archivo
se desactiva por error y nadie puede volver a entrar al panel.
**Debe**: rechazar con 409 si la operación deja el módulo sin ningún Admin activo, y
rechazar la auto-desactivación y el auto-borrado.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/users.py` `[CHOCA]`, `app/tests/test_admin.py`.

### OA-038 · Se puede crear un usuario Admin de cualquier módulo desde el panel de Archivo
`app/routes/admin/users.py:184-205`, `app/static/admin-ui.js:406-415`
**Hoy**: el formulario fija el `<select>` de módulo al del panel, pero el endpoint acepta
cualquier valor —incluido `Global`— sin validarlo contra un catálogo ni contra el rol de
quien crea. Una petición directa da acceso Global.
**Debe**: validar `modulo` contra `("Archivo","RRHH","Global")`, exigir que quien crea sea
Admin de ese módulo, y que sólo un admin Global pueda crear cuentas `Global`.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/users.py` `[CHOCA]`, `app/models.py` `[CHOCA]`.

### OA-039 · No hay política de contraseñas más allá de seis caracteres
`app/routes/admin/users.py:210-211`, `app/static/admin-users.js:612`
**Hoy**: `123456` es válida. No se comprueba longitud en la creación (sólo en el cambio),
ni se fuerza el cambio en el primer acceso, ni hay caducidad, ni se impide reutilizar la
anterior.
**Debe**: mínimo 12 caracteres, comprobación contra una lista de contraseñas comunes,
marca `debe_cambiar_password` en la creación y un medidor de fortaleza en el formulario.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/users.py` `[CHOCA]`, `app/core/security.py`, `app/static/admin-users.js` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`.

### OA-040 · La contraseña nueva se escribe en un campo de texto plano
`app/static/admin-ui.js:43`, `app/static/admin-users.js:610`
**Hoy**: `promptModal` usa `<input type="text">`. Cambiar la contraseña de un usuario
deja la nueva a la vista de cualquiera que pase por detrás, y el navegador la ofrece para
autocompletar en el siguiente campo de texto.
**Debe**: un parámetro `type` en `promptModal` y `type="password"` con botón de mostrar.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/static/admin-users.js` `[CHOCA]`.

### OA-041 · La columna «Contraseña» de la tabla de Acceso no aporta nada y sugiere lo contrario
`app/static/admin-users.js:540`, `:550`, `app/routes/admin/users.py:179`
**Hoy**: el backend inyecta `"••••••••"` fijo en cada fila y la tabla lo pinta. Ocupa una
columna entera de una tabla de siete que no cabe en tablet, y hace creer que el sistema
guarda contraseñas recuperables.
**Debe**: eliminar la columna y el campo del payload; sustituirlos por «Contraseña
actualizada el …», que sí es información.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`, `app/routes/admin/users.py` `[CHOCA]`.

### OA-042 · El servicio de archivos autentica con un nombre de usuario en la URL
`app/routes/files.py:346-354`
**Hoy**: `/api/files/{key}?u=<usuario>` sólo comprueba que ese usuario exista y esté
activo. El nombre de usuario no es un secreto: aparece en la barra superior, en la tabla
de Acceso y en la auditoría. Cualquiera con sesión —o el propio parámetro filtrado en un
`Referer` o en los registros del proxy— puede leer archivos de RRHH desde el panel de
Archivo.
**Debe**: resolver el usuario desde `require_session` e ignorar `u`; comprobar además que
la clave pertenece a un módulo al que ese usuario tiene acceso.
**Esfuerzo**: M. **Archivos**: `app/routes/files.py`, `app/static/app-core.js` `[CHOCA]`.

### OA-043 · Los enlaces a archivos llevan el nombre de usuario a sitios de terceros
`app/static/app-core.js:67-75`, `app/static/admin-monitor.js:158`
**Hoy**: el `?u=` se añade a un `<a target="_blank">`. El navegador manda `Referer` al
destino, y para archivos externos (`https://…`) la URL no se toca pero la de R2 sí.
**Debe**: además de OA-042, `rel="noopener noreferrer"` en todos los `target="_blank"`
del panel (`admin-monitor.js:158`, `admin-edit.js:30`, `:129`).
**Esfuerzo**: S. **Archivos**: `app/static/app-core.js` `[CHOCA]`, `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin-edit.js`.

### OA-044 · La sesión se puede extender indefinidamente desde el navegador
`app/static/admin-ui.js:229-240`
**Hoy**: «Extender sesión» reescribe la marca de tiempo en `localStorage` y nada más. La
cookie HMAC del servidor no se renueva, así que o bien la sesión real ya caducó —y el
botón miente— o bien el TTL de 12 h del cliente es decorativo.
**Debe**: llamar a `/api/auth/restore` para renovar la cookie y reflejar la respuesta; si
falla, cerrar sesión.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/routes/auth.py`.

### OA-045 · El aviso de sesión sólo salta si la pestaña está abierta en el minuto justo
`app/static/admin-ui.js:191-226`
**Hoy**: `_warned` es una variable de módulo y el intervalo es de 30 s: si el portátil
estaba suspendido durante la ventana de aviso, no se avisa nunca y el trabajo del
formulario de alta se pierde al primer 401.
**Debe**: comprobar el tiempo restante también al volver el foco (`visibilitychange`), y
guardar el borrador antes de expulsar (ver OA-089).
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`.

### OA-046 · Purgar es irreversible y se confirma con un solo clic
`app/static/admin-edit.js:336-344`
**Hoy**: «Esta acción es irreversible. ¿Continuar?» y un botón. En la misma columna, a
seis píxeles del botón de restaurar (`admin-edit.js:276-277`), con iconos de tamaño
`btn-xs` inexistente (OA-183). Un clic desviado destruye un documento para siempre.
**Debe**: para el purgado —y sólo para él— exigir escribir el título del documento, como
hacen los sistemas de gestión documental serios; mostrar en el diálogo qué se va a
destruir (archivo digital, versiones, descriptores).
**Esfuerzo**: M. **Archivos**: `app/static/admin-edit.js`, `app/static/admin-ui.js` `[CHOCA]`.

### OA-047 · La auditoría no registra ni las consultas ni las descargas
`app/routes/admin/docs.py:192-242`, `app/routes/files.py:346`
**Hoy**: se registran altas, cambios y borrados, pero no quién consultó un expediente ni
quién descargó un archivo. La compartición externa sí lo hace (`share.py`), lo cual
subraya la asimetría: se rastrea al de fuera y no al de dentro.
**Debe**: registrar `GET /documento/{id}` y `GET /api/files/{key}` con muestreo o
agregación diaria para no inflar la tabla.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/routes/files.py`, `app/main.py`.

### OA-048 · La auditoría es borrable y no está sellada
`app/routes/admin/catalog.py`
**Hoy**: `audit_log` es una tabla normal. Quien tenga acceso a la base puede editarla, y
nada permite demostrar después que no se tocó.
**Debe**: encadenar cada evento con el hash del anterior (`prev_hash`), o al menos un
sello diario firmado que se archive junto al respaldo.
**Esfuerzo**: L. **Archivos**: `app/database.py`, `app/main.py`, `app/routes/admin/catalog.py` `[CHOCA]`.

### OA-049 · No hay límite de peticiones en ningún endpoint del panel
`app/routes/admin/__init__.py`
**Hoy**: `POST /import/documentos` lee el archivo entero en memoria sin tope de tamaño
(`imports.py:359`) y `GET /backup/export` devuelve la base completa. Una sesión legítima
basta para agotar la memoria del lambda o para exfiltrar todo en un bucle.
**Debe**: tope de tamaño en la subida de CSV, límite de peticiones por usuario en los
endpoints de exportación e importación, y registro de los intentos rechazados.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`, `app/main.py`, `app/routes/backup.py`.

### OA-050 · La página carga seis dependencias de tres CDN sin verificación de integridad
`app/static/admin_archive.html:7-16`, `:758-759`
**Hoy**: fuentes de Google, Font Awesome, Bootstrap, Chart.js y jQuery sin `integrity` ni
`crossorigin`, y sin Content-Security-Policy. Un CDN comprometido ejecuta código en un
panel que ve el fondo documental completo.
**Debe**: `integrity`+`crossorigin` en cada etiqueta, y una CSP con `script-src` acotado a
los orígenes concretos.
**Esfuerzo**: M. **Archivos**: `app/static/admin_archive.html`, `app/main.py`, `app/tests/test_static_assets.py`.

---

## C. Organización, coherencia y nomenclatura

### OA-051 · Los plazos de retención se editan en dos pestañas a la vez
`app/static/admin.js:18`, `:21`
**Hoy**: `loadAdminTab("categories")` llama a `loadRetentionConfig()` y
`loadAdminTab("retencion")` también. La misma tabla, editable, en «Tipos» y en
«Retención»: se cambia el plazo en una y la otra sigue mostrando el anterior hasta
recargar. Es exactamente la partición que el `CHANGELOG` 3.3.0 dice haber deshecho.
**Debe**: los plazos viven sólo en «Retención»; en «Tipos» basta una columna de sólo
lectura con enlace a esa pestaña.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-052 · El pane de «Tipos» y el de «Retención» comparten el mismo `tbody`
`app/static/admin_archive.html:451` (`retencion-tipos-body-archivo`)
**Hoy**: `loadRetentionConfig()` busca ese id, que sólo existe dentro del pane de
Retención. Al entrar por «Tipos» se hace la petición, se pinta una tabla que el usuario no
está viendo, y el `input-group` de años queda en el árbol de accesibilidad de un panel
oculto.
**Debe**: consecuencia directa de OA-051; una vez separado, no cargar lo que no se ve.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-053 · La tabla de vencimientos usa ids sin sufijo de módulo
`app/static/admin_archive.html:470` (`vencimientos-table-body`), `:477`
**Hoy**: todos los demás elementos del panel llevan `-archivo` / `-rrhh`. Estos dos no.
En el momento en que RRHH tenga vencimientos —el plan lo contempla— los dos paneles
escribirán en el mismo nodo.
**Debe**: sufijar y ajustar `loadVencimientosTable()`, que hoy busca el id pelado
(`admin.js:123-124`).
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/admin.js` `[CHOCA]`.

### OA-054 · Cuatro nombres para el mismo concepto
`admin_archive.html:96` («Tipologías»), `:149` («Documentos»), `:335` («Directorio Activo
Local»), `:396` («Nueva Categoría»), `:400` («Nombre de la Tipología»), `:419`
(«Taxonomías Activas»), `admin.js:41` («Documentos del archivo»)
**Hoy**: la pestaña se llama «Tipos», la tarjeta «Nueva Categoría», la lista «Taxonomías
Activas», el KPI «Tipologías» y el campo «Nombre de la Tipología». Cuatro palabras para
`tipo_documento`.
**Debe**: «Tipo documental» en toda la interfaz, que es el término archivístico y el que
usa la pestaña.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-categories.js` `[CHOCA]`.

### OA-055 · «Directorio Activo Local» no significa nada
`app/static/admin_archive.html:335`
**Hoy**: el encabezado de la tarjeta del monitor dice «Directorio Activo Local» —que en
informática es otra cosa muy distinta— hasta que `loadAdminTab` lo reescribe como
«Documentos del archivo» (`admin.js:39-41`). O sea: el título correcto existe, pero llega
un instante después y el usuario ve el incorrecto al cargar.
**Debe**: escribir «Documentos del archivo» directamente en el HTML y borrar la
reescritura por JavaScript.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/admin.js` `[CHOCA]`.

### OA-056 · «Clasificación» se pide como Parte I–IV, que es vocabulario de RRHH
`app/static/admin-submit.js:27-33`
**Hoy**: el alta de Archivo obliga a elegir entre «Parte I … Parte IV», las cuatro partes
del expediente de personal. En un fondo institucional eso no clasifica nada, pero es campo
obligatorio, así que todo el mundo deja «Parte I» y el campo queda inservible. El modal de
edición, en cambio, lo pide como texto libre (`admin_archive.html:621`).
**Debe**: un cuadro de clasificación propio del Archivo (ver OA-202); mientras tanto, un
catálogo servido desde `/api/choices` y el mismo control en alta y edición.
**Esfuerzo**: M. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/static/admin_archive.html`, `app/routes/lookups.py`.

### OA-057 · «Retención Física» significa ubicación
`app/static/admin-submit.js:152` (rama RRHH), `app/static/admin_archive.html:437`
**Hoy**: la etiqueta «Retención Física» de la rama RRHH designa el estante donde está la
caja, mientras que en Archivo «Retención» designa el plazo de conservación ISO 15489. Dos
significados incompatibles de la misma palabra en el mismo backoffice.
**Debe**: «Ubicación física» para el estante; «Retención» reservado al plazo.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`.

### OA-058 · La pestaña «Exportar» sólo exporta, pero se llama como el grupo «Administrar»
`app/static/admin_archive.html:483-496`
**Hoy**: un botón y un párrafo. Es el pane más pobre de los nueve y está en el grupo
«Administrar» junto a «Acceso», que es una pantalla completa de gestión de cuentas.
**Debe**: convertirla en «Datos» y reunir ahí exportación, importación CSV (hoy escondida
en «Ingresar», OA-082) y el historial de importaciones y exportaciones.
**Esfuerzo**: M. **Archivos**: `app/static/admin_archive.html`, `app/static/admin.js` `[CHOCA]`, `app/tests/test_admin_panels.py`.

### OA-059 · El breadcrumb se reescribe en cada cambio de pestaña con el mismo texto
`app/static/admin.js:30-33`
**Hoy**: `Panel de Control / Administración - Archivo` se reconstruye entero en cada clic,
y no dice en qué pestaña se está. La ruta real es tres niveles: módulo → panel → pestaña.
**Debe**: `Archivo / Administración / Retención`, actualizando sólo el último tramo.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-060 · La pestaña activa no queda en la URL
`app/static/admin.js:4-13`
**Hoy**: `loadAdminTab` no toca el historial. Recargar F5 devuelve siempre a «Resumen», no
se puede enviar a un compañero el enlace de «Retención», y el botón Atrás del navegador
sale del panel entero.
**Debe**: `history.replaceState` con `#retencion` y leerlo al arrancar; encaja con el
`?docId=` que ya se maneja en `app.js:74-86`.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/static/app.js` `[CHOCA]`.

### OA-061 · «Papelera» está en «Gestionar» y «Retención» en «Controlar», pero disponer y purgar son la misma decisión
`app/static/admin_archive.html:157-172`
**Hoy**: el archivero que decide expurgar un documento vencido tiene que ir a «Retención»
(Controlar) para registrar la disposición y a «Papelera» (Gestionar) para retirarlo. Son
dos pestañas de dos grupos distintos para un solo acto administrativo.
**Debe**: mantener la agrupación por verbo —que es correcta— pero enlazar desde la fila de
disposición «Eliminado por expurgo» a la acción de envío a papelera, con el acta ya
asociada.
**Esfuerzo**: M. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/routes/admin/retention.py`.

### OA-062 · El historial de disposiciones existe en el backend y no se ve en ninguna parte
`app/routes/admin/retention.py:199-220`
**Hoy**: `GET /retencion/disposiciones` devuelve el registro completo de decisiones
—acta, responsable, fecha— y ninguna pestaña lo consume. Es el único registro que un
archivo tiene que poder enseñar en una inspección.
**Debe**: una tabla «Disposiciones registradas» bajo la de vencidos, con filtro por tipo
de decisión y exportación a CSV.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/admin.js` `[CHOCA]`.

### OA-063 · La bandeja de pendientes existe en el backend y tampoco se ve
`app/routes/admin/docs.py:479-517`
**Hoy**: `GET /documentos/pendientes` está implementado y documentado en
`funcionalidades.md` §4 como «Bandeja de Pendientes». En la interfaz sólo hay un badge que
preselecciona un filtro del monitor (`admin-monitor.js:110-118`), lo cual no es una
bandeja: no hay aprobar/rechazar en lote, ni orden por antigüedad de espera, ni quién la
dejó ahí.
**Debe**: usar el endpoint en una vista propia dentro de «Documentos», con selección
múltiple y aprobación en lote.
**Esfuerzo**: M. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-monitor.js` `[CHOCA]`.

### OA-064 · El modal de expediente de RRHH está incrustado en la página de Archivo
`app/static/admin_archive.html:571-583`
**Hoy**: `#rrhh-person-modal` con `modal-xl` completo, y `admin-edit-hr.js` cargado
(`:767`), en un panel donde no hay ni un empleado. Son ~13 KB de marcado y script muertos
en cada carga, y ruido para quien lea el archivo.
**Debe**: quitar el modal y el `<script>` de `admin_archive.html`; el simétrico en
`admin_hr.html` con el modal de Archivo.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/tests/test_paginas.py`.

---

## D. Pestaña Resumen

### OA-065 · Ocho KPIs sin jerarquía: todos pesan lo mismo
`app/static/admin_archive.html:52-128`
**Hoy**: ocho tarjetas idénticas en una rejilla `auto-fit`. «Documentos» (la cifra que
define el fondo) y «Autores» (una curiosidad) tienen el mismo tamaño, el mismo peso
tipográfico y la misma prominencia. A 1440 px salen ocho en fila y la vista se lee como un
listado, no como un tablero.
**Debe**: dos o tres cifras principales grandes (fondo, digitalización, pendientes) y el
resto en una tira secundaria menor.
**Esfuerzo**: M. **Archivos**: `app/static/admin_archive.html`, `app/static/styles.css` `[CHOCA]`.

### OA-066 · Sólo tres KPIs de ocho llevan subtítulo
`app/static/admin_archive.html:67`, `:77`, `:87`
**Hoy**: «Digitalizados», «Pendientes» y «Retención vencida» tienen `.ds-kpi-sub`;
«Documentos», «Tipologías», «Palabras clave», «Autores» y «Último ingreso» no. Las
tarjetas quedan de dos alturas distintas y la rejilla las estira con `height:100%`
(`styles.css:3401`), dejando huecos irregulares bajo las cifras cortas.
**Debe**: subtítulo en las ocho, aunque sea una unidad («documentos», «términos»,
«entes emisores»); o ninguna, y la altura fija.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`.

### OA-067 · «Último ingreso» pone una fecha en un hueco dimensionado para una cifra
`app/static/admin_archive.html:123`, `app/static/admin-charts.js:134`
**Hoy**: `dd/mm/aaaa` en un `h3` de `1.3rem` pensado para un número de tres dígitos. A
390 px la tarjeta se ensancha y rompe la rejilla; a 1440 px la fecha se ve
desproporcionada junto a «4.312».
**Debe**: tiempo relativo («hace 3 días») como cifra principal —`formatRelativeTime` ya
existe en `app-core.js:101`— y la fecha exacta en el subtítulo.
**Esfuerzo**: S. **Archivos**: `app/static/admin-charts.js` `[CHOCA]`.

### OA-068 · Los KPIs no son accionables
`app/static/admin_archive.html:52-128`
**Hoy**: «Pendientes: 12» no lleva a los doce documentos pendientes; «Retención vencida:
7» no lleva a la pestaña Retención. Son ocho cifras que sólo se pueden mirar. Los badges
del monitor sí filtran al pulsarlos (`admin-monitor.js:115`), así que el patrón existe.
**Debe**: cada KPI con destino: pestaña + filtro preaplicado, con `role="link"`, foco y
`cursor:pointer`.
**Esfuerzo**: M. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-charts.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OA-069 · Las tarjetas de KPI llevan el color en un `style` en línea
`app/static/admin_archive.html:53`, `:62`, `:72`, `:82`, `:92`, `:101`, `:110`, `:119`
**Hoy**: `style="border-left: 4px solid var(--viz-1);"`. Ocho colores de la paleta de
datos usados como decoración de marco, justo lo que el propio `CLAUDE.md` dice que no se
hace: «el color pertenece al dato, no al marco». Y un color en línea no lo corrige ninguna
hoja de estilos (nota de accesibilidad del mismo documento).
**Debe**: una clase por semántica (`.ds-kpi--neutral`, `.ds-kpi--alerta`,
`.ds-kpi--aviso`) y quitar el `style`.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/styles.css` `[CHOCA]`.

### OA-070 · Los iconos de los KPI no tienen `aria-hidden`
`app/static/admin_archive.html:59`, `:69`, `:79`, `:89`, `:98`, `:107`, `:116`, `:125`
**Hoy**: ocho `<i class="fas …">` decorativos sin `aria-hidden="true"`. Font Awesome usa
pseudo-elementos, así que algunos lectores de pantalla anuncian caracteres del área de uso
privado entre la etiqueta y la cifra.
**Debe**: `aria-hidden="true"` en todos los iconos decorativos del panel — son más de
setenta en este archivo.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-ui.js` `[CHOCA]`.

### OA-071 · Las cifras cambian sin anunciarse
`app/static/admin-charts.js:126`
**Hoy**: `setEl` escribe con `innerText` en un `h3` sin `aria-live`. Quien usa lector de
pantalla y pulsa «Actualizar Análisis» no recibe ninguna señal de que las ocho cifras
cambiaron.
**Debe**: `aria-live="polite"` en el contenedor de la rejilla y un resumen textual del
tipo «Cifras actualizadas para el rango 2024».
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-charts.js` `[CHOCA]`.

### OA-072 · Los KPI no tienen estado de error, sólo de carga
`app/static/admin-stats.js:33-35`, `app/static/admin-charts.js:70-75`
**Hoy**: si `/stats` falla, el `catch` sólo hace `console.error` y las tarjetas se quedan
con el guión largo del HTML. Indistinguible de «el archivo está vacío». Las gráficas sí
tienen mensaje de error (`admin-charts.js:72-73`).
**Debe**: estado de error explícito por tarjeta con botón de reintento.
**Esfuerzo**: S. **Archivos**: `app/static/admin-stats.js` `[CHOCA]`, `app/static/admin-charts.js` `[CHOCA]`.

### OA-073 · La tarjeta de digitalización desalinea la fila
`app/static/admin_archive.html:222` vs `:231`
**Hoy**: «Documentos por Tipo» usa `.ds-chart-box` (alto fijo 268 px, `styles.css:3409`);
«Estado de Digitalización» es un `card-body` suelto cuya altura depende de cuántos
soportes existan. Con un solo soporte la tarjeta mide la mitad que su vecina y la fila
queda coja.
**Debe**: `min-height` equivalente en la tarjeta de avance, o `align-items:stretch` con
alto compartido.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/styles.css` `[CHOCA]`.

### OA-074 · La barra de avance de digitalización no es accesible
`app/static/admin-charts.js:163-170`
**Hoy**: un `div` con tramos coloreados y `title`. Sin `role="img"`, sin
`role="progressbar"`, sin `aria-valuenow`. El `title` de cada tramo no lo lee ningún
lector de pantalla de forma fiable, y en táctil no hay forma de verlo.
**Debe**: `role="img"` con `aria-label` completo («63 % del fondo con soporte digital:
120 digitalizados, 30 digitales, 88 físicos»), y las cifras también como texto.
**Esfuerzo**: S. **Archivos**: `app/static/admin-charts.js` `[CHOCA]`.

### OA-075 · Los tramos de la barra reciben color por coincidencia exacta de texto
`app/static/admin-charts.js:154`
**Hoy**: `{ "Digitalizado": …, "Digital": …, "Físico": … }`. La consulta normaliza a
`'Físico'` por defecto (`stats.py:112`), pero cualquier valor histórico con acento
distinto o mayúscula distinta cae al color de reserva `C[3]`, y entonces dos soportes
comparten hue.
**Debe**: normalizar en el servidor a un conjunto cerrado y mapear por clave, no por
etiqueta visible.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`, `app/static/admin-charts.js` `[CHOCA]`.

### OA-076 · Las gráficas no tienen alternativa textual ni exportación
`app/static/admin_archive.html:222`, `:239`, `:247`
**Hoy**: tres `<canvas>` sin `role="img"`, sin `aria-label`, sin tabla equivalente. Un
`<canvas>` es un agujero total para un lector de pantalla, y no hay forma de llevar los
datos a un informe.
**Debe**: `role="img"` con `aria-label` resumiendo la serie, un enlace «Ver como tabla»
que despliegue los mismos datos, y descarga PNG/CSV por gráfico.
**Esfuerzo**: M. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-charts.js` `[CHOCA]`.

### OA-077 · «Tendencia Mensual (24 meses)» aparece en blanco con menos de dos meses
`app/static/admin-charts.js:215`
**Hoy**: con un solo mes con datos se muestra «Sin ingresos en los últimos 24 meses», que
es falso: hay ingresos, sólo que en un mes. En un archivo que arranca, esa tarjeta miente
durante las primeras semanas.
**Debe**: dibujar el punto único con una línea de referencia, o decir «Un solo mes con
ingresos: marzo 2026 (14 documentos)».
**Esfuerzo**: S. **Archivos**: `app/static/admin-charts.js` `[CHOCA]`.

### OA-078 · «Documentos por Año» se limita a 10 años sin decirlo
`app/routes/admin/stats.py:97`
**Hoy**: `ORDER BY label DESC LIMIT 10`. En un archivo institucional con fondos de los
años sesenta se ven diez barras y no hay señal de que falte nada; además el orden
descendente hace que el eje X aparezca invertido respecto a la lectura natural.
**Debe**: agrupar por décadas cuando el rango excede diez años, o permitir desplazar; y
ordenar ascendente.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`, `app/static/admin-charts.js` `[CHOCA]`.

### OA-079 · «Documentos por Tipo» reparte 8 colores entre categorías sin orden estable
`app/routes/admin/stats.py:84-91`
**Hoy**: el corte a 7 + «Otros» ordena por volumen. Al añadir un documento, dos tipos
pueden intercambiar posición y con ella su color: la misma tipología es azul un día y
verde al siguiente. El propio `CLAUDE.md` advierte de que el orden de los slots es el
mecanismo de seguridad para daltonismo.
**Debe**: asignar el slot por id de tipo (estable), no por posición en el ranking.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`, `app/static/admin-charts.js` `[CHOCA]`, `app/static/viz-tokens.js` `[CHOCA]`.

### OA-080 · El banner de vencimientos se puede cerrar y no vuelve
`app/static/admin.js:62-70`
**Hoy**: `alert-dismissible` con `data-dismiss="alert"`. Se cierra, y como `_loadAlertasBanner`
sólo se ejecuta al entrar en «Resumen», no reaparece durante la sesión. La única alerta
del panel es también la más fácil de silenciar por accidente.
**Debe**: sin botón de cierre mientras haya vencidos; en su lugar «Ver los 12» que lleve a
la pestaña Retención.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`.

### OA-081 · El filtro analítico ocupa una tarjeta entera para dos fechas
`app/static/admin_archive.html:197-216`
**Hoy**: una `card` con cabecera, título e icono para dos `input[type=date]` y un botón,
por encima de todo el contenido. Empuja las gráficas fuera de la primera pantalla en
portátil, y no ofrece ningún atajo («Este año», «Últimos 12 meses») pese a que las
búsquedas públicas sí los tienen (`.ds-date-chip`, `app.js:262`).
**Debe**: una barra de filtro de una línea con presets, como en la búsqueda pública.
**Esfuerzo**: M. **Archivos**: `app/static/admin_archive.html`, `app/static/styles.css` `[CHOCA]`.

---

## E. Pestaña Ingresar

### OA-082 · La importación CSV masiva está por encima del formulario de alta individual
`app/static/admin_archive.html:256-273`
**Hoy**: lo primero que se ve al entrar a «Ingresar» es la barra de importación. La acción
diaria —dar de alta un documento— queda debajo. Además la importación es la operación
irreversible (OA-022) y la que está más a mano.
**Debe**: el formulario primero; la importación en «Datos» (OA-058) o plegada tras un
enlace «Importar varios desde CSV».
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`.

### OA-083 · Bloque muerto de 26 líneas dentro del pane
`app/static/admin_archive.html:302-327`
**Hoy**: un `col-md-4` con `display:none` que contiene una segunda zona de arrastre
(`file_upload-archivo-legacy`, sin ningún listener) y la tarjeta «Últimos Ingresos» con
`#recent_submissions-archivo`. Ese `<ul>` sí se rellena en cada apertura de la pestaña
(`admin.js:16` → `loadRecentSubmissions`), o sea: una petición a `/list_all` en cada
entrada para pintar algo que nadie ve.
**Debe**: o se muestra «Últimos ingresos» —que es útil y confirma que el alta funcionó— o
se borran el bloque y la llamada.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/admin.js` `[CHOCA]`, `app/static/admin-submit.js` `[CHOCA]`.

### OA-084 · No hay validación por campo: todo sale como aviso flotante
`app/static/admin-submit.js:251-259`
**Hoy**: tres comprobaciones que muestran un toast y salen. El toast aparece arriba a la
derecha, el campo culpable puede estar fuera de la pantalla, no se enfoca y no se marca.
Con el formulario largo (dieciséis campos) hay que buscarlo a ojo.
**Debe**: `is-invalid` + `.invalid-feedback` bajo cada campo, foco en el primero que falle
y `aria-describedby` apuntando al mensaje.
**Esfuerzo**: M. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OA-085 · Se valida en el envío y nunca antes
`app/static/admin-submit.js:243-259`
**Hoy**: ninguna validación al salir del campo. Un título de 600 caracteres se acepta en
pantalla y lo rechaza Pydantic con un 422 genérico (`models.py:78`, `max_length=500`).
**Debe**: validación al `blur` con contador de caracteres en los campos con tope.
**Esfuerzo**: M. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`.

### OA-086 · Cuatro campos marcados con `*` no se comprueban
`app/static/admin-submit.js:41`, `:36`, `:27`
**Hoy**: «Palabras Clave *», «Fecha de Emisión *», «Clasificación *» y «Autor *» llevan
asterisco y `required` en el HTML, pero el envío es por JavaScript
(`e.preventDefault()`), así que la validación nativa del navegador nunca corre, y
`handleNewSubmission` sólo comprueba título, tipo y ubicación. Se puede guardar un
documento sin una sola palabra clave pese al asterisco.
**Debe**: que la lista de obligatorios sea una sola —declarada una vez— y que la
validación la recorra entera.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`.

### OA-087 · La leyenda de obligatorios está al pie, después de los campos
`app/static/admin_archive.html:294`
**Hoy**: «campos obligatorios» aparece bajo el último campo, junto al botón de guardar.
Quien rellena de arriba abajo se encuentra la explicación del asterisco cuando ya no la
necesita.
**Debe**: la leyenda antes del primer campo.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`.

### OA-088 · Los asteriscos son texto plano dentro del `<label>`
`app/static/admin-submit.js:11`, `:15`, `:21`, …
**Hoy**: `Título del Documento *`. El lector de pantalla lee «asterisco», que no significa
nada, y no hay `required`/`aria-required` efectivo sobre el control.
**Debe**: `aria-required="true"` en el control y el asterisco con
`<abbr title="obligatorio">` o `aria-hidden`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`.

### OA-089 · Salir de la pestaña o cerrar el navegador borra el formulario entero
`app/static/admin.js:16`, `app/static/admin-submit.js:2`
**Hoy**: `renderDynamicSubmitFields()` reconstruye el `innerHTML` en cada entrada a la
pestaña. Un clic en «Documentos» y de vuelta a «Ingresar» borra dieciséis campos ya
escritos. Tampoco hay `beforeunload`.
**Debe**: autoguardado del borrador en `localStorage` cada pocos segundos, restauración al
volver con aviso «se recuperó un borrador», y `beforeunload` si hay cambios sin guardar.
**Esfuerzo**: M. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/static/admin.js` `[CHOCA]`.

### OA-090 · Tras guardar, el panel salta a «Resumen»
`app/static/admin-submit.js:333`
**Hoy**: `loadAdminTab("stats")` al terminar. Quien está catalogando una caja de treinta
documentos es expulsado de la pestaña de alta en cada uno y tiene que volver a entrar.
**Debe**: quedarse en «Ingresar», limpiar el formulario, mantener tipo/clasificación/
ubicación —que se repiten dentro de una misma caja— y mostrar el registro creado en
«Últimos ingresos» con enlace para editarlo.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`.

### OA-091 · El error de guardado se traga el motivo real
`app/static/admin-submit.js:334-335`
**Hoy**: `catch { showToast("Error de conexión al registrar el folio.", "error"); }`. Un
422 por fecha inválida, un 413 por tamaño y una caída de red dicen exactamente lo mismo, y
además dicen «de conexión», que en dos de los tres casos es mentira.
**Debe**: distinguir por código y mostrar el `detail` del servidor.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`.

### OA-092 · Si la subida del archivo falla se pierde el formulario y no se guarda nada
`app/static/admin-submit.js:306-325`
**Hoy**: la subida va antes del `POST /submit` en el mismo `try`. Un fallo de R2 aborta
todo; el usuario ve un toast genérico y conserva los datos en pantalla —eso sí— pero no
tiene forma de guardar los metadatos sin el archivo.
**Debe**: ofrecer «Guardar sin archivo digital» al fallar la subida, y reintentar la
subida por separado desde el modal de edición.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`.

### OA-093 · La subida no tiene barra de progreso
`app/static/admin-submit.js:311`
**Hoy**: sólo cambia el rótulo del botón a «Subiendo archivo…» con un icono girando. Un
PDF de 20 MB por una conexión de la Facultad tarda minutos sin ninguna señal de avance, y
`fetch` no ofrece progreso de subida.
**Debe**: `XMLHttpRequest` con `upload.onprogress` y una barra real con porcentaje y
opción de cancelar; `showProgress()` existe en `admin-ui.js:292` pero es indeterminada.
**Esfuerzo**: M. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`.

### OA-094 · La zona de arrastre sólo acepta un archivo y lo sustituye en silencio
`app/static/admin-monitor.js:301-312`
**Hoy**: `files[0]` y nada más. Al soltar tres, se coge el primero sin decirlo. No hay
lista de lo seleccionado, ni botón para quitarlo, ni tamaño visible.
**Debe**: aceptar varios (un documento con anexos), mostrar la lista con nombre, tamaño y
botón de quitar, y advertir si se descarta algo.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-095 · La zona de arrastre no se puede usar con teclado
`app/static/admin_archive.html:284-292`
**Hoy**: el `<input type="file">` está oculto con `display:none` —lo que lo saca del orden
de tabulación— y el único acceso es el botón «Explorar». El `div` contenedor no tiene
`tabindex` ni responde a Enter.
**Debe**: ocultar el input con la técnica de fuera de pantalla (como el enlace de salto,
`styles.css:1655`) para que siga siendo enfocable, y asociarlo a un `<label>`.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/styles.css` `[CHOCA]`.

### OA-096 · El rótulo de la zona de arrastre pierde los acentos al reponerse
`app/static/admin-monitor.js:288`
**Hoy**: el texto inicial es «Arrastra el archivo digital aquí o» y el JS lo repone como
«Arrastra el archivo digital aqui o». Basta seleccionar un archivo y quitarlo para que el
panel pierda la tilde.
**Debe**: una sola cadena, con tilde, definida en un sitio.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`.

### OA-097 · Nada avisa de que el documento ya existe
`app/routes/admin/docs.py:253-277`
**Hoy**: se puede ingresar diez veces el mismo título, autor y fecha sin una sola
advertencia. En catalogación por lotes con dos personas trabajando sobre la misma caja,
los duplicados son la norma, no la excepción.
**Debe**: al salir del campo Título, consultar coincidencias por título+fecha y mostrar
«Ya existe un documento parecido: …» con enlace, sin bloquear.
**Esfuerzo**: M. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`.

### OA-098 · La fecha de emisión se rellena con hoy
`app/static/admin-submit.js:37`
**Hoy**: `value="${new Date().toISOString().substring(0,10)}"`. En un archivo histórico,
la fecha de hoy es casi siempre la respuesta equivocada, y como el campo ya viene relleno
nadie lo revisa. Se acaban catalogando actas de 1978 con fecha de 2026.
**Debe**: campo vacío y obligatorio; y aviso si la fecha es futura.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`.

### OA-099 · Las palabras clave se piden como texto separado por comas
`app/static/admin-submit.js:42`
**Hoy**: un `<input type="text">` libre. No hay autocompletado contra
`descriptores_libres`, ni control de duplicados por mayúsculas o acentos; el backend hace
`ON CONFLICT (nombre)` exacto (`helpers.py:56`), así que «Gestión» y «gestion» son dos
descriptores distintos. El vocabulario controlado se degrada solo. `/api/choices` ya
expone `keywords`.
**Debe**: un selector con autocompletado (TomSelect ya está en el proyecto,
`app-choices.js`), normalización sin acentos y sin mayúsculas al comparar, y aviso al
crear un término nuevo.
**Esfuerzo**: M. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/routes/admin/helpers.py` `[CHOCA]`, `app/static/app-choices.js` `[CHOCA]`.

### OA-100 · «Personas / Dependencias Relacionadas» es texto libre sin estructura
`app/static/admin-submit.js:76`
**Hoy**: una cadena separada por punto y coma, sin autoridad detrás. No se puede buscar
«todos los documentos donde interviene el Decano Flores» con fiabilidad, ni corregir un
nombre en un sitio.
**Debe**: control de autoridades (OA-203); mientras tanto, autocompletado contra los
valores ya usados.
**Esfuerzo**: M. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/routes/lookups.py`.

### OA-101 · El tipo de documento no ofrece crear uno nuevo desde el alta
`app/static/admin-submit.js:22-24`
**Hoy**: un `<select>` cerrado. Si la tipología no existe hay que ir a «Tipos», crearla,
volver, y el formulario ya se habrá reconstruido perdiendo lo escrito (OA-089).
**Debe**: opción «+ Crear tipo documental…» dentro del propio selector.
**Esfuerzo**: M. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/static/admin-categories.js` `[CHOCA]`.

### OA-102 · La ayuda del CSV es un `title` de HTML en un enlace con `tabindex="-1"`
`app/static/admin_archive.html:268-271`
**Hoy**: la única documentación del formato son dos líneas en un atributo `title` con
`data-toggle="tooltip"` que nadie inicializa (jQuery está, pero no hay
`$('[data-toggle="tooltip"]').tooltip()` en ningún `.js`). Y el `tabindex="-1"` lo saca del
teclado, así que sólo existe para el ratón, y ni eso.
**Debe**: un panel de ayuda desplegable con la tabla de columnas, cuáles son obligatorias,
y una plantilla CSV descargable de ejemplo.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-charts.js` `[CHOCA]`.

---

## F. Pestaña Documentos

### OA-103 · No hay selección múltiple ni acciones en lote
`app/static/admin-monitor.js:154-186`
**Hoy**: cada fila tiene sus cinco botones y no hay casillas. Aprobar veinte documentos
son sesenta clics —abrir menú, elegir, esperar—, y mover una caja entera a la papelera es
inviable. Es la carencia más citada frente a Alfresco y SharePoint.
**Debe**: casilla por fila más «seleccionar todo lo filtrado», barra de acciones flotante
con aprobar, rechazar, cambiar tipo, cambiar ubicación, mover a papelera y exportar; y un
endpoint de lote transaccional.
**Esfuerzo**: L. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin_archive.html`, `app/routes/admin/docs.py` `[CHOCA]`.

### OA-104 · Las columnas no se pueden ordenar
`app/static/admin_archive.html:359-367`, `app/routes/admin/docs.py:94`
**Hoy**: el `ORDER BY` es fijo (`fecha_documento DESC NULLS LAST`) y los `<th>` no son
accionables. No se puede ver «los últimos modificados», que es lo que hace falta para
retomar el trabajo, ni ordenar por título para detectar duplicados.
**Debe**: `<th>` con `aria-sort` y botón, parámetros `sort`/`dir` en `/list_all` sobre una
lista blanca de columnas.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-105 · Las columnas no se pueden elegir
`app/static/admin_archive.html:359-367`
**Hoy**: seis columnas fijas. Folio, soporte, páginas, idioma, ubicación y estado de
disposición existen en la base y viajan en la respuesta (`docs.py:89-91`), pero no hay
forma de verlos en la tabla. Un archivero que trabaja por signatura topográfica no puede
ver la signatura.
**Debe**: un selector de columnas persistido por usuario en `localStorage`.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-106 · Los filtros no se pueden guardar ni compartir
`app/static/admin_archive.html:339-356`
**Hoy**: cuatro filtros que se pierden al cambiar de pestaña (`loadAdminTab("monitor")`
resetea la página, `admin.js:17`) y no se reflejan en la URL. «Documentos de 2019
pendientes de revisión de la Mapoteca» hay que recomponerlo a mano cada vez.
**Debe**: filtros en la URL y búsquedas guardadas con nombre, como los «saved searches»
de SharePoint.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin.js` `[CHOCA]`.

### OA-107 · No hay forma de limpiar los filtros
`app/static/admin_archive.html:339-356`
**Hoy**: hay que vaciar la caja de búsqueda y devolver los tres desplegables a su primera
opción, uno a uno. Ni botón de limpiar ni indicación de cuántos filtros hay activos. La
búsqueda pública sí tiene `btn_clear_archivo` (`app.js:240`).
**Debe**: «Limpiar filtros» visible sólo cuando hay alguno activo, con el recuento.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-monitor.js` `[CHOCA]`.

### OA-108 · El filtro de estado usa símbolos Unicode como iconos
`app/static/admin_archive.html:349-352`
**Hoy**: `✓ Aprobados`, `⏳ Revisión`, `✎ Borradores`, `✗ Rechazados` dentro de `<option>`.
Los lectores de pantalla leen «marca de verificación» o «reloj de arena»; el reloj se
renderiza como emoji a color en Windows y rompe la altura de la línea del desplegable; y
el resto del panel usa Font Awesome.
**Debe**: texto llano en las opciones y el icono fuera del `<select>`, o un grupo de
botones de filtro con los mismos badges que la tabla.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`.

### OA-109 · La cabecera de la tabla usa clases de utilidad en vez del sistema
`app/static/admin_archive.html:360`
**Hoy**: `<tr class="bg-light text-dark">`, y `styles.css:2399` redefine `.bg-light` en
oscuro. Las otras cinco tablas del panel usan `thead-light` o `bg-light` sin criterio
(`:448`, `:467`, `:508`, `admin-ui.js:437`, `admin-users.js:540`). Cuatro estilos de
cabecera de tabla en el mismo panel.
**Debe**: una clase `.ds-table` con su `thead` definido una vez.
**Esfuerzo**: M. **Archivos**: `app/static/styles.css` `[CHOCA]`, `app/static/admin_archive.html`, `app/static/admin-ui.js` `[CHOCA]`, `app/static/admin-users.js` `[CHOCA]`.

### OA-110 · La fila entera parece pulsable y no lo es
`app/static/admin_archive.html:18`
**Hoy**: la regla en línea da `cursor:pointer` a toda la fila al pasar por encima
(`.table-hover tbody tr:hover`), pero no hay ningún `onclick` de fila: sólo los botones de
la última columna hacen algo. Se clica en el título esperando abrir el documento y no pasa
nada.
**Debe**: o la fila abre el detalle (con el botón de acciones deteniendo la propagación),
o se quita el `cursor:pointer`.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-monitor.js` `[CHOCA]`.

### OA-111 · Cinco botones de icono por fila sin etiqueta accesible
`app/static/admin-monitor.js:158`, `:180-183`
**Hoy**: ver archivo, ver, editar, compartir y eliminar, sólo con `title`. El propio
`CLAUDE.md` dice que «los controles que sólo llevan icono necesitan `aria-label`; el
`title` no basta». Además los cinco son iguales de peso: eliminar y ver se distinguen sólo
por el color del borde.
**Debe**: `aria-label` con el título del documento incluido («Editar: Acta 12/2026»), y
las acciones destructivas en un menú de desbordamiento.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`.

### OA-112 · El menú de cambio rápido de estado no es un menú
`app/static/admin-ui.js:245-289`
**Hoy**: un `div` posicionado absolutamente en el `<body>` con botones. Sin
`role="menu"`, sin foco al abrir, sin flechas, sin Escape, sin `aria-expanded` en el
disparador. Se cierra sólo con clic fuera, y como está anclado con `scrollY` calculado al
abrir, al desplazarse la página el menú se queda flotando lejos de su fila.
**Debe**: un menú accesible real, anclado con posicionamiento fijo y recolocado al
desplazar; o un `<select>` en línea, que es más simple y ya es accesible.
**Esfuerzo**: M. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OA-113 · Cambiar de estado no pide motivo ni deja comentario
`app/static/admin-ui.js:270-283`, `app/routes/admin/docs.py:454-476`
**Hoy**: se pasa a «Rechazado» con un clic. No se pregunta por qué, no se guarda un
comentario y quien redactó el documento no recibe nada. En AtoM y Alfresco un rechazo sin
motivo no existe.
**Debe**: comentario obligatorio al rechazar, guardado en una tabla de flujo de trabajo y
visible en el historial del documento.
**Esfuerzo**: M. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`, `app/main.py`.

### OA-114 · El cambio de estado no se refleja hasta que vuelve la tabla entera
`app/static/admin-ui.js:279`
**Hoy**: tras el `PATCH` se recarga el monitor completo. La fila salta, la página se
reposiciona y se pierde el sitio donde se estaba trabajando; y los badges de la cabecera
no se actualizan (`_renderMonitorStatusBadges` no se vuelve a llamar).
**Debe**: actualización optimista de la fila y refresco de los contadores.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/static/admin-monitor.js` `[CHOCA]`.

### OA-115 · El estado «Aprobado» se muestra como «OK»
`app/static/admin-monitor.js:148`
**Hoy**: el badge dice «OK» mientras el desplegable de filtro dice «Aprobados», el modal
de edición «✓ Aprobado» (`admin_archive.html:728`) y el menú rápido «Aprobado»
(`admin-ui.js:250`). Cuatro etiquetas para un estado.
**Debe**: «Aprobado» en los cuatro; si no cabe, abreviar por CSS, no por texto distinto.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`.

### OA-116 · Un documento sin estado se pinta como aprobado
`app/static/admin-monitor.js:156`
**Hoy**: `STATUS_BADGES[f.status] || STATUS_BADGES["aprobado"]`. Un valor desconocido
—llegado por importación o por una migración a medias— se presenta con el sello verde de
aprobado. En gestión documental, dar por bueno lo que no se sabe es el peor valor por
defecto posible.
**Debe**: un badge «Sin estado» neutro y visible.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`.

### OA-117 · La cifra de resumen y el paginador dicen cosas distintas
`app/static/admin-monitor.js:129` vs `app/static/admin_archive.html:373`
**Hoy**: el HTML nace con «Mostrando 0 registros» y el JS lo sustituye por «Total: 412
registros en el módulo Archivo». Ni uno ni otro dicen qué se está viendo. Lo estándar es
«1–25 de 412».
**Debe**: «Mostrando 1–25 de 412 documentos», con mención a los filtros activos.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-118 · El paginador no permite saltar de página
`app/static/admin_archive.html:381-383`
**Hoy**: sólo anterior y siguiente. Con 412 documentos y 25 por página, llegar a la
diecisiete son dieciséis clics y dieciséis peticiones.
**Debe**: primera/última, campo de página y `aria-label` en los botones de flecha, que hoy
son iconos pelados.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-monitor.js` `[CHOCA]`.

### OA-119 · El estado vacío del monitor confunde «sin datos» con «sin resultados»
`app/static/admin-monitor.js:141`
**Hoy**: siempre «Ningún archivo coincide con los criterios de búsqueda», incluso con el
archivo recién instalado y cero documentos. La primera pantalla que ve un usuario nuevo le
dice que su búsqueda no encontró nada.
**Debe**: distinguir «Aún no hay documentos — Ingresar el primero» de «Ningún documento
coincide — Limpiar filtros».
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`.

### OA-120 · El esqueleto de carga tiene seis columnas fijas y las filas no se anuncian
`app/static/admin-ui.js:139-146`, `app/static/admin-monitor.js:58-60`
**Hoy**: `showTableSkeleton(id, 6, 6)`. Si se hacen configurables las columnas (OA-105) se
descuadra; y la tabla no lleva `aria-busy`, así que un lector de pantalla anuncia seis
filas vacías como si fueran datos.
**Debe**: derivar el número de columnas del `<thead>` y marcar `aria-busy="true"` mientras
carga.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/static/admin-monitor.js` `[CHOCA]`.

### OA-121 · «Ver» abre un modal con menos datos que la tabla
`app/static/admin-monitor.js:210-224`
**Hoy**: `openAdminDocById` lee de la caché de la tabla y arma un objeto con nueve campos.
Se pierden folio, soporte, páginas, idioma, vencimiento, palabras clave, estado,
personas relacionadas y quién lo modificó por última vez — todo lo cual existe y lo sirve
`GET /documento/{id}` (`docs.py:197-222`).
**Debe**: pedir el documento al servidor y mostrar la ficha completa, incluida la
procedencia y la traza de cambios.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-122 · La exportación CSV sólo exporta la página en pantalla
`app/static/admin-monitor.js:226-277`
**Hoy**: `state.adminTable.results` son 25 filas. El nombre del archivo lo delata
(`…_p1.csv`) y la fila de metadatos también, pero el botón dice «Exportar CSV» sin más y
el toast dice «CSV exportado: 25 registro(s)» tras una búsqueda que dio 412.
**Debe**: exportar el conjunto filtrado completo desde el servidor, en streaming, con
aviso si supera cierto tamaño; y ofrecer también XLSX, que es lo que la Facultad usa.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`.

---

## G. Pestaña Tipos

### OA-123 · Los tipos documentales sólo se pueden crear
`app/static/admin-categories.js:355-364`, `app/routes/admin/catalog.py:325-356`
**Hoy**: la lista es de sólo lectura: no hay renombrar, ni editar la descripción, ni
desactivar, ni fusionar dos tipos duplicados, ni borrar. Un error tipográfico en el nombre
de una tipología es permanente y se propaga a todos los documentos, porque
`tesauro_primario` guarda el texto además del `id_tipo_documento` (`docs.py:381`).
**Debe**: CRUD completo con fusión (reasignar documentos al tipo destino) y desactivación
en vez de borrado cuando el tipo esté en uso.
**Esfuerzo**: L. **Archivos**: `app/routes/admin/catalog.py` `[CHOCA]`, `app/static/admin-categories.js` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-124 · La descripción que se pide no se guarda
`app/static/admin_archive.html:404-406`, `app/routes/admin/catalog.py:350-354`
**Hoy**: el formulario tiene un `<textarea>` «Descripción», `handleAddCategory` lo envía
como `desc` (`admin-categories.js:506`) y el `INSERT` no lo incluye. Se escribe una
descripción cuidada de la tipología y se pierde sin aviso.
**Debe**: persistir en una columna `descripcion` y mostrarla en la lista y como ayuda en
el selector del alta.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/catalog.py` `[CHOCA]`, `app/main.py`, `app/static/admin-categories.js` `[CHOCA]`.

### OA-125 · Crear un tipo que ya existe responde «éxito»
`app/routes/admin/catalog.py:345-347`
**Hoy**: `return {"success": True, "detail": "Ya existe"}`. El cliente sólo mira el estado
HTTP y muestra «¡Nueva tipología guardada con éxito!» (`admin-categories.js:516`). El
usuario cree haber creado algo que no creó.
**Debe**: 409 con mensaje, y en la interfaz «Ese tipo documental ya existe» con enlace a
la entrada existente.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/catalog.py` `[CHOCA]`, `app/static/admin-categories.js` `[CHOCA]`.

### OA-126 · La lista no dice cuántos documentos usa cada tipo
`app/static/admin-categories.js:358-362`
**Hoy**: nombre y un badge fijo «Archivo». Nada más. No se sabe si un tipo está en uso,
lo cual es justo el dato que hace falta para decidir si se puede retirar. El endpoint de
retención ya calcula ese conteo (`retention.py:53`).
**Debe**: uso, plazo de retención (sólo lectura, OA-051) y fecha de creación en cada fila.
**Esfuerzo**: S. **Archivos**: `app/static/admin-categories.js` `[CHOCA]`.

### OA-127 · La lista de tipos no se puede buscar ni ordenar
`app/static/admin-categories.js:356-363`
**Hoy**: una lista plana en el orden que devuelva `/api/choices`. Con cuarenta tipologías
—normal en un fondo institucional— encontrar una es desplazarse a ojo.
**Debe**: caja de búsqueda, orden alfabético o por uso, y agrupación por serie documental
cuando exista el cuadro de clasificación (OA-202).
**Esfuerzo**: S. **Archivos**: `app/static/admin-categories.js` `[CHOCA]`.

### OA-128 · La proporción 5/7 deja el formulario de alta enorme y la lista estrecha
`app/static/admin_archive.html:393`, `:416`
**Hoy**: `col-md-5` para tres campos y `col-md-7` para la lista completa de tipologías y
la sección de palabras clave, que se inyecta debajo a todo el ancho del pane
(`admin-categories.js:406`) rompiendo la rejilla de dos columnas.
**Debe**: formulario compacto arriba o en un modal, y la lista a todo el ancho.
**Esfuerzo**: M. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-categories.js` `[CHOCA]`.

### OA-129 · La sección de palabras clave se recarga entera en cada operación
`app/static/admin-categories.js:459`, `:478`, `:497`
**Hoy**: añadir, renombrar o borrar una palabra clave hace `loadKeywordsSection()`, que
vuelve a pedir la lista completa y reconstruye el `innerHTML`. Con 300 descriptores, cada
alta redibuja 300 nodos, pierde la posición del desplazamiento y el foco, y no hay
paginación ni búsqueda.
**Debe**: actualización puntual del nodo afectado, y búsqueda con paginación en la lista.
**Esfuerzo**: M. **Archivos**: `app/static/admin-categories.js` `[CHOCA]`.

### OA-130 · Renombrar una palabra clave puede fusionar dos sin avisar
`app/routes/admin/catalog.py:288-297`
**Hoy**: el `UPDATE` no comprueba colisión con un nombre existente. Si hay restricción
`UNIQUE (nombre)`, la operación falla con un 500 genérico; si no la hay, quedan dos
descriptores con el mismo nombre y las búsquedas se parten en dos.
**Debe**: detectar la colisión y ofrecer explícitamente fusionar (reasignar los enlaces al
descriptor destino y borrar el origen).
**Esfuerzo**: M. **Archivos**: `app/routes/admin/catalog.py` `[CHOCA]`, `app/static/admin-categories.js` `[CHOCA]`.

### OA-131 · Borrar una palabra clave en uso desvincula documentos sin decir cuáles
`app/static/admin-categories.js:484-501`, `app/routes/admin/catalog.py:300-322`
**Hoy**: el diálogo avisa de cuántos documentos, pero no de cuáles, no ofrece verlos y no
propone la alternativa razonable —fusionarla con otra—. Tras confirmar, los enlaces se
borran y no hay deshacer.
**Debe**: enlace «ver los 12 documentos», opción «fusionar con…» y registro en auditoría
de los documentos afectados.
**Esfuerzo**: M. **Archivos**: `app/static/admin-categories.js` `[CHOCA]`, `app/routes/admin/catalog.py` `[CHOCA]`.

---

## H. Pestaña Papelera

### OA-132 · Nada se borra nunca solo
`app/routes/trash.py:20-72`
**Hoy**: no hay política de retención de la papelera: lo eliminado se queda indefinidamente
ocupando espacio en Neon y en R2, y contando en `COUNT(*)` de tablas que se recorren en
cada consulta. Tampoco se muestra cuánto lleva cada elemento ahí.
**Debe**: purgado automático configurable (por ejemplo 90 días) con aviso previo, ejecutado
por el mismo cron del respaldo; y columna «Días en papelera».
**Esfuerzo**: M. **Archivos**: `app/routes/trash.py` `[CHOCA]`, `app/main.py`, `app/routes/backup.py`.

### OA-133 · No se puede vaciar la papelera ni purgar en lote
`app/static/admin-edit.js:267-279`
**Hoy**: un botón de purgar por fila y una confirmación por cada uno. Vaciar una papelera
con 200 documentos son 400 clics.
**Debe**: «Vaciar papelera» con confirmación reforzada (OA-046) y selección múltiple.
**Esfuerzo**: M. **Archivos**: `app/static/admin-edit.js`, `app/routes/trash.py` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-134 · La papelera no se puede buscar ni filtrar
`app/static/admin_archive.html:499-525`
**Hoy**: una tabla paginada de 20 en 20 sin caja de búsqueda ni filtro por quién borró ni
por fecha. Recuperar un documento concreto de una papelera con cientos es ir página a
página.
**Debe**: buscar por título, filtrar por responsable y por rango de fechas de borrado.
**Esfuerzo**: M. **Archivos**: `app/static/admin_archive.html`, `app/routes/trash.py` `[CHOCA]`, `app/static/admin-edit.js`.

### OA-135 · La papelera no dice por qué se borró
`app/routes/admin/docs.py:435-451`
**Hoy**: se guardan `deleted_at` y `deleted_by`, nada más. Al revisar la papelera meses
después no hay forma de saber si fue un duplicado, un error de catalogación o un expurgo
decidido en acta.
**Debe**: motivo obligatorio al enviar a papelera, guardado en `deleted_reason` y visible
en la tabla; enlazado al acta cuando venga de una disposición (OA-061).
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/main.py`, `app/static/admin-edit.js`.

### OA-136 · Restaurar no comprueba nada
`app/routes/trash.py:75-94`
**Hoy**: se limpia `deleted_at` y ya. Si el tipo documental se borró entre medias, o si en
el intervalo se creó un documento con la misma signatura, el registro vuelve inconsistente
y nadie se entera.
**Debe**: validar referencias al restaurar y avisar de los conflictos antes de confirmar.
**Esfuerzo**: M. **Archivos**: `app/routes/trash.py` `[CHOCA]`.

### OA-137 · Restaurar no lleva a ninguna parte
`app/static/admin-edit.js:328-333`
**Hoy**: un toast «Documento restaurado» y la tabla se recarga. No se dice a qué estado
vuelve (¿aprobado? ¿el que tenía?) ni se ofrece ir a verlo.
**Debe**: indicar el estado resultante y ofrecer «Ver en Documentos».
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit.js`.

### OA-138 · La papelera no se puede exportar
`app/static/admin_archive.html:499-525`
**Hoy**: el resto de tablas del panel tienen alguna salida a CSV; ésta no. Un inventario
de lo eliminado —con quién y cuándo— es justamente lo que pide una auditoría externa.
**Debe**: exportación CSV de la papelera con los mismos campos de la tabla.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit.js`, `app/static/admin_archive.html`.

### OA-139 · La tarjeta roja de la papelera grita
`app/static/admin_archive.html:500-503`
**Hoy**: `card-danger` con la cabecera roja a todo el ancho y texto blanco a 50 % de
opacidad (`text-white-50`), que sobre rojo `#dc3545` no llega a 4,5:1. La papelera no es
un error: es un almacén temporal, y el rojo permanente desensibiliza frente a las alertas
que sí lo son (el banner de vencimientos).
**Debe**: tarjeta neutra con la acción destructiva marcada en rojo sólo en el botón, y
`text-white` sin opacidad reducida.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/tests/test_contraste.py`.

### OA-140 · El paginador de la papelera no se deshabilita ni informa
`app/static/admin_archive.html:520-522`, `app/static/admin-edit.js:322-326`
**Hoy**: `changePapeleraPage` acota por abajo (`Math.max(1, …)`) pero no por arriba: se
puede avanzar indefinidamente y llegar a páginas vacías. Los botones nunca se deshabilitan
y sólo llevan icono, sin `aria-label`.
**Debe**: acotar contra el total, deshabilitar en los extremos y etiquetar.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit.js`, `app/static/admin_archive.html`.

---

## I. Pestaña Retención

### OA-141 · El plazo se guarda tipo a tipo, con un botón por fila
`app/static/admin.js:175-198`
**Hoy**: cuarenta tipos son cuarenta campos y cuarenta botones de guardar, cada uno con su
petición y su toast. Cambiar la política de retención de un fondo es una tarde de clics.
**Debe**: edición en la tabla con un único «Guardar cambios» que envíe el lote, o
guardado al salir del campo con confirmación discreta y deshacer.
**Esfuerzo**: M. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/routes/admin/retention.py`.

### OA-142 · Cambiar un plazo no dice a cuántos documentos afecta
`app/static/admin.js:201-218`, `app/routes/admin/retention.py:67-92`
**Hoy**: se baja «Actas» de 10 a 5 años y 300 documentos pasan a estar vencidos de golpe,
sin ninguna advertencia previa ni resumen posterior.
**Debe**: previsualizar el impacto («este cambio pone 300 documentos en situación de
vencimiento») antes de confirmar.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/retention.py`, `app/static/admin.js` `[CHOCA]`.

### OA-143 · El plazo no distingue el momento de inicio del cómputo
`app/routes/admin/retention.py:112-120`
**Hoy**: siempre `fecha_documento + plazo`. La norma archivística cuenta desde el cierre
del expediente o desde el fin de la vigencia administrativa, no desde la fecha del
documento. Y no hay dos fases (archivo de gestión → archivo central → histórico), que es
como funciona un calendario de conservación real.
**Debe**: plazo en fase de gestión y en fase central, con la disposición final por tipo, y
un campo de fecha de cierre.
**Esfuerzo**: L. **Archivos**: `app/routes/admin/retention.py`, `app/main.py`, `app/static/admin_archive.html`.

### OA-144 · El valor por defecto de 5 años está codificado en cuatro sitios
`retention.py:50`, `:110`, `:115`, `:120`, `stats.py:132`
**Hoy**: `COALESCE(td.plazo_retencion_anios, 5)` repetido. Cambiar la política por defecto
exige tocar cinco literales SQL y no olvidarse de ninguno.
**Debe**: un `DEFAULT 5` en la columna más una constante única, o un parámetro de
configuración.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/retention.py`, `app/routes/admin/stats.py` `[CHOCA]`, `app/main.py`.

### OA-145 · El límite de 100 vencimientos es invisible
`app/static/admin.js:128`, `:130`
**Hoy**: se pide `limite=100` y el resumen dice «100 documentos con retención vencida»
aunque haya 4.000. La cifra del pie contradice el KPI de la cabecera, que sí cuenta todos.
**Debe**: paginar la tabla y que el resumen use el total real del servidor.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/routes/admin/retention.py`.

### OA-146 · El coloreado por urgencia es sólo color
`app/static/admin.js:136`
**Hoy**: `table-danger` por encima de 365 días y `table-warning` por encima de 90. Sin
icono, sin etiqueta, sin orden explicado. Para un usuario con daltonismo rojo-verde las
tres bandas son la misma fila gris, y el fondo `table-danger` de Bootstrap sobre texto
oscuro roza el mínimo de contraste.
**Debe**: una columna «Urgencia» con etiqueta e icono además del fondo, y comprobar los
dos fondos en `test_contraste.py`.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`, `app/tests/test_contraste.py`.

### OA-147 · Registrar una disposición son dos diálogos encadenados
`app/static/admin.js:226-256`
**Hoy**: primero un `promptModal` pide el acta, después un modal artesanal pide la
decisión. Si se cancela el segundo, el acta escrita se pierde. Y el orden está invertido:
se pide el respaldo de una decisión que aún no se ha tomado.
**Debe**: un solo formulario con decisión, acta, fecha y observaciones, y un resumen de lo
que se va a registrar antes de confirmar.
**Esfuerzo**: M. **Archivos**: `app/static/admin.js` `[CHOCA]`.

### OA-148 · El modal de disposición está construido a mano y se sale del sistema
`app/static/admin.js:259-293`
**Hoy**: `div` creado en JavaScript con `style` en línea, `class="modal fade show"` sin
Bootstrap detrás, sin trampa de foco, sin `role="dialog"`, sin `aria-modal`, sin devolver
el foco al cerrar y sin bloquear el desplazamiento del fondo. Escape funciona sólo si el
foco quedó dentro. Es el único modal del panel que no usa la infraestructura de
`admin-ui.js`.
**Debe**: reescribirlo sobre `confirmModal`/`promptModal` extendidos, o crear un
`choiceModal` reutilizable en `admin-ui.js`.
**Esfuerzo**: M. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`.

### OA-149 · Una disposición registrada no se puede rectificar
`app/routes/admin/retention.py:174-180`
**Hoy**: 409 con el mensaje «Rectificarla exige un acta nueva», pero no existe ningún
camino para hacerlo. La decisión es definitiva por omisión, no por diseño.
**Debe**: rectificación con acta nueva que **añade** un registro en lugar de sustituir,
conservando la cadena de decisiones.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/retention.py`, `app/main.py`, `app/static/admin.js` `[CHOCA]`.

### OA-150 · Un documento con disposición no lo parece en ninguna otra pantalla
`app/routes/admin/docs.py:76-99`
**Hoy**: `datos_archivo.disposicion` no viaja en `/list_all`, así que el monitor no
distingue un documento conservado permanentemente de uno marcado para expurgo. La única
pista es que desaparece de la tabla de vencidos.
**Debe**: exponer la disposición y mostrarla como badge en el monitor y en la ficha.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/static/admin-monitor.js` `[CHOCA]`.

---

## J. Pestaña Auditoría

### OA-151 · No hay filtros: sólo una caja de texto
`app/static/admin-ui.js:431-432`, `app/routes/admin/catalog.py:367-371`
**Hoy**: la búsqueda mira `accion` y `usuario`. No se puede filtrar por rango de fechas,
por módulo, por resultado (éxito/fallo) ni por tipo de evento, que son las cuatro cosas
que se piden en cualquier revisión. La tabla muestra columnas de módulo y resultado sobre
las que no se puede filtrar.
**Debe**: filtros por fecha, módulo, evento y resultado, combinables y reflejados en la URL.
**Esfuerzo**: M. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/routes/admin/catalog.py` `[CHOCA]`, `app/static/admin-users.js` `[CHOCA]`.

### OA-152 · La auditoría del panel de Archivo muestra los eventos de RRHH
`app/routes/admin/catalog.py:359-392`
**Hoy**: `get_audit_log` no filtra por módulo y el cliente tampoco lo pide
(`admin-users.js:634`). Un admin de Archivo ve quién editó qué expedientes de personal:
nombres de empleados en el campo `detalle` (`docs.py:604`).
**Debe**: filtrar por los módulos del usuario; sólo el admin Global ve todo.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/catalog.py` `[CHOCA]`, `app/static/admin-users.js` `[CHOCA]`.

### OA-153 · La auditoría no se puede exportar
`app/static/admin-ui.js:425-461`
**Hoy**: ningún botón de exportación, pese a que el monitor y la búsqueda pública sí lo
tienen. Entregar el registro de un periodo a una inspección obliga a copiar de la pantalla,
página a página, de 50 en 50.
**Debe**: exportación CSV del rango filtrado, generada en el servidor.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/catalog.py` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`.

### OA-154 · El detalle se trunca y sólo se puede leer con el ratón
`app/static/admin-users.js:658`
**Hoy**: una celda de 200 px con `text-overflow:ellipsis` y el texto completo en `title`.
El `CHANGELOG` 3.1.0 dice que existe un «modal de detalle al hacer clic en cada fila»: no
está en el código. En táctil y con teclado no hay forma de leer el detalle.
**Debe**: fila expandible o modal con el evento completo, incluyendo IP y agente si se
llegan a registrar.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`.

### OA-155 · La marca de tiempo se muestra en UTC sin decirlo
`app/routes/admin/catalog.py:383`
**Hoy**: `TO_CHAR(timestamp, 'YYYY-MM-DD HH24:MI:SS')` tal cual, mientras el resto del
panel muestra fechas en `dd/mm/aaaa` (`formatISOToSpanish`). Un evento de las 03:10 UTC
—la hora del respaldo automático— aparece como madrugada cuando en Venezuela son las
23:10 del día anterior.
**Debe**: convertir a la zona de la Facultad o etiquetar explícitamente «UTC», y usar el
mismo formato de fecha que el resto del panel.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/catalog.py` `[CHOCA]`, `app/static/admin-users.js` `[CHOCA]`.

### OA-156 · El resultado se colorea comparando cadenas a mano
`app/static/admin-users.js:648`
**Hoy**: `r === "Success" || r === "success" ? … : r === "Failure" ? …`. Cualquier otro
valor cae en gris, y el texto crudo del servidor («Success») se muestra en inglés en una
interfaz en español. Además es color sin icono.
**Debe**: un conjunto cerrado de resultados, traducidos, con badge e icono.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`, `app/database.py`.

### OA-157 · La tabla de auditoría oculta cuatro de sus seis columnas en móvil
`app/static/admin-ui.js:440-443`
**Hoy**: usuario, módulo y detalle llevan `ds-hide-sm`. A 390 px quedan fecha, evento y
resultado: una auditoría sin el actor no es una auditoría.
**Debe**: en pantallas estrechas, tarjetas apiladas con todos los campos en vez de una
tabla mutilada.
**Esfuerzo**: M. **Archivos**: `app/static/admin-users.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OA-158 · No hay estado de carga: la tabla parpadea de vacío a lleno
`app/static/admin-users.js:628-663`
**Hoy**: `loadAuditTab` no pinta esqueleto ni deshabilita el paginador mientras espera. Al
teclear en el buscador (que además dispara por tecla, OA-032), la tabla se queda con los
datos anteriores hasta que llega la respuesta.
**Debe**: `showTableSkeleton` como en el monitor, y `aria-busy`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`.

---

## K. Pestaña Acceso

### OA-159 · El formulario de alta de usuario está debajo de la tabla y sin `<form>`
`app/static/admin-ui.js:395-419`
**Hoy**: cuatro `<input>` sueltos y un botón, sin `<form>`, sin `<label>` (sólo
`placeholder`) y sin agrupar en `<fieldset>`. Enter no envía, el navegador no puede
autocompletar ni ofrecer generar contraseña, y los lectores de pantalla no anuncian el
propósito de cada campo.
**Debe**: un `<form>` real con `<label>` visibles, `autocomplete="new-password"` y envío
con Enter.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`.

### OA-160 · Los `placeholder` hacen de etiqueta
`app/static/admin-ui.js:400`, `:403`
**Hoy**: «Usuario» y «Contraseña» sólo como `placeholder`. Al escribir desaparecen y ya no
se sabe qué campo es cuál; y el gris del `::placeholder` de Bootstrap no llega a 4,5:1.
Este patrón se repite en todo el panel (catorce `placeholder` sin `<label>` en
`admin_archive.html`).
**Debe**: `<label>` visible siempre; el `placeholder` sólo para ejemplos.
**Esfuerzo**: M. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-161 · El módulo del usuario nuevo es un desplegable de una sola opción
`app/static/admin-ui.js:406-408`
**Hoy**: `<select>` con un `<option>`. Un control que no permite elegir nada, ocupando una
cuarta parte del formulario, y que además el backend ignora (OA-038).
**Debe**: texto estático «Módulo: Archivo», o un selector real cuando quien crea es admin
Global.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`.

### OA-162 · La tabla de usuarios se cae entera si falla la petición
`app/static/admin-users.js:534`, `:575-577`
**Hoy**: `container.innerHTML = …` sin comprobar que `container` exista, y el `catch`
sustituye la tabla por una alerta sin botón de reintento. Si `admin-ui.js` no llegó a
inyectar el pane (por ejemplo con un `SyntaxError` en otro script), esto lanza sobre `null`.
**Debe**: guarda de existencia, estado de error con reintento, y estado de carga.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`.

### OA-163 · El botón de estado activo es un interruptor disfrazado de botón
`app/static/admin-users.js:554-559`
**Hoy**: un `<button>` verde que dice «Activo» y que al pulsarlo desactiva. No se sabe si
describe el estado o la acción, no lleva `aria-pressed`, y no hay confirmación pese a que
desactivar a alguien lo deja fuera del sistema al instante.
**Debe**: un interruptor con `role="switch"` y `aria-checked`, o un botón cuya etiqueta
sea la acción («Desactivar») con el estado en una columna aparte.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`.

### OA-164 · «Último Acceso» se recorta a mano
`app/static/admin-users.js:544`
**Hoy**: `u.last_login.substring(0,16).replace('T',' ')`. Da `2026-08-17 14:32` en formato
ISO, distinto del `dd/mm/aaaa` del resto del panel, y sin zona horaria (mismo problema que
OA-155). Con `null` muestra «Nunca», que está bien, pero sin distinguir «cuenta recién
creada» de «cuenta abandonada».
**Debe**: `formatRelativeTime` con la fecha exacta en `title`, y marcar en la tabla las
cuentas sin acceso en más de 90 días.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`.

### OA-165 · La tabla de usuarios no se puede buscar ni ordenar
`app/static/admin-users.js:538-574`
**Hoy**: todos los usuarios del módulo más los globales, sin paginar, sin buscar, sin
ordenar y sin filtrar por rol o por estado.
**Debe**: buscador, filtros por rol y estado, y orden por último acceso —que es como se
detectan las cuentas a revisar.
**Esfuerzo**: M. **Archivos**: `app/static/admin-users.js` `[CHOCA]`, `app/routes/admin/users.py` `[CHOCA]`.

### OA-166 · No hay historial de cambios de permisos por usuario
`app/routes/admin/users.py:204`, `:217`, `:231`, `:242`
**Hoy**: los eventos van a `audit_log`, mezclados con todo lo demás. No hay una vista «qué
se le ha hecho a esta cuenta», que es lo primero que se pregunta cuando alguien accede a
donde no debe.
**Debe**: pestaña o modal de historial por usuario, filtrando `audit_log` por la cuenta.
**Esfuerzo**: M. **Archivos**: `app/static/admin-users.js` `[CHOCA]`, `app/routes/admin/catalog.py` `[CHOCA]`.

---

## L. Pestaña Exportar

### OA-167 · La exportación no dice qué se lleva ni cuánto pesa
`app/static/admin_archive.html:489`, `app/static/admin.js:96-98`
**Hoy**: «Descarga un JSON con documentos, descriptores y tipos». No se dice el número de
registros, ni el tamaño estimado, ni que `tipo_documento` se exporta entero —incluidos los
tipos de RRHH, porque la tabla no está particionada por módulo—.
**Debe**: previsualización con recuentos por tabla y tamaño aproximado antes de descargar.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/routes/backup.py`.

### OA-168 · La exportación por módulo se lleva tipos del otro módulo
`app/static/admin.js:98`
**Hoy**: la lista de tablas incluye `tipo_documento` sin filtro. Un «backup Archivo»
contiene las cuatro Partes de RRHH. No es una fuga grave —son nombres de tipo— pero el
archivo no es lo que dice ser, y restaurarlo en otro entorno reintroduce catálogos ajenos.
**Debe**: exportar sólo los tipos cuya categoría pertenece al módulo.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/routes/backup.py`.

### OA-169 · JSON es el único formato de salida
`app/static/admin_archive.html:490-492`
**Hoy**: un botón, un formato. Para una transferencia archivística hace falta al menos
EAD/XML o CSV; para un informe, XLSX o PDF. JSON sólo sirve para restaurar en este mismo
sistema, lo que hace la exportación inútil como salida de datos.
**Debe**: JSON (respaldo), CSV/XLSX (informe) y EAD 2002 o RiC (transferencia).
**Esfuerzo**: L. **Archivos**: `app/routes/backup.py`, `app/static/admin_archive.html`, `app/static/admin.js` `[CHOCA]`.

### OA-170 · Una exportación grande bloquea la pantalla sin señal
`app/static/admin.js:100-113`
**Hoy**: `res.blob()` acumula todo en memoria mientras el texto dice «Generando backup…»
sin progreso ni cancelación. Con un fondo grande, el navegador puede quedarse sin memoria
y la pestaña no responde.
**Debe**: descarga en streaming con progreso y botón de cancelar; o generación asíncrona
que deje el archivo en R2 y avise cuando esté.
**Esfuerzo**: M. **Archivos**: `app/routes/backup.py`, `app/static/admin.js` `[CHOCA]`.

### OA-171 · No hay historial de exportaciones en esta pestaña
`app/static/admin_archive.html:483-496`
**Hoy**: `backup_history` existe y se alimenta, pero sólo se ve en el panel de Sistema
Global. El admin de Archivo no puede saber cuándo se exportó su módulo por última vez, que
es exactamente la pregunta que trae a esta pestaña.
**Debe**: tabla con las últimas exportaciones del módulo: fecha, quién, filas, tamaño.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/routes/backup.py`, `app/static/admin.js` `[CHOCA]`.

---

## M. Modales, foco y teclado

### OA-172 · Ningún modal atrapa el foco
`app/static/admin_archive.html:535`, `:589`, `app/static/admin-ui.js:19`, `:35`, `app/static/admin.js:270`
**Hoy**: Bootstrap 4 mueve el foco al abrir, pero con `data-backdrop="static"` y
`data-keyboard="false"` (`admin_archive.html:535`, `:572`) la tabulación sigue recorriendo
la página de detrás. Con el modal de edición abierto se puede llegar tabulando a la tabla
del monitor y activar un botón de eliminar que no se ve.
**Debe**: trampa de foco real en los cinco modales, y `aria-hidden`/`inert` sobre el resto
del documento mientras haya uno abierto.
**Esfuerzo**: M. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/static/admin_archive.html`, `app/static/admin.js` `[CHOCA]`.

### OA-173 · El foco no vuelve al cerrar
`app/static/admin-edit.js:78`, `:213`
**Hoy**: se abre el modal desde el botón «Editar» de la fila 14 y al cerrar el foco vuelve
al `<body>`. Navegando con teclado hay que recorrer toda la página otra vez para llegar a
la fila 15.
**Debe**: guardar el elemento que abrió el modal y devolverle el foco en `hidden.bs.modal`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit.js`, `app/static/admin-ui.js` `[CHOCA]`.

### OA-174 · Escape no cierra el modal de edición
`app/static/admin_archive.html:535`, `app/static/admin-ui.js:165-173`
**Hoy**: `#doc-modal` lleva `data-keyboard="false"`, y el manejador global de Escape de
`admin-ui.js` cierra «el primer modal abierto» que encuentre con `querySelector`, que no
tiene por qué ser el que está encima si hay dos apilados (edición + confirmación de
versión).
**Debe**: una pila de modales; Escape cierra el último abierto, y confirma antes si hay
cambios sin guardar.
**Esfuerzo**: M. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-175 · Ctrl+S guarda el modal sin comprobar si el botón está activo
`app/static/admin-ui.js:174-182`
**Hoy**: busca `.btn-save-modal, [data-save-modal]` y hace `.click()`. Si el guardado ya
está en curso, dispara un segundo envío: dos `PUT` concurrentes sobre el mismo documento.
Y el atajo no está documentado en ninguna parte de la interfaz.
**Debe**: ignorar si el botón está deshabilitado, deshabilitarlo durante el envío, y
mostrar los atajos en una ayuda accesible con «?».
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`.

### OA-176 · No hay más atajos que Escape y Ctrl+S
`app/static/admin-ui.js:164-183`
**Hoy**: dos atajos para un backoffice de nueve pestañas. Ni «/» para buscar, ni «n» para
nueva entrada, ni navegación por las pestañas con números.
**Debe**: un conjunto mínimo documentado, respetando que no se disparen dentro de campos
de texto.
**Esfuerzo**: M. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`.

### OA-177 · Las flechas cambian de pestaña y además la activan
`app/static/admin-ui.js:353-366`
**Hoy**: `next.focus(); next.click();`. Cada pulsación de flecha carga una pestaña entera
con sus peticiones. Recorrer las nueve dispara nueve cargas. El patrón ARIA correcto es
selección manual: la flecha mueve el foco, Enter o Espacio activan.
**Debe**: `aria-selected`, `tabindex` móvil (roving) y activación con Enter/Espacio.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`.

### OA-178 · Las pestañas no llevan `aria-selected` ni `aria-controls`
`app/static/admin_archive.html:136-185`
**Hoy**: nueve `role="tab"` sin `aria-selected`, sin `aria-controls` y con los paneles sin
`aria-labelledby` ni `tabindex="0"`. El estado activo es sólo la clase `active` y el
color. Un lector de pantalla no sabe qué pestaña está seleccionada.
**Debe**: los tres atributos, actualizados en `loadAdminTab`.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/admin.js` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`.

### OA-179 · Las pestañas duplican `title` y `aria-label` con el mismo texto
`app/static/admin_archive.html:136`, `:141`, …
**Hoy**: `title="Resumen" aria-label="Resumen"` sobre un enlace cuyo `<span>` ya dice
«Resumen». El nombre accesible se calcula tres veces, y en móvil —donde el `<span>` se
oculta por CSS (`styles.css:2741`)— el `aria-label` es el único que queda, así que hace
falta, pero el `title` sobra y genera un tooltip nativo que tapa la pestaña siguiente.
**Debe**: conservar sólo `aria-label`.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`.

### OA-180 · El modal de detalle no tiene cabecera ni botón de cierre visible arriba
`app/static/admin_archive.html:535-569`
**Hoy**: el título va dentro del cuerpo (`:541`) y el único cierre está en el pie, después
de tres botones de acción. No hay `aria-labelledby` que conecte el diálogo con su título.
Los otros modales del panel sí tienen `modal-header` con «×» (`:592-597`).
**Debe**: cabecera con título y cierre, y `aria-labelledby`.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`.

### OA-181 · Los tres botones del pie del modal de detalle sólo llevan icono
`app/static/admin_archive.html:563-565`
**Hoy**: «Visualizar», «Editar» y «Descargar» son un ojo, un lápiz y una flecha con
`title`. Dos empiezan ocultos (`d-none`) y aparecen según el registro, de modo que el pie
cambia de anchura y los botones bailan de sitio entre documentos.
**Debe**: etiquetas de texto, `aria-label` y posición estable (deshabilitados en vez de
ocultos).
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-monitor.js` `[CHOCA]`.

### OA-182 · Cerrar el modal de edición no avisa de los cambios pendientes
`app/static/admin_archive.html:736`, `app/static/admin-edit.js:181`
**Hoy**: Cancelar, Escape o clic fuera descartan dieciocho campos editados sin preguntar.
Con la subida de archivo ya efectuada (OA-010), además se deja basura en R2.
**Debe**: marcar el formulario como sucio al primer cambio y confirmar antes de descartar.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit.js`.

---

## N. Estética y sistema visual

### OA-183 · `btn-xs` no existe: todos los botones de acción de las tablas salen grandes
`app/static/admin-monitor.js:158`, `:180-183`, `app/static/admin.js:146`, `app/static/admin-users.js:554`, `:563`, `:566`, `app/static/admin-edit.js:276-277`, `:310-311`, `:387-388`
**Hoy**: veinte usos de `.btn-xs`. Bootstrap 4 eliminó esa clase, AdminLTE —que la
definía— no se carga, y `styles.css` no la define (0 coincidencias). Todos esos botones
heredan el tamaño de `.btn`: `0.375rem 0.75rem` y `1rem` de fuente. En la fila del
monitor son cinco botones de tamaño completo que se comen la columna de acciones y
descuadran la altura de la fila. Existe `.ds-tbl-btn` (`styles.css:2716`) hecha justo para
esto, y no la usa nadie.
**Debe**: sustituir `btn-xs` por `ds-tbl-btn` en los veinte sitios, y añadir una guarda que
rechace clases de AdminLTE sin regla propia.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin.js` `[CHOCA]`, `app/static/admin-users.js` `[CHOCA]`, `app/static/admin-edit.js`, `app/static/styles.css` `[CHOCA]`, `app/tests/test_static_assets.py`.

### OA-184 · `.ds-admin-tabs` está definida dos veces con valores incompatibles
`app/static/styles.css:1710-1741` y `:2657-2683`
**Hoy**: dos bloques completos. El primero da `padding: 6px 14px`, `font-size: 0.82rem`,
`border` y una barra de desplazamiento de 3 px visible; el segundo la oculta y cambia
padding, tamaño y colores. Gana el segundo por orden, pero el primero sigue aplicando lo
que el segundo no redefine (los `border-color` del `:hover`), así que el resultado es una
mezcla que nadie diseñó. Lo mismo con `.ds-kpi-card` (`:1669`) frente a `.ds-kpi-mini`
(`:2702`).
**Debe**: una sola definición por componente; borrar la muerta.
**Esfuerzo**: M. **Archivos**: `app/static/styles.css` `[CHOCA]`.

### OA-185 · Cuatro `!important` por regla en la capa de administración
`app/static/styles.css:2681-2686`, `:2702-2706`, `:2716-2721`, `:2726-2728`
**Hoy**: el panel se impone a Bootstrap a base de `!important` (más de 400 en el archivo).
Cualquier ajuste puntual —un botón que debe ser más pequeño en una tabla concreta— exige
otro `!important`, y la especificidad ya no es una herramienta.
**Debe**: una capa `@layer` o un prefijo de espacio de nombres que gane por especificidad
natural, y retirar los `!important` que sólo estaban ahí por orden de carga.
**Esfuerzo**: L. **Archivos**: `app/static/styles.css` `[CHOCA]`.

### OA-186 · Dos bloques `<style>` en línea dentro de la página
`app/static/admin_archive.html:17-22`, `:749-756`
**Hoy**: el primero redefine `.card`, `.info-box` y `.nav-pills` con `!important`; el
segundo duplica `.ds-skeleton`, que ya está en `styles.css:2144`, con colores claros
codificados que en modo oscuro chocan con la versión de `styles.css:2484`. Resultado: el
esqueleto de carga parpadea en gris claro sobre fondo oscuro según qué regla gane.
**Debe**: mover todo a `styles.css` y borrar los bloques en línea.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/styles.css` `[CHOCA]`.

### OA-187 · Seis colores de tarjeta de Bootstrap usados como decoración
`app/static/admin_archive.html:333` (`card-info`), `:394` (`card-warning`), `:417`
(`card-secondary`), `:440` (`card-secondary`), `:459` (`card-warning`), `:484`
(`card-info`), `:500` (`card-danger`), `:277` (`card-primary`), `admin-ui.js:390`
(`card-danger`), `:428` (`card-secondary`), `admin-categories.js:413` (`card-info`)
**Hoy**: azul para el monitor, amarillo para tipologías, gris para retención, rojo para
papelera y para acceso. No hay ninguna regla detrás: el color no significa gravedad, ni
módulo, ni frecuencia de uso. Con nueve pestañas, el panel parece once aplicaciones
distintas.
**Debe**: tarjeta neutra por defecto; el color reservado a los tres estados semánticos
(alerta, aviso, correcto) y a los datos.
**Esfuerzo**: M. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-ui.js` `[CHOCA]`, `app/static/admin-categories.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OA-188 · Cinco radios de borde distintos en la misma pantalla
`admin_archive.html:20` (`0.5rem`), `:410` (`8px`), `:537` (`14px`), `admin-ui.js:21`
(`12px`), `:258` (menú), `styles.css:2669` (`6px`), `:2757` (`8px`), `:2795` (`8px`),
`:2144` (`4px`)
**Hoy**: 4, 5, 6, 8, 12 y 14 px conviven. Un modal con esquinas de 14 px contiene tarjetas
de 8 y botones de 4.
**Debe**: una escala de tres radios (`--ds-radius-sm/md/lg`) declarada como token y usada
en todas partes.
**Esfuerzo**: M. **Archivos**: `app/static/styles.css` `[CHOCA]`, `app/static/admin_archive.html`, `app/static/admin-ui.js` `[CHOCA]`.

### OA-189 · Tamaños de fuente codificados en `style` en línea por toda la página
`admin_archive.html:279`, `:291`, `:294`, `:358`, `:373`, `:382`, `:447`, `:466`, `:486`, `:671`, `:686`, `admin-monitor.js:113`, `admin-submit.js:220`, `:229`, `:232`, `admin-users.js:539`, `admin-ui.js:436`
**Hoy**: `0.6rem`, `0.65rem`, `0.68rem`, `0.7rem`, `0.72rem`, `0.75rem`, `0.78rem`,
`0.8rem`, `0.82rem`, `0.85rem`, `0.87rem`, `0.9rem`. Doce tamaños entre 0,6 y 0,9 rem, la
mayoría en atributos `style` que ninguna hoja puede corregir y que el escalado de fuente
del panel de personalización (`ds_font_scale`, `app-theme.js:214`) no puede tocar.
**Debe**: una escala tipográfica de cinco pasos en tokens, y retirar los `style` de tamaño.
**Esfuerzo**: L. **Archivos**: `app/static/styles.css` `[CHOCA]`, `app/static/admin_archive.html`, `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin-submit.js` `[CHOCA]`, `app/static/admin-users.js` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`.

### OA-190 · Colores literales dentro del JavaScript
`admin-edit.js:66`, `:146`, `:157`, `:162`, `:170`, `admin_archive.html:679-681`,
`admin-categories.js:367`, `admin-ui.js:155`, `:205`, `:320`, `app-core.js:39-43`, `:88`
**Hoy**: `#adb5bd`, `#f8f9fa`, `#fd7e14`, `#28a745`, `#dc3545`, `#fff8f0`, `#f0fff4`,
`#fff5f5`, `#333`, `#fff176`… escritos en cadenas. En modo oscuro, la zona de arrastre del
modal de edición se pinta gris claro sobre fondo oscuro, y el marcador de coincidencias de
búsqueda es amarillo brillante con texto oscuro que desaparece.
**Debe**: variables CSS y clases; el JavaScript alterna clases, nunca colores.
**Esfuerzo**: M. **Archivos**: `app/static/admin-edit.js`, `app/static/admin-ui.js` `[CHOCA]`, `app/static/app-core.js` `[CHOCA]`, `app/static/admin_archive.html`, `app/static/styles.css` `[CHOCA]`.

### OA-191 · Los estados de la zona de arrastre se pintan manipulando `style`
`app/static/admin_archive.html:678-683`, `app/static/admin-edit.js:146`, `:157`, `:162`
**Hoy**: `ondragover`/`ondragleave` en atributos HTML que escriben `this.style.borderColor`.
Si el archivo se suelta fuera del elemento, `dragleave` no llega y la zona se queda azul
para siempre. Y el estado de éxito (verde) no se limpia al abrir otro documento salvo por
un reinicio explícito (`admin-edit.js:66`).
**Debe**: clases `.is-dragover`, `.is-uploading`, `.is-ok`, `.is-error` y un `dragend`
global de seguridad.
**Esfuerzo**: S. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-edit.js`, `app/static/styles.css` `[CHOCA]`.

### OA-192 · El tooltip propio no es accesible y se pega al cursor
`app/static/admin-ui.js:313-329`
**Hoy**: se muestra sólo con `mouseover`, sin `focus`, sin `role="tooltip"`, sin
`aria-describedby`, con fondo `#333` fijo (ilegible junto al modo oscuro del resto) y
posicionado a 10 px del cursor, de modo que cerca del borde derecho se sale de la ventana.
**Debe**: mostrar también al recibir foco, `role="tooltip"` con `aria-describedby`, y
recolocación dentro de la ventana.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OA-193 · Los toasts se apilan sin límite y tapan el panel
`app/static/app-core.js:32-61`, `app/static/admin_archive.html:586`
**Hoy**: contenedor `position:fixed` a 70 px del borde superior, sin tope de cantidad. Una
importación con veinte avisos apila veinte toasts que cubren la columna derecha durante
seis segundos. Además el contenedor no tiene `role="status"` ni `aria-live`, así que los
mensajes no se anuncian.
**Debe**: máximo de tres visibles con agrupación del resto, `aria-live="polite"` y
`aria-live="assertive"` para los errores.
**Esfuerzo**: S. **Archivos**: `app/static/app-core.js` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-194 · Los toasts son la única señal de resultado de las acciones destructivas
`app/static/admin-edit.js:232`, `:341`, `:359`, `app/static/admin-users.js:601`
**Hoy**: se purga un documento y la única confirmación es un toast de tres segundos arriba
a la derecha, lejos de donde estaba el ratón. Si se pierde, no hay forma de saber si la
operación se completó.
**Debe**: confirmación en el sitio de la acción (la fila se atenúa y desaparece con una
transición) más un registro de actividad reciente consultable.
**Esfuerzo**: M. **Archivos**: `app/static/admin-edit.js`, `app/static/styles.css` `[CHOCA]`.

---

## O. Responsive, modo oscuro y densidad

### OA-195 · A 390 px la tabla del monitor pierde tres columnas de seis
`app/static/admin_archive.html:361-366`
**Hoy**: autor, fecha y tipo llevan `ds-hide-sm`/`ds-hide-xs`. Quedan título, estado y
acciones; las acciones son cinco botones de tamaño completo (OA-183) que fuerzan
desplazamiento horizontal dentro del `.table-responsive` pese a que el `CHANGELOG` 3.3.0
afirma «sin desborde horizontal en ninguna página ni pestaña».
**Debe**: en móvil, tarjetas apiladas con los campos etiquetados y un menú de acciones.
**Esfuerzo**: M. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-monitor.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OA-196 · La tabla de vencimientos y la de usuarios no ocultan nada en móvil
`app/static/admin_archive.html:468`, `app/static/admin-users.js:540`
**Hoy**: ocho y siete columnas respectivamente, con una sola marcada `ds-hide-sm` en la
primera y ninguna en la segunda. A 390 px ambas desbordan a lo ancho dentro de su envoltura.
**Debe**: mismo tratamiento de tarjetas que OA-195, o priorización explícita de columnas.
**Esfuerzo**: M. **Archivos**: `app/static/admin_archive.html`, `app/static/admin-users.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OA-197 · Ocho KPIs a 390 px son ocho pantallazos de desplazamiento
`app/static/styles.css:3396-3405`
**Hoy**: `minmax(132px, 1fr)` da dos columnas a 390 px: cuatro filas de tarjetas antes de
llegar a las pestañas. Se entra al panel y no se ve ni una pestaña sin desplazarse.
**Debe**: en móvil, dos o tres cifras clave y el resto tras «Ver todas las cifras».
**Esfuerzo**: M. **Archivos**: `app/static/styles.css` `[CHOCA]`, `app/static/admin_archive.html`.

### OA-198 · La densidad compacta no toca casi nada del panel
`app/static/styles.css:2827-2836`
**Hoy**: diez reglas para `card-body`, `card-header`, `form-group`, `table td/th`,
`btn-sm`, `modal-body` y `ds-kpi-mini`. No afecta a la rejilla de KPIs, ni a la barra de
pestañas, ni a las tarjetas de gráfico (que siguen a 268 px), ni al formulario de alta.
En un panel cuyo problema es la densidad, el interruptor de densidad hace poco.
**Debe**: que la densidad sea un token (`--ds-space`) del que dependan los espaciados, en
vez de una lista de excepciones.
**Esfuerzo**: M. **Archivos**: `app/static/styles.css` `[CHOCA]`.

### OA-199 · El modo oscuro se resuelve con 190 reglas `!important`
`app/static/styles.css:2293-2500`
**Hoy**: cada componente se repinta con `body.dark-mode .x { … !important }`. Los estilos
en línea (OA-190) se escapan igualmente, y añadir un componente exige acordarse de añadir
su pareja oscura — que es exactamente lo que pasó con la consola de IA según el
`CHANGELOG` 3.3.0.
**Debe**: tokens de color en `:root` redefinidos bajo `body.dark-mode`, y componentes que
usen sólo tokens.
**Esfuerzo**: L. **Archivos**: `app/static/styles.css` `[CHOCA]`.

### OA-200 · La barra de pestañas no indica que hay más pestañas a la izquierda
`app/static/admin-ui.js:336-344`
**Hoy**: `_syncTabOverflow` sólo marca `ds-has-overflow` cuando queda contenido a la
**derecha**. Desplazada hasta el final, no hay ninguna señal de que las cinco primeras
pestañas siguen existiendo a la izquierda. Y la barra de desplazamiento está oculta
(`styles.css:2666`).
**Debe**: degradados en ambos extremos según la posición, con botones de flecha en
escritorio.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OA-201 · La animación de entrada de los KPI se repite en cada cambio de pestaña
`app/static/styles.css:3497`
**Hoy**: `.ds-kpi-grid .ds-kpi-mini { animation: ds-fade .3s both; }`. La rejilla está
fuera de los panes, así que no se vuelve a montar; pero al volver a «Resumen» y reescribir
los valores, cualquier repintado que reinicie la animación hace parpadear las ocho
tarjetas. El movimiento debe explicar de dónde viene el contenido, y aquí no viene de
ninguna parte: ya estaba.
**Debe**: animar sólo en la primera carga, marcando la rejilla con una clase que se retire
después.
**Esfuerzo**: S. **Archivos**: `app/static/styles.css` `[CHOCA]`, `app/static/admin-charts.js` `[CHOCA]`.

---

## P. Funcionalidad ausente frente a AtoM / Alfresco / SharePoint / Archivematica

Cada uno de estos cinco es un bloque de trabajo, no un arreglo. Se listan por orden de
distancia respecto a lo que un archivo institucional necesita para funcionar como tal.

### OA-202 · No hay cuadro de clasificación ni unidades de descripción multinivel
`app/schema.sql` (`datos_archivo`), `app/static/admin-submit.js:28-33`
**Hoy**: el fondo es una lista plana de documentos con un `tipo_documento` y un campo de
texto llamado «Clasificación» que ofrece Parte I–IV (OA-056). No existen fondo, subfondo,
sección, serie, unidad documental compuesta ni unidad simple: la jerarquía
`ISAD(G) 3.1.4` (nivel de descripción) simplemente no está. AtoM la tiene como columna
vertebral; sin ella no se puede describir una serie, ni heredar metadatos, ni navegar el
fondo como árbol, ni transferir a otro sistema.
**Debe**: tabla `unidad_descripcion` autorreferente con `nivel` (fondo/sección/serie/
unidad), navegador en árbol en la pestaña Documentos, herencia de metadatos del padre, y
migración que cuelgue lo existente de un fondo por defecto.
**Esfuerzo**: L. **Archivos**: `app/main.py`, `app/models.py` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`, `app/static/admin_archive.html`, `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin-submit.js` `[CHOCA]`.

### OA-203 · ISAD(G) está a medias e ISAAR no existe
`app/static/admin_archive.html:640-674`
**Hoy**: de los 26 elementos de ISAD(G) hay siete: título, fechas, autor, ubicación,
folio, soporte y páginas. Faltan código de referencia normalizado, nivel de descripción,
volumen y soporte de la unidad, historia institucional, historia archivística, forma de
ingreso, alcance y contenido, valoración y selección, nuevos ingresos, organización,
condiciones de acceso, condiciones de reproducción, características físicas,
instrumentos de descripción, existencia de originales y copias, unidades relacionadas y
nota de publicación. Y no hay ningún registro de autoridad ISAAR(CPF): los productores son
texto libre en «Autor» y «Personas relacionadas» (OA-100).
**Debe**: por fases — primero los elementos obligatorios de ISAD(G) que faltan (código de
referencia, nivel, volumen, alcance y contenido, condiciones de acceso), después la tabla
de autoridades con sus relaciones y su vinculación N:N a las unidades.
**Esfuerzo**: L. **Archivos**: `app/main.py`, `app/models.py` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`, `app/static/admin_archive.html`, `app/static/admin-submit.js` `[CHOCA]`, `app/static/admin-edit.js`.

### OA-204 · No hay preservación digital: ni formatos, ni sumas de verificación, ni sellos
`app/routes/files.py:298-343`, `app/storage.py`
**Hoy**: se sube el archivo a R2 y se guarda una URL. No se calcula ninguna suma de
verificación, así que no hay forma de detectar corrupción; no se identifica el formato
(PRONOM/PUID) ni se avisa de formatos en riesgo; no se normaliza a PDF/A pese a que la
propia interfaz lo recomienda (`admin_archive.html:315`); no hay sello de tiempo; y no se
guarda ningún metadato técnico. Es la diferencia entre almacenar y preservar, y es lo
único que hace Archivematica.
**Debe**: SHA-256 al subir, guardado junto al objeto y verificado periódicamente por el
cron; identificación de formato con Siegfried o su equivalente; informe de formatos en
riesgo; y sello de tiempo del registro en el momento de la aprobación.
**Esfuerzo**: L. **Archivos**: `app/storage.py`, `app/routes/files.py`, `app/main.py`, `app/routes/backup.py`, `app/static/admin_archive.html`.

### OA-205 · El flujo de aprobación es un campo de estado, no un flujo
`app/routes/admin/docs.py:454-476`
**Hoy**: cuatro valores en una columna que cualquiera con sesión puede cambiar en cualquier
dirección. No hay revisor asignado, ni comentarios (OA-113), ni plazo, ni notificación, ni
historial de quién pasó qué a qué y cuándo — sólo una línea suelta en `audit_log`. Se
puede pasar de «rechazado» a «aprobado» sin que conste nada.
**Debe**: tabla de transiciones con actor, comentario y marca de tiempo; transiciones
permitidas declaradas; revisor asignado; y vista del flujo en la ficha del documento.
**Esfuerzo**: L. **Archivos**: `app/main.py`, `app/routes/admin/docs.py` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`, `app/static/admin-monitor.js` `[CHOCA]`.

### OA-206 · No hay informes ni ninguna forma de sacar una estadística a papel
`app/static/admin_archive.html:193-251`
**Hoy**: cuatro gráficas en pantalla y una exportación JSON. No hay informe de ingresos
por periodo, ni inventario de una serie, ni relación de entrega, ni certificado de
eliminación, ni estadística anual — documentos que un archivo tiene que emitir con
regularidad y que hoy se hacen a mano en una hoja de cálculo. RRHH sí tiene generación de
un reporte imprimible del expediente (`hr.py`); Archivo no tiene nada equivalente.
**Debe**: un generador de informes con plantillas (inventario, relación de entrega,
estadística de ingresos, certificado de eliminación) que salga a PDF imprimible, con los
filtros aplicados y la firma del responsable.
**Esfuerzo**: L. **Archivos**: `app/routes/admin/`, `app/static/admin_archive.html`, `app/static/admin.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

---

## Q. Rendimiento, concurrencia y deuda técnica

### OA-207 · Las cifras del tablero cargan la tabla entera en pandas
`app/routes/admin/stats.py:22-30`, `app/routes/archive.py` (`fetch_archive_dataframe`)
**Hoy**: `POST /stats` trae todo `datos_archivo` a memoria para contar filas y tipos
distintos. Con 50.000 documentos son decenas de MB por Neon, en un lambda con memoria
limitada, y todo para devolver dos enteros. Y el filtro por fecha se aplica en Python,
donde la comparación es entre cadenas.
**Debe**: dos `COUNT` en SQL con el rango como parámetro; retirar pandas de esta ruta.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`, `app/routes/archive.py` `[CHOCA]`.

### OA-208 · `upsert_descriptors` hace dos viajes a la base por palabra clave
`app/routes/admin/helpers.py:53-63`
**Hoy**: bucle con un `INSERT … RETURNING` y un `INSERT` de enlace por descriptor, cada
uno con su `commit`. Un documento con quince palabras clave son treinta viajes a Neon —que
está en otro continente— por alta. Se ejecuta también en cada edición, porque el `PUT`
borra todos los enlaces y los recrea (`docs.py:402-408`).
**Debe**: un `INSERT … SELECT unnest(%s)` con `ON CONFLICT` para los descriptores y otro
para los enlaces, en una sola transacción.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/helpers.py` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`.

### OA-209 · Cada operación de escritura hace su propio `commit`
`app/database.py` (`db_query(commit=True)`), usado en todo `docs.py`, `imports.py`, `trash.py`
**Hoy**: no existe forma de agrupar varias sentencias en una transacción. El alta de un
documento con palabras clave son N commits independientes; un fallo a mitad deja el
documento creado y los descriptores a medias. El propio `CLAUDE.md` presenta `db_query`
como «único helper», y esa es justamente la limitación.
**Debe**: un gestor de contexto `db_transaction()` que ceda una conexión y confirme una
vez, sin sustituir a `db_query` para el caso simple.
**Esfuerzo**: M. **Archivos**: `app/database.py`, `app/routes/admin/docs.py` `[CHOCA]`, `app/routes/admin/imports.py` `[CHOCA]`, `app/routes/trash.py` `[CHOCA]`.

### OA-210 · El pane de Resumen dispara cuatro peticiones en cada entrada
`app/static/admin.js:15`
**Hoy**: `loadDynamicStats()` lanza `/stats` y `/charts`, y `_loadAlertasBanner()` lanza
`/retencion/vencimientos?limite=100`. Volver a «Resumen» tras editar un documento repite
las tres, sin caché ni ventana de validez, incluso si han pasado dos segundos. Con
`limite=100` la tercera trae cien filas completas para contar tres.
**Debe**: caché de corta duración por pestaña con invalidación al escribir, y un endpoint
de conteo para el banner.
**Esfuerzo**: M. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/static/admin-stats.js` `[CHOCA]`, `app/routes/admin/retention.py`.

### OA-211 · Quince `<script>` sin `defer` bloquean el análisis de la página
`app/static/admin_archive.html:758-777`
**Hoy**: jQuery, Bootstrap y trece archivos propios en secuencia al final del `<body>`,
sin `defer` ni módulos. Son quince peticiones bloqueantes; el primer pintado útil llega
después de todas. Y el orden importa de forma implícita —`admin-ui.js` antes que `app.js`
para que sus paneles existan (`admin-ui.js:384-385`)—, una dependencia frágil que no
declara nada.
**Debe**: módulos ES con importaciones explícitas, o al menos `defer` y un empaquetado
mínimo.
**Esfuerzo**: L. **Archivos**: `app/static/admin_archive.html`, todos los `app/static/*.js` `[CHOCA]`.

### OA-212 · Ninguna prueba mira el backoffice desde el navegador
`app/tests/`
**Hoy**: 361 pruebas y todas de servidor o de análisis estático. `test_admin_panels.py`
comprueba que cada pestaña tenga panel, pero nada comprueba que el panel se rellene, que
la tabla no desborde a 390 px, que el modal atrape el foco, que el modo oscuro no deje
texto ilegible ni que `btn-xs` (OA-183) no exista. El propio `CLAUDE.md` lo dice: «el
frontend no se prueba solo mirando el código». Todos los fallos de esta auditoría que
sobrevivieron a 361 pruebas son de esa clase.
**Debe**: pruebas de navegador (Playwright) para las nueve pestañas en los tres roles, con
capturas de referencia a 390/768/1440 px en claro y oscuro, axe-core en cada pestaña —no
sólo en la carga inicial— y una guarda que rechace clases CSS sin regla definida.
**Esfuerzo**: L. **Archivos**: `app/tests/` (nuevo `test_backoffice_e2e.py`), `requirements-dev.txt`.

---

## Cierre

Los diez primeros por impacto, si hubiera que empezar hoy:

1. **OA-035** — todo `/api/admin` sin control de rol: un usuario Normal de RRHH es
   administrador de Archivo con `curl`.
2. **OA-036** — quien actúa lo declara el cliente: la auditoría es falsificable.
3. **OA-006** — purgar no borra el archivo digital: el fondo acumula huérfanos invisibles.
4. **OA-008 / OA-009** — el historial de versiones pierde versiones en sus dos operaciones
   principales.
5. **OA-183** — `btn-xs` no existe: veinte botones de acción salen a tamaño completo en
   todas las tablas del panel.
6. **OA-001 / OA-002 / OA-003** — la retención da tres cifras distintas y la fecha de
   vencimiento explícita no hace nada.
7. **OA-022** — la importación CSV no es transaccional ni idempotente y no se puede
   deshacer.
8. **OA-103** — sin acciones en lote, el backoffice no escala más allá de unos cientos de
   documentos.
9. **OA-202** — sin cuadro de clasificación jerárquico esto es un inventario, no un
   archivo.
10. **OA-212** — ninguna prueba mira la pantalla, que es donde vive todo lo anterior.
