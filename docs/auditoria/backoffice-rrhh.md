# Auditoría del backoffice del módulo RRHH

Revisión exhaustiva de `app/static/admin_hr.html` y de las ramas «rrhh» de
`admin.js`, `admin-stats.js`, `admin-charts.js`, `admin-monitor.js`,
`admin-submit.js`, `admin-edit-hr.js`, `admin-categories.js`, `admin-ui.js` y
`admin-users.js`, más el backend en `app/routes/hr.py`, `app/routes/hr_alerts.py`,
`app/routes/admin/` y las ramas RRHH de `app/routes/trash.py`.

**Alcance recorrido**: las nueve pestañas (Resumen · Ingresar · Expedientes · Tipos ·
Papelera · Retención · Auditoría · Acceso · Exportar), cada control, campo, botón,
modal, tabla, gráfico y estado, para los tres roles (usuario RRHH, admin RRHH,
admin Global), incluyendo alta de empleado, dossier, historial de cargos, alertas
de jubilación/pensión y reporte imprimible del expediente.

**Convenciones de este documento**

- `[CHOCA]` marca los pendientes que tocan `styles.css`, `app.js`, `app-core.js`,
  `app-shell.js`, `app-theme.js`, `admin.js`, `admin-ui.js`, `admin-charts.js`,
  `admin-submit.js`, `admin-monitor.js`, `admin-edit.js`, `admin-categories.js`,
  `admin-users.js`, `models.py`, `docs.py`, `stats.py`, `catalog.py`, `imports.py`,
  `helpers.py`, `trash.py`, `main.py` o `schema.sql` — todos compartidos con el
  módulo Archivo o con el buscador. Cualquier cambio ahí hay que verificarlo también
  en `admin_archive.html`, que es lo que comprueba `test_admin_panels.py`.
- Esfuerzo: **S** ≤ media jornada · **M** 1–3 jornadas · **L** > 3 jornadas o
  requiere decisión institucional.
- Las auditorías hermanas ya cubren lo suyo y **no se repite aquí**: cuando algo
  depende de un pendiente ajeno se cita por su identificador — `OA-` (backoffice de
  Archivo), `BR-` (buscador RRHH), `BA-` (buscador Archivo).
- Nada de lo que sigue propone deshacer decisiones tomadas: AdminLTE sigue sin
  cargarse, las nueve pestañas siguen agrupadas por verbo y en ningún sitio se
  escribe «Tesauro».
- Total: **300 pendientes** (OR-001 … OR-300).

---

## Índice

| Sección | Rango | Nº |
|---|---|---|
| A. Fallos reales con escenario de fallo | OR-001 … OR-042 | 42 |
| B. Seguridad, autorización y datos personales | OR-043 … OR-060 | 18 |
| C. Pestaña Resumen | OR-061 … OR-080 | 20 |
| D. Pestaña Ingresar — alta y carga de archivos | OR-081 … OR-100 | 20 |
| E. Pestaña Ingresar — importación CSV | OR-101 … OR-116 | 16 |
| F. Pestaña Expedientes | OR-117 … OR-140 | 24 |
| G. Ficha del empleado, dossier e historial de cargos | OR-141 … OR-162 | 22 |
| H. Pestaña Tipos | OR-163 … OR-172 | 10 |
| I. Pestaña Papelera | OR-173 … OR-182 | 10 |
| J. Pestaña Retención | OR-183 … OR-190 | 8 |
| K. Pestaña Auditoría | OR-191 … OR-198 | 8 |
| L. Pestaña Acceso | OR-199 … OR-208 | 10 |
| M. Pestaña Exportar y reporte del expediente | OR-209 … OR-218 | 10 |
| N. Modales, foco y teclado | OR-219 … OR-226 | 8 |
| O. Estética y sistema visual | OR-227 … OR-240 | 14 |
| P. Responsive, modo oscuro y densidad | OR-241 … OR-246 | 6 |
| Q. Funcionalidad ausente frente a SuccessFactors, Workday, BambooHR y la normativa venezolana | OR-247 … OR-278 | 32 |
| R. Rendimiento, concurrencia y deuda técnica | OR-279 … OR-296 | 18 |
| S. Pruebas que faltan | OR-297 … OR-300 | 4 |

---

## A. Fallos reales con escenario de fallo

Ordenados por impacto. Todos tienen un camino concreto para reproducirlos.

### OR-001 · «Ver Expediente» del monitor RRHH nunca abre nada
`app/static/admin-monitor.js:154` · `app/routes/admin/docs.py:154` · `app/routes/hr.py:303`
**Hoy**: la tabla de Expedientes pinta el botón del ojo con
`openRrhhPersonDossier(${JSON.stringify(f.empleado)})`, y `f.empleado` viene de
`list_all`, que lo construye como `e.apellidos || ', ' || e.nombres` →
«Pérez González, Susana María». El endpoint que abre el dossier resuelve por
igualdad exacta contra `e.nombres || ' ' || e.apellidos` → «Susana María Pérez
González». Escenario: se pulsa el ojo en cualquier fila del panel de RRHH y
`POST /api/rrhh/person/profile` responde 404; `openRrhhPersonDossier` hace
`throw new Error()` sin mensaje, así que el usuario no ve ni el motivo. La acción
principal de la pestaña principal del panel no funciona en ninguna fila.
**Debe**: resolver el expediente por `empleado_id`, que la tabla ya tiene en
`f.empleado_id` — es la misma raíz que BR-004.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/hr.js`, `app/routes/hr.py`, `app/models.py` `[CHOCA]`, `app/tests/test_hr.py`.

### OR-002 · La importación CSV de empleados falla en todas las filas
`app/routes/admin/imports.py:108-117` · `app/schema.sql` (tabla `empleados`)
**Hoy**: `empleados.fecha_ingreso` es `DATE NOT NULL` y el `INSERT` del importador
no incluye esa columna, ni existe `DEFAULT`. Escenario: se sube el CSV de personal
que la propia barra documenta (`cedula,nombres,apellidos,cargo,departamento,estado`)
y cada fila lanza `NotNullViolation`, que el `except` de la línea 118 captura fila a
fila. La respuesta es HTTP 200 con `inserted: 0`, y la pantalla titula
«**Importación completada**». La carga masiva de personal no ha funcionado nunca.
**Debe**: aceptar `fecha_ingreso` como columna del CSV y, a falta de ella,
`CURRENT_DATE` explícito; documentarlo en el `title` de la barra.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`, `app/static/admin_hr.html`, `app/tests/test_sql_inserts.py`.

### OR-003 · La importación CSV de documentos de RRHH falla en todas las filas
`app/routes/admin/imports.py:206-215` · `app/schema.sql` (tabla `datos_rrhh`)
**Hoy**: `datos_rrhh.titulo` es `TEXT NOT NULL` y el `INSERT` de la rama RRHH no lo
incluye — la rama de Archivo sí. Mismo desenlace que OR-002: 200 OK, cero
insertados, mensaje de éxito. La segunda barra de importación tampoco ha funcionado
nunca.
**Debe**: componer el título como hace `admin_submit` (`docs.py:329`) o, mejor,
hacer `titulo` nullable y derivarlo del tipo al leer.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`, `app/tests/test_sql_inserts.py`.

### OR-004 · Un CSV de actualización borra nombres, RIF y fechas de quien ya existe
`app/routes/admin/imports.py:92-93`
**Hoy**: cuando la cédula ya existe, el `UPDATE` fija **siempre** `nombres`,
`apellidos`, `rif`, `fecha_jubilacion` y `fecha_pension`, vengan vacíos o no; sólo
el resto de columnas son condicionales. Escenario: RRHH corrige el departamento de
300 personas con un CSV de dos columnas (`cedula,departamento`) y las 300 fichas
quedan sin nombre, sin apellidos, sin RIF y sin fechas de jubilación. No hay forma
de deshacerlo: la importación no es transaccional (OA-022) y no hay copia previa.
**Debe**: toda columna ausente o vacía significa «no tocar», igual que las demás.
Y previsualización obligatoria antes de aplicar (OR-101).
**Esfuerzo**: S. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`, `app/tests/test_admin.py`.

### OR-005 · La zona de arrastre del alta de empleado está muerta
`app/static/admin-edit-hr.js:151-152` · `app/static/admin_hr.html:315` · `app/static/admin_hr.html:333-336`
**Hoy**: `admin-edit-hr.js` redefine `initDropZone()` y se carga **después** de
`admin-monitor.js` (líneas 928 y 930 del HTML), así que gana la versión vieja, que
busca `#pane-admin-rrhh-new [style*="dashed"]`. Ese selector no alcanza a
`.ds-dropzone-compact` —su borde punteado está en `styles.css`, no en línea—; lo
que encuentra es el bloque legacy de la línea 336, dentro de una columna con
`display:none`. Escenario: se arrastra un PDF sobre la zona visible y no pasa nada,
sin mensaje. Sólo funciona «Explorar».
**Debe**: borrar las dos funciones duplicadas de `admin-edit-hr.js` (`initDropZone`
y `exportAdminCSV`, copia literal de `admin-monitor.js`) y una guarda estática que
rechace redefinir una función global ya declarada en otro estático.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit-hr.js`, `app/tests/test_static_analysis.py`.

### OR-006 · Texto doble-codificado en el código, visible en pantalla
`app/static/admin-edit-hr.js:158`
**Hoy**: la etiqueta de archivo seleccionado se compone con `` `ðŸ“„ ${name}` ``:
los bytes UTF-8 de 📄 leídos como Latin-1 y recodificados. El archivo es UTF-8
válido, así que `test_static_assets.py` no lo detecta pese a declarar que caza
«texto doble-codificado». Ese mismo archivo es además el único estático con finales
de línea CRLF.
**Debe**: restaurar el carácter, normalizar a LF y ampliar la guarda para detectar
las secuencias `Ã`, `Â` y `ð` seguidas de otro carácter alto, que es la firma.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit-hr.js`, `app/tests/test_static_assets.py`.

### OR-007 · Cambiar el tipo de un documento de RRHH no cambia el tipo
`app/routes/admin/docs.py:418-419`
**Hoy**: en la rama RRHH de `update_documento`, `doc_type` sólo escribe
`tesauro_primario`; `id_tipo_documento` —la columna que leen el monitor, las
gráficas, la cobertura por Parte, la retención y el reporte— no se toca. En Archivo
se actualizan las dos (líneas 379-381). Escenario: un documento entró como
«Constancia de Trabajo» y es un «Título Universitario»; se corrige en el modal,
sale «guardado», y sigue contando en la Parte equivocada para siempre.
**Debe**: resolver el tipo con `_resolve_or_create_tipo_documento(..., cat_slug)` y
escribir también `id_tipo_documento`.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/tests/test_admin.py`.

### OR-008 · Los tipos nuevos de RRHH caen en la categoría de Archivo
`app/routes/admin/helpers.py:115-122` · `app/routes/admin/docs.py:285` · `app/routes/admin/imports.py:199`
**Hoy**: al ingresar un documento de RRHH con un tipo inexistente,
`_resolve_or_create_tipo_documento(nombre)` se llama **sin** `cat_slug` y el
fallback es `SELECT id FROM categoria ORDER BY id LIMIT 1`. Escenario: se escribe
«Constancia de Reposo»; el tipo se crea colgado de la primera categoría por id, que
no es ninguna Parte de RRHH. Desde ese momento el tipo aparece en el desplegable de
**Archivo**, no cuenta para la cobertura por Parte y no sale en Tipos de RRHH.
**Debe**: pasar siempre el scope; en RRHH, `parte-i` por defecto, y un aviso en
pantalla de que el tipo quedó pendiente de clasificar.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/helpers.py` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`, `app/routes/admin/imports.py` `[CHOCA]`.

### OR-009 · La papelera de RRHH enseña el tipo dos veces y nunca el empleado
`app/routes/trash.py:55-60` · `app/static/admin_hr.html:523` · `app/static/admin-edit.js:267-279`
**Hoy**: la consulta devuelve `titulo` = `td.nombre` y `doc_type` = `td.nombre` (el
mismo valor); el nombre del empleado va en `autor`, que el renderizador no pinta. La
cabecera es `# | Tipo | Empleado | Fecha | …` y las celdas salen en el orden
`titulo, doc_type, fecha, …`. Escenario: se borra el contrato de dos personas y la
papelera muestra dos filas idénticas —«Contrato» y «Contrato»— sin saber de quién
es cada una antes de restaurar.
**Debe**: devolver `empleado` como columna propia y pintar Tipo y Empleado donde
toca. `test_admin_panels.py` ya cuadra encabezados y celdas en el monitor: extender
esa guarda a la papelera.
**Esfuerzo**: S. **Archivos**: `app/routes/trash.py` `[CHOCA]`, `app/static/admin-edit.js` `[CHOCA]`, `app/tests/test_admin_panels.py`.

### OR-010 · Borrar un empleado deja sus documentos vivos e inalcanzables
`app/routes/admin/docs.py:629-639`
**Hoy**: `delete_empleado` marca `deleted_at` sólo en `empleados`; sus filas de
`datos_rrhh` siguen activas. Escenario: se manda a la papelera un expediente
duplicado; el KPI «Documentos» sigue contando sus doce documentos, la gráfica «Docs
por Tipo» también, la papelera de documentos no los lista y el monitor ya no ofrece
ninguna vía para llegar a ellos.
**Debe**: el borrado lógico del empleado arrastra sus documentos con una marca de
operación, en una sola transacción.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/routes/trash.py` `[CHOCA]`, `app/tests/test_admin.py`.

### OR-011 · Restaurar un empleado no restaura su expediente
`app/routes/trash.py:147-154`
**Hoy**: la restauración limpia `deleted_at` sólo de `empleados`. Hoy es coherente
por accidente (por OR-010), pero en cuanto se arregle OR-010 la restauración dejará
a la persona sin documentos. Y si se borraron documentos sueltos antes de borrar al
empleado, restaurarlo nunca los recupera.
**Debe**: restaurar en la misma transacción lo borrado por la misma operación,
identificado por marca de operación y no por fecha.
**Esfuerzo**: M. **Archivos**: `app/routes/trash.py` `[CHOCA]`.

### OR-012 · Purgar un empleado destruye documentos que no estaban en la papelera
`app/routes/trash.py:167-169`
**Hoy**: el purgado ejecuta `DELETE FROM datos_rrhh WHERE empleado_id=%s` sin
filtrar `deleted_at`. Escenario: alguien manda a la papelera un expediente por
error, otro lo purga «para limpiar», y con él se van sus 40 documentos vivos, sus
versiones (OA-007) y su historial de cargos, sin listado previo.
**Debe**: enumerar en el modal qué se va a destruir (N documentos, N versiones, N
movimientos), exigir escribir la cédula para confirmar, y volcar un JSON de
constancia antes del `DELETE`.
**Esfuerzo**: M. **Archivos**: `app/routes/trash.py` `[CHOCA]`, `app/static/admin-edit.js` `[CHOCA]`.

### OR-013 · Registrar un cargo nuevo no cambia el cargo del empleado
`app/routes/hr_alerts.py:205-214` · `app/routes/admin/docs.py:576-578`
**Hoy**: `empleados.cargo_id` e `historial_cargos` son dos verdades separadas y
nada las sincroniza. Escenario: se registra «Profesor Asociado desde 2026-03-01» en
el historial; el historial lo marca «Actual», pero el monitor, el dossier, el
reporte y la exportación siguen diciendo «Profesor Asistente». El propio modal
muestra los dos valores contradictorios en la misma pantalla.
**Debe**: el cargo vigente se **deriva** del tramo abierto del historial y
`empleados.cargo_id` se actualiza en la misma transacción; editar el campo «Cargo»
a mano abre un movimiento de historial en lugar de escribir la columna.
**Esfuerzo**: M. **Archivos**: `app/routes/hr_alerts.py`, `app/routes/admin/docs.py` `[CHOCA]`, `app/static/admin-edit-hr.js`.

### OR-014 · Borrar una entrada del historial deja al empleado sin cargo vigente
`app/routes/hr_alerts.py:226-241`
**Hoy**: el `DELETE` no reabre el tramo anterior. Escenario: se registra un ascenso
con fecha equivocada y se borra para rehacerlo; el tramo previo queda cerrado con la
fecha errónea y el empleado se queda sin ningún cargo marcado como «Actual», con un
hueco en la línea temporal.
**Debe**: al borrar el tramo abierto, reabrir el anterior en la misma transacción; y
ofrecer «editar» además de «borrar».
**Esfuerzo**: S. **Archivos**: `app/routes/hr_alerts.py`, `app/static/admin-edit-hr.js`.

### OR-015 · El alta de un empleado que ya existe descarta todo lo tecleado
`app/routes/admin/docs.py:290-317`
**Hoy**: si la cédula existe se salta el bloque de creación entero. Escenario:
alguien usa la pestaña Ingresar para cargar el contrato de una persona ya
registrada y de paso rellena cargo, departamento, estado, fecha de jubilación,
pensión y nivel educativo. Sale «Ingreso guardado con éxito», el formulario se
limpia (`admin-submit.js:328`), y ninguno de esos campos se ha escrito.
**Debe**: avisar antes de guardar («esta cédula ya existe: se añadirá el documento a
su expediente y los datos personales no se modificarán») con opción de abrir la
ficha, o actualizar explícitamente los campos no vacíos mostrando el diff.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/static/admin-submit.js` `[CHOCA]`.

### OR-016 · La cédula no se normaliza: la misma persona entra cinco veces
`app/routes/admin/docs.py:286-290` · `app/routes/admin/imports.py:69`
**Hoy**: la búsqueda es `WHERE cedula = %s` sobre el texto crudo. `V-12345678`,
`v-12345678`, `V12345678`, `12.345.678` y `12345678` son cinco personas distintas, y
como `cedula` es UNIQUE las cinco conviven. Escenario: el alta manual escribe
`V-12345678` y el CSV de nómina trae `12345678`; el expediente queda partido en dos
y ninguna pantalla lo señala.
**Debe**: normalizar en el borde (letra en mayúscula, sin puntos ni espacios,
patrón `V|E|J|G-########`), validar el formato venezolano e imponer un índice único
sobre la forma normalizada; además, detectar y fusionar los duplicados existentes.
**Esfuerzo**: M. **Archivos**: `app/models.py` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`, `app/routes/admin/imports.py` `[CHOCA]`, `app/main.py` `[CHOCA]`, `app/utils.py`.

### OR-017 · Dos empleados sin RIF chocan contra el índice único
`app/schema.sql` (tabla `empleados`) · `app/routes/admin/imports.py:113`
**Hoy**: `rif VARCHAR(20) UNIQUE`, y el importador pasa cadena vacía cuando la
columna falta, no `NULL`. Escenario: un CSV de 200 personas sin columna `rif`
inserta la primera y falla en las 199 restantes por violación de unicidad; lo que
llega a pantalla es el texto de psycopg2 recortado a 100 caracteres.
**Debe**: normalizar la cadena vacía a `NULL` en el importador y validar el formato
del RIF venezolano (`J|G|V|E-########-#`) en el borde.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`, `app/models.py` `[CHOCA]`.

### OR-018 · El KPI «Documentos» cuenta expedientes vacíos como documentos
`app/routes/admin/stats.py:23-47` · `app/routes/hr.py:84`
**Hoy**: `kpi-total-docs-rrhh` sale de `POST /api/admin/stats`, que cuenta filas del
DataFrame de `fetch_hr_dataframe()`. Ese SQL hace `LEFT JOIN datos_rrhh`, así que un
empleado sin documentos produce una fila igual, y no excluye `deleted_at`.
Escenario: 120 empleados, 80 sin documentos, 260 documentos reales → la tarjeta
marca 340 mientras la gráfica de al lado, alimentada por `totals.total_documents`,
marca 260. Dos cifras contradictorias en la misma pantalla.
**Debe**: una sola definición: el KPI lee `totals.total_documents` y `/stats` cuenta
`id_rrhh` no nulos con `deleted_at IS NULL`.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`, `app/static/admin-stats.js` `[CHOCA]`, `app/tests/test_stats_totales.py`.

### OR-019 · El filtro de fechas del Resumen no afecta a ninguna gráfica
`app/static/admin-stats.js:16-25` · `app/routes/admin/stats.py:68-69`
**Hoy**: la tarjeta se titula «Filtros Analíticos» y el botón «Actualizar Análisis»,
pero el rango sólo viaja a `/stats`, que devuelve dos números de los cuales uno
(`categories_count`) ni siquiera tiene destino en el HTML de RRHH.
`/charts?modulo=RRHH` no acepta fechas. Escenario: se acota a 2024 y las seis
gráficas y los ocho KPI siguen idénticos.
**Debe**: propagar el rango a `/charts` y aplicarlo a todas las series, o retirar el
filtro. Un control que no hace nada es peor que su ausencia.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`, `app/static/admin-charts.js` `[CHOCA]`, `app/static/admin-stats.js` `[CHOCA]`.

### OR-020 · «Sin especificar» sale tres veces en las gráficas de sexo y nivel
`app/routes/admin/stats.py:196-212`
**Hoy**: la etiqueta se calcula con `COALESCE(NULLIF(TRIM(...),''), 'Sin
especificar')` pero el `GROUP BY` es sobre la columna cruda: `NULL`, `''` y `'  '`
son tres grupos que emiten el mismo texto. Escenario: la dona de sexo muestra tres
sectores llamados «Sin especificar» con tres colores de la paleta y la leyenda
repite el texto tres veces; el lector cree que son categorías distintas.
**Debe**: agrupar por la misma expresión que produce la etiqueta.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`.

### OR-021 · El banner de jubilaciones dice «0 días» a quien ya se jubiló
`app/routes/hr_alerts.py:52-58` · `app/static/admin.js:80`
**Hoy**: `dias_restantes` vale 0 para toda fecha pasada y el banner imprime
`(${Number(a.dias_restantes)} días)`. Escenario: alguien cuya jubilación venció hace
tres semanas aparece como «Jubilación Vencida (no procesada) (0 días)», que se lee
como «hoy» y no genera urgencia.
**Debe**: días con signo en la API y dos textos distintos: «vence en N días» /
«vencida hace N días».
**Esfuerzo**: S. **Archivos**: `app/routes/hr_alerts.py`, `app/static/admin.js` `[CHOCA]`.

### OR-022 · Lo vencido hace más de un mes desaparece de la alerta
`app/routes/hr_alerts.py:63-73`
**Hoy**: el `WHERE` sólo admite jubilaciones desde `CURRENT_DATE - 30 días`, aunque
el `CASE` tenga una rama «Jubilación Vencida (no procesada)» sin límite; las
pensiones vencidas no entran en absoluto, porque su rama arranca en
`CURRENT_DATE`. Escenario: un caso sin procesar desde hace cuatro meses —el que más
falta hace perseguir— sale de la lista y nadie vuelve a verlo.
**Debe**: sin corte inferior para lo vencido y no procesado, orden por antigüedad
del vencimiento y contador propio separado del de «próximas».
**Esfuerzo**: S. **Archivos**: `app/routes/hr_alerts.py`, `app/tests/test_hr.py`.

### OR-023 · Las alertas de jubilación incluyen a quien está en la papelera
`app/routes/hr_alerts.py:59-73`
**Hoy**: la consulta no filtra `e.deleted_at IS NULL`, aunque todas las de
`stats.py` sí lo hagan. Escenario: se borra un expediente duplicado y su titular
sigue en el banner del Resumen mientras el KPI «Jubilación < 12 meses» —que sí
filtra (`stats.py:242`)— ya no lo cuenta: la tarjeta dice 3 y el banner lista 4.
**Debe**: `deleted_at IS NULL` en todas las consultas de `hr_alerts.py`.
**Esfuerzo**: S. **Archivos**: `app/routes/hr_alerts.py`, `app/tests/test_hr.py`.

### OR-024 · El KPI «Jubilación < 12 meses» se pierde a quien tiene las dos fechas
`app/routes/admin/stats.py:241-245`
**Hoy**: `COALESCE(e.fecha_jubilacion, e.fecha_pension)` — si hay jubilación, la
pensión no se mira. Escenario: alguien con jubilación en 2031 y pensión el mes que
viene no aparece en el KPI, y sí en el banner, que evalúa las dos por separado.
**Debe**: `LEAST` de las fechas no nulas, con el mismo criterio en los dos sitios.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`, `app/routes/hr_alerts.py`.

### OR-025 · «Mov. de cargo» cuenta el historial de gente borrada
`app/routes/admin/stats.py:227-228`
**Hoy**: `SELECT COUNT(*) FROM historial_cargos` sin unir con `empleados` ni filtrar
`deleted_at`, mientras el resto del bloque `totals` sí filtra. Escenario: se purga a
un empleado y la cifra cae de golpe; se manda otro a la papelera y no se mueve. El
KPI no mide nada estable.
**Debe**: unir con `empleados` y filtrar `deleted_at IS NULL`.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`.

### OR-026 · La etiqueta del CSV elegido nunca cambia
`app/static/admin-charts.js:435-440` · `app/static/admin_hr.html:277-279`
**Hoy**: el listener que escribe el nombre del archivo escucha `.custom-file-input`,
clase de Bootstrap ausente del marcado; las barras usan `.ds-import-file-input` y
los `<span id="csv-label-…">` no los toca nadie. Escenario: se elige un CSV, la
etiqueta sigue diciendo «Elegir CSV…», el usuario duda de si el archivo entró y
pulsa Importar a ciegas.
**Debe**: escuchar la clase real y escribir el nombre —con `title` completo si no
cabe— en el `<span>` correspondiente.
**Esfuerzo**: S. **Archivos**: `app/static/admin-charts.js` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-027 · La ayuda de columnas del CSV enseña las etiquetas HTML en crudo
`app/static/admin_hr.html:283-286` · `app/static/admin_hr.html:299-302`
**Hoy**: los dos iconos «?» llevan `data-toggle="tooltip" data-html="true"` con un
`title` lleno de `<b>` y `<br>`, pero **nadie inicializa los tooltips de Bootstrap**
en todo el proyecto: no hay una sola llamada a `.tooltip()`. Escenario: se pasa el
ratón y el navegador enseña el `title` nativo con `<b>Columnas:</b>` literal, en una
línea. Además `tabindex="-1"` deja esa ayuda fuera del alcance del teclado.
**Debe**: o inicializar los tooltips en las páginas de administración, o pasar la
ayuda al `data-tip` propio de `admin-ui.js:313`, que sí funciona.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin_archive.html` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`.

### OR-028 · El formulario promete 200 MB y el sistema acepta 25
`app/static/admin_hr.html:322` · `app/static/admin_hr.html:345` · `app/static/admin_hr.html:719`
**Hoy**: la zona de carga dice «PDF, DOC, ZIP, PNG — máx 200 MB», el bloque legacy
repite «Peso Máximo: 200MB», el modal de edición dice «PDF · PNG · JPG · TIFF ·
WEBP — máx. 25 MB» y `docs/funcionalidades.md` §4 dice 25 MB con otra lista de
extensiones. Escenario: se arrastra un PDF de 60 MB, se espera toda la subida y el
servidor la rechaza al final, sin haber avisado.
**Debe**: un solo límite y una sola lista de extensiones, declarados en el servidor y
leídos por la interfaz; validación en el cliente antes de empezar a subir.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-submit.js` `[CHOCA]`, `app/routes/admin/`, `docs/funcionalidades.md`.

### OR-029 · «Últimos Ingresos» se carga en cada visita y es invisible
`app/static/admin.js:16` · `app/static/admin-submit.js:197` · `app/static/admin_hr.html:333`
**Hoy**: `loadRecentSubmissions()` pide `/list_all?per_page=5` cada vez que se entra
en Ingresar y pinta en `#recent_submissions-rrhh`, que vive dentro de la columna con
`display:none`. La lista existe, se paga su petición y nadie la ve.
**Debe**: sacarla del bloque muerto y colocarla como columna lateral real del
formulario, o retirar la llamada. El bloque legacy entero (líneas 333-358) sobra.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-submit.js` `[CHOCA]`.

### OR-030 · El estado «Fallecido» no se puede elegir ni pintar
`app/static/admin_hr.html:818` · `app/static/admin-edit-hr.js:18-22` · `app/static/app-core.js:131-138`
**Hoy**: el `<select>` del modal ofrece «Fallecido», pero `openEditEmpleadoModal`
reemplaza todas las opciones por `state.choices.rrhh.estados_catalog`, que se
alimenta de los estados ya existentes. Y `getStatusColor()` no tiene rama para
«Fallecido»: si llegara a existir, el badge sale gris. El filtro de estado del
monitor (`admin_hr.html:378-384`) tampoco lo lista.
**Debe**: un catálogo cerrado y explícito de estados laborales —con «Fallecido» y
«Permiso no remunerado»— con su color, su badge y su opción de filtro en las tres
pantallas.
**Esfuerzo**: S. **Archivos**: `app/static/app-core.js` `[CHOCA]`, `app/static/admin_hr.html`, `app/routes/lookups.py`, `app/main.py` `[CHOCA]`.

### OR-031 · Se puede editar a un empleado que está en la papelera
`app/routes/admin/docs.py:531-554` · `app/routes/admin/docs.py:557-605`
**Hoy**: ni `get_empleado` ni `update_empleado` filtran `deleted_at`. Escenario: una
pestaña abierta antes del borrado, o el botón «atrás», permiten abrir la ficha de
alguien que está en la papelera y guardar cambios; la auditoría registra la
modificación de un expediente borrado y nadie la ve.
**Debe**: 404, o 409 con «está en la papelera», en los dos endpoints — mismo
criterio que OA-005 pide para documentos.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`.

### OR-032 · Se puede crear un documento colgado de un empleado en la papelera
`app/routes/admin/docs.py:290`
**Hoy**: la búsqueda por cédula del alta no filtra `deleted_at`, así que el
documento se ata a un expediente borrado y desaparece con él. Escenario: se archiva
una constancia de alguien cuyo expediente estaba en la papelera; el sistema dice
«guardado con éxito» y el documento no aparece en ninguna pantalla.
**Debe**: detectar el caso y ofrecer restaurar el expediente antes de continuar.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`.

### OR-033 · La auditoría se escribe antes de saber si la operación funcionó
`app/routes/admin/docs.py:247` · `app/routes/admin/catalog.py:84`
**Hoy**: `admin_submit` llama a `log_event(...)` en su primera línea, antes de
resolver el tipo, crear al empleado e insertar el documento; `add_category` hace lo
mismo. Escenario: el alta falla por cédula vacía y la auditoría deja un «Create
Document» exitoso de algo que no existe. La pestaña Auditoría es entonces un
registro de intenciones, no de hechos.
**Debe**: registrar después del commit, con el id resultante, y registrar también
los fallos con `status` distinto.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/routes/admin/catalog.py` `[CHOCA]`.

### OR-034 · El alta de empleado no es transaccional
`app/routes/admin/docs.py:295-338`
**Hoy**: cargo, departamento, estado, empleado y documento son cinco `commit`
independientes. Escenario: la cédula pasa pero el `INSERT` del documento falla
(ubicación vacía, tipo inválido, caída de red con Neon): queda un empleado creado,
sin documentos, con un cargo y un departamento nuevos en el catálogo. La pantalla
dice «Error de conexión» y el reintento crea el documento sobre el empleado a medias.
**Debe**: una transacción por operación de negocio, con el mismo helper que pide
OA-209 para la importación.
**Esfuerzo**: M. **Archivos**: `app/database.py` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-035 · El error real del alta nunca llega al usuario
`app/static/admin-submit.js:334-335`
**Hoy**: `catch { showToast("Error de conexión al registrar el folio.", "error"); }`
descarta la excepción sin mirarla. Escenario: el servidor responde 400 «Cédula es
requerida para RRHH» o 500 por unicidad del RIF, y el usuario lee «error de
conexión», revisa su wifi y reintenta con los mismos datos.
**Debe**: mostrar `e.message` y marcar el campo culpable cuando el error lo
identifica.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`.

### OR-036 · Dos identificadores distintos para el mismo usuario en el mismo archivo
`app/static/admin.js:102-103` frente a `app/static/admin.js:211`
**Hoy**: `handleModuleExport` usa `state.user?.usuario` y `_saveRetentionPlazo` usa
`state.user?.username`; la sesión guarda `username`. Escenario: la exportación del
módulo RRHH viaja siempre con `requester=` vacío y `X-User` vacío, así que la copia
de un fichero de datos personales queda registrada como hecha por nadie.
**Debe**: un solo nombre, y una guarda estática que rechace `state.user.usuario`.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/tests/test_static_analysis.py`.

### OR-037 · Borrar un movimiento de cargo se registra siempre a nombre de «sistema»
`app/static/admin-edit-hr.js:273` · `app/routes/hr_alerts.py:227`
**Hoy**: `_adminDeleteCargo` no manda `requester`, y el endpoint lo declara con
`default=""`, así que `log_event(requester or "sistema", ...)` escribe siempre
«sistema». Escenario: alguien borra tres tramos del historial laboral de un
profesor y la auditoría no puede decir quién fue.
**Debe**: tomar el actor de la sesión en el servidor (OA-036) y, mientras tanto,
enviarlo desde el cliente como hacen las demás llamadas.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit-hr.js`, `app/routes/hr_alerts.py`.

### OR-038 · El cierre automático del cargo anterior solapa un día
`app/routes/hr_alerts.py:199-203`
**Hoy**: al registrar un cargo se pone `fecha_fin = fecha_inicio` del nuevo.
Escenario: el tramo anterior termina el 01/03 y el nuevo empieza el 01/03; ese día
la persona figura en dos cargos, y cualquier cálculo de antigüedad por tramos lo
cuenta dos veces. Tampoco se cierran tramos con `fecha_inicio` posterior, así que
insertar un movimiento antiguo deja dos tramos abiertos a la vez.
**Debe**: `fecha_fin = fecha_inicio - 1 día`, restricción de exclusión en la tabla
para impedir solapes, y validación del orden cronológico al insertar.
**Esfuerzo**: M. **Archivos**: `app/routes/hr_alerts.py`, `app/main.py` `[CHOCA]`, `app/tests/test_hr.py`.

### OR-039 · El catálogo de cargos se llena de duplicados por capitalización
`app/routes/hr_alerts.py:186-195` · `app/routes/admin/helpers.py:86-99`
**Hoy**: `hr_alerts` busca con `LOWER(nombre)=LOWER(%s)` pero inserta el texto tal
cual; `_resolve_or_create_lookup` —que usan el alta, la edición y la importación—
busca con igualdad **exacta**. No hay índice único sobre el nombre. Escenario: el
historial crea «profesor asociado», la ficha «Profesor Asociado» y el CSV «PROFESOR
ASOCIADO»: tres filas, tres barras en cualquier informe de planta y tres entradas en
el autocompletado del datalist.
**Debe**: un solo helper con normalización (trim, colapso de espacios, comparación
insensible), índice único sobre la forma normalizada y `ON CONFLICT`.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/helpers.py` `[CHOCA]`, `app/routes/hr_alerts.py`, `app/main.py` `[CHOCA]`.

### OR-040 · Crear un tipo que ya existe en Archivo dice «guardado» y no guarda
`app/routes/admin/catalog.py:102-104` · `app/static/admin-categories.js:176`
**Hoy**: la comprobación de existencia es global por nombre, sin mirar la categoría.
Escenario: en Tipos de RRHH se crea «Constancia», que ya existe bajo `archivo`; el
endpoint devuelve `{"success": true, "detail": "Ya existe"}`, la pantalla enseña
«¡Nueva tipología guardada con éxito!» en verde y la lista de la Parte elegida sigue
igual. El usuario lo intenta tres veces antes de rendirse.
**Debe**: unicidad por (nombre, categoría) y un mensaje honesto cuando el nombre ya
está tomado en otro módulo, con enlace al tipo existente.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/catalog.py` `[CHOCA]`, `app/static/admin-categories.js` `[CHOCA]`.

### OR-041 · La descripción de la tipología se escribe y se tira
`app/static/admin_hr.html:435-438` · `app/static/admin-categories.js:174` · `app/routes/admin/catalog.py:107-111`
**Hoy**: el formulario tiene un `<textarea>` «Descripción», el cliente lo manda como
`desc` y el `INSERT` de `tipo_documento` no lo guarda en ninguna columna. Escenario:
se documenta con cuidado qué va en «Constancia de Reposo», se guarda, y la
descripción no existe en ningún sitio ni vuelve a mostrarse.
**Debe**: columna `descripcion`, visible en la lista de Tipos y como ayuda del
desplegable del alta — o retirar el campo.
**Esfuerzo**: S. **Archivos**: `app/main.py` `[CHOCA]`, `app/routes/admin/catalog.py` `[CHOCA]`, `app/models.py` `[CHOCA]`, `app/static/admin-categories.js` `[CHOCA]`.

### OR-042 · La pestaña Tipos pide los plazos y los pinta en otra pestaña
`app/static/admin.js:18` · `app/static/admin.js:165`
**Hoy**: `loadAdminTab("categories")` llama a `loadRetentionConfig()`, que escribe en
`#retencion-tipos-body-rrhh`, un `<tbody>` del panel de Retención. Escenario: cada
entrada en Tipos dispara una petición cuyo resultado nadie ve; y si alguien había
editado un plazo en Retención sin guardar, pasar por Tipos se lo sobrescribe por
debajo sin avisar.
**Debe**: cada pestaña carga sólo lo suyo; si Tipos debe mostrar el plazo, que lo
muestre en su propia lista.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`.

---
## B. Seguridad, autorización y datos personales

Un expediente de personal es un fichero de datos personales: cédula, RIF, fecha de
nacimiento, sexo, nivel educativo, fotografía, historial laboral y estado de salud
implícito en los reposos. El backoffice es donde se escribe, y hoy lo que decide
quién puede escribir es el marcado, no el servidor.

### OR-043 · Ningún endpoint de `/api/admin` comprueba el rol ni el módulo
`app/routes/admin/__init__.py:12-16` · `app/routes/admin/deps.py:7-24`
**Hoy**: el router exige `require_session`, que sólo valida que el token sea legible
y devuelve un nombre. Es exactamente OA-035, pero en RRHH el daño es distinto: un
usuario Normal del módulo Archivo, con su propia sesión válida, puede hacer
`curl -X PUT /api/admin/empleado/12` y cambiarle el estado laboral a un profesor, o
`DELETE /api/admin/empleado/12` para mandarlo a la papelera. El control vive sólo en
`configureSidebarVisibilities()` (`app.js:143-145`), que es JavaScript en el
navegador de quien ataca.
**Debe**: una dependencia `require_role(modulo, rol)` sobre cada endpoint, resuelta
contra `usuarios_sistema`, y una prueba que recorra las rutas y falle si alguna
carece de ella. Depende de OA-035; aquí sólo se documenta el impacto específico.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/deps.py` `[CHOCA]`, `app/routes/admin/*.py` `[CHOCA]`, `app/routes/hr_alerts.py`, `app/tests/test_admin.py`.

### OR-044 · Quien actúa lo declara el cliente en todo el módulo RRHH
`app/static/admin-edit-hr.js:62` · `app/static/admin-edit-hr.js:87` · `app/static/admin-edit-hr.js:259`
**Hoy**: `usuario`, `requester` y `registrado_por` viajan en el cuerpo o la query
desde el navegador. Escenario: alguien borra el historial de cargos de un profesor y
envía `registrado_por=susana`; la pestaña Auditoría dirá que fue Susana. Es OA-036,
y en RRHH afecta a operaciones sobre datos personales, que es donde la trazabilidad
es una obligación legal, no una comodidad.
**Debe**: el actor se toma de la sesión en el servidor y los campos del cliente se
ignoran.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/routes/hr_alerts.py`, `app/routes/trash.py` `[CHOCA]`, `app/static/admin-edit-hr.js`.

### OR-045 · Un admin de RRHH puede resetear la clave del admin Global
`app/routes/admin/users.py:153-181` · `app/routes/admin/users.py:208-218`
**Hoy**: `GET /users?modulo=RRHH` devuelve los usuarios de RRHH **más los
`Global`**, y la tabla de la pestaña Acceso pinta para cada uno los botones de
cambiar contraseña, desactivar y eliminar. Escenario: el admin de RRHH cambia la
contraseña del admin Global desde su propio panel y hereda el control del sistema
entero, incluidas las copias de seguridad y el módulo Archivo.
**Debe**: los usuarios `Global` se listan como informativos y sin acciones; sólo otro
Global puede tocarlos, comprobado en el servidor.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/users.py` `[CHOCA]`, `app/static/admin-users.js` `[CHOCA]`.

### OR-046 · Un admin puede borrarse o desactivarse a sí mismo
`app/routes/admin/users.py:221-243` · `app/static/admin-users.js:56-82`
**Hoy**: nada impide `DELETE /users/{mi_id}` ni desactivar la propia cuenta, y
tampoco hay comprobación de que quede al menos un administrador activo por módulo.
Escenario: el único admin de RRHH se desactiva probando el botón y nadie puede
volver a entrar al panel; no hay procedimiento de recuperación documentado.
**Debe**: rechazar la operación sobre uno mismo, y rechazarla también si dejara el
módulo sin ningún administrador activo.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/users.py` `[CHOCA]`, `app/tests/test_admin.py`.

### OR-047 · La nueva contraseña se teclea a la vista de todos
`app/static/admin-users.js:85-87` · `app/static/admin-ui.js:43`
**Hoy**: `handleChangePassword` usa `promptModal`, cuyo `<input>` es
`type="text"`. Escenario: se resetea la clave de un compañero en una oficina
compartida y la contraseña queda en pantalla y en el historial visual de quien pase
por detrás.
**Debe**: `promptModal` acepta un tipo de campo; la contraseña se pide en
`type="password"`, con confirmación y con generación de una clave temporal de un
solo uso como opción por defecto.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/static/admin-users.js` `[CHOCA]`.

### OR-048 · La contraseña reseteada no obliga a cambiarla en el primer acceso
`app/routes/admin/users.py:208-218`
**Hoy**: `change_password` escribe el hash y no marca nada. Escenario: el admin fija
«ucv2026» a diez personas, se lo dicta por WhatsApp y esas diez cuentas siguen con
esa clave un año después.
**Debe**: marca `must_change_password` que el login respeta, y caducidad de la clave
temporal.
**Esfuerzo**: M. **Archivos**: `app/main.py` `[CHOCA]`, `app/routes/admin/users.py` `[CHOCA]`, `app/routes/auth.py`, `app/static/login.html`.

### OR-049 · La política de contraseñas es «seis caracteres»
`app/routes/admin/users.py:210` · `app/static/admin-users.js:88`
**Hoy**: el único requisito es longitud ≥ 6, comprobado en el cliente y en el
servidor. Para un sistema que custodia el fichero de personal de una facultad, con
`README.md` documentando todavía «1234» como contraseña de las cuentas de prueba, es
insuficiente.
**Debe**: mínimo 12 caracteres, rechazo de las contraseñas más comunes y de las que
contengan el nombre de usuario, y un medidor de fuerza en el formulario.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/users.py` `[CHOCA]`, `app/static/admin-users.js` `[CHOCA]`, `README.md`.

### OR-050 · La creación de usuario no valida nada más que «no vacío»
`app/routes/admin/users.py:184-205` · `app/static/admin-users.js:148-168`
**Hoy**: se acepta cualquier nombre de usuario (espacios, mayúsculas, acentos,
cadena de 300 caracteres), sin correo, sin nombre real y sin comprobar que `modulo` y
`rol` estén entre los valores válidos más allá de lo que ofrezca el `<select>`.
Escenario: se crea `Susana ` con un espacio final; `TRIM(usuario)` en la búsqueda de
existencia lo normaliza pero el `INSERT` guarda el original, y luego el login no lo
encuentra.
**Debe**: validación del identificador en `models.py`, `modulo`/`rol` como enumerados
en el servidor, y campos de nombre real y correo para poder avisar a la persona.
**Esfuerzo**: S. **Archivos**: `app/models.py` `[CHOCA]`, `app/routes/admin/users.py` `[CHOCA]`.

### OR-051 · Desde el panel de RRHH sólo se pueden crear usuarios de RRHH
`app/static/admin-ui.js:406-408`
**Hoy**: el `<select>` de módulo del panel inyectado tiene una sola opción, la del
panel. Es razonable para un admin de módulo, pero el admin Global —que entra por la
misma página— no puede crear un usuario de Archivo ni un Global desde aquí, y no hay
ninguna indicación de dónde hacerlo.
**Debe**: el desplegable se compone según el rol de quien mira: un Global ve los tres
valores; un admin de módulo, sólo el suyo. Comprobado también en el servidor.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/routes/admin/users.py` `[CHOCA]`.

### OR-052 · La consulta de un expediente no deja rastro
`app/routes/hr.py:300` · `app/routes/admin/docs.py:531`
**Hoy**: sólo se auditan las escrituras. Abrir el dossier de una persona, ver su
ficha o generar su reporte no queda registrado en ningún sitio. Escenario: se filtra
la fecha de nacimiento y el estado laboral de un profesor y no hay forma de saber
quién consultó ese expediente. La compartición externa de Archivo sí se audita
(`CLAUDE.md`, sección `share.py`) precisamente por esta razón.
**Debe**: registrar la lectura de un expediente concreto —quién, cuándo, qué
empleado— y ofrecer al titular ese registro (LOTTT art. 147, derecho al expediente).
**Esfuerzo**: M. **Archivos**: `app/routes/hr.py`, `app/routes/admin/docs.py` `[CHOCA]`, `app/routes/admin/catalog.py` `[CHOCA]`.

### OR-053 · El panel no distingue datos sensibles de datos ordinarios
`app/static/admin_hr.html:822-864`
**Hoy**: fecha de nacimiento, sexo, nivel educativo, RIF y cédula se editan en la
misma cuadrícula, con el mismo aspecto, que el departamento. No hay marca de
sensibilidad, ni control por campo, ni enmascarado por defecto.
**Debe**: clasificar los campos (identificativo · sensible · laboral), enmascarar los
sensibles hasta que se pulse «mostrar», registrar ese gesto, y permitir que un rol de
sólo-consulta no los vea.
**Esfuerzo**: L. **Archivos**: `app/static/admin_hr.html`, `app/static/styles.css` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-054 · La foto del empleado se acepta como URL externa arbitraria
`app/static/admin_hr.html:867` · `app/static/admin-submit.js:167` · `app/routes/admin/docs.py:567-568`
**Hoy**: `foto_url` es texto libre que se guarda sin validar y se pinta como `src`.
Escenario: alguien pega `https://tracker.ejemplo/x.png`; cada vez que se abre la
ficha o el dossier, el navegador de quien mira hace una petición a ese dominio,
enviando el `Referer` del panel interno. También admite `javascript:` y `data:`, que
según dónde se pinte puede ejecutarse.
**Debe**: subir la foto al almacenamiento propio como cualquier otro adjunto,
rechazar esquemas distintos de `https:` y `/api/files/`, y una CSP que restrinja
`img-src`.
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/routes/admin/docs.py` `[CHOCA]`, `app/storage.py`, `app/main.py` `[CHOCA]`.

### OR-055 · No hay página para subir la foto: sólo se puede pegar una URL
`app/static/admin_hr.html:865-868` · `app/static/admin-submit.js:166-168`
**Hoy**: tanto el alta como la edición piden «URL Foto Avatar». No existe ningún
control para elegir un archivo, recortar y subir, pese a que el sistema ya tiene R2
configurado y una ruta de subida.
**Debe**: control de subida con recorte cuadrado, previsualización y borrado, con la
foto anterior conservada como versión.
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-edit-hr.js`, `app/routes/admin/docs.py` `[CHOCA]`, `app/storage.py`.

### OR-056 · El nombre del tipo de documento se pinta sin escapar en Tipos
`app/static/admin-categories.js:35` · `app/static/admin-categories.js:46`
**Hoy**: la rama RRHH de `loadCategoriesTab()` interpola `${t}` en `innerHTML` sin
`escHtml`, a diferencia de casi todo el resto del panel. Escenario: un admin crea la
tipología `Reposo <img src=x onerror="fetch('/api/admin/backup/export')">`; cada vez
que alguien abre la pestaña Tipos, el marcado se ejecuta con su sesión. Es XSS
almacenado de segundo orden, hermano de BR-003 pero dentro del backoffice.
**Debe**: `escHtml()` en las dos ramas, y una guarda estática que rechace
interpolaciones sin escapar dentro de plantillas asignadas a `innerHTML`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-categories.js` `[CHOCA]`, `app/tests/test_static_analysis.py`.

### OR-057 · El reporte imprimible incluye documentos que están en la papelera
`app/routes/hr.py:465-474`
**Hoy**: la consulta de documentos del reporte no filtra `dr.deleted_at IS NULL`, ni
comprueba que el empleado no esté borrado. Escenario: se elimina un documento
cargado por error, se imprime el expediente para entregarlo al Consejo de Facultad y
el documento borrado sale en el papel, con su fecha y su ubicación física.
**Debe**: filtrar borrados en el reporte, y marcar en él la fecha y el usuario que lo
generó.
**Esfuerzo**: S. **Archivos**: `app/routes/hr.py`, `app/tests/test_hr.py`.

### OR-058 · No hay separación entre «quien archiva» y «quien aprueba»
`app/routes/admin/docs.py:454-476`
**Hoy**: la misma persona que crea un documento puede aprobarlo, y el estado se
cambia con un `PATCH` sin comprobar quién lo creó. Escenario: un asistente carga su
propia constancia de estudios y la marca «Aprobado» sin que nadie la revise.
**Debe**: el aprobador no puede ser el creador; el cambio a «aprobado» exige rol
Admin y queda en auditoría con el par (creador, aprobador).
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/tests/test_admin.py`.

### OR-059 · No hay límite de intentos ni bloqueo tras varios fallos
`app/routes/auth.py` · `app/routes/admin/users.py`
**Hoy**: `is_active` se cambia sólo a mano; no hay contador de intentos fallidos, ni
bloqueo temporal, ni aviso al admin. La pestaña Acceso muestra «Último Acceso» pero
no «último intento fallido».
**Debe**: contador de fallos con bloqueo progresivo, visible en la tabla de Acceso, y
un evento de auditoría por bloqueo.
**Esfuerzo**: M. **Archivos**: `app/routes/auth.py`, `app/main.py` `[CHOCA]`, `app/static/admin-users.js` `[CHOCA]`.

### OR-060 · La sesión se «extiende» sola desde el navegador
`app/static/admin-ui.js:229-240`
**Hoy**: `extendSession()` reescribe la marca de tiempo en `localStorage` y nada
más; no habla con el servidor. Escenario: el aviso de caducidad aparece, se pulsa
«Extender sesión», el banner desaparece y a los diez minutos la siguiente petición
responde 401 y se pierde el formulario a medio rellenar. La UI promete algo que no
puede cumplir.
**Debe**: renovar contra `/api/auth/restore` y sólo entonces borrar el aviso; si el
servidor no renueva, decirlo.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/routes/auth.py`.

---

## C. Pestaña Resumen

### OR-061 · Ocho KPI sin jerarquía visual: todos pesan lo mismo
`app/static/admin_hr.html:52-127`
**Hoy**: las ocho tarjetas son idénticas en tamaño, tipografía y peso; sólo cambia
el color del borde izquierdo. «Empleados» (un dato de contexto) ocupa exactamente lo
mismo que «Jubilación < 12 meses» (una acción pendiente). Con la rejilla
`auto-fit minmax(132px)` el orden de lectura además cambia con el ancho.
**Debe**: dos niveles — un bloque de dos o tres cifras accionables, grandes y
primeras, y el resto como fila secundaria compacta.
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/styles.css` `[CHOCA]`.

### OR-062 · Ninguna tarjeta de KPI es pulsable
`app/static/admin_hr.html:52-127`
**Hoy**: «Sin documentos: 34» es la mejor lista de trabajo que produce el sistema, y
no lleva a ninguna parte: hay que ir a Expedientes y descubrir que no existe filtro
por «sin documentos» (OR-124). Lo mismo con «Jubilación < 12 meses» y con «Mov. de
cargo».
**Debe**: cada KPI accionable enlaza al listado filtrado que lo produce, como ya hacen
los badges de estado del monitor de Archivo (`admin-monitor.js:67-71`).
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-charts.js` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-063 · «Último ingreso» no dice de qué
`app/static/admin_hr.html:118-126` · `app/routes/admin/stats.py:229-231`
**Hoy**: la tarjeta muestra una fecha suelta con el icono de un reloj. El dato es el
`MAX(created_at)` de `datos_rrhh`, es decir la última vez que se archivó un
documento — no el último empleado incorporado, que es lo que «último ingreso»
sugiere en un módulo de RRHH, donde «ingreso» significa incorporación laboral.
**Debe**: renombrar a «Último documento archivado» y añadir de quién y de qué tipo;
o cambiar el dato al último `fecha_ingreso` de la plantilla, que es lo que la palabra
promete.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/routes/admin/stats.py` `[CHOCA]`.

### OR-064 · Las cifras no dicen respecto a cuándo
`app/static/admin_hr.html:52-127`
**Hoy**: ninguna tarjeta lleva variación ni referencia temporal. «Empleados: 412» no
dice si el mes pasado eran 400 o 430; «Documentos: 3.120» no dice cuántos se
archivaron esta semana, que es la única medida de si el trabajo avanza.
**Debe**: variación frente al periodo anterior con signo y una micro-serie de doce
meses en las tarjetas donde la tendencia tenga sentido.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`, `app/static/admin-charts.js` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-065 · El color de borde de cada KPI se elige a mano y sin significado
`app/static/admin_hr.html:53` · `:62` · `:71` · `:80` · `:90` · `:100` · `:109` · `:118`
**Hoy**: cada tarjeta lleva `style="border-left:4px solid var(--viz-N)"` con N
elegido a ojo (1, 3, 7, 4, 8, 2, 5 e `ink-muted`). Los tokens `--viz-*` son la escala
de **series de datos**, cuyo orden es el mecanismo de seguridad para daltonismo
(`CLAUDE.md`); usarlos como decoración de tarjetas rompe esa convención y además el
color no codifica nada: dos tarjetas de alerta y una informativa comparten familia.
**Debe**: un token semántico por estado (neutro · aviso · alerta) y el borde
izquierdo reservado a las tarjetas que están en alerta.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/styles.css` `[CHOCA]`.

### OR-066 · Sólo dos de las ocho tarjetas tienen texto de apoyo
`app/static/admin_hr.html:85` · `:95`
**Hoy**: «Jubilación < 12 meses» y «Sin documentos» llevan `.ds-kpi-sub`; las otras
seis no, así que la rejilla queda con dos tarjetas más altas y seis más bajas, y las
seis sin subtítulo no explican su unidad ni su origen.
**Debe**: subtítulo en todas —aunque sea la unidad o el periodo— para que la altura
sea uniforme y cada cifra se explique sola.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`.

### OR-067 · El banner de alertas no dice cuál es su horizonte y contradice al KPI
`app/static/admin.js:76` · `app/static/admin_hr.html:83`
**Hoy**: el banner consulta `horizonte_dias=90` y el KPI de al lado usa 365 días
(`stats.py:244`) con la etiqueta «Jubilación < 12 meses». Escenario: el KPI dice 7 y
el banner dice «3 empleados con jubilación/pensión próxima (próximos 90 días)»; nada
explica la diferencia y el lector concluye que uno de los dos está mal.
**Debe**: un solo horizonte configurable, mostrado en ambos sitios, o dos etiquetas
que dejen clara la relación (7 en el año, 3 en el trimestre).
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/routes/admin/stats.py` `[CHOCA]`.

### OR-068 · El banner de alertas se cierra y vuelve en cada visita
`app/static/admin.js:82-91`
**Hoy**: el `alert-dismissible` se cierra con la X y reaparece intacto al volver a
Resumen, porque el pane se repinta entero. No hay «posponer», ni «marcar como
gestionado», ni memoria de qué alertas ya se atendieron.
**Debe**: cada alerta se puede posponer con fecha o cerrar con motivo, y ese estado
persiste por empleado y por alerta.
**Esfuerzo**: M. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/routes/hr_alerts.py`, `app/main.py` `[CHOCA]`.

### OR-069 · Si las alertas fallan, el banner desaparece en silencio
`app/static/admin.js:91`
**Hoy**: `catch {}` vacío. Escenario: el endpoint responde 500 por la consulta de
`hr_alerts` y el Resumen se ve exactamente igual que un día sin jubilaciones
próximas. La ausencia de alertas y el fallo del sistema de alertas son
indistinguibles.
**Debe**: estado de error explícito en el banner, con reintento.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`.

### OR-070 · El banner sólo muestra tres nombres y no lleva a la lista completa
`app/static/admin.js:79-87`
**Hoy**: `slice(0,3)` y «…y N más», sin enlace. En Archivo el texto remite a la
pestaña Retención; en RRHH no hay ninguna pantalla que liste las jubilaciones
próximas, así que el «y N más» es un callejón sin salida.
**Debe**: una vista de alertas —o al menos un filtro en Expedientes— a la que
enlace el banner.
**Esfuerzo**: M. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-071 · «Cobertura de Expedientes» no dice qué falta ni a quién
`app/static/admin_hr.html:238-246` · `app/static/admin-charts.js:269-302`
**Hoy**: cuatro barras con el porcentaje de plantilla que tiene al menos un
documento en cada Parte. Es la mejor gráfica del panel y es completamente pasiva: no
se puede pulsar una barra para ver los 87 empleados que no tienen nada en la Parte
II.
**Debe**: barra pulsable que abra Expedientes filtrado por «sin documentos en la
Parte N», y exportable como lista de trabajo.
**Esfuerzo**: M. **Archivos**: `app/static/admin-charts.js` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-072 · La cobertura mide «al menos un documento», no completitud
`app/routes/admin/stats.py:177-194`
**Hoy**: basta un documento en la Parte I para contar como cubierto, aunque la Parte
I exija cinco documentos distintos (partida de nacimiento, título, contrato,
declaración jurada, planilla de ingreso). Escenario: la barra marca 96 % de
cobertura en la Parte I y ningún expediente está realmente completo.
**Debe**: una lista de documentos obligatorios por Parte y por tipo de personal, y
que la cobertura se mida contra ella (ver OR-256).
**Esfuerzo**: L. **Archivos**: `app/main.py` `[CHOCA]`, `app/routes/admin/stats.py` `[CHOCA]`, `app/static/admin-charts.js` `[CHOCA]`.

### OR-073 · La cobertura toma el total de la primera fila
`app/static/admin-charts.js:273`
**Hoy**: `const total = cobertura[0]?.total || 0`. Si la consulta devuelve cero filas
—porque ninguna categoría tiene slug `parte-%`, que es justo lo que provoca OA-001 en
Retención— el bloque entero se salta sin `_sinDatos()`, y el canvas se queda con el
«Cargando…» ya retirado y ningún contenido: una tarjeta en blanco.
**Debe**: total como dato propio de la respuesta y estado vacío explícito cuando no
hay Partes configuradas.
**Esfuerzo**: S. **Archivos**: `app/static/admin-charts.js` `[CHOCA]`, `app/routes/admin/stats.py` `[CHOCA]`.

### OR-074 · Las gráficas con una sola categoría se declaran «sin datos suficientes»
`app/static/admin-charts.js:305` · `:325` · `:351`
**Hoy**: `if (rows.length < 2)` pinta el estado vacío. Escenario: una facultad donde
todo el personal está Activo ve «Todos los empleados están en el mismo estado» —que
está bien redactado— pero también ve «Sin nivel educativo registrado en las fichas»
cuando en realidad todos tienen el mismo nivel, lo cual es falso y desanima a seguir
rellenando.
**Debe**: con una sola categoría, pintar la barra única (el slot 1, como manda
`CLAUDE.md`) y reservar el estado vacío para cero filas.
**Esfuerzo**: S. **Archivos**: `app/static/admin-charts.js` `[CHOCA]`.

### OR-075 · «Top Departamentos» corta en 10 sin decirlo
`app/routes/admin/stats.py:156` · `app/static/admin_hr.html:226`
**Hoy**: `LIMIT 10` y el título dice «Top Departamentos» sin número ni resto. Con 18
departamentos, ocho desaparecen y la suma de las barras no cuadra con el total de
empleados, sin que nada lo explique.
**Debe**: «Top 10 de N departamentos» y una fila «Otros» agregada, como ya hace la
gráfica por tipo de Archivo (`stats.py:88-90`).
**Esfuerzo**: S. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-076 · «Docs por Tipo» agrupa por `nombre_corto`, que puede ser nulo
`app/routes/admin/stats.py:165-171`
**Hoy**: `SELECT td.nombre_corto AS label` sin `COALESCE`, a diferencia de la rama de
Archivo. Escenario: un tipo creado por migración o por importación con
`nombre_corto` nulo produce una barra con etiqueta `null` en la gráfica.
**Debe**: `COALESCE(td.nombre_corto, td.nombre, 'Sin tipo')`, y limitar a 10 con
«Otros» como en Archivo.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`.

### OR-077 · Las seis gráficas no se pueden exportar ni copiar
`app/static/admin_hr.html:217-267`
**Hoy**: son `<canvas>` sin acciones. Para llevar la distribución por departamento a
un informe hay que hacer una captura de pantalla.
**Debe**: en cada tarjeta, un menú con «descargar PNG», «copiar datos» y «descargar
CSV» de la serie.
**Esfuerzo**: M. **Archivos**: `app/static/admin-charts.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OR-078 · Ninguna gráfica tiene alternativa textual
`app/static/admin_hr.html:221` · `:227` · `:235` · `:244` · `:255` · `:264`
**Hoy**: los seis `<canvas>` no llevan `role="img"` ni `aria-label`, y no hay tabla
equivalente. Para un lector de pantalla la pestaña Resumen es una fila de cifras y
seis huecos. Es un fallo de WCAG 1.1.1 que axe-core no marca porque un `<canvas>`
vacío no le parece contenido.
**Debe**: `role="img"` con un resumen en `aria-label` generado del propio dato, y un
`<table>` visualmente oculto con la serie completa bajo cada gráfica.
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-charts.js` `[CHOCA]`.

### OR-079 · La tarjeta de Cobertura no fija altura y salta al cargar
`app/static/admin_hr.html:244`
**Hoy**: `<canvas id="chart-cobertura-rrhh">` es el único sin atributo `height`; los
otros cinco llevan `height="220"`. Con `maintainAspectRatio:false` la altura la pone
`.ds-chart-box`, pero antes de que Chart.js corra el canvas mide 150 px por defecto,
así que la fila entera da un salto visible al pintarse.
**Debe**: `height="220"` como los demás, y reserva de altura en `.ds-chart-box` para
que el estado de carga ocupe lo mismo que la gráfica.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/styles.css` `[CHOCA]`.

### OR-080 · El Resumen no dice quién ni cuándo actualizó nada
`app/static/admin_hr.html:192-267`
**Hoy**: no hay marca de «datos a las 14:32» ni botón de refrescar. Escenario: se
deja la pestaña abierta toda la mañana, se cargan cincuenta documentos desde otro
puesto y las cifras siguen siendo las de las nueve, sin ninguna señal.
**Debe**: marca de tiempo del último refresco, botón de recargar y refresco
automático al volver a la pestaña del navegador.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-stats.js` `[CHOCA]`.

---
## D. Pestaña Ingresar — alta y carga de archivos

### OR-081 · Un solo formulario mezcla dar de alta a una persona y archivar un papel
`app/static/admin-submit.js:81-193` · `app/static/admin_hr.html:310`
**Hoy**: la tarjeta se titula «Nuevo Empleado / Documento» y pide en una sola pasada
los datos personales (nombres, cédula, RIF, nacimiento, sexo, nivel educativo) y los
del documento (tipo, fecha, ubicación física). Son dos operaciones distintas con
frecuencias distintas: se da de alta a una persona una vez y se le archivan
documentos cien veces. Escenario: para archivar el tercer reposo de alguien hay que
volver a teclear su cédula y ver quince campos personales que no se van a usar
(OR-015).
**Debe**: dos flujos separados — «Nuevo empleado» y «Archivar documento» (que empieza
buscando a la persona) — con el segundo accesible también desde la ficha.
**Esfuerzo**: M. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/static/admin_hr.html`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-082 · «Fecha de Ingreso» es en realidad la fecha del documento
`app/static/admin-submit.js:148-149` · `app/routes/admin/docs.py:249` · `:309`
**Hoy**: el campo `reg-fecha-rrhh` se etiqueta «Fecha de Ingreso *», se envía como
`fecha` y el servidor lo usa a la vez como `fecha_documento` del papel y como
`fecha_ingreso` del empleado. Escenario: se archiva en 2026 la partida de nacimiento
de alguien que entró en 1998; si es su primer documento, su fecha de ingreso queda
fijada en 2026, y con ella su antigüedad, su orden en la búsqueda y el filtro por
rango de fechas del buscador.
**Debe**: dos campos distintos: «Fecha de ingreso a la institución» (del empleado) y
«Fecha del documento» (del papel), con validaciones propias.
**Esfuerzo**: M. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`, `app/models.py` `[CHOCA]`.

### OR-083 · «Retención Física» es la ubicación, no un plazo de retención
`app/static/admin-submit.js:152-153`
**Hoy**: el campo se etiqueta «Retención Física *» con el marcador «Ej: Archivo
Central - Caja J-02» y se manda como `ubicacion`. En Archivo el mismo campo se llama
«Ubicación Física», y «Retención» es en este sistema el plazo de conservación (ISO
15489), con su propia pestaña. El nombre significa aquí lo contrario de lo que
significa dos pestañas más allá.
**Debe**: «Ubicación física», idéntico a Archivo.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`.

### OR-084 · No hay validación de cédula ni de RIF venezolanos
`app/static/admin-submit.js:92-103` · `app/models.py`
**Hoy**: `reg-cedula` es texto libre; sólo se comprueba que no esté vacío
(`admin-submit.js:257`). Escenario: se teclea `1234567` sin letra, o se cuela un
teléfono; el sistema lo acepta como cédula, la persona entra al padrón y ningún
cruce con nómina la reconoce.
**Debe**: máscara y validación de `V|E-#######/########` para cédula y
`J|G|V|E-########-#` con dígito verificador para el RIF, en el cliente y en
`models.py`, con mensaje bajo el campo.
**Esfuerzo**: M. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/models.py` `[CHOCA]`, `app/tests/test_admin.py`.

### OR-085 · No se comprueba la coherencia entre nacimiento, ingreso y jubilación
`app/static/admin-submit.js:146-192` · `app/routes/admin/docs.py:295-317`
**Hoy**: nada impide una fecha de nacimiento posterior a la de ingreso, un ingreso
posterior a la jubilación, una jubilación anterior a hoy en un empleado «Activo» o
un nacimiento en 2025. Escenario: un dedo torpe teclea 1999 en vez de 1969 y el
empleado aparece en el KPI de jubilaciones próximas con 26 años.
**Debe**: reglas en el borde — `nacimiento < ingreso < jubilación`, edad al ingreso
entre 16 y 75, aviso (no bloqueo) si el estado no cuadra con las fechas.
**Esfuerzo**: M. **Archivos**: `app/models.py` `[CHOCA]`, `app/static/admin-submit.js` `[CHOCA]`, `app/static/admin-edit-hr.js`.

### OR-086 · El botón de buscar por cédula no avisa de que la persona ya existe
`app/static/admin_hr.html:96-102` · `app/static/admin-submit.js`
**Hoy**: hay un botón «Buscar empleado por cédula» que llama a `_lookupByCedula`, y
un `<small id="reg-cedula-hint-rrhh">` para su resultado, pero la comprobación es
manual: nadie la dispara al salir del campo. Escenario: se teclea una cédula que ya
existe, no se pulsa el botón, y ocurre OR-015 sin ningún aviso previo.
**Debe**: comprobación automática al perder el foco, con un aviso claro («ya existe:
Susana Pérez — se añadirá a su expediente») y enlace a la ficha.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-087 · Los asteriscos de obligatorio no coinciden con lo que se valida
`app/static/admin-submit.js:85-153` · `:256-259`
**Hoy**: llevan asterisco Nombres, Apellidos, Cédula, Tipo, Departamento, Estado,
Fecha e Ubicación; el validador real sólo exige Nombres, Cédula y Ubicación.
Escenario: se envía sin apellidos y sin departamento; el sistema guarda, el nombre
completo queda a medias y el departamento se resuelve a «Por Asignar» sin decirlo.
**Debe**: una sola lista de campos obligatorios, respetada por el marcado, el
validador del cliente y `models.py`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/models.py` `[CHOCA]`.

### OR-088 · Los errores salen como toast y no señalan el campo
`app/static/admin-submit.js:252-259`
**Hoy**: cada fallo es un `showToast(...)` que aparece arriba a la derecha y se va
solo. Escenario: el formulario tiene veinte campos repartidos en seis filas; el toast
dice «La retención física es requerida», desaparece a los pocos segundos y el usuario
recorre el formulario buscando cuál es. El foco no se mueve.
**Debe**: mensaje bajo el campo con `aria-describedby`, `aria-invalid`, borde de
error y foco automático en el primero que falle. Validación al salir de cada campo,
no sólo al enviar.
**Esfuerzo**: M. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OR-089 · Salir de la pestaña con el formulario a medias lo destruye
`app/static/admin.js:12-16`
**Hoy**: `loadAdminTab` quita `show active` del panel y `renderDynamicSubmitFields()`
regenera el HTML entero al volver. Escenario: se llevan quince campos rellenados, se
pulsa Expedientes para comprobar un dato, se vuelve, y el formulario está vacío. No
hay confirmación ni borrador.
**Debe**: conservar el estado del formulario al cambiar de pestaña, confirmar antes
de descartarlo, y guardar un borrador local recuperable.
**Esfuerzo**: M. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/static/admin-submit.js` `[CHOCA]`.

### OR-090 · Nada avisa al cerrar la pestaña del navegador con datos sin guardar
`app/static/admin-submit.js`
**Hoy**: no hay `beforeunload`. Un clic en la X del navegador o en un enlace del menú
lateral se lleva el trabajo sin preguntar.
**Debe**: `beforeunload` cuando el formulario esté sucio, y confirmación en la
navegación interna.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/static/app.js` `[CHOCA]`.

### OR-091 · Tras guardar, la pantalla salta a Resumen
`app/static/admin-submit.js:333`
**Hoy**: `loadAdminTab("stats")` al final del alta. Escenario: se están cargando
treinta contratos seguidos; cada uno lanza al Resumen y hay que volver a Ingresar,
esperar el repintado del formulario y empezar de nuevo. Además dispara cuatro
peticiones (OA-210) por cada documento archivado.
**Debe**: quedarse en Ingresar con un mensaje de éxito que enlace al registro creado
y un botón «Archivar otro para la misma persona» que conserve la cédula.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`.

### OR-092 · Tras guardar se dispara la búsqueda pública del módulo
`app/static/admin-submit.js:332`
**Hoy**: `triggerRrhhSearch()` se llama desde el panel de administración, donde la
sección de búsqueda pública ni siquiera está montada. Es una petición de búsqueda
completa —con sus facetas— cuyo resultado no se pinta en ninguna parte.
**Debe**: invalidar la caché de la búsqueda en vez de ejecutarla.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`.

### OR-093 · El tipo de documento no filtra por Parte
`app/static/admin-submit.js:122-132`
**Hoy**: el `<select>` agrupa los tipos por Parte con `<optgroup>`, lo cual está bien,
pero no hay forma de elegir primero la Parte y ver sólo sus tipos. Con cuatro Partes
y treinta tipos, la lista desplegable es larga y no se puede buscar dentro (es un
`<select>` nativo, no TomSelect como en el buscador).
**Debe**: selector de Parte y tipo en cascada, o TomSelect con búsqueda, coherente
con el resto del sistema.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/static/app-choices.js` `[CHOCA]`.

### OR-094 · No se puede archivar más de un documento a la vez
`app/static/admin-submit.js:243-339`
**Hoy**: un envío, un documento, un archivo adjunto. Escenario: llega un expediente
físico de una persona con veinte piezas escaneadas; hay que repetir el formulario
veinte veces, tecleando la cédula cada vez.
**Debe**: alta múltiple — elegir la persona una vez y añadir N filas de (tipo, fecha,
archivo), o arrastrar N ficheros y clasificarlos en una tabla.
**Esfuerzo**: L. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-095 · La subida no tiene progreso ni se puede cancelar
`app/static/admin-submit.js:310-319`
**Hoy**: el botón cambia a «Subiendo archivo...» con un icono girando; no hay
porcentaje ni barra, y no se puede abortar. Escenario: 20 MB por una conexión lenta
de la facultad: cuatro minutos sin saber si va por el 5 % o por el 95 %, sin poder
cancelar y sin poder hacer otra cosa en la pestaña.
**Debe**: `XMLHttpRequest`/`fetch` con progreso real, porcentaje, velocidad estimada
y botón de cancelar. `showProgress()` de `admin-ui.js:292` ya existe pero sólo se usa
en la importación y es indeterminado.
**Esfuerzo**: M. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`.

### OR-096 · Si la subida funciona y el guardado falla, el archivo queda huérfano
`app/static/admin-submit.js:310-325`
**Hoy**: primero se sube a R2 y luego se inserta la fila. Si el `POST /submit` falla
(OR-034), el objeto ya está en el almacenamiento sin ninguna fila que lo referencie y
sin forma de enumerarlo desde la aplicación.
**Debe**: subida en dos fases con confirmación, o barrido periódico de objetos no
referenciados. Es la cara complementaria de OA-006.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/storage.py`.

### OR-097 · Ni el tipo ni el tamaño del archivo se validan antes de subir
`app/static/admin-submit.js:308-316` · `app/static/admin_hr.html:318`
**Hoy**: el `<input type="file">` del alta no tiene `accept`, a diferencia del modal
de edición (`admin_hr.html:720`). Escenario: se selecciona un `.docx` de 80 MB, se
espera toda la subida y el servidor lo rechaza — o peor, lo acepta y queda un
formato que el visor no sabe mostrar.
**Debe**: `accept` con la lista canónica (OR-028), comprobación de tamaño y de tipo
en el cliente, y verificación del contenido real en el servidor, no de la extensión.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-submit.js` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-098 · El archivo seleccionado no se puede quitar ni previsualizar
`app/static/admin-monitor.js:240-269`
**Hoy**: una vez elegido, la etiqueta cambia a «Archivo: contrato.pdf» y no hay
manera de deseleccionarlo salvo recargando la pestaña, ni de comprobar que es el
correcto antes de enviarlo.
**Debe**: miniatura o primera página, nombre, tamaño y una X para quitarlo.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OR-099 · El alta no ofrece estado del documento: todo entra aprobado
`app/routes/admin/docs.py:319-338` · `app/static/admin-submit.js`
**Hoy**: el `INSERT` de `datos_rrhh` no fija `status`, que toma el `DEFAULT
'aprobado'`. El modal de edición sí ofrece los cuatro estados y el flujo de trabajo
existe en el modelo. Escenario: un asistente carga un documento dudoso y entra
directamente como aprobado y publicado, sin pasar por revisión (ver OR-058).
**Debe**: selector de estado en el alta, con «borrador» por defecto para roles no
Admin.
**Esfuerzo**: S. **Archivos**: `app/static/admin-submit.js` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-100 · El título del documento de RRHH se compone con un campo libre
`app/routes/admin/docs.py:329`
**Hoy**: `f"{req.doc_type} de {req.personas_relacionadas or req.empleado}"`.
`personas_relacionadas` es el campo libre «Personas / Dependencias Relacionadas», que
admite listas con punto y coma. Escenario: se escribe «Susana Pérez; Dirección RRHH»
y el documento se titula «Contrato de Susana Pérez; Dirección RRHH», que es lo que
saldrá en la papelera, en los listados y en cualquier exportación futura.
**Debe**: componer con el nombre del titular del expediente y la fecha, o permitir un
título propio.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`.

---

## E. Pestaña Ingresar — importación CSV

### OR-101 · No hay previsualización antes de aplicar
`app/static/admin-charts.js:382-432` · `app/routes/admin/imports.py`
**Hoy**: se elige el archivo y se pulsa Importar; la primera noticia de lo que va a
pasar es el resultado, ya aplicado. Combinado con OR-004 (borra nombres) y con la
falta de transacción (OA-022), el daño ya está hecho cuando se ve.
**Debe**: modo `dry_run` obligatorio — tabla de las primeras filas ya interpretadas,
recuento de altas y modificaciones, lista de conflictos y un diff por campo antes de
confirmar.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`, `app/static/admin-charts.js` `[CHOCA]`.

### OR-102 · Una importación no se puede deshacer
`app/routes/admin/imports.py:68-124`
**Hoy**: cada fila es su propio commit y no queda registro de qué filas tocó una
importación concreta; la auditoría sólo guarda los totales. Escenario: se sube el
CSV equivocado sobre 400 empleados y no hay ninguna vía de vuelta salvo restaurar la
copia completa del sistema, que borra también todo lo hecho desde entonces.
**Debe**: identificador de lote por importación, guardado en cada fila afectada, con
«deshacer este lote» disponible durante un plazo.
**Esfuerzo**: L. **Archivos**: `app/main.py` `[CHOCA]`, `app/routes/admin/imports.py` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-103 · Subir el mismo CSV dos veces duplica todos los documentos
`app/routes/admin/imports.py:206-215`
**Hoy**: la importación de documentos no tiene clave natural ni comprobación de
duplicados: inserta siempre. Escenario: alguien duda de si la importación funcionó
(y con OR-003 tiene motivos), la repite, y cada expediente acaba con dos copias de
cada documento. Es la parte RRHH de OA-022.
**Debe**: idempotencia por (cédula, tipo, fecha, folio) con `ON CONFLICT DO NOTHING`
y recuento de omitidos por duplicado.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`, `app/main.py` `[CHOCA]`.

### OR-104 · El CSV no puede traer los campos LOTTT documentados en la barra
`app/static/admin_hr.html:276` · `app/static/admin_hr.html:284` · `app/routes/admin/imports.py:82-85`
**Hoy**: el `title` y el tooltip de la barra anuncian «cedula, nombres, apellidos,
cargo, departamento, estado» y como opcionales «rif, fecha_jubilacion,
fecha_pension». El importador acepta además `foto_url`, `fecha_nacimiento`,
`nivel_educativo` y `sexo`, que la ayuda no menciona, y no acepta `fecha_ingreso`,
que es obligatoria (OR-002).
**Debe**: una sola lista de columnas, publicada en la ayuda, en la documentación y en
una plantilla descargable, generada del mismo sitio del que la lee el importador.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/routes/admin/imports.py` `[CHOCA]`, `docs/funcionalidades.md`.

### OR-105 · No hay plantilla CSV descargable
`app/static/admin_hr.html:273-304`
**Hoy**: para saber qué columnas escribir hay que pasar el ratón sobre un icono cuyo
tooltip no funciona (OR-027). No existe un `.csv` de ejemplo con las cabeceras
correctas y una fila de muestra.
**Debe**: enlace «descargar plantilla» junto a cada barra, con las cabeceras exactas y
una fila comentada.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/routes/admin/imports.py` `[CHOCA]`.

### OR-106 · El separador y el decimal se dan por supuestos
`app/routes/admin/imports.py:64`
**Hoy**: `csv.DictReader` con el dialecto por defecto, es decir coma. Excel en
español guarda con punto y coma por omisión. Escenario: se exporta desde Excel, se
sube, y el lector interpreta una sola columna llamada
`cedula;nombres;apellidos;...`; `row.get("cedula")` devuelve `None`, todas las filas
se cuentan como omitidas y el mensaje es «0 insertados, N omitidos» sin explicación.
**Debe**: detección automática del delimitador con `csv.Sniffer`, y decirlo en el
resumen («detectado separador “;”»).
**Esfuerzo**: S. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`.

### OR-107 · Las cabeceras deben coincidir exactamente, sin acentos ni espacios
`app/routes/admin/imports.py:69-85`
**Hoy**: `row.get("cedula")` es sensible a mayúsculas y a espacios. `Cédula`,
`CEDULA` o ` cedula` no se reconocen y la fila se omite en silencio.
**Debe**: normalizar las cabeceras (minúsculas, sin acentos, sin espacios) y aceptar
sinónimos habituales (`ci`, `cedula_identidad`, `documento`).
**Esfuerzo**: S. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`.

### OR-108 · Una cabecera desconocida no produce ningún aviso
`app/routes/admin/imports.py:64-67`
**Hoy**: sólo se comprueba que `fieldnames` no sea `None`. Escenario: el CSV trae
`sueldo`, `telefono` y `correo`; esas tres columnas se ignoran sin decir nada y quien
las preparó cree que se han guardado.
**Debe**: listar en el resultado las columnas ignoradas y las obligatorias ausentes,
antes de aplicar nada.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`.

### OR-109 · Los errores por fila se recortan a cien caracteres y sólo se ven cinco
`app/routes/admin/imports.py:119` · `app/static/admin-charts.js:409-410`
**Hoy**: `str(e)[:100]` guarda un mensaje de psycopg2 truncado, y la pantalla enseña
las cinco primeras y «… y N más», sin forma de ver el resto ni de descargarlas.
Escenario: fallan 380 filas de 400 y el usuario ve cinco fragmentos de error de base
de datos en inglés.
**Debe**: mensajes en español por causa (no la excepción cruda), tabla completa de
errores paginada y descarga del CSV de filas rechazadas listo para corregir y
reintentar.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`, `app/static/admin-charts.js` `[CHOCA]`.

### OR-110 · «Importación completada» en verde aunque no se inserte nada
`app/static/admin-charts.js:418-424`
**Hoy**: la clase de la alerta se decide sólo por si hay errores; con `inserted: 0` y
`skipped: 300` sin excepciones (el caso de OR-106) sale una alerta **verde** que dice
«Importación completada: 0 insertados, 300 omitidos.». Es literalmente el fallo que
`CLAUDE.md` documenta para `test_sql_inserts.py`, reaparecido en la capa de UI.
**Debe**: el color y el titular salen del resultado: cero insertados nunca es éxito.
**Esfuerzo**: S. **Archivos**: `app/static/admin-charts.js` `[CHOCA]`.

### OR-111 · No hay límite de tamaño ni de filas en la importación
`app/routes/admin/imports.py:60` · `:139`
**Hoy**: `await file.read()` carga el archivo entero en memoria y el bucle recorre
todas las filas con varias consultas por fila. Escenario: un CSV de 200 000 filas
—o uno malicioso de 500 MB— agota la memoria de la función serverless y el proceso
muere sin dejar rastro ni resultado parcial.
**Debe**: tope declarado de tamaño y de filas, lectura en flujo y proceso por lotes
con informe de avance.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`.

### OR-112 · Una importación larga no informa de su avance
`app/static/admin-charts.js:403`
**Hoy**: `showProgress()` pinta una barra indeterminada. Escenario: 5 000 filas, cada
una con hasta cinco consultas a Neon desde otro continente: varios minutos con una
barra rayada moviéndose y ninguna indicación de si va por la fila 200 o por la 4 800;
si el usuario recarga, la importación sigue por su cuenta a medias.
**Debe**: proceso por lotes con avance real, o una tarea en segundo plano consultable
con su propio identificador.
**Esfuerzo**: L. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`, `app/static/admin-charts.js` `[CHOCA]`.

### OR-113 · La importación revive expedientes de la papelera sin decirlo
`app/routes/admin/imports.py:90-106`
**Hoy**: la búsqueda por cédula no filtra `deleted_at` y el `UPDATE` tampoco lo
limpia. Escenario: una cédula que está en la papelera se «actualiza» con los datos
nuevos; se cuenta como `updated`, pero el expediente sigue invisible en todas las
pantallas y quien lo subió cree que la persona ya está registrada.
**Debe**: detectar el caso, contarlo aparte y ofrecer restaurar explícitamente.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`.

### OR-114 · La importación no registra el historial de cargos
`app/routes/admin/imports.py:87-88`
**Hoy**: el CSV puede traer `cargo` y el importador lo escribe en `empleados.cargo_id`
sin abrir ningún tramo en `historial_cargos`. Escenario: se carga la planta entera
desde nómina y el sistema queda con 400 personas con cargo y cero movimientos de
carrera: el KPI «Mov. de cargo» marca 0 y el reporte imprimible dice «Sin historial
registrado» para todos.
**Debe**: al importar un cargo, abrir el tramo correspondiente desde `fecha_ingreso`,
y admitir un CSV de historial con (cédula, cargo, desde, hasta, motivo).
**Esfuerzo**: M. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`, `app/routes/hr_alerts.py`.

### OR-115 · La importación de documentos no admite adjuntar el archivo digital
`app/routes/admin/imports.py:206-215`
**Hoy**: sólo metadatos; `file_url` no está entre las columnas. Escenario: hay 3 000
PDF escaneados con la cédula en el nombre y no existe ninguna vía masiva de asociarlos
a su documento: hay que subirlos uno por uno desde el modal de edición.
**Debe**: importación por lote con carpeta o ZIP y emparejamiento por convención de
nombre, con informe de los no emparejados.
**Esfuerzo**: L. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`, `app/storage.py`, `app/static/admin_hr.html`.

### OR-116 · Las dos barras de importación pierden el resultado al cambiar de pestaña
`app/static/admin_hr.html:287` · `app/static/admin.js:16`
**Hoy**: `#csv-import-result-*-rrhh` queda dentro del panel, que no se limpia al
salir pero sí al recargar; y no hay ningún historial de importaciones. Escenario:
llega el resultado con 40 errores, alguien pulsa Expedientes para comprobar uno,
vuelve, y el informe ya no está: hay que repetir la importación para volver a verlo.
**Debe**: historial de importaciones con su informe, consultable después, junto al
historial de copias de seguridad.
**Esfuerzo**: M. **Archivos**: `app/main.py` `[CHOCA]`, `app/routes/admin/imports.py` `[CHOCA]`, `app/static/admin_hr.html`.

---

## F. Pestaña Expedientes

### OR-117 · Sin acciones en lote, la pestaña no escala
`app/static/admin-monitor.js:142-161`
**Hoy**: no hay casillas de selección ni «seleccionar todo»; cada operación es fila a
fila. Escenario: hay que cambiar de departamento a los 34 profesores que pasan de
Biología a Biología Celular: 34 modales, 34 guardados. Es el equivalente RRHH de
OA-103 y aquí pesa más, porque las operaciones de RRHH son casi siempre colectivas
(un concurso, una jubilación masiva, un cambio de adscripción).
**Debe**: selección múltiple con acciones — cambiar departamento, cambiar estado,
exportar, imprimir, mover a papelera — con confirmación que enumere lo afectado.
**Esfuerzo**: L. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin_hr.html`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-118 · La columna «Ubicación» muestra la de un documento cualquiera
`app/routes/admin/docs.py:167`
**Hoy**: `COALESCE(MIN(dr.ubicacion),'')` — el mínimo alfabético de las ubicaciones de
todos los documentos del expediente, presentado como si fuera la ubicación del
expediente. Escenario: una persona con documentos en «Caja A-01» y en «Digitalizado
Exclusivo» aparece siempre como «Caja A-01», aunque el 90 % de su expediente esté en
otro sitio.
**Debe**: o la ubicación física del expediente como dato propio del empleado, o la
lista de ubicaciones distintas con su recuento.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/main.py` `[CHOCA]`.

### OR-119 · La columna «Tipos» enseña sólo el primero por orden alfabético
`app/static/admin-monitor.js:151`
**Hoy**: `(f.tipos||'').split(';')[0].trim()` en un badge, con el resto en el
atributo `title`. Escenario: un expediente con doce tipos distintos muestra
«Acta de Grado» y nada más; para ver el resto hay que dejar el ratón quieto encima, lo
cual no funciona en móvil ni con teclado.
**Debe**: el conteo por Parte (I·II·III·IV) con cuatro micro-indicadores, que es la
información que se necesita de un vistazo, en lugar de un tipo arbitrario.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-120 · La columna «Cargo / Depto.» muestra uno u otro, nunca los dos
`app/static/admin-monitor.js:149`
**Hoy**: `escHtml(f.cargo||f.departamento||'—')` bajo una cabecera que promete los
dos. Escenario: quien tiene cargo nunca ve su departamento en la tabla, y como el
cargo casi siempre existe, la columna es en la práctica «Cargo» con un encabezado que
miente. Además está truncada a 120 px con `text-overflow`, sin `title`.
**Debe**: dos líneas en la misma celda (cargo arriba en negrita, departamento debajo
en secundario) y `title` con el texto completo.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`.

### OR-121 · La tabla no dice cuántos documentos tiene cada expediente
`app/routes/admin/docs.py:164` · `app/static/admin-monitor.js:146-157`
**Hoy**: `list_all` calcula `doc_count` y el renderizador no lo pinta; el buscador
público sí lo enseña (`hr.js:159`). En la pestaña cuyo trabajo es completar
expedientes, el número de documentos es el dato principal y está pedido, servido y
tirado.
**Debe**: columna con el conteo, ordenable, y resalte de los expedientes vacíos.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-122 · Los badges de estado del monitor están apagados en RRHH
`app/static/admin-monitor.js:56`
**Hoy**: `if (!isArchivoModule()) { badgesEl.innerHTML = ""; return; }`. El
contenedor `#monitor-status-badges-rrhh` existe en el HTML, la API
`/status_counts?modulo=RRHH` funciona, y el flujo de trabajo (borrador · revisión ·
aprobado · rechazado) está implementado para `datos_rrhh`. Escenario: hay 40
documentos de personal esperando revisión y no existe ninguna pantalla en todo el
módulo RRHH que los liste.
**Debe**: encender los badges en RRHH —contando documentos del expediente, no
empleados— o una bandeja de pendientes propia. `/documentos/pendientes` ya sirve el
dato (`docs.py:499-514`) y nadie lo llama.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-123 · Desde el monitor no se llega a ningún documento
`app/static/admin-monitor.js:153-157`
**Hoy**: las tres acciones son ver expediente (rota, OR-001), editar empleado y
borrar empleado. No hay forma de abrir, editar, compartir o borrar **un documento**
de RRHH desde el panel, aunque `openEditDocModal`, `compartirDocumento` y
`handleDeleteDoc` existan y el modal `editArchivoModal` esté en la página
(`admin_hr.html:629`) esperando a alguien que lo abra.
**Debe**: la fila del expediente se despliega en sus documentos, con las mismas
acciones que tiene Archivo.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-124 · No se puede filtrar por lo que de verdad hace falta filtrar
`app/static/admin_hr.html:370-387`
**Hoy**: cuatro controles — búsqueda, tipo, persona y estado laboral. No hay filtro
por departamento, por cargo, por rango de fecha de ingreso, por «sin documentos», por
«falta la Parte N», por jubilación próxima ni por nivel educativo. Escenario: la
petición más común de una dirección de RRHH —«dame los activos de Biología con más de
25 años de servicio»— no se puede responder con esta pantalla.
**Debe**: panel de filtros con los criterios de RRHH, combinables, con recuento de
resultados y filtros guardables.
**Esfuerzo**: L. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-monitor.js` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-125 · El filtro de persona duplica al buscador y se rellena mal
`app/static/admin-monitor.js:32-42` · `app/static/admin_hr.html:375`
**Hoy**: en RRHH el desplegable «Persona…» se rellena con
`state.choices.rrhh.people`, la lista completa de personal, y filtra por nombre con
`ILIKE`; es decir, hace lo mismo que el cuadro de búsqueda que tiene al lado. Con 400
empleados es un `<select>` nativo de 400 opciones sin búsqueda.
**Debe**: eliminarlo en RRHH (donde la unidad de la fila ya es la persona) y usar ese
hueco para el filtro de departamento.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-126 · El filtro de estado laboral usa coincidencia parcial
`app/routes/admin/docs.py:128-130`
**Hoy**: `COALESCE(el.estados,'') ILIKE '%Activo%'`. Escenario: en cuanto exista un
estado «Inactivo» o «Reactivado» —y el catálogo se crea solo, OR-039— el filtro
«Activos» los incluye. Es además el mismo parámetro `status_filter` que en Archivo
significa estado del documento, con semántica distinta y sin validación.
**Debe**: igualdad exacta contra el catálogo, y nombres de parámetro distintos para
conceptos distintos.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`.

### OR-127 · Un tecleo, una petición: el buscador no tiene rebote
`app/static/app.js:294` · `app/static/admin-ui.js:7`
**Hoy**: `admin_search-rrhh` dispara `loadMonitorTable()` en cada `input`. Escribir
«González» son ocho consultas con `to_tsvector` y `unaccent` sobre `empleados`, más
ocho `COUNT(DISTINCT)`, contra una base en otro continente; las respuestas llegan
desordenadas y la tabla puede acabar mostrando el resultado de «Gonzá». `debounce()`
existe en `admin-ui.js` y no se usa aquí.
**Debe**: rebote de 300 ms y cancelación de la petición anterior con `AbortController`.
**Esfuerzo**: S. **Archivos**: `app/static/app.js` `[CHOCA]`, `app/static/admin-monitor.js` `[CHOCA]`.

### OR-128 · Ninguna columna se puede ordenar
`app/static/admin_hr.html:390-400` · `app/routes/admin/docs.py:173`
**Hoy**: el orden es fijo, `ORDER BY e.apellidos, e.nombres`, y los `<th>` no son
pulsables. No se puede ordenar por fecha de ingreso, por número de documentos ni por
estado.
**Debe**: encabezados ordenables con indicador de sentido, `aria-sort` y el orden
propagado al servidor y a la exportación.
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-monitor.js` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-129 · El resumen de la tabla contradice a la paginación
`app/static/admin-monitor.js:83` · `app/static/admin_hr.html:405`
**Hoy**: el texto por defecto del HTML es «Mostrando 0 registros» y el JS lo sustituye
por «Total: 412 registros en el módulo RRHH», que no dice cuáles se están viendo.
Escenario: en la página 3 de 25 en 25, la pantalla dice «Total: 412» y «Pág 3 de 17»,
pero no «51–75 de 412», que es lo que se necesita para saber dónde se está.
**Debe**: «Mostrando 51–75 de 412 expedientes», y el mismo texto en el estado inicial.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-130 · La paginación no permite saltar a una página
`app/static/admin_hr.html:406-416`
**Hoy**: sólo anterior y siguiente. Con 412 empleados y 25 por página son 17 páginas
que se recorren de una en una; no hay «primera», «última» ni salto directo.
**Debe**: paginación completa con salto a página e ir al principio y al final.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-monitor.js` `[CHOCA]`.

### OR-131 · Los filtros y la página no sobreviven a nada
`app/static/admin-monitor.js:2-10` · `app/static/admin.js:17`
**Hoy**: `loadAdminTab("monitor")` fuerza `state.adminTable.page = 1`, y ni los
filtros ni la página van a la URL. Escenario: se busca «Rodríguez», se llega a la
página 4, se abre una ficha, se guarda, y se vuelve a la página 1 sin filtro. Con
recargar la pestaña pasa lo mismo, y no se puede enviar un enlace a un compañero.
**Debe**: estado en la URL (búsqueda, filtros, orden, página) y restauración al
volver de un modal.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin.js` `[CHOCA]`.

### OR-132 · El enlace «Editar expediente» del buscador no lleva a ninguna parte útil
`app/static/hr.js:169`
**Hoy**: la tarjeta de resultado del buscador enlaza a
`/static/admin_hr.html?empId=123` — una ruta estática en vez de `/admin/rrhh`, y con
un parámetro que **nadie lee**: no hay una sola referencia a `empId` fuera de esa
línea. Escenario: se pulsa el lápiz sobre un empleado y se aterriza en el Resumen
del panel, sin la ficha, sin el filtro y sin explicación.
**Debe**: enlace profundo real (`/admin/rrhh?tab=monitor&emp=123`) que abra la
pestaña Expedientes con la ficha de esa persona.
**Esfuerzo**: S. **Archivos**: `app/static/hr.js`, `app/static/admin.js` `[CHOCA]`, `app/static/admin-monitor.js` `[CHOCA]`.

### OR-133 · Un error al cargar la tabla deja el esqueleto para siempre
`app/static/admin-monitor.js:46-48`
**Hoy**: `catch (e) { console.error(...) }`. `showTableSkeleton()` ya pintó seis filas
grises. Escenario: la petición falla con 500 y la tabla se queda con el esqueleto
animado indefinidamente, que se lee como «está cargando», no como «ha fallado»; sólo
la consola lo sabe.
**Debe**: estado de error con el motivo y un botón de reintentar, y estado vacío
distinto del de error.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`.

### OR-134 · El estado vacío habla de archivos en el módulo de personas
`app/static/admin-monitor.js:95`
**Hoy**: «Ningún **archivo** coincide con los criterios de búsqueda», el mismo texto
para los dos módulos. En RRHH cada fila es una persona.
**Debe**: «Ningún expediente coincide…», con una acción sugerida («limpiar filtros» o
«dar de alta a esta persona») según el caso.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`.

### OR-135 · El badge de estado laboral se pinta con color en línea
`app/static/admin-monitor.js:150`
**Hoy**: `style="background-color:${c};color:white;padding:3px 6px;"`. Un color en
`style` en línea no lo corrige ninguna hoja de estilos —lo dice `CLAUDE.md`—, así que
en modo oscuro y en los once temas de color el badge conserva exactamente el mismo
fondo, y el blanco fijo sobre el naranja de «Pensionado» ronda el mínimo de contraste
que `test_contraste.py` vigila para otros pares.
**Debe**: una clase por estado, definida en `styles.css`, con su variante oscura, y el
par de colores incluido en `test_contraste.py`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`, `app/tests/test_contraste.py`.

### OR-136 · La fila entera parece pulsable y no lo es
`app/static/admin_hr.html:18`
**Hoy**: `.table-hover tbody tr:hover { …; cursor: pointer; }` en el `<style>` de la
página. La fila cambia de color y el cursor se hace mano, pero no hay ningún manejador
de clic en la fila: sólo los tres botones del final actúan. Escenario: se pulsa el
nombre de la persona, no pasa nada, se vuelve a pulsar más fuerte.
**Debe**: o la fila abre la ficha, o el cursor no promete que lo haga.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-monitor.js` `[CHOCA]`.

### OR-137 · La exportación CSV sólo baja la página que se está viendo
`app/static/admin-monitor.js:180-231`
**Hoy**: `exportAdminCSV()` recorre `state.adminTable.results`, que son 25 filas. El
nombre del archivo incluye `_p3`, así que el sistema sabe que es parcial y aun así el
toast dice «CSV exportado: 25 registro(s)» sin advertir que faltan 387. Escenario:
alguien exporta «la plantilla» para el Consejo y entrega 25 personas.
**Debe**: exportación del conjunto filtrado completo desde el servidor, con aviso si
supera un umbral y proceso en segundo plano si hace falta.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-138 · El CSV exportado sale sin BOM y Excel destroza los acentos
`app/static/admin-monitor.js:223`
**Hoy**: `new Blob(["" + csv], { type: "text/csv;charset=utf-8;" })`. Excel en Windows
—el destino real de este archivo— ignora el `type` y lee en la codificación del
sistema. Escenario: se abre el CSV y aparece «PÃ©rez GonzÃ¡lez» en las 25 filas.
**Debe**: anteponer `﻿`, como ya hace el importador al aceptar `utf-8-sig`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`.

### OR-139 · La primera línea del CSV rompe su propia estructura
`app/static/admin-monitor.js:221-222`
**Hoy**: se antepone una fila de metadatos («# Exportado: …») **antes** de la fila de
cabeceras, con una sola columna. Escenario: cualquier herramienta que lea el CSV
—Excel, pandas, Power BI— toma esa línea como cabecera y el resto queda desalineado;
hay que borrarla a mano cada vez.
**Debe**: los metadatos van en el nombre del archivo o en un `.txt` adjunto; la
primera línea del CSV son las cabeceras.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`.

### OR-140 · La exportación de personal no se audita
`app/static/admin-monitor.js:180-231`
**Hoy**: el CSV se genera en el navegador con datos ya descargados, así que el
servidor no se entera. Escenario: alguien exporta cédulas, RIF, fechas de nacimiento
y nivel educativo de 400 personas y no queda absolutamente ningún registro.
**Debe**: la exportación pasa por un endpoint que la registra en auditoría con el
filtro aplicado y el número de filas.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/static/admin-monitor.js` `[CHOCA]`.

---
## G. Ficha del empleado, dossier e historial de cargos

### OR-141 · La cédula no se puede corregir desde ninguna pantalla
`app/static/admin_hr.html:789-868` · `app/routes/admin/docs.py:557-605`
**Hoy**: el modal de edición no incluye la cédula y `update_empleado` no la acepta.
Escenario: se teclea mal un dígito en el alta (y no hay validación, OR-084); la única
salida es borrar el expediente, purgarlo con todos sus documentos y volver a
empezar. Con OR-016 encima, la vía habitual será crear un duplicado.
**Debe**: cédula editable con confirmación explícita, comprobación de unicidad,
registro en auditoría del valor anterior, y una operación de fusión de expedientes
duplicados.
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-edit-hr.js`, `app/routes/admin/docs.py` `[CHOCA]`, `app/models.py` `[CHOCA]`.

### OR-142 · La fecha de ingreso tampoco se puede corregir
`app/static/admin_hr.html:822-864` · `app/routes/admin/docs.py:561-593`
**Hoy**: la ficha edita nacimiento, sexo, nivel educativo, RIF, jubilación y
pensión, pero no `fecha_ingreso`, que es de donde salen la antigüedad, el orden de la
búsqueda y el filtro por rango del buscador. Y con OR-082 hay muchas probabilidades
de que esté mal.
**Debe**: `fecha_ingreso` editable con validación de coherencia (OR-085).
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-edit-hr.js`, `app/routes/admin/docs.py` `[CHOCA]`, `app/models.py` `[CHOCA]`.

### OR-143 · La ficha no muestra la foto que está editando
`app/static/admin_hr.html:865-868`
**Hoy**: un `<input type="text">` con la URL y nada más. Escenario: se pega una URL
mal copiada y no se descubre hasta abrir el dossier o imprimir el expediente, donde
sale un icono de imagen rota.
**Debe**: previsualización junto al campo, con estado de error si la imagen no carga
y botón de quitar.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-edit-hr.js`.

### OR-144 · La ficha no dice cuántos documentos tiene ni enlaza a ellos
`app/static/admin_hr.html:779-909`
**Hoy**: el modal edita datos personales y el historial de cargos; el expediente
documental —lo que da nombre al sistema— no aparece por ninguna parte. Escenario: se
está corrigiendo la ficha de alguien y hace falta comprobar si tiene el título
consignado: hay que cerrar, ir al ojo (roto, OR-001) y volver.
**Debe**: pestañas dentro de la ficha — Datos · Documentos · Historial · Alertas —
con el expediente completo y sus acciones.
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-edit-hr.js`.

### OR-145 · La ficha no calcula ni muestra la antigüedad ni la edad
`app/static/admin_hr.html:822-864`
**Hoy**: se ven `fecha_nacimiento` y `fecha_ingreso` en crudo, en formato ISO, y el
lector hace la resta mentalmente. Antigüedad y edad son los dos datos que gobiernan
jubilación, prima, vacaciones y escalafón en la LOTTT.
**Debe**: junto a cada fecha, el valor derivado («52 años», «27 años y 4 meses de
servicio»), calculado en el servidor para que coincida con los informes.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/static/admin-edit-hr.js`.

### OR-146 · Las fechas se muestran en formato ISO en un panel en español
`app/static/admin-edit-hr.js:235-236` · `app/static/admin_hr.html:826`
**Hoy**: la tabla del historial pinta `2026-03-01` tal cual, y `formatISOToSpanish()`
—que existe y se usa en el monitor de Archivo— no se llama en ningún punto de la rama
RRHH salvo el KPI de último ingreso.
**Debe**: formato local (`01/03/2026`) en toda la lectura, con el ISO reservado a los
`<input type="date">`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit-hr.js`, `app/static/admin-monitor.js` `[CHOCA]`.

### OR-147 · Si la ficha no carga del servidor, se editan los datos de la tabla
`app/static/admin-edit-hr.js:7-9`
**Hoy**: `try { rec = {...rec, ...await fetch} } catch {}` — si la petición falla, se
sigue con lo que había en `state.adminTable.results`, que no trae `fecha_nacimiento`,
`sexo` ni `foto_url`. Escenario: la red falla, el modal abre con esos tres campos
vacíos, el usuario guarda un cambio de departamento y el `PUT` escribe `null` en los
tres, borrando datos que sí existían.
**Debe**: si la carga falla, no abrir el modal; mostrar el error y ofrecer reintentar.
Y el `PUT` sólo debe enviar los campos que el usuario haya tocado.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit-hr.js`.

### OR-148 · El guardado envía todos los campos, siempre
`app/static/admin-edit-hr.js:49-63` · `app/routes/admin/docs.py:559-593`
**Hoy**: el `PUT` manda las doce columnas en cada guardado, y el servidor escribe
toda la que no sea `None`. Escenario: dos personas abren la misma ficha; una cambia
el cargo, la otra el departamento; la segunda en guardar revierte el cambio de la
primera sin que nadie se entere. No hay control de concurrencia pese a existir
`updated_at`.
**Debe**: enviar sólo lo modificado y comprobar `updated_at` como testigo optimista,
con aviso de conflicto y opción de recargar.
**Esfuerzo**: M. **Archivos**: `app/static/admin-edit-hr.js`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-149 · Cerrar el modal con cambios sin guardar no pregunta nada
`app/static/admin_hr.html:786` · `app/static/admin_hr.html:902`
**Hoy**: la X y «Cancelar» cierran directamente. Escenario: se corrigen ocho campos,
se pulsa Escape por costumbre (`admin-ui.js:166`) y todo se pierde en silencio.
**Debe**: detectar el formulario sucio y confirmar antes de cerrar, por los tres
caminos (X, Cancelar, Escape).
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit-hr.js`, `app/static/admin-ui.js` `[CHOCA]`.

### OR-150 · El modal de empleado no tiene atajo de guardado
`app/static/admin_hr.html:903`
**Hoy**: `admin-ui.js:174-181` implementa Ctrl+S buscando `.btn-save-modal`; el modal
de documento lo tiene (`admin_hr.html:770`) y el de empleado no. Escenario: quien
aprendió Ctrl+S en un modal descubre que en el otro no hace nada — y como el
`preventDefault` sí se ejecuta, tampoco guarda la página: no pasa nada en absoluto.
**Debe**: la clase `btn-save-modal` en el botón de guardar del modal de empleado, y
una guarda que compruebe que todo modal con guardado la lleva.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/tests/test_admin_panels.py`.

### OR-151 · El modal de empleado no atrapa el foco
`app/static/admin_hr.html:779`
**Hoy**: `<div class="modal fade" id="editEmpleadoModal" tabindex="-1" role="dialog">`
sin `aria-modal`, sin `aria-labelledby` y sin trampa de foco propia. Bootstrap 4.6
mueve el foco al abrir pero no impide tabular fuera del diálogo hacia la página de
debajo. Escenario: con teclado, tras el último campo el foco salta a la barra de
navegación mientras el diálogo sigue abierto y el fondo está oscurecido.
**Debe**: `aria-modal="true"`, `aria-labelledby` apuntando al título, ciclo de
tabulación cerrado y devolución del foco al botón que abrió al cerrar. Igual en
`editArchivoModal` y en `rrhh-person-modal`.
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-ui.js` `[CHOCA]`.

### OR-152 · El modal de empleado sí se cierra con Escape; el de documento, no
`app/static/admin_hr.html:629` frente a `app/static/admin_hr.html:575` y `:612`
**Hoy**: `doc-modal` y `rrhh-person-modal` llevan `data-keyboard="false"` y
`data-backdrop="static"`; `editEmpleadoModal` y `editArchivoModal` no. El manejador
global de `admin-ui.js:166-172` cierra «el primer modal abierto» con Escape sin
mirar si tiene cambios (OR-149). El resultado es un comportamiento distinto en cada
diálogo de la misma página.
**Debe**: una sola regla — Escape cierra si no hay cambios sin guardar, y pregunta si
los hay — aplicada a los cuatro.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-ui.js` `[CHOCA]`.

### OR-153 · El historial de cargos se pliega y hay que abrirlo cada vez
`app/static/admin-edit-hr.js:36-39`
**Hoy**: `openEditEmpleadoModal` fuerza `display:none` en el contenedor y sólo carga
el historial cuando alguien pulsa «Ver historial». Escenario: en el trabajo de
depurar la carrera de un profesor —que es para lo que se abre esta ficha— hay dos
clics extra por persona y una espera en blanco cada vez.
**Debe**: cargar el historial junto con la ficha y recordar si estaba desplegado.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit-hr.js`.

### OR-154 · El formulario de nuevo cargo son cuatro controles pegados sin etiquetas
`app/static/admin_hr.html:885-896`
**Hoy**: un `input-group` con cargo, fecha, motivo y un botón «+», sin `<label>` ni
`aria-label`; los marcadores de posición hacen de etiqueta, que es exactamente lo que
WCAG 3.3.2 no admite. A 390 px los cuatro controles se apilan y el `input-group`
pierde su forma. El botón «+» sólo tiene un icono, sin nombre accesible, contra lo
que `CLAUDE.md` documenta como aprendido.
**Debe**: etiquetas reales (visibles o `sr-only`), `aria-label` en el botón, y
apilado con etiqueta en pantallas estrechas.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`.

### OR-155 · Añadir un cargo no valida las fechas contra el resto del historial
`app/static/admin-edit-hr.js:247-267` · `app/routes/hr_alerts.py:175-223`
**Hoy**: sólo se exige que cargo y fecha no estén vacíos. Se admite una fecha
anterior al ingreso, una futura, o una que solape con un tramo existente (OR-038).
Escenario: se registra un ascenso con fecha 3016 por un dedo y el reporte imprimible
del expediente lo muestra como el cargo actual.
**Debe**: validación contra `fecha_ingreso`, contra hoy y contra los tramos
existentes, en el servidor.
**Esfuerzo**: S. **Archivos**: `app/routes/hr_alerts.py`, `app/static/admin-edit-hr.js`.

### OR-156 · Borrar un tramo del historial es irreversible y de un solo clic
`app/static/admin-edit-hr.js:238` · `:269-277`
**Hoy**: hay confirmación genérica («¿Eliminar esta entrada del historial de
cargos?») y el borrado es físico: `DELETE FROM historial_cargos`. Escenario: se borra
por error el tramo de 1998-2007 de un profesor y no hay papelera, ni deshacer, ni
copia, ni auditoría con el contenido borrado (sólo el id, `hr_alerts.py:240`).
**Debe**: borrado lógico con papelera, o al menos volcado del contenido completo al
detalle de auditoría; confirmación que muestre cargo, fechas y motivo.
**Esfuerzo**: S. **Archivos**: `app/routes/hr_alerts.py`, `app/static/admin-edit-hr.js`.

### OR-157 · El historial no se puede editar, sólo añadir y borrar
`app/static/admin-edit-hr.js:228-245`
**Hoy**: no hay acción de edición. Corregir un motivo mal escrito obliga a borrar y
volver a crear, lo que dispara OR-014 y OR-038.
**Debe**: edición en línea de cargo, fechas y motivo, con auditoría del valor
anterior.
**Esfuerzo**: M. **Archivos**: `app/static/admin-edit-hr.js`, `app/routes/hr_alerts.py`.

### OR-158 · El historial no admite el documento que respalda el movimiento
`app/routes/hr_alerts.py:127-132` · `app/schema.sql:156-165`
**Hoy**: `historial_cargos` guarda cargo, fechas, motivo y quién lo registró. No hay
sitio para la resolución del Consejo, el acta del concurso ni el oficio de
designación — que es el documento que convierte el movimiento en un hecho probable.
La pestaña Retención de Archivo sí exige acta para la disposición
(`admin.js:222-235`): el mismo criterio falta aquí.
**Debe**: enlace a un documento del expediente (Parte II) desde cada tramo, con el
número de resolución y su fecha.
**Esfuerzo**: M. **Archivos**: `app/main.py` `[CHOCA]`, `app/routes/hr_alerts.py`, `app/static/admin_hr.html`.

### OR-159 · El historial guarda el cargo pero no la dedicación ni la categoría
`app/schema.sql:156-165`
**Hoy**: un tramo es (cargo, desde, hasta, motivo). En la carrera docente de la UCV
el movimiento relevante combina **categoría** (Instructor, Asistente, Agregado,
Asociado, Titular), **dedicación** (Exclusiva, Tiempo Completo, Medio Tiempo,
Convencional) y **escalafón**; «Profesor Asociado» a medio tiempo y a dedicación
exclusiva son dos situaciones distintas que el sistema no distingue.
**Debe**: categoría, dedicación y escalafón como campos propios del tramo, con sus
catálogos.
**Esfuerzo**: L. **Archivos**: `app/main.py` `[CHOCA]`, `app/routes/hr_alerts.py`, `app/static/admin_hr.html`, `app/routes/lookups.py`.

### OR-160 · El historial no se ve como una línea de tiempo
`app/static/admin-edit-hr.js:228-241`
**Hoy**: una tabla de cuatro columnas dentro de una caja de 200 px con scroll. Para
leer una carrera de treinta años —que es una secuencia con huecos, solapes y saltos—
la tabla obliga a reconstruirla mentalmente.
**Debe**: línea de tiempo vertical con los tramos a escala, huecos visibles y el
tramo abierto destacado; la tabla como vista alternativa.
**Esfuerzo**: M. **Archivos**: `app/static/admin-edit-hr.js`, `app/static/styles.css` `[CHOCA]`.

### OR-161 · El dossier del backoffice es el del buscador público, con sus fallos
`app/static/admin_hr.html:611-623` · `app/static/hr.js:183`
**Hoy**: el panel carga `hr.js` entero (`admin_hr.html:924`) y reutiliza
`openRrhhPersonDossier` y el modal `#rrhh-person-modal` tal cual. Todo lo que BR-005
(no filtra borrados), BR-109 (estilos en línea, sin modo oscuro) y BR-003 señalan
aparece igual dentro del backoffice, y además sin ninguna de las acciones de edición
que aquí sí tendrían sentido.
**Debe**: si el panel va a reutilizar el dossier, que sea con acciones de
administración; si no, un dossier propio. En cualquier caso, no cargar el buscador
público entero para un modal.
**Esfuerzo**: M. **Archivos**: `app/static/hr.js`, `app/static/admin_hr.html`, `app/static/admin-monitor.js` `[CHOCA]`.

### OR-162 · El modal del expediente no tiene botón de imprimir
`app/static/admin_hr.html:611-623`
**Hoy**: el único botón del pie es «Cerrar». `GET /api/rrhh/report/{emp_id}` genera un
expediente imprimible completo y **no hay un solo enlace hacia él en todo el
backoffice**: la única vía es teclear la URL. El reporte es la salida principal que
pide una dirección de RRHH.
**Debe**: «Imprimir expediente» en el pie del dossier, en la ficha y en la fila del
monitor, con la generación registrada en auditoría.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-monitor.js` `[CHOCA]`, `app/routes/hr.py`.

---

## H. Pestaña Tipos

### OR-163 · Un tipo no se puede renombrar, mover de Parte ni desactivar
`app/static/admin-categories.js:26-53` · `app/routes/admin/catalog.py:82-113`
**Hoy**: la pestaña sólo crea. La lista es de sólo lectura y no existe endpoint de
actualización ni de borrado de `tipo_documento`. Escenario: «Constacia de Trabajo»
entra con una errata y queda así para siempre en todos los desplegables, en el
dossier de 400 personas y en el reporte impreso. Con OR-008 encima, los tipos mal
clasificados tampoco se pueden reubicar.
**Debe**: renombrar, mover de Parte, fusionar dos tipos y desactivar sin borrar, con
recuento de uso y auditoría.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/catalog.py` `[CHOCA]`, `app/static/admin-categories.js` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-164 · La lista de tipos no dice cuántos documentos usa cada uno
`app/static/admin-categories.js:44-48`
**Hoy**: cada tipo es una línea con su nombre y un badge que dice «Activa» —una
etiqueta que no informa de nada, porque todos lo son. La sección de palabras clave de
Archivo sí muestra el uso (`admin-categories.js:90`).
**Debe**: recuento de documentos por tipo, ordenable, y marca de los tipos sin uso
como candidatos a retirar.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/catalog.py` `[CHOCA]`, `app/static/admin-categories.js` `[CHOCA]`.

### OR-165 · La lista no muestra el plazo de retención del tipo
`app/static/admin-categories.js:44-48` · `app/static/admin.js:175-194`
**Hoy**: el plazo se edita en la pestaña Retención y no aparece en Tipos, aunque sea
un atributo del tipo. Escenario: se crea «Constancia de Reposo» y nada indica que
acaba de nacer con el plazo por defecto de 5 años, ni invita a revisarlo.
**Debe**: mostrar el plazo en la lista y pedirlo en el formulario de creación, con el
valor por defecto visible.
**Esfuerzo**: S. **Archivos**: `app/static/admin-categories.js` `[CHOCA]`, `app/routes/admin/catalog.py` `[CHOCA]`.

### OR-166 · Los colores de las cuatro Partes están escritos a mano y no cuadran
`app/static/admin-categories.js:27` · `app/routes/hr.py:495-500`
**Hoy**: `admin-categories.js` usa `#0056b3 / #28a745 / #e67e22 / #dc3545` y el
reporte imprimible usa `#0d6efd / #198754 / #fd7e14 / #6f42c1` para las mismas cuatro
Partes. Dos paletas distintas para el mismo concepto, ninguna leída de los tokens
`--viz-*`, ninguna con variante para modo oscuro y ninguna comprobada por
`test_contraste.py` pese a llevar texto blanco encima.
**Debe**: un token por Parte en `styles.css`, leído por las dos pantallas.
**Esfuerzo**: S. **Archivos**: `app/static/styles.css` `[CHOCA]`, `app/static/admin-categories.js` `[CHOCA]`, `app/routes/hr.py`, `app/tests/test_contraste.py`.

### OR-167 · Las Partes están escritas cuatro veces en cuatro sitios distintos
`app/static/admin_hr.html:442-445` · `app/static/admin-categories.js:27` · `app/routes/hr.py:495-500` · `app/schema.sql`
**Hoy**: el `<select>` del formulario, el mapa de colores del listado, el mapa del
reporte y la tabla `categoria` repiten los mismos cuatro nombres y slugs. Escenario:
se decide renombrar la Parte III y hay que acordarse de cuatro lugares; el que se
olvide produce una etiqueta huérfana sin color y sin orden.
**Debe**: las Partes se leen de `/api/choices` y de la tabla `categoria`; ningún
literal repetido.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-categories.js` `[CHOCA]`, `app/routes/hr.py`, `app/routes/lookups.py`.

### OR-168 · Si no hay tipos agrupados, la lista degrada a un listado plano sin Parte
`app/static/admin-categories.js:31-37`
**Hoy**: cuando `tipos_por_parte` viene vacío se pinta la lista plana con un badge
gris «RRHH». Escenario: con los tipos mal clasificados por OR-008 el usuario ve una
lista sin estructura y sin nada que explique por qué ha desaparecido la organización
por Partes.
**Debe**: mostrar siempre las cuatro Partes, incluidas las vacías, y un grupo
explícito «Sin clasificar» con acción para asignarlos.
**Esfuerzo**: S. **Archivos**: `app/static/admin-categories.js` `[CHOCA]`.

### OR-169 · La creación no valida ni longitud ni duplicados en el cliente
`app/static/admin-categories.js:163-184`
**Hoy**: sólo se comprueba que el nombre no esté vacío; se puede enviar un nombre de
500 caracteres, o uno que difiera del existente en un espacio final. El `catch` de la
línea 181 descarta el mensaje del servidor.
**Debe**: longitud máxima, recorte, comprobación de duplicado en vivo contra la lista
ya cargada y propagación del error real.
**Esfuerzo**: S. **Archivos**: `app/static/admin-categories.js` `[CHOCA]`.

### OR-170 · La pestaña no tiene búsqueda ni estados de carga
`app/static/admin-categories.js:7-13`
**Hoy**: si `state.choices` aún no ha llegado, la lista dice «Sin tipologías activas»
—que es un mensaje de vacío, no de carga— y no vuelve a intentarlo cuando llegan.
Con treinta tipos repartidos en cuatro Partes tampoco hay búsqueda.
**Debe**: estado de carga distinto del vacío, reintento cuando lleguen las opciones y
un filtro por nombre.
**Esfuerzo**: S. **Archivos**: `app/static/admin-categories.js` `[CHOCA]`.

### OR-171 · «Taxonomías Activas» y «Tipología» conviven con «Tipos» en la pestaña
`app/static/admin_hr.html:432` · `:449` · `:457` · `app/static/admin_hr.html:152`
**Hoy**: la pestaña se llama «Tipos», el formulario «Nueva Categoría», el campo
«Nombre de la Tipología», el botón «Guardar Tipología» y la lista «Taxonomías
Activas» — cuatro palabras para una cosa, y «Categoría» además choca con la tabla
`categoria`, que en RRHH son las Partes.
**Debe**: «Tipo de documento» en todas partes, con «Parte» reservado a la
agrupación. El mismo criterio de `CLAUDE.md` que prohíbe «Tesauro».
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-categories.js` `[CHOCA]`.

### OR-172 · El botón amarillo de guardar tipología no es un botón de acción primaria
`app/static/admin_hr.html:448-450`
**Hoy**: `btn-warning` a ancho completo con `border-radius:8px` en línea, mientras el
botón de guardar del alta es `btn-success` y el de crear usuario es
`btn-outline-danger` con el mismo radio en línea. Tres acciones equivalentes con tres
colores distintos, ninguno de ellos el color primario, y el radio escrito a mano en
cada sitio.
**Debe**: una sola jerarquía de botones —primario, secundario, destructivo—
definida en `styles.css` y aplicada por clase, sin `style` en línea.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/styles.css` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`.

---

## I. Pestaña Papelera

### OR-173 · Las dos tablas no dicen cuándo caducan sus elementos
`app/static/admin_hr.html:514-565`
**Hoy**: la papelera es indefinida: nada se purga solo y nada indica desde cuándo
está cada elemento salvo la fecha. Escenario: la papelera acumula 2 000 documentos y
80 empleados a lo largo de los años y nadie se atreve a purgar porque no sabe qué es
seguro borrar.
**Debe**: política de retención de la papelera (por ejemplo 90 días), con la fecha de
purga automática visible por fila y aviso antes de que ocurra.
**Esfuerzo**: M. **Archivos**: `app/routes/trash.py` `[CHOCA]`, `app/static/admin_hr.html`, `vercel.json`.

### OR-174 · No hay búsqueda ni filtros en la papelera
`app/static/admin_hr.html:520-530` · `app/routes/trash.py:20-25`
**Hoy**: dos tablas paginadas de 20 en 20, sin buscador, sin filtro por quién borró
ni por fecha. Escenario: se busca un documento borrado la semana pasada entre 600 y
hay que pasar treinta páginas mirando.
**Debe**: búsqueda por nombre y cédula, filtro por autor del borrado y por rango de
fechas, y orden.
**Esfuerzo**: M. **Archivos**: `app/routes/trash.py` `[CHOCA]`, `app/static/admin_hr.html`, `app/static/admin-edit.js` `[CHOCA]`.

### OR-175 · No hay restauración ni purga en lote
`app/static/admin-edit.js:276-277` · `:310-311`
**Hoy**: dos botones por fila. Escenario: una importación equivocada mandó 300
documentos a la papelera y hay que restaurarlos uno a uno, con su confirmación cada
vez.
**Debe**: selección múltiple con «restaurar seleccionados» y «vaciar papelera», esta
última con confirmación reforzada.
**Esfuerzo**: M. **Archivos**: `app/static/admin-edit.js` `[CHOCA]`, `app/routes/trash.py` `[CHOCA]`.

### OR-176 · La purga de un empleado se confirma con un texto genérico
`app/static/admin-edit.js:336-338` (y su gemelo de empleados)
**Hoy**: «Esta acción es irreversible. ¿Continuar?» — el mismo texto que para un
documento. Escenario: se purga a una persona y con ella desaparecen su expediente
completo, su historial de cargos y sus versiones (OR-012), tras una frase que no
menciona ninguna de las tres cosas.
**Debe**: confirmación específica que enumere lo que se destruye y exija escribir la
cédula, como pide OR-012.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit.js` `[CHOCA]`.

### OR-177 · La papelera no dice por qué se borró nada
`app/routes/trash.py:55-60` · `app/routes/admin/docs.py:632-634`
**Hoy**: se guardan `deleted_at` y `deleted_by`, nunca el motivo. Escenario: se
encuentra en la papelera el expediente de un profesor activo y no hay forma de saber
si fue un error, un duplicado o una baja: la decisión de restaurar se toma a ciegas.
**Debe**: motivo obligatorio al borrar, mostrado en la papelera y en la auditoría.
**Esfuerzo**: S. **Archivos**: `app/main.py` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`, `app/routes/trash.py` `[CHOCA]`, `app/static/admin-edit-hr.js`.

### OR-178 · Restaurar y purgar no piden confirmación con el mismo criterio
`app/static/admin-edit.js:328-334` frente a `:336-343`
**Hoy**: restaurar no pregunta nada y purgar sí. Restaurar un empleado devuelve a la
plantilla y a la búsqueda pública a alguien que podía estar borrado a propósito, sin
ninguna confirmación.
**Debe**: confirmación en las dos, proporcional al efecto, y aviso si al restaurar
aparece un conflicto de cédula.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit.js` `[CHOCA]`.

### OR-179 · Restaurar un empleado puede chocar con una cédula ya existente
`app/routes/trash.py:147-154`
**Hoy**: el `UPDATE` no comprueba nada. Escenario: se borra a alguien, se vuelve a
crear con la misma cédula (nada lo impide, porque la búsqueda de existencia no filtra
borrados — OR-032), y al restaurar el original salta la violación de unicidad como un
500 crudo, o quedan dos expedientes de la misma persona.
**Debe**: comprobar el conflicto antes de restaurar y ofrecer fusionar.
**Esfuerzo**: M. **Archivos**: `app/routes/trash.py` `[CHOCA]`.

### OR-180 · Las dos tablas de la papelera no comparten estado de paginación
`app/static/admin-edit.js:242` · `app/static/admin_hr.html:534` · `:560`
**Hoy**: `_papeleraState` tiene tres claves (`archivo`, `rrhh`, `empleados`) y los
botones de RRHH llaman a `changePapeleraPage(±1,'rrhh')` y `(±1,'empleados')`. Los
botones no se deshabilitan nunca en los extremos —a diferencia del monitor
(`admin-monitor.js:91-92`)—, así que se puede avanzar a páginas vacías
indefinidamente y la única señal es la tabla en blanco.
**Debe**: deshabilitar en los límites y mostrar «Mostrando N–M de T», como pide
OR-129 para el monitor.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit.js` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-181 · La papelera de RRHH tiene dos tarjetas de colores distintos sin motivo
`app/static/admin_hr.html:515` · `:541`
**Hoy**: `card-danger` para documentos y `card-secondary` para empleados. El rojo
sugiere que una es más grave que la otra, cuando borrar a una persona arrastra más
consecuencias que borrar un papel. El panel de Archivo sólo tiene una tarjeta, así
que la simetría entre módulos también se rompe.
**Debe**: el mismo tratamiento visual para las dos, con el título distinguiendo el
contenido.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`.

### OR-182 · Al restaurar o purgar se recarga la papelera entera
`app/static/admin-edit.js:332` · `:342`
**Hoy**: `loadPapelera('rrhh')` vuelve a pedir las dos tablas y devuelve ambas a su
página actual, perdiendo el punto de lectura y produciendo un parpadeo. Con 20 filas
y dos peticiones por acción, restaurar diez documentos son veinte peticiones.
**Debe**: quitar la fila del DOM con una transición de salida y actualizar sólo los
contadores.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit.js` `[CHOCA]`.

---
## J. Pestaña Retención

La pestaña arranca vacía en RRHH por OA-001 (el filtro de scope busca `%rrhh%` y los
slugs son `parte-i` … `parte-iv`). Todo lo que sigue supone ese fallo ya corregido;
sin él, nada de esta sección se puede ni siquiera observar.

### OR-183 · La pestaña de RRHH no muestra vencimientos, sólo plazos
`app/static/admin_hr.html:470-495` frente a `app/static/admin_archive.html:432-482`
**Hoy**: en Archivo la pestaña tiene la tabla de plazos **y** la de documentos
vencidos con su botón de disposición; en RRHH sólo la de plazos. `loadAdminTab`
llama a `loadVencimientosTable()` en las dos (`admin.js:21`), pero
`#vencimientos-table-body` no existe en `admin_hr.html`, así que la función sale por
el `return` de la línea 125. Escenario: se configura que los reposos se conservan
tres años y no hay ninguna pantalla que diga cuáles ya cumplieron el plazo.
**Debe**: la tabla de vencimientos también en RRHH, alimentada por
`/api/rrhh/alertas/documentos_vencidos` — que existe, funciona y **no la llama
nadie** (`hr_alerts.py:88`).
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/admin.js` `[CHOCA]`, `app/routes/hr_alerts.py`.

### OR-184 · El endpoint de vencidos de RRHH consulta la tabla de Archivo
`app/routes/hr_alerts.py:94-115`
**Hoy**: `/api/rrhh/alertas/documentos_vencidos` está en el router de RRHH,
documentado como alerta de RRHH, y su `FROM` es `public.datos_archivo`. Escenario:
si alguien lo conecta a la pestaña (OR-183), el panel de RRHH mostrará documentos del
Archivo institucional como si fueran del expediente de personal.
**Debe**: consultar `datos_rrhh` con el empleado asociado, o retirar el endpoint del
router de RRHH.
**Esfuerzo**: S. **Archivos**: `app/routes/hr_alerts.py`, `app/tests/test_hr.py`.

### OR-185 · Un plazo se guarda de uno en uno, sin aviso de cambios sin guardar
`app/static/admin.js:175-194` · `:201-218`
**Hoy**: cada fila tiene su `<input number>` y su botón de disquete. Escenario: se
ajustan los quince plazos de las cuatro Partes, se pulsa guardar en dos, se cambia de
pestaña y los trece restantes se pierden sin ningún aviso.
**Debe**: guardado en bloque con un botón único, marca visible de fila modificada y
aviso al salir con cambios pendientes.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`.

### OR-186 · El plazo no dice sobre qué fecha se cuenta
`app/static/admin_hr.html:471-475`
**Hoy**: el texto explica que el plazo «define cuántos años se conserva cada tipo
documental», sin decir desde cuándo. En RRHH la respuesta correcta casi nunca es la
fecha del documento: un contrato se conserva desde el fin de la relación laboral, no
desde su firma; una evaluación, desde el cierre del periodo.
**Debe**: cada tipo declara su hecho disparador (fecha del documento · egreso ·
jubilación) y el cálculo lo respeta.
**Esfuerzo**: M. **Archivos**: `app/main.py` `[CHOCA]`, `app/routes/admin/retention.py`, `app/static/admin_hr.html`.

### OR-187 · La retención ignora que un expediente de personal casi no prescribe
`app/routes/admin/retention.py` · `app/static/admin_hr.html:473-474`
**Hoy**: el plazo por defecto es 5 años y el máximo admitido, 100 (`admin.js:204`).
Un expediente laboral en Venezuela debe conservarse mientras dure la relación y
décadas después, por prescripción de prestaciones y por prueba de tiempo de servicio
para la pensión; para muchos tipos la respuesta correcta es «permanente», que no se
puede expresar.
**Debe**: admitir «conservación permanente» como valor, y valores por defecto por
Parte alineados con la LOTTT y la ley de archivos, no un 5 genérico.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/retention.py`, `app/static/admin.js` `[CHOCA]`, `app/main.py` `[CHOCA]`.

### OR-188 · No hay disposición documental en RRHH
`app/static/admin.js:226-256`
**Hoy**: `abrirDisposicion()` y `_elegirDisposicion()` —con su acta obligatoria y sus
tres salidas— existen y sólo se invocan desde la tabla de vencimientos de Archivo,
que en RRHH no existe (OR-183). El módulo que más necesita dejar constancia de qué
se hizo con un papel de una persona es el que no tiene la función.
**Debe**: la misma disposición en RRHH, con acta obligatoria, y prohibida sobre
documentos de personal activo.
**Esfuerzo**: M. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/routes/admin/retention.py`.

### OR-189 · El campo de plazo no valida mientras se escribe
`app/static/admin.js:180-182` · `:201-206`
**Hoy**: `min="1" max="100"` en el marcado, y la comprobación real llega al pulsar
guardar con un toast. Escenario: se teclea 500, se pulsa guardar, sale un toast que
desaparece, y el campo se queda con 500 en pantalla — un valor que no está guardado
pero se lee como si lo estuviera.
**Debe**: validación en vivo, borde de error y el valor revertido si no se acepta.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`.

### OR-190 · La confirmación de guardado dura dos segundos y no dice qué cambió
`app/static/admin.js:213-214`
**Hoy**: un toast «Plazo actualizado.» y `is-valid` durante 2 s. No se registra el
valor anterior en la interfaz ni hay historial de cambios de política de retención,
que es una decisión de gobernanza documental y no una preferencia.
**Debe**: mostrar «de 5 a 10 años» en la confirmación y guardar el cambio en
auditoría con ambos valores.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/routes/admin/retention.py`.

---

## K. Pestaña Auditoría

### OR-191 · El panel de RRHH muestra la auditoría de todo el sistema
`app/routes/admin/catalog.py:116-149` · `app/static/admin-users.js:110`
**Hoy**: `/audit_log` no acepta filtro de módulo y el cliente no lo pide. Escenario:
el admin de RRHH ve los eventos de Archivo, los cambios de configuración del
asistente y las copias de seguridad; y al revés, cualquier admin de Archivo ve —en su
propia pestaña— los eventos que nombran empleados en el detalle.
**Debe**: filtrar por módulo según el rol, con un selector para el admin Global.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/catalog.py` `[CHOCA]`, `app/static/admin-users.js` `[CHOCA]`.

### OR-192 · La búsqueda de auditoría no mira el detalle
`app/routes/admin/catalog.py:124-128`
**Hoy**: el `ILIKE` recorre `accion` y `usuario`. El detalle —donde están el
`empleado_id`, la cédula y el tipo de documento— no se busca. Escenario: se quiere
saber quién tocó el expediente 412 y no hay forma de buscarlo.
**Debe**: incluir `detalle` y `modulo` en la búsqueda, con índice de texto completo.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/catalog.py` `[CHOCA]`, `app/main.py` `[CHOCA]`.

### OR-193 · No hay filtro por fecha, por tipo de evento ni por resultado
`app/static/admin-ui.js:425-461`
**Hoy**: un único cuadro de texto en la cabecera del panel inyectado. Escenario: «qué
pasó el martes por la tarde» exige paginar de 50 en 50 hacia atrás hasta encontrarlo.
**Debe**: rango de fechas, selector de tipo de evento, filtro por resultado y por
usuario, todos combinables.
**Esfuerzo**: M. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/routes/admin/catalog.py` `[CHOCA]`.

### OR-194 · La auditoría no se puede exportar
`app/static/admin-ui.js:425-461`
**Hoy**: sólo se puede leer en pantalla, de 50 en 50. Un registro de auditoría sirve
para entregarlo — a una comisión, a control interno, a la propia persona cuyo
expediente se consultó.
**Debe**: exportación CSV/PDF del conjunto filtrado, y la propia exportación
registrada como evento.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/catalog.py` `[CHOCA]`, `app/static/admin-users.js` `[CHOCA]`.

### OR-195 · El detalle se recorta y sólo se puede leer con el ratón encima
`app/static/admin-users.js:134`
**Hoy**: `max-width:200px` con `text-overflow:ellipsis` y el texto completo en el
`title`. Escenario: en móvil o con teclado, el detalle —que es el contenido real del
evento— es inaccesible.
**Debe**: fila desplegable con el detalle completo, formateado.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`.

### OR-196 · No se registra qué cambió, sólo que cambió
`app/routes/admin/docs.py:604` · `app/routes/hr_alerts.py:216-221`
**Hoy**: «Update Empleado — ID: 412 nombres=Susana». No hay valor anterior ni lista
de campos tocados. Escenario: alguien cambió la fecha de jubilación de un profesor y
la auditoría no permite saber cuál era antes ni reconstruirla.
**Debe**: registrar el diff (campo, antes, después) en cada escritura sobre datos
personales, que es lo que convierte la auditoría en reconstrucción.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/database.py` `[CHOCA]`, `app/main.py` `[CHOCA]`.

### OR-197 · La columna «Resultado» pinta «OK» cuando no hay dato
`app/static/admin-users.js:135` · `app/static/admin-users.js:124`
**Hoy**: `escHtml(r.resultado || "OK")` y `colorResult` sólo reconoce `Success` y
`Failure`. Escenario: un evento sin `status` se muestra como «OK» en gris; un fallo
registrado como `error` en minúscula sale también en gris. La columna afirma un
éxito que nadie ha registrado.
**Debe**: «—» cuando no hay dato, un conjunto cerrado de resultados y color con
icono, no sólo color.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`, `app/database.py` `[CHOCA]`.

### OR-198 · El estado de la paginación de auditoría es global y compartido
`app/static/admin-users.js:6` · `:142-146`
**Hoy**: `auditState` es una variable global única, y `changeAuditPage` no recibe el
sufijo del módulo. Escenario: en una sesión de admin Global que ha visitado los dos
paneles, la página de auditoría queda compartida entre ellos y al volver al primero
se aterriza donde lo dejó el segundo. Además `loadAuditTab` no muestra estado de
carga y la tabla queda con los datos viejos mientras llega la nueva página.
**Debe**: estado por módulo, esqueleto de carga y desactivación de los botones
mientras la petición está en vuelo.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`.

---

## L. Pestaña Acceso

### OR-199 · La contraseña se muestra como ocho puntos, como si fuera un dato
`app/routes/admin/users.py:179` · `app/static/admin-users.js:26`
**Hoy**: el endpoint inventa `d["password"] = "••••••••"` y la tabla dedica una
columna entera a mostrarlo. No es información: es una columna de ruido que además
sugiere que el sistema conoce la contraseña.
**Debe**: eliminar la columna y usar ese espacio para lo que sí importa — último
acceso, intentos fallidos, si debe cambiar la clave (OR-048).
**Esfuerzo**: S. **Archivos**: `app/routes/admin/users.py` `[CHOCA]`, `app/static/admin-users.js` `[CHOCA]`.

### OR-200 · El botón de activar/desactivar no confirma nada
`app/static/admin-users.js:30-35` · `:56-66`
**Hoy**: el badge verde «Activo» **es** el botón que desactiva la cuenta, sin
confirmación. Escenario: alguien pulsa el badge creyendo que filtra o que es una
etiqueta y deja fuera del sistema a un compañero en mitad de su jornada; el único
aviso es un toast que se va.
**Debe**: acción explícita fuera del badge, con confirmación que nombre a la persona
y explique el efecto.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`.

### OR-201 · Los parámetros de `confirmModal` van cambiados en el borrado de usuario
`app/static/admin-users.js:70`
**Hoy**: `confirmModal("¿Eliminar al usuario "X"?", "Esta acción es irreversible.",
"Eliminar", "danger")`. El cuarto argumento debe ser una clase completa
(`btn-danger`), porque se aplica como `` `btn ${btnClass} ds-cm-ok` ``
(`admin-ui.js:81`). Con `"danger"` el botón queda `btn danger ds-cm-ok`: sin color de
fondo, sin borde y sin el rojo que señala que la acción destruye una cuenta. Además
la pregunta va en el título y la advertencia en el cuerpo, al revés que en el resto
de confirmaciones del panel.
**Debe**: `btn-danger`, título y cuerpo en el orden habitual, y `confirmModal`
normalizando la clase por si acaso.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`.

### OR-202 · Eliminar un usuario borra su fila y deja su rastro colgando
`app/routes/admin/users.py:235-243` · `app/routes/admin/helpers.py:133-143`
**Hoy**: `DELETE FROM usuarios_sistema`. `datos_rrhh.creado_por` y `updated_by` son
enteros que apuntan a esa tabla, y `_resolve_user_id` cae al «primer usuario por id»
cuando no encuentra a alguien. Escenario: se borra a quien cargó 2 000 documentos y
todos quedan apuntando a un id inexistente; el siguiente ingreso hecho por un usuario
no reconocido se atribuye al usuario número 1.
**Debe**: desactivar en vez de borrar, y si se borra, conservar el nombre en las
filas. `_resolve_user_id` nunca debe inventar un autor: debe fallar.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/users.py` `[CHOCA]`, `app/routes/admin/helpers.py` `[CHOCA]`.

### OR-203 · La tabla de usuarios no tiene búsqueda, orden ni paginación
`app/static/admin-users.js:14-50`
**Hoy**: se pintan todos de golpe. Con veinte usuarios se aguanta; con doscientos,
la pestaña es una lista interminable sin forma de encontrar a nadie.
**Debe**: búsqueda, orden por columna, paginación y filtro por rol y por estado.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`.

### OR-204 · El formulario de alta de usuario no tiene etiquetas
`app/static/admin-ui.js:398-416`
**Hoy**: cuatro controles con `placeholder` como única etiqueta y sin `aria-label`;
para un lector de pantalla son «cuadro de edición» y dos listas sin nombre. Es el
mismo fallo de WCAG 3.3.2 que OR-154, en el panel inyectado que comparten los dos
módulos.
**Debe**: `<label>` real por control, y el título «Registrar Nuevo Usuario» asociado
al grupo con `aria-labelledby`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`.

### OR-205 · Crear usuario no limpia el rol ni avisa de la contraseña por defecto
`app/static/admin-users.js:161-164`
**Hoy**: tras crear se limpian usuario y contraseña, y el rol se queda en lo último
elegido. Escenario: se crean cinco usuarios seguidos; el primero era Admin y los
cuatro siguientes salen Admin sin que nadie lo note, porque el `<select>` conserva el
valor.
**Debe**: restablecer el formulario entero y mostrar un resumen de lo creado
(«susana · RRHH · Normal») antes de confirmar.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`.

### OR-206 · «Último Acceso» no distingue «nunca» de «no registrado»
`app/static/admin-users.js:20`
**Hoy**: `u.last_login ? ... : 'Nunca'`. La columna se rellena con
`substring(0,16).replace('T',' ')`, es decir en UTC y sin zona: para Venezuela son
cuatro horas de desfase, así que un acceso de las 20:00 aparece como medianoche del
día siguiente.
**Debe**: hora local con zona explícita, «hace N días» como texto primario, y «Nunca»
sólo cuando efectivamente no ha entrado.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`, `app/routes/admin/users.py` `[CHOCA]`.

### OR-207 · No hay roles intermedios: o Normal o Admin
`app/static/admin-ui.js:411-414`
**Hoy**: dos roles. En una dirección de RRHH hacen falta al menos cuatro perfiles
distintos: quien consulta, quien archiva, quien aprueba y quien administra el
catálogo y los usuarios. Hoy quien puede archivar puede también borrar expedientes y
purgar la papelera.
**Debe**: roles con permisos por operación, comprobados en el servidor (depende de
OR-043).
**Esfuerzo**: L. **Archivos**: `app/main.py` `[CHOCA]`, `app/routes/admin/deps.py` `[CHOCA]`, `app/routes/admin/users.py` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`.

### OR-208 · La tabla de usuarios no marca cuál es el propio
`app/static/admin-users.js:18-46`
**Hoy**: nada distingue la fila de quien está mirando. Combinado con OR-046, es fácil
desactivarse a uno mismo por descuido.
**Debe**: marcar la fila propia con «(tú)» y sin acciones destructivas.
**Esfuerzo**: S. **Archivos**: `app/static/admin-users.js` `[CHOCA]`.

---

## M. Pestaña Exportar y reporte del expediente

### OR-209 · La exportación de RRHH no incluye el historial de cargos
`app/static/admin.js:96-98`
**Hoy**: `tables = "empleados,datos_rrhh,rrhh_descriptores,tipo_documento"`.
`historial_cargos` queda fuera, igual que `categoria`, `cargos`, `departamentos` y
`estados_laborales` — que son las tablas donde viven los valores a los que apuntan
`cargo_id`, `departamento_id` y `estado_id`. Escenario: se descarga «el respaldo de
RRHH», se restaura en otro sitio y todos los empleados quedan con cargos y
departamentos que son números sin tabla, y sin carrera laboral.
**Debe**: la lista de tablas se deriva del módulo en el servidor, no se escribe a
mano en el cliente, e incluye todo lo necesario para reconstruir el módulo.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/routes/backup.py`.

### OR-210 · La pestaña Exportar no ofrece nada que no sea un JSON completo
`app/static/admin_hr.html:498-511`
**Hoy**: un botón, «Descargar backup RRHH (.json)». No hay exportación de la planta
en CSV o Excel, ni informe de expedientes incompletos, ni listado de jubilaciones
próximas, ni exportación del expediente de una persona — que son las cuatro salidas
que una dirección de RRHH pide de verdad.
**Debe**: catálogo de exportaciones por propósito, con formato elegible y con lo
exportado registrado en auditoría (OR-140).
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/routes/admin/docs.py` `[CHOCA]`, `app/static/admin.js` `[CHOCA]`.

### OR-211 · La descarga no dice cuánto pesa ni avisa de lo que contiene
`app/static/admin.js:100-113`
**Hoy**: se pide el blob entero en memoria y se dispara la descarga. Escenario: con
400 empleados y 6 000 documentos, el JSON son decenas de MB que el navegador acumula
en RAM sin barra de progreso; y en ningún momento se advierte de que ese archivo
contiene cédulas, RIF y fechas de nacimiento de toda la plantilla.
**Debe**: aviso de contenido sensible con confirmación, tamaño estimado, progreso y
descarga en flujo.
**Esfuerzo**: M. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/routes/backup.py`.

### OR-212 · El estado de la exportación se pierde y el error se pinta a medias
`app/static/admin.js:115` · `app/static/admin.js:26-27`
**Hoy**: en el fallo se escribe un `<span class="text-danger">` con sólo un icono y
después se le añade el mensaje con `append` sobre `querySelector("span")` — frágil y
sin contexto. Y `loadAdminTab("export")` limpia el estado al entrar, así que el
resultado de una descarga anterior desaparece al volver.
**Debe**: mensaje de error completo y persistente, con la hora, y un historial de
exportaciones del módulo.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`.

### OR-213 · El reporte del expediente no lleva firma, sello ni verificación
`app/routes/hr.py:513-592`
**Hoy**: el pie dice «Documento generado automáticamente … Confidencial. Solo para uso
oficial», y no hay firma, ni sello de la Dirección, ni número de folio, ni código de
verificación. Escenario: se entrega a un profesor para un trámite ante el IVSS y no
tiene ningún valor probatorio; cualquiera puede editar el HTML antes de imprimirlo.
**Debe**: identificador único del reporte, código de verificación (QR contra una URL
de comprobación), espacio de firma y sello, y registro de cada emisión.
**Esfuerzo**: M. **Archivos**: `app/routes/hr.py`, `app/main.py` `[CHOCA]`.

### OR-214 · El reporte no numera las páginas ni repite las cabeceras
`app/routes/hr.py:516`
**Hoy**: `@media print { @page{size:A4;margin:2cm} }` y nada más. Escenario: un
expediente de 60 documentos ocupa cinco hojas; las cabeceras de la tabla salen sólo
en la primera, las filas se parten por la mitad y las hojas no llevan número, así que
si se caen al suelo no se pueden reordenar.
**Debe**: `thead { display: table-header-group }`, `tr { break-inside: avoid }`,
contador de páginas «N de M» y el nombre y la cédula en el encabezado de cada hoja.
**Esfuerzo**: S. **Archivos**: `app/routes/hr.py`.

### OR-215 · El reporte no dice quién lo generó
`app/routes/hr.py:547`
**Hoy**: «Generado: 02/09/2026 14:31 · N.° Expediente: 412». Falta el usuario que lo
emitió, que es justo lo que hace falta cuando aparece una copia impresa donde no
debía.
**Debe**: usuario emisor en el encabezado y evento de auditoría por cada emisión
(depende de OR-052).
**Esfuerzo**: S. **Archivos**: `app/routes/hr.py`.

### OR-216 · El reporte no muestra la foto ni la antigüedad
`app/routes/hr.py:549-562`
**Hoy**: doce campos en una cuadrícula de tres columnas, todos en crudo: las fechas
en ISO, ningún valor derivado y ninguna fotografía, aunque `foto_url` exista. Un
expediente impreso sin foto no identifica a nadie.
**Debe**: foto en el encabezado, antigüedad y edad calculadas junto a sus fechas, y
fechas en formato local.
**Esfuerzo**: S. **Archivos**: `app/routes/hr.py`.

### OR-217 · El reporte no indica qué falta en el expediente
`app/routes/hr.py:563-570`
**Hoy**: dos recuadros, «Documentos» y «Secciones», y la lista de lo que hay. Nada
sobre lo que debería haber y no está — que es la razón por la que se imprime un
expediente antes de una jubilación o un concurso.
**Debe**: sección «Documentos faltantes por Parte», derivada de la lista de
obligatorios (OR-072 y OR-256).
**Esfuerzo**: M. **Archivos**: `app/routes/hr.py`, `app/routes/admin/stats.py` `[CHOCA]`.

### OR-218 · Los colores del reporte no sobreviven a la impresión
`app/routes/hr.py:495-500` · `:527`
**Hoy**: las cabeceras de Parte usan un fondo con opacidad (`{col}18`), las cajas de
cifras son azul sólido con texto blanco y no hay `print-color-adjust: exact`.
Escenario: en la impresora en blanco y negro de la facultad —el caso normal— las
cajas azules salen como rectángulos grises con texto casi ilegible y las cabeceras de
Parte se pierden.
**Debe**: pensar el reporte para blanco y negro (peso tipográfico, reglas, sangrías)
y añadir color como refuerzo, con `print-color-adjust: exact` donde importe.
**Esfuerzo**: S. **Archivos**: `app/routes/hr.py`.

---

## N. Modales, foco y teclado

### OR-219 · Al cerrar un modal el foco se pierde
`app/static/admin-edit-hr.js:42` · `:71`
**Hoy**: `$("#editEmpleadoModal").modal("show"/"hide")` sin guardar ni restaurar el
elemento que abrió el diálogo. Escenario: con teclado, se llega al botón de editar de
la fila 12, se abre, se guarda, y el foco vuelve al `<body>`: hay que tabular desde
el principio de la página para retomar el trabajo (WCAG 2.4.3).
**Debe**: guardar el disparador al abrir y devolverle el foco al cerrar, en los
cuatro modales de la página.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/static/admin-edit-hr.js`.

### OR-220 · El modal de confirmación no lleva el foco al botón seguro
`app/static/admin-ui.js:71-84`
**Hoy**: `confirmModal` muestra el diálogo y no enfoca nada; `promptModal` sí enfoca
su campo tras 300 ms (`admin-ui.js:115`), un retardo arbitrario que falla si la
animación tarda más. Escenario: aparece «¿Eliminar?» y una pulsación de Intro va al
último elemento enfocado, que puede ser el propio botón de borrar de la fila.
**Debe**: foco inicial en «Cancelar», ciclo de tabulación cerrado, e Intro asociado
sólo al botón enfocado; usar el evento `shown.bs.modal` en lugar de `setTimeout`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`.

### OR-221 · El diálogo de disposición se construye a mano y sin accesibilidad
`app/static/admin.js:259-293`
**Hoy**: un `<div class="modal fade show">` inyectado con
`style="display:block;background:rgba(0,0,0,.5)"`, sin `role="dialog"`, sin
`aria-modal`, sin trampa de foco, sin `aria-label`, con el fondo escrito en línea
—invisible al modo oscuro y a los once temas— y con el foco puesto en la primera
opción, que es una acción, no un punto de entrada seguro.
**Debe**: usar la misma infraestructura que `confirmModal`, con roles, foco y estilos
de la hoja.
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OR-222 · Ctrl+S se secuestra aunque no haya nada que guardar
`app/static/admin-ui.js:174-181`
**Hoy**: el manejador hace `preventDefault()` **antes** de comprobar si hay un modal
abierto. Escenario: se pulsa Ctrl+S en la pestaña Expedientes para guardar la página
y no ocurre nada en absoluto: ni se guarda la página ni se guarda nada del panel, sin
ninguna señal.
**Debe**: comprobar primero y prevenir sólo si hay un guardado que ejecutar; y
anunciar el atajo en la interfaz del modal.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`.

### OR-223 · La navegación por flechas entre pestañas cambia de pestaña al pasar
`app/static/admin-ui.js:353-366`
**Hoy**: `ArrowRight`/`ArrowLeft` hacen `next.focus()` y **`next.click()`**, así que
recorrer las nueve pestañas con el teclado dispara la carga de las nueve: nueve
juegos de peticiones, incluido el Resumen con sus cuatro. El patrón ARIA de tablist
manual separa mover el foco de activar; el automático activa, pero entonces no debe
saltarse el `aria-selected`, que aquí no se actualiza en ningún momento.
**Debe**: elegir un patrón y aplicarlo entero, con `aria-selected`, `tabindex`
móvil y `aria-controls` correctos, y sin recargar paneles al pasar de largo.
**Esfuerzo**: M. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/static/admin_hr.html`, `app/static/admin.js` `[CHOCA]`.

### OR-224 · Las pestañas no declaran su estado a la tecnología asistiva
`app/static/admin_hr.html:132-185`
**Hoy**: los nueve enlaces llevan `role="tab"` y `class="active"`, pero ninguno tiene
`aria-selected` ni `aria-controls`, y los paneles no llevan `aria-labelledby`. Los
`<li class="ds-tab-sep">` con `aria-hidden` están bien resueltos; el estado, no.
**Debe**: `aria-selected` sincronizado con la clase, `aria-controls` al panel y
`aria-labelledby` de vuelta. `test_admin_panels.py` ya cuadra pestañas con paneles:
puede cuadrar también estos atributos.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin.js` `[CHOCA]`, `app/tests/test_admin_panels.py`.

### OR-225 · El cambio de pestaña no se anuncia
`app/static/admin.js:4-13`
**Hoy**: `loadAdminTab` cambia clases y carga datos; nada se anuncia por una región
viva y el foco no se mueve al panel nuevo. Escenario: con lector de pantalla se pulsa
«Expedientes» y no hay ninguna indicación de que el contenido haya cambiado ni de
que se estén cargando 25 filas.
**Debe**: mover el foco al panel (con `tabindex="-1"`) y una región `aria-live`
educada que anuncie «Expedientes, 412 registros».
**Esfuerzo**: S. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-226 · El panel no tiene ningún atajo de teclado propio
`app/static/admin-ui.js:164-183`
**Hoy**: sólo Escape y Ctrl+S. Quien pasa el día archivando expedientes no tiene «/»
para buscar, ni «n» para un ingreso nuevo, ni números para saltar de pestaña, ni
página siguiente con teclado. Es el equivalente de BR-179 dentro del backoffice,
donde el volumen de uso es mayor.
**Debe**: un conjunto pequeño y documentado de atajos, con una ayuda accesible con
«?».
**Esfuerzo**: M. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`, `app/static/ayuda.html`.

---
## O. Estética y sistema visual

### OR-227 · `btn-xs` no existe y todos los botones de acción salen a tamaño completo
`app/static/admin-monitor.js:154-156` · `app/static/admin-edit-hr.js:238` · `app/static/admin-users.js:30`
**Hoy**: quince botones del panel de RRHH usan `btn-xs`, una clase de Bootstrap 3 que
4.6 no define y que `styles.css` no declara: `grep btn-xs styles.css` no devuelve
nada. Es OA-183 y aquí afecta a las tres columnas de acciones (monitor, historial,
usuarios) y a las dos tablas de la papelera. La clase `.ds-tbl-btn`
(`styles.css:2716`), que hace exactamente ese trabajo, existe y **no la usa nadie**.
**Debe**: sustituir `btn-xs` por `.ds-tbl-btn` en todo el módulo y una guarda que
rechace clases sin regla propia ni de Bootstrap.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin-edit-hr.js`, `app/static/admin-users.js` `[CHOCA]`, `app/static/admin-edit.js` `[CHOCA]`, `app/tests/test_static_assets.py`.

### OR-228 · Estilos escritos en línea por todo el panel
`app/static/admin_hr.html:577` · `:614` · `:712-714` · `app/static/admin-monitor.js:147-152` · `app/static/admin-ui.js:155` · `:205` · `:258` · `:320`
**Hoy**: radios, sombras, colores, anchos y bordes escritos en `style="…"` y en
`cssText` desde JavaScript. `border-radius:14px`, `box-shadow:0 18px 40px
rgba(16,24,40,0.2)`, `background:#f8f9fa`, `#adb5bd`, `#333`: valores concretos que no
salen de ningún token y que el modo oscuro, la densidad compacta y los once temas no
pueden alcanzar — lo dice `CLAUDE.md`: «un color en un `style` en línea no lo arregla
ninguna hoja de estilos». Es el nudo que BR-109 identifica en el dossier, dentro del
backoffice.
**Debe**: mover todo a `styles.css` con tokens; ningún color ni radio literal en HTML
ni en JS.
**Esfuerzo**: L. **Archivos**: `app/static/styles.css` `[CHOCA]`, `app/static/admin_hr.html`, `app/static/admin-ui.js` `[CHOCA]`, `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin-edit.js` `[CHOCA]`.

### OR-229 · La página redefine el radio de las tarjetas en su propio `<style>`
`app/static/admin_hr.html:17-22`
**Hoy**: cuatro reglas sueltas en la cabecera del HTML —incluidas dos con
`!important`— que fijan `border-radius: 0.5rem` en `.card` e `.info-box`, el color
activo de las pastillas y el hover de las tablas. Mientras tanto los modales usan
`14px` en línea (OR-228) y los botones `8px`. Tres radios distintos en la misma
pantalla, ninguno tomado de un token.
**Debe**: una escala de radios en `styles.css` (`--ds-radius-sm/md/lg`) y el `<style>`
de la página vacío.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/styles.css` `[CHOCA]`.

### OR-230 · Los estilos del esqueleto y del menú rápido viven al final del `<body>`
`app/static/admin_hr.html:912-919`
**Hoy**: un segundo `<style>` entre dos `<script>`, con `.ds-skeleton`, su
`@keyframes` y tres reglas del menú de estado rápido, todas con colores literales y
sin variante oscura. En modo oscuro el esqueleto sigue siendo un degradado gris claro
que destella sobre fondo oscuro.
**Debe**: llevarlo a `styles.css` con tokens y variante para `body.dark-mode`, y
respetar `prefers-reduced-motion` en el destello.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/styles.css` `[CHOCA]`.

### OR-231 · Cinco familias de tarjeta de color compiten en la misma pestaña
`app/static/admin_hr.html:364` · `:426` · `:455` · `:477` · `:499` · `:515` · `:541`
**Hoy**: `card-info` (monitor y exportar), `card-warning` (nueva tipología),
`card-secondary` (lista de tipos, retención, papelera de empleados), `card-danger`
(papelera de documentos y control de acceso) y `card-primary` (formulario de alta).
El color de la tarjeta no codifica gravedad ni tipo: es decoración heredada de
AdminLTE, y como AdminLTE no se carga, lo que se ve depende de lo que `styles.css`
haya reimplementado para cada una.
**Debe**: una sola familia neutra para los contenedores y el color reservado a
estados —igual que `.ds-chart-card` es neutro a propósito.
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/styles.css` `[CHOCA]`.

### OR-232 · Los tamaños de letra se fijan a mano en veinte sitios
`app/static/admin_hr.html:310` · `:322` · `:325` · `:389` · `:405` · `:414` · `:484` · `app/static/admin-monitor.js:151` · `app/static/admin-users.js:15`
**Hoy**: `0.9rem`, `0.85rem`, `0.82rem`, `0.8rem`, `0.78rem`, `0.75rem`, `0.73rem`,
`0.72rem`, `0.7rem`, `0.68rem`, `0.65rem`, `0.6rem` — doce tamaños distintos escritos
en línea, algunos con dos decimales de diferencia entre sí. No hay escala tipográfica
y la densidad compacta (`styles.css`, sección de personalización) no puede actuar
sobre ninguno.
**Debe**: una escala de cuatro o cinco pasos en tokens, aplicada por clase.
**Esfuerzo**: M. **Archivos**: `app/static/styles.css` `[CHOCA]`, `app/static/admin_hr.html`, `app/static/admin-monitor.js` `[CHOCA]`.

### OR-233 · Los iconos de las pestañas no siguen ningún criterio
`app/static/admin_hr.html:136` · `:141` · `:148` · `:153` · `:158` · `:165` · `:170` · `:177` · `:182`
**Hoy**: nueve iconos de tres familias visuales distintas (`gauge-high`,
`plus-circle`, `id-card`, `sitemap`, `trash-alt`, `calendar-check`, `history`,
`users-cog`, `download`) con pesos y densidades muy distintos; `sitemap` para «Tipos»
y `id-card` para «Expedientes» no comunican lo que hay dentro, y `history` para
Auditoría choca con `clock-rotate-left` que el KPI usa para «Último ingreso».
**Debe**: un solo estilo de icono, revisado uno a uno contra lo que nombra la
pestaña, y ningún icono repetido con dos significados.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin_archive.html` `[CHOCA]`.

### OR-234 · Los KPI no tienen estado de carga propio y muestran una raya
`app/static/admin_hr.html:57` · `app/static/admin-charts.js:126`
**Hoy**: el valor inicial es «—» y `_marcarCargando()` sólo actúa sobre los
`<canvas>`, no sobre las tarjetas. Escenario: durante los tres o cuatro segundos que
tarda `/charts`, las ocho tarjetas muestran una raya que se lee como «no hay dato»,
no como «cargando» — que es exactamente el problema que el comentario de
`admin-stats.js:8-10` dice haber resuelto para las gráficas.
**Debe**: esqueleto en las tarjetas mientras se espera, con la misma pieza que usan
las tablas.
**Esfuerzo**: S. **Archivos**: `app/static/admin-charts.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OR-235 · Sin foco visible propio en casi ningún control
`app/static/styles.css` (5 apariciones de `focus-visible` en 3 973 líneas)
**Hoy**: el panel se apoya en el anillo por defecto de Bootstrap, que en los botones
`btn-outline-*` sobre fondo claro es un halo azul de bajo contraste, y en los badges
convertidos en botón (`admin-monitor.js:67`, `border:none`) no se ve en absoluto.
Escenario: se recorre con teclado la fila de acciones y no se sabe en cuál de los tres
botones se está.
**Debe**: un estilo de foco propio, de contraste alto, aplicado a todo control
interactivo incluidos los badges pulsables, verificado con `test_contraste.py`.
**Esfuerzo**: M. **Archivos**: `app/static/styles.css` `[CHOCA]`, `app/tests/test_contraste.py`.

### OR-236 · Las acciones destructivas no se distinguen de las demás
`app/static/admin-monitor.js:154-156`
**Hoy**: los tres botones de la fila —ver, editar, borrar— son del mismo tamaño, con
el mismo peso, pegados y separados sólo por `mr-1`. El de borrar es
`btn-outline-danger`, un borde rojo fino que a 0,75 rem se distingue poco del gris.
Escenario: se apunta al lápiz, se pulsa un pelo a la derecha, y el expediente se va a
la papelera; la confirmación es lo único que lo evita.
**Debe**: separar la acción destructiva del grupo (menú de desbordamiento o separador
real), con un tratamiento visual claramente distinto.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OR-237 · Los avatares del personal no aparecen en el backoffice
`app/static/admin-monitor.js:147` · `app/static/hr.js:145-148`
**Hoy**: el buscador público muestra la foto o las iniciales sobre el color del
estado; el monitor del panel muestra sólo texto. En una tabla de personas, la cara es
el identificador más rápido y el que evita confundir a dos homónimos —que es
exactamente el riesgo que BR-004 describe.
**Debe**: avatar de 28 px en la primera columna, con iniciales como reserva, usando
la misma pieza que el buscador.
**Esfuerzo**: S. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OR-238 · El espaciado entre secciones se decide con clases sueltas
`app/static/admin_hr.html:52` · `:196` · `:231` · `:248` · `:273` · `:289` · `:515`
**Hoy**: `mb-3`, `mb-2`, `mt-3`, `mb-4`, `p-3`, `p-4`, `py-2 px-3` combinados sin
sistema: la pestaña Resumen separa sus filas de gráficas con `mt-3` y la papelera
separa sus tarjetas con `mb-4`, mientras las barras de importación usan `mb-2` y
`mb-3` consecutivos. El resultado es un ritmo vertical distinto en cada pestaña.
**Debe**: una escala de espaciado propia aplicada al contenedor de cada pestaña, sin
utilidades sueltas por elemento.
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/styles.css` `[CHOCA]`.

### OR-239 · Los badges dicen cosas distintas con la misma forma
`app/static/admin_hr.html:242` · `:253` · `:262` · `app/static/admin-monitor.js:150-151` · `app/static/admin-categories.js:47`
**Hoy**: `badge-light border text-muted` es una nota metodológica en las tarjetas de
gráfica («% con al menos un documento», «LOTTT»); en el monitor es el tipo de
documento; en Tipos, `badge-pill` de color es la palabra «Activa». Tres significados
—nota, dato y estado— con la misma forma visual.
**Debe**: una forma por significado: estado (color sólido), dato (contorno neutro) y
nota metodológica (texto pequeño, sin badge).
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin-categories.js` `[CHOCA]`.

### OR-240 · La miga de pan no navega y repite lo que dice el menú
`app/static/admin_hr.html:43-47` · `app/static/admin.js:32-33`
**Hoy**: «Panel de Control / Administración - RRHH» es texto plano, sin enlaces, y se
reescribe entero en cada cambio de pestaña sin incluir la pestaña actual. Escenario:
se está en «Retención» dentro de «Expedientes» y la miga sigue diciendo lo mismo que
al entrar; no dice dónde se está ni permite subir un nivel.
**Debe**: miga real con la pestaña actual como último tramo, o retirarla y dar ese
espacio al título de la pantalla.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/admin.js` `[CHOCA]`.

---

## P. Responsive, modo oscuro y densidad

### OR-241 · A 390 px la tabla de expedientes deja tres columnas y ninguna útil
`app/static/admin_hr.html:392-398` · `app/static/admin-monitor.js:146-157`
**Hoy**: cinco de las siete columnas llevan `ds-hide-sm`; en móvil quedan Empleado,
Estado y Acciones. Cédula, cargo, departamento, tipos y ubicación desaparecen, así
que la tabla no permite decidir nada: hay que abrir cada ficha. Los tres botones de
acción, a 0,75 rem, quedan por debajo del objetivo táctil de 44 px (WCAG 2.5.5).
**Debe**: en móvil, tarjetas apiladas con los datos que importan y acciones de tamaño
táctil, en lugar de una tabla mutilada.
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-monitor.js` `[CHOCA]`, `app/static/styles.css` `[CHOCA]`.

### OR-242 · El modal de empleado a 390 px es una columna de veinte campos
`app/static/admin_hr.html:790-899`
**Hoy**: cinco filas de `col-md-*` que se apilan enteras en móvil, más el bloque de
historial con su `input-group` de cuatro controles (OR-154). El `modal-body` no tiene
`max-height` ni scroll propio —a diferencia del dossier, que sí lo tiene
(`admin_hr.html:615`)—, así que en pantallas bajas el botón de guardar queda fuera de
la vista y hay que hacer scroll en la página completa con el fondo bloqueado.
**Debe**: `max-height` con scroll interno, pie fijo con las acciones y agrupación por
secciones plegables en móvil.
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/styles.css` `[CHOCA]`.

### OR-243 · Las gráficas quedan a media pantalla en tabletas
`app/static/admin_hr.html:218` · `:224` · `:232` · `:238` · `:249` · `:258`
**Hoy**: `col-md-6`, `col-md-7` y `col-md-5`, sin `col-lg-*` ni `col-sm-*`. A 768 px
justos las seis gráficas siguen en dos columnas de 360 px, donde las barras
horizontales de departamento no caben con su etiqueta; por debajo de 768 px pasan a
una sola columna a ancho completo, que es demasiado para una dona.
**Debe**: rejilla con puntos de corte pensados por gráfica, y anchura máxima para las
donas.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`, `app/static/styles.css` `[CHOCA]`.

### OR-244 · El modo oscuro no llega a la mitad del panel de RRHH
`app/static/styles.css:2815-2821` · `app/static/admin_hr.html:336` · `:712` · `:735`
**Hoy**: hay reglas oscuras para la barra de importación, la zona de arrastre y las
pestañas de administración, pero no para la zona de arrastre del modal de edición
(colores en línea `#adb5bd`/`#f8f9fa`, que además el `ondragover` reescribe a
`#e8f0fe`), ni para la vista previa (`bg-light border`), ni para el esqueleto
(OR-230), ni para los badges de estado (OR-135), ni para el diálogo de disposición
(OR-221). Escenario: en modo oscuro el modal de edición tiene tres rectángulos blancos
brillantes en mitad de la pantalla.
**Debe**: revisar el panel entero en modo oscuro y mover a la hoja todo lo que hoy va
en línea (OR-228).
**Esfuerzo**: M. **Archivos**: `app/static/styles.css` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-245 · La densidad compacta no afecta a nada de RRHH
`app/static/styles.css` (sección de personalización) · `app/static/admin_hr.html`
**Hoy**: el modo compacto actúa sobre reglas de la hoja, y las tablas de RRHH fijan su
tamaño con `style="font-size:0.85rem"` y su altura con paddings de Bootstrap escritos
en el marcado. Escenario: se activa la densidad compacta esperando ver más filas por
pantalla y la tabla de expedientes queda exactamente igual.
**Debe**: la densidad se aplica por variables que las tablas del panel consumen; sin
tamaños en línea (OR-232).
**Esfuerzo**: M. **Archivos**: `app/static/styles.css` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-246 · Con nueve pestañas y poco ancho no se ve dónde se está
`app/static/styles.css:2738-2746` · `app/static/admin-ui.js:336-350`
**Hoy**: por debajo de 576 px las pestañas no activas se reducen a su icono y sólo la
activa conserva el texto — una decisión bien resuelta. Pero entre 576 y 900 px las
nueve conservan el texto y la barra desborda con scroll horizontal, cuya única señal
es la clase `ds-has-overflow`, un degradado en el borde derecho que desaparece en
cuanto se llega al final aunque queden pestañas a la izquierda.
**Debe**: indicador de desbordamiento en los dos lados, y el punto de corte del modo
icono subido a donde de verdad deja de caber.
**Esfuerzo**: S. **Archivos**: `app/static/styles.css` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`.

---

## Q. Funcionalidad ausente frente a SuccessFactors, Workday, BambooHR y la normativa venezolana

Esta sección no habla de fallos sino de lo que el sistema no hace y un sistema de
gestión de personal educativo tiene que hacer. Casi todo es **L** y varias entradas
requieren una decisión institucional antes que una técnica.

### OR-247 · No existe el escalafón docente
`app/schema.sql` · `app/static/admin_hr.html:802-805`
**Hoy**: el cargo es una cadena libre en la tabla `cargos`. La carrera académica de la
UCV se organiza en categorías (Instructor · Asistente · Agregado · Asociado ·
Titular) con tiempos mínimos de permanencia, trabajo de ascenso y veredicto. El
sistema no sabe qué es una categoría, ni cuánto lleva alguien en la suya, ni cuándo le
toca ascender.
**Debe**: categoría como entidad con sus reglas de permanencia, y un aviso de
elegibilidad para ascenso análogo al de jubilación.
**Esfuerzo**: L. **Archivos**: `app/schema.sql` `[CHOCA]`, `app/main.py` `[CHOCA]`, `app/routes/hr_alerts.py`, `app/static/admin_hr.html`.

### OR-248 · No existe la dedicación
`app/schema.sql`
**Hoy**: no hay campo para Exclusiva, Tiempo Completo, Medio Tiempo o Convencional,
pese a que determina la remuneración, la carga docente y la compatibilidad de cargos.
**Debe**: dedicación como campo del empleado y del tramo de historial (OR-159), con
su histórico de cambios.
**Esfuerzo**: M. **Archivos**: `app/schema.sql` `[CHOCA]`, `app/main.py` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-249 · No se distingue al personal docente del administrativo y obrero
`app/schema.sql` · `app/routes/hr.py:545`
**Hoy**: todo el mundo es un `empleado` con un `cargo`, y el reporte se titula
«EXPEDIENTE DEL PERSONAL DOCENTE Y DE INVESTIGACIÓN» para cualquiera. Los tres tipos
de personal tienen contratación distinta, contrato colectivo distinto, escalafón
distinto y expediente distinto.
**Debe**: tipo de personal como campo, con Partes obligatorias, plazos de retención y
reporte propios para cada uno.
**Esfuerzo**: L. **Archivos**: `app/schema.sql` `[CHOCA]`, `app/main.py` `[CHOCA]`, `app/routes/hr.py`, `app/static/admin_hr.html`.

### OR-250 · No hay antigüedad calculada ni tiempo de servicio reconocido
`app/routes/admin/stats.py` · `app/routes/hr.py`
**Hoy**: sólo existe `fecha_ingreso`. No hay antigüedad calculada, ni suma de tramos,
ni reconocimiento de servicio previo en otra institución, ni descuento de permisos no
remunerados — que es como se calcula de verdad el tiempo de servicio para la
jubilación y la prima de antigüedad.
**Debe**: antigüedad como valor derivado y auditable, con tramos reconocidos y
descontados, mostrada en la ficha, el reporte y la exportación.
**Esfuerzo**: L. **Archivos**: `app/schema.sql` `[CHOCA]`, `app/routes/hr.py`, `app/routes/admin/stats.py` `[CHOCA]`.

### OR-251 · La fecha de jubilación se teclea a mano en vez de calcularse
`app/static/admin_hr.html:858` · `app/routes/hr_alerts.py:24-85`
**Hoy**: `fecha_jubilacion` es un campo que alguien escribe. Todo el sistema de
alertas —el KPI, el banner, el horizonte— cuelga de un dato manual que nadie valida
(OR-085) y que en la mayoría de las fichas estará vacío, con lo que las alertas
sencillamente no se disparan.
**Debe**: calcularla de edad y tiempo de servicio según la norma aplicable, con el
valor manual como excepción justificada y visible como tal.
**Esfuerzo**: L. **Archivos**: `app/routes/hr_alerts.py`, `app/schema.sql` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-252 · No hay gestión de vacaciones
`app/schema.sql`
**Hoy**: no existe el concepto. La LOTTT fija días hábiles por año con aumento por
antigüedad, bono vacacional y acumulación limitada; hoy eso se lleva en un Excel
fuera del sistema, y el expediente digital no sabe nada.
**Debe**: derecho anual calculado, solicitudes, aprobaciones, saldo, calendario y
constancia archivada automáticamente en la Parte III.
**Esfuerzo**: L. **Archivos**: `app/schema.sql` `[CHOCA]`, `app/main.py` `[CHOCA]`, nuevas rutas y pantallas.

### OR-253 · No hay permisos ni licencias
`app/schema.sql` · `app/static/admin_hr.html:444`
**Hoy**: la Parte III se llama «Permisos y Formación» y sólo puede contener papeles
sueltos: no hay entidad «permiso» con tipo, fechas, si es remunerado y si computa
para la antigüedad. Un año sabático, un permiso pre y post natal y una licencia no
remunerada son tres cosas con efectos distintos, y el sistema no distingue ninguna.
**Debe**: permisos como entidad con su catálogo de tipos, efecto sobre la antigüedad
y sobre el estado laboral, y su documento de respaldo.
**Esfuerzo**: L. **Archivos**: `app/schema.sql` `[CHOCA]`, `app/main.py` `[CHOCA]`, nuevas rutas y pantallas.

### OR-254 · No hay reposos ni control de asistencia
`app/schema.sql`
**Hoy**: un reposo médico sólo puede existir como un documento escaneado. No hay
fechas de inicio y fin, ni cómputo de días, ni relación con el IVSS, ni alerta de
reposos encadenados — que es justo el dato que una dirección de RRHH necesita seguir.
**Debe**: reposos con fechas, cómputo, estado y alerta; con el diagnóstico tratado
como dato de salud, con acceso restringido (OR-053).
**Esfuerzo**: L. **Archivos**: `app/schema.sql` `[CHOCA]`, `app/main.py` `[CHOCA]`, nuevas rutas y pantallas.

### OR-255 · No hay concursos ni procesos de ingreso
`app/schema.sql` · `app/static/admin_hr.html:442`
**Hoy**: la Parte I se llama «Ingreso y Contratación» y no hay ninguna entidad para el
concurso de credenciales o de oposición: convocatoria, jurado, aspirantes, veredicto y
designación. El motivo «Ascenso por concurso» es texto libre en el historial
(`admin_hr.html:889`).
**Debe**: el concurso como proceso con sus etapas y documentos, del que salen tanto la
designación como el tramo del historial.
**Esfuerzo**: L. **Archivos**: `app/schema.sql` `[CHOCA]`, `app/main.py` `[CHOCA]`, nuevas rutas y pantallas.

### OR-256 · No hay lista de documentos obligatorios por Parte
`app/routes/admin/stats.py:177-194`
**Hoy**: la cobertura se mide como «al menos un documento» (OR-072) porque no existe
ninguna definición de qué debe contener cada Parte. Es la pieza que falta para que el
sistema pueda decir «a este expediente le falta la declaración jurada de patrimonio»,
que es la pregunta que se le hace todos los días.
**Debe**: catálogo de documentos obligatorios por Parte y por tipo de personal, con
completitud calculada por expediente y una lista de faltantes accionable.
**Esfuerzo**: L. **Archivos**: `app/schema.sql` `[CHOCA]`, `app/main.py` `[CHOCA]`, `app/routes/admin/stats.py` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-257 · No hay indicador de completitud por expediente
`app/static/admin-monitor.js:146-157`
**Hoy**: la única señal es el KPI global «Sin documentos». No hay un porcentaje ni un
semáforo por persona, que es lo que convierte la lista en una cola de trabajo
priorizable. Es lo mismo que pide BR-149 desde el buscador, y el sitio natural para
resolverlo es este.
**Debe**: porcentaje de completitud por expediente en la tabla, ordenable y
filtrable, derivado de OR-256.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/static/admin-monitor.js` `[CHOCA]`.

### OR-258 · No hay evaluación de desempeño
`app/schema.sql`
**Hoy**: nada. Ni periodos, ni instrumentos, ni resultados, ni relación con el
ascenso. Es un módulo central en SuccessFactors, Workday y BambooHR, y en la UCV es
además requisito para el escalafón.
**Debe**: al menos el registro del resultado y su periodo, enlazado al documento de
respaldo y al tramo de escalafón.
**Esfuerzo**: L. **Archivos**: `app/schema.sql` `[CHOCA]`, nuevas rutas y pantallas.

### OR-259 · No hay organigrama ni estructura de la facultad
`app/schema.sql`
**Hoy**: `departamentos` es una lista plana de nombres creados sobre la marcha
(OR-039). No hay escuelas, institutos, cátedras ni jerarquía, ni relación de
supervisión entre personas.
**Debe**: estructura organizativa jerárquica, con adscripción del empleado a una
unidad y visualización del organigrama.
**Esfuerzo**: L. **Archivos**: `app/schema.sql` `[CHOCA]`, `app/main.py` `[CHOCA]`, nuevas pantallas.

### OR-260 · No hay plazas: la plantilla no se puede planificar
`app/schema.sql`
**Hoy**: existen personas, no cargos presupuestados. No se puede saber cuántas plazas
de Profesor Agregado hay, cuántas están ocupadas, cuántas se liberan con las
jubilaciones del año ni cuántas están en concurso.
**Debe**: plazas como entidad con su ocupación, y un informe de planta ocupada frente
a autorizada.
**Esfuerzo**: L. **Archivos**: `app/schema.sql` `[CHOCA]`, nuevas rutas y pantallas.

### OR-261 · No hay informes de planta ni de nómina
`app/static/admin_hr.html:498-511`
**Hoy**: la única salida agregada es un JSON de respaldo. Faltan los informes que se
piden cada mes: planta por departamento y categoría, altas y bajas del periodo,
jubilaciones previstas, distribución por dedicación, antigüedad media.
**Debe**: un generador de informes con parámetros, previsualización, exportación a
PDF y CSV y posibilidad de programarlos.
**Esfuerzo**: L. **Archivos**: nuevas rutas y pantallas, `app/routes/admin/stats.py` `[CHOCA]`.

### OR-262 · No hay ninguna conexión con nómina
`app/routes/admin/imports.py`
**Hoy**: el único puente con cualquier otro sistema es un CSV que no funciona
(OR-002). Los datos de personal se teclean dos veces: aquí y en nómina.
**Debe**: importación y conciliación periódica contra nómina, con informe de
diferencias por cédula, antes de plantearse ninguna integración en vivo.
**Esfuerzo**: L. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`, nuevas pantallas.

### OR-263 · El empleado no tiene ninguna vía de acceso a su expediente
`app/routes/hr.py` · `app/static/`
**Hoy**: el sistema es sólo para la dirección de RRHH. La LOTTT (art. 147) reconoce
el derecho del trabajador a conocer su expediente, y el propio `hr_alerts.py` lo cita
en su encabezado sin implementar nada.
**Debe**: acceso de sólo lectura del empleado a su expediente, con constancia de cada
consulta y un canal para solicitar la corrección de un dato.
**Esfuerzo**: L. **Archivos**: `app/routes/auth.py`, `app/routes/hr.py`, nuevas pantallas.

### OR-264 · No hay flujo de aprobación con varias etapas
`app/routes/admin/docs.py:454-476`
**Hoy**: cuatro estados y un `PATCH` que salta de cualquiera a cualquiera, sin
transiciones válidas, sin quién debe aprobar, sin cola de trabajo y sin motivo de
rechazo. Escenario: un documento pasa de «rechazado» a «aprobado» directamente y nada
lo registra como excepción.
**Debe**: máquina de estados con transiciones permitidas, responsable por etapa,
motivo obligatorio en el rechazo y bandeja de pendientes (OR-122).
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/static/admin-ui.js` `[CHOCA]`.

### OR-265 · No hay notificaciones a personas
`app/routes/admin/catalog.py:152-198`
**Hoy**: `/notifications` devuelve los documentos en borrador y revisión, y nadie lo
llama desde el panel de RRHH. No hay correo, ni aviso al aprobador, ni recordatorio de
una jubilación próxima a quien debe preparar el expediente.
**Debe**: notificaciones por evento con destinatario y canal, empezando por las
alertas de jubilación y las aprobaciones pendientes.
**Esfuerzo**: L. **Archivos**: nuevas rutas, `app/routes/admin/catalog.py` `[CHOCA]`.

### OR-266 · No hay tareas ni listas de comprobación de proceso
`app/static/admin_hr.html`
**Hoy**: el sistema guarda documentos, no procesos. Una incorporación, una jubilación
o un egreso son secuencias de diez o quince pasos con responsables y plazos, y hoy
viven en la cabeza de quien los lleva.
**Debe**: plantillas de proceso (onboarding, jubilación, egreso) que generen tareas
con responsable y fecha, y que archiven cada documento en su Parte al completarse.
**Esfuerzo**: L. **Archivos**: `app/schema.sql` `[CHOCA]`, nuevas rutas y pantallas.

### OR-267 · No hay campos definibles por la institución
`app/schema.sql` · `app/static/admin_hr.html:790-868`
**Hoy**: la ficha del empleado tiene exactamente los campos que alguien decidió, y
añadir uno exige una migración, un cambio en `models.py`, otro en el modal y otro en
el reporte. Toda plataforma de RRHH del mercado permite campos propios.
**Debe**: definición de campos adicionales por tipo de personal, con su tipo de dato,
validación y visibilidad por rol.
**Esfuerzo**: L. **Archivos**: `app/schema.sql` `[CHOCA]`, `app/models.py` `[CHOCA]`, `app/static/admin_hr.html`.

### OR-268 · Un documento no puede pertenecer a más de un expediente ni a más de una Parte
`app/schema.sql` (tabla `datos_rrhh`)
**Hoy**: `empleado_id` y `id_tipo_documento` son únicos por fila. Un acta de Consejo
que designa a cinco profesores tiene que cargarse cinco veces, y un mismo documento no
puede figurar a la vez en la Parte I y en la Parte II.
**Debe**: relación N:N entre documento y expediente, con el archivo digital
almacenado una sola vez.
**Esfuerzo**: L. **Archivos**: `app/schema.sql` `[CHOCA]`, `app/main.py` `[CHOCA]`, `app/routes/hr.py`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-269 · No hay OCR ni búsqueda dentro de los documentos
`app/routes/files.py` · `scanner-app/`
**Hoy**: la búsqueda del panel recorre nombres, cédulas y notas; el contenido de los
PDF escaneados es opaco. Existe una aplicación de escáner en el repositorio y ninguna
extracción de texto.
**Debe**: OCR en la subida, texto indexado y búsqueda dentro del expediente, con
resaltado de coincidencias.
**Esfuerzo**: L. **Archivos**: `app/storage.py`, `app/routes/admin/docs.py` `[CHOCA]`, `app/main.py` `[CHOCA]`, `scanner-app/`.

### OR-270 · El expediente no tiene índice ni foliación
`app/routes/hr.py:465-478`
**Hoy**: los documentos se agrupan por Parte y se ordenan por fecha. Un expediente de
personal físico está foliado —cada pieza tiene su número correlativo— y el digital
debería reproducirlo para que ambos se puedan cotejar.
**Debe**: foliación por expediente, índice imprimible con folios y comprobación de
huecos en la secuencia.
**Esfuerzo**: M. **Archivos**: `app/schema.sql` `[CHOCA]`, `app/routes/hr.py`, `app/routes/admin/docs.py` `[CHOCA]`.

### OR-271 · No hay firma electrónica de ningún documento
`app/routes/admin/docs.py` · `app/routes/hr.py:513-592`
**Hoy**: los documentos son ficheros subidos y el reporte es HTML imprimible
(OR-213). Nada está firmado ni sellado en el tiempo, así que nada es oponible.
**Debe**: firma electrónica o, como mínimo, sello de integridad con huella del
archivo, fecha y usuario, verificable después.
**Esfuerzo**: L. **Archivos**: `app/storage.py`, `app/routes/admin/docs.py` `[CHOCA]`, `app/main.py` `[CHOCA]`.

### OR-272 · No se puede fusionar dos expedientes duplicados
`app/routes/admin/docs.py`
**Hoy**: con OR-016 los duplicados son inevitables y no hay ninguna operación para
unirlos: documentos, historial de cargos y datos personales de dos fichas de la misma
persona no se pueden juntar.
**Debe**: detección de duplicados (cédula normalizada, nombre y fecha de nacimiento) y
una fusión asistida que preserve todo y quede auditada.
**Esfuerzo**: L. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, nuevas pantallas.

### OR-273 · No hay política de conservación de datos personales tras el egreso
`app/routes/trash.py` · `app/routes/admin/retention.py`
**Hoy**: quien se retira permanece indefinidamente con todos sus datos, incluidos los
sensibles. La retención se define por tipo documental, nunca por persona.
**Debe**: política de anonimización o de archivo histórico transcurrido el plazo tras
el egreso, distinta de la retención documental y ejecutable con constancia.
**Esfuerzo**: L. **Archivos**: `app/routes/admin/retention.py`, `app/main.py` `[CHOCA]`, nuevas rutas.

### OR-274 · No hay vista de expediente lado a lado con el documento
`app/static/admin_hr.html:574-609`
**Hoy**: el modal de documento tiene un panel de «Miniatura» que sólo pinta un icono
y una etiqueta de formato (`admin_hr.html:584-590`); para ver el archivo hay que
abrirlo en otra pestaña. Quien coteja un expediente necesita ver el papel y sus
metadatos a la vez.
**Debe**: visor incrustado (PDF e imagen) junto a los metadatos, con navegación entre
los documentos del expediente.
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-edit.js` `[CHOCA]`, `app/routes/files.py`.

### OR-275 · No hay historial de cambios visible por expediente
`app/routes/admin/catalog.py:116-149`
**Hoy**: la auditoría es una lista global filtrable por texto (OR-192). No hay «ver el
historial de este expediente» desde la ficha, que es como se pide de verdad.
**Debe**: pestaña «Historial» en la ficha, con los cambios de ese expediente y su
diff (depende de OR-196).
**Esfuerzo**: M. **Archivos**: `app/routes/admin/catalog.py` `[CHOCA]`, `app/static/admin-edit-hr.js`.

### OR-276 · No hay panel de dirección con lo que hay que hacer hoy
`app/static/admin_hr.html:192-267`
**Hoy**: el Resumen son ocho cifras y seis gráficas de composición. Ninguna responde a
«qué tengo que hacer hoy»: expedientes incompletos que caducan, jubilaciones a
preparar, documentos esperando aprobación, importaciones fallidas.
**Debe**: bloque de trabajo pendiente arriba del Resumen, con listas accionables, por
encima de la analítica descriptiva.
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-charts.js` `[CHOCA]`, `app/routes/admin/stats.py` `[CHOCA]`.

### OR-277 · No hay vistas guardadas ni informes personales
`app/static/admin-monitor.js`
**Hoy**: cada consulta se reconstruye a mano cada vez (OR-131). Quien revisa cada
lunes «los activos de Química sin la Parte II» tiene que rehacer los filtros —cuando
existan (OR-124)— semana tras semana.
**Debe**: vistas guardadas con nombre, compartibles por enlace y exportables.
**Esfuerzo**: M. **Archivos**: `app/static/admin-monitor.js` `[CHOCA]`, `app/main.py` `[CHOCA]`, nuevas rutas.

### OR-278 · La ayuda no cubre el backoffice de RRHH
`app/static/ayuda.html` · `app/static/admin_hr.html`
**Hoy**: existe `ayuda.html` y ninguna pestaña del panel enlaza a la parte que le
corresponde; tampoco se explica en pantalla qué es cada Parte, qué significa cada
estado del flujo de trabajo ni qué implica purgar. Es BR-180 dentro del backoffice,
donde las consecuencias de equivocarse son mayores.
**Debe**: ayuda contextual por pestaña, con enlace desde la cabecera de cada una.
**Esfuerzo**: M. **Archivos**: `app/static/ayuda.html`, `app/static/admin_hr.html`.

---
## R. Rendimiento, concurrencia y deuda técnica

### OR-279 · El KPI de documentos trae el expediente completo a memoria
`app/routes/admin/stats.py:23` · `app/routes/hr.py:53-103`
**Hoy**: `POST /api/admin/stats` con `modulo=RRHH` ejecuta `fetch_hr_dataframe()` sin
filtros: un `SELECT` con seis `LEFT JOIN` y un `STRING_AGG` sobre **todos** los
empleados y **todos** sus documentos y descriptores, lo trae entero por la red y lo
carga en un DataFrame de pandas… para devolver dos números. Escenario: 400 empleados
con 6 000 documentos son decenas de miles de filas por cada entrada en la pestaña
Resumen, en una función serverless con memoria limitada y una base en otro continente.
**Debe**: dos `COUNT` en SQL. Y revisar si pandas hace falta en el camino caliente:
hoy `hr.py` lo importa sólo para esto y para `first_nonempty`.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/stats.py` `[CHOCA]`, `app/routes/hr.py`.

### OR-280 · La pestaña Resumen dispara tres peticiones caras en cada entrada
`app/static/admin.js:15` · `app/static/admin-stats.js:16-25`
**Hoy**: `/stats` (OR-279), `/charts` (ocho consultas agregadas) y
`/alertas/jubilaciones`, sin caché ni ventana de validez, cada vez que se entra —
incluida la vuelta automática tras cada alta (OR-091). Escenario: cargar veinte
documentos seguidos son sesenta peticiones agregadas además de las veinte de guardado.
Es OA-210 con el coste multiplicado por el DataFrame.
**Debe**: caché de corta duración por pestaña con invalidación al escribir, y un
endpoint de conteo ligero para el banner.
**Esfuerzo**: M. **Archivos**: `app/static/admin.js` `[CHOCA]`, `app/static/admin-stats.js` `[CHOCA]`, `app/routes/hr_alerts.py`.

### OR-281 · El monitor hace dos consultas pesadas por cada pulsación de tecla
`app/routes/admin/docs.py:143-178` · `app/static/app.js:294`
**Hoy**: sin rebote (OR-127), cada tecla lanza un `COUNT(DISTINCT e.id)` sobre cinco
`LEFT JOIN` y un `SELECT` con `GROUP BY` de trece columnas más dos `STRING_AGG`.
Sobre 400 empleados es tolerable; sobre 5 000 y con `ILIKE '%x%'` sin índice
utilizable, no. El pool es `ThreadedConnectionPool(1,5)`: cinco conexiones para toda
la aplicación.
**Debe**: rebote, cancelación, índices de búsqueda por trigrama y una consulta de
conteo separada y cacheable.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/main.py` `[CHOCA]`, `app/static/app.js` `[CHOCA]`.

### OR-282 · La importación hace hasta cinco viajes a la base por fila
`app/routes/admin/imports.py:86-117`
**Hoy**: por cada fila, tres `_resolve_or_create_lookup`, un `SELECT` de existencia y
un `INSERT`/`UPDATE`, cada uno con su commit. Escenario: 1 000 empleados son 5 000
viajes de ida y vuelta a Neon; a 80 ms cada uno son casi siete minutos, muy por encima
del tiempo máximo de una función de Vercel, y el proceso muere a mitad dejando la
importación aplicada a medias e irreversible (OR-102).
**Debe**: resolver los catálogos una sola vez en memoria, `execute_values` por lotes y
una transacción por lote.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/imports.py` `[CHOCA]`, `app/database.py` `[CHOCA]`.

### OR-283 · Dos usuarios que archivan a la vez pueden crear la misma persona dos veces
`app/routes/admin/docs.py:290-317`
**Hoy**: `SELECT` de existencia por cédula y luego `INSERT`, sin transacción ni
`ON CONFLICT`. Escenario: dos asistentes archivan documentos de la misma persona
nueva en el mismo segundo; los dos ven que no existe y los dos insertan. Uno recibe un
500 crudo por unicidad y su documento se pierde. Lo mismo en
`_resolve_or_create_lookup` y en la creación de cargos de `hr_alerts.py:190-194`.
**Debe**: `INSERT ... ON CONFLICT (cedula) DO NOTHING RETURNING id` con relectura, en
una transacción.
**Esfuerzo**: S. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/routes/admin/helpers.py` `[CHOCA]`, `app/routes/hr_alerts.py`.

### OR-284 · Sin control de concurrencia, el último en guardar gana siempre
`app/routes/admin/docs.py:557-605` · `app/routes/admin/docs.py:346-432`
**Hoy**: `updated_at` se escribe pero nunca se comprueba. Es OR-148 visto desde el
servidor y afecta también a los documentos y al historial de cargos: dos personas
trabajando sobre el mismo expediente se sobrescriben en silencio.
**Debe**: testigo optimista con `updated_at` en el `WHERE` y 409 al fallar, con la
interfaz mostrando el conflicto.
**Esfuerzo**: M. **Archivos**: `app/routes/admin/docs.py` `[CHOCA]`, `app/routes/hr_alerts.py`, `app/static/admin-edit-hr.js`.

### OR-285 · Dos copias de la misma función en dos archivos
`app/static/admin-monitor.js:180-270` y `app/static/admin-edit-hr.js:97-197`
**Hoy**: `exportAdminCSV` e `initDropZone` están duplicadas casi literalmente; gana la
última cargada (OR-005). Cualquier corrección a la exportación (OR-137, OR-138,
OR-139) hay que hacerla dos veces o dejará de aplicarse según el orden de los
`<script>`, que es implícito y frágil.
**Debe**: una sola definición, y una guarda estática de redefiniciones globales entre
estáticos.
**Esfuerzo**: S. **Archivos**: `app/static/admin-edit-hr.js`, `app/tests/test_static_analysis.py`.

### OR-286 · El panel de RRHH carga catorce estáticos sin `defer`
`app/static/admin_hr.html:911-940`
**Hoy**: jQuery, Bootstrap y doce archivos propios en secuencia, bloqueando el
análisis, con un orden implícito que importa (`admin-ui.js` antes que `app.js`,
`admin-monitor.js` antes que `admin-edit-hr.js` — y ese orden es justo el que produce
OR-005). Además el panel carga `hr.js` (el buscador público completo) sólo por el
dossier, y `scanner-client.js`, que aquí no se usa. Es OA-211 con dos estáticos más.
**Debe**: módulos ES con dependencias declaradas, o al menos `defer` y un empaquetado
mínimo; y no cargar el buscador entero en el panel.
**Esfuerzo**: L. **Archivos**: `app/static/admin_hr.html`, todos los `app/static/*.js` `[CHOCA]`.

### OR-287 · Cada `<script>` deja funciones sueltas en el objeto global
`app/static/admin-edit-hr.js` · `app/static/admin-monitor.js` · `app/static/admin.js`
**Hoy**: no hay módulos, ni espacios de nombres, ni `const` de módulo: todo son
funciones globales, `window._adminHistorialEmpId` incluido
(`admin-edit-hr.js:40`). Cualquier nombre repetido se pisa en silencio (OR-285) y
nada declara qué depende de qué; los comentarios de cabecera («Depende de: admin.js»)
son la única documentación de las dependencias.
**Debe**: módulos ES con importaciones explícitas.
**Esfuerzo**: L. **Archivos**: todos los `app/static/*.js` `[CHOCA]`.

### OR-288 · Diez `catch` vacíos o que descartan el error
`app/static/admin.js:71` · `:91` · `app/static/admin-monitor.js:73` · `app/static/admin-edit-hr.js:9` · `:242` · `app/static/admin-submit.js:238` · `:334` · `app/static/admin-categories.js:139` · `:158` · `:181` · `app/static/admin-users.js:51` · `:63` · `:79` · `:99`
**Hoy**: catorce puntos del panel de RRHH tragan la excepción o la sustituyen por un
mensaje genérico. Escenario: media pantalla deja de funcionar y la consola está
limpia; el usuario reporta «no va» y no hay nada que mirar.
**Debe**: un manejador único que registre, muestre el mensaje real y distinga red de
autorización de error de servidor; ninguna captura silenciosa.
**Esfuerzo**: M. **Archivos**: `app/static/app-core.js` `[CHOCA]`, todos los `admin-*.js` `[CHOCA]`.

### OR-289 · El sufijo del módulo se deduce de una cadena en cada función
`app/static/app-core.js:144-150` · usado 40 veces en `admin-*.js`
**Hoy**: `adminSuffixFromTab()` compara `state.activeTab === "admin-rrhh"` y devuelve
`"archivo"` como valor por defecto; `isArchivoModule()` compara
`state.user.modulo === "Archivo"`. Dos formas de responder la misma pregunta, con
resultados distintos si `state.user.modulo` y `state.activeTab` no coinciden — que es
lo que pasa con un admin Global, cuyo `modulo` lo fija `app.js:43` a partir del
`data-page`. Cualquier página nueva que no encaje en esas dos cadenas cae en la rama
de Archivo en silencio.
**Debe**: el módulo activo como estado explícito, fijado una vez al arrancar la
página, y las funciones recibiéndolo por parámetro.
**Esfuerzo**: M. **Archivos**: `app/static/app-core.js` `[CHOCA]`, todos los `admin-*.js` `[CHOCA]`.

### OR-290 · Un bloque muerto de veintiséis líneas en mitad del HTML
`app/static/admin_hr.html:333-358`
**Hoy**: una columna `display:none` con una tarjeta `display:none` dentro, un segundo
`<input type="file" id="file_upload-rrhh-legacy">`, un botón que lo abre y la lista de
últimos ingresos (OR-029). Es lo que rompe la zona de arrastre (OR-005) y lo que hace
invisible la lista de ingresos recientes.
**Debe**: borrarlo entero, moviendo antes «Últimos Ingresos» a un sitio visible.
**Esfuerzo**: S. **Archivos**: `app/static/admin_hr.html`.

### OR-291 · Campos del modal de documento que en RRHH no significan nada
`app/static/admin_hr.html:629-776`
**Hoy**: `admin_hr.html` incluye una copia entera del modal `editArchivoModal` de
Archivo —con Título, Autor/Ente Emisor, Clasificación, Folio, Soporte, Páginas,
Idioma y Palabras Clave— aunque en RRHH ninguno de esos campos se muestre en el
monitor y `update_documento` sólo escriba unos pocos en la rama RRHH
(`docs.py:414-432`). Y el modal ni siquiera es alcanzable desde el panel de RRHH
(OR-123): son 148 líneas de marcado inerte.
**Debe**: un modal de documento de RRHH con sus campos (Parte, tipo, fecha, notas,
ubicación, archivo, estado), y el de Archivo sólo en su página.
**Esfuerzo**: M. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-edit.js` `[CHOCA]`.

### OR-292 · `_resolve_user_id` inventa un autor cuando no encuentra al usuario
`app/routes/admin/helpers.py:133-143`
**Hoy**: si el nombre no está en `usuarios_sistema`, devuelve el id del primer usuario
de la tabla, y si no hay ninguno, un `1` literal. Escenario: cualquier ingreso hecho
con un nombre de usuario que no case exactamente queda atribuido al primer usuario del
sistema —normalmente el administrador—, y `datos_rrhh.creado_por` deja de significar
nada. Es la contraparte de OR-044 en la escritura.
**Debe**: fallar con 400 en vez de inventar; el autor sale de la sesión (OR-043).
**Esfuerzo**: S. **Archivos**: `app/routes/admin/helpers.py` `[CHOCA]`.

### OR-293 · Los mensajes de la interfaz están repartidos por diez archivos
`app/static/admin-submit.js:252-258` · `app/static/admin-monitor.js:95` · `app/static/admin-edit-hr.js:253` · `app/routes/admin/docs.py:288`
**Hoy**: cada texto se escribe donde hace falta, mezclando registros («Ingreso
guardado con éxito», «Error de conexión al registrar el folio», «Ningún archivo
coincide», «Cédula es requerida para RRHH»), con y sin punto final, y algunos en el
servidor y otros en el cliente. No hay glosario ni forma de revisarlos juntos.
**Debe**: un catálogo de mensajes con clave, revisable de una vez y reutilizable
entre los dos módulos.
**Esfuerzo**: M. **Archivos**: `app/static/app-core.js` `[CHOCA]`, todos los `admin-*.js` `[CHOCA]`, `app/routes/admin/` `[CHOCA]`.

### OR-294 · El panel usa jQuery sólo para abrir y cerrar modales
`app/static/admin_hr.html:911` · `app/static/admin-edit-hr.js:42`
**Hoy**: se carga jQuery 3.5.1 entero —bloqueando el análisis (OR-286)— porque los
modales de Bootstrap 4.6 se manejan con `$(el).modal(...)`. Es la única dependencia de
jQuery del proyecto y arrastra además la versión de Bootstrap.
**Debe**: al planificar la salida de Bootstrap 4.6, tratar los modales como el punto
de anclaje; mientras tanto, encapsular las cuatro llamadas en un helper propio para
que el cambio sea local.
**Esfuerzo**: L. **Archivos**: `app/static/admin_hr.html`, `app/static/admin-ui.js` `[CHOCA]`, `app/static/admin-edit-hr.js`.

### OR-295 · El aviso de sesión monta un temporizador por segundo que nunca se detiene
`app/static/admin-ui.js:213-219`
**Hoy**: al aparecer el aviso se crea un `setInterval` de 1 s que sólo se limpia
cuando la sesión ya ha caducado; si el usuario cierra el banner con la X
(`admin-ui.js:208`) el temporizador sigue corriendo contra un elemento que ya no
existe. Y `checkSession` corre cada 30 s durante toda la sesión.
**Debe**: limpiar el intervalo al cerrar el banner y usar `requestAnimationFrame` o un
temporizador que se detenga con la pestaña oculta.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`.

### OR-296 · El menú de estado rápido registra un listener global por apertura
`app/static/admin-ui.js:245-289`
**Hoy**: `openQuickStatusMenu` añade un `document.addEventListener("click", …, true)`
con `setTimeout(…, 10)` y lo retira sólo si el cierre ocurre por clic fuera. Si el
menú se cierra eligiendo una opción (`menu.remove()` en la línea 272), el listener de
captura queda vivo para siempre. Escenario: en una sesión larga se acumulan decenas de
manejadores que inspeccionan cada clic de la página.
**Debe**: retirar el listener en todas las salidas, o usar `AbortController` con
`signal`.
**Esfuerzo**: S. **Archivos**: `app/static/admin-ui.js` `[CHOCA]`.

---

## S. Pruebas que faltan

### OR-297 · Ninguna prueba cubre el backoffice de RRHH desde el navegador
`app/tests/`
**Hoy**: la suite es de servidor y de análisis estático. `test_admin_panels.py`
comprueba que cada pestaña tenga panel y que ambos módulos ofrezcan la misma
navegación, pero nada comprueba que el panel se **rellene**. Los fallos de esta
auditoría que sobrevivieron a la suite entera —el ojo del monitor que nunca abre nada
(OR-001), la zona de arrastre muerta (OR-005), la etiqueta del CSV que no cambia
(OR-026), los tooltips sin inicializar (OR-027), la papelera con las columnas
cambiadas (OR-009), `btn-xs` sin regla (OR-227)— son todos de esa clase. El propio
`CLAUDE.md` lo dice: «el frontend no se prueba solo mirando el código».
**Debe**: pruebas de navegador (Playwright) para las nueve pestañas en los tres roles,
con capturas de referencia a 390/768/1440 px en claro y oscuro, axe-core en cada
pestaña —no sólo en la carga inicial— y una guarda que rechace clases CSS sin regla.
**Esfuerzo**: L. **Archivos**: `app/tests/` (nuevo `test_backoffice_rrhh_e2e.py`), `requirements-dev.txt`.

### OR-298 · Ninguna prueba ejecuta una importación CSV de verdad
`app/tests/test_sql_inserts.py` · `app/routes/admin/imports.py`
**Hoy**: `test_sql_inserts.py` cuadra columnas contra valores y detecta columnas
`NOT NULL` omitidas —fue escrita precisamente por un fallo de la importación— y aun
así OR-002 y OR-003 están vivos: dos `INSERT` que omiten una columna `NOT NULL`.
Merece la pena averiguar por qué no los ve, porque lo que no vea seguirá pasando.
**Debe**: revisar la guarda hasta que falle con estos dos casos, y añadir pruebas de
extremo a extremo de la importación contra una base de prueba, con CSV de ejemplo que
incluyan los casos límite (columna ausente, valores vacíos, separador `;`, cabeceras
con acento, cédula duplicada, fila en la papelera).
**Esfuerzo**: M. **Archivos**: `app/tests/test_sql_inserts.py`, `app/tests/test_imports.py` (nuevo).

### OR-299 · No hay pruebas de autorización por rol
`app/tests/test_admin.py`
**Hoy**: nada comprueba que un usuario de Archivo no pueda escribir en RRHH, ni que un
Normal no pueda borrar un expediente, ni que un admin de RRHH no pueda cambiar la
contraseña de un Global. Mientras eso no se pruebe, OR-043, OR-045 y OR-046 volverán
en el siguiente refactor.
**Debe**: una matriz de rol × endpoint × resultado esperado, recorrida
automáticamente, que falle si aparece una ruta nueva sin entrada en la matriz.
**Esfuerzo**: M. **Archivos**: `app/tests/test_permisos.py` (nuevo), `app/routes/admin/deps.py` `[CHOCA]`.

### OR-300 · No hay pruebas de las reglas de negocio de RRHH
`app/tests/`
**Hoy**: no se prueba la coherencia de fechas (OR-085), ni el cierre de tramos del
historial (OR-038), ni el cálculo de las alertas de jubilación (OR-021 a OR-024), ni
la normalización de cédula y RIF (OR-016, OR-084). Son exactamente las reglas cuyo
fallo produce datos equivocados que nadie detecta a ojo, porque el resultado sigue
pareciendo una fecha y un número.
**Debe**: pruebas de las reglas de negocio con casos límite (bisiestos, tramos
solapados, fechas futuras, cédulas con formato exótico, empleado en papelera) contra
las funciones puras, sin base de datos.
**Esfuerzo**: M. **Archivos**: `app/tests/test_rrhh_reglas.py` (nuevo), `app/utils.py`.

---

## Cierre — orden de ataque por bloques

Seis bloques. El orden importa: cada uno apoya al siguiente, y hacerlos al revés
obliga a repetir trabajo.

**1. Lo que está roto y nadie ha reportado** — una semana.
OR-001 (el ojo del monitor no abre nada), OR-002 y OR-003 (las dos importaciones CSV
fallan en todas las filas), OR-005 (la zona de arrastre está muerta), OR-007 (cambiar
el tipo no cambia el tipo), OR-009 (la papelera enseña el tipo dos veces), OR-026 y
OR-027 (la barra de importación no informa de nada). Son funciones anunciadas que no
existen; hasta que estén, cualquier otra mejora se construye sobre una pantalla que
miente. Aquí entra también OA-001, sin el cual la pestaña Retención de RRHH sigue
saliendo vacía.

**2. Cerrar la puerta** — un fin de semana.
OR-043 (ningún endpoint comprueba rol ni módulo), OR-044 (el actor lo declara el
cliente), OR-045 y OR-046 (un admin de RRHH manda sobre el Global y sobre sí mismo),
OR-052 (consultar un expediente no deja rastro), OR-057 (el reporte imprime lo
borrado). Es el fichero de personal de una facultad y hoy lo protege JavaScript en el
navegador de quien mira. Nada de lo demás debería tocarse antes.

**3. Que los datos dejen de estropearse solos** — dos semanas.
OR-004 (un CSV borra nombres y RIF), OR-010 a OR-012 (borrar y purgar arrastran o
destruyen lo que no deben), OR-013 y OR-014 (el cargo y su historial son dos verdades
distintas), OR-015 a OR-017 (duplicados por cédula y por RIF), OR-031 a OR-034
(escrituras sobre la papelera, auditoría prematura, alta no transaccional), OR-283 y
OR-284 (concurrencia). Este bloque es el que decide si dentro de un año el padrón se
puede creer.

**4. Que las cifras cuadren** — una semana.
OR-018 a OR-025 (el KPI de documentos, el filtro que no filtra, las etiquetas
repetidas, las alertas que se pierden a quien más falta hace). Una pantalla donde dos
tarjetas contiguas dan cifras distintas del mismo hecho deja de usarse, y una vez
que deja de usarse ya no importa lo bien construido que esté el resto.

**5. Sacar los estilos del marcado** — dos semanas.
OR-227 (`btn-xs` no existe), OR-228 (estilos en línea por todas partes), OR-229 y
OR-230 (los dos `<style>` de la página), OR-232 (doce tamaños de letra), OR-135 y
OR-166 (colores de estado y de Parte escritos a mano). Es el nudo: mientras los
colores y los radios vivan en atributos `style`, el modo oscuro (OR-244), la densidad
(OR-245), los once temas y media docena de pendientes de accesibilidad no tienen dónde
engancharse. Hacerlo antes que cualquier trabajo estético, o se hará dos veces. Es la
misma conclusión a la que llega BR-109 desde el buscador, y conviene coordinar ambos.

**6. Convertirlo en un sistema de RRHH** — el resto.
OR-256 (lista de documentos obligatorios por Parte) y OR-257 (completitud por
expediente) son los dos que cambian la naturaleza de la herramienta con el menor
esfuerzo: transforman un depósito de papeles en una cola de trabajo priorizada, y de
ellos cuelgan OR-072, OR-071, OR-217 y OR-276. Después, OR-250 (antigüedad calculada),
OR-247 y OR-248 (escalafón y dedicación) y OR-117 (acciones en lote). Los demás de la
sección Q son proyectos con decisión institucional detrás y no deberían empezarse sin
ella.

Y en paralelo con todo, **OR-297**: mientras ninguna prueba mire la pantalla, todo lo
de esta auditoría puede volver en el siguiente cambio sin que nadie se entere. Los
seis fallos del bloque 1 sobrevivieron a una suite de más de trescientas pruebas.
