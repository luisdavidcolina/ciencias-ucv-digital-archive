# Auditoría — Recorrido visual (renderizado real)

Las siete auditorías anteriores leyeron **código**. Ésta es la primera que ha
**mirado la pantalla**: las diez páginas levantadas en un navegador de verdad,
las nueve pestañas de cada panel de administración activadas una a una, a 390,
768 y 1440 px, en claro y en oscuro, con densidad compacta, con las animaciones
apagadas y recorriendo la página con el tabulador. **No se ha implementado
nada**: esto sólo audita.

241 capturas en [`capturas/`](capturas/), más el volcado de diagnósticos
automáticos en [`capturas/_diagnostico.json`](capturas/_diagnostico.json)
(238 registros: consola, peticiones fallidas, ancho de scroll, elementos que se
salen del viewport, objetivos táctiles, cabeceras contra celdas de cada tabla,
imágenes rotas, iconos sin glifo y el recorrido del foco paso a paso).

---

## 1. Cómo se levantó el entorno (repetible)

No hay `.env` ni base de datos, así que **no se ha conectado con Neon ni se han
pedido credenciales**. Se sustituyó `database.db_query` por un generador de
filas verosímiles y se sirvió la aplicación FastAPI real —rutas reales, HTML
real, JavaScript real— en local.

```bash
# 1. servidor con la base de datos simulada (deja :8099 escuchando)
python docs/auditoria/capturas/_harness/fakedb.py

# 2. el recorrido completo: 10 páginas x 6 condiciones + 9 pestañas por panel
python docs/auditoria/capturas/_harness/capturar.py          # ~20 min

# 3. estados que no se alcanzan con una URL: modales, menú, panel de temas,
#    enlace compartido válido, estado vacío, estado de error, hover
python docs/auditoria/capturas/_harness/estados.py
```

Las tres piezas viven en [`capturas/_harness/`](capturas/_harness/) y no tocan
nada del árbol de la aplicación.

**Cómo funciona la base simulada** (`fakedb.py`): `db_query` no ejecuta SQL;
deduce las columnas que la consulta pide —los alias del `SELECT`, y el esquema
de `app/schema.sql` cuando el SQL usa `*`— y devuelve filas cuyo valor se elige
por el **nombre** de la columna: fechas para `fecha_*`, enteros para `*_count`,
cédulas para `cedula`, nombres con tildes y eñes para `nombre`/`autor`, y así.
El conjunto incluye a propósito **nombres largos con tildes**
(«Ana Cecilia Bermúdez de la Peña Castañeda», «Iñaki Etxebarría Goikoetxea»),
**títulos de 180 caracteres**, **campos vacíos**, **campos nulos**, **arrays**,
**cédulas** y **14 filas** por consulta, que es lo que hace visible el 80 % de
lo que sigue. Un `dict` con `__missing__` cubre cualquier columna que una ruta
lea y el SQL no declare, así que ningún endpoint revienta por una clave que
falte.

**Cobertura del simulador**: de los 76 endpoints `GET`/`POST` de la API, 73
responden 2xx/4xx normales; los tres que no son `/api/health`,
`/api/admin/backup/programado` (503 correcto sin `CRON_SECRET`) y
`POST /api/admin/documento/{id}/versiones` (subida de fichero), y ninguno de los
tres alimenta una pantalla de este recorrido.

**Sesión**: cookie `ds_session` firmada con `core.security.generate_session_token`
y `SECRET_KEY=auditoria-visual-key`, más `archive_session` en `localStorage`,
como administrador Global (`["Archivo","RRHH"]`, rol `Admin` en ambos). El
enlace compartido válido se firma con `generate_share_token("Archivo", 1, 72)`.

**Navegador**: Chromium 151 vía Playwright (`playwright` ya instalado en el
entorno de Python). Capturas de página completa, `device_scale_factor: 1`,
`locale: es-VE`.

**Nombres de fichero**: `<pagina>-<pestaña>-<ancho>-<tema>.png`; las condiciones
extra van como tema (`compacto`, `sinanim`, `foco`), y los estados como pestaña
(`modal-detalle`, `modal-edicion`, `modal-expediente`, `menu-abierto`,
`panel-temas`, `vacio`, `error`, `toasts`, `valido`, `hover-tarjeta`,
`datosbreves`, `tipologia-larga`, `soporte-largo`).

### Lo que NO se ha podido comprobar por no haber base de datos

- **Los datos reales.** Todo lo que aquí se dice sobre *contenido* («14
  documentos con retención vencida») es del simulador. Lo que se audita es la
  **forma**: qué hace la pantalla con textos largos, vacíos, nulos y muchas
  filas. Cuando un hallazgo depende de un valor concreto, se dice.
- **Rendimiento real.** Las latencias aquí son de milisegundos; con Neon en otro
  continente los estados de carga durarán mucho más, y varios de ellos (VI-046,
  VI-047) sólo se ven mientras dura la espera.
- **El asistente de IA en funcionamiento.** `OPENROUTER_API_KEY` es falsa: la
  burbuja se pinta pero no se pudo abrir una conversación real. El catálogo de
  modelos de `/admin/ia` sí es el real (lo descarga de OpenRouter).
- **Subida y previsualización de ficheros.** Sin R2 no hay PDF ni imagen que
  mostrar: el visor, la miniatura real y el flujo de versiones quedan sin ver.
- **La importación CSV, el backup y la restauración**, que escriben.
- **Los once temas de color y las cuatro escalas tipográficas.** El recorrido se
  hizo con el tema por defecto y tamaño normal; `sistema-diseno.md` ya los
  audita a fondo (SD-031…SD-038, SD-020).
- **Impresión y PDF de reportes.**

---

## 2. Cómo leer esto

- Cada pendiente lleva identificador `VI-xxx`, esfuerzo **S** (< 1 h), **M**
  (media jornada), **L** (más).
- **Dónde** dice la pantalla y la condición exacta en la que se ve; **Captura**
  es el fichero de `capturas/` que lo demuestra. Todas las capturas citadas se
  han mirado una a una.
- La columna **Toca** enumera los archivos que habría que abrir. Los marcados
  **[CHOCA]** tocan ficheros compartidos con otros carriles (`styles.css`,
  `app.js`, `app-core.js`, `app-shell.js`, `app-choices.js`, `app-theme.js`) y
  no deben lanzarse en paralelo con agentes que declaren esos mismos ficheros.
- Cuando el recorrido **confirma visualmente** un pendiente ya listado por otra
  auditoría, se cita por su identificador y **no se le da un `VI-` nuevo**; sólo
  aparece aquí lo que ninguna de las siete vio, o lo que cambia de gravedad al
  verse.

---

## 3. Resumen ejecutivo

**Lo primero: el Admin Global no existe en pantalla.** `/admin/sistema` se
pinta con la barra superior, la burbuja del asistente y **nada más**: página en
blanco, sin un solo error en consola, en las seis condiciones (VI-001). La
sección que contiene los KPIs, las copias de seguridad, la auditoría, las
alertas de jubilación y la retención lleva `class="app-tab-section"`, que
`app.js:177` oculta, y **ningún código vuelve a mostrarla** porque el `switch`
de `switchTab()` sólo conoce cuatro identificadores y esa sección no tiene
ninguno. Es el fallo más grave de todo el sistema y ninguna auditoría de código
lo vio: las nueve pestañas existen, sus paneles existen, `test_admin_panels.py`
pasa — y la pantalla está vacía.

**Lo segundo: el sistema de avisos no avisa.** `showToast()` busca
`#ds-toast-container`, que **sólo existe en los tres HTML de administración**.
En `/archivo` y `/rrhh` la función sale por la puerta de atrás sin pintar nada:
«Sesión expirada. Redirigiendo…», «No hay resultados para exportar» y la
confirmación de exportación son **invisibles** para el usuario (VI-002). Se
llamó a `showToast` tres veces seguidas en la pantalla de búsqueda y no apareció
nada: `archivo-toasts-1440-claro.png`.

**Lo tercero: el modo oscuro está a medias, y lo que falta es lo que enmarca.**
Las cabeceras de tarjeta («Filtros Académicos», «Vista»), la migaja de pan, la
barra superior, las cabeceras de panel del backoffice y la zona de subida de los
modales siguen **blancas o rosa claro** sobre el fondo oscuro, y su texto —
calculado para fondo claro — se vuelve ilegible: `rrhh-1440-oscuro.png`,
`admin_archivo-users-1440-oscuro.png`, `admin_archivo-modal-edicion-1440-oscuro.png`.
`sistema-diseno.md` lo predijo por lectura (SD-036, SD-037); aquí está la foto.

**Lo cuarto: el teclado entra en un pasillo invisible.** En `/archivo` el
tabulador pasa por el enlace de salto —que al recibir el foco queda a **y = −25 px**,
o sea fuera de la pantalla (VI-020)— y después por **diez enlaces del menú
lateral cerrado, todos a x = −290 px** (VI-021). Diez pulsaciones de Tab sin que
se mueva nada en pantalla, antes de llegar al primer filtro. `CLAUDE.md`
documenta el enlace de salto como resuelto.

**Y lo quinto, que ninguna auditoría de código puede juzgar: no parece un
producto, parece siete.** El botón principal es azul en Documentos, **verde** en
Ingresar, **amarillo** en Tipos, **rojo de peligro** en Acceso y **teal** en
Exportar (VI-048). El modal de documento tiene cabecera azul marino y el de
empleado **amarillo con texto blanco a 1,9:1** (VI-013). `/admin/ia` no lleva
barra superior ni menú, `/investigacion` no carga la hoja de estilos del sistema
y `/ayuda` tiene su propio CSS: tres de diez páginas están fuera del sistema
visual. Dentro de una misma pantalla conviven fechas `2019-01-01` y `01/01/2019`
(VI-050), un selector de fecha nativo del navegador y otro de flatpickr
(VI-051), y cinco nombres —Categoría, Tipología, Tipo, Taxonomía, Tipos— para el
mismo objeto (VI-049).

**Cifras del recorrido**: 4 condiciones con desborde horizontal real; 14 tablas
que se salen de su `.table-responsive` sin ninguna señal; 40 objetivos táctiles
por debajo de 44 px sólo en `/archivo` a 390 px; 7 «NaN» y 3 «null» visibles en
pantalla; 0 errores de consola en 238 renderizados —el sistema falla **en
silencio**, que es exactamente por qué hacía falta mirar.

Los siete trabajos que cambian la percepción del producto, por orden:
**VI-001** (el panel en blanco), **VI-002** (los avisos mudos),
**VI-010/VI-011/VI-012** (el modo oscuro a medias), **VI-020/VI-021** (el foco),
**VI-048** (un solo color de acción), **VI-031/VI-032** (los gráficos) y
**VI-005** (la etiqueta que se sale de la tarjeta y tapa la columna de filtros).

---

## 4. Fallos visibles — la pantalla está rota

| ID | Título | Dónde se ve | Captura | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|---|
| VI-001 | **El Admin Global se pinta en blanco**, en las seis condiciones | `/admin/sistema`, 390 · 768 · 1440, claro y oscuro | `admin_sistema-1440-claro.png`, `admin_sistema-390-oscuro.png` | Sólo se ven la barra superior, el nombre del usuario y la burbuja del asistente. El `<section class="app-tab-section">` de `admin_system.html:31` queda con `display:none` —lo aplica `app.js:177` a todas las secciones— y **nada lo vuelve a mostrar**: `switchTab()` sólo destapa `#tab-archivo`, `#tab-rrhh`, `#tab-admin-archivo` y `#tab-admin-rrhh`, y esa sección **no tiene `id`**. Cero errores en consola. Las cinco pestañas internas ni siquiera se pueden pulsar: los 30 intentos de clic del recorrido fallaron por elemento no visible. | Dar `id="tab-admin-sistema"` a la sección y mostrarla desde `checkSession()` (la página ya gestiona su sesión aparte), o dejar de cargar `app.js` en esa página. Y una prueba de humo que cargue cada ruta y afirme que el `content-wrapper` tiene contenido visible. | S | `admin_system.html`, `app.js` **[CHOCA]** |
| VI-002 | **Los avisos no se ven en los dos buscadores**: falta el contenedor de toasts | `/archivo` y `/rrhh`, cualquier condición | `archivo-toasts-1440-claro.png`, `archivo-toasts-1440-oscuro.png` | `showToast()` (`app-core.js:35`) hace `getElementById("ds-toast-container")` y **retorna si no existe**. El contenedor está declarado sólo en `admin_archive.html:586`, `admin_hr.html:626` y `admin_system.html:453`. En `/archivo` se dispararon tres toasts (éxito, error, aviso) y no apareció ninguno. Se pierden, entre otros, «Sesión expirada. Redirigiendo al inicio de sesión…» (`app.js:336`), «No hay resultados para exportar» (`app.js:383`) y la confirmación de exportación (`app.js:409`), más los dos de `archive.js` y los dos de `hr.js`. El usuario pulsa Exportar, no pasa nada visible, y no sabe si funcionó. | Que `app-shell.js` inyecte el contenedor junto con la barra y el menú —igual que hace con el enlace de salto—, para que exista en toda página que tenga cáscara. Y que `showToast` avise por consola cuando no encuentre dónde pintar, en vez de callarse. | S | `app-shell.js` **[CHOCA]**, `app-core.js` **[CHOCA]** |
| VI-003 | El panel de personalización no se cierra con `Escape` | Cualquier página con cáscara, 1440 | `archivo-toast-1440-claro.png` (el panel sigue abierto tras pulsar Escape) | Se abre el panel, se pulsa `Escape` y el panel sigue ahí; sólo cierra con la ✕. Es un cajón modal de 320 px que tapa el buscador y la primera columna de resultados. | `keydown` de `Escape` en `app-theme.js`, foco atrapado dentro mientras esté abierto y devuelto al botón al cerrar. | S | `app-theme.js` **[CHOCA]** |
| VI-004 | La burbuja del asistente se superpone al contenido y gana a todo | 1440 y 390, todas las páginas con cáscara | `archivo-1440-claro.png` (tapa los botones de acción de la 4ª tarjeta), `archivo-panel-temas-1440-claro.png` (tapa la muestra «Terracota»), `admin_archivo-monitor-768-claro.png` (tapa la tabla) | La burbuja es fija abajo a la derecha y queda **por encima del panel de personalización**, de la última fila de las tablas y de la columna de acciones de las tarjetas de resultado. En `/archivo` a 1440 tapa literalmente el botón «Ver» de una tarjeta. | Reservar espacio (padding inferior en el contenedor de contenido), bajar su z-index por debajo de paneles y modales, y ocultarla cuando haya un modal abierto. Empareja con SD-009 (la escala de z-index inexistente). | M | `ai-widget.css`, `ai-widget.js`, `styles.css` **[CHOCA]** |
| VI-005 | Una insignia de soporte de 40 caracteres **se sale de la tarjeta y tapa la columna de filtros** | `/archivo`, 1440 claro; se reprodujo con **un solo registro** de datos realistas | `archivo-soporte-largo-1440-claro.png`, `archivo-1440-claro.png` | La insignia de soporte se pinta dentro de `.ds-item-thumbnail`, una columna flex de ancho fijo, sin `max-width` ni recorte. Con `soporte = "Documento digitalizado en alta resolución"` la insignia crece a 340 px, **desborda la tarjeta por la izquierda**, cruza el borde de acento y se dibuja encima de la tarjeta «Tipología» del panel de filtros, tapando además el nombre del autor. `soporte` es texto libre en la base, sin catálogo ni longitud máxima. | `max-width:100%`, `overflow:hidden`, `text-overflow:ellipsis` en la insignia y en la columna de miniatura, y `title` con el valor completo. Y decidir si `soporte` es catálogo cerrado. | S | `archive.js`, `styles.css` **[CHOCA]** |
| VI-006 | Una tipología larga rompe el ancho de la página a 390 px | `/archivo`, 390 claro; con **un solo registro** | `archivo-tipologia-larga-390-claro.png` (documento de 558 px en un viewport de 390) | Con una tipología de 53 caracteres el documento mide **558 px de ancho en una pantalla de 390**: un 43 % de desborde horizontal, con la mitad derecha de cada tarjeta fuera del alcance. La insignia de tipología (`archive.js:180`) no envuelve ni recorta. Con datos cortos no ocurre: `archivo-datosbreves-390-claro.png` mide exactamente 390. | Recortar la insignia con elipsis y permitir el salto de línea del texto. Vale igual para `/rrhh`. | S | `archive.js`, `hr.js`, `styles.css` **[CHOCA]** |
| VI-007 | Con datos reales de longitud variable el desborde llega a las tres anchuras | `/archivo` 390 (694 px) y 768 (875 px), claro y oscuro | `archivo-390-claro.png`, `archivo-768-oscuro.png`, `_diagnostico.json` | El diagnóstico automático señala `div.ds-item-metadata` y `div.ds-item-actions` como los elementos que se salen. `.ds-item-actions` tiene `width:54px` fijo (44 px por debajo de 768) y `.ds-item-metadata` `flex-grow:1` sin `min-width:0`, que es lo que impide a un hijo largo encogerse en un contenedor flex. Es la causa estructural de VI-005 y VI-006. | `min-width:0` en `.ds-item-metadata` y `flex-wrap` en la tarjeta por debajo de 768, con las acciones abajo en fila. | S | `styles.css` **[CHOCA]** |
| VI-008 | La barra de nueve pestañas se corta a 768 y a 390 sin ninguna señal de que siga | `/admin/archivo` y `/admin/rrhh`, 768 y 390, ambos temas | `admin_archivo-monitor-768-claro.png`, `admin_archivo-monitor-390-oscuro.png` | A 768 se ven «Resumen · Ingresar · Documentos · Tipos · Papelera · Retención · Aud…» y el resto queda fuera del contenedor: **Auditoría, Acceso y Exportar son inalcanzables** salvo que el usuario adivine que la barra se arrastra. A 390 sólo quedan iconos, sin texto ni indicador. No hay flecha, ni sombra de borde, ni scrollbar visible. | Barra con desplazamiento explícito (sombras de borde a los lados y flechas), o desplegable «Más» a partir de cierto ancho. Los identificadores internos no cambian. | M | `admin_archive.html`, `admin_hr.html`, `styles.css` **[CHOCA]** |
| VI-009 | Catorce tablas se salen de su `.table-responsive` sin afordancia | Backoffice, todas las pestañas con tabla, 390 y 768 | `admin_archivo-users-390-claro.png`, `admin_rrhh-monitor-768-claro.png` | El diagnóstico mide `table.scrollWidth > wrapper.clientWidth` en 14 combinaciones (Acceso 711 px en 390 y en 768, Auditoría 604, Retención 523, Papelera 471, Documentos 425…). El recorte es limpio y silencioso: **la columna «Acciones» simplemente no está**, y nada indica que exista. Las cabeceras sí cuadran con las celdas en todos los casos (comprobado: `ths` = nº de celdas por fila en las 26 tablas medidas), así que no hay tabla corrida — hay tabla amputada. | Sombra de desplazamiento en el borde derecho del envoltorio, o vista de tarjetas por debajo de 768 con las acciones siempre visibles. | M | `styles.css` **[CHOCA]**, `admin-monitor.js`, `admin-users.js` |
| VI-010 | Las tres tablas de la consola de IA no están envueltas y se cortan a 390 | `/admin/ia`, 390 | `admin_ia-390-claro.png` | Ninguna de las tres tablas (`Últimos días`, `Cambios propuestos`, `Modelo`) lleva `.table-responsive`: a 390 px miden 346 y 494 px y se recortan contra el borde de la tarjeta. `CLAUDE.md` lo pide explícitamente («si una tabla puede no caber, envuélvela»). | Envolver las tres. | S | `admin_ai.html` |
| VI-011 | Los paneles de la consola de IA cortan la última fila **a media línea de texto** | `/admin/ia`, 1440 y 768 | `admin_ia-1440-claro.png` | «Gasto» y «Cambios propuestos» tienen altura fija con `overflow:hidden`: la última fila queda seccionada horizontalmente por la mitad —se lee la mitad superior de los caracteres— sin barra de desplazamiento ni «ver más». Parece una imagen rota, no un panel con más contenido. | Altura máxima con `overflow:auto` y desvanecido inferior, o paginación explícita. | S | `admin_ai.html` |
| VI-012 | El modal de detalle **no oscurece la página**: hay dos sistemas de modal en el producto | `/archivo`, modal abierto, 1440 y 390, ambos temas | `archivo-modal-detalle-1440-claro.png` vs `admin_archivo-modal-edicion-1440-claro.png` | El modal de documento de la búsqueda se pinta sobre la página **sin `modal-backdrop`**: los resultados de detrás se leen igual de nítidos y compiten con el contenido del modal. El de edición del backoffice sí trae backdrop y bloquea el `body` (`modal-open`). Dos comportamientos opuestos para la misma acción. | Un solo componente de modal, con fondo, bloqueo de scroll, foco atrapado y cierre con `Escape`. | M | `archive.js`, `hr.js`, `styles.css` **[CHOCA]** |
| VI-013 | La cabecera del modal de empleado es **amarilla con texto blanco**: 1,9:1 | `/admin/rrhh`, modal de expediente, 1440 y 390 | `admin_rrhh-modal-expediente-1440-claro.png` | «Editar Datos del Empleado» va en blanco sobre `#ffc107`. El mismo modal en Archivo tiene cabecera azul marino con blanco (`admin_archivo-modal-edicion-1440-claro.png`). Y el botón primario del modal —«Guardar Cambios»— es también amarillo, que en el resto del producto significa «editar» o «aviso». | Cabecera y botón con el color de acento del sistema; el amarillo sólo para aviso. Empareja con SD-023. | S | `admin-edit-hr.js`, `styles.css` **[CHOCA]** |
| VI-014 | La palabra «Miniatura» se pinta **una letra por línea** en el modal a 390 px | `/archivo`, modal de detalle, 390, claro y oscuro | `archivo-modal-detalle-390-claro.png` | La rejilla del modal (`.ds-doc-modal-grid`) no colapsa a una columna a 390: la columna de miniatura queda de ~12 px y su título se parte verticalmente, `M i n i a t u r a`, mientras la columna de metadatos parte cada valor en tres líneas («Sala 3 · Estante B · Caja 14»). El punto de corte existe (`styles.css:981`) pero no cubre esta anchura. | Una sola columna por debajo de 768, con la miniatura arriba. | S | `styles.css` **[CHOCA]** |
| VI-015 | La foto del empleado se recorta sin `object-fit` y sale ilegible | `/rrhh`, 1440 y 390, ambos temas | `rrhh-1440-claro.png`, `rrhh-1440-oscuro.png` | Una imagen que no sea cuadrada se muestra recortada por el centro del original, no escalada: en la captura, un logo institucional queda como un fragmento de letras («DAD/LTAI») dentro del círculo. Cualquier foto de carnet apaisada saldrá igual de mal. | `object-fit: cover; object-position: center top` en `.rrhh-person-photo` y respaldo con las iniciales cuando la carga falle. | S | `hr.js`, `styles.css` **[CHOCA]** |
| VI-016 | Un icono inexistente en FontAwesome 6.0 deja un hueco | `/admin/ia`, 390 claro (detectado por el diagnóstico de glifos) | `_diagnostico.json` (`iconosSinGlifo: ["fas fa-shield-halved"]`), `admin_ia-390-claro.png` | `fa-shield-halved` se añadió en FontAwesome **6.1**; las diez páginas cargan **6.0.0** desde CDN. El icono ocupa 0 px: hueco mudo. Es el mismo fallo que `BA-011` señala para `fa-scanner`, en otro fichero. | Sustituir por `fa-shield-alt`, y una guarda que valide los nombres de icono contra la versión cargada. | S | `admin_ai.html` |
| VI-017 | El logo del login se recorta a 390 px | `/login`, 390, claro y oscuro | `login-390-claro.png` | El `<img height="64">` del logotipo apaisado mide ~640 px de ancho: en 390 se corta a la mitad y se lee «UNIVERSIDAD CENTRAL DE V… FACULTAD DE CIE…». Es la primera pantalla del sistema. | `max-width:100%; height:auto` y, por debajo de 480 px, la versión reducida del logotipo. | S | `login.html` |
| VI-018 | El botón de mostrar contraseña mide 31 × 28 px | `/login`, 390 | `login-390-claro.png` | Único objetivo táctil de la pantalla de acceso, y está por debajo del mínimo de 44 px en las dos dimensiones. | 44 × 44 px de área activa aunque el icono siga pequeño. | S | `login.html`, `styles.css` **[CHOCA]** |
| VI-019 | La retención de RRHH no tiene la mitad que sí tiene Archivo | `/admin/rrhh` → Retención vs `/admin/archivo` → Retención, 1440 | `admin_rrhh-retencion-1440-claro.png` vs `admin_archivo-retencion-1440-claro.png` | En Archivo la pestaña trae **dos** bloques: los plazos y «Documentos con Retención Vencida» (con disposición por fila). En RRHH sólo está el primero, y hasta la frase de ayuda está truncada respecto a la de Archivo. La misma pestaña, con el mismo nombre, hace dos cosas distintas según el módulo. Relacionado con `OA-001`, que lo describe como panel vacío desde el código. | O RRHH tiene su bloque de vencidos, o la pestaña explica por qué no aplica. | M | `admin.js`, `admin_hr.html` |

---

## 5. Modo oscuro — lo que queda blanco

Las siete auditorías lo predijeron leyendo `styles.css`; este apartado es la
comprobación una a una, con la captura de cada rectángulo blanco.

| ID | Título | Dónde se ve | Captura | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|---|
| VI-020 | Las cabeceras de tarjeta de los dos buscadores quedan **blancas con texto blanquecino**: los rótulos desaparecen | `/rrhh` y `/archivo`, 1440 · 768 · 390 oscuro | `rrhh-1440-oscuro.png`, `archivo-1440-oscuro.png` | «Filtros», «Filtros Académicos» y «Vista» son barras casi blancas dentro de una tarjeta oscura, y su texto —gris calculado para fondo claro— queda **ilegible**: en la captura, «Vista» prácticamente no se ve. Los tres acordeones («Tipología», «Fecha», «Estado», «Palabras Clave») siguen igualmente en claro. La columna izquierda entera es una isla clara en una interfaz oscura. | Regla oscura para `.card-header` y para las cabeceras de acordeón; a medio plazo, tokens de superficie (SD-004, SD-036). | S | `styles.css` **[CHOCA]** |
| VI-021 | Las cabeceras de panel del backoffice son rectángulos claros —uno rosa— en modo oscuro | `/admin/archivo` → Acceso y Papelera, 1440 oscuro | `admin_archivo-users-1440-oscuro.png`, `admin_archivo-papelera-1440-oscuro.png` | «Control de Acceso» se pinta sobre un degradado rosa claro a blanco, y «Papelera de Reciclaje» igual: dos franjas luminosas de 1.360 px sobre `#1a1d23`. En `/admin/archivo` → Documentos la cabecera es cian claro. | Igual que VI-020: variante oscura de las cabeceras de panel. | S | `styles.css` **[CHOCA]** |
| VI-022 | La barra superior y la migaja de pan **no tienen modo oscuro** | Todas las páginas con cáscara, 1440 · 768 · 390 oscuro | `rrhh-1440-oscuro.png`, `admin_archivo-1440-oscuro.png` | La barra superior sigue blanca y la migaja gris claro: las dos primeras franjas de la pantalla, a plena anchura, contradicen el resto. El usuario que activa el modo oscuro ve un producto a medio pintar. | Reglas oscuras para `.ds-navbar` y `.ds-breadcrumb-wrapper`, que SD-037 ya enumera entre los componentes sin variante oscura. | S | `styles.css` **[CHOCA]** |
| VI-023 | La zona de arrastrar archivo del modal es un **rectángulo blanco puro** en oscuro | `/admin/archivo`, modal de edición, 1440 y 390 oscuro | `admin_archivo-modal-edicion-1440-oscuro.png` | `.ds-upload-zone` conserva su fondo blanco dentro de un modal oscuro: la zona más grande del formulario es la única blanca. | Variante oscura de la zona de subida y de su borde punteado. | S | `styles.css` **[CHOCA]** |
| VI-024 | En oscuro, **el dato de identidad es el menos legible de la fila** | `/archivo` y `/rrhh` 1440 oscuro; `/admin/archivo` → Documentos 390 oscuro | `archivo-1440-oscuro.png`, `admin_archivo-monitor-390-oscuro.png` | Autor («María José Rodríguez Peña»), cédula, departamento y cargo se pintan con el gris apagado pensado para fondo claro y quedan a ras de fondo, mientras la ubicación y las insignias sí se ven. En la tabla de Documentos a 390 el **título del documento** —la columna que identifica la fila— es lo menos legible de la pantalla. | Que el gris apagado se derive del fondo real en cada modo (SD-015 propone el mecanismo), y que el título de fila nunca use el gris apagado. | M | `styles.css` **[CHOCA]** |
| VI-025 | Los textos de ayuda de las cabeceras de panel desaparecen también **en claro** | `/admin/archivo` → Papelera, 1440 claro | `admin_archivo-papelera-1440-claro.png` | «Los documentos aquí pueden restaurarse o eliminarse permanentemente» está escrito en rosa claro sobre la cabecera rosa: sólo se adivina inclinando la pantalla. Es la única explicación de una pantalla con un botón de borrado irreversible. | Contraste AA sobre el fondo real de la cabecera. Empareja con SD-015. | S | `styles.css` **[CHOCA]** |
| VI-026 | El aviso de retención vencida es amarillo sobre amarillo en los dos temas | `/admin/archivo` y `/admin/rrhh` → Resumen, 1440 claro y oscuro | `admin_archivo-stats-1440-claro.png`, `admin_rrhh-modal-expediente-1440-claro.png` | El bloque de aviso mantiene fondo `#fff3cd` y texto `#856404` en modo oscuro, y en claro los tres enlaces de documento van en el mismo tono ocre que el texto normal: la única pista de que son pulsables es la negrita. | Estados semánticos como tokens con su par claro/oscuro (SD-023), y los enlaces con color de enlace y subrayado. | M | `styles.css` **[CHOCA]**, `admin-stats.js` |
| VI-027 | El texto secundario de `/ayuda` en oscuro queda por debajo de lo legible | `/ayuda`, 1440 y 390 oscuro | `ayuda-1440-oscuro.png` | Las tarjetas sí se oscurecen (contra lo que anticipaba SI-163), pero las descripciones y las etiquetas «Próximamente» se quedan en el gris claro original: gris medio sobre gris oscuro. | La página debería consumir la hoja del sistema en vez de sus 180 líneas propias (SI-163); mientras tanto, subir el contraste del texto secundario. | S | `ayuda.html` |

---

## 6. Datos ausentes, nulos y valores basura en pantalla

| ID | Título | Dónde se ve | Captura | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|---|
| VI-028 | **«NaN»** visible en tres pantallas distintas | `/admin/archivo` → Resumen, Retención y Tipos, 1440 claro y oscuro | `admin_archivo-stats-1440-claro.png` («venció NaN días»), `admin_archivo-retencion-1440-claro.png` (7 de 14 filas con «NaN años / NaN días»), `admin_archivo-categories-1440-claro.png` («(NaN)» en las palabras clave) | Cuando falta la fecha o el plazo, la resta se hace igual y se pinta el resultado. El aviso de la cabecera llega a decir «vencióNaN días» sin espacio. Un archivo institucional que muestra «NaN» en su pantalla de control de retención pierde toda credibilidad ante quien lo audite. | Comprobar el operando antes de restar y pintar «—» o «sin plazo definido»; una función `formatDias()` compartida. | S | `admin-stats.js`, `admin.js`, `admin-categories.js` |
| VI-029 | **«null»** literal en la consola del asistente | `/admin/ia`, 1440 y 768 | `admin_ia-1440-claro.png` | La tabla de gasto muestra una fila con la celda «null», y la lista de conversaciones «· público · null msgs · $27.4000». Se concatena el valor crudo sin comprobarlo. | `??` con respaldo y un formateador único para cifras y contadores. | S | `admin_ai.html` |
| VI-030 | Los contadores de pie de tabla dicen **cero con la tabla llena** | `/admin/archivo` → Auditoría y Papelera, 1440 claro | `admin_archivo-audit-1440-claro.png` («0 eventos registrados» bajo 14 filas), `admin_archivo-papelera-1440-claro.png` («0 documento(s) en papelera» bajo 14 filas) | El pie lee un campo de total que la respuesta no trae, mientras la tabla se pinta con las filas que sí llegan. El usuario ve catorce filas y una leyenda que dice cero. | Derivar el contador de lo que se ha pintado cuando no haya total fiable, y no pintar «0» nunca con filas visibles. | S | `admin.js`, `admin-monitor.js` |
| VI-031 | Las palabras clave se pintan como **array de JavaScript** | `/archivo` (tarjeta y modal de detalle), 1440 · 768 · 390 | `archivo-1440-claro.png`, `archivo-modal-detalle-1440-claro.png` | Una insignia contiene literalmente `['Presupuesto', 'Consejo de Facultad', 'Reestructuración']`, corchetes y comillas incluidos, en vez de tres insignias. En el modal, la fila «Clasificación / Palabras Clave» concatena `Resolución; Valor 1; ['Presupuesto', …]`. Ocurre cuando el campo llega como lista y no como cadena. | Normalizar a array y pintar una insignia por término (el código ya lo hace bien en `tesauro_badges`; el fallo está en el campo de clasificación). | S | `archive.js` |
| VI-032 | Los campos vacíos dejan **iconos huérfanos** en las tarjetas de resultado | `/archivo`, 1440 · 768 · 390, ambos temas | `archivo-1440-claro.png` (tarjetas 2, 6 y 12) | Sin autor queda un icono de persona solo; sin ubicación, un alfiler solo; sin fecha, un calendario sin fecha al lado. Tres tarjetas de catorce se ven «rotas» en vez de «incompletas». | No pintar la línea cuando no haya valor, o pintar «Sin autor registrado» en cursiva apagada. Es una decisión de catálogo: un documento sin autor es información. | S | `archive.js`, `hr.js` |
| VI-033 | Las filas con el nombre vacío se convierten en filas fantasma | `/admin/archivo` → Tipos y Retención; `/admin/rrhh` → Expedientes, 1440 | `admin_archivo-categories-1440-claro.png`, `admin_archivo-retencion-1440-claro.png`, `admin_rrhh-monitor-1440-claro.png` | Un tipo documental sin nombre se pinta como una fila alta y vacía, con su insignia «Archivo» a la derecha y su campo de plazo editable — pero sin nada que diga qué se está editando. En Expedientes, cinco filas de catorce no muestran ningún nombre de empleado. | Respaldo explícito («(sin nombre)», con el identificador entre paréntesis) y aviso en la propia fila. | S | `admin-categories.js`, `admin.js`, `admin-monitor.js` |
| VI-034 | Insignias de estado vacías: una píldora gris con un guion | `/admin/rrhh` → Expedientes, 1440 y 768 | `admin_rrhh-monitor-1440-claro.png` (filas 5 y 10) | Un empleado sin estado laboral recibe igualmente su píldora, vacía. Parece un estado más, no un dato ausente. | No pintar la insignia, o «Sin estado» en tono neutro y con contorno discontinuo. | S | `admin-monitor.js` |
| VI-035 | La cabecera de KPIs se queda en guiones y **contradice el aviso de la misma pantalla** | `/admin/archivo` y `/admin/rrhh`, todas las pestañas, 1440 | `admin_archivo-stats-1440-claro.png` | Seis de los ocho KPIs muestran «—»; «RETENCIÓN VENCIDA —» con la coletilla «ninguno vencido», justo encima de un aviso que dice «14 documentos con plazo de retención vencido». En RRHH, siete de ocho en «—» con «DOCUMENTOS 14». No se distingue «no hay dato» de «el dato es cero» ni de «falló la consulta». | Tres estados visualmente distintos (dato, cero, error) y coherencia entre KPI y aviso: si uno de los dos falla, el otro no puede afirmar lo contrario. Amplía SI-190 al backoffice de módulo. | M | `admin-stats.js` |
| VI-036 | «0 % del fondo» con catorce documentos y «0 de 1807» en la misma tarjeta | `/admin/archivo` → Resumen, 1440 | `admin_archivo-stats-1440-claro.png` | La tarjeta «Estado de Digitalización» muestra un `0 %` gigante, «0 de 1807 documentos con soporte digital» y, debajo, una lista de trece categorías que suman 1.807. Tres cifras de la misma tarjeta que no cuadran entre sí. | Una sola fuente para el denominador y ocultar el bloque si el porcentaje no se puede calcular. | S | `admin-charts.js` |
| VI-037 | «1 Registros» | `/rrhh`, 1440 y 390, ambos temas | `rrhh-1440-claro.png` | Concatenación sin plural. Aparece en la cabecera de resultados de los dos buscadores. | Una función `plural(n, "Registro", "Registros")` en `app-core.js` y usarla en los dos buscadores y en los pies de tabla. | S | `app-core.js` **[CHOCA]**, `archive.js`, `hr.js` |

---

## 7. Gráficos

`CLAUDE.md` fija tres reglas para los colores de datos: ocho ranuras validadas
para daltonismo, «Otros» a partir de la novena, y una serie única siempre en la
ranura 1. Al renderizar, las tres se incumplen en la misma pantalla.

| ID | Título | Dónde se ve | Captura | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|---|
| VI-038 | El donut de tipos pinta **trece categorías** y repite tonos | `/admin/archivo` → Resumen, 1440 claro y oscuro | `admin_archivo-stats-1440-claro.png` | La leyenda tiene trece entradas con **dos azules, dos naranjas, dos rojos y dos verdes** indistinguibles a simple vista: la seguridad para daltonismo que documenta `CLAUDE.md` se pierde en cuanto hay más de ocho categorías, y el agrupamiento en «Otros» que la consulta hace con `pos > 7` no se refleja aquí. Además, una sola categoría ocupa el 71 % y las otras doce son astillas de menos de 3°: el donut no comunica nada. | Respetar el tope de ocho ranuras + «Otros» también en el cliente, y para distribuciones tan sesgadas usar barras horizontales ordenadas. | M | `admin-charts.js`, `viz-tokens.js` |
| VI-039 | La barra de digitalización es **monocolor con trece series** | `/admin/archivo` → Resumen, 1440 | `admin_archivo-stats-1440-claro.png` | Una barra apilada de 640 px en la que los trece segmentos son del **mismo naranja**, separados sólo por rendijas blancas; los trece puntos de la leyenda también son naranjas. Es un gráfico que no se puede leer. | Consumir `vizSeries()` como el resto (regla de `CLAUDE.md`), o —mejor para «% digitalizado»— una sola barra de progreso de dos colores. | S | `admin-charts.js` |
| VI-040 | Las etiquetas del eje X de la tendencia mensual se solapan | `/admin/archivo` → Resumen, 1440 y 1440 compacto | `admin_archivo-stats-1440-claro.png`, `admin_archivo-1440-compacto.png` | Etiquetas largas rotadas en diagonal que se pisan entre sí y se salen del área del gráfico por abajo. | Rotación fija a 45°, recorte a n caracteres con `tooltip` completo, y salto de etiquetas cuando no quepan. | S | `admin-charts.js` |
| VI-041 | Con densidad compacta el donut se **recorta**: se ve media rosquilla | `/admin/archivo` → Resumen, 1440 compacto | `admin_archivo-1440-compacto.png` frente a `admin_archivo-1440-claro.png` | Al reducirse el contenedor, el lienzo no recalcula el radio y el gráfico queda cortado por arriba y por la izquierda, con la leyenda reflujada a dos columnas desiguales. Es el único fallo que aparece **sólo** con la densidad compacta. | `resize` observado sobre `.ds-chart-box` y `chart.resize()`, con `maintainAspectRatio:false` como ya documenta `CLAUDE.md`. | S | `admin-charts.js`, `styles.css` **[CHOCA]** |
| VI-042 | Tres tratamientos distintos del «sin datos» en una misma pantalla | `/admin/rrhh` → Resumen, 1440 | `admin_rrhh-modal-expediente-1440-claro.png` (fondo), `admin_rrhh-stats-1440-claro.png` | «Docs por Tipo» pinta un estado vacío con icono y frase; «Top Departamentos» pinta otro con texto distinto; y **«Cobertura de Expedientes» pinta la rejilla y el eje de 0 a 100 % sin una sola barra**, que es lo que hace pensar que el gráfico está roto y no vacío. | Un componente único de estado vacío para gráficos, y nunca ejes sin datos. | S | `admin-charts.js` |
| VI-043 | Las leyendas de los donuts de RRHH se cortan a media palabra | `/admin/rrhh` → Resumen, 1440 | `admin_rrhh-modal-expediente-1440-claro.png` (columna derecha) | La segunda columna de la leyenda se sale de la tarjeta y queda recortada en la primera letra: se leen entradas de un solo carácter («Q», «2», «0», «9»). | Leyenda debajo del gráfico, con envoltura y recorte con elipsis. | S | `admin-charts.js` |

---

## 8. Teclado, foco y objetivos táctiles

| ID | Título | Dónde se ve | Captura | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|---|
| VI-044 | El enlace de salto **no se ve al recibir el foco** | Todas las páginas con cáscara; medido en `/archivo` a 1440 y 390 | `archivo-1440-foco-skip.png`, `_diagnostico.json` | Primera pulsación de Tab: el enlace queda en `y = −25 px` con 42 px de alto — sólo asoman 17 px por debajo del borde superior, medio cortados por la barra. `CLAUDE.md` documenta este enlace como resuelto y explica que se desplaza fuera de pantalla y vuelve al enfocarse; lo que vuelve no llega a entrar del todo. | Que al recibir el foco quede a `top: 8px` con `z-index` por encima de la barra. Comprobación: el rectángulo del elemento enfocado debe estar íntegro dentro del viewport. | S | `app-shell.js` **[CHOCA]**, `styles.css` **[CHOCA]** |
| VI-045 | El tabulador recorre **diez enlaces invisibles** del menú cerrado | `/archivo` 1440 y 390; mismo patrón en todas las páginas con cáscara | `archivo-1440-foco.png`, `_diagnostico.json` (pasos 5–14, todos en `x = −290`) | El menú lateral cerrado sólo está desplazado fuera de pantalla, no retirado del orden de foco: tras la barra superior, el usuario de teclado pulsa Tab diez veces —cerrar menú, Archivo Institucional, Personal, Panel Archivo, Panel RRHH, Sistema Global, Asistente IA, Ayuda, Investigación, Personalización— **sin que se mueva nada en la pantalla**, y sólo en la undécima llega al primer filtro. El foco no arrastra el menú a la vista. | `inert` (o `visibility:hidden`) en el menú cerrado, y foco atrapado dentro cuando se abre. Es el complemento de VI-044: el enlace de salto existe justamente para esto y tampoco se ve. | S | `app-shell.js` **[CHOCA]**, `app.js` **[CHOCA]**, `styles.css` **[CHOCA]** |
| VI-046 | El anillo de foco de las pestañas queda cortado por la pestaña vecina | `/admin/archivo`, 1440, tras 22 tabulaciones | `admin_archivo-1440-foco.png` | El contorno de 2 px alrededor de «Acceso» se dibuja pero el lado derecho queda tapado por el elemento siguiente: el anillo se ve incompleto y, en las pestañas activas, prácticamente no se distingue del fondo. | `outline-offset` negativo o `box-shadow` de foco, y espacio suficiente entre pestañas. | S | `styles.css` **[CHOCA]** |
| VI-047 | 40 objetivos táctiles por debajo de 44 px en una sola pantalla | `/archivo` 390 (40 elementos), backoffice 390 (hasta 40 por pestaña) | `_diagnostico.json`, `archivo-390-claro.png` | Medidos: pestañas de administración **37 × 33**, «Cerrar Sesión» **33 × 30**, cerrar menú **32 × 32**, botones de acción de tabla **30 × 29**, botones de la consola de IA **28 × 29**, mostrar contraseña **31 × 28**. Los botones circulares de las tarjetas de resultado sí llegan a 36 px, y bajan a 30 por debajo de 768 (`styles.css:975`). | 44 × 44 px de área activa mínima en todo lo pulsable por debajo de 992 px, ampliando el área sin agrandar el icono. | M | `styles.css` **[CHOCA]**, `admin_archive.html`, `admin_hr.html`, `admin_ai.html` |
| VI-048 | A 390 px los botones de la consola de IA **pierden su nombre accesible** | `/admin/ia`, 390 | `_diagnostico.json` (pasos 1 y 2 del recorrido de foco, sin texto), `admin_ia-390-claro.png` | «Personalización» y «Admin Global» ocultan su texto por CSS al estrecharse y se quedan como botones de icono **sin `aria-label`**: el lector de pantalla anuncia «botón». `CLAUDE.md` advierte exactamente de esto («los controles que sólo llevan icono necesitan `aria-label`; el `title` no basta»). | `aria-label` fijo en ambos, independiente de si el texto se ve. | S | `admin_ai.html` |

---

## 9. Consistencia visual y lenguaje

| ID | Título | Dónde se ve | Captura | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|---|
| VI-049 | **Cinco colores de botón principal** según la pestaña en que estés | `/admin/archivo`, las nueve pestañas, 1440 | `admin_archivo-new-1440-claro.png` (verde), `admin_archivo-categories-1440-claro.png` (amarillo), `admin_archivo-users-1440-oscuro.png` (rojo), `admin_archivo-export-1440-claro.png` (teal), `archivo-1440-claro.png` (azul) | La acción principal es azul en «Aplicar», **verde** en «Guardar en Archivo», **amarillo** en «Guardar Tipología», **rojo de peligro** en «Crear Usuario» —el mismo rojo que el botón de borrar de la fila de al lado— y **teal** en «Descargar backup». No hay forma de aprender dónde está el botón que confirma. | Un único botón primario con el acento del sistema; verde/amarillo/rojo reservados a estado, y el rojo **sólo** a lo destructivo. Es la consecuencia visible de SD-023 y SD-002. | M | `admin_archive.html`, `admin_hr.html`, `styles.css` **[CHOCA]** |
| VI-050 | **Cinco nombres para el mismo objeto** en una sola pantalla | `/admin/archivo` → Tipos, 1440 | `admin_archivo-categories-1440-claro.png` | La pestaña se llama «Tipos», la tarjeta «Nueva Categoría», el campo «Nombre de la Tipología», el botón «Guardar Tipología» y la lista de la derecha «Taxonomías Activas». Cinco palabras para una cosa, en 1.400 px de pantalla. `CLAUDE.md` sólo prohíbe «Tesauro»; esto es el mismo problema una capa más abajo. | Elegir una: «Tipo documental». Y añadir la regla al glosario de `CLAUDE.md`. | S | `admin_archive.html`, `admin_hr.html`, `CLAUDE.md` |
| VI-051 | Dos formatos de fecha en la misma pantalla | `/admin/archivo` → Retención, Auditoría y Papelera vs Documentos, 1440 | `admin_archivo-retencion-1440-claro.png` (`2019-01-01`) frente a `admin_archivo-monitor-768-claro.png` (`01/01/2019`) | Tres pestañas del mismo panel pintan la fecha en ISO y una en formato local. En Papelera, además, la columna «Fecha» es tan estrecha que parte el ISO en dos líneas (`2019-`/`01-01`) mientras la columna vecina lo muestra entero. | `formatISOToSpanish()` —que ya existe y usa la búsqueda— en todas las tablas. | S | `admin.js`, `admin-monitor.js`, `app-core.js` **[CHOCA]** |
| VI-052 | Dos controles de fecha distintos en el mismo producto | `/admin/archivo` → Resumen e Ingresar (nativo) frente a `/archivo` → Fecha (flatpickr), 1440 | `admin_archivo-stats-1440-claro.png`, `admin_archivo-new-1440-claro.png`, `archivo-1440-claro.png` | El backoffice usa `<input type="date">` con el calendario del navegador —que además no tiene modo oscuro y en `es-VE` muestra el marcador `dd/mm/yyyy`—; la búsqueda usa flatpickr con chips «Todo / Año / Rango». Dos idiomas de interacción para la misma tarea. | Un solo control de fecha en todo el producto. | M | `admin_archive.html`, `admin_hr.html`, `app-choices.js` **[CHOCA]** |
| VI-053 | La credencial del usuario se pinta en **rojo de error** | Todas las páginas con cáscara, 1440 · 768 · 390 | `archivo-1440-claro.png`, `admin_sistema-1440-claro.png` | «ID: admin.global (Archivo - Admin)» va en rojo con un icono rojo de persona, en la esquina donde el usuario espera su identidad, no una alarma. En `/admin/sistema` es lo **único** que se ve además del logo (VI-001), lo que refuerza la sensación de error. | Color neutro o el acento del sistema; el rojo, para el error. | S | `app-shell.js` **[CHOCA]**, `styles.css` **[CHOCA]** |
| VI-054 | Tres de las diez páginas están fuera de la cáscara común | `/admin/ia`, `/ayuda`, `/investigacion`, 1440 | `admin_ia-1440-claro.png`, `ayuda-1440-claro.png`, `investigacion-1440-claro.png` | `/admin/ia` empieza directamente con la migaja: **sin barra superior, sin logotipo y sin menú**, con un botón «← Admin Global» como única salida. `/ayuda` y `/investigacion` tienen su propia cabecera y un botón «Volver a la aplicación» flotando sobre el héroe. Tres maneras distintas de volver, y en dos de ellas el usuario pierde el menú. | La cáscara común (`app-shell.js`) en las tres, como en las otras siete. Empareja con SD-207. | M | `admin_ai.html`, `ayuda.html`, `investigacion.html` |
| VI-055 | La consola de IA usa un lenguaje visual completamente distinto | `/admin/ia`, 1440 | `admin_ia-1440-claro.png` | Tipografía más pequeña, tablas sin bordes de tarjeta, insignias «sí/no» minúsculas en verde y rojo, y los identificadores de modelo en **monoespaciada roja** —el color que en Bootstrap significa error—. Parece una herramienta interna pegada al producto. | Alinear con el sistema: mismas tarjetas, mismas insignias, y los slugs en gris. | M | `admin_ai.html`, `styles.css` **[CHOCA]** |
| VI-056 | Las matrices comparativas de `/investigacion` se pintan a ~9 px | `/investigacion`, 1440 y 768 | `investigacion-1440-claro.png` | Las dos tablas de comparación (9 columnas × 20 filas) usan una tipografía tan pequeña que en 1440 px hay que acercarse a la pantalla; a 768 la tabla mide 1.000 px y se sale de su envoltorio. La página, además, sólo existe en oscuro: ignora el tema del usuario. | Si la página se conserva (SI-156 recomienda no servirla públicamente), tipografía mínima de 13 px y tabla con desplazamiento anunciado. | M | `investigacion.html` |
| VI-057 | El menú principal está escondido tras una hamburguesa **también a 1440 px** | Todas las páginas con cáscara, 1440 | `archivo-1440-claro.png`, `archivo-menu-abierto-1440-claro.png` | En escritorio, con 1.440 px de ancho y una columna de contenido que no llega a llenarlos, la navegación entre módulos exige un clic para abrir un cajón que tapa el contenido y otro para elegir. El producto se comporta como una aplicación móvil en una pantalla de escritorio. | Menú lateral fijo a partir de 992 px, cajón sólo por debajo. Resuelve además parte de VI-045. | M | `app-shell.js` **[CHOCA]**, `styles.css` **[CHOCA]** |
| VI-058 | Dos paginaciones para la misma lista | `/archivo` y `/rrhh`, 1440 · 768 · 390 | `archivo-1440-claro.png` (pie de la página) | Debajo de los resultados hay un paginador numerado con « ‹ 1 2 3 › » y, justo debajo, otra barra con «Anterior — Pág 1 de 3 — Siguiente». Dos controles, la misma función, apilados. | Un solo paginador. | S | `archive.js`, `hr.js`, `app.js` **[CHOCA]** |
| VI-059 | El modal de edición ofrece **dos botones «Guardar»** sin explicar la diferencia | `/admin/archivo`, modal de edición, 1440 | `admin_archivo-modal-edicion-vp900-claro.png` | Al pie conviven «Ver historial», «Cancelar», «Guardar versión» y «Guardar Cambios». Nada dice qué hace uno y no el otro, y el modal mide **1.403 px en un viewport de 900**: hay que desplazarse 500 px para llegar a ellos, y el pie no es fijo. | Una sola acción primaria, la secundaria explicada, y pie fijo dentro del modal. | M | `admin-edit.js`, `styles.css` **[CHOCA]** |
| VI-060 | El título del documento aparece dos veces en el modal de detalle | `/archivo`, modal de detalle, 1440 | `archivo-modal-detalle-1440-claro.png` | Va en la cabecera del modal y otra vez como primera fila de la tabla «Metadata», ocupando ocho líneas entre las dos. | Quitarlo de la tabla; la cabecera ya lo dice. | S | `archive.js` |
| VI-061 | El botón de acción sin etiqueta convive con dos etiquetados | `/archivo`, modal de detalle, 1440 | `archivo-modal-detalle-1440-claro.png` | Al pie: «Cerrar», «Ver Imagen» y un tercer botón amarillo **sólo con un lápiz**, sin texto. | Etiqueta «Editar» o retirarlo del pie. | S | `archive.js` |
| VI-062 | La columna «Contraseña» de la tabla de usuarios es decorativa | `/admin/archivo` → Acceso, 1440 | `admin_archivo-users-1440-oscuro.png` | Ocho puntos idénticos en las catorce filas, ocupando 150 px de una tabla que ya se desborda a 768 (VI-009). No informa de nada — ni de si la contraseña es antigua, ni de si es temporal. | Retirar la columna; la acción «Clave» ya está en Acciones. Si hace falta, «última actualización de la contraseña». | S | `admin-users.js` |
| VI-063 | La tabla de retención separa el nombre de su campo por 1.100 px | `/admin/archivo` y `/admin/rrhh` → Retención, 1440 | `admin_archivo-retencion-1440-claro.png` | «Tipo de Documento» ocupa el 80 % del ancho para un texto de tres palabras, y el campo de plazo con su botón de guardar queda pegado al borde derecho. Con catorce filas es imposible seguir la línea con la vista, y hay **catorce botones de guardar** en lugar de uno. | Ancho máximo de contenido (~720 px), campo junto al nombre, guardado al salir del campo y un único «Guardar cambios» al pie. | M | `admin.js`, `styles.css` **[CHOCA]** |
| VI-064 | La pestaña «Exportar» dedica una novena parte de la navegación a un botón | `/admin/archivo` y `/admin/rrhh` → Exportar, 1440 | `admin_archivo-export-1440-claro.png` | La pantalla entera es una frase y un botón; debajo, 700 px de vacío. La cabecera de KPIs y la barra de pestañas ocupan más que el contenido. | Mover la descarga a un menú de la cabecera y liberar la pestaña, o darle contenido real (histórico de exportaciones, selección de tablas, programación). | M | `admin_archive.html`, `admin_hr.html`, `admin.js` |
| VI-065 | La lista de «Taxonomías Activas» gasta 45 px de alto por una palabra | `/admin/archivo` → Tipos, 1440 | `admin_archivo-categories-1440-claro.png` | Catorce filas de 45 px con un nombre a la izquierda, una insignia «Archivo» idéntica a 700 px a la derecha y **ningún control**: no se puede renombrar ni borrar desde ahí, mientras las palabras clave de abajo sí tienen lápiz y papelera. Debajo del formulario quedan 600 px de columna vacía. | Lista de dos columnas con acciones por fila, insignia sólo cuando aporte, y el formulario y la lista equilibrados. | M | `admin-categories.js`, `styles.css` **[CHOCA]** |
| VI-066 | El formulario de alta no tiene rejilla: los anchos no guardan relación con el contenido | `/admin/archivo` → Ingresar, 1440 | `admin_archivo-new-1440-claro.png` | «Ubicación Física» ocupa los 1.300 px de ancho para un código como «Gaveta 1», mientras el título del documento —el campo más largo— tiene la mitad. Las filas alternan dos y tres columnas sin criterio, y los doce campos van seguidos **sin una sola agrupación**. | Rejilla de 12 columnas con anchos por tipo de dato, y tres bloques con título: identificación, descripción, custodia. | M | `admin_archive.html`, `admin_hr.html` |
| VI-067 | La zona de arrastrar y soltar parte la frase por la mitad | `/admin/archivo` → Ingresar, 1440 | `admin_archivo-new-1440-claro.png` | Se lee «Arrastra el archivo digital aquí o» a la izquierda y, a 1.000 px de distancia, el botón «Explorar» y el texto de formatos. La frase queda colgando de una «o» huérfana. | Bloque centrado: icono, «Arrastra el archivo aquí **o** [Explorar]», y los formatos debajo en pequeño. | S | `admin_archive.html`, `admin_hr.html`, `styles.css` **[CHOCA]** |
| VI-068 | La fecha de emisión llega prerellenada con la fecha de hoy | `/admin/archivo` → Ingresar, 1440 | `admin_archivo-new-1440-claro.png` (02/09/2026 en un formulario vacío) | Un campo obligatorio que ya viene «relleno» es un campo que nadie corrige: el documento de 1998 se cataloga con la fecha de hoy y el error no vuelve a detectarse. | Dejarlo vacío con marcador, y avisar si la fecha coincide con la de hoy. | S | `admin_archive.html`, `admin-submit.js` |

---

## 10. Estados: vacío, error, carga

| ID | Título | Dónde se ve | Captura | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|---|
| VI-069 | El estado de error deja **cuatro esqueletos girando para siempre** y borra el panel de facetas | `/archivo` con la búsqueda respondiendo 500, 1440 | `archivo-error-1440-claro.png` | Confirmado en pantalla lo que `BA-006` describe desde el código: la lista se queda con cuatro esqueletos, la cabecera dice «0 Registros» y no hay ningún mensaje. Y algo que el código no dejaba ver: **la tarjeta «Distribución» desaparece por completo** —estaba en el estado normal— mientras «Vista» permanece, así que la columna izquierda cambia de forma sin explicación. | Estado de error con motivo y «Reintentar» (ya pedido en `BA-006`), y que el panel de facetas conserve su marco con su propio estado de error en vez de evaporarse. | S | `archive.js`, `hr.js` |
| VI-070 | El estado vacío afirma que **el archivo está vacío** aunque sólo lo esté el resultado | `/archivo` con cero resultados, 1440 | `archivo-vacio-1440-claro.png` | El mensaje es «El archivo no contiene documentos registrados aún», se muestre lo que se muestre: con filtros aplicados, con término de búsqueda, o de verdad vacío. Y no ofrece salida: ningún «Limpiar filtros». | Distinguir los tres casos, y en el de «sin resultados» ofrecer quitar el último filtro y mostrar los términos usados. | S | `archive.js`, `hr.js` |
| VI-071 | Un enlace compartido caducado deja 600 px de tarjeta vacía y ninguna salida | `/compartido/<token inválido>`, 1440 · 768 · 390, ambos temas | `compartido-1440-claro.png` | El aviso ocupa 120 px arriba de una tarjeta de 740 px; el resto es blanco. No hay enlace al archivo, ni contacto, ni ninguna acción — y es la única pantalla del sistema que ve alguien de fuera de la Facultad. | Tarjeta ajustada al contenido, con contacto del archivo y enlace público. | S | `compartido.html`, `styles.css` **[CHOCA]** |
| VI-072 | El enlace compartido válido no dice **hasta cuándo** vale | `/compartido/<token válido>`, 1440 claro y oscuro | `compartido-valido-1440-claro.png` | La pantalla dice «Consulta autorizada mediante enlace temporal» pero no muestra la fecha de caducidad, que sí viaja dentro del token. Quien recibe el enlace no sabe si le caduca hoy o en un mes. La fecha del documento, además, sale en ISO (VI-051). | Mostrar «Este enlace caduca el <fecha>» y usar el formato local. | S | `compartido.html`, `share.py` |
| VI-073 | El centro de ayuda está vacío en las seis condiciones | `/ayuda`, 1440 · 768 · 390, claro y oscuro | `ayuda-1440-claro.png`, `ayuda-390-oscuro.png` | Cuatro tarjetas con candado y «Próximamente», dos bloques de estado vacío y un buscador que filtra sobre una lista que no existe: la pantalla completa no contiene una sola frase de ayuda. Es `SI-160` visto de frente, y explica por qué la ayuda no resuelve nada de lo que el resto de la interfaz da por sabido (las cuatro Partes, la retención, los estados). | Contenido real, o retirar la página del menú hasta que lo tenga: una ayuda vacía enseña que el sistema no está terminado. | M | `ayuda.html` |
| VI-074 | El campo de contraseña usa **puntos como marcador** | `/login`, todas las condiciones | `login-390-claro.png`, `login-1440-oscuro.png` | El marcador es `••••••••`, indistinguible de una contraseña ya escrita: en la captura parece que el formulario viene relleno. Además, los dos campos están fuera de un `<form>` y sin `autocomplete`, así que los gestores de contraseñas no ofrecen guardar ni rellenar. | Marcador vacío o textual, `<form>` con `autocomplete="username"` y `"current-password"`. | S | `login.html`, `login.js` |
| VI-075 | El login no tiene tema ni modo oscuro (confirmado en pantalla) | `/login`, 1440 y 390, con preferencia oscura activa | `login-1440-oscuro.png` frente a `login-1440-claro.png` | Las dos capturas son idénticas: la tarjeta sigue clara con la preferencia en «Oscuro». Es exactamente `SI-049`; se registra aquí sólo porque la captura lo prueba y porque es la primera pantalla del sistema. | Ver `SI-049`. | — | — |

---

## 11. Densidad, animaciones y ritmo

| ID | Título | Dónde se ve | Captura | Hoy | Debe | Esf. | Toca |
|---|---|---|---|---|---|---|---|
| VI-076 | La densidad compacta cambia el espaciado ~8 px y **rompe un gráfico** | `/admin/archivo`, 1440 compacto | `admin_archivo-1440-compacto.png` | La única diferencia perceptible en la cabecera es que la etiqueta «RETENCIÓN VENCIDA» deja de partirse en dos líneas —lo que revela que en densidad normal esa tarjeta va corta de ancho (VI-077)—; a cambio, el donut se recorta (VI-041). El ajuste promete «más información por pantalla» y entrega ~2 % de altura. | Que la densidad actúe sobre alto de fila de tabla, alto de tarjeta de resultado y espaciado de formulario, no sólo sobre el `padding` de las secciones. | M | `styles.css` **[CHOCA]**, `app-theme.js` **[CHOCA]** |
| VI-077 | Una de las ocho tarjetas de KPI parte su rótulo en dos líneas | `/admin/archivo` y `/admin/rrhh`, 1440 · 768 | `admin_archivo-stats-1440-claro.png` («RETENCIÓN VENCIDA»), `admin_rrhh-retencion-1440-claro.png` («JUBILACIÓN < 12 MESES») | Ocho tarjetas de ancho idéntico para rótulos de longitud muy distinta: una o dos se parten, y su cifra baja 14 px respecto a las demás. La fila de cifras deja de leerse como una fila. | Rótulos más cortos, o rejilla que reserve dos líneas para todos y alinee las cifras por su línea base. | S | `admin_archive.html`, `admin_hr.html`, `styles.css` **[CHOCA]** |
| VI-078 | Con las animaciones apagadas no cambia nada perceptible, ni siquiera lo que debería | `/archivo` y backoffice, 1440 sin animaciones | `archivo-1440-sinanim.png`, `admin_archivo-1440-sinanim.png` | Las capturas con `ds_anim=off` son idénticas a las normales, lo cual es correcto en reposo — pero el **esqueleto de carga sigue latiendo** (su animación se define en un `<style>` local, `SI-186`) y es justo la animación que más molesta a quien la desactiva. | Que `body.ds-no-anim` y `prefers-reduced-motion` también detengan el esqueleto, y que éste viva en `styles.css`. | S | `styles.css` **[CHOCA]**, `admin_system.html` |
| VI-079 | Las tarjetas de resultado tienen alturas muy dispares y la lista pierde el ritmo | `/archivo`, 1440 · 768 · 390 | `archivo-1440-claro.png` | Entre una tarjeta con resumen, insignias y tres líneas de metadatos y otra con sólo título y autor hay 90 px de diferencia; con campos vacíos (VI-032) el borde de acento de la izquierda queda flotando junto a media tarjeta en blanco. Catorce tarjetas de siete alturas distintas. | Altura mínima común y reserva de las líneas de metadatos, aunque el dato falte. | S | `archive.js`, `styles.css` **[CHOCA]** |
| VI-080 | A 390 px hay que recorrer 550 px de filtros antes del primer resultado | `/archivo` y `/rrhh`, 390, ambos temas | `archivo-390-claro.png` | En móvil la columna de filtros se apila **encima** de los resultados: filtros, facetas de distribución y opciones de vista ocupan la primera pantalla y media; el cuadro de búsqueda aparece después, y el primer resultado a 1,6 pantallas del inicio. | Buscador y resultados primero; filtros en un panel plegable o en un cajón, con el número de filtros activos visible. | M | `archive.html`, `hr.html`, `styles.css` **[CHOCA]** |

---

## 12. Juicio de conjunto: por qué no parece profesional

Lo anterior son defectos concretos. Esto es lo que un archivero o un decano
percibiría en los primeros treinta segundos, y por qué:

1. **Nada indica jerarquía.** En una pantalla de administración hay ocho
   tarjetas de KPI del mismo tamaño (seis vacías), una barra de nueve pestañas,
   un aviso amarillo, una tarjeta de filtros y cuatro gráficos, todos con el
   mismo peso visual y el mismo borde. Nada dice qué mirar primero. Un panel de
   control es una respuesta a una pregunta; éste es un inventario de widgets.
2. **El color no significa nada.** Cinco colores de botón principal (VI-049),
   rojo para la identidad del usuario (VI-053) y para «Crear Usuario», amarillo
   para guardar y para avisar, verde para guardar y para «Activo». Cuando todos
   los colores están usados, ninguno informa.
3. **El espacio no se administra.** Un campo de código con 1.300 px (VI-066),
   un nombre a 1.100 px de su campo (VI-063), una pestaña entera con 700 px de
   vacío (VI-064), 600 px de columna vacía junto al formulario de tipos
   (VI-065) — y, a la vez, tablas amputadas por no caber (VI-009). Sobra sitio
   donde no hace falta y falta donde hace.
4. **Los detalles delatan.** «NaN días» (VI-028), «null msgs» (VI-029),
   «1 Registros» (VI-037), «0 eventos registrados» bajo catorce filas (VI-030),
   un array de JavaScript dentro de una insignia (VI-031), la palabra
   «Miniatura» escrita en vertical (VI-014). Cada uno es pequeño; juntos son la
   diferencia entre un sistema y un prototipo.
5. **Parece software de 2012.** Bordes de 1 px por todas partes, tablas con
   rayado y cuadrícula completa, insignias rectangulares de esquinas mínimas,
   iconos dentro de círculos de colores planos, sombras duras y una barra
   superior blanca con logotipo a la izquierda. Es la estética por defecto de
   Bootstrap 4 de hace una década — y `styles.css` no aporta un punto de vista
   propio, sino 1.117 `!important` para pelearse con ella (SD-036). Lo que
   fecha el producto no es que sea sobrio: es que no ha decidido nada.
6. **Y sin embargo, la mejor pantalla del sistema demuestra que se puede.** La
   página de documento compartido (`compartido-valido-1440-claro.png`) tiene una
   tarjeta con aire, una jerarquía tipográfica clara, una lista de definiciones
   legible y una sola acción azul. Está bien. Es la única pantalla que no
   heredó nada del panel original, y se nota: el problema no es la falta de
   criterio, es que el criterio no se aplicó hacia atrás.

**Si sólo se hace una cosa de estética**, que sea unificar la acción primaria
(VI-049) y el color de estado: es media jornada, toca ficheros ya declarados en
el lote `L0` de `sistema-diseno.md`, y es lo que más cambia la percepción por
euro invertido. **Si se hacen dos**, la segunda es el modo oscuro
(VI-020…VI-024): hoy el interruptor entrega un producto a medio pintar, y es
peor que no tenerlo.

---

## 13. Anexo — lo que se confirmó de otras auditorías

Estos pendientes ya estaban listados; el recorrido añade la prueba visual y, en
algún caso, agrava el diagnóstico. **No se les asigna identificador nuevo.**

| Pendiente ajeno | Qué se ve, y dónde |
|---|---|
| `BA-006` — el error deja el esqueleto girando | `archivo-error-1440-claro.png`. Se agrava: además desaparece el panel de facetas (VI-069). |
| `SI-049` — el login no tiene tema | `login-1440-oscuro.png` es idéntica a la clara (VI-075). |
| `SI-160` — la página de ayuda está vacía | `ayuda-1440-claro.png`: cuatro «Próximamente» y dos estados vacíos (VI-073). |
| `SI-186` — el esqueleto de carga late aunque se apaguen las animaciones | `archivo-1440-sinanim.png` (VI-078). |
| `SI-190` — el panel de Sistema falla y se queda en guiones | No se pudo llegar a verlo: la página entera está en blanco (VI-001), que es un grado más. |
| `SD-036` / `SD-037` — el modo oscuro son reglas sueltas y cubre el 60 % | Confirmado componente a componente en la sección 5. |
| `SD-009` — la escala de z-index no existe | La burbuja del asistente queda por encima del panel de personalización (VI-004). |
| `SD-023` — los colores de estado no son tokens | Cinco colores de botón principal (VI-049) y amarillo con blanco a 1,9:1 (VI-013). |
| `SD-207` — `investigacion.html` fuera del sistema | `investigacion-1440-claro.png`: otra tipografía, otro fondo, otro todo (VI-054, VI-056). |
| `OA-001` — la Retención de RRHH abre incompleta | `admin_rrhh-retencion-1440-claro.png`: falta el bloque de vencidos que Archivo sí tiene (VI-019). |
| `BA-011` — icono de FontAwesome inexistente | Mismo patrón en otro fichero: `fa-shield-halved` en `/admin/ia` (VI-016). |

Y una comprobación en negativo que conviene registrar: **las cabeceras cuadran
con las celdas en las 26 tablas medidas**, en las tres anchuras. La «tabla
corrida» que `CLAUDE.md` teme —columna oculta en el `<th>` pero no en el `<td>`—
no se ha producido en ninguna pantalla. Lo que sí se produce es la tabla
recortada sin avisar (VI-009).
