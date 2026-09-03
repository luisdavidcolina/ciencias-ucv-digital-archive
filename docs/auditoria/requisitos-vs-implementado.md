# Auditoría de trazabilidad — lo prometido contra lo implementado

Carril `RQ`. Ninguna de las siete auditorías anteriores leyó los documentos contractuales del
proyecto. Este documento sí: `Pendientes.xlsx` (la lista del propio cliente),
`requerimientos_resumen.pdf/.tex`, los trece capítulos y los cinco anexos de `docs/tex/`
—en especial `annexes/C_matriz_trazabilidad.tex`—, `docs/funcionalidades.md`, `docs/modulo-ia.md`,
`CLAUDE.md`, `CHANGELOG.md` y `audit_log.csv`. Cada promesa se ha contrastado contra el código.

Cuando un hallazgo ya está descrito en otro carril se **cita por su identificador** (`BA-`, `BR-`,
`OA-`, `OR-`, `SI-`, `SD-`, `IN-`) y no se duplica como pendiente nuevo. Los `RQ-` de aquí son
sólo lo que ningún otro carril vio, porque ningún otro carril leyó estos documentos.

---

## Resumen ejecutivo

| Estado | Nº | Qué significa |
|---|---:|---|
| **CUMPLIDO** | 21 | Existe y hace lo que el documento promete |
| **PARCIAL** | 24 | Existe a medias: la casilla se marca, el propósito no se cumple |
| **AUSENTE** | 18 | Prometido y no está |
| **FANTASMA** | 12 | La documentación afirma que está hecho y no lo está |
| **DERIVA** | 11 | Implementado de forma distinta a lo especificado |
| **Total contrastado** | **86** | 42 requisitos formales (RF/RNF) + 26 pendientes del cliente + 18 compromisos de otros documentos |

### Los cinco titulares

**1. El documento contractual especifica una tecnología que no es la que se construyó.** Los
trece capítulos, el resumen de requerimientos y el glosario describen **R/Shiny sobre un VPS con
PostgreSQL local, 2 GB de RAM y Docker opcional**. `chapters/04_requerimientos.tex` llega a listar
`shiny, bs4Dash, jsonlite, tidyverse, plotly, DT, shinyWidgets` como dependencias, y
`chapters/13_comparativa_dms_shiny.tex` dedica una comparativa entera a justificar por qué **no**
se hace en Python. Lo que existe es **Python + FastAPI sobre Vercel serverless, con Neon y
Cloudflare R2**. Ninguna palabra de la memoria técnica se actualizó. No es un detalle de forma:
la memoria es el entregable que valida la Facultad, y hoy describe un sistema que no existe
(`RQ-001`).

**2. La restricción de soberanía se incumple por construcción.** `chapters/04_requerimientos.tex`
fija como restricción «priorizar procesamiento local/institucional» y
`chapters/01_resumen_ejecutivo.tex` vende «soberanía tecnológica». El sistema corre en Vercel
(EEUU), la base en Neon (fuera del continente, como reconoce `CLAUDE.md`), los archivos en
Cloudflare R2 y el asistente manda contenido documental a OpenRouter. Los expedientes de personal
—cédulas, fechas de nacimiento, sexo, nivel educativo— viven fuera del país sin que exista el
marco legal que `chapters/11_gobernanza_legal.tex` da por aprobado (`RQ-002`, `RQ-003`).

**3. La matriz de trazabilidad no traza nada.** `annexes/C_matriz_trazabilidad.tex` tiene diez
filas y la columna «Evidencia» dice, en las diez, «Cap. 4, Cap. 5» o similar: un documento
apuntando a otro documento. Ni un archivo, ni una prueba, ni un endpoint. Se puede firmar entera
sin que una sola línea de código exista. Y de los 42 requisitos formales sólo diez aparecen en
ella (`RQ-004`).

**4. La lista de pendientes del cliente lleva sin tocarse lo suficiente como para que el cliente
la haya escrito dos veces.** De los 26 puntos de `Pendientes.xlsx`, **9 están hechos, 6 a medias y
11 sin empezar**. Entre los no empezados hay dos que son el cambio de nombre del producto
(«Expedientes RRHH ese es el nuevo nombre», «Archivo recursos humanos en vez de institución») y
dos que son decisiones de arquitectura que nadie le ha respondido (si conviene o no la SPA, si se
puede alternar entre módulos con una sola cuenta).

**5. Hay funcionalidad documentada como existente que el usuario no puede alcanzar.** El filtro
por soporte, que `docs/funcionalidades.md` lista como característica del buscador, está completo
en el backend (`app/routes/archive.py:143-145`) y el control que lo dispararía —`#soporte_archivo`—
no existe en ningún HTML. El campo «Proyecto» de los planos, que el cliente pidió expresamente,
se pinta siempre con el título porque la columna `proyecto` no existe en la base. Los doce casos
están en la sección FANTASMAS.

---

## Tabla de trazabilidad completa

Leyenda de origen: **RF/RNF** = `docs/tex/chapters/04_requerimientos.tex` · **MT** =
`annexes/C_matriz_trazabilidad.tex` · **CL** = `Pendientes.xlsx` (cliente) · **FN** =
`docs/funcionalidades.md` · **RS** = `requerimientos_resumen.tex` · **CAP-n** = capítulo n ·
**CM** = `CLAUDE.md` · **CH** = `CHANGELOG.md` · **IA** = `docs/modulo-ia.md`.

### Requisitos funcionales

| Requisito | Origen | Estado | Evidencia en código | Pendiente |
|---|---|---|---|---|
| RF-001 Login con credenciales institucionales | RF, MT | **DERIVA** | `app/routes/auth.py:71-104`; usuarios propios en `usuarios_sistema`, bcrypt en `app/core/security.py:15` | No hay integración con el directorio de la UCV: «institucional» son cuentas locales creadas a mano. `RQ-005` |
| RF-002 Identificar rol y módulo para controlar la navegación | RF | **PARCIAL** | `app/static/app.js` `configureSidebarVisibilities()` | Controla la *navegación*, en el navegador. El backend no lo comprueba nunca: `IN-131`, `SI-002` |
| RF-003 Cierre de sesión seguro | RF | **PARCIAL** | `app/routes/auth.py:142-146` | Borra la cookie pero no invalida el token HMAC —un token robado sigue sirviendo hasta caducar— y el cierre no se audita. `RQ-006` |
| RF-010 Búsqueda por texto libre sobre metadatos | RF, MT | **CUMPLIDO** | `app/routes/archive.py:74+`, `plainto_tsquery('spanish')` + `ts_rank_cd`, índices GIN | — |
| RF-011 Filtrar por tipología, tesauro y rango de fechas | RF | **PARCIAL** | `app/routes/archive.py:143-145` (soporte); filtros de tipo y fecha en `app/static/archive.html:90-95` | Las facetas no responden al clic (`BA-001`); el filtro de soporte no tiene control en la UI (`RQ-007`) |
| RF-012 Ordenar por criterio alfabético y cronológico | RF | **CUMPLIDO** | `app/routes/archive.py:149-154` `sort_map` | — |
| RF-013 Paginar resultados | RF | **CUMPLIDO** | `app/utils.py` `paginate()`; respuesta `{records,total,page,per_page}` | — |
| RF-014 Tarjetas de resultado con metadatos y ubicación física | RF | **CUMPLIDO** | `app/static/archive.js:163-200` | — |
| RF-015 Modal de detalle documental | RF | **CUMPLIDO** | `app/static/archive.js:228-266`, `app/static/archive.html:160` | — |
| RF-016 Exportar resultados filtrados a **XLS** | RF | **DERIVA** | `app/static/app.js:381-409` `_exportResultsCSV()`, botón `#download_archivo_xls` | Se exporta CSV bajo un botón llamado «xls», y sólo la página actual. `RQ-008` |
| RF-020 Expediente unificado por persona | RF, MT | **CUMPLIDO** | vista `vw_rrhh_persona_index` (`app/main.py:179-190`), `app/routes/hr.py:300+` | — |
| RF-021 Búsqueda por nombre, apellido o identificador | RF | **CUMPLIDO** | `app/routes/hr.py:128-155` (FTS + ILIKE sobre `persona_raw`, `cedula`, `rif`) | — |
| RF-022 Filtrar por tipología, estado, **personas relacionadas** y fecha de ingreso | RF | **DERIVA** | `app/routes/hr.py:156-170`; `people_clauses` sigue en el backend, el control se retiró de `hr.html` | El cliente pidió expresamente «quitar el filtro personas». **Gana el cliente**: hay que borrar el requisito del documento, no reponer el filtro. `RQ-009` |
| RF-023 Dossier con metadatos laborales y archivos | RF | **CUMPLIDO** | `app/static/hr.js:265+`, `app/routes/hr.py:387` | Calidad del marcado: `BR-109` |
| RF-024 Ordenar resultados RRHH | RF | **PARCIAL** | `app/routes/hr.py` | No hay `sort_map` equivalente al de Archivo: el orden es fijo y no hay control. `RQ-010` |
| RF-025 Exportar expediente consolidado por persona a **XLS** | RF | **AUSENTE** | `app/routes/hr.py:445-500` genera un HTML imprimible, no un XLS | `RQ-011` |
| RF-030 Panel administrativo por rol autorizado | RF, MT | **PARCIAL** | `app/static/admin_archive.html`, `admin_hr.html`, `admin.js` | «Por rol autorizado» sólo en el cliente: `IN-131` |
| RF-031 Vistas de ingreso, monitor, categorías, usuarios y estadísticas | RF | **CUMPLIDO** | nueve pestañas en `app/static/admin.js` `loadAdminTab()` | — |
| RF-032 Filtros analíticos por módulo y metadatos | RF | **PARCIAL** | `app/routes/admin/stats.py` | Los KPIs no se pueden acotar por rango de fechas ni por tipo. `RQ-012` |
| RF-033 Indicadores de operación por módulo | RF | **CUMPLIDO** | `app/static/admin-stats.js`, `admin-charts.js` | — |
| RF-040 Registrar eventos de autenticación, **consulta** y **exportación** | RF, MT, CAP-8 | **PARCIAL** | 35 llamadas a `log_event()`; ninguna en `archive.py` ni en `hr.py:110` | No se audita ninguna búsqueda ni ninguna exportación (la exportación ocurre en el navegador y el servidor no se entera). `RQ-013` |
| RF-041 Preservar actor, acción, módulo, detalle y marca temporal | RF | **DERIVA** | `app/database.py:153-179` | El **actor lo manda el cliente** (`req.usuario` en `admin/docs.py:247,411,431,604`; `requester` por query en `admin/docs.py:459`, `admin/users.py:79,88`). Se puede firmar cualquier acción con el nombre de otro. `RQ-014` |
| RF-042 Evidencia para auditoría interna y análisis forense | RF | **PARCIAL** | `app/database.py:174-179` | El `INSERT` va en un hilo disparado y olvidado que además «falla silenciosamente». En Vercel la lambda se congela al responder: pérdida de eventos sin rastro. `RQ-015` |

### Requisitos no funcionales

| Requisito | Origen | Estado | Evidencia en código | Pendiente |
|---|---|---|---|---|
| RNF-001 Segregación por módulo y rol | RNF, MT, CAP-8 | **AUSENTE** | `app/routes/admin/deps.py:7-27`: `require_session` devuelve un nombre y nada más | Ya levantado: `IN-131`, `SI-002`, `OR-043`, `BR-001` |
| RNF-002 Datos de RRHH bajo mínimo privilegio | RNF, CAP-8 | **AUSENTE** | `app/routes/hr.py:110` y `:300` sin `dependencies=_auth`; sólo `:387` y `:445` lo llevan | `BR-001`; añade `RQ-016` (la asimetría dentro del mismo fichero) |
| RNF-003 Trazabilidad integral para **no repudio** | RNF, CAP-8 | **AUSENTE** | mismo caso que RF-041 | Con el actor puesto por el cliente no hay no repudio posible. `RQ-014` |
| RNF-010 Operar en infraestructura institucional de bajo costo | RNF, MT, RS | **DERIVA** | Vercel + Neon + R2 en vez de un VPS de 2 GB | No es «bajo costo institucional» sino tres SaaS extranjeros, dos con facturación por uso. `RQ-002` |
| RNF-011 Sostener crecimiento sin degradación crítica | RNF | **PARCIAL** | `pandas` importado en `archive.py:3`, `hr.py:4`, `lookups.py:5`, `admin/stats.py:6` | Se carga pandas en cada arranque en frío para lo que el SQL ya resuelve; y `ThreadedConnectionPool(1,5)` por lambda contra un plan Neon con tope de conexiones. `RQ-017` |
| RNF-012 Evolución a almacenamiento desacoplado y motor relacional | RNF | **CUMPLIDO** | R2 en `app/storage.py`, Postgres en `app/database.py` | Cumplido de hecho, por un camino distinto al proyectado |
| RNF-020 Estrategia de respaldos periódicos | RNF, MT, CAP-9 | **PARCIAL** | `app/routes/backup.py`, cron en `vercel.json` | Una sola copia, sobre el mismo R2 que guarda los originales: `requerimientos_resumen.tex` exige destino adicional y varias generaciones. `RQ-018` |
| RNF-021 Restauración y recuperación ante contingencias | RNF, CAP-9 | **PARCIAL** | `POST /api/admin/backup/restore?mode=merge\|overwrite` | Existe el endpoint; no existe procedimiento probado ni nadie ha restaurado nunca una copia. `RQ-019` |
| RNF-022 Monitoreo básico de operación e incidentes | RNF, CAP-9 | **PARCIAL** | `app/main.py:715-730` `/api/health` | Nadie lo consulta: sin sonda externa ni alerta, es un endpoint que se mira a mano. `RQ-020` |
| RNF-030 Arquitectura modular y separación de responsabilidades | RNF, MT | **CUMPLIDO** | `app/core/` · `app/routes/` · `models.py` · `utils.py` · `database.py` | — |
| RNF-031 Pruebas unitarias con evolución a integrales | RNF, CAP-10 | **PARCIAL** | 437 pruebas recogidas en `app/tests/` | Ninguna es de integración: `db_query` va mockeado y `require_session` sobrescrito (`SI-226`). El número engaña. `RQ-021` |
| RNF-032 Documentación versionada y trazable | RNF, MT, CAP-10 | **FANTASMA** | `CLAUDE.md`, `CHANGELOG.md`, `docs/tex/` | Está versionada y **es falsa en al menos doce puntos** (ver FANTASMAS). Versionar una descripción incorrecta no da trazabilidad. `RQ-022` |
| RNF-033 Integración continua en cada cambio | RNF, CAP-10 | **AUSENTE** | no existe `.github/`, ni `.gitlab-ci.yml`, ni equivalente | `RQ-023` |
| RNF-034 Entrega continua con compuertas de aprobación | RNF, CAP-10 | **AUSENTE** | despliegue por push a Vercel, sin puerta | `RQ-024` |
| Plataforma: R 4.2.0+, RStudio, paquetes Shiny | RF §plataforma, RS, Glosario | **DERIVA** | Python 3.11 + FastAPI (`app/main.py`, `.python-version`) | La memoria describe otro sistema. **Gana el código**; hay que reescribir la memoria. `RQ-001` |
| Hardware: 2 GB RAM, 1-2 vCPU, 40 GB SSD, volumen aparte para escaneados | RS | **AUSENTE** | Vercel serverless: no hay servidor, ni disco, ni volumen | Todo el capítulo de dimensionamiento es inaplicable. `RQ-002` |
| Red: HTTPS, firewall con puertos mínimos, acceso remoto restringido y registrado | RS | **AUSENTE** | no aplica en serverless; no hay documento sustituto | `RQ-025` |
| Guía técnica de instalación, configuración y recuperación | RS | **AUSENTE** | `docs/` no la tiene; `CLAUDE.md` es guía de desarrollo, no de operación | `RQ-026` |
| Soberanía: procesamiento local/institucional | RF §restricciones, CAP-1 | **AUSENTE** | Vercel + Neon + R2 + OpenRouter | `RQ-003` |

### Matriz de trazabilidad (anexo C) y compromisos de capítulo

| Entrada | Origen | Estado | Evidencia | Pendiente |
|---|---|---|---|---|
| La matriz como instrumento de trazabilidad | MT | **FANTASMA** | `annexes/C_matriz_trazabilidad.tex:8-19`: la columna «Evidencia» apunta a capítulos, no a artefactos | `RQ-004` |
| Cobertura de la matriz | MT | **PARCIAL** | 10 de 42 requisitos formales | `RQ-004` |
| «Incorporar nuevos IDs por cada ampliación aprobada» | MT §lineamientos | **AUSENTE** | IA, papelera, versiones, compartición externa, retención, alertas y escáner no tienen ni un ID | `RQ-027` |
| Cuadro de clasificación y descripción multinivel ISAD(G) | CAP-7 §reglas de consistencia | **AUSENTE** | hay campos ISAD(G) sueltos, no jerarquía | ya señalado en el bloque 5 del `README.md` de auditoría |
| Reglas de calidad de metadatos (integridad, completitud, unicidad) | CAP-7 | **PARCIAL** | validadores Pydantic en `app/models.py` | Sin reglas de completitud por tipología ni de unicidad documental. `RQ-028` |
| Políticas de versionado de estructuras de datos | CAP-7 | **PARCIAL** | huella SHA-256 en `run_migrations()` (`app/main.py`) | El mecanismo nunca corre en producción: `IN-001` |
| Ambientes de desarrollo, pruebas y producción | CAP-9 | **AUSENTE** | un único proyecto Vercel, sin entorno de pruebas | `RQ-029` |
| Validación post-despliegue de flujos críticos | CAP-9 | **AUSENTE** | — | `RQ-030` |
| Bitácora de incidentes y RTO | CAP-9 | **AUSENTE** | — | `RQ-031` |
| Indicadores de calidad (cobertura, MTTR, tasa de incidencias) | CAP-10 | **AUSENTE** | no se mide cobertura ni se registran defectos | `RQ-032` |
| Pruebas de aceptación con actores de negocio | CAP-10 | **AUSENTE** | `Pendientes.xlsx` es el sustituto informal | `RQ-033` |
| Comité funcional, comité técnico, RACI | CAP-11 | **AUSENTE** | no hay registro de decisiones ni de aprobaciones | `RQ-034` |
| Registro de decisiones arquitectónicas | CAP-11 | **AUSENTE** | el cambio R/Shiny → FastAPI no está documentado en ninguna parte | `RQ-001` |
| Fase I: cierre funcional y endurecimiento de seguridad | CAP-12 | **PARCIAL** | 1.556 pendientes abiertos y RNF-001 sin implementar | — |
| Fase II: migración a relacional y automatización de respaldos | CAP-12 | **CUMPLIDO** | Neon + cron de `backup/programado` | Adelantada sobre el plan |
| Fase III: búsqueda semántica y analítica | CAP-12 | **DERIVA** | hay asistente IA (`app/core/ai.py`) pero no búsqueda semántica ni embeddings | Se entregó Fase III antes que Fase I, y no la que se prometió. `RQ-035` |
| Capítulo 12 duplica íntegro el capítulo 13 | CAP-12 | **DERIVA** | `chapters/12_roadmap.tex:35-239` reproduce `13_comparativa_dms_shiny.tex` | El PDF entregado saca la comparativa de DMS impresa dos veces. `RQ-036` |

### Pendientes del cliente (`Pendientes.xlsx`)

| Pendiente del cliente | Estado | Evidencia | Nota |
|---|---|---|---|
| Login con la tecla Enter | **CUMPLIDO** | `app/static/login.js:30-31` | — |
| Que no se cierre la sesión con F5 | **PARCIAL** | `app/static/app.js:13-30` (TTL 12 h) + `app/routes/auth.py:106-134` | La sesión sobrevive, pero la validación es asíncrona y la pantalla de login parpadea antes: es exactamente lo que el cliente volvió a reportar en la última columna de la hoja. `RQ-037` |
| Poner logos y resto de encabezados | **PARCIAL** | `app/static/logo.png`, `logoblanco.png`; marca en `app-shell.js:73-77` | Falta el escudo UCV y el encabezado institucional de la Facultad, sobre todo en el reporte impreso. `RQ-038` |
| Cambiar «Tesauro» por «Palabras Clave» | **CUMPLIDO** | `app/static/archive.html:91`; sólo quedan ids internos `#collapse-arch-tesauro` | — |
| «No son folios sino expedientes o ejemplares» | **AUSENTE** | `admin_archive.html:643`, `admin_hr.html:678`, `admin-submit.js:54` siguen diciendo «N° de Folio / Signatura» | Choca con el campo ISAD(G) `numero_folio` de la v3.2.0. `RQ-039` |
| Fecha de sesión en vez de fecha de emisión para actas y resoluciones | **PARCIAL** | `app/static/archive.js:235,251-253` | Sólo cambia la etiqueta del modal; el alta, la edición, la tabla y el CSV siguen diciendo «fecha». `RQ-040` |
| Campo Proyecto (y tipología/año) específico para planos | **FANTASMA** | `app/static/archive.js:242` pinta `doc.proyecto \|\| doc.titulo` y la columna `proyecto` **no existe** | Se ve siempre el título disfrazado de proyecto. `RQ-041` |
| Rango de fechas «no es muy útil» / sincronizar todos los campos de fecha | **AUSENTE** | `fp-rrhh-range` (`hr.html:79`) y controles distintos en cada pantalla | `RQ-042` |
| Quitar la palabra «Condición» del filtro de estados | **CUMPLIDO** | `app/static/hr.html:91` dice «Estado» | — |
| Poner «Cédula» primero en el placeholder del buscador | **CUMPLIDO** | `app/static/hr.html:135` | — |
| Quitar el filtro de personas | **CUMPLIDO** (en la UI) | ausente de `hr.html`; `people_clauses` sigue vivo en `app/routes/hr.py:166` | Código muerto y requisito RF-022 en conflicto. `RQ-009` |
| Poner apellidos completos y nombres | **AUSENTE** | `app/static/hr.js:138,265` usa `persona_raw` sin separar; la vista no expone nombres/apellidos aparte | `RQ-043` |
| Empezar la carga por docentes y luego el resto del personal | **AUSENTE** | no hay campo `tipo_personal` ni filtro docente/administrativo | `RQ-044` |
| Separar colores: administrativo amarillo, docente azul, archivo color manila | **AUSENTE** | la paleta `--viz-*` es por serie de gráfico, no por tipo de personal ni por módulo | `RQ-045` |
| «Expedientes RRHH» es el nombre nuevo | **AUSENTE** | `hr.html:6` «RRHH», `app-shell.js:23` «Personal», `admin_hr.html:6` «Panel RRHH» | `RQ-046` |
| «Archivo Recursos Humanos» en vez de «Institucional» | **AUSENTE** | `archive.html:6`, `app-shell.js:21` | `RQ-046` |
| Buscar por número de acta dentro de expedientes | **AUSENTE** | `app/routes/hr.py:128-155` no consulta `numero_folio` ni ningún identificador documental | `RQ-047` |
| Evitar cerrar los modales fuera; sólo con la X | **CUMPLIDO** | `data-backdrop="static" data-keyboard="false"` en los seis modales | — |
| Cambiar pronto la palabra «metadatos» | **AUSENTE** | queda en `investigacion.html` y en toda la documentación | `RQ-048` |
| Estados: pensionado, jubilado, activo y retirado | **PARCIAL** | `app/main.py:141-151`: la tabla `estados_laborales` se crea sembrando **un solo** valor, «Activo» | Los otros tres no se siembran: el desplegable nace incompleto. `RQ-049` |
| Log de digitalizados / escaneados | **AUSENTE** | `scanner-app/` y `scanner-client.js` existen; ninguna llamada a `log_event` desde el flujo de escaneo | `RQ-050` |
| Fotos asíncronas, descargadas para más adelante | **AUSENTE** | `app/static/hr.js:145-146,266-267` inyecta `<img>` sin `loading="lazy"`, con URL prefirmada por foto | `RQ-051` |
| Configuración de «Partes» para poder nombrarlas | **AUSENTE** | las cuatro Partes son filas fijas de `categoria` sembradas en `main.py`; no hay pantalla para renombrarlas | `RQ-052` |
| Botón de inicio | **AUSENTE** | `SHELL_SECCIONES` (`app-shell.js:16-48`) no tiene enlace a inicio ni existe página de inicio | `RQ-053` |
| Alternar entre Archivo y RRHH desde una cuenta con ambos permisos | **AUSENTE** | el menú muestra las dos secciones, pero no hay conmutador de contexto | `RQ-054` |
| «Mejorar la ingeniería de software / estructura de carpetas» | **PARCIAL** | reestructurado en `core/` + `routes/` | Falta responderle por escrito. `RQ-055` |
| Duda del cliente: ¿SPA o páginas, con framework, cuidando la RAM del servidor? | **AUSENTE** | páginas separadas + JS vanilla; nadie ha contestado | `RQ-055` |

### `docs/funcionalidades.md` — lo que el documento afirma que el sistema ya hace

| Afirmación | Estado | Evidencia |
|---|---|---|
| Búsqueda full-text con fallback | **CUMPLIDO** | `app/routes/archive.py:74+` |
| Filtrado por fechas, tipos, descriptores **y tipo de soporte** | **FANTASMA** | backend en `archive.py:143-145`; `#soporte_archivo` (`archive.js:39`) no existe en ningún HTML |
| Autocompletado en vivo | **CUMPLIDO** | `app/routes/archive.py:279-295` |
| Ordenamiento dinámico | **CUMPLIDO** | `app/routes/archive.py:149-154` |
| Campos ISAD(G) / ISO 15489 | **CUMPLIDO** | `numero_folio`, `soporte`, `numero_paginas`, `idioma`, `plazo_retencion_anios` en `app/main.py` |
| Búsqueda intra-expediente | **PARCIAL** | `app/static/hr.js:414` filtra en memoria sobre lo ya descargado, no consulta el servidor |
| Reporte imprimible del expediente | **CUMPLIDO** | `app/routes/hr.py:445-500` |
| Cumplimiento LOTTT | **PARCIAL** | los campos existen; sin antigüedad calculada, escalafón ni vacaciones no cubre la ley |
| Alertas de jubilación y de vencimiento documental | **CUMPLIDO** | `app/routes/hr_alerts.py` |
| Subida validada a R2 con URL prefirmadas | **CUMPLIDO** | `app/routes/files.py:32-70`, `app/storage.py` |
| Workflow draft → revisión → aprobado → rechazado | **CUMPLIDO** | `app/routes/admin/docs.py:459-475` |
| Versiones de archivos digitales y restauración | **CUMPLIDO** | `app/routes/trash.py:234-271` |
| Papelera con soft-delete, restauración y purga | **CUMPLIDO** | `app/routes/trash.py` (integridad: `OR-010`…`OR-012`) |
| **«Autenticación basada en roles (RBAC)»** | **FANTASMA** | no existe comprobación de rol en ningún endpoint: `app/routes/admin/deps.py:7-27` |
| Contraseñas hasheadas | **CUMPLIDO** | `app/core/security.py:15-16`, bcrypt |
| **«Registro de auditoría Log_Event»** | **PARCIAL** | existe, con actor falsificable y entrega no garantizada (RF-041, RF-042) |
| Export/restore JSON con historial | **CUMPLIDO** | `app/routes/backup.py` |

### `docs/modulo-ia.md`

| Afirmación | Estado | Evidencia |
|---|---|---|
| El perfil sale de la sesión firmada, «nada de lo que mande el navegador influye» | **CUMPLIDO** | `app/routes/ai.py` resuelve el perfil desde la cookie `ds_session` |
| Doble comprobación de módulo (al armar el esquema y al ejecutar) | **CUMPLIDO** | `app/core/ai_tools.py` |
| Sin sesión degrada a `publico` en vez de 401 | **CUMPLIDO** | `app/routes/ai.py` |
| Las propuestas del editor no se ejecutan solas | **CUMPLIDO** | `app/core/ai_proposals.py` |
| Contradicción con la soberanía declarada | **DERIVA** | el contenido documental viaja a OpenRouter; ningún capítulo lo contempla. `RQ-003` |

---

## Pendientes nuevos

Sólo lo que ninguna de las siete auditorías anteriores registró.

### RQ-001 · La memoria técnica describe un sistema que no se construyó
**Prometido:** trece capítulos, un resumen de requerimientos y un glosario que especifican R/Shiny,
con `shiny, bs4Dash, jsonlite, tidyverse, plotly, DT, shinyWidgets` y una comparativa entera
justificando por qué no Python.
**Hoy:** Python 3.11 + FastAPI + Vercel. Ni una línea de R. El cambio de plataforma no está
registrado en ninguna decisión arquitectónica, pese a que `chapters/11_gobernanza_legal.tex` exige
«registro de aprobaciones y decisiones arquitectónicas».
**Debe pasar:** reescribir `04_requerimientos.tex` §plataforma, `06_diseno_tecnico.tex`,
`13_comparativa_dms_shiny.tex`, `A_glosario.tex` y `requerimientos_resumen.tex` con la pila real, y
añadir un registro de decisión que explique por qué se abandonó R/Shiny. **Gana el código**: la
decisión ya está tomada y funciona; lo que está mal es el papel.
**Esfuerzo:** L · **Archivos:** `docs/tex/chapters/04_requerimientos.tex` `[CHOCA]`,
`docs/tex/chapters/06_diseno_tecnico.tex`, `docs/tex/chapters/13_comparativa_dms_shiny.tex`,
`docs/tex/chapters/12_roadmap.tex` `[CHOCA]`, `docs/tex/annexes/A_glosario.tex`,
`docs/tex/requerimientos_resumen.tex`

### RQ-002 · El dimensionamiento de infraestructura es inaplicable
**Prometido:** 1-2 vCPU, 1-4 GB RAM, 40 GB SSD, volumen separado para escaneados, base en la misma
máquina y coste de 6 a 19 USD/mes en Hetzner o Contabo.
**Hoy:** serverless. No hay máquina, ni disco, ni volumen; el coste es variable y repartido en tres
proveedores. Todo el capítulo de costes de VPS orienta una compra que no procede.
**Debe pasar:** sustituir el dimensionamiento por el modelo real —límites de Vercel, plan de Neon,
clase de almacenamiento de R2— con su coste mensual estimado y su umbral de crecimiento.
**Esfuerzo:** M · **Archivos:** `docs/tex/requerimientos_resumen.tex`,
`docs/tex/chapters/04_requerimientos.tex` `[CHOCA]`, `docs/tex/chapters/09_operacion_despliegue.tex`

### RQ-003 · La restricción de soberanía se incumple y nadie lo ha declarado
**Prometido:** «priorizar procesamiento local/institucional» y «soberanía tecnológica basada en
herramientas abiertas».
**Hoy:** aplicación en Vercel (EEUU), base en Neon (otro continente, según `CLAUDE.md`), archivos en
Cloudflare R2 y contenido documental enviado a OpenRouter. Datos personales del personal —cédula,
fecha de nacimiento, sexo, nivel educativo— fuera del país.
**Debe pasar:** o se declara formalmente la excepción con su análisis de riesgo y el visto bueno
institucional que `11_gobernanza_legal.tex` da por existente, o se plantea el retorno a
infraestructura de la UCV. No puede quedarse sin decidir.
**Esfuerzo:** M (decisión) / L (si se repatría) · **Archivos:**
`docs/tex/chapters/11_gobernanza_legal.tex`, `docs/tex/chapters/08_seguridad_auditoria.tex`,
`docs/tex/chapters/01_resumen_ejecutivo.tex`

### RQ-004 · La matriz de trazabilidad no traza a artefactos y cubre un cuarto de los requisitos
**Prometido:** un anexo que relacione requisito, componente y evidencia verificable.
**Hoy:** diez filas de 42 requisitos, y la evidencia de las diez es «Cap. 4, Cap. 5»: documentos
señalando documentos.
**Debe pasar:** una fila por cada RF y RNF, con la evidencia apuntando a `archivo:línea`, endpoint y
prueba que lo cubre; sin prueba, la fila queda marcada como no verificada.
**Esfuerzo:** M · **Archivos:** `docs/tex/annexes/C_matriz_trazabilidad.tex`

### RQ-005 · «Credenciales institucionales» son cuentas locales
**Prometido:** RF-001, inicio de sesión con credenciales institucionales.
**Hoy:** `usuarios_sistema` con usuarios creados a mano por un admin. Sin LDAP ni SSO de la UCV: un
docente no entra con su cuenta de la Universidad y la baja de personal no revoca nada.
**Debe pasar:** decidir y documentar. O se integra el directorio institucional, o RF-001 se
reformula como «credenciales propias del sistema» y se añade el procedimiento de alta y baja.
**Esfuerzo:** L (integración) / S (reformular) · **Archivos:** `app/routes/auth.py` `[CHOCA]`,
`docs/tex/chapters/04_requerimientos.tex` `[CHOCA]`

### RQ-006 · El cierre de sesión no invalida el token ni se audita
**Prometido:** RF-003, cierre de sesión **seguro**.
**Hoy:** `app/routes/auth.py:142-146` borra la cookie. El token HMAC sigue siendo válido hasta
caducar, así que quien lo tuviera copiado sigue dentro; y a diferencia del login, el logout no llama
a `log_event`, con lo que la sesión no tiene cierre en la auditoría.
**Debe pasar:** lista de revocación —o versión de sesión por usuario, invalidando al cerrar— y un
`log_event` de «Logout».
**Esfuerzo:** M · **Archivos:** `app/routes/auth.py` `[CHOCA]`, `app/core/security.py` `[CHOCA]`

### RQ-007 · El filtro por soporte existe en el servidor y no hay control que lo dispare
**Prometido:** `docs/funcionalidades.md` §1 lo lista como característica del buscador.
**Hoy:** `app/routes/archive.py:143-145` y `:246-248` lo aceptan y filtran; `archive.js:39` lee
`#soporte_archivo`, elemento que no está en `archive.html` ni en ningún otro HTML. Manda siempre
cadena vacía. Además `archive.js:163` usa el icono `fa-scanner`, que no existe en FontAwesome, así
que los digitalizados salen con el icono de reserva.
**Debe pasar:** añadir el selector al acordeón de filtros junto a Palabras Clave y cambiar
`fa-scanner` por un icono real.
**Esfuerzo:** S · **Archivos:** `app/static/archive.html` `[CHOCA]`,
`app/static/archive.js` `[CHOCA]`

### RQ-008 · Se promete XLS y se entrega un CSV de la página actual
**Prometido:** RF-016 y RF-025, exportación a formato XLS.
**Hoy:** `app/static/app.js:381-409` genera un CSV, bajo un botón llamado `#download_archivo_xls`.
Peor: exporta `state.results` —lo que hay en pantalla— no el resultado filtrado completo, así que una
búsqueda de 900 documentos exporta 20 sin avisar.
**Debe pasar:** endpoint de exportación en el servidor que reciba los mismos filtros y devuelva el
conjunto completo; XLSX real si el requisito se mantiene, o corregir el requisito a CSV. Y renombrar
el botón para que no mienta.
**Esfuerzo:** M · **Archivos:** `app/static/app.js` `[CHOCA]`, `app/routes/archive.py` `[CHOCA]`,
`app/routes/hr.py` `[CHOCA]`, `docs/tex/chapters/04_requerimientos.tex` `[CHOCA]`

### RQ-009 · El filtro de personas: código muerto en el servidor y requisito vivo en el papel
**Prometido:** RF-022 lo exige; el cliente pidió quitarlo.
**Hoy:** el control se retiró de `hr.html`; `app/routes/hr.py:166` conserva `people_clauses` y el
modelo sigue aceptando `people_terms`. Queda una entrada de filtrado sin uso legítimo y un requisito
que la auditoría marcará como incumplido para siempre.
**Debe pasar:** retirar `people_terms` del modelo y de la consulta, y borrar la mención en RF-022.
**Gana el cliente.**
**Esfuerzo:** S · **Archivos:** `app/routes/hr.py` `[CHOCA]`, `app/models.py` `[CHOCA]`,
`docs/tex/chapters/04_requerimientos.tex` `[CHOCA]`

### RQ-010 · RRHH no deja ordenar los resultados
**Prometido:** RF-024, orden alfabético y cronológico.
**Hoy:** Archivo tiene `sort_map` (`app/routes/archive.py:149-154`); `hr.py` no tiene equivalente y
el orden lo fija la consulta. El buscador de personal no ofrece el control.
**Debe pasar:** `sort_map` con apellido A-Z/Z-A y fecha de ingreso ascendente/descendente, y su
selector en `hr.html`, igual que en Archivo.
**Esfuerzo:** M · **Archivos:** `app/routes/hr.py` `[CHOCA]`, `app/models.py` `[CHOCA]`,
`app/static/hr.html` `[CHOCA]`, `app/static/hr.js` `[CHOCA]`

### RQ-011 · No existe la exportación del expediente consolidado
**Prometido:** RF-025 y `05_diseno_funcional.tex` §flujo de expediente, paso 4.
**Hoy:** `app/routes/hr.py:445` devuelve un HTML para imprimir. Quien necesite los datos para
cruzarlos —que es para lo que sirve una exportación— no tiene por dónde.
**Debe pasar:** `GET /api/rrhh/expediente/{id}/export` con el expediente completo, documentos e
historial de cargos, en el formato que se decida en `RQ-008`.
**Esfuerzo:** M · **Archivos:** `app/routes/hr.py` `[CHOCA]`, `app/static/hr.js` `[CHOCA]`

### RQ-012 · Las estadísticas no se pueden acotar
**Prometido:** RF-032, filtros analíticos por módulo y metadatos.
**Hoy:** `app/routes/admin/stats.py` devuelve totales históricos. No hay rango de fechas, ni corte
por tipo, ni por departamento; el panel responde «cuántos hay», nunca «cuántos este año», que es la
pregunta de la que sale un informe de gestión.
**Debe pasar:** parámetros `desde`/`hasta`/`tipo`/`departamento` en los endpoints de cifras y
gráficas, con su control en la cabecera del panel.
**Esfuerzo:** M · **Archivos:** `app/routes/admin/stats.py` `[CHOCA]`,
`app/static/admin-stats.js` `[CHOCA]`, `app/static/admin-charts.js` `[CHOCA]`

### RQ-013 · Ni las consultas ni las exportaciones dejan rastro
**Prometido:** RF-040 y `08_seguridad_auditoria.tex`: «registro de eventos críticos: acceso,
**consulta**, **exportación** y administración».
**Hoy:** de 35 `log_event`, ninguno en `archive.py` ni en la búsqueda de `hr.py`. La exportación
ocurre entera en el navegador (`app.js:381`), así que el servidor ni se entera de que alguien se
llevó el listado de personal. Para un archivo, saber quién consultó qué es la mitad del control
interno.
**Debe pasar:** auditar la búsqueda de RRHH —con el término y el número de resultados, no los
datos— y toda exportación, lo que obliga a que la exportación pase por el servidor (`RQ-008`).
**Esfuerzo:** M · **Archivos:** `app/routes/hr.py` `[CHOCA]`, `app/routes/archive.py` `[CHOCA]`,
`app/static/app.js` `[CHOCA]`

### RQ-014 · El actor de la auditoría lo escribe el cliente
**Prometido:** RF-041 («preservar datos de actor») y RNF-003 («trazabilidad integral para **no
repudio**»).
**Hoy:** el nombre viene del cuerpo o de la query: `req.usuario` en
`app/routes/admin/docs.py:247,411,431,604`, `requester` en `docs.py:459` y
`app/routes/admin/users.py:79,88`, `usuario` en `docs.py:436,521,630`. Cualquiera puede borrar un
expediente firmando con el nombre de un compañero. Y `audit_log.csv` sólo contiene tres fallos de
login: no hay corpus real con el que comprobar nada.
**Debe pasar:** el actor sale de `require_session` y de ningún otro sitio; eliminar `usuario`,
`requester` y `creator` de los modelos y de las firmas.
**Esfuerzo:** L · **Archivos:** `app/routes/admin/docs.py` `[CHOCA]`,
`app/routes/admin/users.py` `[CHOCA]`, `app/routes/admin/retention.py` `[CHOCA]`,
`app/routes/admin/imports.py` `[CHOCA]`, `app/routes/trash.py` `[CHOCA]`,
`app/routes/files.py` `[CHOCA]`, `app/routes/hr_alerts.py` `[CHOCA]`, `app/models.py` `[CHOCA]`

### RQ-015 · La auditoría se escribe desde un hilo que puede no llegar a correr
**Prometido:** RF-042, evidencia para análisis forense.
**Hoy:** `app/database.py:174-179` lanza el `INSERT` en un hilo y devuelve la respuesta sin
esperarlo; si falla, se registra en el log de la aplicación y ya. El comentario dice que
`daemon=False` garantiza durabilidad «en entornos serverless», pero Vercel congela la lambda al
responder: el hilo puede quedarse a medias y perderse en silencio. Un registro forense que pierde
eventos sin decirlo es peor que no tenerlo, porque se confía en él.
**Debe pasar:** escritura síncrona dentro de la misma transacción del cambio que se audita para los
eventos críticos, y para los de volumen (consultas) una cola con confirmación.
**Esfuerzo:** M · **Archivos:** `app/database.py` `[CHOCA]`

### RQ-016 · Cuatro rutas en el mismo fichero y sólo dos piden sesión
**Prometido:** RNF-002, mínimo privilegio sobre RRHH.
**Hoy:** en `app/routes/hr.py`, `:387` y `:445` llevan `dependencies=_auth`; `:110` (búsqueda de
personal) y `:300` (perfil completo con cédula, RIF y fechas) no. El fichero se lee como si el
control existiera, y el hueco pasa desapercibido en revisión. La superficie ya está en `BR-001`;
esto es la causa de que nadie lo viera.
**Debe pasar:** que la protección se declare en el `APIRouter`, no ruta a ruta, y que la excepción
sea explícita donde de verdad deba ser pública.
**Esfuerzo:** S · **Archivos:** `app/routes/hr.py` `[CHOCA]`, `app/routes/archive.py` `[CHOCA]`

### RQ-017 · Pandas en la ruta caliente contra un requisito de bajo consumo
**Prometido:** RNF-010 y RNF-011: operar en infraestructura de bajo costo sin degradación.
**Hoy:** `pandas` se importa en `archive.py:3`, `hr.py:4`, `lookups.py:5` y `admin/stats.py:6`, para
tareas que el SQL ya resuelve (agrupar, contar, deduplicar). Son decenas de MB y cientos de
milisegundos en cada arranque en frío, sumados a los viajes de migración que `CLAUDE.md` describe.
Además cada lambda abre su propio `ThreadedConnectionPool(1,5)`: con concurrencia, el plan de Neon
se queda sin conexiones. Es exactamente la preocupación que el cliente escribió en su hoja.
**Debe pasar:** sacar pandas de las rutas —las agregaciones a SQL— y usar el pooler de Neon en vez
de un pool por instancia.
**Esfuerzo:** L · **Archivos:** `app/routes/archive.py` `[CHOCA]`, `app/routes/hr.py` `[CHOCA]`,
`app/routes/lookups.py` `[CHOCA]`, `app/routes/admin/stats.py` `[CHOCA]`,
`app/database.py` `[CHOCA]`, `app/requirements.txt` `[CHOCA]`

### RQ-018 · La copia de seguridad vive en el mismo sitio que el original
**Prometido:** `requerimientos_resumen.tex` §almacenamiento: «debe existir un destino adicional para
copias de seguridad; el repositorio no debe depender de una sola ubicación física» y «conservar
varias generaciones».
**Hoy:** el cron deja la copia en el mismo bucket de R2 donde están los documentos. Un borrado del
bucket, una credencial comprometida —que es el caso, ver FANTASMA F-2— o un error de cuota se lleva
los datos y su respaldo a la vez. Tampoco hay política de generaciones ni de retención.
**Debe pasar:** segundo destino independiente del proveedor principal, rotación con varias
generaciones y verificación de que la copia se puede leer.
**Esfuerzo:** M · **Archivos:** `app/routes/backup.py` `[CHOCA]`, `vercel.json` `[CHOCA]`,
`.env.example` `[CHOCA]`

### RQ-019 · Nadie ha restaurado nunca una copia
**Prometido:** RNF-021 y `09_operacion_despliegue.tex`: procedimientos de restauración y
recuperación.
**Hoy:** existe `POST /api/admin/backup/restore`, pero no hay procedimiento escrito, ni ensayo, ni
constancia de que el JSON exportado se pueda volver a cargar completo. Una restauración que se
prueba por primera vez el día del incidente no es un plan de continuidad.
**Debe pasar:** ensayo de restauración contra una rama de base de datos, documentado, con su tiempo
medido, y repetido cada trimestre.
**Esfuerzo:** M · **Archivos:** `docs/` (guía nueva), `app/tests/test_backup.py` `[CHOCA]`

### RQ-020 · El health check no lo mira nadie
**Prometido:** RNF-022, monitoreo básico de operación e incidentes.
**Hoy:** `/api/health` (`app/main.py:715`) devuelve conteos. No hay sonda externa, ni alerta, ni
destinatario. La caída se detecta cuando alguien de la Facultad llama.
**Debe pasar:** sonda externa cada cinco minutos con aviso a un responsable nombrado, y que el
endpoint distinga «la base responde» de «la base responde con datos coherentes».
**Esfuerzo:** S · **Archivos:** `app/main.py` `[CHOCA]`, `docs/` (guía de operación)

### RQ-021 · 437 pruebas que no pueden fallar por las causas que importan
**Prometido:** RNF-031, «pruebas unitarias y evolución a pruebas integrales»; `10_calidad_qa.tex`
promete además pruebas funcionales, de regresión y de aceptación.
**Hoy:** 437 recogidas, todas unitarias con `db_query` mockeado y `require_session` sobrescrito
(`SI-226`). Ninguna ejecuta SQL real ni puede detectar una autorización ausente. Y la cifra que se
publica varía según el documento: 361 en el `CHANGELOG`, 421 en el `README` de auditoría, 437 al
recogerlas hoy.
**Debe pasar:** capa de integración contra una rama efímera de Neon que cubra los cinco flujos
críticos —login, alta, búsqueda, importación, respaldo— y una sola fuente para el número.
**Esfuerzo:** L · **Archivos:** `app/tests/conftest.py` `[CHOCA]`, `app/tests/` `[CHOCA]`,
`CHANGELOG.md` `[CHOCA]`

### RQ-022 · La documentación versionada describe un sistema que no existe
**Prometido:** RNF-032, documentación versionada y trazable.
**Hoy:** doce afirmaciones falsas repartidas entre `CLAUDE.md`, `CHANGELOG.md`,
`docs/funcionalidades.md` y `docs/tex/` (sección FANTASMAS). El daño no es cosmético: `CLAUDE.md` es
lo que lee cada agente antes de tocar el código, así que cada afirmación falsa se propaga a las
decisiones siguientes.
**Debe pasar:** una pasada de corrección sobre los cuatro documentos y una regla operativa: nada se
declara corregido en el `CHANGELOG` sin una prueba que lo sostenga.
**Esfuerzo:** M · **Archivos:** `CLAUDE.md` `[CHOCA]`, `CHANGELOG.md` `[CHOCA]`,
`docs/funcionalidades.md` `[CHOCA]`

### RQ-023 · No hay integración continua
**Prometido:** RNF-033 y `10_calidad_qa.tex` §pipeline, con seis pasos detallados.
**Hoy:** no existe `.github/`, ni `.gitlab-ci.yml`, ni equivalente. Las 437 pruebas sólo corren si
alguien se acuerda. Nada impide que llegue a `main` el `SyntaxError` que ya llegó dos veces según el
`CHANGELOG` 3.3.0.
**Debe pasar:** flujo que corra `pytest`, `pyflakes` y `node --check` en cada PR y en cada push a
`main`, bloqueando la fusión.
**Esfuerzo:** S · **Archivos:** `.github/workflows/ci.yml` (nuevo), `requirements-dev.txt` `[CHOCA]`

### RQ-024 · No hay compuerta antes de producción
**Prometido:** RNF-034, entrega continua con compuertas de aprobación.
**Hoy:** un push despliega a Vercel. Sin entorno de pruebas (`RQ-029`), sin revisión obligatoria y
sin validación posterior (`RQ-030`), la primera persona que ve un cambio en producción es un
usuario.
**Debe pasar:** despliegue de previsualización obligatorio, aprobación de una segunda persona y
promoción manual a producción.
**Esfuerzo:** M · **Archivos:** `vercel.json` `[CHOCA]`, `.github/workflows/` (nuevo)

### RQ-025 · Los controles de red del resumen de requerimientos no tienen equivalente
**Prometido:** HTTPS, firewall con puertos mínimos, administración remota restringida con
credenciales individuales y registro de actividad.
**Hoy:** en serverless nada de eso aplica tal cual, y no se ha escrito el control sustituto: quién
tiene acceso al panel de Vercel, a Neon y a Cloudflare, con qué cuenta y con qué registro. Hoy el
control de acceso a producción es informal.
**Debe pasar:** inventario de cuentas administrativas de los tres proveedores, MFA obligatorio y
revisión periódica, sustituyendo la sección de red del resumen. Sin credenciales en el repositorio.
**Esfuerzo:** S · **Archivos:** `docs/tex/requerimientos_resumen.tex`, `docs/` (inventario)

### RQ-026 · No existe la guía técnica de instalación y recuperación
**Prometido:** `requerimientos_resumen.tex` §guía técnica: «que otra persona pueda operar o
restaurar el sistema sin depender de que alguien se acuerde de todo».
**Hoy:** `CLAUDE.md` explica cómo desarrollar; nada explica cómo desplegar desde cero, qué variables
hay que fijar en Vercel, cómo crear la base o cómo recuperar. Es el requisito que hace que el
sistema sobreviva al equipo que lo hizo.
**Debe pasar:** guía de despliegue y recuperación con el orden exacto de pasos, probada por alguien
que no haya tocado el proyecto.
**Esfuerzo:** M · **Archivos:** `docs/` (guía nueva), `.env.example` `[CHOCA]`

### RQ-027 · Siete funcionalidades entregadas sin requisito que las respalde
**Prometido:** `C_matriz_trazabilidad.tex` §lineamientos: «incorporar nuevos IDs por cada ampliación
funcional aprobada».
**Hoy:** asistente IA, papelera, versiones de archivo, compartición externa, retención documental,
alertas de jubilación e integración con escáner están en producción y no tienen ni un ID. Nadie las
aprobó formalmente y nadie puede exigir que funcionen.
**Debe pasar:** RF-050 en adelante para cada una, con su criterio de aceptación, y su fila en la
matriz.
**Esfuerzo:** M · **Archivos:** `docs/tex/chapters/04_requerimientos.tex` `[CHOCA]`,
`docs/tex/annexes/C_matriz_trazabilidad.tex`

### RQ-028 · No hay reglas de completitud ni de unicidad documental
**Prometido:** `07_modelo_datos.tex`: «reglas de calidad para integridad, completitud y unicidad» y
«todo documento debe mantener contexto de origen, tipo y ubicación».
**Hoy:** los validadores de `app/models.py` comprueban tipos y enumerados, no completitud por
tipología: un acta puede guardarse sin fecha de sesión y un plano sin proyecto. Y no hay clave
natural que impida cargar el mismo documento dos veces.
**Debe pasar:** perfil de metadatos obligatorios por tipología, aplicado en el alta y en la
importación, y detección de duplicados por título + fecha + tipo.
**Esfuerzo:** L · **Archivos:** `app/models.py` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`,
`app/routes/admin/imports.py` `[CHOCA]`

### RQ-029 · No existe entorno de pruebas
**Prometido:** `09_operacion_despliegue.tex` §ambientes: desarrollo, pruebas y producción.
**Hoy:** un solo proyecto y una sola base. Probar una migración es probarla contra los datos reales
de la Facultad.
**Debe pasar:** proyecto de previsualización en Vercel apuntando a una rama de Neon, creada y
destruida por PR.
**Esfuerzo:** M · **Archivos:** `vercel.json` `[CHOCA]`, `.github/workflows/` (nuevo), `docs/` (guía)

### RQ-030 · No hay validación post-despliegue
**Prometido:** `09_operacion_despliegue.tex`: «validación post despliegue de flujos críticos de
negocio».
**Hoy:** el despliegue termina cuando Vercel dice que sí. Nadie comprueba que se puede entrar,
buscar y abrir un documento. Los cinco endpoints que respondían 500 en producción (`CLAUDE.md`
§pruebas) habrían caído en la primera comprobación.
**Debe pasar:** prueba de humo automática contra la URL desplegada —login, búsqueda, ficha, health—
que revierta si falla.
**Esfuerzo:** M · **Archivos:** `.github/workflows/` (nuevo), `app/tests/` `[CHOCA]`

### RQ-031 · No hay bitácora de incidentes ni objetivo de recuperación
**Prometido:** `09_operacion_despliegue.tex`: «bitácora de incidentes y tiempos de recuperación
objetivo»; `10_calidad_qa.tex` pide además tasa de incidencias y tiempo medio de resolución.
**Hoy:** nada. Los fallos que documenta el `CHANGELOG` se conocen por el relato de quien los
arregló; no hay serie histórica que permita decir si el sistema mejora.
**Debe pasar:** registro de incidentes con impacto y tiempo de resolución, y un RTO/RPO declarado y
acordado con la Facultad.
**Esfuerzo:** S · **Archivos:** `docs/` (bitácora nueva)

### RQ-032 · Los indicadores de calidad no se miden
**Prometido:** `10_calidad_qa.tex` §indicadores: cobertura por componente, tasa de incidencias,
MTTR, cumplimiento de ventanas de liberación y estabilidad de despliegues.
**Hoy:** ninguno. No se calcula cobertura, no se registran defectos, no hay ventanas de liberación.
Los cinco indicadores del capítulo son papel.
**Debe pasar:** `pytest --cov` en el pipeline con umbral mínimo; los otros cuatro o se instrumentan
o se retiran del documento.
**Esfuerzo:** M · **Archivos:** `.github/workflows/` (nuevo), `requirements-dev.txt` `[CHOCA]`,
`docs/tex/chapters/10_calidad_qa.tex`

### RQ-033 · No hay pruebas de aceptación con el negocio
**Prometido:** `10_calidad_qa.tex` §estrategia y los cinco criterios de aceptación de alto nivel de
`04_requerimientos.tex`.
**Hoy:** los cinco criterios nunca se han verificado formalmente —y al menos el primero, «el usuario
accede únicamente al módulo autorizado por su perfil», se incumple (`IN-131`). La aceptación real
ocurre en `Pendientes.xlsx`, una hoja suelta que nadie procesa.
**Debe pasar:** protocolo de aceptación por criterio, con responsable de Archivo y de RRHH firmando
lo que probaron, y `Pendientes.xlsx` convertido en un registro con estado por punto.
**Esfuerzo:** M · **Archivos:** `docs/` (protocolo nuevo)

### RQ-034 · La gobernanza descrita no tiene ninguna huella
**Prometido:** `11_gobernanza_legal.tex`: comité funcional, comité técnico, coordinación
institucional y un RACI de cuatro procesos.
**Hoy:** ningún acta, ninguna aprobación de despliegue, ningún registro de decisión. El cambio de
plataforma (`RQ-001`) es la prueba: la decisión más grande del proyecto no pasó por ninguno de los
tres órganos que el capítulo declara.
**Debe pasar:** o se constituyen y dejan acta, o el capítulo se reescribe describiendo el gobierno
real —que hoy es una persona decidiendo—. Lo que no puede es afirmar una estructura inexistente.
**Esfuerzo:** S (documental) · **Archivos:** `docs/tex/chapters/11_gobernanza_legal.tex`

### RQ-035 · Se entregó la Fase III antes que la Fase I, y no la que se prometió
**Prometido:** `12_roadmap.tex`: Fase I cierre funcional y endurecimiento de seguridad; Fase III
«búsqueda semántica sobre corpus documental institucional».
**Hoy:** existe un asistente IA completo (`app/core/ai*.py`, `docs/modulo-ia.md`) mientras la Fase I
sigue con 1.556 pendientes y sin autorización en el backend. Y lo entregado no es búsqueda
semántica: no hay embeddings ni índice vectorial, es un chat con herramientas sobre la misma
búsqueda por texto.
**Debe pasar:** o se actualiza el roadmap para reconocer el asistente como entrega de Fase III con su
alcance real, o se declara adelanto de alcance y se vuelve a la Fase I. La búsqueda semántica sigue
pendiente, se llame como se llame.
**Esfuerzo:** S (documental) · **Archivos:** `docs/tex/chapters/12_roadmap.tex` `[CHOCA]`

### RQ-036 · El capítulo 12 contiene entero el capítulo 13
**Prometido:** un roadmap y una comparativa de DMS, cada uno con su capítulo.
**Hoy:** `chapters/12_roadmap.tex:35-239` reproduce literalmente `13_comparativa_dms_shiny.tex`,
ampliado. El PDF entregado imprime la comparativa dos veces y el lector directivo —a quien
`01_resumen_ejecutivo.tex` remite a «capítulos 1 al 4 y 11 al 12»— recibe una comparativa de
infraestructura donde esperaba la hoja de ruta.
**Debe pasar:** cortar el duplicado de `12_roadmap.tex` y dejar la comparativa sólo en el 13.
**Esfuerzo:** S · **Archivos:** `docs/tex/chapters/12_roadmap.tex` `[CHOCA]`

### RQ-037 · Al recargar se ve la pantalla de login antes de volver
**Prometido:** el cliente lo pidió dos veces: «que no se cierre la sesión con F5» y «cuando recarga
pasa por la pantalla de login, no sé si eso esté bueno».
**Hoy:** `app/static/app.js:13-30` lee `localStorage` y valida contra `/api/auth/restore` de fondo,
pero la página se pinta antes de saber el resultado, así que el login asoma y desaparece. Para el
usuario es indistinguible de haber perdido la sesión, y es la razón de que lo reportara dos veces.
**Debe pasar:** estado de carga que no muestre login hasta resolver `restore`, y que la sesión se
prolongue con la actividad en vez de caducar en seco a las 12 h.
**Esfuerzo:** M · **Archivos:** `app/static/app.js` `[CHOCA]`, `app/static/login.js` `[CHOCA]`,
`app/static/styles.css` `[CHOCA]`

### RQ-038 · Faltan los encabezados institucionales
**Prometido:** cliente, «poner logos y resto de encabezados».
**Hoy:** hay `logo.png` y `logoblanco.png` y la marca del menú es un favicon de 28 px
(`app/static/app-shell.js:73-77`). No aparecen el escudo de la UCV ni el encabezado de la Facultad
de Ciencias en las páginas ni en el reporte de expediente —que es el que sale impreso y va a un
trámite—.
**Debe pasar:** encabezado institucional en la cáscara y, sobre todo, en el reporte imprimible.
**Esfuerzo:** S · **Archivos:** `app/static/app-shell.js` `[CHOCA]`, `app/routes/hr.py` `[CHOCA]`,
`app/static/styles.css` `[CHOCA]`

### RQ-039 · Se sigue diciendo «folio» donde el cliente dijo «expediente o ejemplar»
**Prometido:** cliente, «no son folios sino expedientes o ejemplares».
**Hoy:** «N° de Folio / Signatura» en `app/static/admin_archive.html:643`,
`app/static/admin_hr.html:678` y `app/static/admin-submit.js:54`, y «N° Folio» en las cabeceras del
CSV (`admin-monitor.js:192`, `admin-edit-hr.js:109`). Aquí hay un choque real: `numero_folio` es el
campo ISAD(G) que introdujo la v3.2.0, y el cliente está diciendo que su unidad de descripción no es
el folio.
**Debe pasar:** resolverlo con el archivista antes de tocar nada. Si la unidad es el expediente, hay
que renombrar la etiqueta (no la columna) y revisar si además falta un nivel de descripción, que es
el cuadro de clasificación ISAD(G) ya recogido en el bloque 5 del `README` de auditoría.
**Esfuerzo:** M · **Archivos:** `app/static/admin_archive.html` `[CHOCA]`,
`app/static/admin_hr.html` `[CHOCA]`, `app/static/admin-submit.js` `[CHOCA]`,
`app/static/admin-monitor.js` `[CHOCA]`, `app/static/admin-edit-hr.js` `[CHOCA]`

### RQ-040 · «Fecha de sesión» sólo cambia una etiqueta del modal
**Prometido:** cliente, «fecha de sesión en vez de fecha de emisión, sólo para actas y
resoluciones».
**Hoy:** `app/static/archive.js:251-253` cambia el rótulo al abrir la ficha. El formulario de alta,
el de edición, la tabla del monitor y el CSV siguen diciendo «fecha», y no existe distinción de
significado en la base: quien carga un acta no sabe qué fecha le están pidiendo, que es justo donde
se produce el error.
**Debe pasar:** que la etiqueta dependa de la tipología también en el alta y en la edición.
**Esfuerzo:** M · **Archivos:** `app/static/admin-submit.js` `[CHOCA]`,
`app/static/admin-edit.js` `[CHOCA]`, `app/static/admin-monitor.js` `[CHOCA]`

### RQ-041 · El campo «Proyecto» de los planos no existe
**Prometido:** cliente, «sólo los planos tienen un campo proyecto; campos específicos proyecto y
título; algunos tienen sólo año y tipología».
**Hoy:** `app/static/archive.js:242` pinta `doc.proyecto || doc.titulo`. No hay columna `proyecto` en
la base, ni campo en el alta, ni en la edición, ni en la importación: la fila «Proyecto» muestra
siempre el título, y quien la lea creerá que es el proyecto. Además la vista de plano pierde
soporte, folio y páginas, que sí se muestran en el resto de tipologías.
**Debe pasar:** columna `proyecto` con migración, campo en alta y edición visible sólo para tipología
plano, y esquema de metadatos por tipología (`RQ-028`) para el caso «sólo año y tipología».
**Esfuerzo:** L · **Archivos:** `app/main.py` `[CHOCA]`, `app/models.py` `[CHOCA]`,
`app/routes/archive.py` `[CHOCA]`, `app/routes/admin/docs.py` `[CHOCA]`,
`app/static/archive.js` `[CHOCA]`, `app/static/admin-submit.js` `[CHOCA]`,
`app/static/admin-edit.js` `[CHOCA]`

### RQ-042 · Los controles de fecha son distintos en cada pantalla y el rango no sirve
**Prometido:** cliente, «rango de fechas no es muy útil» y «sincronizar todos los campos de fechas y
ver si se pueden mejorar aún más».
**Hoy:** `app/static/hr.html:79` usa un rango de sólo lectura; Archivo usa otro control; el alta usa
`<input type="date">`. Tres interacciones distintas para el mismo concepto, y el rango no ofrece los
atajos que resuelven el 90 % de los casos («este año», «últimos 5 años», «antes de»), ni acepta un
año suelto, que es como se busca en un archivo histórico.
**Debe pasar:** un solo componente de fecha en `app-choices.js` reutilizado en las cuatro pantallas,
con atajos y con entrada de año.
**Esfuerzo:** M · **Archivos:** `app/static/app-choices.js` `[CHOCA]`,
`app/static/hr.html` `[CHOCA]`, `app/static/archive.html` `[CHOCA]`,
`app/static/styles.css` `[CHOCA]`

### RQ-043 · Nombres y apellidos no están separados
**Prometido:** cliente, «poner apellidos completos y nombres».
**Hoy:** la vista expone `persona_raw` y la UI la pinta tal cual (`app/static/hr.js:138,265`). Sin
separación no se puede ordenar por apellido —que es como se busca a una persona en un archivo de
personal— ni presentar «Apellidos, Nombres» en el listado y en el reporte.
**Debe pasar:** exponer `apellidos` y `nombres` por separado en `vw_rrhh_persona_index`, presentar
«Apellidos, Nombres» en listado, ficha y reporte, y ordenar por apellido (enlaza con `RQ-010`).
**Esfuerzo:** M · **Archivos:** `app/main.py` `[CHOCA]`, `app/routes/hr.py` `[CHOCA]`,
`app/static/hr.js` `[CHOCA]`

### RQ-044 · No se distingue docente de administrativo
**Prometido:** cliente, «se empezará con la carga de docentes, luego el resto del personal».
**Hoy:** no hay campo `tipo_personal` ni filtro; `empleados` sólo tiene cargo y departamento. No se
puede saber cuánto se lleva cargado de docentes, que es justo el indicador que el cliente necesita
para su plan de carga.
**Debe pasar:** campo `tipo_personal` (docente / administrativo / obrero), filtro en el buscador y
KPI de avance de carga por tipo.
**Esfuerzo:** M · **Archivos:** `app/main.py` `[CHOCA]`, `app/models.py` `[CHOCA]`,
`app/routes/hr.py` `[CHOCA]`, `app/static/hr.html` `[CHOCA]`,
`app/routes/admin/stats.py` `[CHOCA]`

### RQ-045 · No hay código de color por tipo de personal ni por módulo
**Prometido:** cliente, «separar colores: administrativos amarillo, docente azul, archivo como color
sobre manila o cartón».
**Hoy:** la paleta `--viz-*` es por serie de gráfico. No hay color asociado al tipo de personal ni
identidad cromática por módulo: Archivo y RRHH se ven idénticos, y el cliente está pidiendo el código
de color del archivo físico, que es como su equipo ya distingue las carpetas.
**Debe pasar:** tokens semánticos `--mod-archivo` / `--mod-rrhh` y `--personal-docente` /
`--personal-administrativo`, validados en claro y oscuro con los mismos criterios de contraste que
`sistema-diseno.md`, aplicados a la cáscara, las tarjetas y los distintivos. Depende de `RQ-044`.
**Esfuerzo:** M · **Archivos:** `app/static/styles.css` `[CHOCA]`,
`app/static/app-shell.js` `[CHOCA]`, `app/static/hr.js` `[CHOCA]`,
`app/static/archive.js` `[CHOCA]`

### RQ-046 · El producto no se llama como el cliente decidió que se llama
**Prometido:** cliente, «Expedientes RRHH ese es el nuevo nombre» y «Archivo Recursos Humanos en vez
de Institucional».
**Hoy:** «RRHH» en `hr.html:6`, «Personal» en `app-shell.js:23`, «Panel RRHH» en `admin_hr.html:6`,
«Archivo Institucional» en `archive.html:6` y en `app-shell.js:21`. Tres nombres distintos para el
mismo módulo dentro de la misma aplicación, y ninguno es el que el cliente fijó.
**Debe pasar:** fijar la nomenclatura con el cliente y aplicarla en un solo barrido —cáscara,
títulos, encabezados, reportes y documentación— añadiéndola a la sección de terminología de
`CLAUDE.md` junto a la regla de «Palabras Clave».
**Esfuerzo:** M · **Archivos:** `app/static/app-shell.js` `[CHOCA]`, los siete HTML de
`app/static/` `[CHOCA]`, `CLAUDE.md` `[CHOCA]`

### RQ-047 · No se puede buscar un acta por su número dentro de un expediente
**Prometido:** cliente, «permitir por ejemplo número de acta también dentro de expedientes».
**Hoy:** `app/routes/hr.py:128-155` busca sobre `persona_raw`, `cedula`, `rif`, `cargo` y
`departamento`. Ningún identificador documental entra en la consulta, ni `numero_folio`, ni el título
del documento. Quien llega con «necesito el acta 145» no tiene por dónde empezar.
**Debe pasar:** ampliar el FTS de RRHH a los documentos del expediente (título, folio, notas) y
mostrar en el resultado qué documento produjo la coincidencia, no sólo la persona.
**Esfuerzo:** M · **Archivos:** `app/routes/hr.py` `[CHOCA]`, `app/main.py` `[CHOCA]` (índice GIN),
`app/static/hr.js` `[CHOCA]`

### RQ-048 · «Metadatos» sigue en la interfaz y en toda la documentación
**Prometido:** cliente, «pronto cambiar la palabra metadatos».
**Hoy:** aparece en `app/static/investigacion.html` —página que además no debería ser pública,
`SI-156`—, en `docs/funcionalidades.md`, en `07_modelo_datos.tex` y en los requisitos. El cliente
está diciendo que su equipo no usa esa palabra.
**Debe pasar:** acordar el término —«datos de descripción», «ficha»— y sustituirlo en la interfaz; en
la memoria técnica puede quedarse, porque el destinatario es otro.
**Esfuerzo:** S · **Archivos:** `app/static/*.html` `[CHOCA]`, `CLAUDE.md` `[CHOCA]` (terminología)

### RQ-049 · El catálogo de estados laborales nace con un solo valor
**Prometido:** cliente, «pensionado, jubilado, activo y retirado».
**Hoy:** `app/main.py:141-151` crea `estados_laborales` y siembra únicamente «Activo». Los otros tres
no existen hasta que alguien los teclee, así que en una instalación limpia el desplegable ofrece un
único estado y todo el mundo queda «Activo». Es además el campo del que dependen las alertas de
jubilación (`app/routes/hr_alerts.py`).
**Debe pasar:** sembrar los cuatro con `ON CONFLICT DO NOTHING`, y revisar que las alertas casen con
esos nombres exactos.
**Esfuerzo:** S · **Archivos:** `app/main.py` `[CHOCA]`

### RQ-050 · La digitalización no deja registro
**Prometido:** cliente, «log de digitalizados / escaneados».
**Hoy:** `scanner-app/` y `app/static/scanner-client.js` existen y `app/routes/files.py:70` audita la
subida a R2, pero el acto de escanear —quién, qué documento, cuántas páginas, con qué resultado— no
se registra. Para un proyecto cuyo trabajo diario es digitalizar, es el indicador de avance que
falta.
**Debe pasar:** evento de auditoría por digitalización con documento, páginas y operador, y un panel
de avance de digitalización por lote.
**Esfuerzo:** M · **Archivos:** `app/static/scanner-client.js` `[CHOCA]`,
`app/routes/files.py` `[CHOCA]`, `app/routes/admin/stats.py` `[CHOCA]`

### RQ-051 · Las fotos se descargan todas de golpe
**Prometido:** cliente, «foto asíncrona, descargadas para más adelante».
**Hoy:** `app/static/hr.js:145-146` inyecta un `<img>` por resultado, sin `loading="lazy"`, y cada
uno resuelve una URL prefirmada. Una página de resultados dispara decenas de descargas y decenas de
firmas antes de que el usuario mire ninguna. En la red de la Facultad eso es la diferencia entre útil
e inservible.
**Debe pasar:** `loading="lazy"` y `decoding="async"`, miniatura en vez del original, y firmar la URL
sólo cuando la imagen entra en pantalla.
**Esfuerzo:** M · **Archivos:** `app/static/hr.js` `[CHOCA]`, `app/routes/files.py` `[CHOCA]`

### RQ-052 · Las cuatro Partes no se pueden renombrar
**Prometido:** cliente, «configuración de partes para tener nombre/estado».
**Hoy:** las Partes I-IV son filas de `categoria` sembradas en `app/main.py` con slug fijo, y el
nombre está además escrito en la interfaz y en `CLAUDE.md`. Cambiar «Parte II (Escalafón)» exige
tocar código.
**Debe pasar:** que el nombre visible salga del catálogo y sea editable desde el panel, conservando el
slug como clave interna, con `invalidate_choices_cache()` al guardar.
**Esfuerzo:** M · **Archivos:** `app/routes/admin/catalog.py` `[CHOCA]`,
`app/static/admin-categories.js` `[CHOCA]`, `app/routes/lookups.py` `[CHOCA]`

### RQ-053 · No hay página ni botón de inicio
**Prometido:** cliente, «botón de inicio».
**Hoy:** `SHELL_SECCIONES` (`app/static/app-shell.js:16-48`) empieza directamente en Módulos. No hay
destino neutro: quien entra aterriza donde le deje su rol y no tiene a dónde volver. Es también el
sitio natural para el encabezado institucional de `RQ-038` y para el conmutador de `RQ-054`.
**Debe pasar:** página de inicio con acceso a los módulos autorizados, últimas incorporaciones y
avisos, enlazada desde la marca de la cáscara, con su `data-page`, su `<div>` de panel y su rama en
`configureSidebarVisibilities()` —los tres pasos que exige `test_paginas.py`—.
**Esfuerzo:** M · **Archivos:** `app/static/app-shell.js` `[CHOCA]`, `app/static/app.js` `[CHOCA]`,
`app/routes/pages.py` `[CHOCA]`, `app/static/inicio.html` (nuevo),
`app/tests/test_paginas.py` `[CHOCA]`

### RQ-054 · No hay conmutador entre Archivo y RRHH
**Prometido:** cliente, «analizar la posibilidad de usar los dos al mismo tiempo desde una cuenta con
permisos para ambos, y que haya un botón que permita cambiar».
**Hoy:** un usuario Global ve las dos secciones en el menú lateral, pero no hay contexto activo ni
conmutador: cada salto es navegar a otra página y perder el estado de búsqueda.
**Debe pasar:** conmutador de módulo en la barra superior, visible sólo con ambos módulos, que
recuerde el contexto y tiña la interfaz con el color de `RQ-045`. Y responder al cliente por escrito,
que es lo que pidió.
**Esfuerzo:** M · **Archivos:** `app/static/app-shell.js` `[CHOCA]`, `app/static/app.js` `[CHOCA]`,
`app/static/styles.css` `[CHOCA]`

### RQ-055 · Dos preguntas de arquitectura del cliente sin responder
**Prometido:** el cliente pregunta si conviene seguir con una sola aplicación o pasar a páginas, si
usar un framework, y recalca «es súper importante mantener los principios de ligereza de RAM a nivel
de servidor»; y por otro lado pide «mejorar la ingeniería de software o la estructura de carpetas».
**Hoy:** el sistema ya es de páginas separadas con JS vanilla —es decir, la respuesta técnica ya se
tomó— pero nadie se lo ha dicho, y su preocupación por la RAM está viva y sin atender (`RQ-017`).
**Debe pasar:** una nota breve al cliente explicando qué se eligió, por qué, y qué se está haciendo
con el consumo; y añadir esa decisión al registro que reclama `RQ-034`.
**Esfuerzo:** S · **Archivos:** `docs/` (nota de decisión)

---

## FANTASMAS

Afirmaciones de la documentación que el código desmiente. Ordenadas por lo que cuesta creerlas.

### F-1 · `CLAUDE.md` afirma dos veces que `test_secrets.py` existe
`CLAUDE.md` lo describe en §Pruebas y remata en §Variables de entorno: «**Ninguna credencial se
escribe en el código**: `test_secrets.py` lo verifica en cada corrida». El `CHANGELOG` 3.3.0 lo
cuenta entre las guardas nuevas. `app/tests/test_secrets.py` **no existe**: hay 19 ficheros de prueba
y ninguno es ése. Ya recogido como `IN-002`; se repite aquí porque es el patrón que explica los
demás.

### F-2 · El `CHANGELOG` da por corregida la fuga de R2 y las claves siguen en el código
«Las credenciales de Cloudflare R2 estaban escritas en `storage.py`. **Pasan a leerse del
entorno**», v3.3.0 §Seguridad. En `app/storage.py:22-26` está el `# TODO: Mover a variables de
entorno cuando se configure en Vercel` con el endpoint, la access key y la secret key literales. La
docstring del propio fichero (líneas 1-11) también afirma que se leen del entorno. Repositorio
público. Ya recogido como `IN-002`. **El token sigue vivo mientras no se rote en Cloudflare.**

### F-3 · `docs/funcionalidades.md` declara RBAC y no hay control de rol en ningún endpoint
§7: «Autenticación Basada en Roles (RBAC): accesos controlados según módulo asignado y rol». El único
control es `require_session` (`app/routes/admin/deps.py:7-27`), que devuelve un nombre. Ni un
endpoint compara rol ni módulo. `IN-131`, `SI-002`. Lo que este carril añade: RBAC no es sólo una
función ausente, es un **criterio de aceptación firmado** (`04_requerimientos.tex` §criterios,
punto 1) y una fila de la matriz de trazabilidad (RNF-001) que se declara cubierta contra un capítulo
que se limita a repetirla.

### F-4 · La estrategia de migraciones que nunca corre en producción
`CLAUDE.md` dedica una sección entera a la huella SHA-256 de `run_migrations()`, con reglas
detalladas sobre cuándo se registra y cuándo no. Todo eso vive en el `lifespan` de FastAPI, y
`api/index.py:14` monta `Mangum(app, lifespan="off")`. En Vercel no se ejecuta ninguna. `IN-001`.
Este carril añade la consecuencia documental: `07_modelo_datos.tex` declara «políticas de versionado
de estructuras de datos» y su única evidencia es este mecanismo inerte.

### F-5 · El filtro por soporte, listado como funcionalidad, no tiene control en la interfaz
`docs/funcionalidades.md` §1: «Filtrado de documentos por rango de fechas, tipos de documento,
descriptores libres **y tipo de soporte (Físico, Digital, Digitalizado)**». El backend lo soporta
(`app/routes/archive.py:143-145`, `:246-248`) y el cliente lo lee de `#soporte_archivo`
(`app/static/archive.js:39`), elemento que no existe en ningún HTML del proyecto. Manda siempre
vacío. `RQ-007`.

### F-6 · El campo «Proyecto» de los planos muestra el título
`app/static/archive.js:242` pinta «Proyecto» con `doc.proyecto || doc.titulo`. La columna `proyecto`
no existe en `schema.sql`, ni en las migraciones de `main.py`, ni en `models.py`, ni la devuelve
`archive.py`. La fila siempre cae al fallback. Un usuario que consulte un plano leerá el título bajo
la etiqueta «Proyecto» y lo dará por bueno: es peor que no tener el campo. `RQ-041`.

### F-7 · La memoria técnica describe R/Shiny; el sistema es Python
El entregable institucional completo —trece capítulos, el resumen de requerimientos, el glosario, el
anexo de diagramas— especifica R/Shiny, hasta el detalle de listar los paquetes de R y comparar
proveedores de VPS. No queda ni un archivo `.R`. Es el fantasma de mayor superficie del proyecto:
todo lo demás se contrasta contra un documento que, en su capa de plataforma, es ficción. `RQ-001`.

### F-8 · La matriz de trazabilidad certifica cobertura contra sí misma
`annexes/C_matriz_trazabilidad.tex` presenta diez requisitos «con evidencia». La evidencia de
RNF-001 —segregación por módulo y rol— es «Cap. 4, Cap. 8», dos capítulos que se limitan a repetir el
requisito. Un lector de compliance, a quien `01_resumen_ejecutivo.tex` remite expresamente al anexo
C, concluirá que la segregación está verificada. No existe. `RQ-004`.

### F-9 · «Trazabilidad integral para no repudio», con el actor puesto por el cliente
RNF-003 y `08_seguridad_auditoria.tex` prometen no repudio. El actor de cada evento llega en el
cuerpo o en la query (`app/routes/admin/docs.py:247,411,459`, `app/routes/admin/users.py:79,88`).
Cualquiera con acceso a la API firma un borrado con el nombre de un compañero. Es lo contrario del no
repudio: el registro crea evidencia falsificable, que en un procedimiento disciplinario es peor que
no tener registro. `RQ-014`.

### F-10 · El comentario de `database.py` promete durabilidad que el entorno no permite
`app/database.py:176-178`: «`daemon=False`: el proceso no termina hasta que el INSERT se complete,
**garantizando durabilidad en entornos serverless (Vercel)**». En Vercel la lambda se congela al
devolver la respuesta; el hilo no tiene garantía de terminar, y si falla, el `except` sólo escribe en
el log. El comentario tranquiliza a quien lo lea sobre exactamente lo que no está garantizado.
`RQ-015`.

### F-11 · El número de pruebas dice tres cosas distintas y ninguna significa lo que parece
`CHANGELOG` 3.3.0: «de 87 a 361». `docs/auditoria/README.md`: «Las 421 pruebas pasan».
`pytest --collect-only` hoy: 437. Ninguna toca la base ni puede ver una autorización ausente
(`SI-226`). La cifra se usa como argumento de calidad en un proyecto donde `10_calidad_qa.tex`
promete cobertura medida —que no se mide— y pruebas de aceptación —que no se han hecho—. `RQ-021`.

### F-12 · Capítulos que describen una organización sin ninguna huella
`11_gobernanza_legal.tex` declara tres órganos de decisión y un RACI de cuatro procesos;
`09_operacion_despliegue.tex` declara tres ambientes y validación post-despliegue;
`10_calidad_qa.tex` declara un pipeline de seis pasos y cinco indicadores. No existe ni el pipeline,
ni el entorno de pruebas, ni un acta, ni un indicador medido. Un auditor externo que verifique el
capítulo 11 contra la realidad no encontrará nada que verificar. `RQ-023`, `RQ-024`, `RQ-029`,
`RQ-030`, `RQ-032`, `RQ-034`.

---

## Orden de ataque

Este carril no compite con el orden del `README.md` consolidado: se **inserta** en él. Los bloques 0
a 5 de allí siguen mandando en el código; lo de aquí es lo que hay que hacer para que el papel deje
de mentir y para que el cliente reciba respuesta.

### Bloque A — Hoy, antes que nada (va con el bloque 0 del `README`)
`RQ-014` y `RQ-015`: mientras el actor lo ponga el cliente y el evento pueda perderse, **la auditoría
no sirve como evidencia**, y el sistema entero se justifica ante la Facultad por su capacidad de
auditar. Va junto a la rotación del token de R2 (F-2) y a `IN-002`.

### Bloque B — Que la documentación deje de mentir (una persona, en paralelo con todo)
`RQ-022` primero, porque `CLAUDE.md` es lo que lee cada agente antes de escribir código: cada
afirmación falsa se propaga. Luego `RQ-001`, `RQ-002`, `RQ-036` y `RQ-035` sobre la memoria técnica, y
`RQ-004` + `RQ-027` para que la matriz de trazabilidad trace de verdad. **No toca código: se puede
hacer mientras todo lo demás avanza.**

### Bloque C — La red de seguridad documental (va con el bloque 1 del `README`)
`RQ-023` (CI) y `RQ-021` (pruebas de integración) son la misma pieza que `SI-226` e `IN-001`: hacerlos
juntos. Detrás, `RQ-024`, `RQ-029` y `RQ-030` cierran el ciclo de entrega. `RQ-026` (guía de
recuperación), `RQ-018` (segundo destino de respaldo) y `RQ-019` (ensayo de restauración) van aquí,
porque son lo que convierte el cron en un respaldo de verdad. `RQ-020` y `RQ-031` cierran la
operación.

### Bloque D — Deuda con el cliente, por orden de cuánto le duele
Lo que reportó dos veces primero: `RQ-037` (el login que parpadea al recargar). Luego los baratos y
visibles: `RQ-049` (los cuatro estados), `RQ-007` (el filtro de soporte), `RQ-046` (los nombres),
`RQ-038` (los encabezados), `RQ-048` (metadatos). Después los estructurales: `RQ-041` (proyecto en
planos) y `RQ-039` (folio contra expediente), que hay que sentarse a resolver **con el archivista**
antes de tocar nada, porque chocan con los campos ISAD(G) de la v3.2.0. Y `RQ-055`: contestarle por
escrito las dos preguntas de arquitectura que lleva esperando.

### Bloque E — Los requisitos formales que quedan incumplidos
`RQ-016` y `RQ-006` van con `IN-131`, que es la misma cirugía. `RQ-013` con `RQ-008` y `RQ-011`,
porque auditar la exportación exige antes que la exportación pase por el servidor. `RQ-010`, `RQ-012`,
`RQ-047` y `RQ-043` son buscador y panel: encajan en el bloque 3 del `README`, con sus mismos
ficheros. `RQ-005` y `RQ-025` son decisión antes que código.

### Bloque F — Lo que cambia la forma del producto
`RQ-053` (inicio) → `RQ-054` (conmutador) → `RQ-045` (color por módulo y tipo) es una cadena: cada uno
necesita al anterior, y `RQ-045` necesita además `RQ-044`. Va en el bloque 4 del `README`, con el
sistema de diseño, y después de `BR-109`. `RQ-017` (pandas fuera de la ruta caliente), `RQ-051`
(fotos), `RQ-042` (fechas), `RQ-052` (Partes configurables), `RQ-050` (registro de digitalización),
`RQ-040` (fecha de sesión en el alta), `RQ-028` (calidad de metadatos), `RQ-032` y `RQ-033`
(indicadores y aceptación) y `RQ-034` (gobernanza) cierran.

### Lo que no puede hacer un agente
Rotar el token de R2 (F-2). Decidir si los datos de personal pueden seguir fuera de Venezuela
(`RQ-003`). Decidir si la unidad de descripción es el folio o el expediente (`RQ-039`). Y fijar el
nombre del producto (`RQ-046`). Son cuatro decisiones, no cuatro tareas.
