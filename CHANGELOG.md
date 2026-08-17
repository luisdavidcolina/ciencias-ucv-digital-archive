# Changelog — Archivo Institucional Digital, Facultad de Ciencias UCV

Todos los cambios notables a este proyecto están documentados en este archivo.
Formato basado en [Keep a Changelog](https://keepachangelog.com/es/1.0.0/).
Este proyecto usa [Semantic Versioning](https://semver.org/lang/es/).

---

## [3.3.0] — 2026-08-17

### Corregido — Fallos que impedían cargar

- `app-theme.js` y `admin-edit.js` no parseaban: comillas tipográficas usadas
  como delimitador de string. Un `SyntaxError` duro deja el archivo entero sin
  cargar, así que el panel de personalización completo (modo oscuro, temas,
  densidad, notificaciones) y la edición de documentos estaban muertos.
- `core/ai.py` llamaba a `modelo_actual()` y `catalogo_modelos()` en cinco
  sitios tras haber renombrado esas definiciones. Cada llamada era un
  `NameError`: el chat, la validación de modelo y el endpoint de disponibilidad
  respondían 500.
- La pestaña Papelera nunca llamaba a `loadPapelera()`: abría una tabla vacía.
- El panel entraba por "Monitor" mientras el marcado marcaba "Análisis" como
  activa; como los KPIs de cabecera solo se llenan en esa rama, la fila entera
  se quedaba en cero al abrir.
- El monitor de RRHH emitía siete celdas bajo un encabezado de seis columnas:
  la tabla salía corrida y cada dato aparecía bajo el título equivocado. En
  Archivo las etiquetas también estaban desplazadas.
- El panel de Sistema usa `info-box` 58 veces, pero AdminLTE nunca se carga:
  las cajas salían sin estructura, con el fondo del icono ocupando el ancho
  completo y la cifra encima, ilegible.

### Corregido — Codificación

- Texto doble-codificado (UTF-8 guardado como cp1252) en backend, documentación
  y estáticos. No era solo cosmético: en `docs.py` la clase de caracteres del
  fallback de búsqueda estaba corrupta, y en `retention.py` y `docs.py` el daño
  estaba dentro de literales SQL, así que `COALESCE(autor,'—')` devolvía texto
  corrupto al usuario. Se eliminan además 20 BOM.

### Cambiado — Backoffice

- Las pestañas pasan de siete planas a nueve agrupadas por intención
  (operación diaria · configuración · gobernanza documental), con separador
  visible. "Auditoría" era un cajón de sastre con auditoría, retención vencida
  y exportación; la retención estaba partida en dos pestañas y ahora está
  completa en una. Mismo criterio aplicado al panel de Sistema.
- El pane de analíticas apilaba dos generaciones de gráficos: "Distribución por
  Tipo" duplicaba "Documentos por Tipo", "Cronología" duplicaba "Documentos por
  Año" y "Total Documentos" aparecía dos veces. Se elimina la generación vieja,
  incluido "Estado del Sistema", que reportaba RAM y CPU del lambda de Vercel.
- La paleta de gráficos fallaba tres comprobaciones de accesibilidad (banda de
  luminosidad, piso de croma, separación a visión normal). Se sustituye por una
  validada en claro y oscuro contra las superficies reales, expuesta como
  tokens `--viz-*` que los gráficos leen en runtime.
- La consola de IA vivía fuera del sistema de diseño: sin `styles.css`, con su
  propio verde y sin modo oscuro. Queda integrada.

### Añadido

- Capa de movimiento: entrada escalonada de resultados, transición entre
  pestañas y realce al pasar por encima de lo accionable. `prefers-reduced-motion`
  ahora apaga la animación globalmente, no solo en una regla suelta.
- Responsive verificado a 390 px: sin desborde horizontal en ninguna página ni
  pestaña. El logo ocupaba 241 px de 390 y expulsaba de la barra al usuario y al
  botón de salir.
- `requirements-dev.txt` con pytest, httpx y pyflakes.

### Seguridad

- Las credenciales de Cloudflare R2 estaban escritas en `storage.py`. Pasan a
  leerse del entorno. **Siguen en el historial de git y el repositorio es
  público: hay que rotarlas en Cloudflare.**

### Pruebas — de 87 a 361

Guardas nuevas, cada una verificada contra el código roto que la motivó:

- `test_static_assets.py` — UTF-8 válido, sin BOM ni doble codificación, y
  `node --check` sobre cada `.js`.
- `test_static_analysis.py` — un nombre indefinido es un error; sin imports
  muertos; todos los módulos importan.
- `test_secrets.py` — sin credenciales en el código, sin `.env` versionado.
- `test_admin_panels.py` — cada pestaña tiene panel y rama de carga, ambos
  módulos son simétricos, y los encabezados del monitor cuadran con las celdas.

---

## [3.2.0] — 2026-06-24

### Añadido — Estándares Documentales (ISAD(G) / ISO 15489)
- Nuevos campos en `datos_archivo`: `numero_folio`, `soporte` (Físico/Digital/Digitalizado), `numero_paginas`, `idioma`
- Nuevos campos en `datos_rrhh`: `numero_folio`, `soporte`, `numero_paginas`
- Campo `plazo_retencion_anios` en `tipo_documento` (calendario de retención ISO 15489-1:2016 §8)
- Panel "Retención" en Admin Global: visualización y edición inline del plazo por tipo de documento
- Panel "Alertas" en Admin Global: jubilaciones/pensiones próximas + documentos con retención vencida
- Campos ISAD(G) visibles y editables en el modal de edición de Archivo

### Añadido — RRHH / LOTTT Venezuela
- Nuevos campos en `empleados`: `fecha_nacimiento`, `nivel_educativo`, `sexo`, `updated_at`, `updated_by`
- Tabla `historial_cargos` activa con endpoints CRUD (`GET`/`POST`/`DELETE`)
- Modal de edición de empleado incluye los tres nuevos campos con selectores validados
- Sección "Historial de Cargos" colapsable en el dossier del expediente

### Añadido — Nuevas Rutas API
- `GET  /api/rrhh/alertas/jubilaciones?horizonte_dias=365`
- `GET  /api/rrhh/alertas/documentos_vencidos`
- `GET  /api/rrhh/empleado/{id}/historial_cargos`
- `POST /api/rrhh/empleado/{id}/historial_cargos`
- `DEL  /api/rrhh/empleado/{id}/historial_cargos/{hid}`
- `GET  /api/admin/retencion/tipos?scope=archivo|rrhh`
- `PATCH /api/admin/retencion/tipos/{id}`
- `GET  /api/admin/retencion/vencimientos`

### Añadido — v3.2.1 (parche incremental)
- Reporte PDF del expediente incluye: `fecha_ingreso`, `fecha_nacimiento`, `nivel_educativo`, `sexo` — cumplimiento LOTTT
- Sección "Historial de Cargos" en el reporte PDF (tabla cargo / desde / hasta / motivo)
- Dashboard RRHH: dos nuevas gráficas LOTTT — "Nivel Educativo" y "Distribución por Sexo"
- Dashboard RRHH: nueva tarjeta "Movimientos de Cargo" (total de `historial_cargos`)
- Búsqueda pública Archivo: badge de soporte visible en cada tarjeta de resultado
- Búsqueda pública Archivo: folio (`#`) y n° de páginas junto a la ubicación en tarjetas
- Modal de detalle Archivo: filas "Soporte", "N° de Folio/Signatura" y "N° de Páginas"
- Query FTS de Archivo expone `numero_folio`, `soporte` y `numero_paginas` en resultados paginados

### Mejorado — choices.py
- Expone: `keywords` (palabras clave para autocompletar), `soportes`, `idiomas`, `niveles_educativos`, `sexos`, `catalogo.retencion`

---

## [3.1.0] — 2026-06-23

### Añadido
- Toast system: duración variable por tipo, animación slide-in, botón de cierre manual
- Lookup de empleado por cédula en formulario nuevo registro RRHH
- Botón de preview de archivo en modal de edición de documento
- Exportación CSV de resultados de búsqueda desde páginas públicas
- Historial de auditoría: modal de detalle al hacer clic en cada fila
- Ingresos recientes: datos frescos del servidor con badge de status
- Health check `/api/health` con conteos de tablas
- Bloqueo temporal de login tras 5 intentos fallidos consecutivos
- Detección automática de `prefers-color-scheme: dark`
- Viewer de imágenes en modal de documento (antes solo PDF/iframe)
- ARIA roles en paginación y contadores de resultados (`role="navigation"`, `aria-live="polite"`)
- `formatRelativeTime(iso)` en `app.js`

### Corregido
- `formatISOToSpanish`: ahora maneja timestamps ISO 8601 con `T`
- Error 422 de Pydantic en `handleNewSubmission`: extrae `.msg` de cada item del array `detail`
- Estado laboral del empleado se carga dinámicamente desde choices (no hardcodeado)
- Alias `cedula`, `cargo`, `estado` sin prefijo plural en respuesta de búsqueda RRHH
- Logging de errores SQL sin exponer el mensaje completo al cliente

### Mejorado
- CSV import: detección robusta de encoding (utf-8-sig → utf-8 → latin-1 → cp1252)
- Validadores Pydantic en `DocumentSubmitRequest` (modulo, status)
- Protección al eliminar palabras clave en uso (`force=true` requerido)
- `global_summary`: métricas adicionales (arch_rechazados, empleados_activos, total_backups)
- Tabla del monitor RRHH: columnas más informativas (cargo, tipo principal, truncado con tooltip)
- Audit log enriquecido con título/status en eventos Create/Update

---

## [3.0.0] — 2026-06-20

### Arquitectura — Migración a Neon PostgreSQL
- Base de datos migrada de CSV plano a Neon PostgreSQL serverless
- Backend: FastAPI 0.110 + psycopg2 `ThreadedConnectionPool(1,5)`
- Función `db_query()` como único helper para todas las queries
- Migraciones idempotentes en `run_migrations()` al startup

### Módulo Archivo
- Búsqueda FTS con `plainto_tsquery('spanish')` + ranking `ts_rank_cd()`
- GIN indexes para FTS en título, autor, abstract, tesauro
- Paginación server-side: `{records, total, page, per_page}`
- Workflow de aprobación: `draft → revision → aprobado | rechazado`
- Sistema de palabras clave (descriptores_libres) con relación N:N

### Módulo RRHH
- Expediente estructurado en 4 Partes (I: Ingreso, II: Escalafón, III: Permisos, IV: Docs Personales)
- Vista agregada `vw_rrhh_persona_index` para búsqueda por persona
- Dossier del empleado con acceso rápido a documentos de identidad
- Generación de reporte PDF del expediente

### Sistema
- Gestión de usuarios con roles (Admin/Normal) y módulos (Archivo/RRHH/Global)
- `audit_log`: registro de todos los eventos del sistema
- Sistema de backup: export/restore JSON con `backup_history`
- Panel de estadísticas con Chart.js (distribución por tipo, línea de tiempo)
- Admin panels separados: `admin_archivo.html` y `admin_rrhh.html`

---

## [2.x] — Versiones anteriores (CSV + R/Shiny)
> Archivadas. El proyecto fue migrado completamente a la arquitectura actual.
