# Archivo Institucional

Aplicacion web interna para gestion documental de la Facultad de Ciencias UCV, construida con R Shiny y bs4Dash, con separacion funcional entre Archivo y RRHH.

## Autorias

- Autor principal: Luisdavid Colina
- Proyecto academico institucional: Facultad de Ciencias UCV

## Resumen funcional

El sistema implementa:

- Inicio de sesion con control de roles.
- Segmentacion por modulo de trabajo (Archivo y RRHH).
- Vista administrativa con panel de control.
- Buscador y filtros por metadatos (implementados como acordeones colapsables para simplificar el despliegue).
- Tarjetas de resultados con metadatos y ubicacion fisica.
- Modal de detalle de documento estilo DSpace e interfaz de expediente unificado para personal.
- Estadisticas filtrables para usuarios Admin.

## Stack y dependencias

- Lenguaje: R
- Framework: Shiny
- UI dashboard: bs4Dash
- Estilos: CSS personalizado en carpeta www
- Iconografia: Font Awesome (CDN)
- Persistencia actual: archivos CSV locales

## Estructura del proyecto

- app.R: bootstrap explicito con validacion de archivos y errores de source detallados.
- global.R: carga librerias, modulos y fuentes de datos CSV.
- ui.R: layout general, login, sidebar, navbar, scripts JS de apoyo.
- server.R: orquestacion reactiva principal y ensamblaje de modulos.
- www/styles.css: estilo visual completo de la interfaz.
- R/services_auth.R: autenticacion local contra usuarios.csv.
- R/services_filters.R: filtros de busqueda cruzados para Archivo y RRHH.
- R/services_navigation.R: tab por defecto segun modulo y rol.
- R/services_pagination.R: utilidades de paginacion.
- R/server/ui_main_body.R: constructor del cuerpo principal (tabs Archivo, RRHH y Admin).
- R/server/stats_admin.R: salidas y reactivos del panel de estadisticas admin.
- R/server/document_modal.R: modal de documento y handlers de acciones integrales.
- datos_extension.csv: dataset de documentos de Archivo (anteriormente catalogado como Extensión).
- rrhh_personas.csv: Tabla maestra relacional estática con la biografía del personal de RRHH.
- rrhh_archivos.csv: Tabla transaccional relacional con foliaturas y documentos atada por cédula.
- usuarios.csv: credenciales y roles de acceso.

## Contrato de datos (CSV)

datos_extension.csv

- titulo
- autor
- doc_type
- fecha
- abstract
- ubicacion

rrhh_personas.csv

- cedula (Clave Primaria)
- empleado
- rif
- cargo
- departamento
- estado (antiguamente referenciado como estatus)
- fecha_ingreso
- fecha_jubilacion
- fecha_pension
- foto_url

rrhh_archivos.csv

- id_archivo (Clave Primaria)
- cedula (Clave Foránea)
- doc_type
- ubicacion
- personas_relacionadas (relaciones lógicas únicamente con personas físicas)
- tesauro_primario
- tesauro_secundario
- descriptores_libres

usuarios.csv

- usuario
- password
- modulo
- rol

## Credenciales de prueba actuales

- ext_normal / 1234 / modulo Archivo / rol Normal
- ext_admin / 1234 / modulo Archivo / rol Admin
- rrhh_normal / 1234 / modulo RRHH / rol Normal
- rrhh_admin / 1234 / modulo RRHH / rol Admin

## Flujo de autenticacion y navegacion

1. El usuario ingresa credenciales en pantalla de login.
2. server valida con authenticate_user.
3. Si credenciales son validas, se guarda estado de sesion:
    - logged
    - username
    - modulo
    - rol
4. Se envia mensaje al cliente para navegar al tab de busqueda del modulo correspondiente.
5. El sidebar se renderiza dinamicamente segun modulo y rol.

## Modulo Archivo

- Filtros agrupados en Data Display de Acordeones colapsables en panel lateral (Tipología, Fecha, Tesauro).
- Selector integrado de Orden con soporte cronológico y alfabético (A-Z por defecto, Z-A, Recientes y Antiguos).
- Barra de busqueda con boton Buscar (icono) y boton Exportar XLS interactivos en el mismo bloque.
- Paginacion reactiva.
- Exportacion de resultados filtrados a archivo XLS.
- Tarjetas con acciones visuales completas para Admin.

## Modulo RRHH

- **Sistema de Expediente Único:** La vista por defecto acopla la información condensando siempre a la persona como un solo gran expediente (relación 1:1), sin "conteo general" numérico redundante ni mezclado.
- Busqueda directa por Apellidos, Nombres o cedula.
- Filtros agrupados en Data Display de Acordeones colapsables (Tipología, Fecha, Estado, Personas, Tesauro).
- Interfaz interna modal de Expediente Digital "Dossier": Presentación Full Dashboard con cabecera circular de Avatar y listado vertical secuencial de archivos mediante iteraciones nativas adaptables y responsivas en pantallas móviles.
- Selector de Orden lógico y funcional: "Alfabético (A-Z)" (defecto), "Alfabético (Z-A)", "Más recientes primero" y "Más antiguos primero" con reflejo idéntico al abrir modales.
- Exportacion de expediente consolidado por persona a archivo XLS.
- Tarjetas de presentación con metadatos de dependencia, AP y ubicacion resumida.

## Ajustes de interfaz recientes y reestructuraciones

Cambios aplicados en la iteración actual orientada a la experiencia de usuario y precisión de datos funcionales:

- **Identidad Extendida Modificada:** Reemplazo integral en documentación y terminología del modelo conceptual "Extensión" a su par conceptual "Archivo".
- **Estandarización de Semántica (Estado):** Cambio rotundo, normalización de UI, scripts CSV y reportes XLS donde se empleaba "Estatus" cambiándolo permanentemente a "Estado".
- **Perfiles Realistas RRHH:** Purgado y limpieza de pseudónimos departamentales (como Tesorería, Archivo Pasivo) inyectados erróneamente como personas relacionadas en `datos_rrhh.csv`, protegiendo la identidad singular de los metadatos.
- **Consistencia Visual con Acordeones:** Adopción del marco `bs4Accordion` del lado izquierdo de los buscadores tanto de RRHH como de Archivo para compactar y organizar flujos.
- **Base de Datos Relacional:** Partición del dataset RRHH en 2 entidades maestras separadas (`rrhh_personas.csv` y `rrhh_archivos.csv`) utilizando la lógica `1-to-N` ensambladas con `LEFT JOIN` en memoria R, erradicando metadatos duplicados.
- **Rediseño Modal Dashboard:** Renovación completa de las tarjetas y modales RRHH migrando de Tabs colapsantes en filas a una columna vertical semántica para optimizar la responsividad móvil.
- **Campos de Contexto Institucional:** Adición de RIF, Cargo y URL_Fotos (dinámicas) para cada usuario.
- **Enlace de Modal Persistente a ID Crudo:** Corrección de cruces de strings JSON que enviaba atributos alterados al backend, asegurando el acople estricto a las identificaciones `persona_raw`.

## Panel Admin

Incluye areas:

- Nuevo Ingreso
- Monitor de Expedientes
- Categorias
- Usuarios
- Estadisticas

En estadisticas se dispone de filtros analiticos por fecha, tipologia y campos especificos por modulo (usando la variable homologada de `estado`).

## Modal de documento

El modal muestra:

- Titulo y encabezado visual de perfiles integrados.
- Miniatura iconica con perfilado por tipología primaria.
- Metadata estricta segmentada.
- Pestañero lógico (pills) interior en caso de separar documentos/folios por módulo.
- Acciones de sistema (visualizar, editar, descargar).

Estado actual de acciones:

- Visualizar: placeholder en construccion.
- Editar: placeholder en construccion.
- Descargar: placeholder en construccion.

## Seguridad y segregacion actual

- Separacion logica estructurada entre capas de modulo (Archivo/RRHH) y acceso vertical de rol (Normal/Admin).
- Segregación nativa de metadatos.
- Persistencia de mock data en CSV para simulaciones puras.

## Ejecucion local

Requisitos minimos:

- R 4.x
- Paquetes: shiny, bs4Dash, jsonlite

Ejecucion recomendada desde RStudio:

1. Abrir RStudio.
2. Abrir el proyecto desde la carpeta raiz `c:/ciencias-ucv-digital-archive`.
3. Si faltan dependencias, instalar una sola vez en la consola de RStudio:

    install.packages(c("shiny", "bs4Dash", "jsonlite"), repos = "https://cloud.r-project.org")

4. Ubicar la sesion en la carpeta del proyecto:

    setwd("c:/ciencias-ucv-digital-archive")

5. Levantar la aplicacion:

    shiny::runApp()

Tambien puedes abrirla directamente con:

    shiny::runApp("c:/ciencias-ucv-digital-archive")

Nota: existe app.R para bootstrap robusto con mensajes de error de carga mas claros.

## Despliegue

- Existe configuracion de despliegue en la carpeta rsconnect.
- Archivo de token auxiliar presente en deploy_token.R.
- Para despliegues productivos, mover credenciales y tokens a variables de entorno seguras.

## Problemas conocidos y diagnostico

- Si aparece Error sourcing server.R, revisar primero sintaxis de bloques renderUI con multiples elementos.
- El bootstrap de app.R incluye safe_source y detalla archivo y motivo de falla.
- El proyecto usa codificacion UTF-8 para evitar errores por caracteres acentuados en Windows.

## Historial reciente de arquitectura

Cambios estructurales aplicados en esta etapa:

- Ajuste profundo de índices que validan la correspondencia 1:1 para un "Expediente Único".
- Extraccion de constructor de cuerpo principal a R/server/ui_main_body.R.
- Extraccion de estadisticas admin a R/server/stats_admin.R.
- Extraccion de modal y handlers de documento a R/server/document_modal.R.
- Conexion de modulos desde global.R para mantener server.R mas compacto.
- Incorporacion de exportadores XLS modulares adaptables a "Archivo" y "RRHH".
- Consolidacion de tarjetas RRHH por persona y apertura de expediente unificado tras un controlador JS local.

## Proximos pasos recomendados

- Completar acciones reales de visualizar, editar y descargar en modal.
- Extraer modulo adicional para resultados/paginacion de listados.
- Incorporar pruebas unitarias para servicios en carpeta R/services.
- Migrar de CSV a base de datos transaccional cuando se pase a produccion.

## Creditos

Documentacion y desarrollo principal: Luisdavid Colina