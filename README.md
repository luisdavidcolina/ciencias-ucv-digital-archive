# Archivo Institucional

Aplicacion web interna para gestion documental de la Facultad de Ciencias UCV, construida con R Shiny y bs4Dash, con separacion funcional entre Extension y RRHH.

## Autorias

- Autor principal: Luisdavid Colina
- Proyecto academico institucional: Facultad de Ciencias UCV

## Resumen funcional

El sistema implementa:

- Inicio de sesion con control de roles.
- Segmentacion por modulo de trabajo (Extension y RRHH).
- Vista administrativa con panel de control.
- Buscador y filtros por metadatos.
- Tarjetas de resultados con metadatos y ubicacion fisica.
- Modal de detalle de documento estilo DSpace.
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
- R/services_filters.R: filtros de busqueda para Extension y RRHH.
- R/services_navigation.R: tab por defecto segun modulo y rol.
- R/services_pagination.R: utilidades de paginacion.
- R/server/ui_main_body.R: constructor del cuerpo principal (tabs Extension, RRHH y Admin).
- R/server/stats_admin.R: salidas y reactivos del panel de estadisticas admin.
- R/server/document_modal.R: modal de documento y handlers de acciones.
- datos_extension.csv: dataset de documentos de Extension.
- datos_rrhh.csv: dataset de expedientes RRHH.
- usuarios.csv: credenciales y roles de acceso.

## Contrato de datos (CSV)

datos_extension.csv

- titulo
- autor
- doc_type
- fecha
- abstract
- ubicacion

datos_rrhh.csv

- empleado
- cedula
- departamento
- doc_type
- estado
- fecha_ingreso
- ubicacion

usuarios.csv

- usuario
- password
- modulo
- rol

## Credenciales de prueba actuales

- ext_normal / 1234 / modulo Extension / rol Normal
- ext_admin / 1234 / modulo Extension / rol Admin
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

## Modulo Extension

- Filtro por tipologia multiple.
- Filtro por rango de fechas.
- Filtro por rango de anos.
- Orden de resultados.
- Paginacion reactiva.
- Tarjetas con acciones visuales para Admin.

## Modulo RRHH

- Busqueda por empleado o cedula.
- Filtro por tipologia.
- Filtro por estado.
- Filtro por rango de fechas.
- Filtro por rango de anos.
- Tarjetas de expediente con metadatos de adscripcion y ubicacion.

## Panel Admin

Incluye areas:

- Nuevo Ingreso
- Monitor de Expedientes
- Categorias
- Usuarios
- Estadisticas

En estadisticas se dispone de filtros analiticos por fecha, tipologia y campos especificos por modulo.

## Modal de documento

El modal muestra:

- Titulo y encabezado visual.
- Miniatura iconica.
- Metadata principal.
- Descripcion/resumen.
- Acciones (visualizar, editar, descargar).

Estado actual de acciones:

- Visualizar: placeholder en construccion.
- Editar: placeholder en construccion.
- Descargar: placeholder en construccion.

## Seguridad y segregacion actual

- Separacion logica por modulo y rol.
- Distincion de metadatos entre Extension y RRHH.
- Datos almacenados localmente en CSV para entorno prototipo.

## Ejecucion local

Requisitos minimos:

- R 4.x
- Paquetes: shiny, bs4Dash

Ejecucion recomendada desde RStudio:

1. Abrir RStudio.
2. Abrir el proyecto desde la carpeta raiz `c:/ciencias-ucv-digital-archive`.
3. Si faltan dependencias, instalar una sola vez en la consola de RStudio:

    install.packages(c("shiny", "bs4Dash"), repos = "https://cloud.r-project.org")

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

- Extraccion de constructor de cuerpo principal a R/server/ui_main_body.R.
- Extraccion de estadisticas admin a R/server/stats_admin.R.
- Extraccion de modal y handlers de documento a R/server/document_modal.R.
- Conexion de modulos desde global.R para mantener server.R mas compacto.

## Proximos pasos recomendados

- Completar acciones reales de visualizar, editar y descargar en modal.
- Extraer modulo adicional para resultados/paginacion de listados.
- Incorporar pruebas unitarias para servicios en carpeta R/services.
- Migrar de CSV a base de datos transaccional cuando se pase a produccion.

## Creditos

Documentacion y desarrollo principal: Luisdavid Colina