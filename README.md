# Ciencias UCV Digital Archive (DSpace Light)

[![Project Status: Mockup Avanzado](https://img.shields.io/badge/Status-Mockup_Avanzado-orange)]()
[![Privacy: Strict Segregation](https://img.shields.io/badge/Privacy-Strict_Segregation-red)]()
[![Framework: R--Shiny](https://img.shields.io/badge/Framework-R--Shiny--bs4Dash-blue)]()

## 📌 Descripción del Proyecto
Sistema de gestión documental y repositorio digital **no público** diseñado para el Decanato de la Facultad de Ciencias de la Universidad Central de Venezuela. 

Este proyecto implementa una arquitectura optimizada basada en **R y Shiny**, mimetizando la interfaz de usuario de **DSpace 7** pero reduciendo el consumo de recursos de 8GB a menos de 1GB de RAM. Actualmente opera mediante **Arquitectura de Segregación**, bifurcando completamente los entornos y bases de datos para **Extensión** y **Recursos Humanos (RRHH)**.

## 🎯 Objetivos Estratégicos Logrados
* **Segregación Institucional:** División absoluta entre los proyectos académicos de extensión y los expedientes laborales reservados de RRHH, manejados mediante bases de datos CSV independientes (`datos_extension.csv` y `datos_rrhh.csv`).
* **Privacidad de Grado Administrativo:** Metadatos enmascarados para RRHH (Ej: ocultación de autores, énfasis en Cédulas y Estatus Laboral) frente a metadatos académicos abiertos para Extensión.
* **Trazabilidad Física:** Mapeo de ubicación real en contenedores/gabinetes a través de la interfaz (Topografía Documental híbrida).
* **Mimetismo Reactivo (Pixel-Perfect):** Clonación UI exacta usando `bs4Dash` como motor reactivo oculto bajo el CSS original de DSpace 7 Angular.

---

## 🛠️ Stack Tecnológico
* **Frontend/Backend:** R 4.x + Shiny Framework.
* **UI Engine:** `bs4Dash` + overrides agresivos con `bslib` / CSS Puro.
* **Estilos:** Font-family `Nunito` y paleta oficinal (#2b4e72).
* **Bases de Simulacro:** CSV Dinámicos con paginación matemática reactiva R-Shiny.

---

## 🚀 Hoja de Ruta (Roadmap Incremental)

### Fase 1: Core Institucional (Completado)
- [x] Configuración del entorno base R-Shiny.
- [x] Implementación de la Interfaz Estilo DSpace (Layout institucional).
- [x] Clonación de CSS (Inspección viva hacia demo.dspace.org).

### Fase 2: Módulo de Extensión (Completado)
- [x] Taxonomía de archivos de proyectos, actas de consejo, planos y convenios.
- [x] Filtros reactivos y barra de búsqueda específica.
- [ ] Módulo de Autenticación Local.

### Fase 3: Módulo Personalizado de RRHH (Completado)
- [x] Metadatos específicos para expedientes de personal (C.I., Estatus, Ingreso).
- [x] Badges dinámicos de estado laboral (Activo, Inactivo, Jubilado).
- [x] Tableros aislados e íconos de advertencia de seguridad confidencial.

### Fase 4: Backoffice / MyDSpace (Completado)
- [x] Reactivación del Sidebar oscuro administrativo.
- [x] Bandeja "Mi DSpace" para revisión de tareas pendientes de workflow.
- [x] Paginación algorítmica real (5 ítems por hoja).

---

## 🔒 Arquitectura de Datos Actual
Actualmente, el mockups se alimenta de:
1. `datos_extension.csv` : Colección pública (Plano Arquitectónico, Actas, Proyectos).
2. `datos_rrhh.csv` : Colección estrictamente cifrada a la vista (Hojas de Vida, Nóminas).
*Ambos archivos poseen rastreo de "Ubicación Física" vinculando el repositorio digital con estantes y gavetas tangibles en la UCV.*

---

## 🎨 Ingeniería Inversa de UI (Refactorización)

Para asegurar la operatividad y reactividad a largo plazo sin sacrificar estética, hemos implementado el **"Pivote de Hibridación"**:

1. **Estructura (bs4Dash)**: Todo componente central, desde la barra de Pestañas (`bs4TabItems`), la barra de navegación lateral (`bs4DashSidebar`), y los paneles rectangulares de descubrimiento (`bs4Card`) son funciones NATIVAS de Shiny que hablan perfectamente con `server.R`.
2. **Pintura (styles.css)**: Usamos el archivo CSS inyectado para **ocultar a la fuerza** que estamos usando un dashboard de AdminLTE. 
    * Ocultamos el borde redondeado de los cards (`border-radius: 0`).
    * Anulamos las sombras de elevación y forzamos fondos grises pálidos.
    * Actualizamos la fuente maestra a `Nunito`.

### Paginación Angular en R
Se logró programar desde cero un motor matemático en `server.R` que imita `<ul class="pagination">` dividiendo los datasets de CSV bajo variables de estado reactivo (e.g. `p_ext <- reactiveVal(1)`), permitiendo deslizar las vistas sin recargar la sesión web.