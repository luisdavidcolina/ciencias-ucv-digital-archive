# Ciencias UCV Digital Archive (DSpace Light)

[![Project Status: Prototyping](https://img.shields.io/badge/Status-Prototyping-orange)]()
[![Privacy: Internal Only](https://img.shields.io/badge/Privacy-Internal--Only-red)]()
[![Framework: R--Shiny](https://img.shields.io/badge/Framework-R--Shiny--bs4Dash-blue)]()

## 📌 Descripción del Proyecto
Sistema de gestión documental y repositorio digital **no público** diseñado para el Decanato de la Facultad de Ciencias de la Universidad Central de Venezuela. 

Este proyecto implementa una arquitectura optimizada basada en **R y Shiny**, mimetizando la interfaz de usuario de **DSpace 7** pero reduciendo el consumo de recursos de 8GB a menos de 1GB de RAM, permitiendo una gestión fluida de los archivos de **Extensión** y **Recursos Humanos**.

## 🎯 Objetivos Estratégicos
* **Centralización Institucional:** Unificar los archivos digitales de proyectos de extensión y expedientes de personal.
* **Privacidad de Grado Administrativo:** Control de acceso estricto y segregación de módulos para proteger datos sensibles de RRHH.
* **Búsqueda Semántica de Alto Nivel:** Integración de búsqueda vectorial para localización de documentos basada en contexto y contenido, superando la búsqueda por palabras clave tradicional.
* **Eficiencia de Infraestructura:** Despliegue mediante contenedores ligeros compatibles con el hardware existente en la Facultad.

---

## 🛠️ Stack Tecnológico
* **Frontend/Backend:** R 4.x + Shiny Framework.
* **UI Engine:** `bs4Dash` (Bootstrap 4 for Shiny).
* **Base de Datos:** PostgreSQL (Metadatos y auditoría).
* **Motor de Búsqueda:** Búsqueda Semántica Vectorial.
* **Virtualización:** Docker & Docker Compose.

---

## 🚀 Hoja de Ruta (Roadmap Incremental)

### Fase 1: Core Institucional (En curso)
- [ ] Configuración del entorno Dockerizado.
- [ ] Implementación de la Interfaz Estilo DSpace (Layout institucional).
- [ ] Módulo de Autenticación Local.

### Fase 2: Módulo de Extensión
- [ ] Taxonomía de archivos de proyectos y convenios.
- [ ] Sistema de carga masiva de archivos.
- [ ] Visualización nativa de PDFs en navegador.

### Fase 3: Módulo Personalizado de RRHH
- [ ] Metadatos específicos para expedientes de personal.
- [ ] Permisos de visualización restrictiva para personal de apoyo.
- [ ] Encriptación de documentos sensibles en reposo.

### Fase 4: Inteligencia Documental
- [ ] Motor de OCR automático para documentos escaneados.
- [ ] Indexación vectorial para búsquedas complejas.

---

## 🔒 Seguridad y Acceso
El sistema opera bajo una política de **Zero Trust** interna:
1. Las bases de datos y el almacenamiento de archivos no están expuestos a internet.
2. Cada acceso al módulo de RRHH genera un log de auditoría inmutable.
3. El sistema de archivos utiliza volúmenes Docker aislados.

---

## ⚙️ Instalación (Entorno de Desarrollo)
Requiere Docker y Docker Compose instalados.

```bash
git clone https://github.com/tu-usuario/ciencias-ucv-digital-archive.git
cd ciencias-ucv-digital-archive
docker compose up -d
```

---

## 🎨 Ingeniería Inversa de UI (Clonación de DSpace) 

Para lograr que la interfaz sea **clonada con precisión quirúrgica**, no podemos confiar en las configuraciones automáticas de Shiny. Tenemos que aplicar lo que en ingeniería llamamos **"Ingeniería Inversa de UI"**.

DSpace 7 utiliza **Angular** y **Bootstrap 4**. Como tú vas a usar `bs4Dash` (que también es Bootstrap 4), ya tenemos el 50% del camino hecho. El otro 50% se logra "inyectando el ADN" de DSpace en tu código.

Aquí tienes los 4 pilares para asegurar que sea **idéntica en todo**:

### 1. Extracción del "ADN" Visual (Inspección)
No adivines los tamaños. Abre el [Demo oficial de DSpace 7](https://demo.dspace.org/) en Chrome y haz lo siguiente:
1.  **Click derecho** en cualquier elemento (ej: el botón de búsqueda o la barra lateral) -> **Inspeccionar**.
2.  En la pestaña **Computed** (Calculado), verás los valores exactos que usa DSpace:
    * `font-family` (Tipografía).
    * `background-color` (Color exacto en HEX).
    * `padding` y `margin` (Los espacios exactos en píxeles).
    * `height` y `width`.

### 2. El archivo `custom.css` (El Santo Grial)
Para que Shiny se comporte como DSpace, crearemos un archivo CSS que sobrescriba los estilos por defecto. Debes recrear estas 3 áreas críticas:

* **La Barra Lateral (Sidebar):** DSpace usa un gris muy específico (`#f8f9fa`) y un ancho fijo.
* **Tarjetas de Resultados:** DSpace no usa bordes redondeados pronunciados, son casi rectos.
* **Tipografía:** DSpace suele usar una combinación de *Roboto* o *Open Sans*.

**Código para tu `app.R` para inyectar el estilo:**
```r
tags$head(
  tags$style(HTML("
    /* Clonar la barra superior de DSpace */
    .main-header { background-color: #2b4e72 !important; } 
    /* Ajustar el tamaño de la fuente institucional */
    body { font-family: 'Roboto', sans-serif; font-size: 14px; }
    /* Quitar sombras innecesarias para que se vea plano como DSpace */
    .card { box-shadow: none !important; border: 1px solid #dee2e6 !important; }
  "))
)
```

### 3. Mapeo de Componentes
Debes usar los componentes de `bs4Dash` que correspondan exactamente a los de DSpace:

| Elemento DSpace | Componente Shiny (bs4Dash) |
| :--- | :--- |
| **Comunidades/Colecciones** | `bs4SidebarMenuItem` con iconos de carpeta. |
| **Filtros (Facets)** | `bs4Card` en la columna izquierda con `checkboxGroupInput`. |
| **Lista de Tesis/Archivos** | `div` personalizados dentro de un `renderUI` para imitar el listado. |
| **Barra de búsqueda** | `textInput` con un `actionButton` pegado usando `input-group`. |

### 4. Grid System (Maquetación de precisión)
DSpace divide la pantalla en una proporción muy clara. En Shiny lo haremos usando el sistema de 12 columnas:
* **Sidebar (Filtros):** `width = 3` (25% de la pantalla).
* **Main Body (Resultados):** `column(width = 9, ...)` (75% de la pantalla).

> **Nota sobre iconos:** DSpace usa una librería llamada **FontAwesome**. Asegúrate de usar los mismos iconos (la lupa para buscar, el clip para archivos) para que la memoria visual del usuario sienta el entorno como idéntico. Lograr que los **colores y los tamaños de los botones** sean iguales es lo que el ojo humano nota primero.