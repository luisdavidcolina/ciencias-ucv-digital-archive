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