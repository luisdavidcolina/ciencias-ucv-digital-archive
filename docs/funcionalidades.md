# Funcionalidades Específicas del Sistema

A partir del análisis exhaustivo de todos los archivos del código fuente y backend, a continuación se detallan **absolutamente todas las funcionalidades específicas** con las que cuenta actualmente el **Archivo Institucional Digital (Facultad de Ciencias, UCV)**:

## 1. Módulo Archivo Institucional (Gestión Documental General)
- **Búsqueda Avanzada Full-Text**: Motor de búsqueda de texto completo con soporte en español (rankeado por relevancia) y *fallback* para búsquedas numéricas o inexactas.
- **Filtrado Múltiple**: Filtrado de documentos por rango de fechas, tipos de documento (tesauro primario), descriptores libres (tesauro secundario) y tipo de soporte (Físico, Digital, Digitalizado).
- **Autocompletado de Búsqueda**: Sugerencias en vivo y autocompletado para palabras clave y tipos de documento al realizar búsquedas.
- **Ordenamiento Dinámico**: Clasificación de resultados por antigüedad (más recientes, más antiguos) o alfabéticamente (A-Z).
- **Cumplimiento de Estándares**: Soporte de campos bajo norma ISAD(G) e ISO 15489-1:2016 (número de folios, cantidad de páginas, idioma y soporte físico/digital).

## 2. Módulo de Recursos Humanos (Expedientes de RRHH)
- **Expedientes Digitales Unificados**: Agrupación automática de todos los documentos bajo el perfil de un mismo empleado.
- **Búsqueda de Personal**: Búsqueda global del personal por nombre completo, cédula de identidad o RIF, cargo y departamento.
- **Perfil Integral del Empleado**: Vista detallada con todos sus datos laborales, foto (si existe) y lista cronológica y categorizada de documentos asociados a su expediente.
- **Búsqueda Intra-Expediente**: Búsqueda interna para encontrar documentos específicos dentro del expediente de un solo empleado.
- **Historial de Cargos**: Registro y listado cronológico de los movimientos, ascensos y cambios de cargo del empleado dentro de la institución.
- **Generación de Reportes para Impresión (PDF/Imprimible)**: Creación dinámica de un reporte HTML con diseño estilizado de impresión para extraer físicamente el expediente completo de un empleado (incluye resumen numérico, datos personales, lista de documentos e historial de cargos).
- **Cumplimiento LOTTT**: Integración de campos estipulados por la legislación venezolana (fechas clave de jubilación, pensión, nacimiento, nivel educativo y sexo).

## 3. Módulo de Alertas Operativas y Vencimientos
- **Alertas de Próximas Jubilaciones y Pensiones**: Cálculo automatizado que notifica sobre empleados cuya fecha de jubilación o pensión caerá dentro de un horizonte configurable (ej. los próximos 365 días), así como los plazos ya vencidos que no han sido procesados.
- **Alertas de Vencimiento Documental**: Revisión de documentos que han expirado según el "Plazo de Retención en Años" de su tipo de documento (con base en la norma ISO 15489-1:2016), listando el total de días que lleva vencido el documento.

## 4. Módulo de Gestión y Administración (CRUD y Backoffice)
- **Dashboard y Estadísticas**: Conteo de documentos, empleados, historiales, descriptores libres (palabras clave) y desglose de documentos por estado de publicación.
- **Gestión de Documentos (Archivo y RRHH)**: Creación de nuevos documentos e ingreso de metadatos (título, autor, resumen, tipo, folios, idioma, palabras clave). Si un empleado no existe al subir un documento de RRHH, se crea automáticamente el perfil "Por Asignar".
- **Gestión de Archivos Digitalizados**: Subida segura de copias digitales conectada con almacenamiento en la nube (*Cloudflare R2*) validando peso máximo (25MB) y extensiones permitidas, devolviendo URL seguras pre-firmadas válidas por tiempo limitado.
- **Flujo de Trabajo (Workflow)**: Gestión de estatus de documentos con estados como: *Draft*, *Revision*, *Aprobado*, y *Rechazado*.
- **Bandeja de Pendientes**: Listado de todos los documentos estancados en fase *Draft* o *Revision* a la espera de aprobación por un supervisor.
- **Edición de Personal**: Modificación de detalles del perfil de los empleados (cambio de cargo, departamento, estado laboral y fechas importantes).

## 5. Control de Versiones de Archivos Digitales
- **Historial de Versiones**: Cada vez que se reemplaza el archivo digital de un documento (PDF, imagen, etc.), el sistema conserva la versión anterior.
- **Restauración de Versiones**: Capacidad para retroceder el documento y restaurar una versión digital anterior como la principal.
- **Metadatos de Versión**: Control de qué usuario subió la versión y campos para adjuntar comentarios justificando los cambios.

## 6. Módulo de Papelera de Reciclaje (Soft-Delete)
- **Eliminación Segura (Soft-Delete)**: Borrado de documentos o empleados de las vistas públicas y operativas, marcándolos internamente para no perder data por accidente.
- **Bandeja de Papelera**: Panel exclusivo listando todos los registros (documentos y empleados) eliminados junto a su fecha de eliminación y usuario responsable.
- **Restauración de Registros**: Recuperación de elementos borrados de vuelta al sistema general.
- **Purgado Permanente (Hard-Delete)**: Borrado físico, irreversible y en cascada de los datos del servidor (destruyendo archivos, historiales y metadatos asociados).

## 7. Módulo de Seguridad y Usuarios
- **Autenticación Basada en Roles (RBAC)**: Accesos controlados según módulo asignado (`Archivo`, `RRHH`, o `Global`) y rol (`Normal`, `Admin`).
- **Gestión de Usuarios**: Creación de nuevos usuarios, reseteo manual de contraseñas, listado de cuentas y desactivación temporal de accesos (bloqueo sin borrar la cuenta).
- **Cifrado**: Contraseñas *hasheadas* en la base de datos de forma segura.
- **Registro de Auditoría Log_Event**: Traza interna mínima de eventos y movimientos de usuarios.

## 8. Módulo de Respaldo de Base de Datos (Backups)
- **Exportación Segura**: Descarga manual en un solo clic de toda la información (tablas en orden de dependencia sin FK issues) en formato de archivo JSON.
- **Importación / Restauración Flexible**: 
  - Capacidad de restaurar la base de datos subiendo el archivo JSON mediante el método *Merge* (añadir registros sin borrar datos preexistentes).
  - Capacidad mediante método *Overwrite* (borrado total previo, omitiendo tabla de usuarios para evitar quedar bloqueado, antes de rellenar la base de datos de cero).
- **Historial de Backups**: Registro cronológico de todo export y restauración realizada, el usuario que lo corrió, modo usado y filas afectadas.
