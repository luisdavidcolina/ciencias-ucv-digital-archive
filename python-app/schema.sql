-- =============================================================================
-- SCRIPT INSTITUCIONAL DE CREACIÓN Y POBLACIÓN DE BASE DE DATOS DEFINITIVO
-- PROYECTO: GESTIÓN DOCUMENTAL, EXPEDIENTES DE RRHH Y AUDITORÍA DE ARCHIVOS
-- =============================================================================

-- 1. ACTIVAR EXTENSIONES PARA EL TRATAMIENTO DE TEXTO (Para quitar acentos en Slugs)
CREATE EXTENSION IF NOT EXISTS unaccent;
CREATE EXTENSION IF NOT EXISTS pg_trgm;

-- =============================================================================
-- 2. CREACIÓN DE TABLAS MAESTRAS Y DE CONFIGURACIÓN
-- =============================================================================

CREATE TABLE public.categoria (
    id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    nombre CHARACTER VARYING(100) NOT NULL UNIQUE,
    slug CHARACTER VARYING(120) NOT NULL UNIQUE
);

CREATE TABLE public.tipo_documento (
    id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    nombre TEXT NOT NULL UNIQUE,       -- Nombre completo/legal
    nombre_corto TEXT NOT NULL,        -- Etiqueta corta para la UI
    slug CHARACTER VARYING(250) UNIQUE,
    id_categoria INTEGER NOT NULL
);

CREATE TABLE public.cargos (
    id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    nombre CHARACTER VARYING(100) NOT NULL UNIQUE
);

CREATE TABLE public.departamentos (
    id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    nombre CHARACTER VARYING(100) NOT NULL UNIQUE
);

CREATE TABLE public.estados_laborales (
    id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    estados CHARACTER VARYING(50) NOT NULL UNIQUE
);

-- Tabla Unificada: Control de accesos, credenciales web y auditoría en un solo lugar
CREATE TABLE public.usuarios_sistema (
    id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    usuario CHARACTER VARYING(50) NOT NULL UNIQUE,
    nombre_usuario CHARACTER VARYING(100) NOT NULL,
    contrasena CHARACTER VARYING(255) NOT NULL,
    modulo CHARACTER VARYING(50) NOT NULL, -- 'Archivo', 'RRHH' o 'Global'
    rol CHARACTER VARYING(30) NOT NULL     -- 'Admin' o 'Normal'
);

-- =============================================================================
-- 3. CREACIÓN DE TABLAS DE DATOS PRINCIPALES E HISTORIALES
-- =============================================================================

CREATE TABLE public.empleados (
    id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    cedula CHARACTER VARYING(20) NOT NULL UNIQUE,
    nombres CHARACTER VARYING(100) NOT NULL,
    apellidos CHARACTER VARYING(100) NOT NULL,
    rif CHARACTER VARYING(20) UNIQUE,
    cargo_id INTEGER NOT NULL,
    departamento_id INTEGER NOT NULL,
    estado_id INTEGER NOT NULL,
    fecha_ingreso DATE NOT NULL,
    fecha_jubilacion DATE,
    fecha_pension DATE,
    foto_url TEXT
);

CREATE TABLE public.datos_archivo (
    id_archivo INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    titulo TEXT NOT NULL,
    abstract TEXT,
    autor TEXT, -- Ente o departamento que emitió el documento
    fecha_documento DATE DEFAULT CURRENT_DATE,
    tesauro_primario TEXT,
    tesauro_secundario TEXT,
    ubicacion TEXT NOT NULL, -- Gaveta física, estante o 'Digitalizado Exclusivo'
    creado_por INTEGER NOT NULL, -- Pista de Auditoría: ID del usuario del sistema
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE public.datos_rrhh (
    id_rrhh INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    titulo TEXT NOT NULL,
    abstract TEXT,
    autor TEXT, -- Ente o departamento que emitió el documento
    id_tipo_documento INTEGER NOT NULL,
    empleado_id INTEGER, -- Vinculación con el empleado dueño del expediente
    fecha_documento DATE DEFAULT CURRENT_DATE,
    tesauro_primario TEXT,
    tesauro_secundario TEXT,
    ubicacion TEXT NOT NULL, -- Gaveta física, estante o 'Digitalizado Exclusivo'
    creado_por INTEGER NOT NULL, -- Pista de Auditoría: ID del usuario del sistema
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE public.descriptores_libres (
    id_descriptor INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    nombre VARCHAR(100) UNIQUE NOT NULL
);

CREATE TABLE public.archivo_descriptores (
    id_archivo INTEGER REFERENCES public.datos_archivo(id_archivo) ON DELETE CASCADE,
    id_descriptor INTEGER REFERENCES public.descriptores_libres(id_descriptor) ON DELETE CASCADE,
    PRIMARY KEY (id_archivo, id_descriptor)
);

CREATE TABLE public.rrhh_descriptores (
    id_rrhh INTEGER REFERENCES public.datos_rrhh(id_rrhh) ON DELETE CASCADE,
    id_descriptor INTEGER REFERENCES public.descriptores_libres(id_descriptor) ON DELETE CASCADE,
    PRIMARY KEY (id_rrhh, id_descriptor)
);

CREATE TABLE public.fotos_empleado (
    id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    empleado_id INTEGER NOT NULL,
    url TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE public.historial_cargos (
    id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    empleado_id INTEGER NOT NULL,
    cargo_id INTEGER NOT NULL,
    fecha_inicio DATE NOT NULL,
    fecha_fin DATE
);

-- =============================================================================
-- 4. POBLACIÓN DE DATA MAESTRA (MÓDULO DOCUMENTAL, SEGURIDAD Y FLAGS)
-- =============================================================================

-- Inserción de Flags de Auditoría / Valores por defecto controlados
INSERT INTO public.cargos (nombre) VALUES ('Por Asignar');
INSERT INTO public.departamentos (nombre) VALUES ('Por Asignar');
INSERT INTO public.estados_laborales (estados) VALUES
('Pendiente de Registro'), ('Activo'), ('Jubilado'), ('Retirado'), ('Pensionado');

-- Inserción Unificada de tus Credenciales de Prueba Reales para el Sitio Web
-- Contraseña: '1234'
INSERT INTO public.usuarios_sistema (usuario, nombre_usuario, contrasena, modulo, rol) VALUES
('admin', 'Administrador Global', '$2b$12$cbZ7aftcIoviCEMIpEC/i.XwkDOGKnjspUDGDGr8jHvzwceqZAsCm', 'Global', 'Admin'),
('archivo_normal', 'Operador de Archivo', '$2b$12$cbZ7aftcIoviCEMIpEC/i.XwkDOGKnjspUDGDGr8jHvzwceqZAsCm', 'Archivo', 'Normal'),
('archivo_admin', 'Administrador de Archivo', '$2b$12$cbZ7aftcIoviCEMIpEC/i.XwkDOGKnjspUDGDGr8jHvzwceqZAsCm', 'Archivo', 'Admin'),
('rrhh_normal', 'Analista de RRHH', '$2b$12$cbZ7aftcIoviCEMIpEC/i.XwkDOGKnjspUDGDGr8jHvzwceqZAsCm', 'RRHH', 'Normal'),
('rrhh_admin', 'Director de RRHH', '$2b$12$cbZ7aftcIoviCEMIpEC/i.XwkDOGKnjspUDGDGr8jHvzwceqZAsCm', 'RRHH', 'Admin');

-- Inserción en Categorías
INSERT INTO public.categoria (nombre, slug) VALUES
('Parte I', 'parte-i'),
('Parte II', 'parte-ii'),
('Parte III', 'parte-iii'),
('Parte IV', 'parte-iv');

-- Inserción en Tipo de Documento (Catálogo institucional)
INSERT INTO public.tipo_documento (nombre, nombre_corto, id_categoria) VALUES
('Concurso de oposicion/credenciales C.U.', 'Concurso de Oposición/Credenciales', 1),
('Contratos, renovaciones, prorrogas de contratos y suplencias', 'Contratos y Suplencias', 1),
('Disminucion de nomina_formulario A', 'Disminución de Nómina', 1),
('Documentos de ingreso (planillas de solicitud de empleo, evaluacion medica pre_empleo, copia certificado de salud, copia numero de cuenta bancaria, periodo de prueba)', 'Documentos de Ingreso', 1),
('Estudios de cargos y evaluacion de credenciales', 'Evaluación de Cargos', 1),
('Inclusion planta Profesoral (Postgrado)', 'Inclusión Profesoral (Postgrado)', 1),
('Informes de actividades a realizar y realizadas', 'Informes de Actividades', 1),
('Nombramiento provisional, nombramiento definitivo, otros nombramientos y designaciones', 'Nombramientos y Designaciones', 1),
('Retiros, renuncias, disminucion de nomina, calificaciones de despido. En el caso de retiro por fallecimiento del empleado o sucesor, Incluye Acta De Defuncion', 'Retiros y Despidos', 1),
('Solicitud de apertura de concurso de oposicion / nombramiento de jurados', 'Concurso de Oposicion / Nombramiento de Jurados', 1),
('Actas de traslado', 'Actas de Traslado', 2),
('Adecuacion del plan de formacion y capacitacion de docentes (instructores)', 'Adecuación Formación y Capacitacion', 2),
('Aprobacion de informes semestrales, anuales y final del plan de formacion', 'Informes Formación', 2),
('Aprobacion de prorroga de plan de formacion', 'Prórroga Formación', 2),
('Aprobacion de temario de leccion publica', 'Temario Lección Pública', 2),
('Ascensos', 'Ascensos', 2),
('Cambios de dedicacion, cambios de unidades ejecutoras y cambios de financiamiento', 'Cambios de Dedicación, Unidades Ejecutoras y Finaciamento', 2),
('Escalafon', 'Escalafón', 2),
('Jubilacion y pension', 'Jubilación y Pensión', 2),
('Modificaciones y actualizacion de datos (movimientos de personal)', 'Movimientos de Personal', 2),
('Nombramiento de jurado de leccion publica', 'Jurado Lección Pública', 2),
('Normas de permanencia', 'Normas Permanencia', 2),
('Prorroga de leccion publica', 'Prórroga Lección Pública', 2),
('Ratificacion de cargo', 'Ratificación de Cargo', 2),
('Reconocimientos de antigüedad', 'Antigüedad', 2),
('Registro de asignacion de sueldos (RAS)', 'RAS', 2),
('Relaciones de cargos y tiempo de servicio', 'Tiempo de Servicio', 2),
('Solicitudes de calculos de pasivos laborales', 'Pasivos Laborales', 2),
('Transferencias', 'Transferencias', 2),
('Año sabatico e informes de actividades de año sabatico', 'Año Sabático', 3),
('Becas, solicitudes y tramitacion de becas, prorroga de beca_sueldo nacional y exterior', 'Becas', 3),
('Certificados de incapacidad', 'Certificado Incapacidad', 3),
('Financiamientos de estudios y cursos', 'Financiamiento Estudios', 3),
('Informe de actividades por asistencia a congresos y/o eventos', 'Asistencia a Eventos', 3),
('Informes de pasantia, informe de actividades docentes', 'Informes Pasantía y Actividades Docentes', 3),
('Pre y post natal', 'Pre y Post Natal', 3),
('Reposos y permisos, permisos remunerados y no remunerados, reincorporaciones de permisos y de reposos medicos, Asistencia A Eventos, Permisos Por Excedencia Activa_Pasiva', 'Reposos y Permisos', 3),
('Vacaciones', 'Vacaciones', 3),
('Acta de separacion de bienes', 'Separación de Bienes', 4),
('Adelantos de prestaciones sociales y planilla de liquidacion de prestaciones sociales', 'Prestaciones Sociales', 4),
('Amonestaciones', 'Amonestaciones', 4),
('Aportes por defuncion familiar, nacimiento de hijo, lentes, etc', 'Aportes Familiares', 4),
('Autorizaciones', 'Autorizaciones', 4),
('Becas de hijos', 'Becas de Hijos', 4),
('Bonos (nocturno, juguetes, transporte, doctoral)', 'Bonos', 4),
('Botones, diplomas, ordenas, premios, reconocimientos y agradecimientos', 'Reconocimientos', 4),
('Cedula De Identidad', 'Cédula', 4),
('Constancias', 'Constancias', 4),
('Curriculum Vitae Y Anexos', 'CV', 4),
('Declaracion jurada de patrimonio', 'Declaración Jurada', 4),
('Descuentos y reclamos por pago de institutiones educativas', 'Descuentos Educativos', 4),
('HCM y seguro de vida', 'HCM y Seguro', 4),
('Pagos por diferencia de sueldo', 'Diferencia de sueldo', 4),
('Planillas Ari Y Planillas De Declaracion De Impuesto Sobre La Renta Y Otros Documentos Fiscales', 'Declaración Fiscal', 4),
('Planillas Del Seguro Social', 'Seguro Social', 4),
('Primas Y Disminuciones De Primas', 'Primas', 4),
('Recibos de pago y relaciones salariales', 'Recibos de Pago', 4),
('Reclamos de Personal y reclamos por cobros indebidos', 'Reclamos Personal', 4),
('Registro de Informacion Fiscal (RIF)', 'RIF', 4),
('Registro y/o planilla de datos personales o de actualizacion de datos', 'Actualización de datos', 4),
('Registros_partidas de nacimiento matrimonio y divorcio', 'Actas Registro Civil', 4),
('Reintegros de sueldo', 'Reintegros', 4),
('Rutagrama', 'Rutagrama', 4);

-- =============================================================================
-- 5. CARGA Y PROCESAMIENTO AUTOMÁTICO DE EMPLEADOS DE NÓMINA INICIAL
-- (Los slugs de tipo_documento se generan en Python al iniciar la aplicación)
-- =============================================================================

CREATE TEMP TABLE tmp_rrhh_personas (
    cedula varchar(20), nombres varchar(100), apellidos varchar(100), rif varchar(20), cargo text, departamento text,
    estado text, fecha_ingreso date, fecha_jubilacion date, fecha_pension date, foto_url text
);

INSERT INTO tmp_rrhh_personas (cedula, nombres, apellidos, rif, cargo, departamento, estado, fecha_ingreso, fecha_jubilacion, fecha_pension, foto_url) VALUES
('V-12345678', 'Carlos Alberto', 'Gomez Perez', 'J-12345678-0', 'Director General', 'Decanato', 'Activo', '2015-06-20', NULL, NULL, 'https://i.pravatar.cc/150?img=11'),
('V-23456789', 'Maria Fernanda', 'Lopez Ruiz', 'J-23456789-1', 'Analista de Sistemas', 'Informática', 'Activo', '2020-01-10', NULL, NULL, 'https://i.pravatar.cc/150?img=5'),
('V-09876543', 'José Antonio', 'Martinez Rojas', 'J-09876543-2', 'Superintendente', 'Mantenimiento', 'Retirado', '1995-02-15', '2020-03-01', NULL, 'https://i.pravatar.cc/150?img=15'),
('V-18765432', 'Ana Lucia', 'Silva Torres', 'J-18765432-3', 'Coordinadora de Nómina', 'Recursos Humanos', 'Activo', '2018-09-01', NULL, NULL, 'https://i.pravatar.cc/150?img=9'),
('V-15678901', 'Luis Miguel', 'Rodriguez Garcia', 'J-15678901-4', 'Profesor Titular', 'Biología', 'Activo', '2021-03-12', NULL, NULL, 'https://i.pravatar.cc/150?img=8'),
('V-10293847', 'Carmen Elena', 'Suarez Navarro', 'J-10293847-5', 'Asistente Administrativo', 'Química', 'Retirado', '1988-11-04', '2015-12-15', NULL, 'https://i.pravatar.cc/150?img=42'),
('V-21345678', 'David Andres', 'Bello Castillo', 'J-21345678-6', 'Técnico de Laboratorio', 'Física', 'Activo', '2019-07-25', NULL, NULL, 'https://i.pravatar.cc/150?img=13'),
('V-25678912', 'Sofia Valentina', 'Herrera Mendez', 'J-25678912-7', 'Asistente Contable', 'Matemáticas', 'Retirado', '2022-02-14', NULL, NULL, 'https://i.pravatar.cc/150?img=28'),
('V-11223344', 'Pedro Miguel', 'Hernandez Castro', 'J-11223344-5', 'Profesor Titular', 'Matemáticas', 'Jubilado', '1985-03-10', '2015-06-30', NULL, 'https://i.pravatar.cc/150?img=12'),
('V-55443322', 'Luisa Elena', 'Rodriguez Moreno', 'J-55443322-4', 'Secretaria Ejecutiva', 'Decanato', 'Jubilado', '1990-07-15', '2020-12-31', NULL, 'https://i.pravatar.cc/150?img=25'),
('V-77889900', 'Roberto Enrique', 'Vargas Peña', 'J-77889900-6', 'Jefe de Taller', 'Mantenimiento', 'Jubilado', '1992-01-20', '2018-05-15', NULL, 'https://i.pravatar.cc/150?img=18');

-- Poblar catálogos dinámicamente resguardando valores únicos
INSERT INTO public.cargos (nombre) SELECT DISTINCT cargo FROM tmp_rrhh_personas WHERE cargo NOT IN (SELECT nombre FROM public.cargos);
INSERT INTO public.departamentos (nombre) SELECT DISTINCT departamento FROM tmp_rrhh_personas WHERE departamento NOT IN (SELECT nombre FROM public.departamentos);
INSERT INTO public.estados_laborales (estados) SELECT DISTINCT estado FROM tmp_rrhh_personas WHERE estado NOT IN (SELECT estados FROM public.estados_laborales);

-- Cruzar datos de texto e insertar en Empleados con sus IDs correspondientes
INSERT INTO public.empleados (cedula, nombres, apellidos, rif, cargo_id, departamento_id, estado_id, fecha_ingreso, fecha_jubilacion, fecha_pension, foto_url)
SELECT t.cedula, t.nombres, t.apellidos, t.rif, c.id, d.id, e.id, t.fecha_ingreso, t.fecha_jubilacion, t.fecha_pension, t.foto_url
FROM tmp_rrhh_personas t
JOIN public.cargos c ON t.cargo = c.nombre
JOIN public.departamentos d ON t.departamento = d.nombre
JOIN public.estados_laborales e ON t.estado = e.estados;

DROP TABLE tmp_rrhh_personas;

-- =============================================================================
-- 6. CARGA, HOMOLOGACIÓN AUTOMÁTICA E INSERCIÓN DEL CSV DE ARCHIVOS
-- =============================================================================

CREATE TEMP TABLE tmp_csv_archivos (
    titulo text,
    autor text,
    abstract text,
    fecha_documento date,
    tesauro_primario text,
    tesauro_secundario text,
    descriptores_libres text,
    ubicacion text
);

INSERT INTO tmp_csv_archivos (titulo, autor, abstract, fecha_documento, tesauro_primario, tesauro_secundario, descriptores_libres, ubicacion) VALUES
('Informe Anual de Gestión 2023','Decanato Ciencias','Informe anual de gestión institucional de la Facultad de Ciencias correspondiente al año 2023.','2024-01-10','Informe','Parte I','Evaluación de desempeño institucional, Informe, Rendición de cuentas académica, cumplimiento de metas','Digitalizado Exclusivo'),
('Plan Regulador de Áreas Verdes','Arq. Villanueva','Plan maestro de ordenamiento y diseño paisajístico de las áreas verdes del campus universitario.','2022-11-20','Plano Arquitectónico','Parte IV','Ordenamiento territorial interno, Planificación ambiental universitaria, Plano Arquitectónico, inventario vegetal','Mapoteca - Gaveta 1'),
('Planos Eléctricos Edificio A','Ing. Ramirez','Planos técnicos de la red de distribución eléctrica e iluminación del Edificio A de laboratorios.','2023-12-01','Plano Arquitectónico','Parte IV','Actualización de infraestructura técnica, Ingeniería eléctrica edilicia, Plano Arquitectónico, mantenimiento preventivo','Mapoteca - Gaveta 4'),
('Planos Nuevo Pabellón Biología','Arq. Mendez','Planos estructurales y de distribución del nuevo pabellón de investigación del Instituto de Biología.','2024-01-15','Plano Arquitectónico','Parte IV','Arquitectura académica, Infraestructura universitaria, Plano Arquitectónico, distribución espacial','Mapoteca - Gaveta 3'),
('Reglamento Interno de Trabajos de Grado','Consejo de Facultad','Normativa vigente para la presentación y evaluación de trabajos especiales de grado de licenciatura.','2021-05-18','Reglamento','Parte I','Normativa académica, Licenciatura, Trabajo de grado, Consejo de Facultad','Archivo General - Estante A'),
('Presupuesto Anual de Funcionamiento 2024','Dirección de Administración','Distribución presupuestaria aprobada para gastos operativos y de investigación del ejercicio fiscal 2024.','2023-11-15','Presupuesto','Parte II','Planificación financiera, Presupuesto anual, Gastos operativos, Gestión administrativa','Bóveda de Seguridad - Estante 2'),
('Plan de Desarrollo Curricular de Computación','Escuela de Computación','Propuesta de actualización del plan de estudios de la Licenciatura en Computación bajo enfoque por competencias.','2022-06-30','Plan de Estudio','Parte I','Reforma curricular, Computación, Pensum de estudios, Acreditación académica','Archivo General - Estante C'),
('Convenio de Cooperación UCV - IVIC','Rectorado UCV','Acuerdo marco de cooperación institucional para el desarrollo de proyectos de investigación conjunta de postgrado.','2020-10-05','Convenio','Parte III','Alianza interinstitucional, Investigación científica, Postgrado conjunto, Cooperación técnica','Digitalizado Exclusivo'),
('Acta de Grado Promoción Ciencias 2023','Control de Estudios','Acta oficial de otorgamiento de títulos de la promoción de Licenciados de la Facultad de Ciencias.','2023-07-20','Acta de Grado','Parte II','Graduación, Registro de egresados, Actas de grado, Promoción académica','Archivo Histórico - Caja 4'),
('Inventario de Equipos de Alta Tecnología','Coordinación de Laboratorios','Catálogo detallado y estado de funcionamiento de los equipos de microscopía y resonancia de la facultad.','2023-09-12','Inventario','Parte IV','Control patrimonial, Microscopía electrónica, Resonancia magnética, Equipamiento científico','Sótano A - Gaveta 12'),
('Informe de Autoevaluación Institucional 2022','Comisión de Acreditación','Resultado del proceso de autoevaluación interna de la facultad con miras a la reacreditación internacional.','2022-12-15','Informe','Parte I','Calidad educativa, Autoevaluación, Indicadores académicos, Acreditación internacional','Digitalizado Exclusivo'),
('Planos de Instalaciones Sanitarias Edificio B','Ing. Mendoza','Plano de la red de tuberías de aguas blancas, servidas y sistemas de drenaje del Edificio B.','2018-04-22','Plano Sanitario','Parte IV','Red de tuberías, Edificio de docencia, Instalaciones sanitarias, Mantenimiento preventivo','Mapoteca - Gaveta 5'),
('Resolución de Creación del Doctorado en Química','Consejo Universitario','Resolución del Consejo Universitario que aprueba formalmente la creación y apertura del Doctorado en Química.','1998-03-11','Resolución','Parte II','Doctorado en Química, Postgrado, Creación de programa, Resolución oficial','Bóveda de Seguridad - Estante 1'),
('Informe Técnico de Vulnerabilidad Estructural','Instituto de Materiales','Estudio y diagnóstico técnico de las estructuras de concreto y acero de los pabellones más antiguos de la facultad.','2021-10-30','Informe Técnico','Parte III','Vulnerabilidad estructural, Pabellones antiguos, Diagnóstico de concreto, Ingeniería civil','Archivo Técnico - Estante D'),
('Plan Estratégico Decanal 2023-2026','Decanato Ciencias','Líneas estratégicas y metas de desarrollo de la Facultad de Ciencias para el periodo de gestión actual.','2023-03-15','Plan Estratégico','Parte I','Planificación estratégica, Visión decanal, Metas de gestión, Desarrollo institucional','Digitalizado Exclusivo'),
('Acta de Sesión Extraordinaria Nro 15','Consejo de Facultad','Acta oficial de la discusión plenaria del Consejo de Facultad sobre el reinicio presencial de clases.','2022-09-18','Acta de Sesión','Parte II','Consejo de Facultad, Clases presenciales, Discusión académica, Actas oficiales','Archivo de Actas - Tomo 2022'),
('Guía de Normas de Seguridad en Laboratorios','Coordinación de Higiene','Manual y protocolo oficial de seguridad biológica, química y radiológica para estudiantes y profesores.','2019-11-04','Manual','Parte I','Seguridad en laboratorios, Protocolo biológico, Normas de higiene, Prevención de riesgos','Digitalizado Exclusivo'),
('Resolución de Designación de Jefes de Departamento','Decanato Ciencias','Resolución rectoral de designación de los nuevos Jefes de Departamento para el periodo académico actual.','2023-06-15','Resolución','Parte I','Designación de cargos, Jefes de departamento, Gestión académica, Resoluciones decanales','Expedientes de Gestión - Caja 2'),
('Proyecto de Investigación: Nanotecnología Aplicada','Dr. Briceño','Propuesta y fases de ejecución del proyecto de investigación sobre síntesis de nanopartículas de oro en medicina.','2021-08-25','Proyecto','Parte III','Nanotecnología aplicada, Nanopartículas de oro, Investigación médica, Proyecto financiado','Archivo Técnico - Estante E'),
('Planos Estructurales Auditorio de Ciencias','Arq. Villanueva','Plano original detallado del diseño de fundaciones, vigas y techado del Auditorio Principal de la Facultad.','1965-02-18','Plano Arquitectónico','Parte IV','Planos originales, Auditorio de Ciencias, Fundaciones estructurales, Patrimonio arquitectónico','Mapoteca - Gaveta 2');

-- Insertar los expedientes directamente a la tabla oficial de datos_archivo
INSERT INTO public.datos_archivo (titulo, autor, abstract, fecha_documento, tesauro_primario, tesauro_secundario, ubicacion, creado_por)
SELECT
    titulo,
    autor,
    abstract,
    fecha_documento,
    tesauro_primario,
    tesauro_secundario,
    ubicacion,
    2 -- Pista de auditoría vinculada al ID del usuario 'archivo_admin'
FROM tmp_csv_archivos;

-- Población de la tabla de catálogo descriptores_libres con los descriptores de datos_archivo
INSERT INTO public.descriptores_libres (nombre)
SELECT DISTINCT TRIM(val)
FROM (
    SELECT regexp_split_to_table(descriptores_libres, ',') AS val
    FROM tmp_csv_archivos
) sub
WHERE val IS NOT NULL AND TRIM(val) != ''
ON CONFLICT (nombre) DO NOTHING;

-- Relacionar datos_archivo con sus descriptores libres en la tabla de unión
INSERT INTO public.archivo_descriptores (id_archivo, id_descriptor)
SELECT DISTINCT da.id_archivo, dl.id_descriptor
FROM tmp_csv_archivos tmp
JOIN public.datos_archivo da ON da.titulo = tmp.titulo
CROSS JOIN LATERAL regexp_split_to_table(tmp.descriptores_libres, ',') AS term
JOIN public.descriptores_libres dl ON LOWER(TRIM(dl.nombre)) = LOWER(TRIM(term))
ON CONFLICT DO NOTHING;

DROP TABLE tmp_csv_archivos;

-- =============================================================================
-- 6.5 CARGA DE DATOS DE PRUEBA PARA RRHH
-- =============================================================================

CREATE TEMP TABLE tmp_rrhh_documentos (
    titulo text,
    autor text,
    abstract text,
    fecha_documento date,
    tesauro_primario text,
    tesauro_secundario text,
    descriptores_libres text,
    ubicacion text,
    doc_type text
);

INSERT INTO tmp_rrhh_documentos (titulo, autor, abstract, fecha_documento, tesauro_primario, tesauro_secundario, descriptores_libres, ubicacion, doc_type) VALUES
('Contrato Individual de Trabajo','Recursos Humanos','Contrato de trabajo administrativo','2015-06-20','Contratos y Suplencias','Parte I','Contrato, Ingreso, Administrativo','Digitalizado Exclusivo','Contrato'),
('Nombramiento de Cátedra Docente','Recursos Humanos','Nombramiento ordinario provisional de docente','1985-03-10','Nombramientos y Designaciones','Parte I','Nombramiento, Docente, Escalafón','Expedientes Jubilados - Caja RRHH-01','Nombramiento'),
('Acta de Jubilación de Personal Ordinario','Recursos Humanos','Acta de jubilacion ordinaria aprobada por Consejo de Facultad','2015-06-30','Jubilación y Pensión','Parte II','Jubilación, Pensión, Trámite','Digitalizado Exclusivo','Acta de Jubilación'),
('Cedula de Identidad - Expediente Personal','Recursos Humanos','Copia de la cedula de identidad del empleado archivada en el expediente personal.','2015-06-20','Cédula','Parte IV','Cédula, Identificación, Documento personal','Digitalizado Exclusivo','Cedula de Identidad'),
('Registro de Informacion Fiscal del Empleado','Recursos Humanos','Copia del RIF del empleado consignada para el expediente de nomina.','2015-06-20','RIF','Parte IV','RIF, Fiscal, Impuestos, SENIAT','Digitalizado Exclusivo','RIF'),
('Curriculum Vitae y Soportes Academicos','Recursos Humanos','Curriculum vitae y soportes academicos consignados al momento del ingreso.','2015-06-20','CV','Parte IV','Curriculum, Formación académica, Experiencia laboral','Digitalizado Exclusivo','Curriculum Vitae'),
('Planilla de Actualizacion de Datos Personales','Recursos Humanos','Planilla de registro y actualizacion de datos personales del empleado.','2015-06-20','Actualización de datos','Parte IV','Datos personales, Planilla, Actualización','Digitalizado Exclusivo','Planilla de Datos');

-- Insertar los documentos de RRHH en la tabla datos_rrhh (sin la columna descriptores_libres que ahora es relacional)
INSERT INTO public.datos_rrhh (titulo, autor, abstract, fecha_documento, tesauro_primario, tesauro_secundario, ubicacion, creado_por, id_tipo_documento, empleado_id)
SELECT
    tmp.titulo,
    tmp.autor,
    tmp.abstract,
    tmp.fecha_documento,
    tmp.tesauro_primario,
    tmp.tesauro_secundario,
    tmp.ubicacion,
    2, -- creado_por (archivo_admin)
    td.id,
    e.id
FROM tmp_rrhh_documentos tmp
JOIN public.empleados e ON e.cedula = 'V-12345678' OR e.cedula = 'V-11223344'
JOIN public.tipo_documento td ON LOWER(UNACCENT(td.nombre)) =
    CASE
        WHEN LOWER(tmp.doc_type) = 'contrato' THEN 'contratos, renovaciones, prorrogas de contratos y suplencias'
        WHEN LOWER(tmp.doc_type) = 'nombramiento' THEN 'nombramiento provisional, nombramiento definitivo, otros nombramientos y designaciones'
        WHEN LOWER(tmp.doc_type) = 'acta de jubilación' THEN 'jubilacion y pension'
        WHEN LOWER(tmp.doc_type) = 'cedula de identidad' THEN 'cedula de identidad'
        WHEN LOWER(tmp.doc_type) = 'rif' THEN 'registro de informacion fiscal (rif)'
        WHEN LOWER(tmp.doc_type) = 'curriculum vitae' THEN 'curriculum vitae y anexos'
        WHEN LOWER(tmp.doc_type) = 'planilla de datos' THEN 'registro y/o planilla de datos personales o de actualizacion de datos'
        ELSE LOWER(tmp.doc_type)
    END;

-- Población de la tabla de catálogo descriptores_libres con los descriptores de datos_rrhh
INSERT INTO public.descriptores_libres (nombre)
SELECT DISTINCT TRIM(val)
FROM (
    SELECT regexp_split_to_table(descriptores_libres, ',') AS val
    FROM tmp_rrhh_documentos
) sub
WHERE val IS NOT NULL AND TRIM(val) != ''
ON CONFLICT (nombre) DO NOTHING;

-- Relacionar datos_rrhh con sus descriptores libres en la tabla de unión
INSERT INTO public.rrhh_descriptores (id_rrhh, id_descriptor)
SELECT DISTINCT dr.id_rrhh, dl.id_descriptor
FROM tmp_rrhh_documentos tmp
JOIN public.datos_rrhh dr ON dr.titulo = tmp.titulo
CROSS JOIN LATERAL regexp_split_to_table(tmp.descriptores_libres, ',') AS term
JOIN public.descriptores_libres dl ON LOWER(TRIM(dl.nombre)) = LOWER(TRIM(term))
ON CONFLICT DO NOTHING;

DROP TABLE tmp_rrhh_documentos;

-- =============================================================================
-- 7. CREACIÓN DE LLAVES FORÁNEAS (INTEGRIDAD REFERENCIAL)
-- =============================================================================

ALTER TABLE ONLY public.tipo_documento
    ADD CONSTRAINT fk_tipo_documento_categoria FOREIGN KEY (id_categoria) REFERENCES public.categoria(id) ON DELETE RESTRICT;

ALTER TABLE ONLY public.datos_archivo
    ADD CONSTRAINT fk_datos_archivo_usuario FOREIGN KEY (creado_por) REFERENCES public.usuarios_sistema(id) ON DELETE RESTRICT;

ALTER TABLE ONLY public.datos_rrhh
    ADD CONSTRAINT fk_datos_rrhh_tipo FOREIGN KEY (id_tipo_documento) REFERENCES public.tipo_documento(id) ON DELETE RESTRICT,
    ADD CONSTRAINT fk_datos_rrhh_empleado FOREIGN KEY (empleado_id) REFERENCES public.empleados(id) ON DELETE SET NULL,
    ADD CONSTRAINT fk_datos_rrhh_usuario FOREIGN KEY (creado_por) REFERENCES public.usuarios_sistema(id) ON DELETE RESTRICT;

ALTER TABLE ONLY public.empleados
    ADD CONSTRAINT fk_empleado_cargo FOREIGN KEY (cargo_id) REFERENCES public.cargos(id) ON DELETE RESTRICT,
    ADD CONSTRAINT fk_empleado_departamento FOREIGN KEY (departamento_id) REFERENCES public.departamentos(id) ON DELETE RESTRICT,
    ADD CONSTRAINT fk_empleado_estado FOREIGN KEY (estado_id) REFERENCES public.estados_laborales(id) ON DELETE RESTRICT;

ALTER TABLE ONLY public.fotos_empleado
    ADD CONSTRAINT fk_fotos_empleado_ref FOREIGN KEY (empleado_id) REFERENCES public.empleados(id) ON DELETE CASCADE;

ALTER TABLE ONLY public.historial_cargos
    ADD CONSTRAINT fk_historial_empleado FOREIGN KEY (empleado_id) REFERENCES public.empleados(id) ON DELETE CASCADE,
    ADD CONSTRAINT fk_historial_cargo FOREIGN KEY (cargo_id) REFERENCES public.cargos(id) ON DELETE RESTRICT;

-- =============================================================================
-- 8. ÍNDICES PARA RENDIMIENTO INSTITUCIONAL (BUENAS PRÁCTICAS)
-- =============================================================================

-- 8.1 Índices en claves foráneas (aceleran JOINs)
CREATE INDEX idx_datos_rrhh_empleado     ON public.datos_rrhh(empleado_id);
CREATE INDEX idx_datos_rrhh_tipo_doc     ON public.datos_rrhh(id_tipo_documento);
CREATE INDEX idx_datos_rrhh_creado_por   ON public.datos_rrhh(creado_por);
CREATE INDEX idx_datos_archivo_creado_por ON public.datos_archivo(creado_por);
CREATE INDEX idx_empleados_cargo         ON public.empleados(cargo_id);
CREATE INDEX idx_empleados_departamento  ON public.empleados(departamento_id);
CREATE INDEX idx_empleados_estado        ON public.empleados(estado_id);
CREATE INDEX idx_fotos_empleado_ref      ON public.fotos_empleado(empleado_id);
CREATE INDEX idx_historial_empleado      ON public.historial_cargos(empleado_id);
CREATE INDEX idx_tipo_documento_categoria ON public.tipo_documento(id_categoria);
CREATE INDEX idx_archivo_descriptores_arch ON public.archivo_descriptores(id_archivo);
CREATE INDEX idx_archivo_descriptores_desc ON public.archivo_descriptores(id_descriptor);
CREATE INDEX idx_rrhh_descriptores_rrhh ON public.rrhh_descriptores(id_rrhh);
CREATE INDEX idx_rrhh_descriptores_desc ON public.rrhh_descriptores(id_descriptor);

-- 8.2 Índices en fechas (aceleran ORDER BY y filtros por rango)
CREATE INDEX idx_datos_archivo_fecha     ON public.datos_archivo(fecha_documento DESC);
CREATE INDEX idx_datos_rrhh_fecha        ON public.datos_rrhh(fecha_documento DESC);
CREATE INDEX idx_empleados_fecha_ingreso ON public.empleados(fecha_ingreso DESC);

-- 8.3 Índices GIN para búsqueda de texto (aceleran ILIKE con unaccent)
CREATE INDEX idx_datos_archivo_titulo_trgm      ON public.datos_archivo USING GIN (unaccent(titulo) gin_trgm_ops);
CREATE INDEX idx_datos_archivo_tesauro1_trgm    ON public.datos_archivo USING GIN (unaccent(tesauro_primario) gin_trgm_ops);
CREATE INDEX idx_datos_archivo_tesauro2_trgm    ON public.datos_archivo USING GIN (unaccent(tesauro_secundario) gin_trgm_ops);
CREATE INDEX idx_descriptores_libres_nombre_trgm ON public.descriptores_libres USING GIN (unaccent(nombre) gin_trgm_ops);
CREATE INDEX idx_datos_rrhh_titulo_trgm         ON public.datos_rrhh USING GIN (unaccent(titulo) gin_trgm_ops);
CREATE INDEX idx_empleados_nombres_trgm         ON public.empleados USING GIN (unaccent(nombres || ' ' || apellidos) gin_trgm_ops);

-- =============================================================================
-- 9. VISTA AGREGADA DE ÍNDICE DE PERSONAS (RRHH)
-- Simplifica la lógica de búsqueda y listado de personal evitando Pandas.
-- =============================================================================

CREATE OR REPLACE VIEW public.vw_rrhh_persona_index AS
SELECT
    e.id                                            AS empleado_id,
    e.cedula,
    e.rif,
    e.nombres || ' ' || e.apellidos                 AS persona_raw,
    COALESCE(c.nombre,  'Sin cargo asignado')       AS cargo,
    COALESCE(d.nombre,  '')                         AS departamento,
    COALESCE(el.estados,'Sin estado')               AS estado,
    TO_CHAR(e.fecha_ingreso,    'YYYY-MM-DD')       AS fecha_ingreso,
    TO_CHAR(e.fecha_jubilacion, 'YYYY-MM-DD')       AS fecha_jubilacion,
    TO_CHAR(e.fecha_pension,    'YYYY-MM-DD')       AS fecha_pension,
    COALESCE(e.foto_url, '')                        AS foto_url,
    COUNT(dr.id_rrhh)                               AS doc_count,
    COALESCE(
        STRING_AGG(DISTINCT COALESCE(td.nombre_corto, td.nombre, ''), '; ')
        FILTER (WHERE td.nombre IS NOT NULL AND td.nombre <> ''),
        ''
    )                                               AS tipos
FROM public.empleados e
LEFT JOIN public.cargos            c  ON e.cargo_id        = c.id
LEFT JOIN public.departamentos     d  ON e.departamento_id = d.id
LEFT JOIN public.estados_laborales el ON e.estado_id       = el.id
LEFT JOIN public.datos_rrhh        dr ON dr.empleado_id    = e.id
LEFT JOIN public.tipo_documento    td ON dr.id_tipo_documento = td.id
GROUP BY e.id, c.nombre, d.nombre, el.estados;

-- =============================================================================
-- 10. COLUMNA file_url PARA VISOR DE DOCUMENTOS DIGITALIZADOS
-- Ejecutar en instancias existentes si el esquema ya fue creado sin esta columna.
-- ALTER TABLE public.datos_archivo ADD COLUMN IF NOT EXISTS file_url TEXT;
-- ALTER TABLE public.datos_rrhh    ADD COLUMN IF NOT EXISTS file_url TEXT;
-- =============================================================================

-- =============================================================================
-- Fin del Script. Base de datos e inicio de sesión integrados correctamente.
-- =============================================================================
