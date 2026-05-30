-- =============================================================================
-- SCRIPT INSTITUCIONAL DE CREACIÓN Y POBLACIÓN DE BASE DE DATOS DEFINITIVO
-- PROYECTO: GESTIÓN DOCUMENTAL, EXPEDIENTES DE RRHH Y AUDITORÍA DE ARCHIVOS
-- =============================================================================

-- 1. ACTIVAR EXTENSIONES PARA EL TRATAMIENTO DE TEXTO (Para quitar acentos en Slugs)
CREATE EXTENSION IF NOT EXISTS unaccent;

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
    codigo_documento CHARACTER VARYING(30) UNIQUE, -- Almacena códigos tipo 'DOC-001'
    titulo TEXT NOT NULL,
    abstract TEXT,
    autor_ente TEXT, -- Ente o departamento que emitió el documento
    id_tipo_documento INTEGER NOT NULL,
    empleado_id INTEGER, -- Vinculación con el empleado dueño del expediente
    fecha_documento DATE DEFAULT CURRENT_DATE,
    tesauro_primario TEXT,
    tesauro_secundario TEXT,
    descriptores_libres TEXT,
    ubicacion TEXT NOT NULL, -- Gaveta física, estante o 'Digitalizado Exclusivo'
    creado_por INTEGER NOT NULL, -- Pista de Auditoría: ID del usuario del sistema
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
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
-- 5. GENERACIÓN AUTOMÁTICA DE SLUGS (PROCESO INSTITUCIONAL)
-- =============================================================================

UPDATE public.tipo_documento
SET slug = SUBSTRING(
    REGEXP_REPLACE(
        REGEXP_REPLACE(
            LOWER(UNACCENT(nombre)),
            '[^a-z0-9\s_]', '', 'g'
        ),
        '\s+', '-', 'g'
    ) FROM 1 FOR 250
);

-- =============================================================================
-- 6. CARGA Y PROCESAMIENTO AUTOMÁTICO DE EMPLEADOS DE NÓMINA INICIAL
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
-- 7. CARGA, HOMOLOGACIÓN AUTOMÁTICA E INSERCIÓN DEL CSV DE ARCHIVOS
-- =============================================================================

CREATE TEMP TABLE tmp_csv_archivos (
    id_archivo varchar(20), cedula varchar(20), doc_type text, ubicacion text, nombres text, apellidos text
);

INSERT INTO tmp_csv_archivos (id_archivo, cedula, doc_type, ubicacion, nombres, apellidos) VALUES
('DOC-001','V-12345678','Hoja de Vida','Expedientes Activos - Gaveta 14','Carlos Alberto','Gomez Perez'),
('DOC-002','V-12345678','Contrato','Digitalizado Exclusivo','Carlos Alberto','Gomez Perez'),
('DOC-003','V-12345678','Resoluciones','Expedientes Activos - Gaveta 14','Carlos Alberto','Gomez Perez'),
('DOC-004','V-18765432','Evaluación Desempeño','Expedientes Activos - Gaveta 10','Ana Lucia','Silva Torres'),
('DOC-005','V-18765432','Nómina Especial','Digitalizado Exclusivo','Ana Lucia','Silva Torres'),
('DOC-006','V-11223344','Nombramiento','Expedientes Jubilados - Caja RRHH-01','Pedro Miguel','Hernandez Castro'),
('DOC-007','V-11223344','Acta de Jubilación','Digitalizado Exclusivo','Pedro Miguel','Hernandez Castro'),
('DOC-008','V-25678912','Evaluación Desempeño','Expedientes Activos - Gaveta 11','Sofia Valentina','Herrera Mendez'),
('DOC-009','V-25678912','Contrato','Caja RRHH-02','Sofia Valentina','Herrera Mendez'),
('DOC-010','V-09876543','Certificado Médico','Expedientes Pasivos - Sotano A','José Antonio','Martinez Rojas'),
('DOC-011','V-23456789','Antecedentes Penales','Expedientes Activos - Gaveta 12','Maria Fernanda','Lopez Ruiz'),
('DOC-012','V-15678901','Título Universitario','Expedientes Activos - Gaveta 13','Luis Miguel','Rodriguez Garcia'),
('DOC-013','V-21345678','Oficios Varios','Expedientes Pasivos - Sotano B','David Andres','Bello Castillo'),
('DOC-014','V-21345678','Contrato','Digitalizado Exclusivo','David Andres','Bello Castillo'),
('DOC-015','V-10293847','Hoja de Vida','Expedientes Activos - Gaveta 15','Carmen Elena','Suarez Navarro'),
('DOC-016','V-55443322','Resoluciones','Expedientes Activos - Gaveta 16','Luisa Elena','Rodriguez Moreno'),
('DOC-017','V-55443322','Contrato','Digitalizado Exclusivo','Luisa Elena','Rodriguez Moreno'),
('DOC-018','V-77889900','Evaluación Desempeño','Expedientes Activos - Gaveta 17','Roberto Enrique','Vargas Peña'),
('DOC-019','V-25678912','Nómina Especial','Digitalizado Exclusivo','Sofia Valentina','Herrera Mendez'),
('DOC-020','V-12345678','Nombramiento','Expedientes Jubilados - Caja RRHH-03','Carlos Alberto','Gomez Perez');

-- Control de Alertas de Empleados No Registrados (Mapeo a banderas Por Asignar)
INSERT INTO public.empleados (cedula, nombres, apellidos, cargo_id, departamento_id, estado_id, fecha_ingreso)
SELECT DISTINCT
    csv.cedula,
    csv.nombres,
    csv.apellidos,
    (SELECT id FROM public.cargos WHERE nombre = 'Por Asignar'),
    (SELECT id FROM public.departamentos WHERE nombre = 'Por Asignar'),
    (SELECT id FROM public.estados_laborales WHERE estados = 'Pendiente de Registro'),
    CURRENT_DATE
FROM tmp_csv_archivos csv
WHERE NOT EXISTS (SELECT 1 FROM public.empleados e WHERE e.cedula = csv.cedula);

-- Insertar los expedientes traduciendo dinámicamente los términos del CSV al catálogo oficial institucional
INSERT INTO public.datos_archivo (codigo_documento, titulo, autor_ente, id_tipo_documento, empleado_id, ubicacion, creado_por)
SELECT
    csv.id_archivo,
    csv.doc_type || ' de ' || csv.nombres || ' ' || csv.apellidos,
    'Recursos Humanos',
    td.id,
    emp.id,
    csv.ubicacion,
    2 -- Pista de auditoría vinculada al ID del usuario 'archivo_admin'
FROM tmp_csv_archivos csv
JOIN public.empleados emp ON csv.cedula = emp.cedula
JOIN public.tipo_documento td ON LOWER(UNACCENT(td.nombre)) =
    CASE
        WHEN LOWER(csv.doc_type) = 'hoja de vida' THEN 'curriculum vitae y anexos'
        WHEN LOWER(csv.doc_type) = 'contrato' THEN 'contratos, renovaciones, prorrogas de contratos y suplencias'
        WHEN LOWER(csv.doc_type) = 'resoluciones' THEN 'nombramiento provisional, nombramiento definitivo, otros nombramientos y designaciones'
        WHEN LOWER(csv.doc_type) = 'evaluación desempeño' THEN 'estudios de cargos y evaluacion de credenciales'
        WHEN LOWER(csv.doc_type) = 'nómina especial' THEN 'registro de asignacion de sueldos (ras)'
        WHEN LOWER(csv.doc_type) = 'nombramiento' THEN 'nombramiento provisional, nombramiento definitivo, otros nombramientos y designaciones'
        WHEN LOWER(csv.doc_type) = 'acta de jubilación' THEN 'jubilacion y pension'
        WHEN LOWER(csv.doc_type) = 'certificado médico' THEN 'documentos de ingreso (planillas de solicitud de empleo, evaluacion medica pre_empleo, copia certificado de salud, copia numero de cuenta bancaria, periodo de prueba)'
        WHEN LOWER(csv.doc_type) = 'antecedentes penales' THEN 'documentos de ingreso (planillas de solicitud de empleo, evaluacion medica pre_empleo, copia certificado de salud, copia numero de cuenta bancaria, periodo de prueba)'
        WHEN LOWER(csv.doc_type) = 'título universitario' THEN 'curriculum vitae y anexos'
        WHEN LOWER(csv.doc_type) = 'oficios varios' THEN 'modificaciones y actualizacion de datos (movimientos de personal)'
        ELSE LOWER(csv.doc_type) -- Por si acaso hay coincidencia exacta de otro tipo
    END;

DROP TABLE tmp_csv_archivos;

-- =============================================================================
-- 8. CREACIÓN DE LLAVES FORÁNEAS (INTEGRIDAD REFERENCIAL)
-- =============================================================================

ALTER TABLE ONLY public.tipo_documento
    ADD CONSTRAINT fk_tipo_documento_categoria FOREIGN KEY (id_categoria) REFERENCES public.categoria(id) ON DELETE RESTRICT;

ALTER TABLE ONLY public.datos_archivo
    ADD CONSTRAINT fk_datos_archivo_tipo FOREIGN KEY (id_tipo_documento) REFERENCES public.tipo_documento(id) ON DELETE RESTRICT,
    ADD CONSTRAINT fk_datos_archivo_empleado FOREIGN KEY (empleado_id) REFERENCES public.empleados(id) ON DELETE SET NULL,
    ADD CONSTRAINT fk_datos_archivo_usuario FOREIGN KEY (creado_por) REFERENCES public.usuarios_sistema(id) ON DELETE RESTRICT;

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
-- Fin del Script. Base de datos e inicio de sesión integrados correctamente.
-- =============================================================================
