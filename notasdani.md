Escribo las dudas que me surgen mientras leo el README.md 

Parte objetivos: Respecto a la soberania, el objetivo 1. como garantizas que los datos no los agarrre alguien externo?
Audit trail, tracking de quién accede a qué. Debo ver como funciona esto. Y todo en general xd. 
El objetivo de escalabilidad, como subo los csv a postgresql? 

Parte inicio rapido: tutto ok 200. Solo tuve error intalando numpy que me pidio una version anterior a la que tenia instalada pero nada mas. Por cierto bellisimo como se ve en el navegador la parte del login. 

Donde tenias escrito credenciales de prueba ext_normal y ext_admin, te coloque archivo_normal y archivo_admin.

Para yo entender la arquitectura basicamnete funciona asi: 

 Usuario
  ↓ (click / input)
rrhh.js
  ↓ fetch("/api/rrhh/buscar")
Navegador
  ↓ HTTP request
FastAPI (/api/rrhh/buscar)
  ↓
CSV (rrhh_personas.csv)
  ↓
FastAPI (filtra datos)
  ↓
JSON response
  ↓
rrhh.js
  ↓
Pantalla actualizada


Estructura del proyecto:
Puedo editar en python-app/static/: diseño de la web, botones,formularios,lógica del navegador,llamadas a la API,comportamiento visual

Salte a la parte de pendientes, mucha info previa que no entedere de una. :)

He agregado el logo de ciencias a la barra lateral. 

