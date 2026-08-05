# Módulo de IA — Asistente del Archivo

Adaptado de dos implementaciones ya en producción: el asistente del PMS Diamond (Laravel +
OpenRouter, con herramientas, caché de prompt y separación por modos) y el bot de la app
Diary (Vercel + OpenRouter, con tope de gasto diario y cerebro editable). Lo que sigue es lo
que se trajo, lo que se dejó y por qué.

## Qué es

Una burbuja de chat flotante en todas las pantallas. Responde preguntas sobre los documentos
del Archivo y —para personal autenticado— sobre los expedientes de RRHH: qué hay, de qué
trata, de qué año es, dónde está en físico y **el enlace al digitalizado cuando existe**.

## La arquitectura, en una frase por capa

| Archivo | Qué hace |
|---|---|
| `python-app/core/ia.py` | La tubería: llamada a OpenRouter, ciclo de herramientas, caché de prompt, costo. No sabe nada del archivo. |
| `python-app/core/ia_tools.py` | Las herramientas: **lo único que toca la base**. Todas de solo lectura. |
| `python-app/core/ia_prompts.py` | El prompt de sistema por perfil, más la parte editable desde el panel. |
| `python-app/core/ia_propuestas.py` | La bandeja de aprobación: donde una propuesta se vuelve un cambio real. |
| `python-app/routes/ia.py` | Los endpoints, la resolución de permisos y el tope de gasto. |
| `python-app/static/ia-widget.js` | La burbuja. No sabe nada: manda y pinta. |
| `python-app/static/admin_ia.html` | Panel: cerebro, gasto, conversaciones y elección de modelo. |

## Tres perfiles — lo más importante

No es el mismo asistente con distinta piel. Lo separan **con quién habla** y **qué puede
hacer**. El perfil sale de la sesión firmada (cookie `ds_session`, HMAC) y de
`usuarios_sistema`; **nada de lo que mande el navegador influye**.

| | `publico` | `consulta` | `editor` |
|---|---|---|---|
| Quién | Sin sesión | Sesión, rol Normal | Sesión, rol Admin |
| Alcance | Solo Archivo | Sus módulos | Sus módulos |
| Escritura | ❌ | ❌ | Propone, no ejecuta |
| Adjuntar archivos | ❌ | ❌ | ✅ |
| Historial | 10 mensajes | 20 | 30 |

Y **encima del perfil hay un segundo filtro: el módulo.** Un admin del Archivo tiene perfil
`editor` pero no ve ninguna herramienta de RRHH, porque su usuario no tiene ese módulo. Los
dos filtros son independientes y se aplican los dos.

**La barrera de privacidad es el esquema, no el prompt.** Lo que impide que un desconocido
saque la cédula y el expediente de un empleado no es que se lo hayamos prohibido por
escrito: es que `buscar_empleado` **no está en la lista de herramientas** que se le ofrece
al modelo. El prompt es la primera línea y la más débil.

La comprobación se hace **dos veces** —al armar el esquema y al ejecutar la herramienta— a
propósito: en el proyecto original casi se filtra un huésped porque alguien olvidó pasar el
modo en una llamada y el default lo concedió. Un argumento se olvida; dos cerrojos no.

Sin sesión, `/api/ia/chat` no lanza 401: **degrada a `publico`**. Un chat que se cae al
expirar la sesión es un chat que la gente deja de usar, y degradar es seguro porque ese
perfil no tiene ninguna herramienta de personal.

## Las herramientas

| Herramienta | publico | consulta | editor | Módulo |
|---|---|---|---|---|
| `buscar_archivo` (texto, tipo, autor, palabra clave, años, solo digitalizados) | ✅ | ✅ | ✅ | — |
| `ver_documento` (ficha completa + enlace) | ✅ | ✅ | ✅ | — |
| `listar_tipos_documento` | ✅ | ✅ | ✅ | — |
| `listar_palabras_clave` | ✅ | ✅ | ✅ | — |
| `estadisticas` | ✅ | ✅ | ✅ | — |
| `ir_a` (lleva a una pantalla) | ✅ | ✅ | ✅ | — |
| `buscar_empleado` | ❌ | ✅ | ✅ | rrhh |
| `expediente_empleado` | ❌ | ✅ | ✅ | rrhh |
| `buscar_documento_rrhh` | ❌ | ✅ | ✅ | rrhh |
| `documentos_por_vencer` | ❌ | ✅ | ✅ | rrhh |
| `mis_adjuntos` | ❌ | ❌ | ✅ | — |
| `proponer_actualizacion` | ❌ | ❌ | ✅ | — |
| `proponer_documento` | ❌ | ❌ | ✅ | — |
| `proponer_adjuntar_archivo` | ❌ | ❌ | ✅ | — |
| `proponer_palabras_clave` | ❌ | ❌ | ✅ | archivo |

**El modelo nunca escribe SQL ni recibe una conexión.** Solo llama a estas funciones, con los
argumentos que declara el esquema, y todo lo que llega de él se valida antes de tocar la
base.

`ir_a` no devuelve un enlace en el texto: devuelve una ruta de una **lista cerrada** que el
servidor valida, y el navegador decide si redirige.

## Escritura: el asistente propone, una persona ejecuta

Ninguna herramienta modifica el archivo. Las `proponer_*` crean una fila en `ia_propuestas`
que aparece en el chat como **[Aprobar] [Rechazar]**. El cambio ocurre solo si alguien
aprieta el botón, y entonces `core/ia_propuestas.py` lo ejecuta y lo registra en
`audit_log`.

No es exceso de celo. Un modelo que se equivoca al leer da una respuesta que el usuario
descarta; uno que se equivoca al escribir corrompe la ficha de un documento institucional y
nadie se entera hasta que alguien la busca. Además, la propuesta deja escrito qué se pidió,
quién lo aprobó y cuándo — exactamente lo que un archivo debe poder demostrar.

Tres detalles que sostienen esto:

- **La lista blanca de columnas se revalida al aprobar**, no solo al proponer. Entre crear la
  propuesta y aprobarla pasa tiempo y la fila es editable en la base; si la lista viviera
  solo en el momento de crear, bastaría tocar el JSON para escribir en cualquier columna. El
  SQL se arma con nombres de columna que salen de un `set` del código, **nunca del JSON**.
- **La propuesta guarda el estado anterior.** Quien aprueba ve "titulo: 'X' → 'Y'", no
  "titulo → Y". Un cambio sin su antes no se puede juzgar.
- **El prompt le prohíbe decir "listo".** Después de proponer, el modelo tiene instrucción
  explícita de no afirmar que ya está hecho — es el error que haría creer a un usuario que
  guardó algo que sigue pendiente.

## Archivos

El usuario sube con el clip del chat (`POST /api/ia/adjuntar` → R2, mismas validaciones que
un digitalizado: extensiones permitidas y 25 MB). Queda en `ia_adjuntos` **anclado a la
conversación**. Luego el asistente lo ve con `mis_adjuntos` y ofrece engancharlo a un
documento con `proponer_adjuntar_archivo`.

**El asistente no puede subir nada y no existe herramienta para hacerlo.** La subida siempre
la inicia una persona: un modelo que pudiera escribir en el bucket es un modelo que puede
llenarlo, y el bucket lo paga la Facultad.

El adjunto se busca *restringido a su conversación*. Si se buscara por id a secas, bastaría
adivinar un número para enganchar el archivo de otra persona.

## Costo — lo que de verdad mueve la aguja

1. **Caché de prompt.** El prompt de sistema (~2000 tokens) se reenvía entero en cada mensaje
   y no cambia nunca. Cacheado cuesta ~10%. Medido en el proyecto original: Sonnet **con**
   caché ($0.001378/mensaje) salía más barato que Haiku **sin** caché ($0.001771).
   Requisito: lo estable va primero y solo, en `ia.bloque_sistema()`. **Si metes la fecha de
   hoy en el prompt de sistema, el caché no sirve para nada.**
2. **Tope de vueltas de herramientas** (`MAX_VUELTAS = 5`). Cada vuelta es una llamada paga
   que reenvía toda la conversación. Sin tope, un modelo obstinado quema dinero sin fin.
3. **Tope de historial** (20/10). Sin él la conversación se paga entera en cada mensaje y el
   costo crece al cuadrado.
4. **Tope de gasto diario**, que corta **antes** de llamar al modelo. La idea es no hacer la
   llamada.

El panel muestra el **costo por mil mensajes**, no los $/millón: éstos engañan, porque en
este uso la salida pesa el grueso del costo y un modelo con caché le gana a otro con precio
de lista más bajo. También marca en rojo los modelos **sin soporte de herramientas**: ésos no
pueden consultar el archivo, solo conversar.

El costo no se estima — OpenRouter devuelve el real de cada llamada y se guarda en
`ia_mensajes.costo`.

### El modelo se cambia desde el panel, no desde el código

`/admin/ia` lista el catálogo real de OpenRouter (~350 modelos, cacheado 6 h) y basta un
clic para cambiarlo. **Se valida contra el catálogo antes de guardar**: un slug mal escrito
—el id nativo `claude-haiku-4-5` en vez del de OpenRouter `anthropic/claude-haiku-4.5`— se
guardaría sin quejarse y el chat empezaría a fallar en cada mensaje, con el error apareciendo
lejos de donde se cometió. Si el catálogo no se puede consultar se deja pasar: mejor confiar
en quien administra que bloquear un cambio legítimo porque OpenRouter esté caído.

Por defecto **`anthropic/claude-haiku-4.5`** ($1/$5 por millón, la mitad que Sonnet 5) — el
mismo default que Diamond, y por el mismo motivo: el grueso del tráfico son preguntas que se
resuelven con una búsqueda y un párrafo.

### Los topes viven en la base, no en el entorno

`tope_diario` y `max_tokens` se editan desde `/admin/ia`; las variables de entorno quedan
solo como respaldo. En Vercel, cambiar una variable obliga a redesplegar — y nadie hace un
deploy para subir el tope un día de mucho uso, así que en la práctica el asistente quedaría
cortado hasta el día siguiente. (En Diamond el mismo problema era peor: la config estaba
cacheada y había que parchearla por SSH.)

## Configuración

Variables de entorno (en Vercel, o en el `.env` local):

```
OPENROUTER_API_KEY=sk-or-v1-...     # única obligatoria
OPENROUTER_MODEL=anthropic/claude-haiku-4.5
IA_MAX_TOKENS=1500
IA_TOPE_DIARIO=1.00                 # dólares por día
IA_HABILITADA=true
```

**La clave vive solo en el servidor.** El navegador habla con `/api/ia/*` y es FastAPI quien
llama a OpenRouter. Si viajara al front, cualquiera la lee del bundle.

Si falta la clave, `/api/ia/disponible` devuelve `false`, la burbuja **no se muestra** y el
panel explica por qué. Nunca un 500 por configuración faltante.

**Ojo con los slugs:** vamos por OpenRouter, no por la API nativa. Los identificadores llevan
prefijo de proveedor y punto (`anthropic/claude-haiku-4.5`), no guiones ni sin prefijo
(`claude-haiku-4-5` da error de modelo inexistente). Si hay duda se consulta el catálogo en
el panel; no se adivina.

## Las tablas

`ia_conversaciones`, `ia_mensajes`, `ia_config`, `ia_propuestas` e `ia_adjuntos`, creadas
por `run_migrations()` en el arranque. Son nuevas: no alteran ni tocan una sola fila de lo
que ya existe.

**Guardar las conversaciones NO hace que el modelo aprenda solo.** No reentrena nada. Lo que
compra es que *nosotros* veamos dónde falla —sobre todo cuando contesta "no encontré nada"— y
con eso completemos la información institucional o ajustemos el prompt. El aprendizaje lo
hace una persona leyendo esto. Y sirve para lo que se siente enseguida: ver el gasto real y
**auditar qué herramienta llamó**, que es lo único que distingue una respuesta correcta de
una inventada.

## Control de conversaciones

Traído de Diary ("persistencia estilo Gemini"), con una regla de privacidad añadida.

Desde el chat, con sesión: el botón del reloj abre el historial, un clic **restaura** la
conversación completa —mensajes, traza de herramientas y propuestas que quedaran sin
resolver—, la papelera **borra** una sola, y el `+` **abre una nueva** dejando la anterior
guardada. Sin sesión no hay historial: las conversaciones del público no tienen dueño y no
se pueden retomar, a propósito.

**Tu conversación es tuya.** Solo un administrador Global ve las ajenas. Un admin de módulo
que pudiera abrir el hilo de un compañero convertiría el asistente en una herramienta de
vigilancia, y la gente dejaría de usarlo — que es la peor forma de perder la función. El
Global sí puede, porque responde por el gasto y por lo que el asistente contesta, y sin leer
los hilos donde falla no hay forma de mejorarlo.

A quien no debe ver una conversación se le devuelve **404, no 403**: tampoco se le confirma
que existe.

Borrar un hilo se lleva sus mensajes y adjuntos en cascada, pero **no borra los objetos de
R2**: pueden estar ya enganchados a un documento por una propuesta aprobada, y borrarlos
dejaría la ficha apuntando al vacío. Limpiar huérfanos es una tarea aparte, no un efecto
secundario de vaciar un chat.

### Esto son datos personales

Las conversaciones en modo `staff` pueden contener nombres y cédulas de empleados. Conviene
dejar una retención en un cron:

```sql
DELETE FROM public.ia_conversaciones WHERE ultima_at < NOW() - INTERVAL '6 months';
```

Los mensajes se van solos por el `ON DELETE CASCADE`. En el navegador el hilo vive en
`sessionStorage`, no en `localStorage`: al cerrar la pestaña se pierde, para que una consulta
sobre un expediente no quede en un equipo compartido de la Facultad.

## El cerebro editable

En `/admin/ia` se cargan la identidad, el tono, la **información institucional** (horarios,
ubicación de gavetas, a quién dirigirse) y reglas adicionales. Vive en la base y no en
variables de entorno porque cambiar cómo se presenta el asistente no debería requerir un
redespliegue ni un desarrollador.

> **Todo hueco en la información institucional es una invitación a rellenarlo.** En el
> proyecto original, la base decía que existían wifi y estacionamiento, no que fueran
> gratis — y el modelo completó el hueco con "both are included". La respuesta no es más
> prompt: es completar la información.

## Lo que queda pendiente

- **Búsqueda semántica (pgvector)** sobre los resúmenes: hoy la búsqueda es FTS + ILIKE, así
  que "documentos sobre presupuesto" no encuentra uno titulado "asignación de recursos".
- **Lectura del PDF digitalizado**: hoy el asistente sabe *qué* documento es y da el enlace,
  pero no lee su contenido.
- **Canal externo** (widget para la web de la Facultad, con clave pública + validación de
  origen exacto y `Origin` en lista cerrada). El diseño está probado en Diamond; aquí no se
  trajo porque todavía no hay a quién exponerlo.
