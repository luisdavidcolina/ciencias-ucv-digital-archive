# Auditoría completa — 1.863 pendientes

Barrido a fondo de las cuatro superficies del sistema (los dos buscadores y los dos
backoffices), más el Admin Global con la consola de IA, el sistema de diseño y la
ingeniería del backend. **Diez auditorías**, ninguna implementación: esto es el mapa,
no el trabajo.

Las siete primeras leyeron código. Las tres últimas cerraron los huecos que dejaron:
lo que se prometió al cliente y nadie había contrastado, la digitalización —que es la
función central de un archivo y no existe— y **mirar la pantalla**, que resultó ser el
carril más productivo por hallazgo.

| Carril | Prefijo | Nº | Archivo |
|---|---|---|---|
| Buscador Archivo | `BA` | 172 | [buscador-archivo.md](buscador-archivo.md) |
| Buscador RRHH | `BR` | 180 | [buscador-rrhh.md](buscador-rrhh.md) |
| Backoffice Archivo | `OA` | 212 | [backoffice-archivo.md](backoffice-archivo.md) |
| Backoffice RRHH | `OR` | 300 | [backoffice-rrhh.md](backoffice-rrhh.md) |
| Sistema · IA · páginas | `SI` | 240 | [sistema-ia-paginas.md](sistema-ia-paginas.md) |
| Sistema de diseño | `SD` | 236 | [sistema-diseno.md](sistema-diseno.md) |
| Ingeniería | `IN` | 216 | [ingenieria.md](ingenieria.md) |
| Digitalización y escáner | `DG` | 172 | [digitalizacion-escaner.md](digitalizacion-escaner.md) |
| Requisitos vs implementado | `RQ` | 55 | [requisitos-vs-implementado.md](requisitos-vs-implementado.md) |
| Recorrido visual (241 capturas) | `VI` | 80 | [recorrido-visual.md](recorrido-visual.md) |

Cada pendiente lleva archivo:línea, el escenario concreto de fallo, qué debe pasar,
esfuerzo S/M/L y **la lista de archivos que tocaría**. 1.343 van marcados `[CHOCA]`
porque tocan ficheros compartidos: esa marca es lo que hace repartible el trabajo.

---

## Lo que el barrido descubrió, y que no estaba en la lista de nadie

Las 421 pruebas pasan. Eso no significa lo que parecía significar.

**1. La autorización no existe en el backend.** `require_session` devuelve un nombre y
ni un solo endpoint comprueba rol ni módulo con él. La separación Archivo/RRHH vive
en `app.js`, es decir, en el navegador del usuario. Un usuario Normal de Archivo se
descarga la base entera con los hashes bcrypt (`SI-002`, `IN-129`), edita expedientes
de personal (`OR-043`) y el fichero de personal se lee sin sesión ninguna (`BR-001`).
`IN-131` lo generaliza: es una sola ausencia, no cuarenta fallos.

**2. La suite no puede ver el punto 1.** El fixture `client` sobrescribe
`require_session` globalmente (`SI-226`), así que ninguna prueba puede detectar una
autorización ausente. Y los mocks de `db_query` impiden que el SQL se ejecute nunca,
que es como las importaciones de RRHH llevan desde siempre insertando cero filas
mientras la pantalla dice «Importación completada» (`OR-002`, `OR-003`).

**3. La documentación describe arreglos que no están en el árbol.** `CLAUDE.md`
afirma dos veces que `test_secrets.py` impide credenciales en el código; el fichero
no existe. El `CHANGELOG` de la v3.3.0 da por corregida la fuga de R2; las claves
siguen en `storage.py:23-26` y en el historial de un repositorio público.

**4. Las migraciones no corren nunca en producción.** `api/index.py` monta
`Mangum(app, lifespan="off")` y todo el arranque vive en el `lifespan`. Toda la
estrategia de huella SHA-256 que documenta `CLAUDE.md` es código muerto en Vercel
(`IN-001`).

**5. Hay funcionalidad que probablemente nunca funcionó, y nadie la reportó porque la
pantalla dice que fue bien.** Ninguna faceta del buscador responde a un clic
(`BA-001`). «Ver Expediente» da 404 en todas las filas (`OR-001`). El arrastrar y
soltar del alta está muerto (`OR-005`). La pestaña Retención de RRHH abre siempre
vacía (`OA-001`). Y el caso mayor, que sólo apareció al renderizar: **el Admin Global
entero está en blanco** (`VI-001`) — `app.js:177` oculta su sección y el `switch` que
la devolvería no conoce su identificador. Copias, auditoría, alertas y retención no se
ven, sin un solo error en consola, mientras `test_admin_panels.py` pasa en verde.

**6. La memoria técnica especifica otro sistema.** Trece capítulos describen R/Shiny
sobre un VPS con PostgreSQL local, con un capítulo dedicado a justificar por qué *no*
Python. Lo construido es Python + FastAPI + Vercel + Neon + R2, y el cambio no está
registrado en ninguna decisión (`RQ`, sección de fantasmas). De 86 promesas
contrastadas: 21 cumplidas, 24 parciales, 18 ausentes, **12 fantasmas**, 11 con deriva.

**7. La digitalización no existe.** `scanner-app/` son 424 líneas de puente WebSocket
para códigos de barras, y sin `HID_VID`/`HID_PID` escoge «el primer teclado»: emite por
red lo que el usuario teclea, sin autenticar y escuchando en `0.0.0.0` (`DG-029`). El
contenido de los PDF nunca se extrae, así que **la búsqueda de texto completo no puede
ver ningún documento** (`DG-104`).

---

## Orden de ataque

No es por gravedad: es por dependencia. Cada bloque necesita que el anterior esté hecho.

### Bloque 0 — Parar la hemorragia (hoy, a mano)
- **Rotar el token de R2 en Cloudflare.** El secreto está expuesto en un repositorio
  público; sacarlo del código no lo revoca. Esto no lo puede hacer un agente.
- `IN-002` mover las credenciales al entorno · escribir el `test_secrets.py` que
  `CLAUDE.md` da por existente.
- `SI-156` dejar de servir públicamente el informe interno de `/investigacion`.

### Bloque 1 — La red de seguridad (sin esto, nada más se puede tocar con confianza)
`SI-226` arreglar el fixture que anula la autorización · pruebas contra base real en
vez de mocks · `IN-001` que las migraciones corran en Vercel · integración continua.
**Un solo agente. Es la base de todo lo demás y toca ficheros que todos usan.**

### Bloque 2 — Autorización y transacciones (las mismas rutas, a la vez)
`IN-131` autorización real por rol y módulo en el backend · `IN-164` unidades
atómicas · `OR-004` la importación que borra nombres · `OR-010`/`OR-011`/`OR-012`
los borrados que dejan huérfanos o destruyen de más.
**Pocos agentes, coordinados: tocan `deps.py` y casi todas las rutas.**

### Bloque 3 — Fallos visibles, por pantalla
`BA-001`, `BA-002`, `BA-003`, `OA-001`…`OA-003`, `OR-001`, `BR-003`, `BR-004`…
**Muy paralelizable: cada carril tiene sus ficheros.**

### Bloque 4 — Estética y sistema de diseño
Los 22 lotes de `sistema-diseno.md`, en su orden: `L0` primero (sólo añade tokens al
principio del fichero), `L1`–`L15` por rangos de líneas de `styles.css`, `LX` el
último porque invalida todos los rangos. Antes de tocar RRHH hay que hacer `BR-109`:
el dossier se pinta con ~100 `style` en línea y no hay dónde enganchar nada.
**Aquí es donde caben veinte agentes a la vez.**

### Bloque 4b — Lo que se ve roto en pantalla
`VI-001` el Admin Global en blanco · `VI-002` los avisos que no avisan · `VI-020`/`VI-021`
el pasillo invisible del teclado · el modo oscuro a medias · un solo color de acción.
Van con el Bloque 4 y se apoyan en las 241 capturas de `capturas/`.

### Bloque 5 — Lo que falta para ser el mejor del mundo
Cuadro de clasificación jerárquico y descripción multinivel ISAD(G) en Archivo;
antigüedad calculada, escalafón, dedicación, vacaciones, permisos, concursos,
evaluación y checklist de completitud del expediente en RRHH; firma, sello y
verificación en los reportes, sin lo cual no sirven para trámite oficial.

### Bloque 6 — Digitalización
Cerrar hoy el registrador de pulsaciones y la escucha en `0.0.0.0` (`DG-029`, `DG-003`).
Después, la decisión de empaquetado portable —Electron con `electron-builder`,
argumentada en `digitalizacion-escaner.md`— y la captura por móvil, que es la única vía
que entrega digitalización real sin comprar ni instalar nada en la Facultad.

---

## Lo que no puede hacer un agente

Estas decisiones bloquean trabajo y son del dueño:

1. **Rotar el token de R2 en Cloudflare.** Expuesto en un repositorio público.
2. **Decidir si los datos de personal pueden seguir fuera de Venezuela.** El requisito
   de soberanía se incumple por construcción y nadie lo ha declarado.
3. **Decidir con el archivista si la unidad de descripción es el folio o el expediente.**
4. **Fijar el nombre del producto** (el cliente ya lo decidió; nadie lo aplicó).
5. **Inventariar qué escáneres hay realmente en la Facultad.** Sin eso, elegir entre
   TWAIN, SANE y captura por móvil es adivinar.
6. **Aprobar el gasto de firma de código** si se va a Electron: certificado de Windows
   y cuenta de Apple con notarización.
