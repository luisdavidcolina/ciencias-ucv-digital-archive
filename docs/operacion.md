# Operación: rotación de secretos

Procedimiento para IN-179. No hay hoy ningún procedimiento escrito de rotación
de secretos: este documento lo cubre para los cinco secretos reales del
sistema. Los nombres de variable y sus comentarios vienen de `.env.example`;
dónde se leen, de `app/core/config.py`, `app/database.py`, `app/storage.py` y
`app/core/ai.py`.

Ninguna credencial se escribe aquí ni en ningún otro fichero del repositorio
(`app/tests/test_secrets.py` lo vigila). Este documento describe **dónde** se
cambia cada una y **qué se rompe** al hacerlo — no valores.

## Antes de rotar cualquier secreto

1. Confirmar en qué entorno se está rotando (Vercel tiene variables por
   proyecto; si hay entornos de *Preview* además de *Production*, la variable
   se actualiza en los dos o el *Preview* queda con el valor viejo).
2. Avisar con antelación si la rotación va a cerrar sesiones activas
   (`SECRET_KEY`) — ver más abajo.
3. Cambiar la variable en Vercel **no** la aplica sola: Vercel exige un nuevo
   despliegue (o un *redeploy* del último) para que la función serverless la
   lea. Hasta ese redeploy, la instancia en producción sigue usando el valor
   anterior.

---

## `SECRET_KEY`

**Dónde se cambia**: Vercel → panel del proyecto → variables de entorno →
`SECRET_KEY`, seguido de un redeploy.

**Para qué se usa**: firma HMAC de dos cosas distintas con la misma clave
(`app/core/security.py`, ver IN-145): las sesiones de usuario y los tokens de
`/compartido/<token>` (`share.py`). Ninguna de las dos tiene tabla propia —
el token lleva su propio contenido firmado, no hay estado que borrar en base
de datos.

**Efecto colateral de rotarla**: cambia la clave con la que se verifican
firmas ya emitidas, así que **todo lo firmado con la clave anterior deja de
validar en el mismo instante**:

- Todas las sesiones de usuario activas se cierran — cada persona tiene que
  volver a iniciar sesión.
- Todos los enlaces `/compartido/<token>` ya repartidos dejan de funcionar,
  aunque no hubieran caducado. `test_share.py` documenta explícitamente que
  un enlace "no sobrevive a un cambio de `SECRET_KEY`" — es el comportamiento
  esperado, no un bug, pero hay que avisar a quien haya recibido un enlace
  antes de rotar.

**Cuándo rotarla**: si se sospecha que la clave se filtró (por ejemplo,
quedó en un commit o en un log). Es el secreto de rotación más disruptiva de
los cinco porque no hay forma de rotarlo sin ese efecto — no hay mecanismo de
gracia (dos claves válidas a la vez) implementado hoy.

**Orden recomendado para minimizar la interrupción**:
1. Elegir una ventana de baja actividad (fuera de horario de consulta).
2. Actualizar la variable en Vercel y redesplegar.
3. Avisar a los usuarios de que deben volver a iniciar sesión.
4. Volver a emitir cualquier enlace `/compartido/<token>` que siga
   necesitándose (se genera de nuevo desde el documento, no se puede
   "revalidar" el viejo).

## `CRON_SECRET`

**Dónde se cambia**: dos sitios, y tienen que quedar sincronizados:
1. Vercel → variables de entorno → `CRON_SECRET` (+ redeploy).
2. La propia configuración del *Cron Job* de Vercel que llama a
   `/api/admin/backup/programado` (declarado en `vercel.json`) — Vercel Cron
   envía el valor vigente de la variable de entorno en
   `Authorization: Bearer <CRON_SECRET>` en cada invocación, así que en la
   práctica basta con el paso 1 seguido del redeploy: no hay un segundo valor
   guardado aparte que haya que editar a mano en el panel de Cron Jobs. Aun
   así, conviene revisar en el panel de Vercel → Cron Jobs que la próxima
   ejecución programada quede después del redeploy, no antes.

**Para qué se usa**: autentica únicamente la llamada del cron al backup
programado (`routes/backup.py`, ruta sin `require_session` porque no la llama
un navegador). El endpoint **falla cerrado**: sin `CRON_SECRET` definido
responde 503.

**Efecto colateral de rotarla**: ninguna sesión de usuario se ve afectada.
El único efecto es una ventana entre "se cambió la variable en Vercel" y "se
completó el redeploy" en la que, si el cron dispara justo en ese momento, la
invocación puede traer el `Bearer` viejo contra el código nuevo (o viceversa)
y el backup de ese día falla con 503. Como el intento fallido queda igual
registrado en `backup_history` (ver `CLAUDE.md`, sección "Backup
programado"), el efecto es detectable, no silencioso.

**Cuándo rotarla**: si se sospecha filtración, o de forma periódica junto con
el resto. Es el secreto de menor impacto en el usuario final de los tres que
tienen efecto colateral, pero exige coordinar el momento con el horario del
cron (declarado en `vercel.json`: 07:10 UTC = 03:10 en Venezuela).

**Orden recomendado**:
1. Generar el nuevo valor y actualizarlo en Vercel.
2. Redesplegar.
3. Confirmar en `backup_history` que la siguiente ejecución del cron (al día
   siguiente) aparece como éxito.
4. Evitar rotar en la hora inmediatamente anterior a las 03:10 hora de
   Venezuela, para no arriesgar una ejecución a medio redeploy.

## Credenciales de Cloudflare R2 (`R2_ENDPOINT`, `R2_ACCESS_KEY`, `R2_SECRET_KEY`, `R2_BUCKET`)

**Dónde se cambia**: dos paneles, en este orden:
1. Panel de Cloudflare → R2 → gestión de tokens de API: revocar el token
   actual y emitir uno nuevo (`R2_ACCESS_KEY`/`R2_SECRET_KEY` son el par de
   ese token). `R2_ENDPOINT` y `R2_BUCKET` normalmente no cambian al rotar un
   token — solo cambian si además se mueve de cuenta o de bucket.
2. Vercel → variables de entorno → las cuatro variables `R2_*` (+ redeploy).

**Advertencia de esta auditoría (IN-002)**: hoy estas cuatro credenciales
están escritas como literales en `app/storage.py`, en el historial de git.
Eso significa que **rotar la variable de entorno en Vercel no basta**: hay
que revocar el token viejo en Cloudflare (paso 1 de arriba) para que las
credenciales expuestas en el historial dejen de servir. Cambiar solo la
variable de entorno sin revocar el token deja el token viejo — el que está
en git — perfectamente válido para cualquiera que clone el repositorio.
Sacar la lectura de estas variables del código fuente hacia
`os.environ.get` es trabajo de IN-002, no de este documento.

**Para qué se usa**: `app/storage.py` — subida, descarga, borrado y URLs
prefirmadas de todos los ficheros digitalizados (Archivo y RRHH) y del backup
programado, que vive en el mismo bucket.

**Efecto colateral de rotarla**: ninguna sesión de usuario se cierra. El
efecto es que, entre el momento en que se revoca el token viejo en Cloudflare
y el momento en que termina el redeploy con el token nuevo en Vercel, **toda
subida, descarga o borrado de archivos falla** (`is_configured()` en
`storage.py` decide si el almacenamiento está disponible; con credenciales
inválidas las llamadas a R2 fallarán, no que el sistema deje de "ver"
R2 como configurado). La búsqueda y consulta de metadatos no dependen de R2
y siguen funcionando con normalidad durante esa ventana.

**Orden recomendado para minimizar la interrupción**:
1. Emitir el token nuevo en Cloudflare **sin revocar todavía el viejo** (si
   el plan lo permite tener dos tokens activos a la vez).
2. Actualizar las cuatro variables en Vercel con los valores del token nuevo
   y redesplegar.
3. Confirmar que una subida y una descarga de archivo funcionan en
   producción.
4. Solo entonces revocar el token viejo en Cloudflare — así se evita la
   ventana de fallo de archivos, al costo de que el token viejo (el que está
   en el historial de git) siga siendo válido unos minutos más.
5. Si el token viejo no puede coexistir con uno nuevo (algunos planes limitan
   tokens activos), aceptar la ventana de fallo de archivos y hacerlo en
   horario de baja actividad.

## `OPENROUTER_API_KEY`

**Dónde se cambia**: panel de OpenRouter (gestión de claves de API) para
revocar/emitir, y Vercel → variables de entorno → `OPENROUTER_API_KEY`
(+ redeploy).

**Para qué se usa**: `app/core/ai.py`, cliente del asistente de IA. La clave
vive solo del lado del servidor — el navegador nunca la ve, habla con
`/api/ia/*` (ver `.env.example`).

**Efecto colateral de rotarla**: ninguna sesión ni enlace compartido se ve
afectado. Según `.env.example`: "si falta, la burbuja de chat simplemente no
aparece" — así que en la ventana entre revocar la clave vieja y completar el
redeploy con la nueva, el asistente de IA deja de responder (o desaparece la
burbuja si `OPENROUTER_API_KEY` queda vacía), pero el resto del sistema
(búsqueda, RRHH, Archivo, backup) no se entera.

**Orden recomendado**:
1. Emitir la clave nueva en OpenRouter sin revocar la vieja todavía.
2. Actualizar la variable en Vercel y redesplegar.
3. Confirmar que el asistente responde en producción.
4. Revocar la clave vieja en OpenRouter.

## `DATABASE_URL`

**Dónde se cambia**: panel de Neon → el proyecto → cadena de conexión
(*Connection string*), y Vercel → variables de entorno → `DATABASE_URL`
(+ redeploy). Neon permite regenerar la contraseña del rol de conexión sin
crear una rama nueva ni perder datos.

**Para qué se usa**: única fuente de conexión a PostgreSQL, leída una sola
vez en `app/core/config.py` (IN-033: `database.py` la toma de ahí, no la
relee por su cuenta).

**Efecto colateral de rotarla**: es el secreto de mayor impacto si algo sale
mal en el orden de los pasos, porque **todo** depende de la base de datos:
sesiones, búsqueda, panel admin, backup. A diferencia de `SECRET_KEY`, las
sesiones y enlaces compartidos ya emitidos siguen siendo válidos (son HMAC
locales, no dependen de una tabla) — lo que se rompe es cualquier petición
que necesite consultar o escribir en Neon mientras la variable en Vercel no
apunte todavía a una cadena válida.

**Orden recomendado para minimizar la interrupción**:
1. En Neon, generar la contraseña nueva del rol (sin borrar todavía el
   acceso con la contraseña vieja, si el panel lo permite).
2. Actualizar `DATABASE_URL` en Vercel con la cadena nueva y redesplegar.
3. Verificar `/api/health` (o el endpoint de salud vigente) contra
   producción antes de dar la rotación por terminada.
4. Solo entonces invalidar la contraseña vieja en Neon.

---

## Orden recomendado cuando hay que rotar varios secretos a la vez

Si la rotación es preventiva (no una filtración activa), rotar en este orden
minimiza el número de sesiones cerradas y de ventanas de fallo simultáneas:

1. `OPENROUTER_API_KEY` y credenciales de R2 (bajo impacto, revocación
   diferida posible).
2. `CRON_SECRET` (bajo impacto, solo requiere cuidar el horario del cron).
3. `DATABASE_URL` (impacto alto si falla el orden, pero no cierra sesiones).
4. `SECRET_KEY` al final, porque es la única que cierra sesiones y enlaces
   compartidos de forma inevitable — así solo se avisa una vez a los
   usuarios en toda la operación, no una vez por cada secreto rotado.

Si la rotación es reactiva (una credencial concreta se filtró), esa
credencial se rota primero, sin esperar a las demás.
