# Nota W1/W1b: qué falta para pruebas de integración contra base real

La suite actual (`app/tests/`) mockea `db_query` en cada test — nunca abre una conexión de
verdad a Postgres/Neon. Eso es correcto para pruebas unitarias rápidas (la suite entera corre
en ~20 s) y ya cubre bien la lógica de cada endpoint aislada de la base. Lo que no cubre, y que
ningún mock puede cubrir, es si el SQL real que se manda contra un Postgres real hace lo que el
mock asume que hace: columnas que ya no existen, tipos que no castean como se espera, un JOIN
que trae menos filas de las que el test cree, una migración que no corrió, una constraint que
rechaza el INSERT. Esa es la clase de fallo que esta ola de auditoría (SI-226 y las fichas
BR/OR que documenta) no puede detectar con los fixtures actuales, por diseño.

## Qué se necesitaría

**1. Una base real donde correr contra un esquema de verdad.** Dos opciones razonables:

- **Postgres vía Docker** (`docker-compose` con `postgres:16` o la versión que use Neon,
  arrancado en CI antes de `pytest`). Rápido, gratis, aislado por corrida, pero exige mantener
  a mano el esquema (o correr las migraciones del proyecto contra ese contenedor antes de cada
  suite) para que no se desincronice del real.
- **Un branch de prueba de Neon**, creado por CI al vuelo (Neon tiene API/CLI para crear y
  borrar branches de un proyecto en segundos, heredando el esquema y opcionalmente los datos
  del branch padre) y destruido al terminar. Más fiel al motor real (Neon tiene particularidades
  de pooling/serverless que un Postgres de Docker no reproduce), pero necesita credenciales de
  Neon en el runner de CI — y esas credenciales, por regla del repo, no pueden vivir en git; irían
  como secret de GitHub Actions.

Para esta primera ola, Docker es lo más simple de arrancar sin tocar cuentas ni secretos; el
branch de Neon es la opción a evaluar si Docker diverge demasiado del esquema real con el tiempo.

**2. Fixtures de datos.** Los mocks actuales (`_archivo_row`, `_rrhh_view_row`, `_user_row` en
`conftest.py`) ya definen la forma de una fila típica de cada tabla. Para integración real
harían falta *seeds* SQL (o factories con `psycopg2`/`INSERT`) que dejen la base de prueba en un
estado conocido antes de cada test, y limpieza (transacción que se revierte, o `TRUNCATE`) entre
tests para que no se contaminen entre sí. Lo más barato: envolver cada test de integración en una
transacción que nunca hace commit.

**3. Cómo encajaría con el conftest actual.** No haría falta tocar los fixtures existentes
(`client`, `anon_client`, `client_as` de SI-226 siguen sirviendo para las pruebas unitarias
rápidas). Se añadiría un fixture nuevo y **separado**, algo como `db_conn`/`real_client`, marcado
con un marker de pytest propio (`@pytest.mark.integration`) para poder excluirlo del run rápido
por defecto (`pytest -m "not integration"`) y correrlo aparte (posiblemente en un job de CI
distinto, más lento, que solo se dispara en según qué condición — por ejemplo solo en `main` o
solo bajo demanda, no en cada push, para no encarecer ni ralentizar cada PR). Ese fixture
sustituiría el mock de `db_query` por una conexión real a la base de prueba (Docker o branch de
Neon) y dejaría que el código de producción hable con Postgres de verdad.

## Por qué no se hace en esta ola

Es un cambio de infraestructura (levantar Postgres en CI o gestionar branches de Neon
efímeros, credenciales, limpieza de datos entre tests), no un cambio de fixture como SI-226.
Mezclarlo aquí habría ampliado mucho el alcance de W1b. Queda para que lo retome el carril que
corresponda cuando la ola de auditoría llegue a esa capa.
