# Buzón — lo que hace falta tocar y no es de mi carril

Si un pendiente necesita escribir en un archivo que **no es de tu carril**, no lo toques.
Anótalo aquí y sigue con el siguiente. El dueño de ese archivo lo aplicará en tanda.

Esto no es una cola de deseos: es el mecanismo que evita que veinte agentes se pisen. Un
apunte aquí vale más que un conflicto de fusión en `main.py`.

## Cómo se anota

```
- [ ] `IN-057` · **archivo**: `app/main.py` · **carril dueño**: H1a-migraciones
      **quién lo pide**: bruno (A3-buscador-rrhh)
      **qué hace falta**: alinear la expresión del índice GIN con la de la consulta de BA-002.
```

## Pendientes anotados

- [ ] **mojibake en dos documentos de auditoría** · **archivos**: `docs/auditoria/backoffice-rrhh.md`,
      `docs/auditoria/buscador-archivo.md` · **carril dueño**: ninguno (son documentos, no código)
      **quién lo pide**: revisión de W0/W1, 2026-09-03
      **qué hace falta**: `test_static_assets.py::test_sin_mojibake` falla en estos dos archivos
      (2 secuencias en cada uno, ej. 'é'/'á' doble-codificados). No es una regresión de W0/W1 — ya
      estaba en el commit de las auditorías. Se repara re-codificando esas líneas a cp1252 y
      decodificando como UTF-8, igual que documenta la propia guarda.

## Contexto: los archivos más disputados

Estos son los que más pendientes de otros carriles necesitan tocar. Ninguno se toca fuera de
su carril dueño:

| Archivo | Pendientes de otros carriles | Carril dueño |
|---|---|---|
| `app/main.py` | 81 | `H1a-migraciones` |
| `app/static/styles.css` | 68 | `G1-estilos` (por lotes) |
| `app/schema.sql` | 9 | `H1a-migraciones` |
| `app/models.py` | 7 | `H1d-modelos` |
| `app/static/app.js` | 6 | `H2-app-js` |
| `app/routes/admin/deps.py` | 2 | `H1c-autorizacion` |
| `app/database.py` | 2 | `H1b-conexion` |
