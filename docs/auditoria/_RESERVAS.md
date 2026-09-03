# Reservas de carril

**Se reserva ANTES de escribir la primera línea.** Si el carril que quieres ya tiene una
reserva `en curso` a nombre de otro, no lo cojas: coge el siguiente libre.

Tu nombre tiene que ser **único**. Si dos agentes van con el mismo nombre, la reserva de uno
le aparece al otro como propia y este fichero deja de proteger nada. Si no te han dado
nombre, pídelo antes de escribir aquí.

Al terminar, cambia tu estado a `terminado` y añade el sha del último commit.

| Carril | Agente | Desde | Estado | Commit |
|---|---|---|---|---|
| W0 | agente-w0-hemorragia | 2026-09-03 | terminado | c54c29c |
| W1 | agente-w1-redseguridad | 2026-09-03 | terminado (parcial: IN-001) | 3851a1e |
| W1b | agente-w1b-fixture-ci | 2026-09-03 | terminado | f2a5b10 |
| O1 (H1c-autorizacion) | agente-o1-autorizacion | 2026-09-03 | terminado | 9ff1fc1 |
| O2 (H1b-conexion) | agente-o2-transacciones | 2026-09-03 | terminado | defd015 |
| C1-docs-backend | agente-c1-docs | 2026-09-03 | terminado | 66c0b2b |
| C2-catalogo | agente-c2-catalogo | 2026-09-03 | terminado | 28bbff1 |
| C3-stats-backend | agente-c3-stats | 2026-09-03 | en curso | |
| C4-retencion | agente-c4-retencion | 2026-09-03 | terminado | 249dba7 (ver nota en _BUZON.md: commit compartido por una carrera de git concurrente con C2-catalogo, contenido correcto) |
| C6-usuarios-backend | agente-c6-usuarios | 2026-09-03 | en curso | |
| C7-papelera | agente-c7-papelera | 2026-09-03 | en curso | |
| A2-archivo-backend | agente-a2-archivo-backend | 2026-09-03 | terminado | 19e86f1 (ver nota en _BUZON.md: commit compartido por una carrera de git concurrente, contenido correcto) |
| A4-rrhh-backend | agente-a4-rrhh-backend | 2026-09-03 | terminado | d71096c (ver nota en _BUZON.md: commit compartido por una carrera de git concurrente, contenido correcto) |
| B4-admin-tabs | agente-b4-admin-tabs | 2026-09-03 | en curso | |
| B5-admin-monitor | agente-b5-admin-monitor | 2026-09-03 | en curso | |
| B6-admin-ui | agente-b6-admin-ui | 2026-09-03 | en curso | |
| B7-admin-submit | agente-b7-admin-submit | 2026-09-03 | en curso | |
| B8-admin-edit-archivo | agente-b8-admin-edit-archivo | 2026-09-03 | en curso | |
| B9-admin-edit-rrhh | agente-b9-admin-edit-rrhh | 2026-09-03 | en curso | |
| B10-admin-charts | agente-b10-admin-charts | 2026-09-03 | en curso | |
| B11-admin-categorias | agente-b11-admin-categorias | 2026-09-03 | en curso | |
| B12-admin-usuarios | agente-b12-admin-usuarios | 2026-09-03 | en curso | |
| D1-ia-backend | agente-d1-ia-backend | 2026-09-03 | en curso | |
| D2-ia-frontend | agente-d2-ia-frontend | 2026-09-03 | en curso | |
| C9-auth | agente-c9-auth | 2026-09-03 | en curso | |
| C10-ficheros-r2 | agente-c10-ficheros-r2 | 2026-09-03 | en curso | |
| F1-paginas-estaticas | agente-f1-paginas | 2026-09-03 | en curso | |
| F2-cascara | agente-f2-cascara | 2026-09-03 | en curso | |
| E2-escaner-cliente | agente-e2-escaner-cliente | 2026-09-03 | en curso | |

## Cómo se escribe una fila

```
| A3-buscador-rrhh | bruno | 2026-09-02 14:20 | en curso | |
| A3-buscador-rrhh | bruno | 2026-09-02 14:20 | terminado | a1b2c3d |
```

## Carriles que no se cogen sin permiso

- `W0`, `W1`, `H1c`, `H1b` — van primero y solos; el resto depende de ellos.
- `LX` — va el último de todos: parte `styles.css` en módulos e invalida los rangos de línea
  con los que trabajan los demás lotes de diseño.
- `L0` y `BR-109` — van antes que cualquier otro trabajo estético.
