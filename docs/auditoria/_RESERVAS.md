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
| O1 (H1c-autorizacion) | agente-o1-autorizacion | 2026-09-03 | terminado | 82b03a4 |
| O2 (H1b-conexion) | agente-o2-transacciones | 2026-09-03 | en curso | |

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
