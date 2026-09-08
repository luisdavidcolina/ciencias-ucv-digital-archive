"""Consultas de app/routes/share.py, movidas tal cual (IN-042 paso 3).

Sin cambios de SQL ni de comportamiento respecto al original: solo se
mueve el cuerpo de las funciones que hablaban con la base de datos fuera
del modulo de rutas.
"""
from database import db_query

TABLAS = {
    "Archivo": ("datos_archivo", "id_archivo"),
    "RRHH": ("datos_rrhh", "id_rrhh"),
}


def leer_documento(modulo: str, doc_id: int) -> dict | None:
    tabla, pk = TABLAS[modulo]
    return db_query(
        f"""SELECT d.{pk} AS id, d.titulo, d.autor, d.file_url, d.ubicacion,
                   TO_CHAR(d.fecha_documento, 'YYYY-MM-DD') AS fecha,
                   COALESCE(td.nombre_corto, td.nombre, '') AS tipo,
                   COALESCE(d.soporte, 'Físico')            AS soporte
            FROM public.{tabla} d
            LEFT JOIN public.tipo_documento td ON d.id_tipo_documento = td.id
            WHERE d.{pk} = %s AND d.deleted_at IS NULL
              AND COALESCE(d.status, 'aprobado') = 'aprobado'""",
        [doc_id], fetch="one",
    )


def listar_revocados() -> list[dict]:
    return db_query(
        "SELECT jti, motivo, creado_en FROM public.enlaces_revocados "
        "ORDER BY creado_en DESC",
        fetch="all",
    )


def insertar_revocado(jti: str, motivo: str | None) -> None:
    db_query(
        "INSERT INTO public.enlaces_revocados (jti, motivo) VALUES (%s, %s) "
        "ON CONFLICT (jti) DO NOTHING",
        [jti, motivo or None], fetch="none", commit=True,
    )
