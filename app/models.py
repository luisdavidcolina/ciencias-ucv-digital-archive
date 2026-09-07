import re
from datetime import date
from typing import Annotated, List, Optional
from pydantic import BaseModel, Field, field_validator, model_validator

_SAFE_URL_RE = re.compile(r'^(/|https?://)', re.IGNORECASE)
_DATE_RE     = re.compile(r'^\d{4}-\d{2}-\d{2}$')
_CEDULA_RE   = re.compile(r'^[VEJPGvejpg]?-?\d{6,9}$')

# Aliases de longitud comunes
Str255  = Annotated[str, Field(max_length=255)]
Str500  = Annotated[str, Field(max_length=500)]
Str4000 = Annotated[str, Field(max_length=4000)]

def _validate_file_url(v):
    """Rechaza javascript:, data:, y otros esquemas peligrosos.

    IN-149: `files.py:146` ya rechaza `".." in key` al servir un archivo, pero
    esa misma comprobación no existía aquí — un `file_url` con recorrido de
    ruta (`/api/files/../../otra-cosa`) pasaba esta validación con sólo
    empezar por `/`, y quedaba guardado en el documento a la espera de que
    algún visor lo siguiera."""
    if v and not _SAFE_URL_RE.match(v):
        raise ValueError("file_url debe comenzar con / o https://")
    if v and ".." in v:
        raise ValueError("file_url no puede contener '..'")
    return v or None

def _validate_date(v: str | None, field_name: str = "fecha") -> str | None:
    if v and not _DATE_RE.match(v):
        raise ValueError(f"{field_name} debe tener formato YYYY-MM-DD")
    return v or None

def _normalize_cedula(v):
    """OR-016/OR-084: 'V-12345678', 'v12345678', '12.345.678' y '12345678'
    son la misma persona. Normaliza a '<letra>-<dígitos>' (letra por defecto V)
    y rechaza lo que no tenga forma de cédula, para que dejen de convivir
    como registros distintos bajo el UNIQUE de `cedula`."""
    if not v:
        return v or None
    raw = v.strip().upper().replace(".", "").replace(" ", "")
    if not _CEDULA_RE.match(raw):
        raise ValueError("cedula debe tener el formato V-12345678 (letra opcional, 6 a 9 dígitos)")
    digits = raw.lstrip("VEJPG-")
    letra = raw[0] if raw[0] in "VEJPG" else "V"
    return f"{letra}-{digits}"

def _parse_iso_date(v):
    if not v:
        return None
    try:
        return date.fromisoformat(v)
    except ValueError:
        return None


# =============================================================================
# AUTENTICACIÓN
# =============================================================================

class LoginRequest(BaseModel):
    username: Annotated[str, Field(max_length=100)]
    password: Annotated[str, Field(max_length=200)]


class RestoreSessionRequest(BaseModel):
    username: Annotated[str, Field(max_length=100)]


# =============================================================================
# BÚSQUEDA
# =============================================================================

class ArchivoSearchRequest(BaseModel):
    search_term: Annotated[Optional[str], Field(max_length=500)] = ""
    doc_types:   Optional[List[Str255]] = []
    tesauro_terms: Optional[List[Str255]] = []
    date_start:  Optional[str] = ""
    date_end:    Optional[str] = ""
    sort_mode:   Optional[str] = "Alfabético (A-Z)"
    soporte:     Optional[str] = ""
    page:        int = 1
    per_page:    int = 10


class RrhhSearchRequest(BaseModel):
    search_term:  Annotated[Optional[str], Field(max_length=500)] = ""
    doc_types:    Optional[List[Str255]] = []
    people_terms: Optional[List[Str255]] = []
    estados:      Optional[List[Str255]] = []
    date_start:   Optional[str] = ""
    date_end:     Optional[str] = ""
    sort_mode:    Optional[str] = "Alfabético (A-Z)"
    page:         int = 1
    per_page:     int = 10


class RrhhProfileRequest(BaseModel):
    persona: Annotated[str, Field(max_length=300)]


# =============================================================================
# ADMINISTRACIÓN
# =============================================================================

_SOPORTES_VALIDOS = ("Físico", "Digital", "Digitalizado")

class DocumentSubmitRequest(BaseModel):
    modulo:               str
    usuario:              Annotated[str, Field(max_length=100)]
    titulo:               Annotated[Optional[str], Field(max_length=500)]  = ""
    autor:                Annotated[Optional[str], Field(max_length=255)]  = ""
    resumen:              Annotated[Optional[str], Field(max_length=4000)] = ""
    empleado:             Annotated[Optional[str], Field(max_length=300)]  = ""
    nombres:              Annotated[Optional[str], Field(max_length=200)]  = ""
    apellidos:            Annotated[Optional[str], Field(max_length=200)]  = ""
    cedula:               Annotated[Optional[str], Field(max_length=20)]   = ""
    personas_relacionadas:Annotated[Optional[str], Field(max_length=1000)] = ""
    departamento:         Annotated[Optional[str], Field(max_length=200)]  = ""
    estado:               Annotated[Optional[str], Field(max_length=100)]  = ""
    fecha_jubilacion:     Optional[str] = ""
    fecha_pension:        Optional[str] = ""
    foto_url:             Annotated[Optional[str], Field(max_length=2048)] = ""
    rif:                  Annotated[Optional[str], Field(max_length=20)]   = ""
    cargo:                Annotated[Optional[str], Field(max_length=200)]  = ""
    doc_type:             Annotated[str, Field(max_length=200)]
    fecha:                str
    ubicacion:            Annotated[str, Field(max_length=500)]
    tesauro_secundario:   Annotated[Optional[str], Field(max_length=200)]  = ""
    descriptores_libres:  Annotated[Optional[str], Field(max_length=2000)] = ""
    status:               Optional[str] = "aprobado"
    notas:                Annotated[Optional[str], Field(max_length=4000)] = ""
    numero_folio:         Annotated[Optional[str], Field(max_length=100)]  = None
    soporte:              Annotated[Optional[str], Field(max_length=50)]   = "Físico"
    numero_paginas:       Optional[int] = None
    file_url:             Annotated[Optional[str], Field(max_length=2048)] = None
    fecha_vencimiento:    Optional[str] = None
    fecha_nacimiento:     Optional[str] = None
    nivel_educativo:      Annotated[Optional[str], Field(max_length=100)]  = None
    sexo:                 Optional[str] = None

    @field_validator("modulo")
    @classmethod
    def modulo_must_be_valid(cls, v):
        if v not in ("Archivo", "RRHH"):
            raise ValueError("modulo debe ser 'Archivo' o 'RRHH'")
        return v

    @field_validator("file_url", "foto_url", mode="before")
    @classmethod
    def file_url_scheme(cls, v):
        return _validate_file_url(v)

    @field_validator("status")
    @classmethod
    def status_must_be_valid(cls, v):
        allowed = ("draft", "revision", "aprobado", "rechazado")
        if v and v not in allowed:
            raise ValueError(f"status inválido, debe ser uno de: {', '.join(allowed)}")
        return v or "aprobado"

    @field_validator("soporte")
    @classmethod
    def soporte_valid(cls, v):
        if v and v not in _SOPORTES_VALIDOS:
            raise ValueError(f"soporte debe ser uno de: {', '.join(_SOPORTES_VALIDOS)}")
        return v or "Físico"

    @field_validator("fecha", "fecha_jubilacion", "fecha_pension",
                     "fecha_nacimiento", "fecha_vencimiento", mode="before")
    @classmethod
    def fecha_format(cls, v):
        return _validate_date(v)

    @field_validator("cedula")
    @classmethod
    def cedula_format(cls, v):
        return _normalize_cedula(v)

    @model_validator(mode="after")
    def rrhh_campos_obligatorios(self):
        """OR-087: la pantalla marca con asterisco Nombres, Apellidos, Cédula,
        Departamento y Estado (además de Tipo/Fecha/Ubicación, ya obligatorios
        a nivel de campo) pero el backend sólo exigía Nombres, Cédula y
        Ubicación. Sin esto entraban expedientes que la propia pantalla
        prometía completos."""
        if self.modulo == "RRHH":
            faltantes = [campo for campo, valor in (
                ("nombres", self.nombres), ("apellidos", self.apellidos),
                ("cedula", self.cedula), ("departamento", self.departamento),
                ("estado", self.estado),
            ) if not (valor or "").strip()]
            if faltantes:
                raise ValueError(f"faltan campos obligatorios: {', '.join(faltantes)}")
        return self

    @model_validator(mode="after")
    def fecha_nacimiento_coherente(self):
        """OR-085: nada impedía una fecha de nacimiento posterior a la de
        ingreso o jubilación — un dedo torpe teclea 1999 en vez de 1969 y la
        persona aparece con 26 años en el KPI de jubilaciones próximas."""
        nacimiento = _parse_iso_date(self.fecha_nacimiento)
        if not nacimiento:
            return self
        for campo, valor in (("fecha (documento)", self.fecha),
                              ("fecha_jubilacion", self.fecha_jubilacion),
                              ("fecha_pension", self.fecha_pension)):
            otra = _parse_iso_date(valor)
            if otra and nacimiento >= otra:
                raise ValueError(f"fecha_nacimiento debe ser anterior a {campo}")
        return self


class StatsRequest(BaseModel):
    modulo:     str
    date_start: Optional[str] = ""
    date_end:   Optional[str] = ""
    doc_types:  Optional[List[Str255]] = []
    status:     Optional[str] = ""
    dept:       Annotated[Optional[str], Field(max_length=200)] = ""
    author:     Annotated[Optional[str], Field(max_length=255)] = ""
    only_recent:Optional[bool] = False


class CategoryCreateRequest(BaseModel):
    name:    Annotated[str, Field(max_length=200)]
    desc:    Annotated[str, Field(max_length=500)]
    scope:   Annotated[str, Field(max_length=50)]
    usuario: Annotated[str, Field(max_length=100)]
    parte:   Annotated[Optional[str], Field(max_length=50)] = ""


class KeywordRequest(BaseModel):
    nombre: Annotated[str, Field(max_length=200)]


class UserCreateRequest(BaseModel):
    # OA-039/IN-142/SI-036: el piso real (12, y rechazo de contraseñas
    # comunes) lo aplica `_validar_fuerza_password` en `admin/users.py` —
    # aquí sólo el mínimo de Pydantic, pero alineado con esa política para
    # que el 422 llegue antes que el 400 del endpoint, no en contra de él.
    usuario:  Annotated[str, Field(max_length=100)]
    password: Annotated[str, Field(min_length=12, max_length=200)]
    modulo:   Annotated[str, Field(max_length=50)]
    rol:      Annotated[str, Field(max_length=50)]

    @field_validator("password")
    @classmethod
    def password_min_length(cls, v):
        if len(v.strip()) < 12:
            raise ValueError("La contraseña debe tener al menos 12 caracteres")
        return v.strip()

    @field_validator("usuario")
    @classmethod
    def usuario_not_empty(cls, v):
        if not v.strip():
            raise ValueError("El nombre de usuario no puede estar vacío")
        return v.strip()


class DocumentUpdateRequest(BaseModel):
    modulo:               str
    id:                   int
    titulo:               Annotated[Optional[str], Field(max_length=500)]  = None
    autor:                Annotated[Optional[str], Field(max_length=255)]  = None
    resumen:              Annotated[Optional[str], Field(max_length=4000)] = None
    doc_type:             Annotated[Optional[str], Field(max_length=200)]  = None
    fecha:                Optional[str] = None
    ubicacion:            Annotated[Optional[str], Field(max_length=500)]  = None
    palabras_clave:       Annotated[Optional[str], Field(max_length=2000)] = None
    tesauro_secundario:   Annotated[Optional[str], Field(max_length=200)]  = None
    personas_relacionadas:Annotated[Optional[str], Field(max_length=1000)] = None
    file_url:             Annotated[Optional[str], Field(max_length=2048)] = None
    status:               Optional[str] = None
    notas:                Annotated[Optional[str], Field(max_length=4000)] = None
    numero_folio:         Annotated[Optional[str], Field(max_length=100)]  = None
    soporte:              Annotated[Optional[str], Field(max_length=50)]   = None
    numero_paginas:       Optional[int] = None
    idioma:               Annotated[Optional[str], Field(max_length=10)]   = None
    fecha_vencimiento:    Optional[str] = None
    usuario:              Annotated[str, Field(max_length=100)]
    updated_at:           Annotated[Optional[str], Field(max_length=100)] = None

    @field_validator("file_url", mode="before")
    @classmethod
    def file_url_scheme(cls, v):
        return _validate_file_url(v)

    @field_validator("soporte")
    @classmethod
    def soporte_valid(cls, v):
        if v and v not in _SOPORTES_VALIDOS:
            raise ValueError(f"soporte debe ser uno de: {', '.join(_SOPORTES_VALIDOS)}")
        return v

    @field_validator("fecha", "fecha_vencimiento", mode="before")
    @classmethod
    def fecha_format(cls, v):
        return _validate_date(v)


class EmpleadoUpdateRequest(BaseModel):
    nombres:          Annotated[Optional[str], Field(max_length=200)] = None
    apellidos:        Annotated[Optional[str], Field(max_length=200)] = None
    cargo:            Annotated[Optional[str], Field(max_length=200)] = None
    departamento:     Annotated[Optional[str], Field(max_length=200)] = None
    estado:           Annotated[Optional[str], Field(max_length=100)] = None
    fecha_jubilacion: Optional[str] = None
    fecha_pension:    Optional[str] = None
    fecha_nacimiento: Optional[str] = None
    nivel_educativo:  Annotated[Optional[str], Field(max_length=100)] = None
    sexo:             Optional[str] = None
    foto_url:         Annotated[Optional[str], Field(max_length=2048)] = None
    rif:              Annotated[Optional[str], Field(max_length=20)]   = None
    usuario:          Annotated[str, Field(max_length=100)]

    @field_validator("foto_url", mode="before")
    @classmethod
    def foto_url_scheme(cls, v):
        return _validate_file_url(v)

    @field_validator("sexo")
    @classmethod
    def sexo_valid(cls, v):
        if v and v.upper() not in ("M", "F", "O", ""):
            raise ValueError("sexo debe ser M, F u O")
        return (v or "").upper() or None

    @field_validator("nivel_educativo")
    @classmethod
    def nivel_educativo_valid(cls, v):
        allowed = {"Bachiller", "TSU", "Universitario", "Especialización",
                   "Maestría", "Doctorado", "Postdoctorado", ""}
        if v and v not in allowed:
            raise ValueError(f"nivel_educativo inválido: {v}")
        return v or None

    @field_validator("fecha_jubilacion", "fecha_pension", "fecha_nacimiento", mode="before")
    @classmethod
    def fecha_format(cls, v):
        return _validate_date(v)

    @model_validator(mode="after")
    def fecha_nacimiento_coherente(self):
        """OR-085: aplicado también a la edición del expediente, no sólo al
        alta — un dedo torpe puede corregir una fecha y volver a introducir
        el mismo error de orden."""
        nacimiento = _parse_iso_date(self.fecha_nacimiento)
        if not nacimiento:
            return self
        for campo, valor in (("fecha_jubilacion", self.fecha_jubilacion),
                              ("fecha_pension", self.fecha_pension)):
            otra = _parse_iso_date(valor)
            if otra and nacimiento >= otra:
                raise ValueError(f"fecha_nacimiento debe ser anterior a {campo}")
        return self


class PasswordChangeRequest(BaseModel):
    # OA-039/IN-142/SI-036: igual que en UserCreateRequest, alineado con el
    # mínimo real de `admin/users.py` (12).
    new_password: Annotated[str, Field(min_length=12, max_length=200)]

    @field_validator("new_password")
    @classmethod
    def password_min_length(cls, v):
        if len(v.strip()) < 12:
            raise ValueError("La contraseña debe tener al menos 12 caracteres")
        return v.strip()


class DocumentStatusUpdateRequest(BaseModel):
    status:  str
    usuario: Annotated[str, Field(max_length=100)]

    @field_validator("status")
    @classmethod
    def status_valid(cls, v):
        allowed = ("draft", "revision", "aprobado", "rechazado")
        if v not in allowed:
            raise ValueError(f"status debe ser uno de: {', '.join(allowed)}")
        return v
