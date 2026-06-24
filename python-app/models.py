from typing import List, Optional
from pydantic import BaseModel


# =============================================================================
# AUTENTICACIÓN
# =============================================================================

class LoginRequest(BaseModel):
    username: str
    password: str


class RestoreSessionRequest(BaseModel):
    username: str


# =============================================================================
# BÚSQUEDA
# =============================================================================

class ArchivoSearchRequest(BaseModel):
    search_term: Optional[str] = ""
    doc_types: Optional[List[str]] = []
    tesauro_terms: Optional[List[str]] = []
    date_start: Optional[str] = ""
    date_end: Optional[str] = ""
    sort_mode: Optional[str] = "Alfabético (A-Z)"
    page: int = 1
    per_page: int = 10


class RrhhSearchRequest(BaseModel):
    search_term: Optional[str] = ""
    doc_types: Optional[List[str]] = []
    people_terms: Optional[List[str]] = []
    estados: Optional[List[str]] = []
    date_start: Optional[str] = ""
    date_end: Optional[str] = ""
    sort_mode: Optional[str] = "Alfabético (A-Z)"
    page: int = 1
    per_page: int = 10


class RrhhProfileRequest(BaseModel):
    persona: str


# =============================================================================
# ADMINISTRACIÓN
# =============================================================================

class DocumentSubmitRequest(BaseModel):
    modulo: str
    usuario: str
    titulo: Optional[str] = ""
    autor: Optional[str] = ""
    resumen: Optional[str] = ""
    empleado: Optional[str] = ""
    nombres: Optional[str] = ""
    apellidos: Optional[str] = ""
    cedula: Optional[str] = ""
    personas_relacionadas: Optional[str] = ""
    departamento: Optional[str] = ""
    estado: Optional[str] = ""
    fecha_jubilacion: Optional[str] = ""
    fecha_pension: Optional[str] = ""
    foto_url: Optional[str] = ""
    rif: Optional[str] = ""
    cargo: Optional[str] = ""
    doc_type: str
    fecha: str
    ubicacion: str
    tesauro_secundario: Optional[str] = ""
    descriptores_libres: Optional[str] = ""


class StatsRequest(BaseModel):
    modulo: str
    date_start: Optional[str] = ""
    date_end: Optional[str] = ""
    doc_types: Optional[List[str]] = []
    status: Optional[str] = ""
    dept: Optional[str] = ""
    author: Optional[str] = ""
    only_recent: Optional[bool] = False


class CategoryCreateRequest(BaseModel):
    name: str
    desc: str
    scope: str
    usuario: str
    parte: Optional[str] = ""  # slug de categoría: 'parte-i', 'parte-ii', 'archivo', etc.


class KeywordRequest(BaseModel):
    nombre: str


class UserCreateRequest(BaseModel):
    usuario: str
    password: str
    modulo: str
    rol: str
    creator: str


class DocumentUpdateRequest(BaseModel):
    modulo: str
    id: int
    titulo: Optional[str] = None
    autor: Optional[str] = None
    resumen: Optional[str] = None
    doc_type: Optional[str] = None
    fecha: Optional[str] = None
    ubicacion: Optional[str] = None
    palabras_clave: Optional[str] = None  # comma-separated
    tesauro_secundario: Optional[str] = None
    personas_relacionadas: Optional[str] = None
    usuario: str


class EmpleadoUpdateRequest(BaseModel):
    nombres: Optional[str] = None
    apellidos: Optional[str] = None
    cargo: Optional[str] = None
    departamento: Optional[str] = None
    estado: Optional[str] = None
    fecha_jubilacion: Optional[str] = None
    fecha_pension: Optional[str] = None
    foto_url: Optional[str] = None
    rif: Optional[str] = None
    usuario: str


class PasswordChangeRequest(BaseModel):
    new_password: str
    requester: str
