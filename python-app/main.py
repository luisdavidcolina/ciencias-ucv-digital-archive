import os
import csv
import logging
import platform
import multiprocessing
from datetime import datetime
from typing import List, Optional, Dict, Any
from fastapi import FastAPI, HTTPException, status
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from fastapi.responses import FileResponse, RedirectResponse
from pydantic import BaseModel
import pandas as pd

# Configurar logging
logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger("DigitalArchive")

app = FastAPI(title="Ciencias UCV Digital Archive", version="2.0.0")

# Habilitar CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Deshabilitar cacheo de archivos estáticos en cliente
@app.middleware("http")
async def add_no_cache_header(request, call_next):
    response = await call_next(request)
    response.headers["Cache-Control"] = "no-store, no-cache, must-revalidate, max-age=0"
    return response

# Rutas a archivos de datos
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CSV_ARCHIVO = os.path.join(BASE_DIR, "datos_archivo.csv")
CSV_RRHH_PERSONAS = os.path.join(BASE_DIR, "rrhh_personas.csv")
CSV_RRHH_ARCHIVOS = os.path.join(BASE_DIR, "rrhh_archivos.csv")
CSV_USERS = os.path.join(BASE_DIR, "usuarios.csv")
CSV_AUDIT = os.path.join(BASE_DIR, "audit_log.csv")

# Helpers de base de datos
def get_users_df() -> pd.DataFrame:
    if os.path.exists(CSV_USERS):
        return pd.read_csv(CSV_USERS, dtype=str).fillna("")
    return pd.DataFrame(columns=["usuario", "password", "modulo", "rol"])

def get_archivo_df() -> pd.DataFrame:
    if os.path.exists(CSV_ARCHIVO):
        df = pd.read_csv(CSV_ARCHIVO, dtype=str).fillna("")
        for col in ["tesauro_primario", "tesauro_secundario", "descriptores_libres", "resumen"]:
            if col not in df.columns:
                df[col] = ""
        # Backfill tesauro
        empty_mask = df["tesauro_primario"].str.strip() == ""
        if "doc_type" in df.columns:
            df.loc[empty_mask, "tesauro_primario"] = df.loc[empty_mask, "doc_type"]
        return df
    return pd.DataFrame(columns=["id", "titulo", "autor", "fecha", "doc_type", "ubicacion", "tesauro_primario", "tesauro_secundario", "descriptores_libres", "resumen"])

def get_rrhh_df() -> pd.DataFrame:
    if os.path.exists(CSV_RRHH_PERSONAS) and os.path.exists(CSV_RRHH_ARCHIVOS):
        df_p = pd.read_csv(CSV_RRHH_PERSONAS, dtype=str).fillna("")
        df_a = pd.read_csv(CSV_RRHH_ARCHIVOS, dtype=str).fillna("")
        df = pd.merge(df_p, df_a, on="cedula", how="left").fillna("")
        
        # Asegurar columnas
        for col in ["personas_relacionadas", "foto_url", "fecha_jubilacion", "fecha_pension", "estado", "rif", "cargo"]:
            if col not in df.columns:
                df[col] = ""
        # Backfill personas_relacionadas con empleado
        if "empleado" in df.columns:
            empty_mask = df["personas_relacionadas"].str.strip() == ""
            df.loc[empty_mask, "personas_relacionadas"] = df.loc[empty_mask, "empleado"]
        return df
    return pd.DataFrame(columns=["cedula", "empleado", "personas_relacionadas", "departamento", "estado", "doc_type", "fecha_ingreso", "ubicacion", "foto_url", "fecha_jubilacion", "fecha_pension", "rif", "cargo", "id_archivo"])

def log_event(usuario: str, accion: str, modulo: str, detalle: str, status_str: str = "Success"):
    now_str = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    file_exists = os.path.exists(CSV_AUDIT)
    with open(CSV_AUDIT, mode="a", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        if not file_exists:
            writer.writerow(["timestamp", "usuario", "accion", "modulo", "detalle", "status"])
        writer.writerow([now_str, usuario, accion, modulo, detalle, status_str])

# --- HELPERS DE NEGOCIO ORIGINALES DE R ---
def split_terms(val_str: str) -> List[str]:
    if not val_str:
        return []
    return [t.strip() for t in str(val_str).split(";") if t.strip()]

def format_rrhh_person_name(name: str) -> str:
    name = name.strip()
    if not name:
        return ""
    if "," in name:
        parts = [p.strip() for p in name.split(",") if p.strip()]
        if len(parts) >= 2:
            return f"{parts[0]}, {' '.join(parts[1:])}"
        return name
    
    parts = name.split()
    if len(parts) <= 1:
        return name
    if len(parts) == 2:
        return f"{parts[1]}, {parts[0]}"
    
    surnames = " ".join(parts[-2:])
    given_names = " ".join(parts[:-2])
    return f"{surnames}, {given_names}"

def first_non_empty_value(values) -> str:
    if values is None:
        return ""
    series = pd.Series(values).dropna().astype(str).str.strip()
    series = series[series != ""]
    if series.empty:
        return ""
    return series.iloc[0]

def build_rrhh_person_index(df: pd.DataFrame) -> List[Dict[str, Any]]:
    if df.empty:
        return []
    
    # Extraer todas las personas únicas vinculadas (empleado o personas_relacionadas)
    all_persons = set()
    for _, row in df.iterrows():
        emp = row.get("empleado", "").strip()
        if emp:
            all_persons.add(emp)
        rel = split_terms(row.get("personas_relacionadas", ""))
        all_persons.update(rel)
        
    all_persons = sorted(list(all_persons))
    profiles = []
    
    for p in all_persons:
        # Encontrar todas las filas en las que p esté vinculada
        p_rows = []
        p_indices = []
        for idx, row in df.iterrows():
            emp = row.get("empleado", "").strip()
            rel = split_terms(row.get("personas_relacionadas", ""))
            if p == emp or p in rel:
                p_rows.append(row)
                p_indices.append(idx)
                
        if not p_rows:
            continue
            
        p_df = pd.DataFrame(p_rows)
        
        # Atributos agregados
        doc_count = len(p_df)
        primary_count = sum(p_df["empleado"] == p)
        
        # Obtener datos de las filas principales o más comunes
        primary_rows = p_df[p_df["empleado"] == p]
        cedula = primary_rows["cedula"].iloc[0] if not primary_rows.empty else p_df["cedula"].iloc[0]
        rif = primary_rows["rif"].iloc[0] if not primary_rows.empty and "rif" in primary_rows.columns else ""
        cargo = primary_rows["cargo"].iloc[0] if not primary_rows.empty and "cargo" in primary_rows.columns else ""
        
        # Semicolon collapsed unique values
        deptos = "; ".join(sorted(p_df["departamento"].dropna().unique().tolist()))
        estados = "; ".join(sorted(p_df["estado"].dropna().unique().tolist()))
        tipos = "; ".join(sorted(p_df["doc_type"].dropna().unique().tolist()))
        fecha_ingreso = "; ".join(sorted(p_df["fecha_ingreso"].dropna().unique().tolist()))
        
        foto_url = first_non_empty_value(p_df["foto_url"] if "foto_url" in p_df.columns else [])

        profiles.append({
            "persona_raw": p,
            "persona": format_rrhh_person_name(p),
            "doc_count": doc_count,
            "primary_count": primary_count,
            "cedulas": cedula,
            "rifs": rif,
            "departamentos": deptos,
            "cargos": cargo or "Sin cargo asignado",
            "estatuses": estados or "Sin estado",
            "tipos": tipos,
            "fecha_ingreso": fecha_ingreso,
            "foto_url": foto_url,
            "row_indices": p_indices
        })
        
    return profiles

# Modelos de Petición
class LoginRequest(BaseModel):
    username: str
    password: str

class RestoreSessionRequest(BaseModel):
    username: str

class ArchivoSearchRequest(BaseModel):
    search_term: Optional[str] = ""
    doc_types: Optional[List[str]] = []
    tesauro_terms: Optional[List[str]] = []
    date_start: Optional[str] = ""
    date_end: Optional[str] = ""
    sort_mode: Optional[str] = "Alfabético (A-Z)"

class RrhhSearchRequest(BaseModel):
    search_term: Optional[str] = ""
    doc_types: Optional[List[str]] = []
    people_terms: Optional[List[str]] = []
    estados: Optional[List[str]] = []
    date_start: Optional[str] = ""
    date_end: Optional[str] = ""
    sort_mode: Optional[str] = "Alfabético (A-Z)"

class DocumentSubmitRequest(BaseModel):
    modulo: str # "Archivo" o "RRHH"
    usuario: str
    # Datos Archivo
    titulo: Optional[str] = ""
    autor: Optional[str] = ""
    resumen: Optional[str] = ""
    # Datos RRHH
    empleado: Optional[str] = ""
    cedula: Optional[str] = ""
    personas_relacionadas: Optional[str] = ""
    departamento: Optional[str] = ""
    estado: Optional[str] = ""
    fecha_jubilacion: Optional[str] = ""
    fecha_pension: Optional[str] = ""
    foto_url: Optional[str] = ""
    rif: Optional[str] = ""
    cargo: Optional[str] = ""
    # Comunes
    doc_type: str
    fecha: str # Para Archivo fecha_emision, para RRHH fecha_ingreso
    ubicacion: str

class RrhhProfileRequest(BaseModel):
    persona: str

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

class UserCreateRequest(BaseModel):
    usuario: str
    password: str
    modulo: str
    rol: str
    creator: str

# Endpoints
@app.post("/api/auth/login")
def login(req: LoginRequest):
    df = get_users_df()
    user_rows = df[(df["usuario"].str.strip() == req.username.strip()) & (df["password"].str.strip() == req.password.strip())]
    if len(user_rows) >= 1:
        # Aggregar módulos y roles por módulo (soporte para filas múltiples por mismo usuario)
        modules = []
        roles = {}
        for _, row in user_rows.iterrows():
            mod = str(row.get("modulo", "")).strip()
            rol = str(row.get("rol", "Normal")).strip() or "Normal"
            if mod and mod not in modules:
                modules.append(mod)
            if mod:
                roles[mod] = rol

        primary_mod = modules[0] if modules else "Archivo"
        primary_role = roles.get(primary_mod, "Normal")

        log_event(req.username, "Login Success", ";".join(modules), f"Roles: {roles}")
        return {
            "success": True,
            "user": {
                "username": req.username.strip(),
                "modules": modules,
                "roles": roles,
                # compatibilidad con cliente anterior
                "modulo": primary_mod,
                "rol": primary_role
            }
        }

    log_event(req.username, "Login Failure", "Auth", "Credenciales incorrectas", "Failure")
    raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED, detail="Credenciales incorrectas")

@app.post("/api/auth/restore")
def restore_session(req: RestoreSessionRequest):
    df = get_users_df()
    user_rows = df[df["usuario"].str.strip() == req.username.strip()]
    if len(user_rows) >= 1:
        modules = []
        roles = {}
        for _, row in user_rows.iterrows():
            mod = str(row.get("modulo", "")).strip()
            rol = str(row.get("rol", "Normal")).strip() or "Normal"
            if mod and mod not in modules:
                modules.append(mod)
            if mod:
                roles[mod] = rol

        primary_mod = modules[0] if modules else "Archivo"
        primary_role = roles.get(primary_mod, "Normal")

        log_event(req.username, "Session Restored", ";".join(modules), f"Roles: {roles}")
        return {
            "success": True,
            "user": {
                "username": req.username.strip(),
                "modules": modules,
                "roles": roles,
                "modulo": primary_mod,
                "rol": primary_role
            }
        }

    raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED, detail="Sesión no encontrada")

@app.get("/api/choices")
def get_choices():
    df_arch = get_archivo_df()
    df_rh = get_rrhh_df()
    
    # Archivo choices
    arch_doc_types = sorted(df_arch["doc_type"].dropna().unique().tolist())
    arch_doc_types = [x for x in arch_doc_types if x.strip()]
    
    tesauro_set = set()
    for col in ["doc_type", "tesauro_primario", "tesauro_secundario", "descriptores_libres"]:
        if col in df_arch.columns:
            for val in df_arch[col].dropna():
                tesauro_set.update(split_terms(val))
    arch_tesauro = sorted(list(tesauro_set))
    
    # RRHH choices
    rh_doc_types = sorted(df_rh["doc_type"].dropna().unique().tolist())
    rh_doc_types = [x for x in rh_doc_types if x.strip()]
    
    rh_estados = sorted(df_rh["estado"].dropna().unique().tolist())
    rh_estados = [x for x in rh_estados if x.strip()]
    
    people_set = set()
    if "empleado" in df_rh.columns:
        people_set.update(df_rh["empleado"].dropna().str.strip())
    if "personas_relacionadas" in df_rh.columns:
        for val in df_rh["personas_relacionadas"].dropna():
            people_set.update(split_terms(val))
    rh_people = sorted([x for x in list(people_set) if x.strip()])
    
    # Límites
    arch_dates = pd.to_datetime(df_arch["fecha"], errors='coerce').dropna()
    min_arch = arch_dates.min().strftime("%Y-%m-%d") if not arch_dates.empty else "2000-01-01"
    max_arch = arch_dates.max().strftime("%Y-%m-%d") if not arch_dates.empty else datetime.now().strftime("%Y-%m-%d")
    
    rh_dates = pd.to_datetime(df_rh["fecha_ingreso"], errors='coerce').dropna()
    min_rh = rh_dates.min().strftime("%Y-%m-%d") if not rh_dates.empty else "2000-01-01"
    max_rh = rh_dates.max().strftime("%Y-%m-%d") if not rh_dates.empty else datetime.now().strftime("%Y-%m-%d")
    
    return {
        "archivo": {
            "doc_types": arch_doc_types,
            "tesauro": arch_tesauro,
            "min_date": min_arch,
            "max_date": max_arch
        },
        "rrhh": {
            "doc_types": rh_doc_types,
            "estados": rh_estados,
            "people": rh_people,
            "min_date": min_rh,
            "max_date": max_rh
        }
    }

@app.post("/api/archivo/buscar")
def search_archivo(req: ArchivoSearchRequest):
    df = get_archivo_df()
    
    # 1. Búsqueda por texto (Título o Autor)
    if req.search_term:
        term = req.search_term.lower().strip()
        df = df[
            df["titulo"].str.lower().str.contains(term, na=False) |
            df["autor"].str.lower().str.contains(term, na=False) |
            df["resumen"].str.lower().str.contains(term, na=False) |
            df["ubicacion"].str.lower().str.contains(term, na=False)
        ]
        
    # 2. Filtrar por tipos de documento
    if req.doc_types:
        df = df[df["doc_type"].isin(req.doc_types)]
        
    # 3. Filtrar por Tesauro (primario, secundario o libres)
    if req.tesauro_terms:
        keep = []
        for idx, row in df.iterrows():
            row_terms = set(split_terms(row.get("doc_type", "")))
            for col in ["tesauro_primario", "tesauro_secundario", "descriptores_libres"]:
                row_terms.update(split_terms(row.get(col, "")))
            if any(t in row_terms for t in req.tesauro_terms):
                keep.append(idx)
        df = df.loc[keep]
        
    # 4. Rango de fechas
    if req.date_start and req.date_end:
        df = df[(df["fecha"] >= req.date_start) & (df["fecha"] <= req.date_end)]
        
    # 5. Ordenamiento
    if req.sort_mode == "Alfabético (A-Z)":
        df = df.sort_values(by="titulo", key=lambda c: c.str.lower())
    elif req.sort_mode == "Alfabético (Z-A)":
        df = df.sort_values(by="titulo", key=lambda c: c.str.lower(), ascending=False)
    elif req.sort_mode == "Más recientes primero":
        df = df.sort_values(by="fecha", ascending=False)
    elif req.sort_mode == "Más antiguos primero":
        df = df.sort_values(by="fecha")
        
    records = df.to_dict(orient="records")
    # Inyectar ID sintético en los registros devueltos
    for idx, rec in enumerate(records):
        rec["__idx"] = idx + 1
        rec["tesauro_badges"] = sorted(list(set(split_terms(rec.get("doc_type", "")) + split_terms(rec.get("tesauro_primario", "")) + split_terms(rec.get("tesauro_secundario", "")) + split_terms(rec.get("descriptores_libres", "")))))
        
    return records

@app.post("/api/rrhh/buscar")
def search_rrhh(req: RrhhSearchRequest):
    df = get_rrhh_df()
    
    # Filtrar tipologías
    if req.doc_types:
        df = df[df["doc_type"].isin(req.doc_types)]
        
    # Filtrar Estados
    if req.estados:
        df = df[df["estado"].isin(req.estados)]
        
    # Filtrar por Rango de Fechas
    if req.date_start and req.date_end:
        df = df[(df["fecha_ingreso"] >= req.date_start) & (df["fecha_ingreso"] <= req.date_end)]
        
    # Filtrar por Personas
    if req.people_terms:
        keep = []
        for idx, row in df.iterrows():
            row_people = {row.get("empleado", "").strip()}
            row_people.update(split_terms(row.get("personas_relacionadas", "")))
            if any(p in row_people for p in req.people_terms):
                keep.append(idx)
        df = df.loc[keep]
        
    # Construir Perfiles Agregados
    profiles = build_rrhh_person_index(df)
    
    # Filtrar por término de búsqueda en perfiles agregados (nombre, cedula, departamento, cargo)
    if req.search_term:
        term = req.search_term.lower().strip()
        profiles = [
            p for p in profiles if (
                term in p["persona"].lower() or
                term in p["persona_raw"].lower() or
                term in p["cedulas"].lower() or
                term in p["departamentos"].lower() or
                term in p["cargos"].lower() or
                term in p["estatuses"].lower() or
                term in p["tipos"].lower()
            )
        ]
        
    # Aplicar ordenación en perfiles
    if req.sort_mode == "Alfabético (A-Z)":
        profiles = sorted(profiles, key=lambda x: x["persona"].lower())
    elif req.sort_mode == "Alfabético (Z-A)":
        profiles = sorted(profiles, key=lambda x: x["persona"].lower(), reverse=True)
    elif req.sort_mode == "Más recientes primero":
        profiles = sorted(profiles, key=lambda x: x["fecha_ingreso"], reverse=True)
    elif req.sort_mode == "Más antiguos primero":
        profiles = sorted(profiles, key=lambda x: x["fecha_ingreso"])
        
    return profiles

@app.post("/api/rrhh/person/profile")
def get_rrhh_person_profile(req: RrhhProfileRequest):
    df = get_rrhh_df()
    p = req.persona
    
    # Encontrar todas las filas en las que p esté vinculada
    keep = []
    row_list = []
    for idx, row in df.iterrows():
        emp = row.get("empleado", "").strip()
        rel = split_terms(row.get("personas_relacionadas", ""))
        if p == emp or p in rel:
            keep.append(idx)
            row_dict = row.to_dict()
            row_dict["__idx"] = idx + 1 # ID Real para abrir
            row_list.append(row_dict)
            
    if not row_list:
        raise HTTPException(status_code=404, detail="Persona no encontrada en expedientes")
        
    p_df = pd.DataFrame(row_list)
    
    # Atributos de Perfil
    foto_url = first_non_empty_value(p_df["foto_url"] if "foto_url" in p_df.columns else [])
    cedulas = "; ".join(sorted(p_df["cedula"].dropna().unique().tolist()))
    rifs = "; ".join(sorted(p_df["rif"].dropna().unique().tolist())) if "rif" in p_df.columns else ""
    departamentos = "; ".join(sorted(p_df["departamento"].dropna().unique().tolist()))
    cargos = "; ".join(sorted(p_df["cargo"].dropna().unique().tolist())) if "cargo" in p_df.columns else "Sin cargo asignado"
    statuses = "; ".join(sorted(p_df["estado"].dropna().unique().tolist()))
    fecha_ingreso = "; ".join(sorted(p_df["fecha_ingreso"].dropna().unique().tolist()))
    fecha_jubilacion = "; ".join(sorted(p_df["fecha_jubilacion"].dropna().unique().tolist())) if "fecha_jubilacion" in p_df.columns else ""
    fecha_pension = "; ".join(sorted(p_df["fecha_pension"].dropna().unique().tolist())) if "fecha_pension" in p_df.columns else ""
    
    categories = sorted(p_df["doc_type"].dropna().unique().tolist())
    
    return {
        "persona_raw": p,
        "persona": format_rrhh_person_name(p),
        "foto_url": foto_url or "",
        "cedulas": cedulas,
        "rifs": rifs,
        "departamentos": departamentos,
        "cargos": cargos,
        "statuses": statuses,
        "fecha_ingreso": fecha_ingreso,
        "fecha_jubilacion": fecha_jubilacion,
        "fecha_pension": fecha_pension,
        "categories": categories,
        "rows": row_list
    }

@app.post("/api/admin/stats")
def get_admin_stats(req: StatsRequest):
    df = get_archivo_df() if req.modulo == "Archivo" else get_rrhh_df()
    
    if df.empty:
        return {"total_docs": 0, "categories_count": 0, "by_type": [], "timeline": []}
        
    fecha_col = "fecha" if req.modulo == "Archivo" else "fecha_ingreso"
    
    # 1. Rango de fechas
    if req.date_start and req.date_end:
        df = df[(df[fecha_col] >= req.date_start) & (df[fecha_col] <= req.date_end)]
        
    # 2. Tipología
    if req.doc_types:
        df = df[df["doc_type"].isin(req.doc_types)]
        
    # 3. Filtros específicos
    if req.modulo == "RRHH":
        if req.status:
            df = df[df["estado"] == req.status]
        if req.dept:
            df = df[df["departamento"] == req.dept]
    else:
        if req.author:
            df = df[df["autor"] == req.author]
        if req.only_recent:
            cutoff = (datetime.now().year - 2)
            df = df[pd.to_datetime(df["fecha"], errors='coerce').dt.year >= cutoff]
            
    total_docs = len(df)
    categories_count = len(df["doc_type"].dropna().unique())
    
    # Conteos por Tipo
    by_type = []
    if total_docs > 0:
        conteos = df["doc_type"].value_counts()
        for t_name, count in conteos.items():
            by_type.append({
                "type": t_name,
                "count": int(count),
                "pct": int(round(count / total_docs * 100))
            })
            
    # Cronología de Ingresos por Año
    timeline = []
    if total_docs > 0:
        years = pd.to_datetime(df[fecha_col], errors='coerce').dt.year.dropna().astype(int)
        if not years.empty:
            conteos_anio = years.value_counts().sort_index()
            max_c = int(conteos_anio.max())
            for y_val, count in conteos_anio.items():
                timeline.append({
                    "year": str(y_val),
                    "count": int(count),
                    "pct_width": int(round(count / max_c * 100)) if max_c > 0 else 0
                })
                
    # Estado del Sistema
    system_status = "Operativo" if total_docs > 0 else "Sin Datos"
    
    return {
        "total_docs": total_docs,
        "categories_count": categories_count,
        "by_type": by_type,
        "timeline": timeline,
        "system": {
            "status": system_status,
            "ram": f"{round(30 + total_docs * 0.005, 2)} MB",
            "cpu": multiprocessing.cpu_count(),
            "os": f"{platform.system()} {platform.release()}"
        }
    }

@app.post("/api/admin/submit")
def admin_submit(req: DocumentSubmitRequest):
    log_event(req.usuario, "Create Document", req.modulo, f"Tipo: {req.doc_type}, Ubicacion: {req.ubicacion}")
    
    if req.modulo == "Archivo":
        df = get_archivo_df()
        new_id = str(len(df) + 1)
        new_row = {
            "id": new_id,
            "titulo": req.titulo or "Sin título",
            "autor": req.autor or "Anónimo",
            "fecha": req.fecha,
            "doc_type": req.doc_type,
            "ubicacion": req.ubicacion,
            "tesauro_primario": req.doc_type,
            "tesauro_secundario": "",
            "descriptores_libres": "",
            "resumen": req.resumen or ""
        }
        # Append to CSV
        file_exists = os.path.exists(CSV_ARCHIVO)
        with open(CSV_ARCHIVO, mode="a", newline="", encoding="utf-8") as f:
            writer = csv.writer(f)
            if not file_exists:
                writer.writerow(new_row.keys())
            writer.writerow(new_row.values())
        return {"success": True, "id": new_id}
    else:
        df_p = pd.read_csv(CSV_RRHH_PERSONAS, dtype=str).fillna("") if os.path.exists(CSV_RRHH_PERSONAS) else pd.DataFrame(columns=["cedula", "empleado", "rif", "cargo", "departamento", "estado", "fecha_ingreso", "fecha_jubilacion", "fecha_pension", "foto_url"])
        df_a = pd.read_csv(CSV_RRHH_ARCHIVOS, dtype=str).fillna("") if os.path.exists(CSV_RRHH_ARCHIVOS) else pd.DataFrame(columns=["id_archivo", "cedula", "doc_type", "ubicacion", "personas_relacionadas"])
        
        # Generar nuevo id de archivo
        next_num = len(df_a) + 1
        new_id_arch = f"DOC-{str(next_num).zfill(3)}"
        
        # Si la persona no existe, se registra
        if req.cedula not in df_p["cedula"].values:
            new_p = {
                "cedula": req.cedula,
                "empleado": req.empleado,
                "rif": req.rif or "",
                "cargo": req.cargo or "Sin cargo asignado",
                "departamento": req.departamento,
                "estado": req.estado,
                "fecha_ingreso": req.fecha,
                "fecha_jubilacion": req.fecha_jubilacion or "",
                "fecha_pension": req.fecha_pension or "",
                "foto_url": req.foto_url or ""
            }
            # Append to rrhh_personas
            with open(CSV_RRHH_PERSONAS, mode="a", newline="", encoding="utf-8") as f:
                writer = csv.writer(f)
                if not os.path.exists(CSV_RRHH_PERSONAS) or os.stat(CSV_RRHH_PERSONAS).st_size == 0:
                    writer.writerow(new_p.keys())
                writer.writerow(new_p.values())
                
        # Agregar registro en rrhh_archivos
        new_a = {
            "id_archivo": new_id_arch,
            "cedula": req.cedula,
            "doc_type": req.doc_type,
            "ubicacion": req.ubicacion,
            "personas_relacionadas": req.personas_relacionadas or req.empleado
        }
        with open(CSV_RRHH_ARCHIVOS, mode="a", newline="", encoding="utf-8") as f:
            writer = csv.writer(f)
            if not os.path.exists(CSV_RRHH_ARCHIVOS) or os.stat(CSV_RRHH_ARCHIVOS).st_size == 0:
                writer.writerow(new_a.keys())
            writer.writerow(new_a.values())
            
        return {"success": True, "id": new_id_arch}

@app.get("/api/admin/list_all")
def list_all_files(modulo: str, search: Optional[str] = "", type_filter: Optional[str] = ""):
    if modulo == "Archivo":
        df = get_archivo_df()
        if search:
            s = search.lower().strip()
            df = df[df["titulo"].str.lower().str.contains(s, na=False) | df["autor"].str.lower().str.contains(s, na=False)]
        if type_filter:
            df = df[df["doc_type"] == type_filter]
            
        records = df.to_dict(orient="records")
        for idx, r in enumerate(records):
            r["__idx"] = idx + 1
        return records
    else:
        df = get_rrhh_df()
        if search:
            s = search.lower().strip()
            df = df[df["empleado"].str.lower().str.contains(s, na=False) | df["cedula"].str.lower().str.contains(s, na=False)]
        if type_filter:
            df = df[df["doc_type"] == type_filter]
            
        records = df.to_dict(orient="records")
        for idx, r in enumerate(records):
            r["__idx"] = idx + 1
        return records

@app.post("/api/admin/add_category")
def add_category(req: CategoryCreateRequest):
    log_event(req.usuario, "Create Category", req.scope, f"Nueva Tipología: {req.name}")
    # Guardar mínimo en un archivo si existiese tipologías, sino solo registrar en auditoría.
    # Como la tipología se extrae dinámicamente, al registrar un documento con ella aparecerá de inmediato.
    return {"success": True}

@app.get("/api/admin/users")
def get_users_list():
    df = get_users_df()
    df_clean = df.copy()
    if "password" in df_clean.columns:
        df_clean["password"] = "••••••••"
    return df_clean.to_dict(orient="records")

@app.post("/api/admin/users/create")
def create_user(req: UserCreateRequest):
    df = get_users_df()
    if req.usuario.strip() in df["usuario"].values:
        raise HTTPException(status_code=400, detail="Usuario ya existe")
        
    new_user = {
        "usuario": req.usuario.strip(),
        "password": req.password.strip(),
        "modulo": req.modulo,
        "rol": req.rol
    }
    
    file_exists = os.path.exists(CSV_USERS)
    with open(CSV_USERS, mode="a", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        if not file_exists or os.stat(CSV_USERS).st_size == 0:
            writer.writerow(new_user.keys())
        writer.writerow(new_user.values())
        
    log_event(req.creator, "Create User", req.modulo, f"Nuevo: {req.usuario} ({req.rol})")
    return {"success": True}

# Montar archivos estáticos
static_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "static")
if not os.path.exists(static_path):
    os.makedirs(static_path)

# Rutas para forzar el uso de la UI original en los endpoints principales
@app.get("/", include_in_schema=False)
async def root():
    return RedirectResponse(url="/login")

@app.get("/login", include_in_schema=False)
async def serve_login():
    return FileResponse(os.path.join(static_path, "login.html"))

@app.get("/archivo", include_in_schema=False)
async def serve_archivo():
    return FileResponse(os.path.join(static_path, "archivo.html"))

@app.get("/rrhh", include_in_schema=False)
async def serve_rrhh():
    return FileResponse(os.path.join(static_path, "rrhh.html"))

@app.get("/admin/archivo", include_in_schema=False)
async def serve_admin_archivo():
    return FileResponse(os.path.join(static_path, "admin_archivo.html"))

@app.get("/admin/rrhh", include_in_schema=False)
async def serve_admin_rrhh():
    return FileResponse(os.path.join(static_path, "admin_rrhh.html"))

app.mount("/static", StaticFiles(directory=static_path), name="static")
app.mount("/assets", StaticFiles(directory="static/assets"), name="assets")

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="127.0.0.1", port=8000)
