import os

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles

from database import ensure_audit_table
from utils import populate_missing_slugs
from routes.auth    import router as auth_router
from routes.archivo import router as archivo_router
from routes.rrhh    import router as rrhh_router
from routes.admin   import router as admin_router
from routes.choices import router as choices_router
from routes.pages   import router as pages_router

# =============================================================================
# APLICACION
# =============================================================================

app = FastAPI(title="Ciencias UCV Digital Archive", version="3.0.0-Neon")

# CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Sin cacheo de estaticos en el cliente
@app.middleware("http")
async def add_no_cache_header(request, call_next):
    response = await call_next(request)
    response.headers["Cache-Control"] = "no-store, no-cache, must-revalidate, max-age=0"
    return response


# =============================================================================
# STARTUP
# =============================================================================

@app.on_event("startup")
def on_startup():
    ensure_audit_table()
    populate_missing_slugs()


# =============================================================================
# ROUTERS
# =============================================================================

app.include_router(auth_router)
app.include_router(archivo_router)
app.include_router(rrhh_router)
app.include_router(admin_router)
app.include_router(choices_router)
app.include_router(pages_router)


# =============================================================================
# ARCHIVOS ESTATICOS
# =============================================================================

static_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "static")
if os.path.exists(static_path):
    app.mount("/static", StaticFiles(directory=static_path), name="static")
    assets_path = os.path.join(static_path, "assets")
    if os.path.exists(assets_path):
        app.mount("/assets", StaticFiles(directory=assets_path), name="assets")


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="127.0.0.1", port=8000)
