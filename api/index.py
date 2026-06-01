import os
import sys

# Añadir el directorio base al PATH de Python para resolver correctamente las importaciones
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from python_app.main import app
from mangum import Mangum

# Adaptador ASGI para que Vercel Serverless entienda FastAPI
handler = Mangum(app, lifespan="off")
