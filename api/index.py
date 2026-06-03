import os
import sys

# Añadir el subdirectorio de la aplicación al PATH de Python
# Esto permite que "import database", "from routes.auth", etc. se importen correctamente sin romper la estructura
base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(base_dir, "python-app"))
sys.path.insert(0, base_dir)

from main import app
from mangum import Mangum

# Adaptador ASGI para que Vercel Serverless entienda FastAPI
handler = Mangum(app, lifespan="off")
