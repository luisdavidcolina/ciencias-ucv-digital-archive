"""Cache en memoria con TTL. Usado principalmente por choices.py."""
import time
from typing import Any, Optional


class TTLCache:
    def __init__(self, ttl_seconds: int = 300):
        self._store: dict = {}
        self._ttl = ttl_seconds

    def get(self, key: str) -> Optional[Any]:
        entry = self._store.get(key)
        if entry and (time.time() - entry["ts"]) < self._ttl:
            return entry["val"]
        return None

    def set(self, key: str, value: Any) -> None:
        self._store[key] = {"val": value, "ts": time.time()}

    def invalidate(self, key: str = None) -> None:
        if key:
            self._store.pop(key, None)
        else:
            self._store.clear()
