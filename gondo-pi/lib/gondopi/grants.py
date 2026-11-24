"""Grant store: (kind, resource) -> scope + expiry, persisted per project for 'always'."""
from __future__ import annotations

import fnmatch
import json
import os
import threading
import time

from .paths import PROJECTS_DIR, write_private

ONCE, WINDOW, ALWAYS, DENY = "once", "window", "always", "deny"


class Grants:
    def __init__(self, project: str):
        self.path = os.path.join(PROJECTS_DIR, project, "grants.json")
        self.lock = threading.Lock()
        self.session: dict[str, dict] = {}   # key -> {"scope", "expires", "label"}
        self.persist: dict[str, dict] = {}
        try:
            self.persist = json.load(open(self.path))
        except (OSError, ValueError):
            pass

    @staticmethod
    def key(kind: str, resource: str) -> str:
        return f"{kind}:{resource}"

    def check(self, kind: str, resource: str) -> str | None:
        """'allow' / 'deny' / None (= ask)."""
        k = self.key(kind, resource)
        now = time.time()
        with self.lock:
            for store in (self.session, self.persist):
                for pat, g in list(store.items()):
                    if pat == k or fnmatch.fnmatchcase(k, pat):
                        if g.get("expires") and g["expires"] < now:
                            del store[pat]
                            continue
                        return "deny" if g["scope"] == DENY else "allow"
        return None

    def add(self, kind: str, resource: str, scope: str, secs: int | None = None, label: str = "") -> None:
        k = self.key(kind, resource)
        g = {"scope": scope, "label": label or resource, "at": time.time()}
        if scope == WINDOW and secs:
            g["expires"] = time.time() + secs
        if scope == DENY and secs:
            g["expires"] = time.time() + secs
        with self.lock:
            if scope == ONCE:
                return
            if scope == ALWAYS:
                self.persist[k] = g
                write_private(self.path, json.dumps(self.persist, indent=2))
            else:
                self.session[k] = g

    def revoke(self, key: str) -> bool:
        with self.lock:
            hit = self.session.pop(key, None) is not None
            if key in self.persist:
                del self.persist[key]
                write_private(self.path, json.dumps(self.persist, indent=2))
                hit = True
            return hit

    def active(self) -> list[dict]:
        now = time.time()
        out = []
        with self.lock:
            for src, store in (("session", self.session), ("project", self.persist)):
                for k, g in store.items():
                    if g.get("expires") and g["expires"] < now:
                        continue
                    left = int(g["expires"] - now) if g.get("expires") else None
                    out.append({"key": k, "scope": g["scope"], "left": left, "where": src})
        return sorted(out, key=lambda x: x["key"])

    def next_expiry(self, kind: str) -> float | None:
        now = time.time()
        with self.lock:
            exps = [g["expires"] for k, g in self.session.items()
                    if k.startswith(kind + ":") and g.get("expires") and g["expires"] > now]
        return max(exps) if exps else None


class Audit:
    def __init__(self, path: str):
        os.makedirs(os.path.dirname(path), mode=0o700, exist_ok=True)
        self.f = open(path, "a", buffering=1)
        os.chmod(path, 0o600)
        self.lock = threading.Lock()
        self.recent: list[str] = []
        self.events: list[dict] = []      # structured, for the panel

    def log(self, event: str, **kw) -> None:
        rec = {"t": round(time.time(), 3), "event": event, **kw}
        with self.lock:
            self.f.write(json.dumps(rec) + "\n")
            line = f"{time.strftime('%H:%M:%S')} {event} " + " ".join(f"{k}={v}" for k, v in kw.items())
            self.recent = (self.recent + [line])[-50:]
            self.events = (self.events + [rec])[-200:]


def fmt_left(secs: int | None) -> str:
    if secs is None:
        return "always"
    h, m = divmod(secs // 60, 60)
    return f"{h}h{m:02d}m" if h else f"{m}m"
