"""Desktop notifications (trusted side) for requests that wait too long in the panel.

No pop-ups *inside* zellij (the panel is the only place you answer). But if a request has
been waiting ~30s and you're not looking at the session, a desktop notification tells you:
  Linux (niri, any notification daemon): notify-send with an "Open" action that focuses the
      terminal window running the session (niri) and switches zellij to the gondo-pi tab
  macOS: a notification via osascript; clicking it focuses the terminal app
The notification text is the request's title only (host / repo / account), never a secret.
"""
from __future__ import annotations

import json
import os
import shutil
import subprocess
import threading
import time

from .paths import MAC

DELAY = 30


def _niri_env() -> dict:
    env = dict(os.environ)
    rt = env.get("XDG_RUNTIME_DIR") or f"/run/user/{os.getuid()}"
    env.setdefault("XDG_RUNTIME_DIR", rt)
    if not env.get("NIRI_SOCKET"):
        try:
            socks = sorted(f for f in os.listdir(rt) if f.startswith("niri.") and f.endswith(".sock"))
            if socks:
                env["NIRI_SOCKET"] = os.path.join(rt, socks[-1])
        except OSError:
            pass
    if not env.get("DBUS_SESSION_BUS_ADDRESS") and os.path.exists(os.path.join(rt, "bus")):
        env["DBUS_SESSION_BUS_ADDRESS"] = f"unix:path={rt}/bus"
    return env


def _niri_focus(session: str, env: dict) -> bool:
    """Focus the window showing the zellij session: app_id/title match newsuk / the session."""
    if not shutil.which("niri") or not env.get("NIRI_SOCKET"):
        return False
    try:
        wins = json.loads(subprocess.run(["niri", "msg", "--json", "windows"], env=env,
                                         capture_output=True, text=True, timeout=5).stdout or "[]")
    except (OSError, ValueError, subprocess.SubprocessError):
        return False
    project = session.removeprefix("pi-").rsplit("-", 1)[0]
    def score(w):
        t, a = (w.get("title") or "").lower(), (w.get("app_id") or "").lower()
        return (session.lower() in t) * 4 + (project.lower() in t or project.lower() == a) * 2 + ("kitty" in a or "ghostty" in a or a == project.lower())
    best = max(wins, key=score, default=None)
    if not best or score(best) == 0:
        return False
    subprocess.run(["niri", "msg", "action", "focus-window", "--id", str(best["id"])], env=env,
                   capture_output=True, timeout=5)
    return True


def _show_linux(title: str, body: str, session: str) -> None:
    env = _niri_env()
    if not shutil.which("notify-send"):
        return
    p = subprocess.run(["notify-send", "--app-name=gondo-pi", "--urgency=normal", "--icon=dialog-password",
                        "--action=open=Open", "--expire-time=60000", f"gondo-pi: {title}", body],
                       env=env, capture_output=True, text=True, timeout=120)
    if p.stdout.strip() == "open":
        _niri_focus(session, env)
        subprocess.run(["zellij", "-s", session, "action", "go-to-tab-name", "gondo-pi"],
                       capture_output=True, timeout=5)


def _show_mac(title: str, body: str, session: str) -> None:
    def esc(x):
        return x.replace("\\", "\\\\").replace('"', '\\"')
    subprocess.run(["/usr/bin/osascript", "-e",
                    f'display notification "{esc(body)}" with title "gondo-pi" subtitle "{esc(title)}"'],
                   capture_output=True, timeout=10)


class Notifier:
    """Watches the pending queue; one notification per request that waits > DELAY seconds."""

    def __init__(self, sup, session: str):
        self.sup, self.session = sup, session
        self.done: set = set()

    def _panel_visible(self) -> bool:
        # the panel polls panel-state ~1/s while it is drawn; zellij only draws the active tab,
        # so a recent poll means you're probably looking at it
        return time.time() - getattr(self.sup, "panel_seen", 0) < 2

    def run(self) -> None:
        while True:
            time.sleep(5)
            try:
                for p in self.sup.pending.list():
                    pid = p.get("id")
                    if pid in self.done or time.time() - p.get("at", time.time()) < DELAY or self._panel_visible():
                        continue
                    self.done.add(pid)
                    title = str(p.get("title", "a request"))[:120]
                    body = "waiting for you in the gondo-pi tab (Alt-2)"
                    show = _show_mac if MAC else _show_linux
                    threading.Thread(target=show, args=(title, body, self.session), daemon=True).start()
                    self.sup.audit.log("notified", what=title[:80])
            except Exception:
                pass

    def start(self) -> None:
        if os.environ.get("GONDO_PI_NOTIFY", "1") != "0":
            threading.Thread(target=self.run, daemon=True).start()
