"""Push gondo-pi state into the zjstatus bar ({pipe_gondopi} slot) via `zellij pipe`.

Runs on the trusted side (supervisor thread). Uses zjstatus's line protocol:
    zjstatus::pipe::pipe_gondopi::<text with #[...] colours>
"""
from __future__ import annotations

import os
import subprocess
import threading
import time

from .grants import fmt_left

# catppuccin macchiato
CRUST, SURF0, TEXT, OVL = "#181926", "#363a4f", "#cad3f5", "#6e738d"
GREEN, YELLOW, RED, MAUVE, PEACH = "#a6da95", "#eed49f", "#ed8796", "#c6a0f6", "#f5a97f"


def _seg(color: str, icon: str, text: str) -> str:
    return (f"#[bg={CRUST},fg={color}]\ue0b6#[bg={color},fg={CRUST},bold]{icon} "
            f"#[bg={SURF0},fg={TEXT},bold] {text} #[bg={CRUST},fg={SURF0}]\ue0b4#[bg={CRUST}]  ")


def render(st: dict) -> str:
    n = st.get("pending_count", 0)
    if n:
        return _seg(YELLOW, "\uf071", f"{n} waiting · Alt-2")
    cur = st.get("aws_current")
    live = st.get("aws_logged_in") or {}
    if cur:
        aws_txt = "\uf0c2 " + cur + (f" ✓{fmt_left(live[cur])}" if live.get(cur) else " ✗")
    else:
        aws_txt = "\uf0c2 –  (Alt-a)"
    lock = "\uf09c" if st.get("keychain_unlocked") else "\uf023"   # unlock / lock
    color = GREEN if st.get("keychain_unlocked") else MAUVE
    pushes = sum(1 for g in st.get("grants", []) if g["key"].startswith("git-push:") and g["scope"] != "deny")
    extra = f" · ⇡{pushes}" if pushes else ""
    vm = " VM" if st.get("vm") else ""
    return _seg(color, lock, f"gondo-pi{vm} · {aws_txt}{extra}")


class BarPusher:
    def __init__(self, sup, session: str):
        self.sup, self.session = sup, session
        self.last = None
        self.kick = threading.Event()

    def push_now(self) -> None:
        self.kick.set()

    def _send(self, text: str) -> None:
        subprocess.run(["zellij", "-s", self.session, "pipe", f"zjstatus::pipe::pipe_gondopi::{text}"],
                       stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=5)

    def run(self) -> None:
        while True:
            try:
                text = render(self.sup.status())
                # resend periodically too: a newly attached client / reloaded plugin starts empty
                if text != self.last or int(time.time()) % 30 < 3:
                    self._send(text)
                    self.last = text
            except Exception:
                pass
            self.kick.wait(3)
            self.kick.clear()

    def start(self) -> None:
        threading.Thread(target=self.run, daemon=True).start()


def maybe_start(sup) -> BarPusher | None:
    sess = os.environ.get("GONDO_PI_ZELLIJ_SESSION")
    if not sess or not os.environ.get("GONDO_PI_ZJSTATUS"):
        return None
    b = BarPusher(sup, sess)
    b.start()
    return b
