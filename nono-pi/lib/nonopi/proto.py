"""Wire protocol between nono-pi components.

Unix stream socket, one JSON object per line (request), one JSON object per
line (response). For streaming ops (git/ssh) the first response line is
{"ok": true, "stream": true}; after that the socket carries the raw ssh
protocol both ways.

Sockets (all under STATE_DIR/run, created 0700, sockets 0600):
  ctl.sock     host-side only (helpers, pinentry, nono approval webhook)
  broker.sock  connect-only from the sandbox (git/ssh/sign/requests)
"""
from __future__ import annotations

import json
import os
import socket

from .paths import RUN_DIR

CTL = os.path.join(RUN_DIR, "ctl.sock")
BROKER = os.path.join(RUN_DIR, "broker.sock")
MAX_LINE = 1 << 20


def connect(path: str, timeout: float | None = None) -> socket.socket:
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    if timeout:
        s.settimeout(timeout)
    s.connect(path)
    return s


def send(s: socket.socket, obj: dict) -> None:
    s.sendall((json.dumps(obj, separators=(",", ":")) + "\n").encode())


def recv(f) -> dict | None:
    line = f.readline(MAX_LINE)
    if not line:
        return None
    return json.loads(line)


def call(path: str, obj: dict, timeout: float | None = 600) -> dict:
    """One request, one response."""
    with connect(path, timeout) as s:
        send(s, obj)
        f = s.makefile("rb")
        r = recv(f)
        return r if r is not None else {"ok": False, "error": "no response"}
