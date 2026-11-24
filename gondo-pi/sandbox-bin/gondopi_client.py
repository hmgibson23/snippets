"""Client side of the gondo-pi broker (runs INSIDE the agent tier).

Holds no credentials. Talks to the supervisor over:
  - unix socket  $PI_BROKER           (nono mode)
  - tcp host:port + token                  (VM mode: PI_BROKER=tcp://host:port, PI_BROKER_TOKEN)
"""
import json
import os
import socket


def connect():
    addr = os.environ.get("PI_BROKER", "")
    if addr.startswith("tcp://"):
        host, _, port = addr[6:].rpartition(":")
        s = socket.create_connection((host, int(port)))
    else:
        s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        s.connect(addr)
    return s


def request(obj):
    """Returns (socket, file, first-response)."""
    tok = os.environ.get("PI_BROKER_TOKEN")
    if tok:
        obj = dict(obj, token=tok)
    s = connect()
    s.sendall((json.dumps(obj) + "\n").encode())
    f = s.makefile("rb", buffering=0)
    line = b""
    while not line.endswith(b"\n"):
        c = f.read(1)
        if not c:
            break
        line += c
    return s, f, (json.loads(line) if line else {"ok": False, "error": "broker closed"})


def call(obj):
    s, _, r = request(obj)
    s.close()
    return r
