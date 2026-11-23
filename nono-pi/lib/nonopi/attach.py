"""Thin pty client for the VM daemon (trusted side, one per zellij pane).

Connects to RUN_DIR/vmd.sock, puts the local terminal in raw mode and relays
bytes + window size. Nothing secret flows here: it is your terminal <-> a pty
inside the microVM.
"""
from __future__ import annotations

import json
import os
import select
import shutil
import signal
import socket
import struct
import sys
import termios
import time
import tty

from . import proto
from .vm import VMD_SOCK

I, R, E, O, X = b"i", b"r", b"e", b"o", b"x"
DIM, RST = "\033[38;2;110;115;141m", "\033[0m"


def _frame(t: bytes, body: bytes) -> bytes:
    return t + struct.pack(">I", len(body)) + body


def wait_for_vm(timeout: float = 180) -> bool:
    spin, t0, shown = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏", time.time(), False
    while time.time() - t0 < timeout:
        if os.path.exists(VMD_SOCK):
            try:
                s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
                s.connect(VMD_SOCK)
                s.close()
                if shown:
                    sys.stderr.write("\r\033[K")
                return True
            except OSError:
                pass
        if sys.stderr.isatty() and time.time() - t0 > 0.3:
            shown = True
            sys.stderr.write(f"\r{DIM}{spin[int(time.time() * 10) % 10]} starting the nono-pi VM…{RST}")
            sys.stderr.flush()
        time.sleep(0.1)
    return False


def run(kind: str, args: list[str] | None = None) -> tuple[int, str]:
    """One session in the VM. Returns (exit code, reason): reason is 'exit',
    'vm-restart' (VM recreated: reconnect), 'vm-stop' or 'lost'."""
    fd_in, fd_out = sys.stdin.fileno(), sys.stdout.fileno()
    is_tty = os.isatty(fd_in)
    rows, cols = shutil.get_terminal_size((120, 40)).lines, shutil.get_terminal_size((120, 40)).columns
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.connect(VMD_SOCK)
    s.sendall((json.dumps({"op": "exec", "kind": kind, "args": args or [], "rows": rows, "cols": cols,
                           "tty": is_tty, "term": os.environ.get("TERM", "xterm-256color")}) + "\n").encode())
    buf = b""
    while b"\n" not in buf:
        d = s.recv(65536)
        if not d:
            return 1, "lost"
        buf += d
    line, buf = buf.split(b"\n", 1)
    hello = json.loads(line)
    if not hello.get("ok"):
        sys.stderr.write(f"nono-pi: {hello.get('error', 'VM refused the session')}\n")
        return 1, "exit"

    def on_winch(*_):
        sz = shutil.get_terminal_size((cols, rows))
        try:
            s.sendall(_frame(R, json.dumps({"rows": sz.lines, "cols": sz.columns}).encode()))
        except OSError:
            pass

    old = termios.tcgetattr(fd_in) if is_tty else None
    prev = signal.signal(signal.SIGWINCH, on_winch) if is_tty else None
    code, reason = 1, "lost"
    try:
        if is_tty:
            tty.setraw(fd_in)
        stdin_open = True
        while True:
            # drain complete frames
            while len(buf) >= 5:
                n = struct.unpack(">I", buf[1:5])[0]
                if len(buf) < 5 + n:
                    break
                t, body, buf = buf[:1], buf[5:5 + n], buf[5 + n:]
                if t == O:
                    os.write(fd_out, body)
                elif t == X:
                    r = json.loads(body)
                    return int(r.get("code", 0)), r.get("reason", "exit")
            rl = [s] + ([fd_in] if stdin_open else [])
            try:
                ready, _, _ = select.select(rl, [], [])
            except InterruptedError:
                continue
            if s in ready:
                d = s.recv(262144)
                if not d:
                    return code, reason
                buf += d
            if fd_in in ready:
                d = os.read(fd_in, 65536)
                if d:
                    s.sendall(_frame(I, d))
                else:
                    stdin_open = False
                    s.sendall(_frame(E, b""))
    finally:
        if is_tty:
            termios.tcsetattr(fd_in, termios.TCSADRAIN, old)
            signal.signal(signal.SIGWINCH, prev)
        s.close()


def shell_loop(main: bool = False) -> int:
    """A fish shell in the VM; reconnects (fresh shell) when the VM is recreated."""
    if not wait_for_vm():
        sys.stderr.write("nono-pi: the VM is not running (quit and rerun `pis`)\n")
        return 1
    while True:
        code, reason = run("shell")
        if reason == "vm-restart":
            sys.stderr.write(f"\r\n{DIM}⬡ the VM was restarted for new access — fresh shell, /workspace is unchanged{RST}\r\n")
            wait_for_vm()
            continue
        return code


def pi_loop(pi_args: list[str]) -> int:
    """pi in the VM. Restarts with --continue after new access (exit 75) or a VM
    restart; after a normal quit it waits for Enter so the tab stays useful."""
    from .launcher import RESTART_EXIT, _drop_initial_prompts
    args, first = list(pi_args), True
    while True:
        if not wait_for_vm():
            sys.stderr.write("nono-pi: the VM is not running\n")
            return 1
        if not first and not any(a in args for a in ("-c", "--continue", "--session", "-r", "--resume")):
            args = ["--continue"] + _drop_initial_prompts(args)
        first = False
        code, reason = run("pi", args)
        if reason == "vm-stop" or reason == "lost" and not os.path.exists(VMD_SOCK):
            return 0
        if reason == "vm-restart":
            continue
        if code == RESTART_EXIT:
            try:
                r = proto.call(proto.CTL, {"op": "vm-apply"}, timeout=180)
                why = (r.get("applied") or {}).get("why", "")
            except OSError:
                why = ""
            sys.stderr.write(f"\r\n{DIM}nono-pi: restarting pi (same session){': ' + why if why else ''}…{RST}\r\n")
            continue
        sys.stderr.write(f"\r\n{DIM}pi exited{f' (status {code})' if code else ''}. "
                         f"Enter = start pi again (same session) · your shell is tab 1{RST}\r\n")
        try:
            input()
        except (EOFError, KeyboardInterrupt):
            return code
        args = list(pi_args)
        first = False
