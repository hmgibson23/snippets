"""Trusted prompts. In a nono-pi zellij session: queued in the nono-pi tab
(answered there, no pop-ups). Elsewhere (nono-pi-key in a plain terminal):
/dev/tty, else an OS dialog.

Only the supervisor (trusted side) calls this. The agent cannot reach the
zellij socket or dialog tools, so it cannot fake or answer a prompt.

Two kinds:
  choose(title, body, options, default) -> option key or None
  secret(title, body) -> str or None      (hidden input)
"""
from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
import tempfile
import threading

from .paths import LINUX, MAC, RUN_DIR

_lock = threading.Lock()   # one prompt at a time: no stacking/confusion
PROMPT_TIMEOUT = 300


def _zellij_session() -> str | None:
    return os.environ.get("NONO_PI_ZELLIJ_SESSION")


def _osascript_choose(title: str, body: str, labels: list[str], default: str) -> str | None:
    script = ('on run argv\n'
              '  set opts to items 3 thru -1 of argv\n'
              '  set r to choose from list opts with title (item 1 of argv) with prompt (item 2 of argv) '
              'default items {item 3 of argv} OK button name "OK" cancel button name "Deny"\n'
              '  if r is false then return ""\n'
              '  return item 1 of r\n'
              'end run')
    ordered = [default] + [l for l in labels if l != default]
    r = subprocess.run(["/usr/bin/osascript", "-e", script, title, body, *ordered],
                       capture_output=True, text=True, timeout=PROMPT_TIMEOUT)
    v = r.stdout.strip()
    return v or None


def _osascript_secret(title: str, body: str) -> str | None:
    script = ('on run argv\n'
              '  set r to display dialog (item 2 of argv) default answer "" with hidden answer '
              'with title (item 1 of argv) buttons {"Cancel", "OK"} default button "OK" with icon caution '
              'giving up after 300\n'
              '  if gave up of r then error number -128\n'
              '  return text returned of r\n'
              'end run')
    r = subprocess.run(["/usr/bin/osascript", "-e", script, title, body], capture_output=True, text=True,
                       timeout=PROMPT_TIMEOUT + 10)
    return r.stdout.rstrip("\n") if r.returncode == 0 else None


def _zenity_secret(title, body):
    for cmd in (["zenity", "--password", f"--title={title}"], ["kdialog", "--title", title, "--password", body]):
        if shutil.which(cmd[0]):
            r = subprocess.run(cmd, capture_output=True, text=True, timeout=PROMPT_TIMEOUT)
            return r.stdout.rstrip("\n") if r.returncode == 0 else None
    return None


def _zenity_choose(title, body, labels, default):
    if shutil.which("zenity"):
        args = ["zenity", "--list", "--radiolist", f"--title={title}", f"--text={body}", "--column=", "--column=Choice"]
        for l in labels:
            args += ["TRUE" if l == default else "FALSE", l]
        r = subprocess.run(args, capture_output=True, text=True, timeout=PROMPT_TIMEOUT)
        return r.stdout.strip() or None
    return None


def _tty(spec: dict) -> dict | None:
    try:
        tty = open("/dev/tty", "r+")
    except OSError:
        return None
    with tty:
        tty.write(f"\n🔒 nono-pi: {spec['title']}\n{spec.get('body', '')}\n")
        if spec["kind"] == "secret":
            import getpass
            try:
                return {"value": getpass.getpass("> ", stream=tty)}
            except (EOFError, KeyboardInterrupt):
                return None
        opts = spec["options"]
        for i, (k, label) in enumerate(opts, 1):
            tty.write(f"  {i}) {label}{'  [default]' if k == spec['default'] else ''}\n")
        tty.write("> ")
        tty.flush()
        ans = tty.readline().strip()
        if not ans:
            return {"choice": spec["default"]}
        try:
            return {"choice": opts[int(ans) - 1][0]}
        except (ValueError, IndexError):
            return None


def _has_gui() -> bool:
    if MAC:
        return not os.environ.get("SSH_CONNECTION")
    return bool(os.environ.get("DISPLAY") or os.environ.get("WAYLAND_DISPLAY"))


# Set by the supervisor when running inside zellij with the control panel.
QUEUE = None            # pending.Pending
PANEL_TIMEOUT = 900     # a request waits this long in the nono-pi tab, then counts as denied
ON_QUEUE = None         # callback(count) -> nudge the status line


def _via_panel(spec: dict) -> tuple[bool, dict | None]:
    """In a zellij session every request waits in the nono-pi tab (no pop-ups):
    the bar shows "N waiting · Alt-2", you answer there. Returns (handled, answer)."""
    if QUEUE is None:
        return False, None
    i = QUEUE.add(spec)
    try:
        if ON_QUEUE:
            ON_QUEUE(QUEUE.count())
        got, ans = QUEUE.wait(i, PANEL_TIMEOUT)
        return True, (ans if got else None)
    finally:
        QUEUE.done(i)
        if ON_QUEUE:
            ON_QUEUE(QUEUE.count())


def ask(spec: dict) -> dict | None:
    if os.environ.get("NONO_PI_PROMPT_AUTO"):        # tests: canned answers
        import json as _j
        return _j.loads(os.environ["NONO_PI_PROMPT_AUTO"]).get(spec.get("id", spec["kind"]))
    if _zellij_session() and QUEUE is not None:
        handled, ans = _via_panel(spec)   # several requests can wait in the panel at once
        if handled:
            return ans
    with _lock:
        # outside a nono-pi zellij session (e.g. `nono-pi-key` in a plain terminal)
        if sys.stdin.isatty() or os.path.exists("/dev/tty"):
            r = _tty(spec)
            if r is not None or not _has_gui():
                return r
        if _has_gui():
            if spec["kind"] == "secret":
                v = _osascript_secret(spec["title"], spec.get("body", "")) if MAC else _zenity_secret(spec["title"], spec.get("body", ""))
                return {"value": v} if v else None
            labels = [label for _, label in spec["options"]]
            dlabel = dict(spec["options"])[spec["default"]]
            pick = (_osascript_choose if MAC else _zenity_choose)(spec["title"], spec.get("body", ""), labels, dlabel)
            if pick is None:
                return {"choice": "deny"} if MAC else None
            return {"choice": next(k for k, l in spec["options"] if l == pick)}
        return None


def choose(title: str, body: str, options: list[tuple[str, str]], default: str, pid: str = "") -> str | None:
    r = ask({"kind": "choose", "id": pid or "choose", "title": title, "body": body,
             "options": options, "default": default})
    return r.get("choice") if r else None


def secret(title: str, body: str, pid: str = "") -> str | None:
    r = ask({"kind": "secret", "id": pid or "secret", "title": title, "body": body})
    v = r.get("value") if r else None
    return v or None
