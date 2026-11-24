"""nono-pi control panel: a trusted TUI in the zellij tab "nono-pi".

Catppuccin-macchiato curses UI, styled like the zjstatus bar. Tabs:
  1 Overview  keychain / ssh key / AWS / pending / recent activity at a glance
  2 Requests  queued decisions + secret prompts (Okta password, 6-digit code,
              API tokens) answered right here in a modal
  3 Grants    active grants with time left, revoke
  4 Network   allowlist, add / remove live
  5 AWS       accounts with login state; Enter logs in and shows progress
  6 Secrets   stored secret names (never values); add / replace / delete
  7 Log       audit trail
Global: Alt-1 / Alt-2 (zellij) switch shell <-> nono-pi; here: ←/→ or 1-7 tabs, ↑/↓ or j/k,
u unlock, L lock now, P deny-all-new 10 min, q back to pi.

Trusted side only: talks to the supervisor over ctl.sock; the agent can reach
neither ctl.sock nor zellij, so it cannot drive this panel.
"""
from __future__ import annotations

import curses
import locale
import threading
import time

from . import proto
from .grants import fmt_left

TABS = ["Overview", "Requests", "Grants", "Network", "AWS", "Secrets", "Log"]
ICON = {"Overview": "\uf015", "Requests": "\uf0f3", "Grants": "\uf084", "Network": "\uf0ac",
        "AWS": "\uf0c2", "Secrets": "\uf023", "Log": "\uf03a"}

# catppuccin macchiato (hex -> nearest xterm-256 when the terminal can't redefine colours)
PALETTE = {
    "crust": (0x18, 0x19, 0x26), "mantle": (0x1e, 0x20, 0x30), "base": (0x24, 0x27, 0x3a),
    "surface0": (0x36, 0x3a, 0x4f), "surface1": (0x49, 0x4d, 0x64), "overlay0": (0x6e, 0x73, 0x8d),
    "subtext": (0xa5, 0xad, 0xcb), "text": (0xca, 0xd3, 0xf5), "blue": (0x8a, 0xad, 0xf4),
    "peach": (0xf5, 0xa9, 0x7f), "green": (0xa6, 0xda, 0x95), "yellow": (0xee, 0xd4, 0x9f),
    "red": (0xed, 0x87, 0x96), "mauve": (0xc6, 0xa0, 0xf6), "teal": (0x8b, 0xd5, 0xca),
    "lavender": (0xb7, 0xbd, 0xf8),
}


def _xterm256(rgb: tuple[int, int, int]) -> int:
    """Nearest xterm-256 colour index."""
    def cube(v):
        return 0 if v < 48 else 1 if v < 115 else (v - 35) // 40
    r, g, b = rgb
    ci = 16 + 36 * cube(r) + 6 * cube(g) + cube(b)
    levels = [0, 95, 135, 175, 215, 255]
    cr = (levels[cube(r)], levels[cube(g)], levels[cube(b)])
    gray = round((r + g + b) / 3)
    gi = 232 + max(0, min(23, (gray - 8) // 10))
    gv = 8 + 10 * (gi - 232)
    d = lambda a: sum((x - y) ** 2 for x, y in zip(a, rgb))
    return ci if d(cr) <= d((gv, gv, gv)) else gi


def call(op: dict, timeout: float = 30) -> dict:
    try:
        return proto.call(proto.CTL, op, timeout=timeout)
    except OSError as e:
        return {"ok": False, "error": f"supervisor not reachable ({e.__class__.__name__})"}


class Theme:
    def __init__(self):
        self.pairs: dict[tuple[str, str], int] = {}
        self.idx = {}
        curses.start_color()
        curses.use_default_colors()
        n = 16
        for name, rgb in PALETTE.items():
            if curses.can_change_color() and curses.COLORS >= 256:
                curses.init_color(n, *(int(v * 1000 / 255) for v in rgb))
                self.idx[name] = n
                n += 1
            else:
                self.idx[name] = _xterm256(rgb) if curses.COLORS >= 256 else -1

    def __call__(self, fg: str, bg: str | None = None, *attrs) -> int:
        key = (fg, bg or "")
        if key not in self.pairs:
            pid = len(self.pairs) + 1
            curses.init_pair(pid, self.idx.get(fg, -1), self.idx.get(bg, -1) if bg else -1)
            self.pairs[key] = pid
        a = curses.color_pair(self.pairs[key])
        for x in attrs:
            a |= x
        return a


class Panel:
    def __init__(self, scr):
        self.s = scr
        self.t = Theme()
        self.tab = 0
        self.sel = 0
        self.msg, self.msg_kind, self.msg_at = "", "info", 0.0
        self.st: dict = {"ok": False, "error": "connecting…"}
        self.busy: dict | None = None     # {"label", "since"} for long ops (AWS login)
        self.lock = threading.Lock()
        curses.curs_set(0)
        scr.timeout(250)
        scr.keypad(True)

    # ------------------------------------------------------------ data
    def refresh(self):
        st = call({"op": "panel-state"}, timeout=5)
        with self.lock:
            self.st = st

    def items(self) -> list:
        st, t = self.st, TABS[self.tab]
        if t == "Requests":
            return st.get("pending", [])
        if t == "Grants":
            return st.get("grants", [])
        if t == "Network":
            return [{"host": h} for h in st.get("allow", [])]
        if t == "AWS":
            return st.get("aws", [])
        if t == "Secrets":
            return [{"name": n} for n in st.get("secrets", [])]
        if t == "Log":
            return list(reversed(st.get("events", [])))
        return []

    def flash(self, text: str, kind: str = "info"):
        self.msg, self.msg_kind, self.msg_at = text, kind, time.time()

    def run_bg(self, label: str, op: dict, ok_text, timeout: float = 600):
        self.busy = {"label": label, "since": time.time()}

        def work():
            r = call(op, timeout=timeout)
            self.busy = None
            if r.get("ok"):
                self.flash(ok_text(r) if callable(ok_text) else ok_text, "ok")
            else:
                self.flash(f"{label}: {r.get('error', 'failed or cancelled')}", "err")
            self.refresh()
        threading.Thread(target=work, daemon=True).start()

    # ------------------------------------------------------------ primitives
    def put(self, y, x, text, attr=0, maxw=None):
        h, w = self.s.getmaxyx()
        if y < 0 or y >= h or x >= w:
            return 0
        maxw = (w - x - (1 if y == h - 1 else 0)) if maxw is None else min(maxw, w - x)
        if maxw <= 0:
            return 0
        text = text[:maxw]
        try:
            self.s.addstr(y, x, text, attr)
        except curses.error:
            pass
        return len(text)

    def pill(self, y, x, icon, text, color, active=True):
        """zjstatus-style pill: (icon| text )"""
        t = self.t
        body = "surface0" if active else "mantle"
        n = self.put(y, x, "\ue0b6", t(color))
        n += self.put(y, x + n, f"{icon} ", t("crust", color, curses.A_BOLD))
        n += self.put(y, x + n, f" {text} ", t("text" if active else "overlay0", body, curses.A_BOLD if active else 0))
        n += self.put(y, x + n, "\ue0b4", t(body))
        return n

    def fill(self, y, attr):
        h, w = self.s.getmaxyx()
        self.put(y, 0, " " * (w - (1 if y == h - 1 else 0)), attr)

    def box(self, y, x, hgt, wid, color, title=""):
        t = self.t(color)
        self.put(y, x, "╭" + "─" * (wid - 2) + "╮", t)
        for i in range(1, hgt - 1):
            self.put(y + i, x, "│", t)
            self.put(y + i, x + wid - 1, "│", t)
        self.put(y + hgt - 1, x, "╰" + "─" * (wid - 2) + "╯", t)
        if title:
            self.put(y, x + 2, f" {title} ", self.t(color, None, curses.A_BOLD))

    # ------------------------------------------------------------ modal input (hidden for secrets)
    def ask(self, title: str, body: str = "", hidden: bool = False) -> str | None:
        h, w = self.s.getmaxyx()
        bw = min(70, w - 4)
        lines = [l for l in body.splitlines() if l.strip()][:6]
        bh = 6 + len(lines)
        y0, x0 = max(1, (h - bh) // 2), max(1, (w - bw) // 2)
        buf = ""
        self.s.timeout(-1)
        curses.curs_set(1)
        try:
            while True:
                for i in range(bh):
                    self.put(y0 + i, x0, " " * bw, self.t("text", "mantle"))
                self.box(y0, x0, bh, bw, "peach" if hidden else "blue", ("\uf023 " if hidden else "") + title)
                for i, l in enumerate(lines):
                    self.put(y0 + 1 + i, x0 + 2, l[: bw - 4], self.t("subtext", "mantle"))
                fy = y0 + 2 + len(lines)
                shown = ("•" * len(buf)) if hidden else buf
                self.put(fy, x0 + 2, " " * (bw - 4), self.t("text", "surface0"))
                self.put(fy, x0 + 2, (" " + shown)[-(bw - 5):], self.t("text", "surface0", curses.A_BOLD))
                self.put(fy + 2, x0 + 2, "enter confirm · esc cancel" + (" · input is hidden" if hidden else ""),
                         self.t("overlay0", "mantle"))
                self.s.move(fy, min(x0 + 3 + len(shown), x0 + bw - 3))
                self.s.refresh()
                ch = self.s.get_wch()
                if ch in ("\n", "\r", curses.KEY_ENTER):
                    return buf
                if ch == "\x1b":
                    return None
                if ch in (curses.KEY_BACKSPACE, "\x7f", "\b"):
                    buf = buf[:-1]
                elif ch == "\x15":       # ctrl-u
                    buf = ""
                elif isinstance(ch, str) and ch.isprintable():
                    buf += ch
        finally:
            curses.curs_set(0)
            self.s.timeout(250)

    # ------------------------------------------------------------ actions
    def act(self, key: str):
        t = TABS[self.tab]
        items = self.items()
        cur = items[self.sel] if 0 <= self.sel < len(items) else None
        r = None
        if t == "Requests" and cur:
            if cur.get("kind") == "secret":
                if key in ("\n", "\r", " "):
                    v = self.ask(cur.get("title", "secret"), cur.get("body", ""), hidden=True)
                    if v is not None:
                        r = call({"op": "answer", "id": cur["id"], "value": v})
                elif key == "d":
                    r = call({"op": "answer", "id": cur["id"], "value": None})
            else:
                opts = cur.get("options", [])
                pick = None
                if key in ("\n", "\r", " "):
                    pick = cur.get("default")
                elif key == "d":
                    pick = "deny"
                elif key.isdigit() and 1 <= int(key) <= len(opts):
                    pick = opts[int(key) - 1][0]
                if pick:
                    r = call({"op": "answer", "id": cur["id"], "choice": pick})
        elif t == "Grants" and cur and key in ("x", "\x7f"):
            r = call({"op": "revoke", "key": cur["key"]})
        elif t == "Network":
            if key == "a":
                h = self.ask("allow host", "e.g. api.example.com or *.example.com\napplies immediately")
                if h:
                    r = call({"op": "net-allow", "host": h.strip()})
            elif key in ("x", "\x7f") and cur:
                r = call({"op": "net-remove", "host": cur["host"]})
        elif t == "AWS" and cur:
            if key in ("\n", "\r", "l", "R"):
                force = key == "R"
                def done(r, name=cur["name"]):
                    who = (r.get("arn") or "").split(":assumed-role/")[-1].split("/")[0]
                    how = "existing session reused (R = fresh login)" if r.get("reused") else "logged in via Okta"
                    check = "" if r.get("verified", True) else " (unverified)"
                    return f"AWS {name}: {how} · {who or '?'} @ {r.get('account_id') or '?'}{check}"
                self.run_bg(f"AWS {'re-' if force else ''}login {cur['name']}",
                            {"op": "aws-login", "account": cur["name"], "force": force}, done)
            elif key == "o":
                r = call({"op": "aws-logout", "account": cur["name"]})
            elif key == "p":
                r = call({"op": "aws-pick", "account": cur["name"]})
        elif t == "Secrets":
            if key in ("s", "a"):
                n = self.ask("secret name", "letters, digits, _ . : -")
                if n:
                    v = self.ask(f"value for {n}", "stored in the nono-pi keychain; never shown to the agent", hidden=True)
                    if v:
                        r = call({"op": "secret-set", "name": n, "value": v})
            elif key in ("x", "\x7f") and cur:
                r = call({"op": "secret-rm", "name": cur["name"]})
        if key == "u":
            pw = self.ask("unlock nono-pi keychain", hidden=True)
            if pw:
                r = call({"op": "unlock", "password": pw})
                if r and not r.get("ok"):
                    r = {"ok": False, "error": "wrong password"}
        elif key == "L":
            r = call({"op": "lock"})
        elif key == "P":
            r = call({"op": "panic"})
        if r is not None:
            self.flash("done" if r.get("ok") else r.get("error", "failed"), "ok" if r.get("ok") else "err")
            self.refresh()

    # ------------------------------------------------------------ drawing
    TOP = 4        # first content row (title, tabs, rule, spacer)

    def draw_header(self):
        t, st = self.t, self.st
        h, w = self.s.getmaxyx()
        # row 0: title (left) · status chips (right)
        x = 2
        x += self.put(0, x, "\uf023 nono-pi", t("mauve", None, curses.A_BOLD))
        x += self.put(0, x, "  ·  ", t("overlay0"))
        x += self.put(0, x, st.get("project", "?"), t("text", None, curses.A_BOLD))
        if st.get("vm"):
            x += self.put(0, x, "  ·  VM", t("teal", None, curses.A_BOLD))
        if self.busy:
            spin = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"[int(time.time() * 8) % 10]
            self.put(0, x + 3, f"{spin} {self.busy['label']} {int(time.time() - self.busy['since'])}s",
                     t("yellow", None, curses.A_BOLD))
        chips = []
        unlocked = st.get("keychain_unlocked")
        chips.append(("\uf09c unlocked" if unlocked else "\uf023 locked", "green" if unlocked else "red"))
        n = len(st.get("pending", []))
        if n:
            chips.append((f"\uf0f3 {n} waiting", "yellow"))
        rx = w - 2
        for text, c in reversed(chips):
            rx -= len(text) + 3
            self.put(0, rx, f" {text} ", t("crust", c, curses.A_BOLD))
        # row 1: tabs (no boxes): active = peach + underline, others = subtext
        x = 2
        for i, name in enumerate(TABS):
            label = f"{i + 1} {name}"
            if name == "Requests" and n:
                label += f" ({n})"
            if i == self.tab:
                attr = t("peach", None, curses.A_BOLD | curses.A_UNDERLINE)
            elif name == "Requests" and n:
                attr = t("yellow", None, curses.A_BOLD)
            else:
                attr = t("subtext")
            x += self.put(1, x, label, attr) + 4
        # row 2: thin rule
        self.put(2, 1, "─" * (w - 3), t("surface1"))

    def draw_footer(self):
        t = self.t
        h, w = self.s.getmaxyx()
        tab_keys = {"Overview": [], "Requests": [("⏎", "allow"), ("1-5", "choose"), ("d", "deny")],
                    "Grants": [("x", "revoke")], "Network": [("a", "add host"), ("x", "remove")],
                    "AWS": [("⏎", "log in"), ("R", "re-login"), ("o", "log out"), ("p", "pre-approve")],
                    "Secrets": [("s", "add"), ("x", "delete")], "Log": []}[TABS[self.tab]]
        common = [("←→", "tabs"), ("u", "unlock"), ("L", "lock"), ("P", "deny new 10m"), ("Alt-1", "shell")]
        # toast (above the hints)
        if self.msg and time.time() - self.msg_at < 8:
            color = {"ok": "green", "err": "red", "info": "blue"}[self.msg_kind]
            icon = {"ok": "\uf00c", "err": "\uf00d", "info": "\uf05a"}[self.msg_kind]
            self.put(h - 3, 2, f" {icon}  {self.msg} ", t("crust", color, curses.A_BOLD))
        self.put(h - 2, 1, "─" * (w - 3), t("surface1"))
        x = 2
        for i, (k, d) in enumerate(tab_keys + ([("│", "")] if tab_keys else []) + common):
            if k == "│":
                x += self.put(h - 1, x, "│  ", t("surface1"))
                continue
            x += self.put(h - 1, x, k, t("lavender", None, curses.A_BOLD))
            x += self.put(h - 1, x, f" {d}   ", t("overlay0"))

    def row(self, y, i, cols, color="text"):
        """A list row; selected rows get a highlight bar + ▌ marker."""
        t = self.t
        h, w = self.s.getmaxyx()
        sel = i == self.sel
        bg = "surface0" if sel else None
        self.put(y, 1, " " * (w - 3), t("text", bg) if bg else 0)
        self.put(y, 1, "▌" if sel else " ", t("peach", bg))
        x = 3
        for text, c, width, bold in cols:
            self.put(y, x, text.ljust(width)[:width] if width else text, t(c, bg, curses.A_BOLD) if bold else t(c, bg))
            x += (width or len(text)) + 1

    def draw_overview(self):
        t, st = self.t, self.st
        h, w = self.s.getmaxyx()
        y = self.TOP
        unlocked = st.get("keychain_unlocked")
        ssh = st.get("ssh_key_loaded_for") or 0
        aws = st.get("aws") or []
        live = [a for a in aws if a.get("session_left")]
        picked = [a for a in aws if a.get("picked")]
        n = len(st.get("pending", []))
        pushes = [g["key"].split(":", 1)[1].split("/", 1)[-1] for g in st.get("grants", [])
                  if g["key"].startswith("git-push:") and g["scope"] != "deny"]
        if live:
            a = live[0]
            aws_v = a["name"]
            aws_s = f"{a.get('role', '')} @ {a.get('account_id', '')} · {fmt_left(a['session_left'])} left"
            aws_c = "green"
        elif picked:
            aws_v, aws_s, aws_c = picked[0]["name"], "not logged in — Enter on tab 5", "yellow"
        else:
            aws_v, aws_s, aws_c = "off", "no account picked", "overlay0"
        signed = st.get("signed", 0)
        cards = [
            ("\uf09c" if unlocked else "\uf023", "Keychain", "unlocked" if unlocked else "locked",
             "auto-locks after the window" if unlocked else "press u to unlock", "green" if unlocked else "red"),
            ("\uf084", "SSH key", f"{fmt_left(ssh)} left" if ssh else "not loaded",
             "in nono-pi's private agent" if ssh else "loads on first push / commit", "green" if ssh else "overlay0"),
            ("\uf0c2", "AWS", aws_v, aws_s, aws_c),
            ("\uf0f3", "Requests", f"{n} waiting" if n else "none",
             "Enter on tab 2" if n else "all caught up", "yellow" if n else "green"),
            ("\uf062", "Push grants", (f"{len(pushes)} repo" + ("" if len(pushes) == 1 else "s")) if pushes else "none",
             ", ".join(pushes) if pushes else "first push to a repo asks", "blue"),
            ("\uf044", "Signed", f"{signed} commit" + ("" if signed == 1 else "s"),
             "automatic, namespace git", "lavender"),
        ]
        cols = 3 if w >= 110 else (2 if w >= 70 else 1)
        gap = 2
        cw = (w - 4 - gap * (cols - 1)) // cols
        for i, (icon, title, value, sub, color) in enumerate(cards):
            cx = 2 + (i % cols) * (cw + gap)
            cy = y + (i // cols) * 5
            self.box(cy, cx, 4, cw, "surface1")
            self.put(cy, cx + 2, f" {icon} {title} ", t(color, None, curses.A_BOLD))
            self.put(cy + 1, cx + 2, value[: cw - 4], t("text", None, curses.A_BOLD))
            self.put(cy + 2, cx + 2, sub[: cw - 4], t("overlay0"))
        yy = y + ((len(cards) + cols - 1) // cols) * 5
        self.put(yy, 2, "\uf03a Activity", t("subtext", None, curses.A_BOLD))
        self.put(yy, 14, "─" * max(0, w - 17), t("surface1"))
        rows = max(0, h - yy - 4)
        for j, ev in enumerate(list(reversed(st.get("events", [])))[:rows]):
            self.event_row(yy + 1 + j, ev, w)

    EVENT_STYLE = {
        "git": ("\uf126", "blue"), "sign": ("\uf044", "lavender"), "decision": ("\uf0e3", "peach"),
        "aws-login": ("\uf0c2", "green"), "aws-login-failed": ("\uf0c2", "red"), "aws-logout": ("\uf0c2", "overlay0"),
        "ssh-key-loaded": ("\uf084", "green"), "ssh-key-expired": ("\uf084", "overlay0"),
        "secret-use": ("\uf023", "mauve"), "secret-stored": ("\uf023", "mauve"),
        "keychain-unlock": ("\uf09c", "green"), "keychain-lock": ("\uf023", "overlay0"),
        "net-allow": ("\uf0ac", "teal"), "net-remove": ("\uf0ac", "overlay0"), "net-allowlist": ("\uf0ac", "teal"),
        "auto-deny": ("\uf05e", "red"), "panic": ("\uf05e", "red"),
    }

    def event_text(self, ev: dict) -> str:
        e = ev.get("event", "")
        if e == "git":
            return f"{ev.get('op')}  {ev.get('repo', '')}"
        if e == "sign":
            return f"signed commit  {ev.get('what', '')}"
        if e == "decision":
            return f"{ev.get('kind')}  {ev.get('resource')}  →  {ev.get('choice')}"
        if e in ("aws-login", "aws-logout", "aws-login-failed"):
            return f"{e.replace('-', ' ')}  {ev.get('account', '')}"
        if e == "ssh-key-loaded":
            return f"ssh key loaded for {fmt_left(int(ev.get('life', 0)))}"
        if e in ("net-allow", "net-remove", "net-allowlist"):
            return f"{e.replace('-', ' ')}  {ev.get('host', '')}"
        rest = "  ".join(f"{k}={v}" for k, v in ev.items() if k not in ("t", "event"))
        return f"{e}  {rest}".strip()

    def event_row(self, y: int, ev: dict, w: int):
        t = self.t
        icon, color = self.EVENT_STYLE.get(ev.get("event", ""), ("\uf111", "overlay0"))
        ts = time.strftime("%H:%M:%S", time.localtime(ev.get("t", 0)))
        self.put(y, 4, ts, t("overlay0"))
        self.put(y, 14, icon, t(color))
        self.put(y, 17, self.event_text(ev)[: max(0, w - 20)], t("text"))

    def draw_requests(self):
        t = self.t
        h, w = self.s.getmaxyx()
        items = self.items()
        if not items:
            self.put(self.TOP + 1, 4, "\uf00c  nothing waiting", t("green", None, curses.A_BOLD))
            self.put(self.TOP + 3, 4, "requests from pi (push, new host, AWS account, secrets, Okta code) appear here", t("overlay0"))
            return
        y = self.TOP
        for i, it in enumerate(items):
            sel = i == self.sel
            body = [l for l in it.get("body", "").splitlines() if l.strip()][:5]
            hgt = 4 + len(body)
            if y + hgt > h - 2:
                self.put(y, 4, f"… {len(items) - i} more", t("overlay0"))
                break
            color = "peach" if sel else ("yellow" if it.get("kind") != "secret" else "mauve")
            self.box(y, 2, hgt, w - 4, color, ("\uf023 " if it.get("kind") == "secret" else "\uf071 ") + it.get("title", ""))
            for j, l in enumerate(body):
                self.put(y + 1 + j, 4, l[: w - 8], t("subtext"))
            oy = y + 1 + len(body) + (1 if body else 0)
            if it.get("kind") == "secret":
                self.put(oy, 4, " ⏎ enter value ", t("crust", "peach" if sel else "mauve", curses.A_BOLD))
                self.put(oy, 21, " d cancel ", t("crust", "surface1"))
            else:
                x = 4
                for j, (k, lbl) in enumerate(it.get("options", [])):
                    is_def = k == it.get("default")
                    c = ("green" if is_def else "red" if k in ("deny", "panic") else "blue")
                    x += self.put(oy, x, f" {j + 1} {lbl}{' ⏎' if is_def else ''} ",
                                  t("crust", c, curses.A_BOLD) if (sel and is_def) or is_def else t(c, "surface0")) + 1
            y += hgt + 1

    def draw_list(self):
        t, tab = self.t, TABS[self.tab]
        h, w = self.s.getmaxyx()
        items = self.items()
        self.sel = max(0, min(self.sel, len(items) - 1)) if items else 0
        y = self.TOP
        heads = {"Grants": [("grant", 58), ("left", 9), ("where", 8)], "Network": [("allowed host (others ask)", 60)],
                 "AWS": [("account", 32), ("role @ id", 30), ("session", 16), ("", 12)], "Secrets": [("name (values are never shown)", 60)],
                 "Log": [("time      event", 0)]}[tab]
        x = 3
        for name, width in heads:
            self.put(y, x, name.upper(), t("overlay0", None, curses.A_BOLD))
            x += width + 1
        y += 1
        if not items:
            empty = {"Grants": "no active grants", "Network": "empty", "AWS": "no AWS accounts configured",
                     "Secrets": "none stored (or keychain locked — press u)", "Log": "nothing yet"}[tab]
            self.put(y + 1, 4, empty, t("overlay0"))
            return
        start = max(0, self.sel - (h - y - 4))
        for i, it in list(enumerate(items))[start: start + h - y - 3]:
            if tab == "Grants":
                ok = it["scope"] != "deny"
                cols = [(("✓ " if ok else "✗ ") + it["key"], "green" if ok else "red", 58, True),
                        (fmt_left(it["left"]), "text", 9, False), (it["where"], "overlay0", 8, False)]
            elif tab == "Network":
                cols = [(it["host"], "text", 60, False)]
            elif tab == "AWS":
                live = it.get("session_left")
                state = f"✓ {fmt_left(live)}" if live else "— not logged in"
                tag = ("● picked " if it.get("picked") else "") + ("PROD" if it.get("prod") else "")
                cols = [(it["name"], "text", 32, True),
                        (f"{it.get('role', '?')} @ {it.get('account_id', '?')}", "subtext", 30, False),
                        (state, "green" if live else "overlay0", 16, bool(live)),
                        (tag, "red" if it.get("prod") else "blue", 12, False)]
            elif tab == "Secrets":
                cols = [("\uf084 " + it["name"], "text", 60, False)]
            else:
                self.event_row(y, it, w)
                y += 1
                continue
            self.row(y, i, cols)
            y += 1

    def draw(self):
        self.s.erase()
        h, w = self.s.getmaxyx()
        if h < 10 or w < 50:
            self.put(0, 0, "nono-pi: make this pane bigger", self.t("yellow"))
            self.s.refresh()
            return
        self.draw_header()
        if not self.st.get("ok"):
            self.put(4, 4, f"\uf071  {self.st.get('error', 'supervisor not running')}", self.t("red", None, curses.A_BOLD))
        elif TABS[self.tab] == "Overview":
            self.draw_overview()
        elif TABS[self.tab] == "Requests":
            self.draw_requests()
        else:
            self.draw_list()
        self.draw_footer()
        self.s.refresh()

    # ------------------------------------------------------------ loop
    def run(self):
        last = 0.0
        jumped = False
        while True:
            if time.time() - last > 1.0:
                self.refresh()
                last = time.time()
                pend = self.st.get("pending")
                if pend and not jumped:           # new request: show it
                    self.tab, self.sel, jumped = 1, 0, True
                if not pend:
                    jumped = False
            self.draw()
            try:
                ch = self.s.get_wch()
            except curses.error:
                continue
            n = len(self.items())
            if ch == "q":
                call({"op": "panel-hide"})
            elif ch in (curses.KEY_RIGHT, "\t", "l") and not (TABS[self.tab] == "AWS" and ch == "l"):
                self.tab, self.sel = (self.tab + 1) % len(TABS), 0
            elif ch in (curses.KEY_LEFT, curses.KEY_BTAB, "h"):
                self.tab, self.sel = (self.tab - 1) % len(TABS), 0
            elif ch in (curses.KEY_DOWN, "j"):
                self.sel = min(self.sel + 1, max(0, n - 1))
            elif ch in (curses.KEY_UP, "k"):
                self.sel = max(0, self.sel - 1)
            elif isinstance(ch, str) and ch in "1234567" and not (TABS[self.tab] == "Requests" and self.items()):
                self.tab, self.sel = int(ch) - 1, 0
            elif ch == curses.KEY_RESIZE:
                pass
            elif isinstance(ch, str):
                self.act(ch)
                last = 0.0


def main() -> None:
    locale.setlocale(locale.LC_ALL, "")
    import os
    os.environ.setdefault("ESCDELAY", "25")
    curses.wrapper(lambda scr: Panel(scr).run())
