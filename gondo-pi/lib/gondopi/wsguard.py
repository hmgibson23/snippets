"""Workspace guard (trusted side): files in the project that make HOST tools run commands.

/workspace is your project, read-write from the VM. A few files in it are executed by tools
that run on the host (your own git, direnv, mise, editors), so something in the VM could plant
a command there that runs outside the VM later:

  .git/config     core.fsmonitor, core.hooksPath, core.sshCommand, core.editor/pager,
                  filter.*.clean/smudge/process, diff.*.textconv/command, merge.*.driver,
                  credential.helper, include.path/includeIf, alias.* (! shell aliases), ...
  .git/hooks/*    run by host git on commit / checkout / push
  .envrc          direnv, .mise.toml / .tool-versions tasks, .vscode/tasks.json, ...

Rules:
  * dangerous .git/config keys are reverted to the session-start value (and audited)
  * new or changed executable hooks are moved to .git/gondo-pi-quarantine/ (audited)
  * changes to the other trigger files are reported in the panel (not reverted: they are
    normal project files) so you can look before running direnv/mise on the host
"""
from __future__ import annotations

import hashlib
import os
import re
import subprocess
import threading
import time

GIT_DANGEROUS = re.compile(
    r"^(core\.(fsmonitor|hookspath|sshcommand|editor|pager|askpass|gitproxy|alternaterefsCommand)|"
    r"sequence\.editor|gpg\.(program|\w+\.program)|filter\..*\.(clean|smudge|process)|"
    r"diff\..*\.(textconv|command)|diff\.external|merge\..*\.driver|credential\..*|"
    r"include\.path|includeif\..*|alias\..*|uploadpack\.packobjectshook|"
    r"receive\.procreceiverefs|safe\.directory|http\..*\.(sslcert|sslkey|cookiefile)|"
    r"remote\..*\.(uploadpack|receivepack|vcs)|url\..*\.(insteadof|pushinsteadof))$", re.I)
REPORT_FILES = (".envrc", ".mise.toml", "mise.toml", ".tool-versions", ".vscode/tasks.json",
                ".vscode/settings.json", ".idea/workspace.xml", "package.json", "Makefile",
                ".pre-commit-config.yaml", ".nvim.lua", ".exrc", ".lazy.lua")


def _git_config(path: str) -> list[tuple[str, str]]:
    if not os.path.exists(path):
        return []
    r = subprocess.run(["git", "config", "--file", path, "--list"], capture_output=True, text=True)
    out = []
    for line in r.stdout.splitlines():
        k, _, v = line.partition("=")
        out.append((k.lower(), v))
    return out


def _sha(path: str) -> str | None:
    try:
        with open(path, "rb") as f:
            return hashlib.sha256(f.read()).hexdigest()
    except OSError:
        return None


class WorkspaceGuard:
    def __init__(self, root: str, audit, notify=None, interval: float = 3.0):
        self.root, self.audit, self.notify, self.interval = root, audit, notify, interval
        self.events: list[dict] = []
        self._stop = threading.Event()
        self.git_dirs = self._git_dirs()
        self.base_cfg = {g: [kv for kv in _git_config(os.path.join(g, "config")) if GIT_DANGEROUS.match(kv[0])]
                         for g in self.git_dirs}
        self.base_hooks = {g: self._hooks(g) for g in self.git_dirs}
        self.base_files = {f: _sha(os.path.join(root, f)) for f in REPORT_FILES}

    # repos: the project itself and repos one level down (e.g. ~/Projects/newsuk/<repo>)
    def _git_dirs(self) -> list[str]:
        out = []
        for d in [self.root] + [os.path.join(self.root, x) for x in self._ls(self.root)]:
            g = os.path.join(d, ".git")
            if os.path.isdir(g):
                out.append(g)
        return out

    @staticmethod
    def _ls(d: str) -> list[str]:
        try:
            return [x for x in os.listdir(d) if not x.startswith(".") and os.path.isdir(os.path.join(d, x))]
        except OSError:
            return []

    @staticmethod
    def _hooks(gitdir: str) -> dict[str, str | None]:
        hd = os.path.join(gitdir, "hooks")
        try:
            names = [n for n in os.listdir(hd) if not n.endswith(".sample")]
        except OSError:
            return {}
        return {n: _sha(os.path.join(hd, n)) for n in names if os.path.isfile(os.path.join(hd, n))}

    def _event(self, kind: str, **kw) -> None:
        e = {"t": time.time(), "kind": kind, **kw}
        self.events.append(e)
        del self.events[:-50]
        self.audit.log("workspace-" + kind, **kw)
        if self.notify:
            self.notify(e)

    def check(self) -> None:
        for g in list(self.git_dirs):
            repo = os.path.relpath(os.path.dirname(g), self.root) if os.path.dirname(g) != self.root else "."
            cfg = os.path.join(g, "config")
            now = [kv for kv in _git_config(cfg) if GIT_DANGEROUS.match(kv[0])]
            if now != self.base_cfg.get(g, []):
                added = [kv for kv in now if kv not in self.base_cfg.get(g, [])]
                self._restore_cfg(cfg, g)
                self._event("git-config-reverted", repo=repo, keys=",".join(k for k, _ in added)[:200])
            hooks = self._hooks(g)
            for name, h in hooks.items():
                if self.base_hooks.get(g, {}).get(name) != h:
                    q = os.path.join(g, "gondo-pi-quarantine")
                    os.makedirs(q, exist_ok=True)
                    dst = os.path.join(q, f"{name}.{int(time.time())}")
                    try:
                        os.replace(os.path.join(g, "hooks", name), dst)
                        os.chmod(dst, 0o600)
                    except OSError:
                        continue
                    self._event("git-hook-quarantined", repo=repo, hook=name, moved_to=os.path.relpath(dst, self.root))
            self.base_hooks[g] = self._hooks(g)
        for f, h in self.base_files.items():
            now = _sha(os.path.join(self.root, f))
            if now != h:
                self.base_files[f] = now
                self._event("trigger-file-changed", file=f, state="removed" if now is None else "changed")

    def _restore_cfg(self, cfg: str, gitdir: str) -> None:
        """Drop dangerous keys that weren't there at session start; restore ones that changed."""
        base = self.base_cfg.get(gitdir, [])
        for k, v in [kv for kv in _git_config(cfg) if GIT_DANGEROUS.match(kv[0])]:
            if (k, v) not in base:
                subprocess.run(["git", "config", "--file", cfg, "--unset-all", k], capture_output=True)
        for k, v in base:
            have = [kv for kv in _git_config(cfg) if kv[0] == k]
            if (k, v) not in have:
                subprocess.run(["git", "config", "--file", cfg, "--add", k, v], capture_output=True)

    def run(self) -> None:
        while not self._stop.wait(self.interval):
            try:
                self.check()
            except Exception as e:  # never take the supervisor down
                self.audit.log("workspace-guard-error", err=f"{type(e).__name__}: {e}"[:200])

    def start(self) -> None:
        threading.Thread(target=self.run, daemon=True).start()

    def stop(self) -> None:
        self._stop.set()
