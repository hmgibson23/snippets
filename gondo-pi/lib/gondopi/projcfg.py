"""Per-project settings (trusted side).

Two places, merged in this order (later wins):
  ~/.config/gondo-pi/projects/<project-id>.json   yours, on the host: always trusted
  <project>/.gondo-pi.json                         in the repo: shareable with the team, BUT
                                                   writable from the VM, so it only takes
                                                   effect after you approve its exact content
                                                   (sha256 kept on the host; any change asks again)
Keys (all optional):
  "allow":   ["api.example.com", "*.cdn.example.com"]   extra allowed hosts
  "aws":     "nuk-digital-dev-architecture"             current AWS account at start
  "ports":   [3000, 5173]                                forward when they start listening
  "vm":      {"memory": "8G", "cpus": 6}
Anything else is ignored. The repo file can't add credentials or mounts.
"""
from __future__ import annotations

import hashlib
import json
import os
import re

from .paths import CONFIG_DIR, write_private

REPO_FILE = ".gondo-pi.json"
HOST_RE = re.compile(r"^(\*\.)?[a-z0-9.-]+\.[a-z]{2,}$")


def _clean(d: dict) -> dict:
    out = {}
    if isinstance(d.get("allow"), list):
        out["allow"] = [h.lower() for h in d["allow"] if isinstance(h, str) and HOST_RE.match(h.lower())][:50]
    if isinstance(d.get("aws"), str) and re.fullmatch(r"[A-Za-z0-9._-]{1,64}", d["aws"]):
        out["aws"] = d["aws"]
    if isinstance(d.get("ports"), list):
        out["ports"] = [int(p) for p in d["ports"] if isinstance(p, int) and 1024 <= p <= 65535][:20]
    if isinstance(d.get("vm"), dict):
        vm = {}
        if isinstance(d["vm"].get("memory"), str) and re.fullmatch(r"\d{1,3}G", d["vm"]["memory"]):
            vm["memory"] = d["vm"]["memory"]
        if isinstance(d["vm"].get("cpus"), int) and 1 <= d["vm"]["cpus"] <= 32:
            vm["cpus"] = d["vm"]["cpus"]
        if vm:
            out["vm"] = vm
    return out


def host_file(project: str) -> str:
    return os.path.join(CONFIG_DIR, "projects", f"{project}.json")


def _approved_path(project: str) -> str:
    return os.path.join(CONFIG_DIR, "projects", f"{project}.repo-approved.json")


def load(project: str, cwd: str, ask=None) -> tuple[dict, str | None]:
    """(settings, note). `ask(title, body) -> bool` approves a new/changed repo file."""
    out: dict = {}
    try:
        out = _clean(json.load(open(host_file(project))))
    except (OSError, ValueError):
        pass
    repo = os.path.join(cwd, REPO_FILE)
    note = None
    if os.path.isfile(repo):
        raw = open(repo, "rb").read()[:65536]
        digest = hashlib.sha256(raw).hexdigest()
        try:
            approved = json.load(open(_approved_path(project))).get("sha256")
        except (OSError, ValueError):
            approved = None
        try:
            settings = _clean(json.loads(raw))
        except ValueError:
            return out, f"{REPO_FILE}: not valid JSON, ignored"
        if digest != approved:
            if not ask:
                return out, f"{REPO_FILE} changed: not applied (approve it in the panel)"
            body = json.dumps(settings, indent=1)[:600]
            if not ask(f"to apply {REPO_FILE} from the project (it is writable from the VM)", body):
                return out, f"{REPO_FILE}: not approved, ignored"
            write_private(_approved_path(project), json.dumps({"sha256": digest}))
        for k, v in settings.items():
            if k == "allow":
                out["allow"] = sorted(set(out.get("allow", []) + v))
            else:
                out.setdefault(k, v)          # the host file wins over the repo file
    return out, note
