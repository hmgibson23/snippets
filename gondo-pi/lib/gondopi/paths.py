"""gondo-pi shared paths and config (trusted side only)."""
from __future__ import annotations

import json
import os
import platform
import shutil

HOME = os.path.expanduser("~")
MAC = platform.system() == "Darwin"
LINUX = platform.system() == "Linux"

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))  # ~/snippets/gondo-pi
BIN = os.path.join(ROOT, "bin")
SANDBOX_BIN = os.path.join(ROOT, "sandbox-bin")   # the only gondo-pi code visible inside the sandbox
PI_EXT = os.path.join(ROOT, "pi-extension")
VM_DIR = os.path.join(ROOT, "vm")

_CFG_BASE = os.environ.get("XDG_CONFIG_HOME", os.path.join(HOME, ".config"))
_STATE_BASE = os.path.join(HOME, ".local", "state")


def _migrate(base: str, old: str, new: str) -> None:
    """One-time rename from nono-pi: move the directory, leave a symlink at the old path."""
    o, n = os.path.join(base, old), os.path.join(base, new)
    if os.path.isdir(o) and not os.path.islink(o) and not os.path.exists(n):
        try:
            os.rename(o, n)
            os.symlink(new, o)
        except OSError:
            pass


if not os.environ.get("GONDO_PI_CONFIG"):
    _migrate(_CFG_BASE, "nono-pi", "gondo-pi")
if not os.environ.get("GONDO_PI_STATE"):
    _migrate(_STATE_BASE, "nono-pi", "gondo-pi")
CONFIG_DIR = os.environ.get("GONDO_PI_CONFIG") or os.path.join(_CFG_BASE, "gondo-pi")
STATE_DIR = os.environ.get("GONDO_PI_STATE") or os.path.join(_STATE_BASE, "gondo-pi")
HOST_DIR = os.path.join(STATE_DIR, "host")          # never readable from the sandbox
SANDBOX_DIR = os.path.join(STATE_DIR, "sandbox")    # read-only in the sandbox
PROJECTS_DIR = os.path.join(STATE_DIR, "projects")  # per-project remembered choices + grants
AUDIT_DIR = os.path.join(STATE_DIR, "audit")
# Sockets: the broker socket must be connectable from the sandbox, ctl must not.
RUN_DIR = os.path.join(STATE_DIR, "run")

DEFAULT_WINDOW_SECS = 4 * 3600


DEFAULT_CONFIG: dict = {
    "services": {
        "github": {"upstream": "https://api.github.com", "env_var": "GH_TOKEN",
                   "format": "token {}", "source": "gh-token"},
    },
    "aws": {"enabled": False, "default_account": "", "accounts": {}, "prod_accounts": []},
    "network": {
        # Hosts allowed without asking. Credential-route hosts (services, AWS) are
        # always reachable. Anything else asks via the supervisor.
        "allow": [
            "github.com", "*.github.com", "*.githubusercontent.com", "codeload.github.com",
            "registry.npmjs.org", "*.npmjs.org", "registry.yarnpkg.com", "nodejs.org",
            "pypi.org", "files.pythonhosted.org", "*.pythonhosted.org",
            "crates.io", "*.crates.io", "static.rust-lang.org", "proxy.golang.org", "sum.golang.org",
            "registry.terraform.io", "releases.hashicorp.com", "*.hashicorp.com",
            "objects.githubusercontent.com", "ghcr.io", "*.docker.io", "*.docker.com",
            "formulae.brew.sh", "*.amazonaws.com", "*.aws.amazon.com", "*.api.aws",
            # LLM providers pi talks to
            "api.anthropic.com", "api.openai.com", "chatgpt.com", "auth.openai.com",
            "*.githubcopilot.com", "api.deepseek.com", "openrouter.ai", "*.openrouter.ai",
            "opencode.ai", "*.opencode.ai", "generativelanguage.googleapis.com",
            "*.kiro.dev", "*.amazoncognito.com", "oidc.*.amazonaws.com",
            # pi itself (update check, package registry, docs)
            "pi.dev", "*.pi.dev", "models.dev",
            # neovim plugin tooling (Mason registry, treesitter parsers come from github)
            "api.mason-registry.dev",
        ],
        "ask": True,
    },
    "grants": {   # default windows in seconds
        "push": DEFAULT_WINDOW_SECS, "aws": DEFAULT_WINDOW_SECS, "net": DEFAULT_WINDOW_SECS,
        "fetch_auto": True, "sign_auto": True, "ssh_interactive": "ask",
    },
    "keychain": {"lock_after": DEFAULT_WINDOW_SECS},
    "ssh": {"keys": ["~/.ssh/id_ed25519"]},
    "zellij": True,
    "extra_read": [], "extra_allow": [],
}


def config_path() -> str:
    return os.path.join(CONFIG_DIR, "config.json")


def _merge(base: dict, over: dict) -> dict:
    out = dict(base)
    for k, v in over.items():
        out[k] = _merge(base[k], v) if isinstance(v, dict) and isinstance(base.get(k), dict) else v
    return out


# list settings where the user's entries ADD to gondo-pi's defaults (so a config that
# lists one extra host doesn't silently drop the whole default allowlist)
_ADDITIVE = {("network", "allow")}


def _merge_lists(base: dict, cfg: dict) -> dict:
    out = _merge(base, cfg)
    for sect, key in _ADDITIVE:
        user = (cfg.get(sect) or {}).get(key)
        if isinstance(user, list):
            out[sect][key] = list(dict.fromkeys(base[sect][key] + user))
    # defaults you removed in the panel
    deny = set((cfg.get("network") or {}).get("deny", []))
    out["network"]["allow"] = [h for h in out["network"]["allow"] if h not in deny]
    return out


def load_config() -> dict:
    path = config_path()
    if not os.path.exists(path):
        cfg = json.loads(json.dumps(DEFAULT_CONFIG))
        from . import awsdisc
        accounts = awsdisc.discover()
        if accounts and shutil.which("saml2aws"):
            old = os.path.join(HOME, ".pi", "agent", "extensions", "saml2aws-login.config.json")
            default, prod = "", []
            try:
                o = json.load(open(old))
                default, prod = o.get("defaultProfile", ""), o.get("prodAccounts", [])
            except (OSError, ValueError):
                pass
            cfg["aws"] = {"enabled": True, "accounts": accounts, "prod_accounts": prod,
                          "default_account": default if default in accounts else sorted(accounts)[0]}
        save_config(cfg)
    with open(path) as f:
        cfg = json.load(f)
    return _merge_lists(DEFAULT_CONFIG, cfg)


def save_config(cfg: dict) -> None:
    os.makedirs(CONFIG_DIR, mode=0o700, exist_ok=True)
    tmp = config_path() + ".tmp"
    with open(tmp, "w") as f:
        json.dump(cfg, f, indent=2)
        f.write("\n")
    os.replace(tmp, config_path())


def write_private(path: str, text: str, mode: int = 0o600) -> None:
    os.makedirs(os.path.dirname(path), mode=0o700, exist_ok=True)
    tmp = path + ".tmp"
    fd = os.open(tmp, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, mode)
    with os.fdopen(fd, "w") as f:
        f.write(text)
    os.replace(tmp, path)


def project_id(cwd: str) -> str:
    import hashlib
    real = os.path.realpath(cwd)
    return os.path.basename(real).replace(" ", "_")[:40] + "-" + hashlib.sha256(real.encode()).hexdigest()[:8]
