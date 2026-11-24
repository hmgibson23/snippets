"""Network/credential policy for the VM (trusted side). Replaces the old nono profile.

Produces the `net`, `routes` and `aws` parts of the vmd spec (enforced by
vm/netpolicy.mjs) and the guest-side AWS config with placeholder keys. Nothing
here is secret: placeholders are random per session and worthless outside it;
real tokens are fetched by vmd from the supervisor per request.
"""
from __future__ import annotations

import os
import secrets

from .paths import SANDBOX_DIR, write_private

# GitHub API paths that get the gh token. NOT /copilot_internal/**: pi's Copilot
# provider brings its own token there (we only ever replace our placeholder anyway).
GITHUB_EXCLUDE = ["/copilot_internal"]


def placeholder_key(i: int) -> str:
    return f"NONOPI{i:014d}"


# The guest's [default] profile: "whatever account is current" (`aws-use` / the panel switch
# it on the host; vmd resolves this key per request). Explicit --profile X keeps its own key.
CURRENT_KEY = placeholder_key(0)


_SESSION_PLACEHOLDERS: dict[str, str] = {}   # stable for the life of the process (= the session)


def placeholder(name: str) -> str:
    if name not in _SESSION_PLACEHOLDERS:
        _SESSION_PLACEHOLDERS[name] = f"nonopi_{name}_{secrets.token_hex(12)}"
    return _SESSION_PLACEHOLDERS[name]


def routes(cfg: dict) -> tuple[dict, dict]:
    """(routes for vmd, guest env with placeholders)."""
    out, env = {}, {}
    for name, svc in cfg["services"].items():
        up = svc["upstream"].rstrip("/")
        host = up.split("://", 1)[1].split("/", 1)[0].split(":", 1)[0]
        ph = placeholder(name)
        r = {"host": host, "placeholder": ph, "format": svc.get("format", "Bearer {}"),
             "header": svc.get("header", "authorization").lower()}
        if "paths" in svc:
            r["paths"] = svc["paths"]
        if svc.get("source") == "gh-token":
            r["excludePaths"] = GITHUB_EXCLUDE
        out[name] = r
        env[svc.get("env_var", name.upper() + "_TOKEN")] = ph
    return out, env


def aws(cfg: dict, session: dict) -> tuple[dict, dict, str | None]:
    """(aws spec for vmd, guest env, guest aws-config text)."""
    a = cfg["aws"]
    accounts = a.get("accounts", {})
    if not a.get("enabled") or not accounts:
        return {"enabled": False}, {}, None
    profiles, lines = {}, []
    for i, (name, acct) in enumerate(sorted(accounts.items()), start=1):
        key = placeholder_key(i)
        profiles[key] = name
        lines += [f"[profile {name}]", f"region = {acct.get('region', 'eu-west-2')}",
                  f"aws_access_key_id = {key}", "aws_secret_access_key = nono-pi-placeholder-not-a-secret", ""]
    # region of [default] is only a starting point: the re-signer uses the request's real region
    region = accounts.get(a.get("default_account", ""), {}).get("region", "eu-west-2")
    lines = ["[default]", f"region = {region}", f"aws_access_key_id = {CURRENT_KEY}",
             "aws_secret_access_key = nono-pi-placeholder-not-a-secret", ""] + lines
    env = {"AWS_SHARED_CREDENTIALS_FILE": "/dev/null", "AWS_EC2_METADATA_DISABLED": "true",
           "AWS_SDK_UA_APP_ID": "nono-pi",
           # SDKs default to chunked-signed uploads; vmd re-signs whole bodies, so ask for that
           "AWS_REQUEST_CHECKSUM_CALCULATION": "when_required",
           "AWS_RESPONSE_CHECKSUM_VALIDATION": "when_required"}
    return {"enabled": True, "profiles": profiles, "currentKey": CURRENT_KEY}, env, "\n".join(lines)


def build(cfg: dict, session: dict) -> tuple[dict, dict]:
    """(policy part of the vmd spec, guest env additions). Writes the guest aws config."""
    r, env = routes(cfg)
    aspec, aenv, aws_cfg = aws(cfg, session)
    env.update(aenv)
    path = os.path.join(SANDBOX_DIR, "aws-config")
    if aws_cfg:
        write_private(path, aws_cfg)
    elif os.path.exists(path):
        os.unlink(path)
    net = cfg["network"]
    spec = {"net": {"allow": sorted(set(net.get("allow", []) + session.get("allow_hosts", []))),
                    "ask": bool(net.get("ask", True))},
            "routes": r, "aws": aspec}
    return spec, env
