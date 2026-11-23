"""Generate the nono profile + sandbox-side files for one session (trusted side)."""
from __future__ import annotations

import json
import os
import sys

from . import proto
from .paths import BIN, CONFIG_DIR, HOME, HOST_DIR, PI_EXT, ROOT, RUN_DIR, SANDBOX_BIN, SANDBOX_DIR, STATE_DIR, write_private

HELPER = os.path.join(BIN, "nono-pi-helper")
PY = sys.executable

DENY_VARS = ["AWS_*", "*TOKEN*", "*SECRET*", "*PASSWORD*", "*PASSWD*", "*API_KEY*", "*APIKEY*",
             "*_KEY", "*CREDENTIAL*", "*COOKIE*", "SAML2AWS_*", "OP_SESSION_*", "VAULT_*",
             "SSH_AUTH_SOCK", "SSH_AGENT_PID", "ZELLIJ*", "GIT_ASKPASS", "SSH_ASKPASS*", "PI_NONO_CTL"]

DEFAULT_AWS_SERVICES = ["sts", "s3", "ec2", "ecs", "ecr", "api.ecr", "eks", "lambda", "logs", "monitoring",
                        "cloudformation", "ssm", "secretsmanager", "kms", "dynamodb", "sqs", "sns", "rds",
                        "elasticloadbalancing", "autoscaling", "route53resolver", "events", "states",
                        "apigateway", "acm", "elasticache", "es", "athena", "glue", "kinesis", "firehose",
                        "codebuild", "codepipeline", "ssm-incidents", "tagging", "bedrock", "bedrock-runtime",
                        "bedrock-agent", "bedrock-agent-runtime"]
GLOBAL_AWS = {"iam": ("https://iam.amazonaws.com", "us-east-1", "iam"),
              "s3_global": ("https://s3.amazonaws.com", "us-east-1", "s3"),
              "sts_global": ("https://sts.amazonaws.com", "us-east-1", "sts"),
              "route53": ("https://route53.amazonaws.com", "us-east-1", "route53"),
              "cloudfront": ("https://cloudfront.amazonaws.com", "us-east-1", "cloudfront"),
              "organizations": ("https://organizations.us-east-1.amazonaws.com", "us-east-1", "organizations")}
SERVICE_NAME = {"api.ecr": "ecr", "bedrock-runtime": "bedrock", "bedrock-agent": "bedrock",
                "bedrock-agent-runtime": "bedrock"}
AWS_DENY_OPS = ["AssumeRole", "AssumeRoleWithSAML", "AssumeRoleWithWebIdentity", "GetSessionToken",
                "GetFederationToken", "GetAuthorizationToken", "CreateAccessKey", "Unknown"]

# GitHub API paths that get the gh token. NOT /copilot_internal/**: pi's Copilot
# provider sends its own token there (a route would replace it -> 403).
_GH = ["user", "users", "repos", "orgs", "search", "graphql", "gists", "notifications", "issues", "rate_limit",
       "meta", "emojis", "events", "feeds", "licenses", "markdown", "octocat", "zen", "organizations",
       "repositories", "teams", "enterprises", "app", "apps", "applications", "installation",
       "marketplace_listing", "codes_of_conduct", "gitignore", "projects", "networks", "advisories",
       "classrooms", "assignments", "versions"]
GITHUB_API_RULES = [{"method": "*", "path": f"/{t}"} for t in _GH] + [{"method": "*", "path": f"/{t}/**"} for t in _GH]


def placeholder_key(i: int) -> str:
    return f"NONOPI{i:014d}"


def aws_files(cfg: dict) -> tuple[dict, str, str]:
    """Map ALL configured accounts (unpicked ones ask on first use in the supervisor)."""
    accounts = cfg["aws"].get("accounts", {})
    host_lines, child_lines, profiles = [], [], {}
    for i, (name, acct) in enumerate(sorted(accounts.items()), start=1):
        region = acct.get("region", "eu-west-2")
        host_lines += [f"[profile {name}]", f"region = {region}",
                       f"credential_process = {PY} {HELPER} aws-creds {name}", ""]
        key = placeholder_key(i)
        child_lines += [f"[profile {name}]", f"region = {region}", f"aws_access_key_id = {key}",
                        "aws_secret_access_key = nono-pi-placeholder-not-a-secret", ""]
        profiles[key] = name
    default = cfg["aws"].get("_session_default")
    if default in accounts:
        key = next(k for k, v in profiles.items() if v == default)
        child_lines = ["[default]", f"region = {accounts[default].get('region', 'eu-west-2')}",
                       f"aws_access_key_id = {key}", "aws_secret_access_key = nono-pi-placeholder-not-a-secret",
                       ""] + child_lines
    host_cfg = os.path.join(HOST_DIR, "aws-config")
    child_cfg = os.path.join(SANDBOX_DIR, "aws-config")
    write_private(host_cfg, "\n".join(host_lines))
    write_private(child_cfg, "\n".join(child_lines))
    return profiles, host_cfg, child_cfg


def _symlink_targets() -> tuple[list[str], list[str]]:
    agent = os.path.join(HOME, ".pi", "agent")
    rw, ro = [], []
    for entry in ("extensions", "skills", "models.json", "profiles.json", "local-models.json", "settings.json"):
        p = os.path.join(agent, entry)
        if os.path.islink(p):
            real = os.path.realpath(p)
            rw.append(real if os.path.isdir(real) else os.path.dirname(real))
    for d in (os.path.join(agent, "skills"), os.path.join(HOME, ".agents", "skills")):
        if os.path.isdir(d):
            for name in os.listdir(d):
                p = os.path.join(d, name)
                if os.path.islink(p) and os.path.isdir(os.path.realpath(p)):
                    ro.append(os.path.realpath(p))
    return rw, ro


def credential_routes(cfg: dict) -> tuple[dict, dict]:
    creds, capture = {}, {}
    for name, svc in cfg["services"].items():
        src = svc.get("source", "keychain")
        cmd = [PY, HELPER, "capture", svc.get("secret", name)] + (["gh-token"] if src == "gh-token" else [])
        route = {"upstream": svc["upstream"], "credential_key": f"cmd://{name}",
                 "env_var": svc.get("env_var", name.upper() + "_TOKEN"),
                 "credential_format": svc.get("format", "Bearer {}")}
        if "header" in svc:
            route["inject_header"] = svc["header"]
        if "paths" in svc:
            route["endpoint_rules"] = [{"method": "*", "path": p} for p in svc["paths"]]
        elif src == "gh-token":
            route["endpoint_rules"] = GITHUB_API_RULES
        creds[name] = route
        capture[name] = {"command": cmd, "timeout_secs": 300, "cache_ttl_secs": 3600}
    return creds, capture


def aws_routes(profiles: dict, services: list[str]) -> dict:
    auth = {"profiles": profiles}
    deny = {"default": {"decision": "allow"},
            "deny": [{"method": "*", "path": "/**", "operations": AWS_DENY_OPS,
                      "reason": "would return real credentials to the sandbox"}]}
    out = {}
    for svc in services:
        route = {"upstream": f"https://{svc}.*.amazonaws.com",
                 "aws_auth": dict(auth, service=SERVICE_NAME.get(svc, svc))}
        if svc in ("sts", "ecr", "api.ecr"):
            route["endpoint_policy"] = deny
        out["aws_" + svc.replace(".", "_").replace("-", "_")] = route
    for key, (upstream, region, service) in GLOBAL_AWS.items():
        route = {"upstream": upstream, "aws_auth": dict(auth, region=region, service=service)}
        if service in ("sts", "iam"):
            route["endpoint_policy"] = deny
        out["aws_" + key] = route
    return out


def git_env() -> dict:
    """git config via env (no file changes): brokered ssh + brokered signing."""
    kv = [("gpg.ssh.program", os.path.join(SANDBOX_BIN, "nono-pi-sign")),
          ("core.sshCommand", os.path.join(SANDBOX_BIN, "ssh"))]
    env = {"GIT_CONFIG_COUNT": str(len(kv)), "GIT_SSH_COMMAND": os.path.join(SANDBOX_BIN, "ssh")}
    for i, (k, v) in enumerate(kv):
        env[f"GIT_CONFIG_KEY_{i}"], env[f"GIT_CONFIG_VALUE_{i}"] = k, v
    return env


def build(cfg: dict, session: dict) -> tuple[dict, dict]:
    """Returns (nono profile, env for the nono process)."""
    creds, capture = credential_routes(cfg)
    nono_env: dict = {}
    env_set = {"SHELL": "/bin/bash", "PI_SANDBOX": "nono-pi", "PI_BROKER": proto.BROKER,
               **git_env()}
    if os.environ.get("NONO_PI_ZJSTATUS"):
        env_set["PI_NONO_ZJSTATUS_BAR"] = "1"
    read_files, read_dirs = [], [SANDBOX_BIN, PI_EXT]
    if cfg["aws"].get("enabled") and cfg["aws"].get("accounts") and not session.get("aws_off"):
        cfg["aws"]["_session_default"] = (session.get("aws") or [None])[0]
        profiles, host_cfg, child_cfg = aws_files(cfg)
        creds.update(aws_routes(profiles, cfg["aws"].get("services", DEFAULT_AWS_SERVICES)))
        env_set.update(AWS_CONFIG_FILE=child_cfg, AWS_SHARED_CREDENTIALS_FILE="/dev/null",
                       AWS_EC2_METADATA_DISABLED="true", AWS_SDK_UA_APP_ID="nono-pi")
        if session.get("aws"):
            env_set["AWS_PROFILE"] = session["aws"][0]
        read_files.append(child_cfg)
        nono_env = {"AWS_CONFIG_FILE": host_cfg, "AWS_SHARED_CREDENTIALS_FILE": "/dev/null"}

    ssh = os.path.join(HOME, ".ssh")
    ssh_ro = [p for p in (os.path.join(ssh, "known_hosts"), os.path.join(ssh, "config"),
                          os.path.join(ssh, "id_ed25519.pub"), os.path.join(ssh, "id_rsa.pub"),
                          os.path.join(HOME, ".config", "git", "allowed_signers")) if os.path.exists(p)]
    rw_links, ro_links = _symlink_targets()
    read_dirs += ro_links
    net = cfg["network"]
    network = {"credentials": sorted(creds), "custom_credentials": creds,
               "allow_domain": sorted(set(net.get("allow", []) + session.get("allow_hosts", [])))}
    if net.get("ask", True):
        network["approval_backends"] = {"nonopi": {"type": "webhook", "url": f"unix://{proto.CTL}",
                                                   "timeout_secs": 300}}
        network["approval_defaults"] = {"backend": "nonopi"}

    profile = {
        "meta": {"name": "nono-pi", "description": "pi with brokered credentials (generated by nono-pi)"},
        "extends": "default",
        "security": {"ipc_mode": "full"},
        "groups": {"include": ["node_runtime", "python_runtime", "rust_runtime", "go_runtime", "bun_runtime",
                               "git_config", "user_tools",
                               {"name": "user_caches_macos", "when": "macos"},
                               {"name": "homebrew_macos", "when": "macos"},
                               {"name": "user_caches_linux", "when": "linux"}]},
        "workdir": {"access": "readwrite"},
        "filesystem": {
            "allow": sorted(set([os.path.join(HOME, ".pi"), os.path.join(HOME, ".npm")] + rw_links
                                + cfg.get("extra_allow", []))),
            "read": sorted(set([os.path.join(HOME, ".config", "git"), os.path.join(HOME, ".config", "gh"),
                                os.path.join(HOME, ".agents")] + read_dirs + cfg.get("extra_read", []))),
            "read_file": sorted(set(ssh_ro + read_files)),
            "bypass_protection": sorted(set(p for p in ssh_ro if p.startswith(ssh))),
            # broker: connect only. ctl.sock and the private agent stay unreachable.
            "unix_socket": [proto.BROKER],
        },
        "environment": {"deny_vars": DENY_VARS, "case_insensitive_vars": True, "set_vars": env_set},
        "network": network,
        "credential_capture": capture,
        "allow_launch_services": True,
        "open_urls": {"allow_origins": ["https://auth.openai.com", "https://claude.ai", "https://github.com"],
                      "allow_localhost": True},
        "platform_overrides": {
            "macos": {"filesystem": {"deny": ["/usr/bin/osascript", "/usr/bin/osacompile"]},
                      # LMDB (fff frecency db) needs System V semaphores
                      "unsafe_macos_seatbelt_rules": ["(allow ipc-sysv-sem)"]},
            "linux": {"environment": {"deny_vars": ["DISPLAY", "WAYLAND_DISPLAY", "XAUTHORITY", "DBUS_SESSION_BUS_ADDRESS"]},
                      # default-deny AF_UNIX pathname sockets (Landlock can't deny inside an allow):
                      # only the broker is connectable; ctl.sock, the private agent, zellij, D-Bus are not.
                      "linux": {"af_unix_mediation": "pathname"}},
        },
    }
    return profile, nono_env


def nono_proxy_profile(cfg: dict, session: dict) -> tuple[dict, dict]:
    """VM mode: the same network/credential settings, for a standalone `nono proxy`."""
    full, nono_env = build(cfg, session)
    prof = {"meta": {"name": "nono-pi-vm-proxy"}, "extends": "default",
            "network": full["network"], "credential_capture": full["credential_capture"]}
    return prof, nono_env
