"""nono-pi supervisor: the trusted side of a session.

Owns grants, prompts, audit, the keystore, a private ssh-agent, and the
brokers. Started by the launcher before the agent tier; lives as long as the
session.

Servers:
  ctl.sock     (host only)  route-secret, aws-creds, net-approve (vmd), secret-get,
                            pin (saml2aws pinentry), status, grants, revoke
  broker.sock  (sandbox)    git, ssh, sign, request-secret, request-aws, status, grants
  tcp 127.0.0.1:<port>      VM mode: same broker protocol, token-authenticated
"""
from __future__ import annotations

import datetime as dt
import json
import os
import re
import secrets
import shlex
import signal
import socket
import socketserver
import subprocess
import sys
import tempfile
import threading
import time

from . import keystore, pending, prompt, proto
from .grants import ALWAYS, DENY, ONCE, WINDOW, Audit, Grants, fmt_left
from .paths import AUDIT_DIR, BIN, HOME, HOST_DIR, MAC, RUN_DIR, load_config, write_private

GIT_OPS = {"git-upload-pack": "fetch", "git-upload-archive": "fetch", "git-receive-pack": "push"}
AWS_SKEW = dt.timedelta(minutes=5)


def _host_of(dest: str) -> str:
    return dest.split("@", 1)[-1]


class Supervisor:
    def __init__(self, project: str, cwd: str, label: str, vm: bool = False):
        self.cfg = load_config()
        self.project, self.cwd, self.label, self.vm = project, cwd, label, vm
        self.grants = Grants(project)
        os.makedirs(RUN_DIR, mode=0o700, exist_ok=True)
        os.chmod(RUN_DIR, 0o700)
        os.makedirs(HOST_DIR, mode=0o700, exist_ok=True)
        os.chmod(HOST_DIR, 0o700)
        self.audit = Audit(os.path.join(AUDIT_DIR, f"{project}.{time.strftime('%Y%m%d')}.jsonl"))
        self.store = keystore.open_store()
        self.unlock_lock = threading.Lock()
        self.aws_lock = threading.Lock()
        self.key_lock = threading.Lock()
        self.aws_account_locks: dict[str, threading.Lock] = {}
        self.agent_sock = os.path.join(RUN_DIR, "agent.sock")
        self.agent_pid: int | None = None
        self.agent_loaded_until = 0.0
        self.pending_restart: dict | None = None
        self.vmhost = None            # VM mode: the session's VmHost (proxy + vmd)
        self.vm_apply_lock = threading.Lock()
        self.vm_token = secrets.token_hex(24)
        self.picked_aws: list[str] = []
        self.aws_current: str | None = None   # the VM's default profile follows this (vmd resolves it)
        self.servers: list[socketserver.BaseServer] = []
        self.signed = 0
        self.denied_until = 0.0
        self.pending = pending.Pending()
        self.bar = None
        self.panel_seen = 0.0          # last time a `nono-pi panel` polled (no-zellij mode)
        # requests always queue for the panel (zellij tab, or `nono-pi panel` in any terminal);
        # without zellij and no panel open, they fall back to a desktop dialog (prompt.py)
        prompt.QUEUE = self.pending
        prompt.PANEL_ALIVE = lambda: bool(os.environ.get("NONO_PI_ZELLIJ_SESSION")) or time.time() - self.panel_seen < 5

    # ------------------------------------------------------------ helpers
    def who(self) -> str:
        return f"pi in {self.label}{' (VM)' if self.vm else ''}"

    def window(self, kind: str) -> int:
        return int(self.cfg["grants"].get(kind, 4 * 3600))

    def panic(self) -> bool:
        return time.time() < self.denied_until

    def decide(self, kind: str, resource: str, title: str, body: str, *, always_ok: bool = True,
               window_ok: bool = True) -> bool:
        """Check grants; ask if needed. Returns allowed."""
        g = self.grants.check(kind, resource)
        if g:
            return g == "allow"
        if self.panic():
            self.audit.log("auto-deny", kind=kind, resource=resource, why="panic")
            return False
        win = self.window(kind)
        opts = []
        if window_ok:
            opts.append(("window", f"Allow {fmt_left(win)}"))
        opts.append(("once", "Once"))
        if always_ok:
            opts.append(("always", "Always (project)"))
        opts += [("deny", "Deny"), ("panic", "Deny all new 10m")]
        choice = prompt.choose(f"{self.who()} wants {title}", body, opts,
                               "window" if window_ok else "once", pid=f"{kind}")
        self.audit.log("decision", kind=kind, resource=resource, choice=choice)
        if self.bar:
            self.bar.push_now()
        if choice in (None, "deny"):
            self.grants.add(kind, resource, DENY, 120)
            return False
        if choice == "panic":
            self.denied_until = time.time() + 600
            return False
        self.grants.add(kind, resource, {"window": WINDOW, "once": ONCE, "always": ALWAYS}[choice], win)
        return True

    # ------------------------------------------------------------ keystore
    def ensure_unlocked(self, why: str) -> bool:
        with self.unlock_lock:
            if not self.store.exists():
                return False
            if self.store.is_unlocked():
                return True
            for attempt in range(3):
                pw = prompt.secret("unlock nono-pi keychain",
                                   f"{self.who()}: {why}\nUnlocks for {fmt_left(self.cfg['keychain']['lock_after'])}."
                                   + ("\n\nWrong password, try again." if attempt else ""), pid="unlock")
                if pw is None:
                    return False
                if self.store.unlock(pw):
                    self.audit.log("keychain-unlock")
                    return True
            return False

    def ks_get(self, name: str, why: str) -> str | None:
        try:
            return self.store.get(name)
        except keystore.Locked:
            if not self.ensure_unlocked(why):
                return None
            return self.store.get(name)

    def ks_set(self, name: str, value: str) -> None:
        try:
            self.store.set(name, value)
        except keystore.Locked:
            if self.ensure_unlocked(f"store {name}"):
                self.store.set(name, value)

    # ------------------------------------------------------------ secrets
    def secret_get(self, name: str, host: str) -> str | None:
        v = self.ks_get(name, f"API credential '{name}' for {host}")
        if v is not None:
            self.audit.log("secret-use", name=name, host=host)
            return v
        v = prompt.secret(f"secret needed: {name}",
                          f"{self.who()} called {host}, which needs '{name}'.\n"
                          f"Stored in the nono-pi keychain; the agent never sees it.", pid="secret")
        if v:
            self.ks_set(name, v)
            self.audit.log("secret-stored", name=name, host=host)
        return v

    def gh_token(self) -> str | None:
        r = subprocess.run(["gh", "auth", "token"], capture_output=True, text=True)
        return r.stdout.strip() or None

    # ------------------------------------------------------------ AWS
    def aws_identity(self, account: str) -> dict:
        """Who a cached session really is (sts get-caller-identity, host side)."""
        try:
            c = json.loads(self.store.get(f"aws_session_{account}") or "null")
        except (keystore.Locked, ValueError):
            c = None
        if not c:
            return {}
        env = {k: v for k, v in os.environ.items() if not k.startswith("AWS_")}
        env.update(AWS_ACCESS_KEY_ID=c["AccessKeyId"], AWS_SECRET_ACCESS_KEY=c["SecretAccessKey"],
                   AWS_SESSION_TOKEN=c.get("SessionToken", ""), AWS_CONFIG_FILE="/dev/null",
                   AWS_SHARED_CREDENTIALS_FILE="/dev/null",
                   AWS_REGION=self.cfg["aws"]["accounts"].get(account, {}).get("region", "eu-west-2"))
        try:
            r = subprocess.run(["aws", "sts", "get-caller-identity", "--output", "json", "--no-cli-pager"],
                               env=env, capture_output=True, text=True, timeout=20)
            d = json.loads(r.stdout) if r.returncode == 0 else {}
        except (OSError, ValueError, subprocess.TimeoutExpired):
            d = {}
        exp = c.get("Expiration", "")
        if not d:   # STS unreachable / rejected: fall back to what we asked for
            arn = self.cfg["aws"]["accounts"].get(account, {}).get("role_arn", "")
            return {"account_id": arn.split(":")[4] if arn.count(":") >= 5 else None,
                    "arn": arn.replace(":iam::", ":sts::").replace(":role/", ":assumed-role/") + "/?",
                    "expires": exp, "verified": False}
        return {"account_id": d.get("Account"), "arn": d.get("Arn"), "expires": exp, "verified": True}

    def aws_creds(self, account: str) -> dict | None:
        accts = self.cfg["aws"].get("accounts", {})
        acct = accts.get(account)
        if not acct:
            return None
        if account not in self.picked_aws:
            ok = self.decide("aws", account, f"AWS account {account}",
                             f"role {acct['role_arn']}\nfirst use this session (not picked at launch)")
            if not ok:
                return None
        # one login per account at a time: concurrent callers (SDK retries, parallel
        # requests) wait for it instead of each asking for the password again
        with self.aws_lock:
            lk = self.aws_account_locks.setdefault(account, threading.Lock())
        with lk:
            key = f"aws_session_{account}"
            cached = self.ks_get(key, f"AWS session for {account}")
            if cached:
                try:
                    c = json.loads(cached)
                    exp = dt.datetime.fromisoformat(c["Expiration"].replace("Z", "+00:00"))
                    if exp - AWS_SKEW > dt.datetime.now(dt.timezone.utc):
                        return c
                except (ValueError, KeyError):
                    pass
            c = self._saml2aws(account, acct)
            if c:
                self.ks_set(key, json.dumps(c, separators=(",", ":")))
                self.audit.log("aws-login", account=account)
                if self.bar:
                    self.bar.push_now()
            return c

    def _aws_sessions(self, all_accounts: bool = False) -> dict:
        """account -> seconds left, for cached STS sessions (no unlock prompt)."""
        out = {}
        if not (self.store.exists() and self.store.is_unlocked()):
            return out
        names = sorted(self.cfg["aws"].get("accounts", {})) if all_accounts else \
            list(dict.fromkeys(self.picked_aws + ([self.aws_current] if self.aws_current else [])))
        for name in names:
            try:
                c = self.store.get(f"aws_session_{name}")
                if c:
                    exp = dt.datetime.fromisoformat(json.loads(c)["Expiration"].replace("Z", "+00:00"))
                    left = int((exp - dt.datetime.now(dt.timezone.utc)).total_seconds())
                    if left > 0:
                        out[name] = left
            except (keystore.Locked, ValueError, KeyError):
                pass
        return out

    def aws_logout(self, account: str) -> None:
        """Drop cached STS sessions (one account, or all with '')."""
        try:
            names = self.store.names()
        except Exception:
            names = []
        for n in names:
            if n.startswith("aws_session_") and (not account or n == f"aws_session_{account}"):
                try:
                    self.store.rm(n)
                except keystore.Locked:
                    pass
        self.audit.log("aws-logout", account=account or "all")

    def aws_accounts(self) -> list[dict]:
        """For pickers (fzf in the VM, the panel): never contains credentials."""
        prod = set(self.cfg["aws"].get("prod_accounts", []))
        live = self._aws_sessions(all_accounts=True)
        out = []
        for name, a in sorted(self.cfg["aws"].get("accounts", {}).items()):
            parts = a["role_arn"].split(":")
            out.append({"name": name, "account_id": parts[4], "role": a["role_arn"].rsplit("/", 1)[-1],
                        "region": a.get("region", "eu-west-2"), "prod": parts[4] in prod,
                        "current": name == self.aws_current, "session_left": live.get(name)})
        return out

    def aws_use(self, account: str, via: str) -> dict:
        """Make `account` the VM's current AWS account (plain `aws`, SDKs, terraform follow it),
        logging in if needed. From the VM it needs your OK in the panel once per window; from
        the panel / host it is your own action."""
        accts = self.cfg["aws"].get("accounts", {})
        if account not in accts:
            return {"ok": False, "error": f"unknown account {account} (have: {', '.join(sorted(accts))})"}
        if via == "vm" and account != self.aws_current:
            acct = accts[account]
            ok = self.decide("aws-use", account, f"to switch AWS to {account}",
                             f"role {acct['role_arn']}\nevery plain `aws` in the VM will use it", always_ok=False)
            if not ok:
                return {"ok": False, "error": "denied in the nono-pi panel"}
        if account not in self.picked_aws:
            self.picked_aws.append(account)
        c = self.aws_creds(account)
        if not c:
            return {"ok": False, "error": f"AWS login for {account} failed or was cancelled"}
        prev, self.aws_current = self.aws_current, account
        self.audit.log("aws-use", account=account, via=via, previous=prev or "")
        if self.bar:
            self.bar.push_now()
        ident = self.aws_identity(account)
        return {"ok": True, "account": account, **ident}

    def _saml2aws(self, account: str, acct: dict) -> dict | None:
        for attempt in range(2):
            pw = self.ks_get("okta_password", "Okta password for AWS")
            if pw is None:
                pw = prompt.secret("Okta password", f"AWS login for {account}"
                                   + ("\nPrevious login failed; check the password." if attempt else ""),
                                   pid="okta")
                if not pw:
                    return None
            with tempfile.TemporaryDirectory(prefix="aws-", dir=HOST_DIR) as d:
                env = {k: v for k, v in os.environ.items() if not k.startswith("AWS_")}
                env.update(SAML2AWS_PASSWORD=pw, AWS_SHARED_CREDENTIALS_FILE=os.path.join(d, "c"),
                           AWS_CONFIG_FILE=os.path.join(d, "cfg"),
                           # Okta 6-digit code -> our pinentry -> trusted prompt
                           PATH=BIN + os.pathsep + env.get("PATH", ""),
                           PI_NONO_CTL=proto.CTL)
                args = ["saml2aws", "login", "--skip-prompt", "--force", "--quiet", "--credential-process",
                        "--prompter", "pinentry-nono-pi",
                        "--idp-account", acct.get("idp_account", "default"), "--profile", account,
                        "--role", acct["role_arn"], "--session-duration", str(acct.get("session_duration", 3600))]
                r = subprocess.run(args, env=env, capture_output=True, text=True, stdin=subprocess.DEVNULL,
                                   timeout=300)
            if r.returncode == 0:
                try:
                    self.ks_set("okta_password", pw)
                    return json.loads(r.stdout)
                except json.JSONDecodeError:
                    pass
            self.audit.log("aws-login-failed", account=account, rc=r.returncode,
                           err=r.stderr.strip().splitlines()[-1][:200] if r.stderr.strip() else "")
            try:
                self.store.rm("okta_password")
            except keystore.Locked:
                pass
        return None

    # ------------------------------------------------------------ ssh agent
    def _agent_env(self) -> dict:
        env = {k: v for k, v in os.environ.items() if k not in ("SSH_AUTH_SOCK", "SSH_AGENT_PID")}
        env["SSH_AUTH_SOCK"] = self.agent_sock
        return env

    def start_agent(self) -> None:
        if os.path.exists(self.agent_sock):
            os.unlink(self.agent_sock)
        out = subprocess.run(["ssh-agent", "-a", self.agent_sock, "-s"], capture_output=True, text=True).stdout
        m = re.search(r"SSH_AGENT_PID=(\d+)", out)
        self.agent_pid = int(m.group(1)) if m else None
        # The agent must die with us even on SIGKILL: a watchdog kills it when our pid goes away.
        if self.agent_pid:
            subprocess.Popen(["/bin/sh", "-c", f"while kill -0 {os.getpid()} 2>/dev/null; do sleep 5; done; "
                              f"kill {self.agent_pid} 2>/dev/null; rm -f '{self.agent_sock}'"],
                             stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                             start_new_session=True)

    def ensure_key(self, why: str) -> bool:
        """Load the ssh key(s) into the private agent for the rest of the window."""
        with self.key_lock:           # parallel git fetches: load once, not once per fetch
            return self._ensure_key(why)

    def _ensure_key(self, why: str) -> bool:
        now = time.time()
        if self.agent_loaded_until - 60 > now:
            return True
        life = int(self.cfg["keychain"]["lock_after"])
        ok = False
        for kp in self.cfg["ssh"]["keys"]:
            path = os.path.expanduser(kp)
            if not os.path.exists(path):
                continue
            passphrase = self.ks_get(f"ssh_passphrase:{os.path.basename(path)}", why)
            if passphrase is None and _needs_passphrase(path):
                passphrase = prompt.secret("ssh key passphrase", f"{path}\n{why}", pid="sshpass")
                if passphrase is None:
                    continue
                self.ks_set(f"ssh_passphrase:{os.path.basename(path)}", passphrase)
            with tempfile.TemporaryDirectory(dir=HOST_DIR) as d:
                ask = os.path.join(d, "ask")
                write_private(ask, "#!/bin/sh\nprintf '%s\\n' \"$NPK_PASS\"\n", 0o700)
                env = self._agent_env()
                env.update(SSH_ASKPASS=ask, SSH_ASKPASS_REQUIRE="force", DISPLAY=env.get("DISPLAY", ":0"),
                           NPK_PASS=passphrase or "")
                r = subprocess.run(["ssh-add", "-q", "-t", str(life), path], env=env,
                                   capture_output=True, text=True, stdin=subprocess.DEVNULL)
            if r.returncode == 0:
                ok = True
            else:
                self.audit.log("ssh-add-failed", key=path, err=r.stderr.strip()[:200])
        if ok:
            self.agent_loaded_until = now + life
            self.audit.log("ssh-key-loaded", life=life)
        return ok

    def drop_keys(self) -> None:
        subprocess.run(["ssh-add", "-D"], env=self._agent_env(), capture_output=True)
        self.agent_loaded_until = 0

    # ------------------------------------------------------------ git / ssh broker
    def check_git(self, dest: str, port: str, command: str) -> tuple[bool, str]:
        """Validate + authorize a git-over-ssh exec. Returns (ok, message)."""
        try:
            argv = shlex.split(command)
        except ValueError:
            return False, "nono-pi: unparseable remote command"
        if len(argv) != 2 or argv[0] not in GIT_OPS:
            return False, f"nono-pi: only git fetch/push over ssh is brokered (got {argv[:1]})"
        op, repo = GIT_OPS[argv[0]], argv[1].strip("/").removesuffix(".git")
        host = _host_of(dest)
        if not re.fullmatch(r"[A-Za-z0-9._~/-]+", repo) or ".." in repo:
            return False, "nono-pi: refusing odd repo path"
        res = f"{host}/{repo}"
        if op == "fetch":
            if not self.cfg["grants"].get("fetch_auto", True) and not self.decide(
                    "git-fetch", res, f"to fetch {res}", f"git-upload-pack on {host}"):
                return False, "nono-pi: fetch denied"
        else:
            if not self.decide("git-push", res, f"to PUSH to {res}",
                               f"git-receive-pack {repo} on {host}:{port}", always_ok=False):
                return False, f"nono-pi: push to {res} denied"
        self.audit.log("git", op=op, repo=res)
        return True, ""

    def check_interactive(self, dest: str, argv: list[str]) -> bool:
        return self.decide("ssh", _host_of(dest), f"an ssh session to {dest}",
                           "interactive / remote command: " + (" ".join(argv) or "(shell)"),
                           always_ok=False, window_ok=False)

    def ssh_cmd(self, dest: str, port: str, rest: list[str], tty: bool) -> list[str]:
        cmd = ["ssh", "-o", "BatchMode=yes", "-o", "IdentitiesOnly=no", "-o", "ForwardAgent=no",
               "-o", "ClearAllForwardings=yes", "-o", "PermitLocalCommand=no", "-p", port]
        cmd += ["-tt"] if tty else ["-T"]
        return cmd + ["--", dest] + rest

    # ------------------------------------------------------------ signing broker
    def sign(self, payload: bytes, pubkey: str) -> tuple[bool, str]:
        if not re.match(rb"^(tree [0-9a-f]{40,64}\n|object [0-9a-f]{40,64}\n)", payload):
            return False, "nono-pi: only git commit/tag objects are signed"
        if not self.ensure_key("sign git commit"):
            return False, "nono-pi: ssh key unavailable"
        with tempfile.TemporaryDirectory(dir=HOST_DIR) as d:
            msg, pub = os.path.join(d, "m"), os.path.join(d, "k.pub")
            with open(msg, "wb") as f:
                f.write(payload)
            write_private(pub, pubkey.strip() + "\n")
            r = subprocess.run(["ssh-keygen", "-Y", "sign", "-n", "git", "-f", pub, "-U", msg],
                               env=self._agent_env(), capture_output=True, text=True)
            if r.returncode != 0:
                return False, "nono-pi: signing failed: " + r.stderr.strip()[:200]
            sig = open(msg + ".sig").read()
        m = re.search(rb"\ncommitter [^\n]*", payload)
        self.signed += 1
        self.audit.log("sign", what=(m.group(0).decode(errors="replace")[1:80] if m else "tag"))
        return True, sig

    # ------------------------------------------------------------ net approvals (from vmd)
    def _host_allowed(self, host: str) -> bool:
        import fnmatch
        return any(host == p or (p.startswith("*.") and fnmatch.fnmatchcase(host, p))
                   for p in load_config()["network"].get("allow", []))

    def approve_net(self, host: str) -> bool:
        host = str(host)[:253].lower()
        if self._host_allowed(host):       # added live from the panel
            self.audit.log("net-allowlist", host=host)
            return True
        if not self.cfg["network"].get("ask", True):
            return False
        return self.decide("net", host, f"network access to {host}", "host is not on the allowlist")

    def route_secret(self, name: str, host: str) -> str | None:
        """vmd asks for the real value behind a route placeholder (never sent to the guest)."""
        svc = self.cfg["services"].get(name)
        if not svc:
            return None
        up_host = svc["upstream"].split("://", 1)[-1].split("/", 1)[0].split(":", 1)[0]
        if host != up_host:            # a route's secret only ever goes to its own host
            self.audit.log("route-mismatch", name=name, host=host)
            return None
        if svc.get("source") == "gh-token":
            return self.gh_token()
        return self.secret_get(svc.get("secret", name), host)

    # ------------------------------------------------------------ credential requests (restart)
    def request_secret(self, req: dict) -> dict:
        name = re.sub(r"[^a-z0-9_]", "_", req.get("name", "").lower())[:40]
        upstream = req.get("upstream", "")
        if not name or not re.fullmatch(r"https://[A-Za-z0-9.-]+(:\d+)?", upstream):
            return {"ok": False, "error": "need name and an https://host upstream"}
        header = req.get("header") or "Authorization"
        fmt = req.get("format") or "Bearer {}"
        env_var = re.sub(r"[^A-Z0-9_]", "_", (req.get("env_var") or name.upper() + "_TOKEN").upper())
        body = (f"route: {upstream}\nheader: {header}: {fmt.replace('{}', '<secret>')}\n"
                f"agent sees: ${env_var} (placeholder only)")
        if req.get("reason"):
            body += f"\n\nagent's reason (unverified): {req['reason'][:200]}"
        choice = prompt.choose(f"{self.who()} requests a credential '{name}'", body,
                               [("yes", "Approve + enter value"), ("deny", "Deny")], "yes", pid="request")
        self.audit.log("credential-request", name=name, upstream=upstream, choice=choice)
        if choice != "yes":
            return {"ok": False, "error": "denied by user"}
        if self.ks_get(name, f"store {name}") is None:
            v = prompt.secret(f"value for {name}", f"{upstream} ({header})", pid="secret")
            if not v:
                return {"ok": False, "error": "no value entered"}
            self.ks_set(name, v)
        cfg = load_config()
        cfg["services"][name] = {"upstream": upstream, "env_var": env_var, "format": fmt,
                                 **({"header": header} if header != "Authorization" else {})}
        from .paths import save_config
        save_config(cfg)
        # a credential route only changes the network policy + guest env: reload vmd in
        # place (shells keep running; `pi` restarts itself with --continue to pick up $ENV)
        if self.vmhost:
            self.vmhost.reload()
        self.audit.log("policy-reload", why=f"credential {name}")
        return {"ok": True, "restart": True, "env_var": env_var,
                "note": "pi restarts in a moment with the same session; then use $" + env_var +
                        " (new shells have it too; in an open shell: exec fish)"}

    def request_access(self, req: dict) -> dict:
        path = os.path.realpath(os.path.expanduser(req.get("path", "")))
        mode = "write" if req.get("write") else "read"
        if not path.startswith(HOME) or any(s in path for s in ("/.ssh", "/.aws", "/.config/nono-pi",
                                                                   "/Library/Keychains", "/.gnupg")):
            return {"ok": False, "error": "path not allowed"}
        choice = prompt.choose(f"{self.who()} requests {mode} access", path +
                               (f"\n\nagent's reason (unverified): {req['reason'][:200]}" if req.get("reason") else ""),
                               [("yes", "Allow (restart)"), ("deny", "Deny")], "yes", pid="access")
        self.audit.log("access-request", path=path, mode=mode, choice=choice)
        if choice != "yes":
            return {"ok": False, "error": "denied by user"}
        cfg = load_config()
        key = "extra_allow" if mode == "write" else "extra_read"
        cfg[key] = sorted(set(cfg.get(key, []) + [path]))
        from .paths import save_config
        save_config(cfg)
        # a new directory needs a new VM mount: the VM restarts (shells reopen fresh,
        # pi resumes) once pi is idle - `pi` in the VM calls `nono-pi apply` - or after 20s
        self.pending_restart = {"why": f"{mode} {path}"}
        self._vm_apply_soon()
        return {"ok": True, "restart": True}

    # ------------------------------------------------------------ VM: new access
    def vm_apply(self) -> dict | None:
        """A new credential route / mount needs a new policy spec and a
        recreated VM. Idempotent; returns what was applied (None if nothing)."""
        with self.vm_apply_lock:
            why = self.pending_restart
            if not why or not self.vmhost:
                return None
            self.pending_restart = None
            self.cfg = load_config()
            self.vmhost.restart(why.get("why", "new access"))
            self.audit.log("vm-restart", why=why.get("why", ""))
            return why

    def _vm_apply_soon(self) -> None:
        # pi exits 75 and calls vm-apply when its turn ends; if pi isn't the one that
        # asked (e.g. you ran `nono-pi request-secret` in a shell), apply after a grace period
        if self.vmhost:
            def later():
                time.sleep(20)
                try:
                    self.vm_apply()
                except Exception as e:
                    self.audit.log("vm-restart-failed", err=str(e)[:200])
            threading.Thread(target=later, daemon=True).start()

    # ------------------------------------------------------------ panel
    def zellij(self, args: list[str]) -> None:
        sess = os.environ.get("NONO_PI_ZELLIJ_SESSION")
        if sess:
            subprocess.run(["zellij", "-s", sess, *args], capture_output=True)

    def edit_allow(self, host: str, add: bool) -> dict:
        host = host.strip().lower()
        if not re.fullmatch(r"(\*\.)?[a-z0-9.-]+\.[a-z]{2,}", host):
            return {"ok": False, "error": "not a hostname"}
        # edit only the USER's lists in the raw config file (defaults are merged in on load)
        from .paths import DEFAULT_CONFIG, config_path, save_config
        raw = json.load(open(config_path()))
        net = raw.setdefault("network", {})
        allow, deny = net.setdefault("allow", []), net.setdefault("deny", [])
        if add:
            if host not in allow:
                allow.append(host)
            if host in deny:
                deny.remove(host)
        else:
            if host in allow:
                allow.remove(host)
            if host in DEFAULT_CONFIG["network"]["allow"] and host not in deny:
                deny.append(host)          # remember that a default was removed
        net["allow"], net["deny"] = sorted(allow), sorted(deny)
        if not net["deny"]:
            net.pop("deny")
        save_config(raw)
        self.cfg = load_config()
        if not add:
            self.grants.revoke(f"net:{host}")
        self.audit.log("net-allow" if add else "net-remove", host=host, via="panel")
        if self.vmhost:
            try:
                self.vmhost.reload()      # vmd picks up the new allowlist for the next request
            except OSError:
                pass
        return {"ok": True}

    def panel_state(self) -> dict:
        st = self.status()
        st["pending"] = self.pending.list()
        st["allow"] = sorted(load_config()["network"].get("allow", []))
        prod = set(self.cfg["aws"].get("prod_accounts", []))
        aws = []
        unlocked = self.store.exists() and self.store.is_unlocked()
        for name, a in sorted(self.cfg["aws"].get("accounts", {}).items()):
            left = None
            if unlocked:
                try:
                    c = self.store.get(f"aws_session_{name}")
                    if c:
                        exp = dt.datetime.fromisoformat(json.loads(c)["Expiration"].replace("Z", "+00:00"))
                        left = max(0, int((exp - dt.datetime.now(dt.timezone.utc)).total_seconds()))
                except (keystore.Locked, ValueError, KeyError):
                    pass
            parts = a["role_arn"].split(":")
            aws.append({"name": name, "picked": name in self.picked_aws, "session_left": left,
                        "current": name == self.aws_current,
                        "prod": parts[4] in prod, "account_id": parts[4],
                        "role": a["role_arn"].rsplit("/", 1)[-1]})
        st["aws"] = aws
        try:
            st["secrets"] = [n for n in self.store.names() if not n.startswith(("aws_session_", "ssh_passphrase:"))] if unlocked else []
        except Exception:
            st["secrets"] = []
        return st

    # ------------------------------------------------------------ status
    def status(self) -> dict:
        return {"ok": True, "project": self.label, "vm": self.vm, "pid": os.getpid(),
                "zellij_session": os.environ.get("NONO_PI_ZELLIJ_SESSION", ""),
                "aws_picked": self.picked_aws, "aws_current": self.aws_current,
                "aws_accounts": sorted(self.cfg["aws"].get("accounts", {})),
                "grants": self.grants.active(), "signed": self.signed,
                "keychain_unlocked": self.store.exists() and self.store.is_unlocked(),
                "ssh_key_loaded_for": max(0, int(self.agent_loaded_until - time.time())),
                "pending_count": self.pending.count(),
                "aws_logged_in": self._aws_sessions(),
                "recent": self.audit.recent[-30:],
                "events": self.audit.events[-200:]}

    # ------------------------------------------------------------ servers
    def serve(self) -> None:
        self.start_agent()
        for path, handler in ((proto.CTL, CtlHandler), (proto.BROKER, BrokerHandler)):
            if os.path.exists(path):
                os.unlink(path)
            srv = _UnixServer(path, handler)
            srv.sup = self
            os.chmod(path, 0o600)
            self.servers.append(srv)
            threading.Thread(target=srv.serve_forever, daemon=True).start()
        if self.vm:
            srv = _TcpServer(("127.0.0.1", 0), BrokerHandler)
            srv.sup = self
            self.vm_port = srv.server_address[1]
            self.servers.append(srv)
            threading.Thread(target=srv.serve_forever, daemon=True).start()
        threading.Thread(target=self._reaper, daemon=True).start()
        from . import zjbar
        self.bar = zjbar.maybe_start(self)
        if self.bar:
            prompt.ON_QUEUE = lambda n: self.bar.push_now()

    def _reaper(self) -> None:
        while True:
            time.sleep(30)
            if self.agent_loaded_until and time.time() > self.agent_loaded_until:
                self.drop_keys()
                self.audit.log("ssh-key-expired")

    def shutdown(self) -> None:
        for s in self.servers:
            s.shutdown()
        if self.agent_pid:
            try:
                os.kill(self.agent_pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
        for p in (proto.CTL, proto.BROKER, self.agent_sock):
            try:
                os.unlink(p)
            except FileNotFoundError:
                pass


def _needs_passphrase(path: str) -> bool:
    r = subprocess.run(["ssh-keygen", "-y", "-P", "", "-f", path], capture_output=True)
    return r.returncode != 0


class _UnixServer(socketserver.ThreadingMixIn, socketserver.UnixStreamServer):
    daemon_threads = True


class _TcpServer(socketserver.ThreadingMixIn, socketserver.TCPServer):
    daemon_threads = True
    allow_reuse_address = True


class _Base(socketserver.StreamRequestHandler):
    sup: Supervisor

    def setup(self):
        super().setup()
        self.sup = self.server.sup  # type: ignore[attr-defined]

    def reply(self, obj: dict) -> None:
        self.wfile.write((json.dumps(obj, separators=(",", ":")) + "\n").encode())
        self.wfile.flush()


class CtlHandler(_Base):
    """Host-side only (vmd, panel, nono-pi-key, pinentry)."""

    def handle(self):
        try:
            self._handle()
        except Exception as e:
            self.sup.audit.log("ctl-error", err=f"{type(e).__name__}: {e}"[:200])
            try:
                self.reply({"ok": False, "error": f"nono-pi supervisor error: {type(e).__name__}"})
            except OSError:
                pass

    def _handle(self):
        first = self.rfile.readline(proto.MAX_LINE)
        try:
            req = json.loads(first)
        except ValueError:
            return
        op, s = req.get("op"), self.sup
        if op == "route-secret":            # vmd: real value for a credential route
            v = s.route_secret(str(req.get("name", "")), str(req.get("host", "")))
            self.reply({"ok": v is not None, "value": v})
        elif op == "net-approve":           # vmd: host not on the allowlist
            self.reply({"ok": s.approve_net(req.get("host", ""))})
        elif op == "secret-get":
            v = s.gh_token() if req.get("source") == "gh-token" else s.secret_get(req["name"], req.get("host", ""))
            self.reply({"ok": v is not None, "value": v})
        elif op == "aws-creds":
            c = s.aws_creds(req["account"])
            self.reply({"ok": c is not None, "creds": c})
        elif op == "pin":
            v = prompt.secret("Okta verification code", "6-digit code from your authenticator app", pid="okta-code")
            self.reply({"ok": bool(v), "value": v})
        elif op == "status":
            self.reply(s.status())
        elif op == "vm-apply":
            self.reply({"ok": True, "applied": s.vm_apply()})
        elif op == "shutdown-orphan":
            # only honoured when our zellij session really is gone (the launcher checked; re-check here)
            sess = os.environ.get("NONO_PI_ZELLIJ_SESSION", "")
            alive = True
            if sess:
                try:
                    alive = sess in subprocess.run(["zellij", "list-sessions", "--no-formatting", "--short"],
                                                   capture_output=True, text=True, timeout=5).stdout.split()
                except (OSError, subprocess.SubprocessError):
                    alive = True
            self.reply({"ok": not alive})
            if not alive:
                s.audit.log("orphan-shutdown", session=sess)
                os.kill(os.getpid(), signal.SIGTERM)
        elif op == "vm-ssh-exec":
            # Gondolin execPolicy: the guest's ssh exec, attributed by gondolind on the host
            host, port, cmd = req.get("host", ""), str(req.get("port", 22)), req.get("command", "")
            ok, msg = s.check_git(f"{req.get('user') or 'git'}@{host}", port, cmd)
            if ok and not s.ensure_key(f"git over ssh to {host}"):
                ok, msg = False, "nono-pi: ssh key unavailable"
            self.reply({"ok": ok, "error": msg})
        elif op == "revoke":
            self.reply({"ok": s.grants.revoke(req["key"])})
        elif op == "panel-state":
            s.panel_seen = time.time()
            self.reply(s.panel_state())
        elif op == "answer":
            ans = {"choice": req["choice"]} if "choice" in req else ({"value": req["value"]} if req.get("value") else None)
            self.reply({"ok": s.pending.answer(int(req["id"]), ans)})
        elif op in ("net-allow", "net-remove"):
            self.reply(s.edit_allow(req.get("host", ""), op == "net-allow"))
        elif op == "aws-pick":
            a = req.get("account", "")
            if a in s.cfg["aws"].get("accounts", {}) and a not in s.picked_aws:
                s.picked_aws.append(a)
                s.audit.log("aws-pick", account=a)
            self.reply({"ok": True})
        elif op == "aws-login":
            acct = req.get("account") or s.cfg["aws"].get("default_account", "")
            if acct not in s.cfg["aws"].get("accounts", {}):
                return self.reply({"ok": False, "error": f"unknown account {acct}"})
            if req.get("force"):
                s.aws_logout(acct)
            if acct not in s.picked_aws:          # a login you start yourself is an approval
                s.picked_aws.append(acct)
            before = set(s.audit.recent)
            c = s.aws_creds(acct)
            fresh = any("aws-login " in l for l in set(s.audit.recent) - before)
            ident = s.aws_identity(acct) if c else {}
            self.reply({"ok": c is not None, "account": acct, "reused": not fresh, **ident})
        elif op == "aws-logout":
            s.aws_logout(req.get("account") or "")
            self.reply({"ok": True})
        elif op == "aws-use":                # panel / nono-pi-key: your own action
            self.reply(s.aws_use(str(req.get("account", "")), via="host"))
        elif op == "aws-accounts":
            self.reply({"ok": True, "accounts": s.aws_accounts(), "current": s.aws_current})
        elif op == "aws-current":            # vmd: which account the "current" placeholder key means
            self.reply({"ok": True, "account": s.aws_current})
        elif op == "secret-set":
            n = re.sub(r"[^A-Za-z0-9_.:-]", "_", req.get("name", ""))[:80]
            if not n or not req.get("value"):
                return self.reply({"ok": False, "error": "name and value required"})
            s.ks_set(n, req["value"])
            s.audit.log("secret-stored", name=n, via="panel")
            self.reply({"ok": True})
        elif op == "secret-rm":
            try:
                self.reply({"ok": s.store.rm(req.get("name", ""))})
            except keystore.Locked:
                self.reply({"ok": False, "error": "keychain locked"})
        elif op == "unlock":
            self.reply({"ok": s.store.unlock(req.get("password", ""))})
        elif op == "lock":
            s.store.lock()
            s.drop_keys()
            s.audit.log("keychain-lock", via="panel")
            self.reply({"ok": True})
        elif op == "panic":
            s.denied_until = time.time() + 600
            s.audit.log("panic", via="panel")
            self.reply({"ok": True})
        elif op == "panel-hide":
            s.zellij(["action", "go-to-tab", "1"])
            self.reply({"ok": True})
        else:
            self.reply({"ok": False, "error": f"unknown op {op}"})


class BrokerHandler(_Base):
    """Reachable from the agent tier. Never returns a credential."""

    def handle(self):
        try:
            self._handle()
        except Exception as e:  # never crash the server thread; tell the client
            self.sup.audit.log("broker-error", err=f"{type(e).__name__}: {e}"[:200])
            try:
                self.reply({"ok": False, "error": f"nono-pi supervisor error: {type(e).__name__}"})
            except OSError:
                pass

    def _handle(self):
        try:
            req = json.loads(self.rfile.readline(proto.MAX_LINE))
        except ValueError:
            return
        s = self.sup
        if isinstance(self.server, _TcpServer) and not secrets.compare_digest(str(req.get("token", "")), s.vm_token):
            return self.reply({"ok": False, "error": "bad token"})
        op = req.get("op")
        if op in ("git", "ssh"):
            return self._ssh(req)
        if op == "sign":
            ok, out = s.sign(req.get("payload", "").encode("latin-1"), req.get("pubkey", ""))
            return self.reply({"ok": ok, "sig": out} if ok else {"ok": False, "error": out})
        if op == "request-secret":
            return self.reply(s.request_secret(req))
        if op == "request-access":
            return self.reply(s.request_access(req))
        if op == "aws-use":                  # from the VM: asks you in the panel (once per window)
            r = s.aws_use(str(req.get("account", "")), via="vm")
            return self.reply({k: v for k, v in r.items() if k in ("ok", "error", "account", "arn", "account_id", "expires", "verified")})
        if op == "aws-accounts":
            return self.reply({"ok": True, "accounts": s.aws_accounts(), "current": s.aws_current})
        if op == "aws-login":
            acct = req.get("account") or s.cfg["aws"].get("default_account")
            c = s.aws_creds(acct) if acct else None
            return self.reply({"ok": c is not None, "account": acct})   # creds never returned
        if op == "status":
            return self.reply(s.status())
        if op == "revoke":
            return self.reply({"ok": s.grants.revoke(req.get("key", ""))})
        if op == "restart-pending":
            return self.reply({"ok": True, "pending": s.pending_restart})
        if op == "vm-apply":       # only applies what the user already granted
            return self.reply({"ok": True, "applied": s.vm_apply()})
        self.reply({"ok": False, "error": f"unknown op {op}"})

    def _ssh(self, req: dict):
        s = self.sup
        dest, port = str(req.get("dest", "")), str(req.get("port", "22"))
        rest = [str(a) for a in req.get("argv", [])]
        if not re.fullmatch(r"[A-Za-z0-9._-]+@?[A-Za-z0-9._-]*", dest) or not port.isdigit():
            return self.reply({"ok": False, "error": "bad destination"})
        if req["op"] == "git":
            ok, msg = s.check_git(dest, port, " ".join(rest))
            tty = False
        else:
            ok, msg = s.check_interactive(dest, rest), "nono-pi: ssh denied"
            tty = bool(req.get("tty"))
        if not ok:
            return self.reply({"ok": False, "error": msg})
        if not s.ensure_key(f"ssh to {dest}"):
            return self.reply({"ok": False, "error": "nono-pi: ssh key unavailable"})
        self.reply({"ok": True, "stream": True})
        cmd = s.ssh_cmd(dest, port, rest, tty)
        sock = self.request
        if tty:
            import pty
            pid, fd = pty.fork()
            if pid == 0:
                os.execvpe(cmd[0], cmd, s._agent_env())
            _pump_fd(sock, fd, pid)
        else:
            p = subprocess.Popen(cmd, env=s._agent_env(), stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                 stderr=subprocess.DEVNULL, bufsize=0)
            _pump_proc(sock, p)


def _pump_proc(sock: socket.socket, p: subprocess.Popen) -> None:
    def up():
        try:
            while True:
                d = sock.recv(65536)
                if not d:
                    break
                p.stdin.write(d)
        except OSError:
            pass
        finally:
            try:
                p.stdin.close()
            except OSError:
                pass
    t = threading.Thread(target=up, daemon=True)
    t.start()
    try:
        while True:
            d = p.stdout.read1(65536) if hasattr(p.stdout, "read1") else os.read(p.stdout.fileno(), 65536)
            if not d:
                break
            sock.sendall(d)
    except OSError:
        pass
    p.wait()
    try:
        sock.shutdown(socket.SHUT_WR)
    except OSError:
        pass


def _pump_fd(sock: socket.socket, fd: int, pid: int) -> None:
    import select
    try:
        while True:
            r, _, _ = select.select([sock, fd], [], [])
            if sock in r:
                d = sock.recv(65536)
                if not d:
                    break
                os.write(fd, d)
            if fd in r:
                try:
                    d = os.read(fd, 65536)
                except OSError:
                    break
                if not d:
                    break
                sock.sendall(d)
    finally:
        try:
            os.kill(pid, signal.SIGHUP)
        except ProcessLookupError:
            pass
        os.waitpid(pid, 0)
