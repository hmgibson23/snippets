"""`gondo-pi doctor` (checks + how to fix) and `gondo-pi gc` (disk use + prune). Host side."""
from __future__ import annotations

import glob
import json
import os
import shutil
import subprocess
import sys
import time

from .paths import CONFIG_DIR, HOME, LINUX, MAC, PROJECTS_DIR, STATE_DIR, VM_DIR, load_config

G, Y, R, D, B, X = "\033[38;2;166;218;149m", "\033[38;2;238;212;159m", "\033[38;2;237;135;150m", "\033[38;2;110;115;141m", "\033[1m", "\033[0m"


def _run(args, **kw) -> subprocess.CompletedProcess:
    try:
        return subprocess.run(args, capture_output=True, text=True, timeout=kw.pop("timeout", 20), **kw)
    except (OSError, subprocess.SubprocessError) as e:
        return subprocess.CompletedProcess(args, 127, "", str(e))


def doctor() -> int:
    bad = 0
    def ok(msg, _fix=""): print(f"  {G}✓{X} {msg}")
    def warn(msg, fix=""):
        print(f"  {Y}!{X} {msg}" + (f"\n      {D}fix: {fix}{X}" if fix else ""))
    def fail(msg, fix=""):
        nonlocal bad
        bad += 1
        print(f"  {R}✗{X} {msg}" + (f"\n      {D}fix: {fix}{X}" if fix else ""))

    print(f"{B}host{X}")
    from .vm import kvm_ok, image_exists, image_drift, IMAGE_TAG
    k, why = kvm_ok()
    (ok if k else fail)(f"hypervisor: {why}", "" if k else "enable KVM (nested virt / kvm group)")
    for tool, need, fix in [("node", True, "install node >= 22"), ("zellij", False, "install zellij (or use pis --no-zellij)"),
                            ("git", True, "install git"), ("ssh-agent", True, "install openssh"),
                            ("gh", False, "install the GitHub CLI"), ("saml2aws", False, "install saml2aws (AWS logins)"),
                            ("aws", False, "install aws-cli v2 (verifies AWS logins)"), ("fzf", False, "install fzf")]:
        if shutil.which(tool):
            ok(tool)
        elif need:
            fail(f"{tool} not found", fix)
        else:
            warn(f"{tool} not found", fix)
    if LINUX:
        (ok if shutil.which("secret-tool") else warn)("secret-tool (keystore)" if shutil.which("secret-tool") else
                                                      "secret-tool missing: secrets only live for the session", "install libsecret")
        (ok if shutil.which("podman") else fail)("podman (image builds)" if shutil.which("podman") else "podman missing", "install podman")

    print(f"{B}gondolin + image{X}")
    pkg = os.path.join(VM_DIR, "node_modules", "@earendil-works", "gondolin", "package.json")
    if os.path.exists(pkg):
        ok(f"gondolin {json.load(open(pkg))['version']}")
        net = os.path.join(VM_DIR, "node_modules/@earendil-works/gondolin/dist/src/qemu/net.js")
        patched = "gondo-pi: sni preparse" in open(net).read() if os.path.exists(net) else False
        (ok if patched else fail)("TLS resumption patch" if patched else "TLS resumption patch not applied (https git clones fail in the VM)",
                                  "" if patched else f"cd {VM_DIR} && node patch-gondolin.mjs")
    else:
        fail("gondolin not installed", f"cd {VM_DIR} && npm ci --ignore-scripts && node patch-gondolin.mjs")
    if image_exists():
        drift = image_drift()
        (warn if drift else ok)(f"image {IMAGE_TAG}" + (f": out of date ({drift})" if drift else ""), "gondo-pi image rebuild" if drift else "")
    else:
        warn(f"image {IMAGE_TAG} not built yet (first `pis` builds it, a few minutes)")

    print(f"{B}credentials{X}")
    from . import keystore
    st = keystore.open_store()
    if MAC:
        (ok if st.exists() else fail)("gondo-pi keychain" if st.exists() else "no gondo-pi keychain", "" if st.exists() else "gondo-pi setup")
    try:
        names = st.names() if st.exists() else []
        ok(f"keystore: {len(names)} item(s)" + ("" if st.is_unlocked() else " (locked: unlocks on first use)"))
    except Exception as e:
        warn(f"keystore: {type(e).__name__}")
    cfg = load_config()
    for kp in cfg["ssh"]["keys"]:
        p = os.path.expanduser(kp)
        if not os.path.exists(p):
            continue
        nopass = _run(["ssh-keygen", "-y", "-P", "", "-f", p]).returncode == 0
        (warn if nopass else ok)(f"{kp}: " + ("NO passphrase" if nopass else "passphrase set"), "gondo-pi setup" if nopass else "")
        for b in glob.glob(p + ".gondo-pi-backup-*") + glob.glob(p + ".nono-pi-backup-*"):
            warn(f"unprotected key backup: {b}", f"verify `ssh -T git@github.com`, then rm {b}")
        pub = p + ".pub"
        if os.path.exists(pub) and shutil.which("gh"):
            key = open(pub).read().split()[1]
            ghkeys = _run(["gh", "api", "user/keys", "-q", ".[].key"]).stdout
            (ok if key in ghkeys else fail)(f"{os.path.basename(pub)} on GitHub" if key in ghkeys else f"{os.path.basename(pub)} is not on your GitHub account",
                                            "" if key in ghkeys else f"gh ssh-key add {pub} --title $(hostname)")
            signers = open(os.path.expanduser("~/.config/git/allowed_signers")).read() if os.path.exists(os.path.expanduser("~/.config/git/allowed_signers")) else ""
            (ok if key in signers else warn)("signing key in allowed_signers" if key in signers else "signing key not in ~/.config/git/allowed_signers (git log --show-signature can't verify)")
    if shutil.which("gh"):
        r = _run(["gh", "auth", "token"])
        (ok if r.returncode == 0 and r.stdout.strip() else fail)("gh is logged in" if r.returncode == 0 else "gh not logged in", "" if r.returncode == 0 else "gh auth login")
    kh = os.path.join(HOME, ".ssh", "known_hosts")
    has_gh = os.path.exists(kh) and "github.com" in open(kh).read()
    (ok if has_gh else warn)("github.com in known_hosts" if has_gh else "github.com not in ~/.ssh/known_hosts", "" if has_gh else "ssh -T git@github.com (once, on the host)")
    if cfg["aws"].get("enabled"):
        ok(f"AWS: {len(cfg['aws'].get('accounts', {}))} accounts")
        if not os.path.exists(os.path.join(HOME, ".saml2aws")):
            fail("~/.saml2aws missing (Okta settings)", "copy it from a working machine or run saml2aws configure")

    print(f"{B}session{X}")
    from . import proto
    try:
        s = proto.call(proto.CTL, {"op": "status"}, timeout=2)
        ok(f"running: {s.get('project')} (pid {s.get('pid')})")
    except OSError:
        ok("no session running")
    stray = _run(["pgrep", "-f", "vmd.mjs|gondolin-krun-runner"]).stdout.split()
    if stray and not os.path.exists(os.path.join(STATE_DIR, "run", "vmd.sock")):
        warn(f"{len(stray)} stray VM process(es) without a session", "gondo-pi gc --stray")
    print()
    print(f"{G}all good{X}" if not bad else f"{R}{bad} problem(s){X}")
    return 1 if bad else 0


def _size(path: str) -> int:
    total = 0
    for root, _, files in os.walk(path):
        for f in files:
            try:
                total += os.lstat(os.path.join(root, f)).st_size
            except OSError:
                pass
    return total


def _h(n: int) -> str:
    for unit in ("B", "K", "M", "G"):
        if n < 1024:
            return f"{n:.0f}{unit}"
        n /= 1024
    return f"{n:.1f}T"


def gc(argv: list[str]) -> int:
    """Show what gondo-pi keeps on disk; with --yes prune the safe-to-delete parts."""
    yes = "--yes" in argv or "-y" in argv
    rows = []   # (label, path(s), size, prunable, how)
    gimg = os.path.join(HOME, ".cache", "gondolin", "images")
    if os.path.isdir(gimg):
        rows.append(("gondolin images (all)", gimg, _size(gimg), False, "untagged ones pruned below"))
    npm = os.path.join(STATE_DIR, "vm-pi-npm")
    from .vm import _pi_npm_key_current
    cur = _pi_npm_key_current()
    for d in sorted(glob.glob(os.path.join(npm, "*"))):
        name = os.path.basename(d)
        rows.append((f"pi packages build {name}", d, _size(d), name != cur,
                     "current" if name == cur else ("unfinished build" if name.endswith(".tmp") else "old lockfile")))
    for d in sorted(glob.glob(os.path.join(STATE_DIR, "vm-nvim", "*"))):
        rows.append((f"nvim plugins ({os.path.basename(d)})", d, _size(d), False, "kept: warm neovim"))
    for d in sorted(glob.glob(os.path.join(PROJECTS_DIR, "*"))):
        cache = os.path.join(d, "vm-cache")
        if os.path.isdir(cache):
            age = time.time() - max((os.path.getmtime(os.path.join(r, f)) for r, _, fs in os.walk(cache) for f in fs[:50]), default=0)
            rows.append((f"tool cache {os.path.basename(d)}", cache, _size(cache), age > 30 * 86400,
                         "unused 30d+" if age > 30 * 86400 else "in use"))
    audit = glob.glob(os.path.join(STATE_DIR, "audit", "*.jsonl"))
    old_audit = [a for a in audit if time.time() - os.path.getmtime(a) > 90 * 86400]
    if audit:
        rows.append(("audit logs", os.path.join(STATE_DIR, "audit"), sum(os.path.getsize(a) for a in audit), bool(old_audit),
                     f"{len(old_audit)} older than 90d" if old_audit else "kept"))
    print(f"{B}gondo-pi disk use{X}")
    for label, path, size, prunable, how in rows:
        print(f"  {_h(size):>6}  {label:<44} {Y if prunable else D}{how}{X}")
    if LINUX and shutil.which("podman"):
        r = _run(["podman", "images", "--format", "{{.Repository}}:{{.Tag}} {{.Size}}", "--filter", "dangling=true"])
        n = len([l for l in r.stdout.splitlines() if l.strip()])
        if n:
            print(f"  {'':>6}  podman dangling build layers: {n}              {Y}prunable{X}")
    if not yes:
        print(f"\n{D}gondo-pi gc --yes   deletes the {Y}highlighted{D} entries, untagged gondolin images, podman dangling layers{X}")
        return 0
    for label, path, size, prunable, how in rows:
        if not prunable:
            continue
        if label == "audit logs":
            for a in old_audit:
                os.unlink(a)
        else:
            shutil.rmtree(path, ignore_errors=True)
        print(f"  removed {label} ({_h(size)})")
    _run(["npx", "--no-install", "gondolin", "image", "rm", "--untagged", "--yes"], cwd=VM_DIR, timeout=120)
    if LINUX and shutil.which("podman"):
        _run(["podman", "image", "prune", "-f"], timeout=120)
    if not os.path.exists(os.path.join(STATE_DIR, "run", "vmd.sock")):   # no session: VM processes are strays
        _run(["pkill", "-f", "gondolin-krun-runner"])
    print(f"{G}done{X}")
    return 0
