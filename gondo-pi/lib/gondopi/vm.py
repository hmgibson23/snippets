"""VM mode (trusted side): a Gondolin microVM devbox per gondo-pi session.

`VmHost` owns, for the life of the session:
  - the VM daemon `vm/vmd.mjs`: one VM, many pty sessions (your fish shells, pi),
    reachable only over a host-side unix socket (RUN_DIR/vmd.sock, 0600). Its
    network policy (vm/netpolicy.mjs) is the guest's only HTTP(S) exit: allowlist,
    approvals, credential injection, AWS re-signing.
  - the read-only share mounted at /gondo-pi (shims, pi extension, fish config)

The guest also reaches the supervisor broker (token-auth TCP) and ssh to git
hosts through Gondolin's host-side ssh proxy, using the supervisor's private
agent with every exec checked by the supervisor. Nothing secret is in the guest:
env holds per-session placeholders.

The rootfs is copy-on-write and throwaway: every session starts clean; only
/workspace (the project) and ~/.pi persist.
"""
from __future__ import annotations

import hashlib
import json
import os
import re
import shutil
import socket
import subprocess
import sys
import tempfile
import time

from . import policy
from .paths import HOME, HOST_DIR, PI_EXT, PROJECTS_DIR, RUN_DIR, SANDBOX_BIN, SANDBOX_DIR, STATE_DIR, VM_DIR, write_private

GUEST_HOME = "/home/pi"
IMAGE_TAG = "gondo-pi:2"
VMD_SOCK = os.path.join(RUN_DIR, "vmd.sock")
FISH_DIR = os.path.join(HOME, ".config", "fish")
# host-only fish bits that make no sense (or would fail) in the VM
FISH_SKIP = {"conf.d/gondo-pi.fish", "conf.d/saml2aws.fish", "conf.d/omf.fish", "conf.d/nvm.fish",
             "conf.d/00_fig_pre.fish", "conf.d/99_fig_post.fish", "fish_variables"}


def kvm_ok() -> tuple[bool, str]:
    if sys.platform == "darwin":
        return True, "hvf"
    if os.access("/dev/kvm", os.R_OK | os.W_OK):
        return True, "kvm"
    return False, "no /dev/kvm (nested virtualization off?)"


def _host_pi_version() -> str:
    try:
        v = subprocess.run(["pi", "--version"], capture_output=True, text=True, timeout=20).stdout.strip()
        return v if re.fullmatch(r"\d+\.\d+\.\d+[\w.+-]*", v) else ""
    except (OSError, subprocess.SubprocessError):
        return ""


def _image_inputs() -> dict:
    """What an image is built from: pi version (follows the host) + the image definition files."""
    arm = os.uname().machine in ("arm64", "aarch64")
    files = ["image-arm64.json"] if arm else ["Containerfile.arch", "image-x86_64-arch.json"]
    h = hashlib.sha256()
    for f in files:
        with open(os.path.join(VM_DIR, f), "rb") as fh:
            h.update(fh.read())
    return {"pi": _host_pi_version() or PI_FALLBACK, "definition": h.hexdigest()[:16]}


IMAGE_STAMP = os.path.join(STATE_DIR, "vm-image.json")
PI_FALLBACK = "1.6.0"


def image_drift() -> str | None:
    """Why the image is out of date (host pi updated, image definition changed), or None."""
    try:
        built = json.load(open(IMAGE_STAMP))
    except (OSError, ValueError):
        return None                    # built before stamps existed: don't nag
    now = _image_inputs()
    why = []
    if built.get("pi") != now["pi"]:
        why.append(f"pi {built.get('pi')} in the VM, {now['pi']} on the host")
    if built.get("definition") != now["definition"]:
        why.append("the image definition changed")
    return "; ".join(why) or None


LEGACY_IMAGE_TAG = "nono-pi:2"


def image_exists() -> bool:
    r = subprocess.run(["npx", "--no-install", "gondolin", "image", "ls"], cwd=VM_DIR, capture_output=True, text=True)
    tags = {l.split()[0] for l in r.stdout.splitlines() if l.split()}
    if IMAGE_TAG in tags:
        return True
    if LEGACY_IMAGE_TAG in tags:     # built before the rename: same image, new name
        return subprocess.run(["npx", "--no-install", "gondolin", "image", "tag", LEGACY_IMAGE_TAG, IMAGE_TAG],
                              cwd=VM_DIR, capture_output=True).returncode == 0
    return False


def ensure_image(rebuild: bool = False) -> None:
    if not rebuild and image_exists():
        return
    inputs = _image_inputs()
    print(f"gondo-pi: building the VM image ({IMAGE_TAG}, pi {inputs['pi']}; a few minutes)…", file=sys.stderr)
    if os.uname().machine in ("arm64", "aarch64"):
        # macOS: Alpine image, built in a podman machine; pi pinned to the host's version
        cfg = json.load(open(os.path.join(VM_DIR, "image-arm64.json")))
        cmds = cfg["postBuild"]["commands"]
        cfg["postBuild"]["commands"] = [re.sub(r"pi-coding-agent@[\w.+-]+", f"pi-coding-agent@{inputs['pi']}", c) for c in cmds]
        tmp = os.path.join(VM_DIR, ".image-arm64.build.json")
        with open(tmp, "w") as f:
            json.dump(cfg, f)
        try:
            subprocess.check_call(["npx", "--no-install", "gondolin", "build", "--config", tmp, "--tag", IMAGE_TAG], cwd=VM_DIR)
        finally:
            os.unlink(tmp)
    else:
        # Linux x86_64: Arch devbox. Rootless: podman builds the userland, gondolin wraps it
        # (gondolin's own postBuild needs root for chroot).
        subprocess.check_call(["podman", "build", "--build-arg", f"PI_VERSION={inputs['pi']}", "--build-arg", "PARALLEL=2",
                               "-t", "localhost/gondo-pi-arch:2", "-f", "Containerfile.arch", "."], cwd=VM_DIR)
        subprocess.check_call(["node", "patch-gondolin.mjs"], cwd=VM_DIR)
        subprocess.check_call(["npx", "--no-install", "gondolin", "build", "--config", "image-x86_64-arch.json",
                               "--tag", IMAGE_TAG], cwd=VM_DIR)
    write_private(IMAGE_STAMP, json.dumps({**inputs, "built": time.strftime("%Y-%m-%d %H:%M")}))
    # the old tag (pre-rename) and its image are superseded
    subprocess.run(["npx", "--no-install", "gondolin", "image", "rm", LEGACY_IMAGE_TAG, "--yes"],
                   cwd=VM_DIR, capture_output=True)


def _vm_size() -> tuple[str, int]:
    """Half the host, capped: memory '8G', cpus 6."""
    try:
        if sys.platform == "darwin":
            mem = int(subprocess.run(["sysctl", "-n", "hw.memsize"], capture_output=True, text=True).stdout)
        else:
            mem = os.sysconf("SC_PAGE_SIZE") * os.sysconf("SC_PHYS_PAGES")
        gib = max(2, min(8, mem // (2 * 1024 ** 3)))
    except (ValueError, OSError):
        gib = 4
    return f"{gib}G", max(2, min(6, (os.cpu_count() or 4) // 2))


def _drop_url_rewrites(gitconfig: str) -> str:
    """Remove [url "..."] sections (insteadOf / pushInsteadOf) from a gitconfig, nothing else."""
    out, skipping = [], False
    for line in gitconfig.splitlines(keepends=True):
        stripped = line.strip()
        if stripped.startswith("["):
            skipping = stripped.lower().startswith("[url ")
        if not skipping:
            out.append(line)
    return "".join(out)


def _copy_fish(dst: str) -> None:
    """Your fish config, minus host-only plugins/secrets, plus the VM tag."""
    if os.path.isdir(FISH_DIR):
        src = os.path.realpath(FISH_DIR)
        for root, dirs, files in os.walk(src):
            rel_root = os.path.relpath(root, src)
            for f in files:
                rel = os.path.normpath(os.path.join(rel_root, f))
                if rel in FISH_SKIP or f.endswith((".bak", ".copy")) or ".copy" in f:
                    continue
                out = os.path.join(dst, rel)
                os.makedirs(os.path.dirname(out), exist_ok=True)
                try:
                    shutil.copy(os.path.join(root, f), out)
                except OSError:
                    pass
    # host-only `source /path` lines (e.g. CachyOS's /usr/share/cachyos-fish-config) would
    # error in the VM: make them conditional
    cfgf = os.path.join(dst, "config.fish")
    if os.path.exists(cfgf):
        lines = []
        for line in open(cfgf).read().splitlines():
            m = re.match(r"^(\s*)source\s+(/\S+)\s*$", line)
            lines.append(f"{m.group(1)}test -f {m.group(2)}; and source {m.group(2)}" if m else line)
        open(cfgf, "w").write("\n".join(lines) + "\n")
    for f in ("functions/newsuk.fish", "functions/vm.fish"):
        try:
            os.unlink(os.path.join(dst, f))     # host-only helpers: no VMs inside the VM
        except FileNotFoundError:
            pass
    cd = os.path.join(dst, "conf.d")
    os.makedirs(cd, exist_ok=True)
    with open(os.path.join(cd, "zz_gondo_pi_vm.fish"), "w") as f:
        f.write(VM_FISH)


VM_FISH = r'''# gondo-pi VM (generated): you are inside the microVM - no secrets in here.
set -g fish_greeting ""
set -gx EDITOR nvim
# host paths from your config don't exist here; keep the VM's own PATH first
set -gx PATH /gondo-pi/bin /gondo-pi-cache/mise/shims /gondo-pi-cache/cargo/bin /usr/local/bin /usr/bin /bin /usr/sbin /sbin $HOME/.local/bin $HOME/go/bin
command -q mise; and mise activate fish | source
command -q direnv; and direnv hook fish | source
command -q zoxide; and zoxide init fish | source
# aws-use: switch the AWS account the whole VM uses (fzf picker without arguments)
complete -c aws-use -f -a "(aws-use --list 2>/dev/null | string trim -l -c '*')" -d "AWS account"
complete -c aws-use -l current -d "print the current account"
# pure: the VM tag replaces its container/ssh user@host segment
set -g pure_enable_container_detection false
functions -q fish_prompt; and functions -c fish_prompt __gondopi_orig_prompt
function fish_prompt
    set -l s $status
    set_color -b 363a4f 8bd5ca; printf ' ⬡ vm '; set_color normal; printf ' '
    if functions -q __gondopi_orig_prompt
        __gondopi_status $s
        __gondopi_orig_prompt
    else
        set_color 8aadf4; printf '%s' (prompt_pwd); set_color normal
        test $s -ne 0; and begin; set_color ed8796; printf ' [%s]' $s; set_color normal; end
        printf ' ❯ '
    end
end
function __gondopi_status --argument-names s
    return $s
end
function fish_title
    echo (string replace -r '^/workspace' "$GONDO_PI_PROJECT" -- $PWD)
end
if status is-interactive; and test "$GONDO_PI_VM_SESSION" = 1
    set_color 6e738d
    echo "⬡ gondo-pi VM · $GONDO_PI_PROJECT · throwaway Linux devbox · no secrets in here"
    if set -q GONDO_PI_ZELLIJ
        echo "  type pi to start pi · aws-use picks the AWS account · requests wait in the gondo-pi tab (Alt-2)"
    else
        echo "  type pi to start pi · aws-use picks the AWS account · answer requests with: gondo-pi panel (on the host)"
    end
    set_color normal
end
'''

GUEST_SETUP = r'''set -e
echo gondo-pi-vm > /proc/sys/kernel/hostname 2>/dev/null || hostname gondo-pi-vm 2>/dev/null || true
echo gondo-pi-vm > /etc/hostname 2>/dev/null || true
grep -q gondo-pi-vm /etc/hosts 2>/dev/null || echo "127.0.1.1 gondo-pi-vm" >> /etc/hosts
mkdir -p /home/pi/.config /home/pi/.local/bin
rm -rf /home/pi/.config/fish && cp -R /gondo-pi/fish /home/pi/.config/fish
[ -f /gondo-pi/gitconfig ] && cp /gondo-pi/gitconfig /home/pi/.gitconfig || true
[ -d /gondo-pi/git ] && mkdir -p /home/pi/.config/git && cp /gondo-pi/git/* /home/pi/.config/git/ || true
mkdir -p /home/pi/.ssh && for f in known_hosts id_ed25519.pub id_rsa.pub; do [ -f /gondo-pi/$f ] && cp /gondo-pi/$f /home/pi/.ssh/ || true; done
[ -f /gondo-pi/aws-config ] && mkdir -p /home/pi/.aws && cp /gondo-pi/aws-config /home/pi/.aws/config || true
'''


def _pi_npm_key_current() -> str | None:
    """The vm-pi-npm build dir name for the host's current pi package lockfile (as vmd computes it)."""
    npm = os.path.join(HOME, ".pi", "agent", "npm")
    try:
        data = open(os.path.join(npm, "package.json"), "rb").read() + open(os.path.join(npm, "package-lock.json"), "rb").read()
    except OSError:
        return None
    arch = {"arm64": "aarch64", "aarch64": "aarch64"}.get(os.uname().machine, os.uname().machine)
    return hashlib.sha256(data).hexdigest()[:16] + "-" + arch


class VmHost:
    """The session's VM: share + vmd. start() once; restart() for new access, reload() for policy edits."""

    def __init__(self, sup, cfg: dict, session: dict):
        self.sup, self.cfg, self.session = sup, cfg, session
        self.vmd = None
        self.share = None
        self.spec_path = os.path.join(HOST_DIR, "vm-spec.json")

    def start(self) -> None:
        try:
            self._start()
        except RuntimeError as e:
            if "timed out" not in str(e):
                raise
            # a cold first boot (krun/hvf) can occasionally miss gondolin's readiness window
            self.stop()
            self._start()

    def _start(self) -> None:
        ensure_image()
        os.makedirs(SANDBOX_DIR, mode=0o700, exist_ok=True)
        self.share = tempfile.mkdtemp(prefix="vmshare-", dir=SANDBOX_DIR)
        self._write_spec()
        self._write_share()
        log = open(os.path.join(HOST_DIR, "vmd.log"), "w")
        try:
            os.unlink(VMD_SOCK)
        except FileNotFoundError:
            pass
        self.vmd = subprocess.Popen(["node", os.path.join(VM_DIR, "vmd.mjs"), self.spec_path], cwd=VM_DIR,
                                    stdout=log, stderr=subprocess.STDOUT, stdin=subprocess.DEVNULL,
                                    start_new_session=True)
        for _ in range(1200):              # first boot of a new image can take a while
            if os.path.exists(VMD_SOCK):
                return
            if self.vmd.poll() is not None:
                break
            time.sleep(0.1)
        self.stop()
        raise RuntimeError("VM did not start: " + open(log.name).read()[-600:])

    def _write_share(self) -> None:
        share = self.share
        guest_extra = os.path.join(VM_DIR, "guest", "nvim-site")
        if os.path.isdir(guest_extra):
            shutil.copytree(guest_extra, os.path.join(share, "nvim-site"))
        shutil.copytree(SANDBOX_BIN, os.path.join(share, "bin"))
        shutil.copytree(PI_EXT, os.path.join(share, "pi-extension"))
        _copy_fish(os.path.join(share, "fish"))
        aws_cfg = os.path.join(SANDBOX_DIR, "aws-config")
        if os.path.exists(aws_cfg):
            shutil.copy(aws_cfg, os.path.join(share, "aws-config"))
        for f in ("known_hosts", "id_ed25519.pub", "id_rsa.pub"):
            src = os.path.join(HOME, ".ssh", f)
            if os.path.exists(src):
                shutil.copy(src, os.path.join(share, f))
        gitcfg = os.path.join(HOME, ".gitconfig")
        if os.path.exists(gitcfg):
            # host paths (signingkey, allowedSignersFile, includeIf path) -> the guest home,
            # where the share's copies land (GUEST_SETUP)
            text = open(gitcfg).read().replace(HOME + "/", GUEST_HOME + "/").replace("~/", GUEST_HOME + "/")
            # `url.git@github.com:.insteadOf https://github.com/` would push every public
            # clone (lazy.nvim, mise, ...) through the ssh broker and your key; in the VM
            # https fetches go through the network policy instead. Remotes set to git@ keep ssh.
            text = _drop_url_rewrites(text)
            with open(os.path.join(share, "gitconfig"), "w") as f:
                f.write(text)
        gdir = os.path.join(HOME, ".config", "git")
        if os.path.isdir(gdir):
            os.makedirs(os.path.join(share, "git"), exist_ok=True)
            for f in ("allowed_signers", "newsuk.gitconfig", "ignore", "config", "attributes"):
                src = os.path.join(gdir, f)
                if os.path.isfile(src):
                    shutil.copy(src, os.path.join(share, "git", f))

    def _env(self, extra: dict) -> dict:
        bin_dir = "/gondo-pi/bin"
        env = {
            "HOME": GUEST_HOME, "USER": "root", "SHELL": "/usr/bin/fish", "LANG": "C.UTF-8",
            "PATH": f"{bin_dir}:/gondo-pi-cache/mise/shims:/gondo-pi-cache/cargo/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin",
            "PI_BROKER": "tcp://gondo-broker.internal:7070", "PI_BROKER_TOKEN": self.sup.vm_token,
            # gondolin's host-side ssh proxy does the real ssh; inside, plain openssh talks to it
            "GIT_SSH_COMMAND": "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null",
            "GIT_CONFIG_COUNT": "2",
            "GIT_CONFIG_KEY_0": "gpg.ssh.program", "GIT_CONFIG_VALUE_0": f"{bin_dir}/gondo-pi-sign",
            "GIT_CONFIG_KEY_1": "safe.directory", "GIT_CONFIG_VALUE_1": "*",
            "PI_SANDBOX": "gondo-pi", "GONDO_PI_PROJECT": os.path.basename(self.sup.cwd),
            "BROWSER": "/gondo-pi/bin/xdg-open",
            **({"GONDO_PI_ZELLIJ": "1"} if os.environ.get("GONDO_PI_ZELLIJ_SESSION") else {}),
            # persistent per-project caches (see _cache_dir); state/data stay throwaway
            "XDG_CACHE_HOME": "/gondo-pi-cache/xdg", "npm_config_cache": "/gondo-pi-cache/npm",
            "UV_CACHE_DIR": "/gondo-pi-cache/uv", "PIP_CACHE_DIR": "/gondo-pi-cache/pip",
            "CARGO_HOME": "/gondo-pi-cache/cargo", "GOMODCACHE": "/gondo-pi-cache/go/mod",
            "GOCACHE": "/gondo-pi-cache/go/build", "MISE_DATA_DIR": "/gondo-pi-cache/mise",
            "ZIG_GLOBAL_CACHE_DIR": "/gondo-pi-cache/zig",
            # gondolin's CA (it terminates TLS to apply the policy); node/aws need telling
            "NODE_EXTRA_CA_CERTS": "/run/gondolin/ca-certificates.crt",
            "AWS_CA_BUNDLE": "/run/gondolin/ca-certificates.crt",
            "GIT_SSL_CAINFO": "/run/gondolin/ca-certificates.crt",
        }
        if os.environ.get("GONDO_PI_ZJSTATUS"):
            env["PI_GONDO_ZJSTATUS_BAR"] = "1"
        env.update(extra)
        if os.path.exists(os.path.join(SANDBOX_DIR, "aws-config")):
            env["AWS_CONFIG_FILE"] = f"{GUEST_HOME}/.aws/config"
        return env

    def _write_spec(self) -> None:
        self.cfg = self.sup.cfg
        mem, cpus = _vm_size()
        proj = getattr(self.sup, "project_settings", {}) or {}
        pol, penv = policy.build(self.cfg, {**self.session, "project": proj})
        spec = {
            "socket": VMD_SOCK, "workspace": self.sup.cwd, "piDir": os.path.join(HOME, ".pi"),
            "cacheDir": self._cache_dir(), "dataDir": self._data_dir(), "nvim": self._nvim(),
            "shareDir": self.share, "guestHome": GUEST_HOME,
            "extraRead": {p: p for p in self.cfg.get("extra_read", []) if os.path.isdir(p)},
            # pi's code + config: read-only to the guest (copied into the VM at boot), so nothing
            # in the VM can change extensions/skills/settings that a host pi would load
            "piLinks": self._pi_links()[0], "piLinksRo": self._pi_links()[1],
            "piWritable": self._pi_writable(),
            "piNpm": self._pi_npm_dir(),
            "extraAllow": {p: p for p in self.cfg.get("extra_allow", []) if os.path.isdir(p)},
            "brokerPort": self.sup.vm_port, **pol,
            "ctlSocket": os.path.join(STATE_DIR, "run", "ctl.sock"),
            "sshHosts": ["github.com", "gitlab.com", "bitbucket.org", "ssh.github.com:443"],
            "agentSocket": self.sup.agent_sock, "knownHosts": os.path.join(HOME, ".ssh", "known_hosts"),
            "env": self._env(penv), "label": f"gondo-pi {os.path.basename(self.sup.cwd)}", "image": IMAGE_TAG,
            "memory": proj.get("vm", {}).get("memory") or self.cfg.get("vm", {}).get("memory", mem),
            "cpus": proj.get("vm", {}).get("cpus") or self.cfg.get("vm", {}).get("cpus", cpus),
            "autoForward": proj.get("ports", []),
            "setup": GUEST_SETUP,
        }
        write_private(self.spec_path, json.dumps(spec))

    def _pi_links(self) -> tuple[dict, dict]:
        """~/.pi/agent entries that are symlinks (e.g. -> ~/snippets/.pi/agent/skills) would dangle
        in the VM. Mount each target at the guest path the (relative) link resolves to, rw like
        ~/.pi itself; symlinked skills inside those dirs (-> other repos) get read-only mounts.
        Returns (rw mounts guest->host, ro mounts guest->host)."""
        rw, ro = {}, {}
        agent = os.path.join(HOME, ".pi", "agent")
        guest_agent = f"{GUEST_HOME}/.pi/agent"
        for name in sorted(os.listdir(agent)) if os.path.isdir(agent) else []:
            p = os.path.join(agent, name)
            if not os.path.islink(p):
                continue
            real = os.path.realpath(p)
            if not os.path.exists(real) or real.startswith(os.path.join(HOME, ".ssh")):
                continue
            guest = os.path.normpath(os.path.join(guest_agent, os.readlink(p)))
            if os.path.isabs(os.readlink(p)):
                guest = real        # absolute link: same path in the guest
            if os.path.isdir(real):
                rw[guest] = real
                for sub in sorted(os.listdir(real)):
                    sp = os.path.join(real, sub)
                    if os.path.islink(sp) and os.path.isdir(os.path.realpath(sp)):
                        t = os.readlink(sp)
                        g = t if os.path.isabs(t) else os.path.normpath(os.path.join(guest, t))
                        ro[g] = os.path.realpath(sp)
            else:
                # single file (models.json, ...): gondolin mounts directories only, so mount
                # the file's directory and let the existing relative link resolve into it
                rw[os.path.dirname(guest)] = os.path.dirname(real)
        return rw, ro

    def _pi_writable(self) -> dict:
        """The only parts of ~/.pi the VM writes back to the host: pi's data, never code or config.
        auth.json is not here: the supervisor merges it back (sync_pi_auth)."""
        agent = os.path.join(HOME, ".pi", "agent")
        out = {}
        for d in ("sessions", "fff", "missions", "teams", "web-search-cache", "pi-web-access", "guards"):
            hp = os.path.join(agent, d)
            if os.path.isdir(hp) and not os.path.islink(hp):
                out[f"{GUEST_HOME}/.pi/agent/{d}"] = hp
        mem = os.path.join(HOME, ".pi", "memory")
        if os.path.isdir(mem):
            out[f"{GUEST_HOME}/.pi/memory"] = mem
        return out

    def _pi_npm_dir(self) -> str:
        """pi's npm packages, installed for Linux (the host's ~/.pi/agent/npm has macOS native
        binaries, e.g. fff). Kept per image+lockfile; built inside the VM on first use."""
        d = os.path.join(STATE_DIR, "vm-pi-npm")
        os.makedirs(d, mode=0o700, exist_ok=True)
        return d

    def _data_dir(self) -> str:
        """Per-project shell state that should outlive the throwaway VM: fish history, zoxide."""
        d = os.path.join(PROJECTS_DIR, self.sup.project, "vm-data")
        for sub in ("fish", "zoxide"):
            os.makedirs(os.path.join(d, sub), mode=0o700, exist_ok=True)
        return d

    def _nvim(self) -> dict:
        """Your neovim: config read-only from the host (same files), plugins/Mason/parsers built
        for Linux in the VM and kept per image (host copies hold macOS binaries), state per project."""
        cfg = os.path.realpath(os.path.join(HOME, ".config", "nvim"))
        if not os.path.isdir(cfg):
            return {}
        data = os.path.join(STATE_DIR, "vm-nvim", os.uname().machine)   # holds nvim-data.tar
        state = os.path.join(PROJECTS_DIR, self.sup.project, "vm-data", "nvim-state")
        for d in (data, state):
            os.makedirs(d, mode=0o700, exist_ok=True)
        host_lazy = os.path.join(HOME, ".local", "share", "nvim", "lazy")
        return {"config": cfg, "data": data, "state": state,
                "hostLazy": host_lazy if os.path.isdir(host_lazy) else None}

    def _cache_dir(self) -> str:
        """Per-project tool caches (mise, uv, npm, cargo, go, pip): mounted at ~/.cache etc.
        in the guest so a throwaway rootfs still reinstalls fast. Downloads only, never secrets."""
        d = os.path.join(PROJECTS_DIR, self.sup.project, "vm-cache")
        os.makedirs(d, mode=0o700, exist_ok=True)
        return d

    def restart(self, why: str) -> None:
        """New credential route / new mount: new spec + recreated VM.
        Panes reconnect by themselves (shells start fresh, pi resumes with --continue)."""
        self.cfg = self.sup.cfg
        self._write_spec()
        aws_cfg = os.path.join(SANDBOX_DIR, "aws-config")
        dst = os.path.join(self.share, "aws-config")
        if os.path.exists(aws_cfg):
            shutil.copyfile(aws_cfg, dst)
        vmd_call({"op": "restart", "why": why}, timeout=120)

    def reload(self) -> None:
        """Allowlist edits etc.: new spec, same VM (applies to the next request)."""
        self.cfg = self.sup.cfg
        self._write_spec()
        vmd_call({"op": "reload"})

    def alive(self) -> bool:
        return bool(self.vmd and self.vmd.poll() is None)

    def stop(self) -> None:
        if self.vmd and self.vmd.poll() is None:
            self.vmd.terminate()
            try:
                # vmd saves auth + neovim plugins back to the host before it closes the VM
                self.vmd.wait(90)
            except subprocess.TimeoutExpired:
                self.vmd.kill()
        if self.share:
            shutil.rmtree(self.share, ignore_errors=True)
        for p in (VMD_SOCK, self.spec_path):
            try:
                os.unlink(p)
            except FileNotFoundError:
                pass


def vmd_call(obj: dict, timeout: float = 10) -> dict:
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.settimeout(timeout)
    s.connect(VMD_SOCK)
    s.sendall((json.dumps(obj) + "\n").encode())
    buf = b""
    while not buf.endswith(b"\n"):
        d = s.recv(65536)
        if not d:
            break
        buf += d
    s.close()
    return json.loads(buf or b'{"ok": false}')
