"""VM mode (trusted side): a Gondolin microVM devbox per nono-pi session.

`VmHost` owns, for the life of the session:
  - the VM daemon `vm/vmd.mjs`: one VM, many pty sessions (your fish shells, pi),
    reachable only over a host-side unix socket (RUN_DIR/vmd.sock, 0600). Its
    network policy (vm/netpolicy.mjs) is the guest's only HTTP(S) exit: allowlist,
    approvals, credential injection, AWS re-signing.
  - the read-only share mounted at /nono-pi (shims, pi extension, fish config)

The guest also reaches the supervisor broker (token-auth TCP) and ssh to git
hosts through Gondolin's host-side ssh proxy, using the supervisor's private
agent with every exec checked by the supervisor. Nothing secret is in the guest:
env holds per-session placeholders.

The rootfs is copy-on-write and throwaway: every session starts clean; only
/workspace (the project) and ~/.pi persist.
"""
from __future__ import annotations

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
IMAGE_TAG = "nono-pi:2"
VMD_SOCK = os.path.join(RUN_DIR, "vmd.sock")
FISH_DIR = os.path.join(HOME, ".config", "fish")
# host-only fish bits that make no sense (or would fail) in the VM
FISH_SKIP = {"conf.d/nono-pi.fish", "conf.d/saml2aws.fish", "conf.d/omf.fish", "conf.d/nvm.fish",
             "conf.d/00_fig_pre.fish", "conf.d/99_fig_post.fish", "fish_variables"}


def kvm_ok() -> tuple[bool, str]:
    if sys.platform == "darwin":
        return True, "hvf"
    if os.access("/dev/kvm", os.R_OK | os.W_OK):
        return True, "kvm"
    return False, "no /dev/kvm (nested virtualization off?)"


def ensure_image() -> None:
    r = subprocess.run(["npx", "--no-install", "gondolin", "image", "ls"], cwd=VM_DIR, capture_output=True, text=True)
    if any(l.split()[:1] == [IMAGE_TAG] for l in r.stdout.splitlines()):
        return
    print(f"nono-pi: building the VM image ({IMAGE_TAG}, one-time, a few minutes)…", file=sys.stderr)
    if os.uname().machine in ("arm64", "aarch64"):
        # macOS: Alpine image, built in a podman machine
        subprocess.check_call(["npx", "--no-install", "gondolin", "build", "--config", "image-arm64.json",
                               "--tag", IMAGE_TAG], cwd=VM_DIR)
        return
    # Linux x86_64: Arch devbox. Rootless: podman builds the userland, gondolin wraps it
    # (gondolin's own postBuild needs root for chroot).
    subprocess.check_call(["podman", "build", "-t", "localhost/nono-pi-arch:2", "-f", "Containerfile.arch", "."], cwd=VM_DIR)
    subprocess.check_call(["node", "patch-gondolin.mjs"], cwd=VM_DIR)
    subprocess.check_call(["npx", "--no-install", "gondolin", "build", "--config", "image-x86_64-arch.json",
                           "--tag", IMAGE_TAG], cwd=VM_DIR)


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
    with open(os.path.join(cd, "zz_nono_pi_vm.fish"), "w") as f:
        f.write(VM_FISH)


VM_FISH = r'''# nono-pi VM (generated): you are inside the microVM - no secrets in here.
set -g fish_greeting ""
set -gx EDITOR nvim
# host paths from your config don't exist here; keep the VM's own PATH first
set -gx PATH /nono-pi/bin /nono-pi-cache/mise/shims /nono-pi-cache/cargo/bin /usr/local/bin /usr/bin /bin /usr/sbin /sbin $HOME/.local/bin $HOME/go/bin
command -q mise; and mise activate fish | source
command -q direnv; and direnv hook fish | source
command -q zoxide; and zoxide init fish | source
# pure: the VM tag replaces its container/ssh user@host segment
set -g pure_enable_container_detection false
functions -q fish_prompt; and functions -c fish_prompt __nonopi_orig_prompt
function fish_prompt
    set -l s $status
    set_color -b 363a4f 8bd5ca; printf ' ⬡ vm '; set_color normal; printf ' '
    if functions -q __nonopi_orig_prompt
        __nonopi_status $s
        __nonopi_orig_prompt
    else
        set_color 8aadf4; printf '%s' (prompt_pwd); set_color normal
        test $s -ne 0; and begin; set_color ed8796; printf ' [%s]' $s; set_color normal; end
        printf ' ❯ '
    end
end
function __nonopi_status --argument-names s
    return $s
end
function fish_title
    echo (string replace -r '^/workspace' "$NONO_PI_PROJECT" -- $PWD)
end
if status is-interactive; and test "$NONO_PI_VM_SESSION" = 1
    set_color 6e738d
    echo "⬡ nono-pi VM · $NONO_PI_PROJECT · throwaway Linux devbox · no secrets in here"
    if set -q NONO_PI_ZELLIJ
        echo "  type pi to start pi · git / gh / aws / npm just work · requests wait in the nono-pi tab (Alt-2)"
    else
        echo "  type pi to start pi · git / gh / aws / npm just work · answer requests with: nono-pi panel (on the host)"
    end
    set_color normal
end
'''

GUEST_SETUP = r'''set -e
hostname nono-pi-vm 2>/dev/null || true; echo nono-pi-vm > /etc/hostname 2>/dev/null || true
grep -q nono-pi-vm /etc/hosts 2>/dev/null || echo "127.0.1.1 nono-pi-vm" >> /etc/hosts
mkdir -p /home/pi/.config /home/pi/.local/bin
rm -rf /home/pi/.config/fish && cp -R /nono-pi/fish /home/pi/.config/fish
[ -f /nono-pi/gitconfig ] && cp /nono-pi/gitconfig /home/pi/.gitconfig || true
mkdir -p /home/pi/.ssh && for f in known_hosts id_ed25519.pub id_rsa.pub; do [ -f /nono-pi/$f ] && cp /nono-pi/$f /home/pi/.ssh/ || true; done
[ -f /nono-pi/aws-config ] && mkdir -p /home/pi/.aws && cp /nono-pi/aws-config /home/pi/.aws/config || true
'''


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
        for _ in range(300):
            if os.path.exists(VMD_SOCK):
                return
            if self.vmd.poll() is not None:
                break
            time.sleep(0.1)
        self.stop()
        raise RuntimeError("VM did not start: " + open(log.name).read()[-600:])

    def _write_share(self) -> None:
        share = self.share
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
            shutil.copy(gitcfg, os.path.join(share, "gitconfig"))

    def _env(self, extra: dict) -> dict:
        bin_dir = "/nono-pi/bin"
        env = {
            "HOME": GUEST_HOME, "USER": "root", "SHELL": "/usr/bin/fish", "LANG": "C.UTF-8",
            "PATH": f"{bin_dir}:/nono-pi-cache/mise/shims:/nono-pi-cache/cargo/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin",
            "PI_BROKER": "tcp://nono-broker.internal:7070", "PI_BROKER_TOKEN": self.sup.vm_token,
            # gondolin's host-side ssh proxy does the real ssh; inside, plain openssh talks to it
            "GIT_SSH_COMMAND": "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null",
            "GIT_CONFIG_COUNT": "2",
            "GIT_CONFIG_KEY_0": "gpg.ssh.program", "GIT_CONFIG_VALUE_0": f"{bin_dir}/nono-pi-sign",
            "GIT_CONFIG_KEY_1": "safe.directory", "GIT_CONFIG_VALUE_1": "*",
            "PI_SANDBOX": "nono-pi", "NONO_PI_PROJECT": os.path.basename(self.sup.cwd),
            **({"NONO_PI_ZELLIJ": "1"} if os.environ.get("NONO_PI_ZELLIJ_SESSION") else {}),
            # persistent per-project caches (see _cache_dir); state/data stay throwaway
            "XDG_CACHE_HOME": "/nono-pi-cache/xdg", "npm_config_cache": "/nono-pi-cache/npm",
            "UV_CACHE_DIR": "/nono-pi-cache/uv", "PIP_CACHE_DIR": "/nono-pi-cache/pip",
            "CARGO_HOME": "/nono-pi-cache/cargo", "GOMODCACHE": "/nono-pi-cache/go/mod",
            "GOCACHE": "/nono-pi-cache/go/build", "MISE_DATA_DIR": "/nono-pi-cache/mise",
            "ZIG_GLOBAL_CACHE_DIR": "/nono-pi-cache/zig",
            # gondolin's CA (it terminates TLS to apply the policy); node/aws need telling
            "NODE_EXTRA_CA_CERTS": "/run/gondolin/ca-certificates.crt",
            "AWS_CA_BUNDLE": "/run/gondolin/ca-certificates.crt",
            "GIT_SSL_CAINFO": "/run/gondolin/ca-certificates.crt",
        }
        if os.environ.get("NONO_PI_ZJSTATUS"):
            env["PI_NONO_ZJSTATUS_BAR"] = "1"
        env.update(extra)
        if os.path.exists(os.path.join(SANDBOX_DIR, "aws-config")):
            env["AWS_CONFIG_FILE"] = f"{GUEST_HOME}/.aws/config"
        return env

    def _write_spec(self) -> None:
        self.cfg = self.sup.cfg
        mem, cpus = _vm_size()
        pol, penv = policy.build(self.cfg, self.session)
        spec = {
            "socket": VMD_SOCK, "workspace": self.sup.cwd, "piDir": os.path.join(HOME, ".pi"),
            "cacheDir": self._cache_dir(),
            "shareDir": self.share, "guestHome": GUEST_HOME,
            "extraRead": {**self._pi_links()[1], **{p: p for p in self.cfg.get("extra_read", []) if os.path.isdir(p)}},
            "piLinks": self._pi_links()[0],
            "piNpm": self._pi_npm_dir(),
            "extraAllow": {p: p for p in self.cfg.get("extra_allow", []) if os.path.isdir(p)},
            "brokerPort": self.sup.vm_port, **pol,
            "ctlSocket": os.path.join(STATE_DIR, "run", "ctl.sock"),
            "sshHosts": ["github.com", "gitlab.com", "bitbucket.org", "ssh.github.com:443"],
            "agentSocket": self.sup.agent_sock, "knownHosts": os.path.join(HOME, ".ssh", "known_hosts"),
            "env": self._env(penv), "label": f"nono-pi {os.path.basename(self.sup.cwd)}", "image": IMAGE_TAG,
            "memory": self.cfg.get("vm", {}).get("memory", mem), "cpus": self.cfg.get("vm", {}).get("cpus", cpus),
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

    def _pi_npm_dir(self) -> str:
        """pi's npm packages, installed for Linux (the host's ~/.pi/agent/npm has macOS native
        binaries, e.g. fff). Kept per image+lockfile; built inside the VM on first use."""
        d = os.path.join(STATE_DIR, "vm-pi-npm")
        os.makedirs(d, mode=0o700, exist_ok=True)
        return d

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
                self.vmd.wait(6)
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
