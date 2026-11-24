"""nono-pi launcher (trusted side).

Flow:
  pis                   outer: pick access (remembered per project), then start/attach
                        zellij session `pi-<project>`: tab "shell" runs `nono-pi --inner`
                        (supervisor + VM, then your fish shell in it), tab "nono-pi" the
                        panel. Type `pi` in any VM shell.
  pis --no-zellij       the same in this terminal: your fish shell in the VM, requests
                        answered in `nono-pi panel` in another terminal (or a dialog).
  pis -- <pi args>      no zellij, run pi directly in the VM (scripts: -p, --mode ...).
"""
from __future__ import annotations

import json
import os
import re
import shutil
import signal
import subprocess
import sys
import time

from . import proto
from .paths import BIN, HOME, PROJECTS_DIR, RUN_DIR, load_config, project_id, write_private

RESTART_EXIT = 75   # pi-extension: exit with this to restart pi with new access


def die(msg: str):
    sys.exit(f"nono-pi: {msg}")


# ------------------------------------------------------------------ choices
def choices_path(pid: str) -> str:
    return os.path.join(PROJECTS_DIR, pid, "choices.json")


def load_choices(pid: str) -> dict | None:
    try:
        return json.load(open(choices_path(pid)))
    except (OSError, ValueError):
        return None


def pick_accounts(cfg: dict, preset: list[str] | None) -> list[str]:
    aws = cfg["aws"]
    accounts = sorted(aws.get("accounts", {}))
    if not aws.get("enabled") or not accounts:
        return []
    prod = set(aws.get("prod_accounts", []))
    label = lambda n: n + ("  [PROD]" if aws["accounts"][n]["role_arn"].split(":")[4] in prod else "")
    default = aws.get("default_account") if aws.get("default_account") in accounts else accounts[0]
    first = [a for a in (preset or [default]) if a in accounts] or [default]
    ordered = first + [a for a in accounts if a not in first]
    if shutil.which("fzf"):
        r = subprocess.run(["fzf", "--multi", "--height=~50%", "--reverse", "--prompt=AWS> ",
                            "--header=nono-pi: AWS account(s) to pre-approve (TAB = several, Enter = top, ESC = none).\n"
                                      "Others still work: they ask on first use."],
                           input="\n".join(label(a) for a in ordered), stdout=subprocess.PIPE, text=True)
        return [l.split()[0] for l in r.stdout.splitlines() if l.strip()]
    print("nono-pi: AWS account(s) to pre-approve (others ask on first use):", file=sys.stderr)
    for i, a in enumerate(ordered, 1):
        print(f"  {i}) {label(a)}", file=sys.stderr)
    print("  0) none", file=sys.stderr)
    ans = input("choose [1, e.g. 1,3]: ").strip() or "1"
    if ans == "0":
        return []
    try:
        return [ordered[int(x) - 1] for x in ans.replace(" ", "").split(",")]
    except (ValueError, IndexError):
        die("invalid choice")


def resolve_choices(cfg: dict, pid: str, argv: list[str]) -> tuple[dict, list[str]]:
    """--aws a,b|none, -c/--configure. Remembered per project."""
    rest, aws_arg, configure = [], None, False
    argv = [a for a in argv if a not in ("--no-zellij", "--zellij")]
    it = iter(argv)
    for a in it:
        if a == "--aws":
            aws_arg = next(it, "")
        elif a.startswith("--aws="):
            aws_arg = a.split("=", 1)[1]
        elif a in ("--configure", "--pick"):
            configure = True
        elif a in ("--vm", "--no-vm", "--nono"):
            if a != "--vm":
                die("the nono sandbox is gone: nono-pi always runs in the microVM now")
        else:
            rest.append(a)
    saved = load_choices(pid) or {}
    ch = dict(saved)
    accounts = cfg["aws"].get("accounts", {})
    if aws_arg is not None:
        picked = [] if aws_arg in ("none", "") else [x for x in aws_arg.split(",") if x]
        bad = [x for x in picked if x not in accounts]
        if bad:
            die(f"unknown AWS account(s): {', '.join(bad)} (have: {', '.join(sorted(accounts))})")
        ch["aws"] = picked
    elif configure and sys.stdin.isatty() and sys.stderr.isatty():
        ch["aws"] = pick_accounts(cfg, saved.get("aws"))
    # no picker at launch: accounts are chosen on demand (`aws-use` in the VM, or the panel)
    ch.pop("vm", None)
    ch["aws"] = [a for a in ch.get("aws", []) if a in accounts]
    if ch != saved:
        write_private(choices_path(pid), json.dumps(ch, indent=2))
    return ch, rest


# ------------------------------------------------------------------ outer
def outer(argv: list[str]) -> None:
    if os.environ.get("PI_SANDBOX") == "nono-pi":
        die("already inside a nono-pi sandbox")
    from .vm import kvm_ok
    ok, why = kvm_ok()
    if not ok:
        die(f"needs a hypervisor for the microVM: {why}")
    cfg = load_config()
    cwd = os.getcwd()
    bad = _bad_workdir(cwd)
    if bad:
        die(bad)
    pid = project_id(cwd)
    if argv[:1] == ["--print-policy"]:
        from . import policy
        ch, _ = resolve_choices(cfg, pid, argv[1:] + ([] if "--aws" in argv else ["--aws", ",".join((load_choices(pid) or {}).get("aws", [])) or "none"]))
        p, env = policy.build(cfg, {"aws": ch["aws"]})
        print(json.dumps({"policy": p, "guest_env": env}, indent=2))
        return
    no_zellij = "--no-zellij" in argv or (not cfg.get("zellij", True) and "--zellij" not in argv)
    ch, rest = resolve_choices(cfg, pid, argv)
    # `pis -- <pi args>`: pi itself in the VM, in this terminal (scripts, -p, --mode json ...)
    run_pi = "--" in argv
    inner_cmd = [sys.executable, os.path.join(BIN, "nono-pi"), "--inner", "--project", pid,
                 "--aws", ",".join(ch["aws"]) or "none"] + (["--pi"] if run_pi else []) + ["--"] + rest
    if run_pi or no_zellij or not shutil.which("zellij") or not sys.stdout.isatty():
        os.execv(sys.executable, inner_cmd)
    sess = "pi-" + pid
    if os.environ.get("ZELLIJ"):
        # already in a zellij session: run inner here; requests show in this session's panel
        os.environ["NONO_PI_ZELLIJ_SESSION"] = os.environ.get("ZELLIJ_SESSION_NAME", "")
        os.environ["NONO_PI_IN_USER_ZELLIJ"] = "1"
        os.execv(sys.executable, inner_cmd)
    if _zellij_alive(sess):
        os.execvp("zellij", ["zellij", "attach", sess])
    layout = os.path.join(RUN_DIR, f"layout-{pid}.kdl")
    os.makedirs(RUN_DIR, mode=0o700, exist_ok=True)
    q = lambda xs: " ".join(json.dumps(a) for a in xs)
    me = [sys.executable, os.path.join(BIN, "nono-pi")]
    # Same look as your normal zellij: the shared zjstatus bar (if you have it)
    # with nono-pi's state in its {pipe_nonopi} slot; otherwise stock bars plus a
    # 1-line nono-pi status pane.
    bar = _zjstatus_bar()
    if bar:
        template = bar + "\n        children"
    else:
        template = """pane size=1 borderless=true {
            plugin location="zellij:tab-bar"
        }
        children
        pane size=2 borderless=true {
            plugin location="zellij:status-bar"
        }"""
    env_args = f'"NONO_PI_ZELLIJ_SESSION={sess}" "NONO_PI_HOLD_ON_ERROR=1" "NONO_PI_ZJSTATUS={'1' if bar else ''}"'
    # every new pane / tab in this session = another fish shell in the same VM
    vm_shell = os.path.join(RUN_DIR, "vm-shell")
    write_private(vm_shell, f"#!/bin/sh\nexec {json.dumps(sys.executable)} {json.dumps(os.path.join(BIN, 'nono-pi'))} --vm-shell\n")
    os.chmod(vm_shell, 0o700)
    write_private(layout, f"""layout {{
    default_tab_template {{
    {template}
    }}
    tab name="shell" focus=true {{
    pane name="⬡ vm" focus=true command="env" {{
        args {env_args} {q(inner_cmd)}
        close_on_exit true
    }}
    }}
    tab name="nono-pi" {{
    pane name="🔒 nono-pi control" command={json.dumps(me[0])} {{
        args {q(me[1:] + ["--panel"])}
    }}
    }}
}}
""")
    _zellij_forget(sess)
    zj = ["zellij"]
    cfgfile = _session_zellij_config(pid, vm_shell)
    if cfgfile:
        zj += ["--config", cfgfile]
    os.execvp("zellij", zj + ["--session", sess, "--new-session-with-layout", layout,
                              "options", "--show-startup-tips", "false", "--show-release-notes", "false",
                              "--default-mode", "normal", "--default-shell", vm_shell])


ZJSTATUS_PART = os.path.join(HOME, ".config", "zellij", "parts", "zjstatus-bar.kdl")
ZJSTATUS_WASM = os.path.join(HOME, ".config", "zellij", "plugins", "zjstatus.wasm")


# Alt-a in a nono-pi session: the AWS account picker (fzf) in a floating pane, in the VM.
_AWS_BIND = ('        bind "Alt a" {{ Run {cmd} {{ floating true; close_on_exit true; name "AWS account"; '
             'width "80%"; height "50%"; x "10%"; y "15%"; }}; }}')


def _session_zellij_config(pid: str, vm_shell: str) -> str | None:
    """Your zellij config + nono-pi's session keys (Alt-a = aws-use), for this session only.
    Your own config file is never changed. Returns None if there is nothing to extend."""
    src = os.environ.get("ZELLIJ_CONFIG_FILE") or os.path.join(HOME, ".config", "zellij", "config.kdl")
    try:
        text = open(src).read()
    except OSError:
        text = ""
    cmd = " ".join(json.dumps(a) for a in [sys.executable, os.path.join(BIN, "nono-pi"), "--vm-run", "aws-use"])
    bind = _AWS_BIND.format(cmd=cmd)
    if 'bind "Alt a"' in text:
        return None                    # you bound Alt-a yourself: leave it alone
    # zellij 0.46 only reads the FIRST `keybinds` block, so ours goes inside yours
    block = '    shared_except "locked" {\n' + bind + '\n    }'
    m = re.search(r'^keybinds\b[^{\n]*\{', text, re.M)
    if m:
        text = text[:m.end()] + "\n" + block + text[m.end():]
    else:
        text += "\n// nono-pi session keys\nkeybinds {\n" + block + "\n}\n"
    out = os.path.join(RUN_DIR, f"zellij-{pid}.kdl")
    write_private(out, text)
    return out


def _zjstatus_bar() -> str | None:
    """Your zjstatus bar (shared with ~/.config/zellij/layouts/hugo.kdl), if installed."""
    if not (os.path.exists(ZJSTATUS_PART) and os.path.exists(ZJSTATUS_WASM)):
        return None
    lines = [l for l in open(ZJSTATUS_PART).read().splitlines() if not l.lstrip().startswith("//")]
    return "\n        ".join(lines).strip()


def _bad_workdir(cwd: str) -> str | None:
    real = os.path.realpath(cwd)
    home = os.path.realpath(HOME)
    if real in ("/", home) or home.startswith(real + os.sep):
        return (f"run pis inside a project directory, not {cwd} "
                "(pi gets read-write access to the current directory; nono refuses your whole home)")
    return None


def _zellij_forget(sess: str) -> None:
    subprocess.run(["zellij", "delete-session", sess], capture_output=True)


# ------------------------------------------------------------------ inner
def inner(argv: list[str]) -> None:
    from .supervisor import Supervisor
    pid, aws, rest, run_pi = None, [], [], False
    it = iter(argv)
    for a in it:
        if a == "--pi":
            run_pi = True
        elif a == "--project":
            pid = next(it)
        elif a == "--aws":
            v = next(it)
            aws = [] if v == "none" else v.split(",")
        elif a == "--":
            rest = list(it)
    cwd = os.getcwd()
    pid = pid or project_id(cwd)
    cfg = load_config()
    other = _supervisor_running()
    if other:
        where = other.get("project", "?")
        zs = other.get("zellij_session") or ""
        hint = f"`pis` in that project, or `zellij attach {zs}`" if zs else "`pis` in that project"
        print(f"nono-pi: another nono-pi session is running for project '{where}' (pid {other.get('pid', '?')}); "
              f"one at a time for now. Reattach with {hint}, or quit it there.", file=sys.stderr)
        _hold_if_error(1)
        sys.exit(1)
    sup = Supervisor(pid, cwd, os.path.basename(cwd), vm=True)
    sup.picked_aws = aws
    sup.aws_current = aws[0] if aws else None       # pis --aws a,b: a is current from the start
    sup.serve()
    return _inner_vm(sup, cfg, aws, rest, run_pi)


def _inner_vm(sup, cfg: dict, aws: list[str], rest: list[str], run_pi: bool = False) -> None:
    """The supervisor + the session's VM live in this process. You land in fish
    inside the VM (in zellij: the main shell pane; every other pane is another
    shell in the same VM). Exiting this shell ends the session. With run_pi
    (`pis -- <pi args>`): pi itself in the VM, then exit."""
    from . import attach
    from .vm import VmHost

    def _bye(*_):
        raise SystemExit(0)
    signal.signal(signal.SIGTERM, _bye)
    signal.signal(signal.SIGHUP, _bye)
    host = VmHost(sup, cfg, {"aws": aws})
    sup.vmhost = host
    rc = 0
    try:
        if sys.stderr.isatty():
            sys.stderr.write("\033[38;2;110;115;141m⬡ starting the nono-pi VM…\033[0m")
            sys.stderr.flush()
        host.start()
        if sys.stderr.isatty():
            sys.stderr.write("\r\033[K")
        if run_pi:
            rc, _ = attach.run("pi", rest)
        else:
            if not os.environ.get("NONO_PI_ZELLIJ_SESSION") and sys.stderr.isatty():
                sys.stderr.write("\033[38;2;110;115;141mnono-pi: requests (push, new host, AWS login, secrets) wait for "
                                 "`nono-pi panel` in another terminal (or a dialog if you have a desktop)\033[0m\n")
            rc = attach.shell_loop(main=True)
    except SystemExit as e:
        rc = e.code if isinstance(e.code, int) else 0
    except BaseException as e:
        print(f"\nnono-pi: {type(e).__name__}: {e}", file=sys.stderr)
        _hold_if_error(1)
        rc = 1
    finally:
        host.stop()
        sup.shutdown()
    _end_zellij_session()
    sys.exit(rc)


# pi options that take a value (so their value is not mistaken for a prompt)
_PI_VALUE_OPTS = {"--provider", "--model", "--api-key", "--system-prompt", "--append-system-prompt",
                  "--thinking", "--mode", "--session", "--session-id", "--fork", "--session-dir",
                  "--name", "-n", "--export", "-e", "--extension", "--tools", "--models",
                  "--prompt-template", "--skill", "--theme"}


def _end_zellij_session() -> None:
    """The main VM shell exited: close the whole nono-pi zellij session (and the VM).
    Detaching (zellij session mode `d`) keeps everything running instead."""
    sess = os.environ.get("NONO_PI_ZELLIJ_SESSION", "")
    if sess.startswith("pi-") and not os.environ.get("NONO_PI_IN_USER_ZELLIJ"):
        # detached + delayed: killing the session kills this pane (and us) mid-cleanup otherwise
        subprocess.Popen(["/bin/sh", "-c", 'sleep 0.3; zellij kill-session "$1"; zellij delete-session "$1"', "sh", sess],
                         stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                         start_new_session=True, env={k: v for k, v in os.environ.items() if not k.startswith("ZELLIJ")})


def _hold_if_error(rc: int) -> None:
    """Inside zellij the pane would just vanish; keep the error on screen."""
    if rc not in (0, None) and os.environ.get("NONO_PI_HOLD_ON_ERROR") and sys.stdin.isatty():
        print(f"\nnono-pi: pi exited with status {rc}. Press Enter to close this session.", file=sys.stderr)
        try:
            input()
        except (EOFError, KeyboardInterrupt):
            pass


def _drop_initial_prompts(args: list[str]) -> list[str]:
    """On restart (--continue) keep all options, drop positional prompt text."""
    out, i = [], 0
    while i < len(args):
        a = args[i]
        if a in _PI_VALUE_OPTS and i + 1 < len(args):
            out += [a, args[i + 1]]
            i += 2
            continue
        if a.startswith("-"):
            out.append(a)
        i += 1
    return out


def _zellij_alive(name: str) -> bool:
    try:
        out = subprocess.run(["zellij", "list-sessions", "--no-formatting", "--short"],
                             capture_output=True, text=True, timeout=5).stdout.split()
    except (OSError, subprocess.SubprocessError):
        return True     # can't tell: assume alive, never kill a live session
    return name in out


def _supervisor_running() -> dict | None:
    """The status of the live supervisor, or None. A supervisor whose zellij session is gone
    (crashed / killed from outside) is an orphan: ask it to stop, then report free."""
    try:
        st = proto.call(proto.CTL, {"op": "status"}, timeout=2)
    except OSError:
        return None
    if not st.get("ok"):
        return None
    zs = st.get("zellij_session") or ""
    if zs.startswith("pi-") and not _zellij_alive(zs):
        print(f"nono-pi: cleaning up an orphaned session for '{st.get('project')}' "
              f"(its zellij session {zs} is gone)…", file=sys.stderr)
        try:
            proto.call(proto.CTL, {"op": "shutdown-orphan"}, timeout=5)
        except OSError:
            pass
        for _ in range(50):
            time.sleep(0.1)
            try:
                proto.call(proto.CTL, {"op": "status"}, timeout=1)
            except OSError:
                return None
        pid = st.get("pid")
        if pid:
            try:
                os.kill(int(pid), signal.SIGTERM)
                time.sleep(1)
            except OSError:
                pass
        try:
            proto.call(proto.CTL, {"op": "status"}, timeout=1)
        except OSError:
            return None
    return st


def main(argv: list[str]) -> None:
    if argv[:1] == ["--inner"]:
        return inner(argv[1:])
    if argv[:1] == ["--vm-shell"]:
        from . import attach
        sys.exit(attach.shell_loop())
    if argv[:1] == ["--vm-run"] and argv[1:]:
        # one command in the session's VM, e.g. `aws-use` from Alt-a (fish -lc: your env + PATH)
        from . import attach
        if not attach.wait_for_vm(30):
            sys.exit("nono-pi: the VM is not running")
        rc, _ = attach.run("cmd", ["fish", "-lc", " ".join(argv[1:])])
        sys.exit(rc)
    if argv[:1] in (["--panel"], ["panel"]):
        from . import panel
        return panel.main()
    if argv[:1] in (["setup"], ["--setup"]):
        from . import setup
        return setup.main(argv[1:])
    if argv[:1] == ["--init-config"]:
        load_config()
        return
    return outer(argv)
