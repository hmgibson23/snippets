"""One-time setup (trusted side, run in your terminal): `nono-pi setup`.

1. Create the dedicated nono-pi keychain (macOS) with its own password,
   auto-lock after the grant window and on sleep.
2. Migrate secrets the v1 helper stored in the login keychain (service nono-pi).
3. ssh keys without a passphrase: back them up, add a passphrase
   (generated, stored only in the nono-pi keychain), so a copied key file is
   useless. Your own ssh keeps working via the macOS login keychain
   (UseKeychain) — we add the passphrase there too.
"""
from __future__ import annotations

import getpass
import os
import secrets
import shutil
import subprocess
import sys
import time

from . import keystore
from .paths import HOME, MAC, load_config


def say(msg: str) -> None:
    print(f"nono-pi setup: {msg}")


def ask_yes(q: str, default: bool = True) -> bool:
    a = input(f"{q} [{'Y/n' if default else 'y/N'}] ").strip().lower()
    return default if not a else a.startswith("y")


def ensure_keychain(store, lock_after: int) -> None:
    if not MAC:
        say("Linux: using Secret Service (secret-tool); nothing to create")
        return
    if store.exists():
        say(f"keychain exists: {keystore.KEYCHAIN}")
        store.set_lock_after(lock_after)
        if not store.is_unlocked():
            pw = getpass.getpass("nono-pi keychain password: ")
            if not store.unlock(pw):
                sys.exit("wrong password")
        return
    say("creating the nono-pi keychain. Choose a password you'll type about once per "
        f"{lock_after // 3600}h (it is separate from your login password).")
    while True:
        p1 = getpass.getpass("new nono-pi keychain password: ")
        p2 = getpass.getpass("again: ")
        if p1 and p1 == p2 and len(p1) >= 8:
            break
        print("passwords differ or are shorter than 8 characters")
    store.create(p1, lock_after)
    say("created (auto-locks after the window and on sleep)")


def migrate_login_keychain(store) -> None:
    if not MAC:
        return
    r = subprocess.run(["/usr/bin/security", "dump-keychain"], capture_output=True, text=True,
                       stdin=subprocess.DEVNULL)
    names, acct = [], None
    for line in r.stdout.splitlines():
        line = line.strip()
        if line.startswith("keychain:"):
            acct = None
        elif line.startswith('"acct"<blob>='):
            acct = line.split("=", 1)[1].strip('"')
        elif line == '"svce"<blob>="nono-pi"' and acct:
            names.append(acct)
    for n in sorted(set(names)):
        if n.startswith("aws_session_"):
            subprocess.run(["/usr/bin/security", "delete-generic-password", "-s", "nono-pi", "-a", n],
                           capture_output=True)
            continue
        v = subprocess.run(["/usr/bin/security", "find-generic-password", "-s", "nono-pi", "-a", n, "-w"],
                           capture_output=True, text=True).stdout.rstrip("\n")
        if v:
            store.set(n, v)
            subprocess.run(["/usr/bin/security", "delete-generic-password", "-s", "nono-pi", "-a", n],
                           capture_output=True)
            say(f"moved {n} from the login keychain")


def passphrase_keys(store, keys: list[str]) -> None:
    for kp in keys:
        path = os.path.expanduser(kp)
        if not os.path.exists(path):
            continue
        if subprocess.run(["ssh-keygen", "-y", "-P", "", "-f", path], capture_output=True).returncode != 0:
            say(f"{path}: already has a passphrase")
            name = f"ssh_passphrase:{os.path.basename(path)}"
            if not store.get(name):
                pp = getpass.getpass(f"passphrase for {path} (stored in the nono-pi keychain): ")
                store.set(name, pp)
            continue
        if not ask_yes(f"{path} has NO passphrase: anything running as you can copy it. Add one now?"):
            continue
        backup = f"{path}.nono-pi-backup-{time.strftime('%Y%m%d%H%M%S')}"
        shutil.copy2(path, backup)
        os.chmod(backup, 0o600)
        if MAC:
            pp = secrets.token_urlsafe(24)   # also goes into the login keychain below
        else:
            # Linux: no login-keychain equivalent wired up for your own ssh, so you choose it
            # (you type it for ssh/git outside nono-pi; nono-pi keeps it in the Secret Service)
            while True:
                pp = getpass.getpass(f"new passphrase for {path} (min 10 chars): ")
                if len(pp) >= 10 and pp == getpass.getpass("again: "):
                    break
                print("passphrases differ or are too short")
        r = subprocess.run(["ssh-keygen", "-p", "-q", "-P", "", "-N", pp, "-f", path], capture_output=True, text=True)
        if r.returncode != 0:
            say(f"failed: {r.stderr.strip()} (backup kept at {backup})")
            continue
        store.set(f"ssh_passphrase:{os.path.basename(path)}", pp)
        if MAC:
            # so your own ssh/git (outside nono-pi) keeps working without typing it
            ask = os.path.join(os.path.dirname(backup), ".nono-pi-askpass")
            with open(ask, "w") as f:
                f.write("#!/bin/sh\nprintf '%s\\n' \"$NPK\"\n")
            os.chmod(ask, 0o700)
            env = dict(os.environ, SSH_ASKPASS=ask, SSH_ASKPASS_REQUIRE="force", DISPLAY=":0", NPK=pp)
            subprocess.run(["/usr/bin/ssh-add", "--apple-use-keychain", path], env=env, capture_output=True,
                           stdin=subprocess.DEVNULL)
            os.unlink(ask)
        say(f"{path}: passphrase added ({'random, kept in the nono-pi keychain and the login keychain for your own ssh' if MAC else 'kept in the Secret Service for nono-pi; you type it for your own ssh'}). Backup: {backup}")
        say(f"  verify, then delete the backup:  ssh -T git@github.com && rm {backup}")


def main(argv: list[str]) -> None:
    if not sys.stdin.isatty():
        sys.exit("run `nono-pi setup` in a terminal")
    cfg = load_config()
    store = keystore.open_store()
    ensure_keychain(store, int(cfg["keychain"]["lock_after"]))
    migrate_login_keychain(store)
    passphrase_keys(store, cfg["ssh"]["keys"])
    say("done. Start a session with `pis` in a project.")
