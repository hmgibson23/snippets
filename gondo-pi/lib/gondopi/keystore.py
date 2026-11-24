"""Secret storage (trusted side only).

macOS: a dedicated keychain file `gondo-pi.keychain-db`, driven through
Security.framework with user interaction disabled (no system dialogs: the
`security` CLI on a locked custom keychain pops an un-typeable modal). The
keychain auto-locks after the grant window and on sleep; gondo-pi unlocks it
with a password the user types into a gondo-pi prompt.

Linux: Secret Service via `secret-tool` (attribute service=gondo-pi). Devbox
fallback (no Secret Service) TBD; `GONDO_PI_KEYSTORE=file` uses an in-memory
store for tests.
"""
from __future__ import annotations

import ctypes
import ctypes.util
import os
import shutil
import subprocess

from .paths import HOME, MAC

SERVICE = b"gondo-pi"
LEGACY_SERVICE = b"nono-pi"     # items written before the rename (read, then rewritten under SERVICE)
KEYCHAIN = os.path.join(HOME, "Library", "Keychains", "gondo-pi.keychain-db")
LEGACY_KEYCHAIN = os.path.join(HOME, "Library", "Keychains", "nono-pi.keychain-db")

errSecItemNotFound = -25300
errSecInteractionNotAllowed = -25308
errSecAuthFailed = -25293
errSecNoSuchKeychain = -25294


class Locked(Exception):
    pass


class _Mac:
    def __init__(self, path: str = KEYCHAIN):
        self.path = path.encode()
        self.S = ctypes.cdll.LoadLibrary(ctypes.util.find_library("Security"))
        self.S.SecKeychainSetUserInteractionAllowed(ctypes.c_bool(False))
        self._kc = None

    # --- keychain lifecycle
    def exists(self) -> bool:
        return os.path.exists(self.path.decode())

    def _open(self):
        if self._kc is None:
            kc = ctypes.c_void_p()
            rc = self.S.SecKeychainOpen(self.path, ctypes.byref(kc))
            if rc != 0:
                raise OSError(f"SecKeychainOpen {rc}")
            self._kc = kc
        return self._kc

    def create(self, password: str, lock_after: int) -> None:
        kc = ctypes.c_void_p()
        pw = password.encode()
        rc = self.S.SecKeychainCreate(self.path, len(pw), pw, False, None, ctypes.byref(kc))
        if rc != 0:
            raise OSError(f"SecKeychainCreate {rc}")
        self._kc = kc
        self.set_lock_after(lock_after)

    def set_lock_after(self, secs: int) -> None:
        # SecKeychainSettings { UInt32 version; Boolean lockOnSleep; Boolean useLockInterval; UInt32 lockInterval; }
        class Settings(ctypes.Structure):
            _fields_ = [("version", ctypes.c_uint32), ("lockOnSleep", ctypes.c_bool),
                        ("useLockInterval", ctypes.c_bool), ("lockInterval", ctypes.c_uint32)]
        s = Settings(1, True, True, int(secs))
        self.S.SecKeychainSetSettings(self._open(), ctypes.byref(s))

    def is_unlocked(self) -> bool:
        st = ctypes.c_uint32()
        if self.S.SecKeychainGetStatus(self._open(), ctypes.byref(st)) != 0:
            return False
        return bool(st.value & 1)

    def unlock(self, password: str) -> bool:
        pw = password.encode()
        return self.S.SecKeychainUnlock(self._open(), len(pw), pw, True) == 0 and self.is_unlocked()

    def lock(self) -> None:
        if self.exists():
            self.S.SecKeychainLock(self._open())

    # --- items
    def _get(self, svc: bytes, name: str) -> str | None:
        a = name.encode()
        n, d = ctypes.c_uint32(), ctypes.c_void_p()
        rc = self.S.SecKeychainFindGenericPassword(self._open(), len(svc), svc, len(a), a,
                                                   ctypes.byref(n), ctypes.byref(d), None)
        if rc == 0:
            v = ctypes.string_at(d, n.value).decode()
            self.S.SecKeychainItemFreeContent(None, d)
            return v
        if rc in (errSecInteractionNotAllowed, errSecAuthFailed):
            raise Locked()
        return None

    def get(self, name: str) -> str | None:
        v = self._get(SERVICE, name)
        if v is None:
            v = self._get(LEGACY_SERVICE, name)
            if v is not None:            # migrate on first use after the rename
                self.set(name, v)
                self._rm(LEGACY_SERVICE, name)
        return v

    def _item(self, name: str, svc: bytes = SERVICE):
        a = name.encode()
        item = ctypes.c_void_p()
        rc = self.S.SecKeychainFindGenericPassword(self._open(), len(svc), svc, len(a), a,
                                                   None, None, ctypes.byref(item))
        if rc in (errSecInteractionNotAllowed, errSecAuthFailed):
            raise Locked()
        return item if rc == 0 else None

    def set(self, name: str, value: str) -> None:
        a, v = name.encode(), value.encode()
        item = self._item(name)
        if item:
            rc = self.S.SecKeychainItemModifyAttributesAndData(item, None, len(v), v)
        else:
            rc = self.S.SecKeychainAddGenericPassword(self._open(), len(SERVICE), SERVICE, len(a), a,
                                                      len(v), v, None)
        if rc in (errSecInteractionNotAllowed, errSecAuthFailed):
            raise Locked()
        if rc != 0:
            raise OSError(f"keychain write {rc}")

    def _rm(self, svc: bytes, name: str) -> bool:
        item = self._item(name, svc)
        return bool(item) and self.S.SecKeychainItemDelete(item) == 0

    def rm(self, name: str) -> bool:
        a = self._rm(SERVICE, name)
        b = self._rm(LEGACY_SERVICE, name)
        return a or b

    def names(self) -> list[str]:
        r = subprocess.run(["/usr/bin/security", "dump-keychain", self.path.decode()],
                           capture_output=True, text=True, stdin=subprocess.DEVNULL, timeout=10)
        names, acct = set(), None
        for line in r.stdout.splitlines():
            line = line.strip()
            if line.startswith("keychain:"):
                acct = None
            elif line.startswith('"acct"<blob>='):
                acct = line.split("=", 1)[1].strip('"')
            elif line in ('"svce"<blob>="gondo-pi"', '"svce"<blob>="nono-pi"') and acct:
                names.add(acct)
        return sorted(names)


class _SecretTool:
    def exists(self) -> bool:
        return True

    def is_unlocked(self) -> bool:
        return True

    def unlock(self, password: str) -> bool:
        return True

    def lock(self) -> None:
        pass

    def create(self, password: str, lock_after: int) -> None:
        pass

    def set_lock_after(self, secs: int) -> None:
        pass

    def _get(self, svc, name):
        r = subprocess.run(["secret-tool", "lookup", "service", svc, "account", name],
                           capture_output=True, text=True)
        return r.stdout if r.returncode == 0 and r.stdout else None

    def get(self, name):
        v = self._get("gondo-pi", name)
        if v is None:
            v = self._get("nono-pi", name)          # written before the rename: migrate
            if v is not None:
                self.set(name, v)
                subprocess.run(["secret-tool", "clear", "service", "nono-pi", "account", name], capture_output=True)
        return v

    def set(self, name, value):
        r = subprocess.run(["secret-tool", "store", f"--label=gondo-pi {name}", "service", "gondo-pi",
                            "account", name], input=value, capture_output=True, text=True)
        if r.returncode != 0:
            raise OSError(r.stderr.strip())

    def rm(self, name):
        old = subprocess.run(["secret-tool", "clear", "service", "nono-pi", "account", name], capture_output=True)
        return subprocess.run(["secret-tool", "clear", "service", "gondo-pi", "account", name],
                              capture_output=True).returncode == 0 or old.returncode == 0

    def names(self):
        out = set()
        for svc in ("gondo-pi", "nono-pi"):
            r = subprocess.run(["secret-tool", "search", "--all", "service", svc], capture_output=True, text=True)
            out |= {l.split("=", 1)[1].strip() for l in (r.stdout + r.stderr).splitlines()
                    if l.strip().startswith("attribute.account = ")}
        return sorted(out)


class _Memory:
    """Tests / devbox without a keyring: lives only as long as the supervisor."""
    def __init__(self):
        self.d: dict = {}
        self.unlocked = True

    def exists(self): return True
    def is_unlocked(self): return self.unlocked
    def unlock(self, password): self.unlocked = True; return True
    def lock(self): self.unlocked = False
    def create(self, password, lock_after): pass
    def set_lock_after(self, secs): pass

    def get(self, name):
        if not self.unlocked:
            raise Locked()
        return self.d.get(name)

    def set(self, name, value):
        if not self.unlocked:
            raise Locked()
        self.d[name] = value

    def rm(self, name): return self.d.pop(name, None) is not None
    def names(self): return sorted(self.d)


def open_store():
    kind = os.environ.get("GONDO_PI_KEYSTORE")
    if kind == "memory":
        return _Memory()
    if MAC and kind != "secret-tool":
        path = os.environ.get("GONDO_PI_KEYCHAIN", KEYCHAIN)
        if path == KEYCHAIN and not os.path.exists(KEYCHAIN) and os.path.exists(LEGACY_KEYCHAIN):
            path = LEGACY_KEYCHAIN       # created before the rename; `gondo-pi setup` renames it
        return _Mac(path)
    if shutil.which("secret-tool"):
        return _SecretTool()
    return _Memory()
