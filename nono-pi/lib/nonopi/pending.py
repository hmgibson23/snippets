"""Pending-request queue for the zellij control panel.

In a zellij session, a prompt becomes a pending item: the bar shows
"⚠ N waiting · Alt-2" and the nono-pi tab lists it; you answer there. Nothing
pops up. Outside zellij, prompts go to the terminal / dialog path in prompt.py.

Identical requests (same kind/title/body, e.g. retries of one AWS login) share
one item: answering it answers all of them.
"""
from __future__ import annotations

import itertools
import threading
import time

_ids = itertools.count(1)


class Pending:
    def __init__(self):
        self.lock = threading.Lock()
        self.items: dict[int, dict] = {}
        self.events: dict[int, threading.Event] = {}
        self.answers: dict[int, dict | None] = {}
        self.waiters: dict[int, int] = {}       # id -> number of callers waiting on it
        self.by_key: dict[tuple, int] = {}

    @staticmethod
    def _key(spec: dict) -> tuple:
        return (spec.get("kind"), spec.get("id"), spec.get("title"), spec.get("body"))

    def add(self, spec: dict) -> int:
        k = self._key(spec)
        with self.lock:
            i = self.by_key.get(k)
            if i is not None and i in self.items:
                self.waiters[i] += 1
                return i
            i = next(_ids)
            self.items[i] = dict(spec, id=i, at=time.time())
            self.events[i] = threading.Event()
            self.waiters[i] = 1
            self.by_key[k] = i
        return i

    def answer(self, i: int, ans: dict | None) -> bool:
        with self.lock:
            ev = self.events.get(i)
            if not ev or ev.is_set():
                return False
            self.answers[i] = ans
            # answered: stop showing it, even while callers are still collecting the answer
            item = self.items.pop(i, None)
            if item:
                self.by_key.pop(self._key(item), None)
            ev.set()
            return True

    def wait(self, i: int, timeout: float) -> tuple[bool, dict | None]:
        ev = self.events[i]
        got = ev.wait(timeout)
        return got, self.answers.get(i)

    def done(self, i: int) -> None:
        with self.lock:
            self.waiters[i] = self.waiters.get(i, 1) - 1
            if self.waiters[i] > 0:
                return
            item = self.items.pop(i, None)
            if item:
                self.by_key.pop(self._key(item), None)
            self.events.pop(i, None)
            self.answers.pop(i, None)
            self.waiters.pop(i, None)

    def list(self) -> list[dict]:
        with self.lock:
            return [dict(v) for v in sorted(self.items.values(), key=lambda x: x["id"])]

    def count(self) -> int:
        with self.lock:
            return len(self.items)
