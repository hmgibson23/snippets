import assert from "node:assert/strict";
import { runWithCredentials, tail } from "./exec.ts";

const env = { AWS_ACCESS_KEY_ID: "AKIAEXAMPLE12345", AWS_SECRET_ACCESS_KEY: "secretsecret1234567890", AWS_SESSION_TOKEN: "tokentokentoken123456" };
const alive = (pid: number) => { try { process.kill(pid, 0); return true; } catch { return false; } };

// plain run: output, exit code, redaction
let r = await runWithCredentials("echo hi; echo $AWS_SECRET_ACCESS_KEY; exit 3", env, { timeoutMs: 5000 });
assert.equal(r.exitCode, 3);
assert.match(r.output, /hi/);
assert.match(r.output, /\[REDACTED\]/);
assert.doesNotMatch(r.output, /secretsecret/);

// abort kills the whole tree (a grandchild holding the pipe used to hang the call forever)
const ac = new AbortController();
setTimeout(() => ac.abort(), 300);
let t0 = Date.now();
r = await runWithCredentials("sleep 300 & echo $! > /tmp/aws_exec_gc.pid; wait", env, { timeoutMs: 60_000, signal: ac.signal });
assert.ok(Date.now() - t0 < 4000, "abort returned promptly");
assert.equal(r.aborted, true);
const { readFileSync } = await import("node:fs");
await new Promise((res) => setTimeout(res, 200));
assert.equal(alive(Number(readFileSync("/tmp/aws_exec_gc.pid", "utf8"))), false, "grandchild is dead");

// timeout does the same
t0 = Date.now();
r = await runWithCredentials("sleep 300", env, { timeoutMs: 300 });
assert.equal(r.timedOut, true);
assert.ok(Date.now() - t0 < 4000);

// already-aborted signal never spawns
const done = new AbortController();
done.abort();
r = await runWithCredentials("touch /tmp/aws_exec_should_not_exist", env, { timeoutMs: 5000, signal: done.signal });
assert.equal(r.aborted, true);

// progress callback sees redacted output while the command is still running
const seen: string[] = [];
r = await runWithCredentials("echo $AWS_ACCESS_KEY_ID; sleep 0.6", env, { timeoutMs: 5000, onProgress: (s) => seen.push(s) });
assert.ok(seen.length > 0 && seen.every((s) => !s.includes("AKIAEXAMPLE")));

// only the tail comes back for huge output
r = await runWithCredentials("seq 1 200000", env, { timeoutMs: 10_000 });
assert.equal(r.truncated, true);
assert.match(r.output, /200000\n?$/);
assert.ok(r.output.length < 60 * 1024);
assert.equal(tail("abc", 10).cut, false);

console.log("ok");
