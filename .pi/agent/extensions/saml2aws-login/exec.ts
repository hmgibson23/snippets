import { spawn } from "node:child_process";
import { StringDecoder } from "node:string_decoder";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { DEFAULT_ALLOWED_HOSTS, minimalEnv, proxyEnv, sandboxProfile, scratchAwsDir } from "./harden";
import { startProxy, type ProxyHandle } from "./proxy";
import { assertNotExpired, redact } from "./vault";

/** Newest output we keep in memory (the head is dropped, the tail is what you debug from). */
const MAX_KEPT = 1024 * 1024;
/** Most we hand back to the model. */
export const MAX_RETURNED = 50 * 1024;
/** Politeness window between SIGTERM and SIGKILL (terraform needs it to release its state lock). */
const KILL_GRACE_MS = 5_000;
/** After the shell exits, how long we wait for leftover grandchildren to close the pipes. */
const PIPE_DRAIN_MS = 1_000;
const UPDATE_INTERVAL_MS = 250;
const UPDATE_TAIL = 8 * 1024;

export interface ExecResult {
  output: string;
  exitCode: number | null;
  signal: NodeJS.Signals | null;
  timedOut: boolean;
  aborted: boolean;
  truncated: boolean;
  /** Hosts the sandbox refused (empty when not sandboxed). */
  refusedHosts: string[];
}

/** Keep the last `max` bytes (on a char boundary) of `text`. */
export function tail(text: string, max: number): { text: string; cut: boolean } {
  if (Buffer.byteLength(text) <= max) return { text, cut: false };
  let out = Buffer.from(text).subarray(-max).toString("utf8");
  if (out.charCodeAt(0) === 0xfffd) out = out.slice(1); // we sliced into a multibyte char
  const nl = out.indexOf("\n");
  return { text: nl >= 0 && nl < 200 ? out.slice(nl + 1) : out, cut: true };
}

export async function runWithCredentials(
  command: string,
  injected: Record<string, string>,
  opts: {
    cwd?: string;
    timeoutMs: number;
    signal?: AbortSignal;
    baseEnv?: NodeJS.ProcessEnv;
    /** Called (throttled) with the redacted tail of the output so far. */
    onProgress?: (tailText: string) => void;
    /** Extra variables the command may inherit from pi (default: only harden.ENV_ALLOW). */
    inheritEnv?: readonly string[];
    /** macOS: run under sandbox-exec, network only via a proxy that allows these hosts. */
    sandbox?: { allowedHosts?: readonly string[] } | false;
  },
): Promise<ExecResult> {
  assertNotExpired(injected);

  if (opts.signal?.aborted) {
    return { output: "", exitCode: null, signal: null, timedOut: false, aborted: true, truncated: false, refusedHosts: [] };
  }

  // Only a minimal inherited env (no SSH agent, no other tools' tokens; inherited AWS creds
  // are not on the list either), a throwaway AWS config dir, then the vault's credentials.
  const scratch = scratchAwsDir(injected.AWS_PROFILE, injected.AWS_REGION ?? injected.AWS_DEFAULT_REGION);
  let proxy: ProxyHandle | undefined;
  let profileDir: string | undefined;
  let argv = ["/bin/bash", "-c", command];
  let env: NodeJS.ProcessEnv = { ...minimalEnv(opts.baseEnv ?? process.env, opts.inheritEnv), ...scratch.env, ...injected };
  try {
    if (opts.sandbox) {
      proxy = await startProxy([...DEFAULT_ALLOWED_HOSTS, ...(opts.sandbox.allowedHosts ?? [])]);
      profileDir = mkdtempSync(join(tmpdir(), "aws-exec-sb-"));
      const profile = join(profileDir, "profile.sb");
      writeFileSync(profile, sandboxProfile(proxy.port), { mode: 0o600 });
      argv = ["/usr/bin/sandbox-exec", "-f", profile, ...argv];
      env = { ...env, ...proxyEnv(proxy.port) };
    }
  } catch (error) {
    scratch.cleanup();
    await proxy?.close();
    throw error;
  }
  const cleanup = async () => {
    scratch.cleanup();
    if (profileDir) rmSync(profileDir, { recursive: true, force: true });
    await proxy?.close();
  };

  const result = await new Promise<ExecResult>((resolve, reject) => {
    // detached = own process group, so we can signal the shell AND everything it started (terraform, aws, ...).
    // Killing only bash leaves grandchildren running and holding the output pipes open.
    const child = spawn(argv[0], argv.slice(1), { cwd: opts.cwd, env, stdio: ["ignore", "pipe", "pipe"], detached: true });
    const decoder = new StringDecoder("utf8");
    let buf = "";
    let dropped = false;
    let timedOut = false;
    let aborted = false;
    let settled = false;
    let killTimer: NodeJS.Timeout | undefined;
    let drainTimer: NodeJS.Timeout | undefined;
    let lastUpdate = 0;
    let updateTimer: NodeJS.Timeout | undefined;

    const signalGroup = (sig: NodeJS.Signals) => {
      if (child.pid === undefined) return;
      try { process.kill(-child.pid, sig); } catch { try { child.kill(sig); } catch { /* already gone */ } }
    };

    const stop = () => {
      if (killTimer) return;
      signalGroup("SIGTERM");
      killTimer = setTimeout(() => signalGroup("SIGKILL"), KILL_GRACE_MS);
    };

    const progress = () => {
      updateTimer = undefined;
      lastUpdate = Date.now();
      if (opts.onProgress && buf) opts.onProgress(redact(tail(buf, UPDATE_TAIL).text, injected));
    };

    const onData = (chunk: Buffer) => {
      buf += decoder.write(chunk);
      if (buf.length > MAX_KEPT * 2) { buf = buf.slice(-MAX_KEPT); dropped = true; }
      if (opts.onProgress && !updateTimer) {
        updateTimer = setTimeout(progress, Math.max(0, UPDATE_INTERVAL_MS - (Date.now() - lastUpdate)));
      }
    };
    child.stdout.on("data", onData);
    child.stderr.on("data", onData);

    const timer = setTimeout(() => { timedOut = true; stop(); }, opts.timeoutMs);
    const onAbort = () => { aborted = true; stop(); };
    opts.signal?.addEventListener("abort", onAbort, { once: true });

    const finish = (code: number | null, sig: NodeJS.Signals | null) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      if (killTimer) clearTimeout(killTimer);
      if (drainTimer) clearTimeout(drainTimer);
      if (updateTimer) clearTimeout(updateTimer);
      opts.signal?.removeEventListener("abort", onAbort);
      signalGroup("SIGKILL"); // sweep up anything the shell left behind
      child.stdout.destroy();
      child.stderr.destroy();
      buf += decoder.end();
      const kept = tail(buf, MAX_RETURNED);
      let output = redact(kept.text, injected);
      if (dropped || kept.cut) output = "[earlier output truncated]\n" + output;
      resolve({ output, exitCode: code, signal: sig, timedOut, aborted, truncated: dropped || kept.cut, refusedHosts: [...(proxy?.refused ?? [])] });
    };

    child.on("error", (err) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      if (killTimer) clearTimeout(killTimer);
      opts.signal?.removeEventListener("abort", onAbort);
      reject(err);
    });
    // 'exit' = the shell is gone. Don't wait forever for 'close': a stray grandchild can hold the pipes open.
    child.on("exit", (code, sig) => { drainTimer = setTimeout(() => finish(code, sig), PIPE_DRAIN_MS); });
    child.on("close", (code, sig) => finish(code, sig));
  }).finally(cleanup);
  return result;
}
