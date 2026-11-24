// nono-pi VM daemon (trusted side). Owns ONE Gondolin microVM per nono-pi session
// and serves any number of pty sessions on it (your fish shells, pi, commands)
// over a host-only unix socket. Every zellij pane is a thin client of this.
//
// Guest egress: NOTHING except
//   - HTTP(S) through Gondolin's userspace stack + vm/netpolicy.mjs (allowlist, approvals
//     in the panel, credential injection, AWS SigV4 re-signing; the guest only has placeholders)
//   - one host-mapped TCP port -> supervisor broker (token-auth; sign, requests, status)
//   - ssh to allowlisted git hosts via Gondolin's host-side ssh proxy, using the
//     supervisor's PRIVATE agent; every exec request is checked by the supervisor
//
// Wire protocol on the socket (host side only, 0600 in a 0700 dir):
//   client -> {"op":"exec","kind":"shell"|"pi"|"cmd","args":[...],"rows":R,"cols":C,"tty":bool,"term":T}\n
//   daemon -> {"ok":true,"id":N}\n   then binary frames both ways: 1 byte type + u32 BE length + payload
//     client: i=input  r=resize {"rows","cols"}  e=stdin EOF
//     daemon: o=output x=exit {"code","reason"}   (reason "vm-restart" = reconnect)
//   client -> {"op":"restart"} (re-read spec, recreate VM: new mounts)  {"op":"reload"} (re-read env)
//             {"op":"status"}  {"op":"stop"}
import { createConnection, createServer } from "node:net";
import { readFileSync, unlinkSync } from "node:fs";
import { VM, RealFSProvider, ReadonlyProvider } from "@earendil-works/gondolin";
import { createPolicy } from "./netpolicy.mjs";

const specPath = process.argv[2];
let spec = JSON.parse(readFileSync(specPath, "utf8"));
const log = (...a) => console.log(new Date().toTimeString().slice(0, 8), ...a);

function ctl(obj) {
	return new Promise((resolve) => {
		const s = createConnection(spec.ctlSocket);
		let buf = "";
		s.on("connect", () => s.write(JSON.stringify(obj) + "\n"));
		s.on("data", (d) => {
			buf += d;
			const i = buf.indexOf("\n");
			if (i >= 0) {
				s.destroy();
				try { resolve(JSON.parse(buf.slice(0, i))); } catch { resolve({ ok: false }); }
			}
		});
		s.on("error", () => resolve({ ok: false, error: "supervisor unreachable" }));
	});
}

const netLog = [];
const policy = createPolicy({
	ctl,
	getSpec: () => spec,
	log: (m) => { netLog.push(m); if (netLog.length > 200) netLog.shift(); if (process.env.NONO_PI_NET_DEBUG) log("net", m); },
});

async function createVm() {
	const httpHooks = { isRequestAllowed: policy.isRequestAllowed, onRequest: policy.onRequest };
	const mounts = {
		"/workspace": new RealFSProvider(spec.workspace),
		[spec.guestHome + "/.pi"]: new RealFSProvider(spec.piDir),
		...(spec.cacheDir ? { "/nono-pi-cache": new RealFSProvider(spec.cacheDir) } : {}),
		"/nono-pi": new ReadonlyProvider(new RealFSProvider(spec.shareDir)),
	};
	for (const [g, h] of Object.entries(spec.piLinks ?? {})) mounts[g] = new RealFSProvider(h);
	if (spec.piNpm) mounts["/nono-pi-npm"] = new RealFSProvider(spec.piNpm);
	for (const [g, h] of Object.entries(spec.extraRead ?? {})) mounts[g] = new ReadonlyProvider(new RealFSProvider(h));
	for (const [g, h] of Object.entries(spec.extraAllow ?? {})) mounts[g] = new RealFSProvider(h);
	const vm = await VM.create({
		sandbox: { imagePath: spec.image, vmm: spec.vmm ?? (process.platform === "darwin" ? "krun" : undefined) },
		rootfs: { mode: "cow" }, // throwaway: every session starts from the clean image
		httpHooks,
		dns: { mode: "synthetic", syntheticHostMapping: "per-host" },
		tcp: { hosts: { "nono-broker.internal:7070": `127.0.0.1:${spec.brokerPort}` } },
		ssh: {
			allowedHosts: spec.sshHosts,
			agent: spec.agentSocket,
			knownHostsFile: spec.knownHosts,
			execPolicy: async (req) => {
				const r = await ctl({ op: "vm-ssh-exec", host: req.hostname, port: req.port, user: req.guestUsername, command: req.command });
				return r.ok ? { allow: true } : { allow: false, exitCode: 128, message: r.error ?? "nono-pi: denied" };
			},
		},
		vfs: { mounts },
		env: spec.env,
		memory: spec.memory ?? "4G",
		cpus: spec.cpus ?? 4,
		sessionLabel: spec.label,
	});
	// guest setup: a writable copy of your fish config (the share is read-only)
	if (spec.setup) {
		const r = await vm.exec(["/bin/sh", "-c", spec.setup], { stdout: "buffer", stderr: "buffer", env: spec.env });
		if (r.exitCode !== 0) log("guest setup failed:", r.stderr?.toString().slice(-300));
	}
	// pi's npm packages for Linux: the host tree has macOS native binaries (fff, ffi-rs).
	// Built once per lockfile inside the VM, in the background (first run: a minute or two);
	// the guest `pi` wrapper waits for /run/nono-pi-npm.done.
	if (spec.piNpm) void ensurePiNpm(vm).catch((e) => log("pi npm:", e?.message ?? e));
	return vm;
}

async function ensurePiNpm(vm) {
	const script = `set -e
src=${spec.guestHome}/.pi/agent/npm
[ -f "$src/package-lock.json" ] || exit 0
key=$(cat "$src/package.json" "$src/package-lock.json" | sha256sum | cut -c1-16)-$(uname -m)
dst=/nono-pi-npm/$key
if [ ! -f "$dst/.ok" ]; then
  rm -rf "$dst.tmp" && mkdir -p "$dst.tmp"
  cp "$src/package.json" "$src/package-lock.json" "$dst.tmp/"
  [ -d "$src/scripts" ] && cp -R "$src/scripts" "$dst.tmp/"
  # same flags pi's own installer uses; the postinstall script (host peers patch) runs after
  (cd "$dst.tmp" && npm ci --ignore-scripts --legacy-peer-deps --include=optional --no-audit --no-fund --loglevel=error \
     && npm run --if-present --silent postinstall) >&2
  touch "$dst.tmp/.ok" && rm -rf "$dst" && mv "$dst.tmp" "$dst"
  ls /nono-pi-npm | grep -vx "$key" | grep -v '\.tmp$' | while read old; do rm -rf "/nono-pi-npm/$old"; done
fi
echo "$dst"`;
	const t0 = Date.now();
	const mark = (txt) => vm.exec(["/bin/sh", "-c", `printf '%s\n' "$1" > /run/nono-pi-npm.done`, "sh", txt], { stdout: "buffer", stderr: "buffer" });
	const r = await vm.exec(["/bin/sh", "-c", script], { stdout: "buffer", stderr: "buffer", env: spec.env });
	const dir = r.stdout.toString().trim();
	if (r.exitCode !== 0 || !dir) {
		const err = r.stderr.toString().slice(-400);
		log("pi npm packages for linux failed:", err);
		await mark("failed: " + err.split("\n").filter(Boolean).pop());
		return;
	}
	// overlay: bind the linux tree over ~/.pi/agent/npm inside the guest (host tree untouched)
	const b = await vm.exec(["/bin/sh", "-c", `mount --bind "${dir}" ${spec.guestHome}/.pi/agent/npm`], { stdout: "buffer", stderr: "buffer" });
	if (b.exitCode !== 0) log("bind ~/.pi/agent/npm failed:", b.stderr.toString().slice(-300));
	await mark(b.exitCode === 0 ? "ok" : "failed: bind");
	const took = Date.now() - t0;
	if (took > 3000) log(`pi npm packages for linux ready (${Math.round(took / 1000)}s)`);
}

// ---------------------------------------------------------------- sessions
const FRAME = { input: 0x69, resize: 0x72, eof: 0x65, output: 0x6f, exit: 0x78 }; // i r e o x
const sessions = new Map();
let nextId = 1;
let vm = null;
let vmReady = null; // promise while (re)creating

function frame(type, payload) {
	const body = Buffer.isBuffer(payload) ? payload : Buffer.from(typeof payload === "string" ? payload : JSON.stringify(payload));
	const h = Buffer.alloc(5);
	h[0] = type;
	h.writeUInt32BE(body.length, 1);
	return Buffer.concat([h, body]);
}

function commandFor(req) {
	const args = Array.isArray(req.args) ? req.args.map(String) : [];
	if (req.kind === "pi") return ["/bin/sh", "-c", 'exec pi -e /nono-pi/pi-extension "$@"', "pi", ...args];
	if (req.kind === "cmd" && args.length) return ["/bin/sh", "-c", 'exec "$@"', "sh", ...args];
	return ["/usr/bin/fish", "-l"];
}

async function startSession(sock, req, rest) {
	await vmReady;
	const id = nextId++;
	const tty = req.tty !== false;
	const term = !req.term || req.term === "xterm-ghostty" ? "xterm-256color" : req.term;
	const env = { ...spec.env, TERM: term, COLORTERM: "truecolor", NONO_PI_VM_SESSION: String(id) };
	let proc;
	try {
		proc = vm.exec(commandFor(req), { cwd: "/workspace", pty: tty, stdin: true, env, stdout: "pipe", stderr: "pipe" });
	} catch (e) {
		sock.end(JSON.stringify({ ok: false, error: String(e?.message ?? e) }) + "\n");
		return;
	}
	const s = { id, sock, proc, kind: req.kind ?? "shell", done: false };
	sessions.set(id, s);
	sock.write(JSON.stringify({ ok: true, id }) + "\n");
	if (tty && req.rows && req.cols) proc.resize(req.rows, req.cols);
	const send = (d) => { if (!sock.destroyed) sock.write(frame(FRAME.output, d)); };
	proc.stdout?.on("data", send);
	proc.stderr?.on("data", send);
	let buf = rest ?? Buffer.alloc(0);
	const pump = () => {
		while (buf.length >= 5) {
			const len = buf.readUInt32BE(1);
			if (buf.length < 5 + len) break;
			const type = buf[0], body = buf.subarray(5, 5 + len);
			buf = buf.subarray(5 + len);
			try {
				if (type === FRAME.input) proc.write(Buffer.from(body));
				else if (type === FRAME.resize) { const r = JSON.parse(body); if (tty) proc.resize(r.rows, r.cols); }
				else if (type === FRAME.eof) proc.end();
			} catch { /* session already gone */ }
		}
	};
	sock.on("data", (d) => { buf = Buffer.concat([buf, d]); pump(); });
	pump();
	sock.on("close", () => {
		// pane closed: end that guest process (a pty hangup)
		if (!s.done) { try { proc.end(); } catch {} ; try { proc.write("\x04"); } catch {} }
	});
	const finish = (code, reason) => {
		if (s.done) return;
		s.done = true;
		sessions.delete(id);
		if (!sock.destroyed) sock.end(frame(FRAME.exit, { code, reason }));
	};
	s.finish = finish;
	proc.then((r) => finish(r.exitCode ?? 0, "exit"), (e) => finish(1, `error: ${e?.message ?? e}`));
}

async function restartVm(why) {
	log(`restarting the VM (${why})…`);
	spec = JSON.parse(readFileSync(specPath, "utf8"));
	policy.forget();
	vmReady = (async () => {
		for (const s of sessions.values()) s.finish?.(0, "vm-restart");
		const old = vm;
		vm = null;
		if (old) await Promise.race([old.close(), new Promise((r) => setTimeout(r, 3000))]);
		vm = await createVm();
		log("VM ready");
	})();
	await vmReady;
}

// ---------------------------------------------------------------- server
process.umask(0o077);
try { unlinkSync(spec.socket); } catch {}
vmReady = (async () => { vm = await createVm(); })();
try {
	await vmReady;
} catch (e) {
	console.error("nono-pi: could not start the VM:", e?.message ?? e);
	process.exit(1);
}

const server = createServer((sock) => {
	let head = Buffer.alloc(0);
	const onData = (d) => {
		head = Buffer.concat([head, d]);
		const nl = head.indexOf(10);
		if (nl < 0) return;
		sock.off("data", onData);
		let req;
		try { req = JSON.parse(head.subarray(0, nl).toString()); } catch { return sock.destroy(); }
		const rest = head.subarray(nl + 1);
		const reply = (o) => sock.end(JSON.stringify(o) + "\n");
		if (req.op === "exec") return void startSession(sock, req, rest);
		if (req.op === "status") return reply({ ok: true, sessions: [...sessions.values()].map((s) => ({ id: s.id, kind: s.kind })) });
		if (req.op === "reload") { spec = JSON.parse(readFileSync(specPath, "utf8")); policy.forget(); return reply({ ok: true }); }
		if (req.op === "net-log") return reply({ ok: true, log: netLog.slice(-50) });
		if (req.op === "restart") return void restartVm(req.why ?? "new access").then(() => reply({ ok: true }), (e) => reply({ ok: false, error: String(e) }));
		if (req.op === "stop") { reply({ ok: true }); return void shutdown(0); }
		reply({ ok: false, error: `unknown op ${req.op}` });
	};
	sock.on("data", onData);
	sock.on("error", () => {});
});
server.listen(spec.socket, () => log(`VM ready · ${spec.image} · ${spec.cpus ?? 4} cpu · ${spec.memory ?? "4G"}`));

let stopping = false;
async function shutdown(code) {
	if (stopping) return;
	stopping = true;
	for (const s of sessions.values()) s.finish?.(0, "vm-stop");
	server.close();
	try { unlinkSync(spec.socket); } catch {}
	if (vm) await Promise.race([vm.close(), new Promise((r) => setTimeout(r, 3000))]);
	process.exit(code);
}
process.on("SIGTERM", () => shutdown(0));
process.on("SIGINT", () => shutdown(0));
process.on("SIGHUP", () => shutdown(0));
