// gondo-pi VM daemon (trusted side). Owns ONE Gondolin microVM per gondo-pi session
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
import { createConnection, createServer, createServer as createTcpServer } from "node:net";
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
	log: (m) => { netLog.push(m); if (netLog.length > 200) netLog.shift(); if (process.env.GONDO_PI_NET_DEBUG) log("net", m); },
});

async function createVm() {
	const httpHooks = { isRequestAllowed: policy.isRequestAllowed, onRequest: policy.onRequest };
	const mounts = {
		"/workspace": new RealFSProvider(spec.workspace),
		// ~/.pi read-only: copied onto the VM disk at boot (setupPi); only pi's data dirs below are
		// writable host mounts; auth.json goes back through the supervisor (validated merge)
		"/gondo-pi-host/pi": new ReadonlyProvider(new RealFSProvider(spec.piDir)),
		...Object.fromEntries(Object.entries(spec.piWritable ?? {}).map(([g, h]) => [g, new RealFSProvider(h)])),
		...(spec.cacheDir ? { "/gondo-pi-cache": new RealFSProvider(spec.cacheDir) } : {}),
		// per-project shell state (fish history, zoxide db): survives the throwaway rootfs
		...(spec.dataDir ? { [spec.guestHome + "/.local/share/fish"]: new RealFSProvider(spec.dataDir + "/fish"),
			[spec.guestHome + "/.local/share/zoxide"]: new RealFSProvider(spec.dataDir + "/zoxide") } : {}),
		// your neovim: config read-only (the host's files), plugins + Mason + parsers for Linux
		// (shared across projects, built in the VM), shada/undo/sessions per project
		// your neovim: the host config read-only (copied into the VM at boot: lazy-lock writes stay
		// in the VM); plugins + Mason + parsers for Linux as a tarball on the host, unpacked onto the
		// VM disk (thousands of files: the shared mount is far too slow) and saved back at exit
		...(spec.nvim?.config ? { "/gondo-pi-host/nvim-config": new ReadonlyProvider(new RealFSProvider(spec.nvim.config)),
			"/gondo-pi-host/nvim-data": new RealFSProvider(spec.nvim.data),
			...(spec.nvim.hostLazy ? { "/gondo-pi-host/nvim-lazy": new ReadonlyProvider(new RealFSProvider(spec.nvim.hostLazy)) } : {}),
			[spec.guestHome + "/.local/state/nvim"]: new RealFSProvider(spec.nvim.state) } : {}),
		"/gondo-pi": new ReadonlyProvider(new RealFSProvider(spec.shareDir)),
	};
	for (const [g, h] of Object.entries(spec.piLinks ?? {})) mounts["/gondo-pi-host/links" + g] = new ReadonlyProvider(new RealFSProvider(h));
	for (const [g, h] of Object.entries(spec.piLinksRo ?? {})) mounts["/gondo-pi-host/links" + g] = new ReadonlyProvider(new RealFSProvider(h));
	if (spec.piNpm) mounts["/gondo-pi-npm"] = new RealFSProvider(spec.piNpm);
	for (const [g, h] of Object.entries(spec.extraRead ?? {})) mounts[g] = new ReadonlyProvider(new RealFSProvider(h));
	for (const [g, h] of Object.entries(spec.extraAllow ?? {})) mounts[g] = new RealFSProvider(h);
	const vm = await VM.create({
		sandbox: { imagePath: spec.image, vmm: spec.vmm ?? (process.platform === "darwin" ? "krun" : undefined) },
		rootfs: { mode: "cow", size: spec.rootfsSize ?? "16G" }, // throwaway: every session starts from the clean image
		httpHooks,
		dns: { mode: "synthetic", syntheticHostMapping: "per-host" },
		tcp: { hosts: { "gondo-broker.internal:7070": `127.0.0.1:${spec.brokerPort}` } },
		ssh: {
			allowedHosts: spec.sshHosts,
			agent: spec.agentSocket,
			knownHostsFile: spec.knownHosts,
			execPolicy: async (req) => {
				const r = await ctl({ op: "vm-ssh-exec", host: req.hostname, port: req.port, user: req.guestUsername, command: req.command });
				return r.ok ? { allow: true } : { allow: false, exitCode: 128, message: r.error ?? "gondo-pi: denied" };
			},
		},
		vfs: { mounts },
		env: spec.env,
		memory: spec.memory ?? "4G",
		cpus: spec.cpus ?? 4,
		sessionLabel: spec.label,
	});
	await setupPi(vm);
	// guest setup: a writable copy of your fish config (the share is read-only)
	if (spec.setup) {
		const r = await vm.exec(["/bin/sh", "-c", spec.setup], { stdout: "buffer", stderr: "buffer", env: spec.env });
		if (r.exitCode !== 0) log("guest setup failed:", r.stderr?.toString().slice(-300));
	}
	// pi's npm packages for Linux: the host tree has macOS native binaries (fff, ffi-rs).
	// Built once per lockfile inside the VM, in the background (first run: a minute or two);
	// the guest `pi` wrapper waits for /run/gondo-pi-npm.done.
	// slow parts in the background: shells are usable right away; `pi` and `nvim` wait for theirs
	if (spec.piNpm) void ensurePiNpm(vm).catch((e) => log("pi npm:", e?.message ?? e));
	if (spec.nvim?.config) void setupNvim(vm).catch((e) => log("nvim:", e?.message ?? e));
	return vm;
}

// pi: a writable copy of ~/.pi on the VM disk (host copy is read-only to the guest). Symlinked
// entries (extensions/skills/models.json -> ~/snippets/...) are copied as their targets. The
// npm tree is skipped (replaced by the Linux build) and so are the data dirs (mounted rw).
async function setupPi(vm) {
	const H = spec.guestHome;
	const skip = Object.keys(spec.piWritable ?? {}).map((g) => g.slice(`${H}/.pi/`.length));
	const excl = ["agent/npm", "agent/install", "agent/bin", ...skip].map((x) => `--exclude=./${x}`).join(" ");
	const links = Object.keys({ ...(spec.piLinks ?? {}), ...(spec.piLinksRo ?? {}) });
	const r = await vm.exec(["/bin/sh", "-c", `set -e
mkdir -p ${H}/.pi
# link targets first, at the paths the (relative) links resolve to: copied, not mounted
for g in ${links.map((g) => `'${g}'`).join(" ")}; do mkdir -p "$(dirname "$g")"; rm -rf "$g"; cp -a "/gondo-pi-host/links$g" "$g"; done
(cd /gondo-pi-host/pi && tar -cf - ${excl} .) | (cd ${H}/.pi && tar -xf -)
mkdir -p ${H}/.pi/agent/npm
chmod -R u+w ${H}/.pi 2>/dev/null || true
cp -p ${H}/.pi/agent/auth.json /run/gondo-pi-auth.orig 2>/dev/null || true`], { stdout: "buffer", stderr: "buffer" });
	if (r.exitCode !== 0) log("pi setup:", r.stderr.toString().slice(-400));
}

// auth.json written by pi in the VM (new OAuth tokens, a new API key): handed to the
// supervisor, which validates and merges it into the host file. Called on exit and by ops.
async function readGuestAuth(vm) {
	if (!vm) return null;
	const r = await vm.exec(["/bin/sh", "-c", `cmp -s ${spec.guestHome}/.pi/agent/auth.json /run/gondo-pi-auth.orig && exit 3; cat ${spec.guestHome}/.pi/agent/auth.json`],
		{ stdout: "buffer", stderr: "buffer" });
	if (r.exitCode !== 0) return null;
	return r.stdout.toString();
}

async function syncAuth(vm) {
	const text = await readGuestAuth(vm).catch(() => null);
	if (!text) return;
	const res = await ctl({ op: "pi-auth-sync", auth: text });
	if (res.ok) await vm.exec(["/bin/sh", "-c", `cp -p ${spec.guestHome}/.pi/agent/auth.json /run/gondo-pi-auth.orig`], { stdout: "buffer" }).catch(() => {});
	log(`pi auth sync: ${res.ok ? (res.changed ?? []).join(", ") || "no change" : res.error}`);
}

// neovim: config copy (writable in the VM only) + plugin data unpacked onto the VM disk
async function setupNvim(vm) {
	const r = await vm.exec(["/bin/sh", "/gondo-pi/bin/gondo-pi-nvim-setup", spec.guestHome], { stdout: "buffer", stderr: "buffer" });
	if (r.exitCode !== 0) log("nvim setup:", r.stderr.toString().slice(-300));
}

// at session end: plugins/Mason/parsers back to the host (so the next VM starts warm)
async function saveNvim(vm) {
	if (!spec.nvim?.config || !vm) return;
	const H = spec.guestHome;
	const r = await vm.exec(["/bin/sh", "-c", `set -e
[ -d ${H}/.local/share/nvim/lazy ] || exit 0
cd ${H}/.local/share/nvim
now=$(find lazy mason site -maxdepth 2 2>/dev/null | sort | md5sum | cut -c1-32)
[ "$now" = "$(cat /run/gondo-pi-nvim.fp 2>/dev/null)" ] && exit 0
tar -cf /gondo-pi-host/nvim-data/nvim-data.tar.new --exclude=./site/plugin/gondo-pi.lua --exclude=./site/lua/gondo_pi_clipboard.lua . && mv /gondo-pi-host/nvim-data/nvim-data.tar.new /gondo-pi-host/nvim-data/nvim-data.tar`],
		{ stdout: "buffer", stderr: "buffer" });
	if (r.exitCode !== 0) log("nvim save:", r.stderr.toString().slice(-300));
}

async function ensurePiNpm(vm) {
	const script = `set -e
src=/gondo-pi-host/pi/agent/npm   # host tree, read-only: only package.json + lock are used
[ -f "$src/package-lock.json" ] || exit 0
key=$(cat "$src/package.json" "$src/package-lock.json" | sha256sum | cut -c1-16)-$(uname -m)
dst=/gondo-pi-npm/$key
if [ ! -f "$dst/.ok" ]; then
  rm -rf "$dst.tmp" && mkdir -p "$dst.tmp"
  cp "$src/package.json" "$src/package-lock.json" "$dst.tmp/"
  [ -d "$src/scripts" ] && cp -R "$src/scripts" "$dst.tmp/"
  # same flags pi's own installer uses; the postinstall script (host peers patch) runs after
  (cd "$dst.tmp" && npm ci --ignore-scripts --legacy-peer-deps --include=optional --no-audit --no-fund --loglevel=error \
     && npm run --if-present --silent postinstall) >&2
  touch "$dst.tmp/.ok" && rm -rf "$dst" && mv "$dst.tmp" "$dst"
  ls /gondo-pi-npm | grep -vx "$key" | grep -v '\.tmp$' | while read old; do rm -rf "/gondo-pi-npm/$old"; done
fi
echo "$dst"`;
	const t0 = Date.now();
	const mark = (txt) => vm.exec(["/bin/sh", "-c", `printf '%s\n' "$1" > /run/gondo-pi-npm.done`, "sh", txt], { stdout: "buffer", stderr: "buffer" });
	const r = await vm.exec(["/bin/sh", "-c", script], { stdout: "buffer", stderr: "buffer", env: spec.env });
	const dir = r.stdout.toString().trim();
	if (r.exitCode !== 0 || !dir) {
		const err = r.stderr.toString().slice(-400);
		log("pi npm packages for linux failed:", err);
		await mark("failed: " + err.split("\n").filter(Boolean).pop());
		return;
	}
	// copy the tree onto the VM's own disk and bind it over ~/.pi/agent/npm (host tree untouched):
	// pi loads ~20 extensions = thousands of small files; over the shared mount that is ~7x slower
	const b = await vm.exec(["/bin/sh", "-c", `set -e; rm -rf /opt/pi-npm; mkdir -p /opt/pi-npm; cp -a "${dir}/." /opt/pi-npm/; mount --bind /opt/pi-npm ${spec.guestHome}/.pi/agent/npm`], { stdout: "buffer", stderr: "buffer" });
	if (b.exitCode !== 0) log("bind ~/.pi/agent/npm failed:", b.stderr.toString().slice(-300));
	await mark(b.exitCode === 0 ? "ok" : "failed: bind");
	const took = Date.now() - t0;
	if (took > 3000) log(`pi npm packages for linux ready (${Math.round(took / 1000)}s)`);
}

// ---------------------------------------------------------------- port forwards
// host 127.0.0.1:<hostPort> -> guest 127.0.0.1:<guestPort>, one guest `nc` per connection.
// Host-local only (never 0.0.0.0): only you, on this machine, reach the dev server.
const forwards = new Map(); // guestPort -> {server, hostPort, conns}

async function listening() {
	if (!vm) return [];
	const r = await vm.exec(["/bin/sh", "-c", "cat /proc/net/tcp /proc/net/tcp6 2>/dev/null | awk 'NR>1 && $4==\"0A\"{print $2}'"], { stdout: "buffer", stderr: "buffer" });
	const ports = new Set();
	for (const line of r.stdout.toString().split("\n")) {
		const m = /:([0-9A-F]{4})$/i.exec(line.trim());
		if (m) ports.add(parseInt(m[1], 16));
	}
	// sshd / gondolin internals aren't dev servers
	return [...ports].filter((p) => p >= 1024 && p !== 22).sort((a, b) => a - b);
}

async function forwardPort(guestPort, wantHostPort) {
	if (forwards.has(guestPort)) return forwards.get(guestPort).hostPort;
	const entry = { conns: new Set() };
	const server = createTcpServer((sock) => {
		if (!vm) return sock.destroy();
		const p = vm.exec(["/bin/sh", "-c", `exec nc 127.0.0.1 ${guestPort}`], { stdin: true, stdout: "pipe", stderr: "pipe" });
		entry.conns.add(sock);
		p.stdout.on("data", (d) => sock.write(d));
		sock.on("data", (d) => { try { p.write(d); } catch {} });
		sock.on("end", () => { try { p.end(); } catch {} });
		sock.on("error", () => {});
		sock.on("close", () => entry.conns.delete(sock));
		p.then(() => sock.end(), () => sock.destroy());
	});
	const tryListen = (port) => new Promise((res, rej) => { server.once("error", rej); server.listen(port, "127.0.0.1", () => res(server.address().port)); });
	let hostPort;
	try { hostPort = await tryListen(wantHostPort ?? guestPort); }
	catch { hostPort = await tryListen(0); }   // same port taken on the host: any free one
	Object.assign(entry, { server, hostPort });
	forwards.set(guestPort, entry);
	log(`forward 127.0.0.1:${hostPort} -> vm:${guestPort}`);
	return hostPort;
}

function unforward(guestPort) {
	const e = forwards.get(guestPort);
	if (!e) return false;
	e.server.close();
	for (const c of e.conns) c.destroy();
	forwards.delete(guestPort);
	return true;
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
	if (req.kind === "pi") return ["/bin/sh", "-c", 'exec pi -e /gondo-pi/pi-extension "$@"', "pi", ...args];
	if (req.kind === "cmd" && args.length) return ["/bin/sh", "-c", 'exec "$@"', "sh", ...args];
	return ["/usr/bin/fish", "-l"];
}

async function startSession(sock, req, rest) {
	await vmReady;
	const id = nextId++;
	const tty = req.tty !== false;
	const term = !req.term || req.term === "xterm-ghostty" ? "xterm-256color" : req.term;
	const env = { ...spec.env, TERM: term, COLORTERM: "truecolor", GONDO_PI_VM_SESSION: String(id) };
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
		if (old) await Promise.race([syncAuth(old).catch(() => {}).then(() => saveNvim(old)).then(() => old.close()), new Promise((r) => setTimeout(r, 60000))]);
		vm = await createVm();
		log("VM ready");
		// forwards relay into the current VM per connection, so they keep working: nothing to redo
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
	console.error("gondo-pi: could not start the VM:", e?.message ?? e);
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
		if (req.op === "ports") return void listening().then((l) => reply({ ok: true, listening: l,
			forwards: [...forwards.entries()].map(([g, e]) => ({ guest: g, host: e.hostPort, conns: e.conns.size })) }), (e) => reply({ ok: false, error: String(e) }));
		if (req.op === "forward") return void forwardPort(Number(req.port), req.hostPort ? Number(req.hostPort) : undefined)
			.then((h) => reply({ ok: true, guest: Number(req.port), host: h }), (e) => reply({ ok: false, error: String(e?.message ?? e) }));
		if (req.op === "unforward") return reply({ ok: unforward(Number(req.port)) });
		if (req.op === "auth-sync") return void syncAuth(vm).then(() => reply({ ok: true }), (e) => reply({ ok: false, error: String(e) }));
		if (req.op === "restart") return void restartVm(req.why ?? "new access").then(() => reply({ ok: true }), (e) => reply({ ok: false, error: String(e) }));
		if (req.op === "stop") { reply({ ok: true }); return void shutdown(0); }
		reply({ ok: false, error: `unknown op ${req.op}` });
	};
	sock.on("data", onData);
	sock.on("error", () => {});
});
// project settings `ports`: forward them as soon as something listens (no prompt: you set them)
setInterval(async () => {
	if (!vm || !(spec.autoForward ?? []).length) return;
	try {
		const live = await listening();
		for (const p of spec.autoForward) if (live.includes(p) && !forwards.has(p)) {
			const h = await forwardPort(p);
			void ctl({ op: "forwarded", port: p, host: h });
		}
	} catch {}
}, 3000).unref();

// refreshed OAuth tokens shouldn't wait for a clean exit
setInterval(() => { if (vm) void syncAuth(vm).catch(() => {}); }, 5 * 60_000).unref();

server.listen(spec.socket, () => log(`VM ready · ${spec.image} · ${spec.cpus ?? 4} cpu · ${spec.memory ?? "4G"}`));

let stopping = false;
async function shutdown(code) {
	if (stopping) return;
	stopping = true;
	for (const s of sessions.values()) s.finish?.(0, "vm-stop");
	server.close();
	try { unlinkSync(spec.socket); } catch {}
	if (vm) await Promise.race([syncAuth(vm).catch(() => {}).then(() => saveNvim(vm)).catch(() => {}).then(() => vm.close()), new Promise((r) => setTimeout(r, 60000))]);
	process.exit(code);
}
process.on("SIGTERM", () => shutdown(0));
process.on("SIGINT", () => shutdown(0));
process.on("SIGHUP", () => shutdown(0));
