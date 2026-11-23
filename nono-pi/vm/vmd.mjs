// nono-pi VM daemon (trusted side). Owns ONE Gondolin microVM per nono-pi session
// and serves any number of pty sessions on it (your fish shells, pi, commands)
// over a host-only unix socket. Every zellij pane is a thin client of this.
//
// Guest egress: NOTHING except
//   - one host-mapped TCP port -> host `nono proxy` (allowlist, approvals,
//     credential injection, AWS SigV4 signing; the guest only has placeholders)
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
import { VM, RealFSProvider, ReadonlyProvider, createHttpHooks } from "@earendil-works/gondolin";

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

async function createVm() {
	const { httpHooks } = createHttpHooks({ allowedHosts: [] }); // deny all mediated HTTP(S): only the mapped proxy
	const mounts = {
		"/workspace": new RealFSProvider(spec.workspace),
		[spec.guestHome + "/.pi"]: new RealFSProvider(spec.piDir),
		"/nono-pi": new ReadonlyProvider(new RealFSProvider(spec.shareDir)),
	};
	for (const [g, h] of Object.entries(spec.extraRead ?? {})) mounts[g] = new ReadonlyProvider(new RealFSProvider(h));
	for (const [g, h] of Object.entries(spec.extraAllow ?? {})) mounts[g] = new RealFSProvider(h);
	const vm = await VM.create({
		sandbox: { imagePath: spec.image, vmm: spec.vmm ?? (process.platform === "darwin" ? "krun" : undefined) },
		rootfs: { mode: "cow" }, // throwaway: every session starts from the clean image
		httpHooks,
		dns: { mode: "synthetic", syntheticHostMapping: "per-host" },
		tcp: {
			hosts: {
				"nono-proxy.internal:3128": `127.0.0.1:${spec.proxyPort}`,
				"nono-broker.internal:7070": `127.0.0.1:${spec.brokerPort}`,
			},
		},
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
	return vm;
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
		if (req.op === "reload") { spec = JSON.parse(readFileSync(specPath, "utf8")); return reply({ ok: true }); }
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
