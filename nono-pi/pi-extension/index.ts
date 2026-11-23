// nono-pi extension: loaded only inside a nono-pi sandbox (PI_SANDBOX=nono-pi).
// Talks to the trusted supervisor through the broker socket. Never handles a
// credential: it can only ask the supervisor, which prompts the user on the
// trusted side.
import { connect, type Socket } from "node:net";
import { Type } from "typebox";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

const RESTART_EXIT = 75; // launcher restarts pi with --continue on this code

function brokerCall(obj: Record<string, unknown>, timeoutMs = 600_000): Promise<any> {
	return new Promise((resolve) => {
		const addr = process.env.PI_BROKER ?? "";
		const token = process.env.PI_BROKER_TOKEN;
		let sock: Socket;
		if (addr.startsWith("tcp://")) {
			const [host, port] = addr.slice(6).split(":");
			sock = connect(Number(port), host);
		} else {
			sock = connect(addr);
		}
		let buf = "";
		const done = (v: any) => {
			clearTimeout(timer);
			sock.destroy();
			resolve(v);
		};
		const timer = setTimeout(() => done({ ok: false, error: "timeout waiting for the user" }), timeoutMs);
		sock.on("connect", () => sock.write(JSON.stringify(token ? { ...obj, token } : obj) + "\n"));
		sock.on("data", (d) => {
			buf += d.toString();
			const i = buf.indexOf("\n");
			if (i >= 0) {
				try {
					done(JSON.parse(buf.slice(0, i)));
				} catch {
					done({ ok: false, error: "bad response" });
				}
			}
		});
		sock.on("error", (e) => done({ ok: false, error: `nono-pi broker unavailable: ${e.message}` }));
	});
}

function left(s: number | null): string {
	if (s == null) return "always";
	const h = Math.floor(s / 3600), m = Math.floor((s % 3600) / 60);
	return h ? `${h}h${String(m).padStart(2, "0")}m` : `${m}m`;
}

async function refreshStatus(ctx: ExtensionContext) {
	const st = await brokerCall({ op: "status" }, 5000);
	// The zellij bar (zjstatus) already shows nono-pi state; don't duplicate it in pi's footer.
	if (process.env.PI_NONO_ZJSTATUS_BAR === "1") {
		ctx.ui.setStatus("nono-pi", undefined);
		return st;
	}
	if (!st.ok) {
		ctx.ui.setStatus("nono-pi", "🔒 nono-pi: supervisor unreachable");
		return st;
	}
	const aws = st.aws_picked?.length ? `AWS ${st.aws_picked[0]}${st.aws_picked.length > 1 ? `+${st.aws_picked.length - 1}` : ""}` : "AWS off";
	const pushes = (st.grants ?? []).filter((g: any) => g.key.startsWith("git-push:") && g.scope !== "deny").length;
	const parts = [`🔒 nono-pi${st.vm ? " VM" : ""}`, aws, `push ${pushes}`, st.keychain_unlocked ? "🔓" : "🔐"];
	ctx.ui.setStatus("nono-pi", parts.join(" · "));
	return st;
}

// Restart (to load new nono routes / grants) only once pi is idle, so the
// session file is complete; the launcher reruns pi with --continue.
let restartWanted = false;
function restartWhenIdle(ctx: ExtensionContext) {
	restartWanted = true;
	ctx.ui.notify("nono-pi: pi restarts with the new access when this turn ends (same session)", "info");
	if (ctx.isIdle()) setTimeout(() => process.exit(RESTART_EXIT), 300);
}
async function maybeRestart(ctx: ExtensionContext, r: any) {
	if (r?.ok && r.restart) restartWhenIdle(ctx);
}

export default function nonoPi(pi: ExtensionAPI) {
	if (process.env.PI_SANDBOX !== "nono-pi") return;

	// Inside nono-pi the old in-process AWS vault (saml2aws-login ext: aws_exec,
	// /saml2aws-login) cannot work and must not be used: AWS creds never enter the
	// sandbox. Plain `aws`/terraform/SDKs work through the proxy instead.
	const LEGACY_TOOLS = new Set(["aws_exec"]);
	const dropLegacy = () => {
		const active = pi.getActiveTools();
		const next = active.filter((t) => !LEGACY_TOOLS.has(t));
		if (next.length !== active.length) pi.setActiveTools(next);
	};

	pi.on("session_start", async (_e, ctx) => {
		dropLegacy();
		await refreshStatus(ctx);
	});

	pi.on("before_agent_start", async (event) => {
		dropLegacy();
		const st = await brokerCall({ op: "status" }, 3000);
		const aws = st.ok
			? `AWS: run plain \`aws ...\` / terraform / SDKs; credentials are injected outside the sandbox. ` +
			  `Default profile: ${st.aws_picked?.[0] ?? "none"}. Other accounts: \`aws --profile <name>\` ` +
			  `(names: ${(st.aws_accounts ?? []).join(", ")}). The first call to an account may wait for the user to approve / log in ` +
			  `(Okta) in the nono-pi tab; do NOT ask the user to run /saml2aws-login and never use aws_exec.`
			: "";
		const sec =
			"You run inside nono-pi: you never see credentials. git push/pull, commit signing, gh, aws and allowlisted hosts just work. " +
			"If a command is blocked or waiting, the user is answering a nono-pi request; if access is missing, use the nono_request tool. " +
			aws;
		return { systemPrompt: event.systemPrompt + "\n\n<nono-pi>\n" + sec + "\n</nono-pi>" };
	});

	// Keep muscle memory working: /saml2aws-login becomes an AWS login via nono-pi.
	pi.registerCommand("saml2aws-login", {
		description: "nono-pi: log in to AWS (answer the Okta prompt in the nono-pi tab)",
		handler: async (args, ctx) => {
			const acct = args.trim() || undefined;
			ctx.ui.notify(`nono-pi: AWS login${acct ? ` ${acct}` : ""}: answer the prompt in the nono-pi tab`, "info");
			const r = await brokerCall({ op: "aws-login", account: acct ?? null });
			ctx.ui.notify(r.ok ? `AWS ${r.account}: logged in (credentials stay outside the sandbox)` : `AWS login failed: ${r.error ?? "denied/cancelled"}`, r.ok ? "info" : "error");
			await refreshStatus(ctx);
		},
	});
	pi.on("agent_settled", async (_e, ctx) => {
		if (restartWanted) return void setTimeout(() => process.exit(RESTART_EXIT), 300);
		await refreshStatus(ctx);
	});

	pi.registerCommand("grants", {
		description: "nono-pi: show active grants (push, AWS, hosts) and recent credential use",
		handler: async (args, ctx) => {
			if (args.trim().startsWith("revoke ")) {
				const r = await brokerCall({ op: "revoke", key: args.trim().slice(7) });
				ctx.ui.notify(r.ok ? "revoked" : "no such grant", r.ok ? "info" : "warning");
				return;
			}
			const st = await refreshStatus(ctx);
			if (!st.ok) return ctx.ui.notify(st.error, "error");
			const g = (st.grants ?? []).map((x: any) => `  ${x.scope === "deny" ? "✗" : "✓"} ${x.key}  (${left(x.left)}, ${x.where})`);
			ctx.ui.notify(
				[
					`nono-pi ${st.vm ? "(VM) " : ""}· ${st.project}`,
					`keychain: ${st.keychain_unlocked ? "unlocked" : "locked"} · ssh key loaded: ${st.ssh_key_loaded_for ? left(st.ssh_key_loaded_for) : "no"} · commits signed: ${st.signed}`,
					`AWS picked: ${st.aws_picked.join(", ") || "none"} (others ask on first use)`,
					"grants:",
					...(g.length ? g : ["  (none)"]),
					"recent:",
					...(st.recent ?? []).map((l: string) => "  " + l),
					"revoke with: /grants revoke <key>",
				].join("\n"),
				"info",
			);
		},
	});

	pi.registerCommand("aws", {
		description: "nono-pi: AWS status, or `/aws login [account]` to log in now",
		handler: async (args, ctx) => {
			const [sub, acct] = args.trim().split(/\s+/);
			if (sub === "login") {
				ctx.ui.notify(`nono-pi: AWS login${acct ? ` ${acct}` : ""}: answer the prompt on the nono-pi pane`, "info");
				const r = await brokerCall({ op: "aws-login", account: acct || null });
				ctx.ui.notify(r.ok ? `AWS ${r.account}: logged in (credentials stay outside the sandbox)` : `AWS login failed: ${r.error ?? "denied"}`, r.ok ? "info" : "error");
				return refreshStatus(ctx);
			}
			const st = await refreshStatus(ctx);
			ctx.ui.notify(`AWS picked: ${st.aws_picked?.join(", ") || "none"}\nall: ${st.aws_accounts?.join(", ")}\nUse: aws --profile <account> …  (or /aws login <account>)`, "info");
		},
	});

	pi.registerCommand("secret", {
		description: "nono-pi: add an API credential: /secret NAME https://api.host [Header] [format]",
		handler: async (args, ctx) => {
			const [name, upstream, header, ...fmt] = args.trim().split(/\s+/);
			if (!name || !upstream) return ctx.ui.notify("usage: /secret NAME https://api.host [Header] [format e.g. 'Bearer {}']", "warning");
			const r = await brokerCall({ op: "request-secret", name, upstream, header, format: fmt.join(" ") || undefined });
			ctx.ui.notify(r.ok ? `credential ${name} added; inside, use $${r.env_var}` : `not added: ${r.error}`, r.ok ? "info" : "warning");
			await maybeRestart(ctx, r);
		},
	});

	pi.registerTool({
		name: "nono_request",
		label: "nono-pi request",
		description:
			"You run inside the nono-pi sandbox: you never see credentials, the user grants access on a trusted prompt. " +
			"git push/pull, commit signing, gh, aws (any account via --profile; unpicked accounts prompt the user), and allowlisted hosts already work: just run the command. " +
			"Use this tool only when a command fails for lack of access that cannot work otherwise: " +
			"kind=secret to request a NEW API credential for an HTTPS API (give name, upstream https://host, header, format like 'Bearer {}'); then use the returned $ENV_VAR as the token in the documented header (it is a placeholder the proxy swaps for the real value). " +
			"kind=access to request read (or write) access to a directory outside the project. " +
			"kind=aws_login to log in to an AWS account ahead of time. " +
			"Requests show the user exactly what is asked; always give a short honest reason. pi restarts with the same session after secret/access grants.",
		parameters: Type.Object({
			kind: Type.Union([Type.Literal("secret"), Type.Literal("access"), Type.Literal("aws_login")]),
			name: Type.Optional(Type.String({ description: "secret: short name, e.g. stripe" })),
			upstream: Type.Optional(Type.String({ description: "secret: https://api.example.com" })),
			header: Type.Optional(Type.String({ description: "secret: header name, default Authorization" })),
			format: Type.Optional(Type.String({ description: "secret: value format with {} placeholder, default 'Bearer {}'" })),
			path: Type.Optional(Type.String({ description: "access: absolute directory path" })),
			write: Type.Optional(Type.Boolean({ description: "access: need write access" })),
			account: Type.Optional(Type.String({ description: "aws_login: account name" })),
			reason: Type.String({ description: "why you need it (shown to the user)" }),
		}),
		async execute(_id, p, _signal, _onUpdate, ctx) {
			let r: any;
			if (p.kind === "secret") r = await brokerCall({ op: "request-secret", name: p.name, upstream: p.upstream, header: p.header, format: p.format, reason: p.reason });
			else if (p.kind === "access") r = await brokerCall({ op: "request-access", path: p.path, write: p.write, reason: p.reason });
			else r = await brokerCall({ op: "aws-login", account: p.account });
			if (r.ok && r.restart) {
				restartWhenIdle(ctx);
				return { content: [{ type: "text", text: `Granted. pi restarts with the same session in a moment; then ${r.env_var ? `use $${r.env_var}` : "retry"}.` }], details: r };
			}
			return { content: [{ type: "text", text: r.ok ? `ok: ${JSON.stringify(r)}` : `not granted: ${r.error}` }], details: r, isError: !r.ok };
		},
	});
}
