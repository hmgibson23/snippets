// nono-pi network policy for the Gondolin VM (trusted side, runs inside vmd).
//
// Every HTTP(S) request from the guest goes through Gondolin's userspace network
// stack, which terminates TLS with its own CA. Here we decide, per request:
//   1. is the host allowed?            allowlist, else ask the supervisor (panel)
//   2. credential routes               placeholder header -> real token (from the supervisor)
//   3. AWS                             strip the guest's placeholder SigV4, re-sign with the
//                                      real STS session; deny ops that would mint credentials
// Nothing secret is ever written back into the guest: tokens live only in this process,
// cached briefly, fetched from the supervisor over its host-only ctl socket.
import { SignatureV4 } from "@smithy/signature-v4";
import { Sha256 } from "@aws-crypto/sha256-js";
import { classifyGraphql, classifyRest } from "./github-guard.mjs";

const AWS_DENY_OPS = new Set(["AssumeRole", "AssumeRoleWithSAML", "AssumeRoleWithWebIdentity", "GetSessionToken",
	"GetFederationToken", "GetAuthorizationToken", "CreateAccessKey", "GetCredentialsForIdentity"]);
const AWS_PLACEHOLDER = /^NONOPI\d{14}$/;   // NONOPI00000000000000 = "the current account"
const SIGNED_STREAM = /^STREAMING-(AWS4-HMAC-SHA256|UNSIGNED)-PAYLOAD/;
const SERVICE_ALIAS = { "api.ecr": "ecr", "bedrock-runtime": "bedrock", "bedrock-agent": "bedrock",
	"bedrock-agent-runtime": "bedrock", "runtime.sagemaker": "sagemaker" };
const GLOBAL = { "iam.amazonaws.com": ["us-east-1", "iam"], "sts.amazonaws.com": ["us-east-1", "sts"],
	"s3.amazonaws.com": ["us-east-1", "s3"], "route53.amazonaws.com": ["us-east-1", "route53"],
	"cloudfront.amazonaws.com": ["us-east-1", "cloudfront"], "organizations.us-east-1.amazonaws.com": ["us-east-1", "organizations"] };
const CRED_TTL_MS = 60_000;

function hostMatch(host, pattern) {
	if (pattern === host) return true;
	if (pattern.startsWith("*.")) return host.endsWith(pattern.slice(1));
	return false;
}

export function createPolicy({ ctl, getSpec, log }) {
	const credCache = new Map(); // key -> {value, at}
	const hostDecisions = new Map(); // host -> {ok, at}

	async function cached(key, fetcher, ttl = CRED_TTL_MS) {
		const c = credCache.get(key);
		if (c && Date.now() - c.at < ttl) return c.value;
		const v = await fetcher();
		if (v != null) credCache.set(key, { value: v, at: Date.now() });
		return v;
	}

	// ---------------------------------------------------------------- 1. host policy
	async function hostAllowed(host) {
		const spec = getSpec();
		if ((spec.net?.allow ?? []).some((p) => hostMatch(host, p))) return true;
		if (routeFor(host) || awsTarget(host)) return true; // credential routes are always reachable
		const d = hostDecisions.get(host);
		if (d && Date.now() - d.at < (d.ok ? 3600_000 : 30_000)) return d.ok;
		if (!spec.net?.ask) return false;
		const r = await ctl({ op: "net-approve", host });
		const ok = r.ok === true;
		hostDecisions.set(host, { ok, at: Date.now() });
		return ok;
	}

	// ---------------------------------------------------------------- 2. credential routes
	function routeFor(host, path = "/") {
		for (const [name, r] of Object.entries(getSpec().routes ?? {})) {
			if (r.host !== host) continue;
			if (r.paths && !r.paths.some((p) => (p.endsWith("/**") ? path.startsWith(p.slice(0, -3)) : path === p || path.startsWith(p + "/"))))
				continue;
			if (r.excludePaths?.some((p) => path.startsWith(p))) continue;
			return { name, ...r };
		}
		return null;
	}

	async function injectCredential(req, url) {
		const route = routeFor(url.hostname, url.pathname);
		if (!route) return null;
		const header = route.header ?? "authorization";
		const sent = req.headers.get(header) ?? "";
		// only replace OUR placeholder: a real token the guest brought itself (e.g. pi's
		// Copilot login) passes through untouched
		if (!sent.includes(route.placeholder)) return null;
		const value = await cached(`route:${route.name}`, async () => {
			const r = await ctl({ op: "route-secret", name: route.name, host: url.hostname });
			return r.ok ? r.value : null;
		});
		if (!value) return new Response(`nono-pi: no credential for ${route.name}\n`, { status: 401 });
		if (route.guard === "github") {
			const blocked = await guardGithub(req, url);
			if (blocked) return blocked;
		}
		const h = new Headers(req.headers);
		h.set(header, (route.format ?? "Bearer {}").replace("{}", value));
		log(`route ${route.name} ${req.method} ${url.hostname}${url.pathname.slice(0, 60)}`);
		return new Request(req, { headers: h });
	}

	// GitHub: requests made with YOUR token (injected above) are classified first
	async function guardGithub(req, url) {
		let c;
		if (url.pathname === "/graphql" && req.method === "POST") {
			c = classifyGraphql(new TextDecoder().decode(await req.clone().arrayBuffer()));
		} else {
			c = classifyRest(req.method, url.pathname);
		}
		if (c.action === "allow") return null;
		if (c.action === "deny") {
			log(`github deny ${c.why}`);
			return new Response(JSON.stringify({ message: `nono-pi: blocked: ${c.why} (account/repo admin changes with your token are not allowed from the VM)` }),
				{ status: 403, headers: { "content-type": "application/json" } });
		}
		// content writes through the API = a push: same grant as `git push` (asks in the panel)
		const r = await ctl({ op: "gh-push-check", host: "github.com", repo: c.repo ?? "", what: c.what ?? `${req.method} ${url.pathname}` });
		if (r.ok) return null;
		log(`github push denied ${c.repo ?? c.what}`);
		return new Response(JSON.stringify({ message: r.error ?? "nono-pi: push denied" }), { status: 403, headers: { "content-type": "application/json" } });
	}

	// ---------------------------------------------------------------- 3. AWS re-signing
	function awsTarget(host) {
		if (GLOBAL[host]) return { region: GLOBAL[host][0], service: GLOBAL[host][1] };
		if (!host.endsWith(".amazonaws.com")) return null;
		const parts = host.slice(0, -".amazonaws.com".length).split(".");
		const ri = parts.findIndex((p) => /^[a-z]{2}(-gov)?-[a-z]+-\d$/.test(p));
		if (ri < 0) {
			// legacy global S3: <bucket>.s3.amazonaws.com
			return parts[parts.length - 1] === "s3" ? { region: "us-east-1", service: "s3" } : null;
		}
		const region = parts[ri];
		const before = parts.slice(0, ri);   // e.g. ["sts"], ["my-bucket","s3"], ["bedrock-runtime"], ["api","ecr"], ["s3","dualstack"]
		if (before.some((p) => p === "s3" || p.startsWith("s3-"))) return { region, service: "s3" };
		const joined = before.join(".");
		if (SERVICE_ALIAS[joined]) return { region, service: SERVICE_ALIAS[joined] };
		const svc = before[before.length - 1];
		return svc ? { region, service: SERVICE_ALIAS[svc] ?? svc } : null;
	}

	function placeholderKey(auth) {
		const m = /Credential=([^/,\s]+)\//.exec(auth ?? "");
		return m && AWS_PLACEHOLDER.test(m[1]) ? m[1] : null;
	}

	async function awsOperation(req, url, bodyText) {
		const target = req.headers.get("x-amz-target"); // JSON protocols: "Service.Operation"
		if (target) return target.split(".").pop();
		const q = url.searchParams.get("Action");
		if (q) return q;
		if (bodyText) {
			const m = /(?:^|&)Action=([A-Za-z]+)/.exec(bodyText);
			if (m) return m[1];
		}
		return null;
	}

	async function resignAws(req, url, target) {
		const key = placeholderKey(req.headers.get("authorization"));
		if (!key) return null; // unsigned / presigned / real creds the user typed: not ours
		const spec = getSpec();
		let account = spec.aws?.profiles?.[key];
		if (!account && key === spec.aws?.currentKey) {
			// [default] = the current account, switched on the host (`aws-use`, the panel)
			const r = await ctl({ op: "aws-current" });
			account = r.ok ? r.account : null;
			if (!account) return new Response("nono-pi: no AWS account selected yet: run `aws-use` (or pick one in the nono-pi tab), or use --profile <account>\n", { status: 403 });
		}
		if (!account) return new Response("nono-pi: unknown AWS profile placeholder\n", { status: 403 });

		const contentSha = req.headers.get("x-amz-content-sha256");
		let body = req.body ? new Uint8Array(await req.clone().arrayBuffer()) : undefined;
		const bodyText = body && body.length < 1_000_000 ? new TextDecoder().decode(body) : "";
		const op = await awsOperation(req, url, bodyText);
		if (["sts", "iam", "ecr", "cognito-identity"].includes(target.service) && op && AWS_DENY_OPS.has(op)) {
			log(`aws deny ${target.service}:${op} (${account})`);
			return new Response(`nono-pi: ${op} is blocked (it would hand real credentials to the VM)\n`, { status: 403 });
		}
		const creds = await cached(`aws:${account}`, async () => {
			const r = await ctl({ op: "aws-creds", account });
			return r.ok ? r.creds : null;
		}, 120_000);
		if (!creds) return new Response(`nono-pi: no AWS session for ${account} (denied or login failed)\n`, { status: 403 });

		// headers to sign: everything the client sent except its own auth/date/token
		const headers = {};
		for (const [k, v] of req.headers) {
			if (["authorization", "x-amz-date", "x-amz-security-token", "x-amz-content-sha256", "host",
				"content-length", "connection", "transfer-encoding", "expect"].includes(k)) continue;
			headers[k] = v;
		}
		headers.host = url.host;
		// streaming SigV4 (aws-chunked uploads): decode the chunks into one plain body and sign it whole
		if (contentSha && SIGNED_STREAM.test(contentSha) && body) {
			body = decodeAwsChunked(body);
			delete headers["content-encoding"];
			delete headers["x-amz-decoded-content-length"];
			delete headers["x-amz-trailer"];
		}
		const signer = new SignatureV4({
			credentials: { accessKeyId: creds.AccessKeyId, secretAccessKey: creds.SecretAccessKey, sessionToken: creds.SessionToken },
			region: target.region, service: target.service, sha256: Sha256, uriEscapePath: target.service !== "s3",
			applyChecksum: true,
		});
		if (target.service === "s3") headers["x-amz-content-sha256"] = body?.length ? await sha256hex(body) : await sha256hex(new Uint8Array());
		const query = {};
		for (const [k, v] of url.searchParams) query[k] = v;
		const signed = await signer.sign({
			method: req.method, protocol: url.protocol, hostname: url.hostname, port: url.port ? Number(url.port) : undefined,
			path: url.pathname, query, headers, body,
		});
		const out = new Headers();
		for (const [k, v] of Object.entries(signed.headers)) if (k !== "host") out.set(k, v);
		log(`aws ${target.service}${op ? ":" + op : ""} ${account} ${req.method} ${url.pathname.slice(0, 50)}`);
		return new Request(url, { method: req.method, headers: out, body: body?.length ? body : undefined, duplex: "half" });
	}

	// ---------------------------------------------------------------- hooks
	return {
		isRequestAllowed: async (req) => {
			const host = new URL(req.url).hostname;
			const ok = await hostAllowed(host);
			if (!ok) log(`deny ${host}`);
			return ok;
		},
		onRequest: async (orig) => {
			const url = new URL(orig.url);
			// encoding first: AWS signs every header the client sent, so it must not change after
			const enc = identityEncoding(orig);
			const req = enc ?? orig;
			const aws = awsTarget(url.hostname);
			const out = (aws && getSpec().aws?.enabled ? await resignAws(req, url, aws) : null)
				?? await injectCredential(req, url);
			return out ?? enc ?? undefined;
		},
		forget() {
			credCache.clear();
			hostDecisions.clear();
		},
	};
}

// Ask upstreams for an uncompressed body (returns a new Request, or null if nothing to change).
function identityEncoding(req) {
	const ae = req.headers.get("accept-encoding");
	if (!ae || ae.trim() === "identity") return null;
	const h = new Headers(req.headers);
	h.set("accept-encoding", "identity");
	return new Request(req, { headers: h });
}

async function sha256hex(buf) {
	const h = new Sha256();
	h.update(buf);
	return Buffer.from(await h.digest()).toString("hex");
}

// aws-chunked: <hex-size>;chunk-signature=<sig>\r\n<data>\r\n ... 0;chunk-signature=...\r\n[trailers]\r\n
export function decodeAwsChunked(buf) {
	const out = [];
	let i = 0;
	const b = Buffer.from(buf);
	while (i < b.length) {
		const eol = b.indexOf("\r\n", i);
		if (eol < 0) break;
		const size = parseInt(b.subarray(i, eol).toString().split(";")[0], 16);
		if (!Number.isFinite(size) || size === 0) break;
		out.push(b.subarray(eol + 2, eol + 2 + size));
		i = eol + 2 + size + 2;
	}
	return new Uint8Array(Buffer.concat(out));
}
