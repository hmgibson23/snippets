// Offline tests for vm/netpolicy.mjs (run: `node vm/netpolicy.test.mjs`).
// The guest signs AWS requests with a placeholder key; the policy must re-sign them with the
// real session so that an independent signer with the real creds produces the same signature.
import assert from "node:assert/strict";
import { createPolicy, decodeAwsChunked } from "./netpolicy.mjs";
import { classifyGraphql, classifyRest } from "./github-guard.mjs";
import { SignatureV4 } from "@smithy/signature-v4";
import { Sha256 } from "@aws-crypto/sha256-js";

const REAL = { AccessKeyId: "AKIAREALREALREALREAL", SecretAccessKey: "realsecretrealsecretrealsecretrealsecret", SessionToken: "realsession" };
const PH = "NONOPI00000000000001";
const spec = {
	aws: { enabled: true, profiles: { [PH]: "acct-a" }, currentKey: "NONOPI00000000000000" },
	routes: { github: { host: "api.github.com", placeholder: "nonopi_github_x", format: "token {}", header: "authorization", excludePaths: ["/copilot_internal"], guard: "github" } },
	net: { allow: ["registry.npmjs.org", "*.pythonhosted.org"], ask: true },
};
const asked = [];
let current = null;
const pushAsked = [];
const pol = createPolicy({
	getSpec: () => spec,
	log: () => {},
	ctl: async (o) => {
		if (o.op === "aws-creds") return o.account === "acct-a" ? { ok: true, creds: REAL } : { ok: false };
		if (o.op === "aws-current") return { ok: true, account: current };
		if (o.op === "route-secret") return o.name === "github" && o.host === "api.github.com" ? { ok: true, value: "gho_REAL" } : { ok: false };
		if (o.op === "net-approve") { asked.push(o.host); return { ok: o.host === "ok.example.com" }; }
		if (o.op === "gh-push-check") { pushAsked.push(o.repo || o.what); return { ok: o.repo === "me/granted", error: "denied" }; }
		return { ok: false };
	},
});

const q = (u) => { const o = {}; for (const [k, v] of u.searchParams) o[k] = v; return o; };
async function guestReq(url, method, headers, body, service, region) {
	const u = new URL(url);
	const s = new SignatureV4({ credentials: { accessKeyId: PH, secretAccessKey: "x" }, region, service, sha256: Sha256, uriEscapePath: service !== "s3" });
	const signed = await s.sign({ method, protocol: u.protocol, hostname: u.hostname, path: u.pathname, query: q(u), headers: { host: u.host, ...headers }, body });
	return new Request(url, { method, headers: signed.headers, body, duplex: "half" });
}
async function verify(r, service, region) {
	const u = new URL(r.url);
	const auth = r.headers.get("authorization");
	const signedHeaders = /SignedHeaders=([^,]+)/.exec(auth)[1].split(";");
	const headers = { host: u.host };
	for (const [k, v] of r.headers) if (k !== "authorization" && signedHeaders.includes(k)) headers[k] = v;
	const s = new SignatureV4({ credentials: { accessKeyId: REAL.AccessKeyId, secretAccessKey: REAL.SecretAccessKey, sessionToken: REAL.SessionToken }, region, service, sha256: Sha256, uriEscapePath: service !== "s3" });
	const body = r.body ? new Uint8Array(await r.arrayBuffer()) : undefined;
	const d = r.headers.get("x-amz-date").replace(/(\d{4})(\d\d)(\d\d)T(\d\d)(\d\d)(\d\d)Z/, "$1-$2-$3T$4:$5:$6Z");
	const again = await s.sign({ method: r.method, protocol: u.protocol, hostname: u.hostname, path: u.pathname, query: q(u), headers, body }, { signingDate: new Date(d) });
	assert.ok(auth.includes(`Credential=${REAL.AccessKeyId}/`), "re-signed with the real key");
	assert.equal(r.headers.get("x-amz-security-token"), REAL.SessionToken);
	assert.equal(again.headers.authorization, auth, "signature verifies with the real creds");
	assert.ok(!JSON.stringify([...r.headers]).includes(PH), "placeholder key is gone");
}

// --- AWS: re-signed and verifiable
for (const [url, method, headers, body, service, region] of [
	["https://sts.eu-west-2.amazonaws.com/", "POST", { "content-type": "application/x-www-form-urlencoded" }, "Action=GetCallerIdentity&Version=2011-06-15", "sts", "eu-west-2"],
	["https://my-bucket.s3.eu-west-2.amazonaws.com/?list-type=2&prefix=a%20b/", "GET", {}, undefined, "s3", "eu-west-2"],
	["https://my-bucket.s3.eu-west-2.amazonaws.com/dir/a%20b+c.txt", "PUT", { "content-type": "text/plain" }, "hello", "s3", "eu-west-2"],
	["https://dynamodb.eu-west-2.amazonaws.com/", "POST", { "content-type": "application/x-amz-json-1.0", "x-amz-target": "DynamoDB_20120810.ListTables" }, "{}", "dynamodb", "eu-west-2"],
	["https://bedrock-runtime.us-east-1.amazonaws.com/model/eu.anthropic.claude-haiku-4-5%3A0/invoke", "POST", { "content-type": "application/json" }, "{\"x\":1}", "bedrock", "us-east-1"],
]) {
	const out = await pol.onRequest(await guestReq(url, method, headers, body, service, region));
	assert.ok(out instanceof Request, `${url} -> Request`);
	assert.equal(out.url, url);
	await verify(out, service, region);
}

// --- AWS: [default] (current-account key) follows the current account; none selected -> 403 hint
{
	const mk = async () => {
		const u = new URL("https://sts.eu-west-2.amazonaws.com/");
		const sg = new SignatureV4({ credentials: { accessKeyId: "NONOPI00000000000000", secretAccessKey: "x" }, region: "eu-west-2", service: "sts", sha256: Sha256 });
		const signed = await sg.sign({ method: "POST", protocol: u.protocol, hostname: u.hostname, path: "/", query: {}, headers: { host: u.host, "content-type": "application/x-www-form-urlencoded" }, body: "Action=GetCallerIdentity&Version=2011-06-15" });
		return new Request(u, { method: "POST", headers: signed.headers, body: "Action=GetCallerIdentity&Version=2011-06-15", duplex: "half" });
	};
	current = null;
	const none = await pol.onRequest(await mk());
	assert.ok(none instanceof Response && none.status === 403 && /aws-use/.test(await none.text()), "no account selected: hint");
	current = "acct-a";
	const out = await pol.onRequest(await mk());
	assert.ok(out instanceof Request, "current account: re-signed");
	await verify(out, "sts", "eu-west-2");
}

// --- AWS: credential-minting operations are blocked
for (const body of ["Action=AssumeRole&Version=2011-06-15&RoleArn=x", "Action=GetSessionToken&Version=2011-06-15"]) {
	const out = await pol.onRequest(await guestReq("https://sts.amazonaws.com/", "POST", { "content-type": "application/x-www-form-urlencoded" }, body, "sts", "us-east-1"));
	assert.ok(out instanceof Response && out.status === 403, body);
}
{
	const out = await pol.onRequest(await guestReq("https://api.ecr.eu-west-2.amazonaws.com/", "POST", { "content-type": "application/x-amz-json-1.1", "x-amz-target": "AmazonEC2ContainerRegistry_V20150921.GetAuthorizationToken" }, "{}", "ecr", "eu-west-2"));
	assert.equal(out.status, 403, "ecr GetAuthorizationToken blocked");
}
// a request signed with something that is NOT our placeholder passes through untouched
{
	const r = new Request("https://sts.amazonaws.com/", { method: "POST", headers: { authorization: "AWS4-HMAC-SHA256 Credential=AKIAOTHER/20260101/us-east-1/sts/aws4_request, SignedHeaders=host, Signature=00" }, body: "Action=GetCallerIdentity", duplex: "half" });
	assert.equal(await pol.onRequest(r), undefined);
}
// aws-chunked bodies are decoded before re-signing
assert.equal(Buffer.from(decodeAwsChunked(Buffer.from("5;chunk-signature=aa\r\nhello\r\n6;chunk-signature=bb\r\n world\r\n0;chunk-signature=cc\r\n\r\n"))).toString(), "hello world");

// --- credential routes: placeholder -> real, only on its host and paths
{
	const out = await pol.onRequest(new Request("https://api.github.com/user", { headers: { authorization: "token nonopi_github_x" } }));
	assert.equal(out.headers.get("authorization"), "token gho_REAL");
	assert.equal(await pol.onRequest(new Request("https://api.github.com/copilot_internal/v2/token", { headers: { authorization: "token nonopi_github_x" } })), undefined, "copilot path excluded");
	assert.equal(await pol.onRequest(new Request("https://api.github.com/user", { headers: { authorization: "token gho_user_brought" } })), undefined, "foreign tokens pass through");
	assert.equal(await pol.onRequest(new Request("https://evil.example.com/", { headers: { authorization: "token nonopi_github_x" } })), undefined, "never injected on other hosts");
}

// --- GitHub guard: your token can't be used for account/repo admin or persistence
{
	const gh = (method, path, body) => pol.onRequest(new Request(`https://api.github.com${path}`,
		{ method, headers: { authorization: "token nonopi_github_x", "content-type": "application/json" }, body, duplex: "half" }));
	const isBlocked = async (r) => r instanceof Response && r.status === 403;
	for (const [m, p] of [["POST", "/user/keys"], ["POST", "/user/ssh_signing_keys"], ["POST", "/user/gpg_keys"],
		["POST", "/repos/me/repo/keys"], ["POST", "/repos/me/repo/hooks"], ["PUT", "/repos/me/repo/collaborators/evil"],
		["PUT", "/repos/me/repo/actions/secrets/X"], ["DELETE", "/repos/me/repo"], ["PATCH", "/repos/me/repo"],
		["POST", "/repos/me/repo/transfer"], ["PUT", "/repos/me/repo/branches/main/protection"], ["PATCH", "/user"],
		["PUT", "/orgs/newsuk/memberships/evil"], ["POST", "/orgs/newsuk/hooks"], ["POST", "/repos/me/repo/actions/runners/registration-token"]]) {
		assert.ok(await isBlocked(await gh(m, p, m === "DELETE" ? undefined : "{}")), `${m} ${p} must be blocked`);
	}
	// reads and normal collaboration work
	for (const [m, p] of [["GET", "/user/keys"], ["GET", "/repos/me/repo"], ["POST", "/repos/me/repo/issues"], ["POST", "/repos/me/repo/pulls"],
		["POST", "/repos/me/repo/issues/1/comments"], ["POST", "/repos/me/repo/pulls/1/reviews"], ["PATCH", "/repos/me/repo/pulls/1"], ["POST", "/gists"]]) {
		const r = await gh(m, p, m === "GET" ? undefined : "{}");
		assert.ok(r instanceof Request && r.headers.get("authorization") === "token gho_REAL", `${m} ${p} allowed with the real token`);
	}
	// content writes through the API need the repo's push grant
	const ok = await gh("PUT", "/repos/me/granted/contents/README.md", "{}");
	assert.ok(ok instanceof Request, "granted repo: API content write allowed");
	assert.ok(await isBlocked(await gh("PUT", "/repos/me/other/contents/README.md", "{}")), "no grant: API content write refused");
	assert.ok(await isBlocked(await gh("POST", "/repos/me/other/git/refs", "{}")), "no grant: create ref refused");
	assert.ok(await isBlocked(await gh("PUT", "/repos/me/other/pulls/3/merge", "{}")), "no grant: merge refused");
	assert.deepEqual(pushAsked, ["me/granted", "me/other", "me/other", "me/other"]);
	// GraphQL
	const q = (query) => gh("POST", "/graphql", JSON.stringify({ query }));
	assert.ok((await q("query { viewer { login } }")) instanceof Request, "graphql query allowed");
	assert.ok(await isBlocked(await q('mutation { addSshKey(input:{title:"x", key:"ssh-ed25519 AAA"}) { clientMutationId } }')), "graphql addSshKey blocked");
	assert.ok(await isBlocked(await q("mutation { deleteRepository(input:{repositoryId:\"R\"}) { clientMutationId } }")), "graphql deleteRepository blocked");
	assert.ok((await q('mutation { addComment(input:{subjectId:"I", body:"hi"}) { clientMutationId } }')) instanceof Request, "graphql addComment allowed");
	assert.ok(await isBlocked(await q('mutation { createCommitOnBranch(input:{}) { clientMutationId } }')), "graphql commit without grant refused");
	assert.equal(classifyRest("GET", "/user/keys").action, "allow");
	assert.equal(classifyGraphql("not json").action, "deny");
	// a token the VM brought itself is not ours: untouched (and not guarded - it isn't your token)
	assert.equal(await pol.onRequest(new Request("https://api.github.com/user/keys", { method: "POST", headers: { authorization: "token gho_theirs" }, body: "{}", duplex: "half" })), undefined);
}

// --- host policy
assert.equal(await pol.isRequestAllowed(new Request("https://registry.npmjs.org/x")), true);
assert.equal(await pol.isRequestAllowed(new Request("https://files.pythonhosted.org/x")), true);
assert.equal(await pol.isRequestAllowed(new Request("https://sts.eu-west-2.amazonaws.com/")), true, "credential routes reachable");
assert.equal(await pol.isRequestAllowed(new Request("https://ok.example.com/")), true);
assert.equal(await pol.isRequestAllowed(new Request("https://no.example.com/")), false);
assert.equal(await pol.isRequestAllowed(new Request("https://no.example.com/again")), false);
assert.deepEqual(asked, ["ok.example.com", "no.example.com"], "decisions are cached");

console.log("netpolicy: all tests passed");
