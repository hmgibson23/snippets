// GitHub API guard (trusted side): which requests made with OUR injected token are allowed.
//
// The VM gets your gh token only through the credential route, so it can't be read,
// but it can be *used*. Without this guard the VM could e.g. add its own ssh key to
// your account (POST /user/keys) and keep access outside the VM. Rules:
//   deny     account/org takeover & persistence (keys, tokens, collaborators, hooks,
//            secrets, runners, branch protection, repo delete/transfer, ...)
//   push     API writes that change repository content (contents, git data, merges,
//            releases) need the same per-repo grant as `git push` (asks in the panel)
//   allow    everything else (reads, issues, PRs, comments, reviews, checks, gists)
// GraphQL: queries allowed; mutations matched by name with the same three classes.

const DENY_REST = [
	// your account: keys, emails, tokens, social accounts, interaction limits
	[/^\/user\/(keys|gpg_keys|ssh_signing_keys|emails|public_emails|social_accounts|interaction-limits|blocks|codespaces\/secrets)(\/|$)/, "*"],
	[/^\/user$/, "PATCH"],
	[/^\/user\/(installations|repository_invitations)(\/|$)/, "PUT|PATCH|DELETE"],
	[/^\/applications\//, "*"],
	[/^\/authorizations/, "*"],
	// repositories: delete, transfer, visibility/settings, access, automation, secrets
	[/^\/repos\/[^/]+\/[^/]+$/, "PATCH|DELETE"],
	[/^\/repos\/[^/]+\/[^/]+\/(transfer|collaborators|invitations|keys|hooks|actions\/(secrets|variables|runners|permissions|oidc)|environments|secret-scanning|dependabot\/secrets|codespaces\/secrets|rulesets|branches\/[^/]+\/protection|pages|vulnerability-alerts|automated-security-fixes|private-vulnerability-reporting|teams|autolinks|interaction-limits|properties)(\/|$)/, "POST|PUT|PATCH|DELETE"],
	[/^\/repos\/[^/]+\/[^/]+\/actions\/workflows\/[^/]+\/(enable|disable)$/, "PUT"],
	// orgs/teams/enterprises: membership, settings, hooks, secrets, runners
	[/^\/orgs\/[^/]+(\/(members|memberships|invitations|outside_collaborators|hooks|actions|secrets|dependabot|codespaces|teams|blocks|security-managers|credential-authorizations|personal-access-token(s|-requests)|settings|properties|rulesets|interaction-limits|organization-roles)(\/|$)|$)/, "POST|PUT|PATCH|DELETE"],
	[/^\/teams\//, "POST|PUT|PATCH|DELETE"],
	[/^\/enterprises\//, "POST|PUT|PATCH|DELETE"],
	// creating repos (exfiltration target) / forks to other owners are allowed: pushes still need a grant
];

const PUSH_REST = [
	[/^\/repos\/([^/]+)\/([^/]+)\/(contents|git\/(refs|commits|trees|blobs|tags)|merges|merge-upstream|releases|branches\/[^/]+\/rename|dispatches|actions\/workflows\/[^/]+\/dispatches|deployments|statuses)(\/|$)/, "POST|PUT|PATCH|DELETE"],
	[/^\/repos\/([^/]+)\/([^/]+)\/pulls\/\d+\/(merge|update-branch)$/, "PUT"],
];

const DENY_GQL = /\b(addSshKey|addGpgKey|createSshSigningKey|deleteSshKey|deleteGpgKey|addDeployKey|deleteDeployKey|createRepositoryRuleset|updateRepositoryRuleset|deleteRepositoryRuleset|createBranchProtectionRule|updateBranchProtectionRule|deleteBranchProtectionRule|updateRepository|deleteRepository|transferRepository|archiveRepository|unarchiveRepository|updateTeamsRepository|inviteEnterpriseAdmin|updateEnterprise\w*|updateOrganization\w*|removeOutsideCollaborator|updateIpAllowList\w*|createIpAllowListEntry|createEnvironment|deleteEnvironment|addEnterpriseSupportEntitlement|createSponsorship\w*|createApp\w*|regenerate\w*)\s*\(/;
const ASK_GQL = /\b(createGist|updateGist|createRepository|cloneTemplateRepository)\s*\(/;
const PUSH_GQL = /\b(createCommitOnBranch|createRef|updateRef|updateRefs|deleteRef|mergeBranch|mergePullRequest|enablePullRequestAutoMerge|updatePullRequestBranch|createRelease|updateRelease|deleteRelease)\s*\(/;

function methodMatches(spec, method) {
	return spec === "*" || spec.split("|").includes(method);
}

// data leaving through your account where anyone can read it: ask each time
const ASK_REST = [
	[/^\/gists(\/[^/]+)?$/, "POST|PATCH"],
	[/^\/user\/repos$/, "POST"],            // new repos (public by default for some tools)
	[/^\/orgs\/[^/]+\/repos$/, "POST"],
];

/** -> {action: "allow"} | {action: "deny", why} | {action: "push", repo} | {action: "ask", why}; repo = "owner/name" */
export function classifyRest(method, path) {
	method = method.toUpperCase();
	if (method === "GET" || method === "HEAD" || method === "OPTIONS") return { action: "allow" };
	const p = decodeURIComponent(path).replace(/\/+$/, "") || "/";
	for (const [re, spec] of ASK_REST) if (re.test(p) && methodMatches(spec, method)) return { action: "ask", why: `${method} ${p}` };
	for (const [re, spec] of DENY_REST) if (re.test(p) && methodMatches(spec, method)) return { action: "deny", why: `${method} ${p}` };
	for (const [re, spec] of PUSH_REST) {
		const m = re.exec(p);
		if (m && methodMatches(spec, method)) return { action: "push", repo: `${m[1]}/${m[2]}` };
	}
	return { action: "allow" };
}

/** GraphQL body -> same classes; repo unknown for push-class mutations (asks without a repo). */
export function classifyGraphql(bodyText) {
	let q = "";
	try {
		const j = JSON.parse(bodyText);
		q = Array.isArray(j) ? j.map((x) => x?.query ?? "").join("\n") : (j?.query ?? "");
	} catch {
		return { action: "deny", why: "unparseable GraphQL body" };
	}
	if (!/^\s*(#[^\n]*\n\s*)*mutation\b/m.test(q) && !/\bmutation\b/.test(q)) return { action: "allow" };
	const d = DENY_GQL.exec(q);
	if (d) return { action: "deny", why: `GraphQL ${d[1]}` };
	const a = ASK_GQL.exec(q);
	if (a) return { action: "ask", why: `GraphQL ${a[1]}` };
	const pmatch = PUSH_GQL.exec(q);
	if (pmatch) return { action: "push", repo: null, what: pmatch[1] };
	return { action: "allow" };
}
