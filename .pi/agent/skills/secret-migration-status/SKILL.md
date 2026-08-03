---
name: secret-migration-status
description: "Check whether specific CircleCI context/project secrets for News UK repos (newsuk / newscorp-ghfb) have actually been migrated to AWS Secrets Manager and are loaded at CircleCI build time, or are still sitting as plaintext CircleCI context vars. Point it at a repo + secret name(s) (or a CircleCI inventory row like 'project  context  masked  variable_name'). Traces real consumers via org-wide code search, checks whether the secret is already handled upstream by a shared orb/service before assuming per-app migration is needed, verifies live AWS Secrets Manager state (never assumes from code search alone), checks IAM readiness, and — when migration is actually needed — drafts the IAM PR and code PR. Use when the user asks 'is X secret rotated / in Secrets Manager', 'what's the status of this context', or 'migrate this secret to SM'."
---

# Secret Migration Status Check (News UK / newsuk + newscorp-ghfb)

Answers one question rigorously: **for this repo + secret name, is it actually in Secrets
Manager and loaded at build time, or still a plaintext CircleCI context var — and if not,
what's the smallest correct PR to fix it?**

Built from a real investigation (`wd-release-cli` context → `wd-virgin-radio-player`). Every
gotcha below was a real mistake caught mid-flight, not theoretical. Read them before
skipping steps.

Companion to `rotate-secrets` / `rotate-secrets-auto` (use those for the full interactive
rotation/write flow, provider validation, and org-wide bulk runs). This skill is the fast,
narrow **status-check + minimal-fix** path for one repo/context/secret at a time.

---

## Hard rules (inherited, non-negotiable)

- Never print, echo, or ask the user to paste a real secret value into chat.
- **CircleCI context and project env var values are write-only** — no API, no UI, no token
  scope can ever read back a value once set. "Copy the existing value into SM" is not a real
  option. Migrating a context-only secret is always a **genuine rotation** (new credential
  generated, old one revoked), never a copy. Say this explicitly to the user before creating
  any placeholder secret.
- Never assume a secret does/doesn't exist in Secrets Manager from code search alone.
  Code search proves what's *referenced*, not what's *stored*. Get live AWS access
  (`aws sts get-caller-identity`) and run `aws secretsmanager list-secrets` /
  `describe-secret` against the actual account before claiming existence or absence.
- Never assume the SM naming convention from the repo name. Check what's *actually there*
  first — conventions drift (e.g. `wd-virgin-radio-player` the repo vs `wd-virgin/` the real
  SM prefix already in use for that app).
- Never assume the first column of a CircleCI inventory row is a GitHub repo. It may be a
  **CircleCI context name** shared across many repos. If `gh api repos/<org>/<name>` 404s
  and org-wide code search shows the name only ever appears as `context: <name>` inside
  other repos' `.circleci/config.yml`, that's your answer — it's a context, not a repo.
- Before concluding a secret needs migrating, check whether it's **already handled upstream**
  — by the orb itself, by a shared backend service, or by org onboarding tooling — rather
  than by the consuming repo. Real example: `RELEASE_API_TOKEN` was already fetched from SM
  *inside* the `nukengprod/release-api` orb's own job definitions (since ~Oct 2023); no
  per-app work was needed for it at all. `RELEASE_API_ORB_OIDC_ROLE` is a shared, non-secret,
  org-wide ARN provisioned by a Backstage scaffolder — never scope IAM to it per-app.
- Check repo push access (`gh api repos/<org>/<repo> --jq .permissions`) **before** attempting
  to branch/push. If `push: false`, check `allow_forking`; if forking is also blocked by org
  policy, stop and hand the user a validated diff/patch instead of failing silently or
  guessing at write access.
- IAM PRs stay **draft** until the paired code PR exists, the SM secret exists, and both link
  to each other (see `rotate-secrets-auto` STEP F). If the user explicitly asks to un-draft
  anyway, comply but leave a PR comment stating exactly what's still outstanding — don't
  silently imply it's done.
- `terraform fmt -check -diff` (or a brace/bracket balance check if terraform isn't
  installed) on every Terraform edit before pushing. Diff the file before/after your edit to
  confirm the change is isolated to what you intended.

---

## Step 1 — Resolve what you're actually looking at

Given a CircleCI inventory row (`project  context  masked  variable_name`, org column
optional) or a plain repo+secret ask:

```bash
gh api repos/<org>/<name> --jq '{full_name,private,archived}' 2>&1
```

- **200** → it's a real repo. Continue to Step 2 treating `<name>` as the repo.
- **404** → check org membership/scopes aren't the problem (`gh api orgs/<org>/memberships/<you>`,
  `gh auth status`), then org-wide search for the literal string:
  ```bash
  gh api "search/code?q=%22<name>%22+org:<org>" --jq '.items[] | .repository.full_name + " :: " + .path'
  ```
  If every hit is `context: <name>` (or `context: [<name>]`) inside *other* repos'
  `.circleci/config.yml`, `<name>` is a **shared CircleCI context**, not a repo. Note every
  consuming repo found — that's your real scope, and the secret's blast radius.

## Step 2 — Trace every real consumer

For each consuming repo, pull `.circleci/config.yml` and grep for the secret name(s) plus
`context:`, `orbs:`, `aws-secret-name`, `secretsmanager`:

```bash
gh api "repos/<org>/<repo>/contents/.circleci/config.yml" --jq '.content' | base64 -d > /tmp/<repo>.yml
grep -n "context:\|<SECRET_VAR>\|aws-secret-name\|secretsmanager\|orbs:" /tmp/<repo>.yml
```

Note exactly how the var is passed to whatever job/orb consumes it (a literal param, an
`env_var_name`-typed orb param, a `pre-steps` hook, etc.) — this dictates the fix.

## Step 3 — Check whether it's already handled upstream (don't reinvent)

Before assuming the consuming repo needs new plumbing, check:

- **The orb's own source.** If the consumer is a shared org orb (`nukengprod/*` etc.), pull
  its job/command definitions from the orb's source repo (usually `newsuk/nuk-circle-orbs`)
  and check if it *already* fetches the credential from SM internally:
  ```bash
  gh api repos/newsuk/nuk-circle-orbs/contents/orbs/<orb>/jobs/<job>.yml --jq '.content' | base64 -d
  ```
  Check `git log --path` on that file for when SM support was added — a repo pinned to an
  **older orb version than that date** genuinely needs the old plaintext var; a repo on a
  newer pin may already get it for free.
- **Org onboarding tooling.** Check for a Backstage scaffolder / setup script that documents
  the canonical env vars for this integration (search `global-backstage` or similar) — it
  tells you which vars are shared+non-secret (provisioned automatically, safe to hardcode)
  vs which are genuinely per-team custom secrets (manually added, actually need migrating).
- **The backend service's own IaC**, if the credential is consumed server-side rather than
  client-side (check `helm/templates/externalsecrets.yaml`, `terraform/*/secrets.tf` in the
  service's own repo) — it may already be SM-backed under a *different* name than what the
  client passes, meaning the client-side var is a separate, possibly-dead credential.
- **Whether the var is even referenced anywhere in code at all.** Org-wide `search/code` for
  the exact var name across every consumer. Zero hits anywhere (orb, backend, all consuming
  repos) = likely dead/unused. Flag as "confirm with owning team before deleting", don't
  silently migrate a var nothing reads.

## Step 4 — Verify live AWS state (never skip this)

```bash
aws sts get-caller-identity --no-cli-pager
```

If this fails, stop and ask the user for access to the right account(s) — do not proceed on
inference alone. Map repo/context → AWS account via:

```bash
# Account alias → ID map (source of truth, can drift from any skill's cached copy)
gh api repos/newscorp-ghfb/nuk-cloudnative-aws-iam/contents/circleci-user-role/variables.tf \
  --jq '.content' | base64 -d | sed -n '/variable "accounts"/,/^}/p'
```

Known News UK Wireless/Broadcasting account IDs (confirmed live this session — verify
against the live `variables.tf` above if working outside this team):

| Alias | Account ID | Env |
|---|---|---|
| `aws-wireless-dev` | `101255119164` | dev |
| `aws-wireless-production` | `395514801065` | staging + prod (same account, split by tag) |

Once authenticated against the right account:

```bash
aws secretsmanager list-secrets --region eu-west-1 --no-cli-pager \
  --query "SecretList[].Name" --output text | tr '\t' '\n' | sort
```

Read the **actual existing prefix convention for this app** from that list (it may not match
the repo name — e.g. `wd-virgin/` not `wd-virgin-radio-player/`). Confirm no duplicate/
pre-existing secret for what you're about to create. If a precedent secret exists for a
sibling app doing the same integration, `describe-secret` it for its **tags only** (never
its value) to mirror the tagging convention:

```bash
aws secretsmanager describe-secret --secret-id <precedent> --region eu-west-1 --no-cli-pager \
  --query '{ARN:ARN,Tags:Tags}'
```

## Step 5 — Check IAM readiness

Determine the consumer model first (same as `rotate-secrets-auto` Step 0): **CI job** (secret
used only inside the pipeline) vs **runtime artifact** (Lambda/ECS/K8s reads it at runtime).
This decides which role needs the grant.

Find the existing OIDC role(s) for the repo:

```bash
gh api repos/newscorp-ghfb/nuk-cloudnative-aws-iam/contents/circleci-user-role --jq '.[].name' \
  > /tmp/iam-files.txt
grep -iE "(^|-)(${REPO}|${APP_NOPREFIX})(-|\.)" /tmp/iam-files.txt
```

Role file naming and location are **not fixed** — small/shared teams (e.g. Wireless) often
keep every app's role in one shared `roles-policies-<team>-<env>.tf` file rather than one
file per app; larger apps may own their IAM in a dedicated `<app>-infrastructure`/
`<app>-genesis-infrastructure` repo instead of `nuk-cloudnative-aws-iam` at all — check both.
If the role already grants `secretsmanager:GetSecretValue` scoped to the right path, no IAM
change is needed; just wire the code. If scoped too narrowly or absent, that's the IAM PR.

## Step 6 — Draft the IAM PR (only if needed)

`newscorp-ghfb` blocks `git clone` via SAML SSO — use `gh api` contents PUT, not git, for
every read/write on IAM repos:

```bash
gh api repos/newscorp-ghfb/nuk-cloudnative-aws-iam/contents/circleci-user-role/<file>.tf --jq '.content' \
  | base64 -d > /tmp/<file>.tf
# edit locally (add a read-only secretsmanager statement scoped to the real path from Step 4)
terraform fmt -check -diff /tmp/<file>.tf
diff /tmp/<file>.tf <(gh api .../contents/.../<file>.tf --jq '.content' | base64 -d)  # confirm isolated
MAIN_SHA=$(gh api repos/newscorp-ghfb/nuk-cloudnative-aws-iam/git/ref/heads/master --jq '.object.sha')
gh api -X POST repos/newscorp-ghfb/nuk-cloudnative-aws-iam/git/refs \
  --field ref="refs/heads/chore/<app>-sm-secrets-$(date +%Y-%m-%d)" --field sha="${MAIN_SHA}"
gh api -X PUT repos/newscorp-ghfb/nuk-cloudnative-aws-iam/contents/circleci-user-role/<file>.tf \
  --field message="..." --field content="$(base64 < /tmp/<file>.tf)" \
  --field branch="<branch>" --field sha="<existing-file-sha>"
gh pr create --repo newscorp-ghfb/nuk-cloudnative-aws-iam --draft --base master --head <branch> ...
```

Note: default branch may be `master` not `main` on older infra repos — check
`gh api repos/<org>/<repo> --jq .default_branch` first, don't assume.

Grant **read-only** (`GetSecretValue` + `DescribeSecret`) scoped to the real path with a
trailing `*` (AWS's 6-char ARN suffix), unless this same PR's Terraform also creates the
secret (then add the write set — see `rotate-secrets-auto` for the full action table). Keep
draft until Step 7's code PR exists; comment (don't silently merge) if the user asks to
un-draft early.

## Step 7 — Draft the code PR

Check push access **first**:

```bash
gh api repos/<org>/<repo> --jq '.permissions'
```

- `push: true` → proceed with the normal branch/commit/PR flow (git if SSO allows, `gh api`
  contents PUT if not).
- `push: false` → check `allow_forking`; if fork also fails with an org policy 403, **stop**.
  Prepare the full diff/patched file locally, validate it (YAML/HCL parse + isolated diff
  check), and hand it to the user with exact PR title/body text to use themselves. Don't
  guess or retry blindly.

Wiring pattern for a CI-job consumer (mirror the closest existing sibling repo that already
does this, e.g. `wd-talksport-radio`'s `aws-secrets-manager/get-aws-secret` pre-step):

```yaml
orbs:
  aws-secrets-manager: nukengprod/aws-secrets-manager@<version matching a working sibling repo>

commands:
  fetch_<credential>:
    steps:
      - aws-cli/setup:
          role_arn: <the role from Step 5/6>
      - aws-secrets-manager/get-aws-secret:
          aws-secret-name: <real SM path from Step 4>
          key-var-name: <ENV_VAR_NAME the consumer expects>
```

Invoke as `pre-steps` on the job(s) that need it; drop the old `context:` entry once every
var that job needed is covered another way. If a var truly isn't secret (e.g. a username or
a shared ARN), prefer hardcoding it or moving it to a plain (non-context) CircleCI project
env var rather than migrating it into Secrets Manager — SM is for secrets, not config.

Validate before pushing:
```bash
python3 -c "import yaml; yaml.safe_load(open('<file>')); print('YAML OK')"
diff <(gh api .../contents/.circleci/config.yml --jq '.content' | base64 -d) <file>  # isolated?
```

## Step 8 — Report status honestly

Always state explicitly, per secret:
- **Where it lives today** (context name, or already-SM with real path).
- **Whether SM already has it** (live-verified, not inferred) — and if not, that a
  placeholder was created and a **real value requires a genuine rotation**, not a copy.
- **What's blocking "done"**: SM secret real value not written / code PR not merged / IAM PR
  not applied / no push access / need a `CIRCLE_TOKEN` to set plain project env vars (this
  skill has no way to read or write CircleCI context/project env vars directly without one —
  say so rather than silently skipping that step).
- Never say "migrated" or "rotated" unless every one of: IAM grants live, SM secret has a
  real (non-placeholder) value, code PR merged and a pipeline run has actually succeeded
  using it, is true.
