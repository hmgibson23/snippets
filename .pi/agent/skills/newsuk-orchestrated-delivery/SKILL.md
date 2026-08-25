---
name: newsuk-orchestrated-delivery
description: The News UK orchestrate → implement → review → verify → ship loop we run on newsuk repos (nuk-deadline, nuk-wp-slack-app, infra/Terraform + Python Lambdas). YOU are the orchestrator (plan inline, verify independently, commit/PR/deploy). A subagent implements (default luna max = github-copilot/gpt-5.6-luna, xhigh). A separate subagent independently reviews (default terra high = github-copilot/gpt-5.6-terra, high). ALL subagent roles use github-copilot provider models ONLY for News UK work — never route a News UK role to another provider. Model tiers may be swapped per the user, but the provider stays github-copilot. Use when the user says "run the newsuk pipeline", "orchestrate this / implement with luna and review with terra", or asks to plan a change, have luna build it, terra review it, then verify + PR/deploy. Enforces the hard lesson that a merged/deployed fix can be necessary-but-insufficient, so the orchestrator ALWAYS verifies at the data/runtime layer, not just that code compiles.
---

# News UK orchestrated delivery loop

You (the model running right now) are the **orchestrator**. You do NOT hand the
whole job to one agent and trust the summary. You:

1. **Plan** the change inline (investigate + root-cause + write a concrete spec).
2. **Dispatch the implementer** subagent to build it.
3. **Dispatch an independent reviewer** subagent to review the whole diff.
4. **Remediate** (feed findings back) until the review is clean.
5. **Verify it yourself** — run the gates AND check real runtime/data behaviour.
6. **Ship** — commit (clean), push, PR, and deploy only if authorized.

Only the implementer and reviewer are subagents. Planning, remediation triage,
and final verification are you, inline.

## Roles & models (defaults — user may swap the TIER, never the provider)

**Hard rule for News UK work: every subagent role uses the `github-copilot`
provider ONLY.** Do not route a News UK implement/review role to `openai-codex`,
Anthropic direct, or any other provider, even if the same model id exists there.

| Role | Who | Default model | Agent file |
| --- | --- | --- | --- |
| Orchestrator | you (this session) | whatever you are | — |
| Implementer | subagent | `github-copilot/gpt-5.6-luna` (xhigh) = **luna max** | `copilot-implementer-dynamic` |
| Reviewer | subagent | `github-copilot/gpt-5.6-terra` (high) = **terra high** | `terra-reviewer-dynamic` |

Copilot tiers available for swapping (all `github-copilot/…`): `gpt-5.6-luna`,
`gpt-5.6-sol`, `gpt-5.6-terra`, `gpt-5.5`, `gpt-5.4`, `gpt-5.3-codex`,
`claude-opus-4.8`, `claude-sonnet-5`, etc. If the user asks to swap a role's
model, keep it on `github-copilot/…`; if they name a bare tier word, resolve it
to the copilot variant (read `model-provider-routing` only to confirm the id,
not to pick a non-copilot provider).

Dispatch with the `subagent` tool (it spawns a real headless `pi` and pins the
model from the agent file's `model:` frontmatter — there is no per-call model
override). To change a role's tier, edit that agent file's `model:` line (or
generate a per-run agent file per `subagent-dispatch`), always keeping the
`github-copilot/` prefix.

## Stage 1 — Plan (you decide; DELEGATE the investigation)

**Do not do heavy codebase spelunking or data diagnostics inline.** Delegate the
investigation/diagnostic to a subagent (the implementer agent is fine, run
read-only), exactly like a normal implement dispatch — pointing it at the real
code and, where useful, the real data (DDB/metrics/logs). You (the orchestrator)
OWN the decision: you read the subagent's findings, decide the root cause, and
write the task-by-task spec. But the grep/read/reproduce work itself is a
subagent job, not something you grind through in the main session.

- If the user handed you a concrete spec, use it verbatim — no investigation
  needed. Otherwise dispatch a **read-only diagnostic** subagent: "find X, dump
  the real values, reproduce the failure, propose the mechanism" (no repo edits,
  no commit, scratch code in /tmp only). This is the "a" in the user's usual
  "a then b" — diagnose via subagent, then implement via subagent.
- Then, inline, turn its findings into a task-by-task spec with exact file paths
  and line ranges, acceptance criteria, and the evidence each task must produce.
- Note the newsuk specifics the implementer will need (see "House rules").

## Stage 2 — Implement (dispatch the implementer subagent)

Give the implementer a self-contained task containing:
- The branch to work on (`git checkout <branch>` first; keep prior commits).
- The exact spec (files, lines, acceptance criteria).
- **Canonical source paths** — edit the real source, never a generated build
  copy (e.g. edit `src/app/app.py`, NOT `infra/modules/app/build/app_src/app.py`,
  which is git-ignored and rebuilt).
- The gate commands to run and paste output for: tests
  (`python3 -m pytest tests/ -q`), `terraform fmt -check -recursive` +
  `terraform validate`, `py_compile`.
- **No-secrets rules**: never touch `secrets.txt`, `.env*`, `tfplan*`, or
  `scripts/` dumps; scan the diff for secrets.
- **Do NOT commit** — leave changes in the working tree for review + your commit.
- Evidence requirement: paste real command output; prove every new symbol is
  wired into a production path; never weaken/skip a test to get green.

## Stage 3 — Review (dispatch the reviewer subagent, independent)

Dispatch `terra-reviewer-dynamic` (read-only) on the same branch/diff. It must
check, in priority order: does it fix the **root cause** or just a symptom;
production wiring of every new symbol/resource; correctness + blast radius
(infra: in-place vs replace, stateful destroys, IAM/KMS, hot-partition risk);
test quality (mocks hiding the bug? weakened tests? regression guard?); secret
hygiene. It returns P0/P1/P2 findings with `file:line` and an
APPROVE / REQUEST CHANGES verdict. The reviewer does not edit prod/test code.

## Stage 4 — Remediate (loop until clean)

If the reviewer returns P0/P1: triage inline, then either re-dispatch the
implementer with the specific findings, or fix trivial items inline. Re-review
the whole diff. Repeat until zero P0/P1. Do not auto-loop forever — if it
stalls or the findings are contested, stop and bring it to the user.

## Stage 5 — Verify (you, inline — THE NON-NEGOTIABLE STEP)

Do not trust the implementer's or reviewer's summaries. Independently:
- Re-run the gates yourself: `pytest`, `terraform validate`, secret scan of the
  full diff, confirm only intended files changed (no build copy, `secrets.txt`,
  `tfplan`, or `scripts/`).
- For infra/runtime changes, **verify the real behaviour, not just that it
  compiles**. This is the hard-won rule of this pipeline: a change can pass
  tests, merge, and deploy and STILL leave the real problem in place. Prove the
  fix works against reality — e.g. invoke the Lambda and confirm it returns
  cleanly (not `Runtime.OutOfMemory`), read the live ESM/config, query the
  actual table/metric, confirm the user-visible symptom is gone. If you claimed
  "done" without checking runtime once in this session, that is the mistake this
  stage exists to prevent.

## Stage 6 — Ship

- Commit ONLY the intended files (`git add <files>` explicitly; never `git add -A`).
  Re-scan the diff for secrets first.
- Push the branch; create/update the PR with a description that states the root
  cause, the fix, the verification evidence, and any follow-ups.
- **Deploy only if the user authorized it.** Follow the Terraform safety gate
  (plan → confirm 0 destroy of stateful resources, ESMs/tables update in-place
  not replace → apply). After apply, verify live (Stage 5 again against prod).
- Record durable findings/decisions in memory.

## House rules (News UK specifics)

- **AWS creds**: subagents do NOT inherit your shell env, and creds live in a
  profile FILE (saml2aws). Every subagent that touches AWS must
  `export AWS_PROFILE=<profile>` (e.g. `nuk-digital-dev-architecture`),
  `AWS_REGION=eu-west-1`, `AWS_DEFAULT_REGION=eu-west-1`, `AWS_PAGER=""`, and
  verify `aws sts get-caller-identity` (expect the dev account) before acting.
  If creds are expired, STOP and ask the user to re-auth
  (`saml2aws login --profile <profile>`).
- **Terraform**: state is remote (S3). Build uses `uv` (triggered by the app
  Lambda `null_resource` on code hash change). Always `plan` before `apply`;
  a non-zero "to destroy" that is only a benign `null_resource` rebuild is OK,
  but any destroy/replace of a DynamoDB table, S3 bucket, OpenSearch, secret,
  or an event-source-mapping being REPLACED (not updated in place) is a STOP.
- **Canonical vs build copies**: `src/**` is authored; `infra/modules/*/build/**`
  is generated/git-ignored — never edit or commit it.
- **Secret hygiene**: `secrets.txt`, `.env*`, `tfplan*` are git-ignored for a
  reason. Never `git add -f` them. Scan every diff before committing.
- **"Check its work"**: independent verification (Stages 3 and 5) is the point
  of this pipeline. The recurring failure mode is declaring victory on a fix
  that was necessary but not sufficient. Verify at the layer where the user
  actually sees the problem.
