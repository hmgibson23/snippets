---
name: animoid-full-pipeline
description: Use when the user hands you an Animoid plan and wants the full pipeline run - end to end, not just one dispatch. Triggers on "run the full pipeline", "pipeline this plan", "deepseek then terra then sol", or an explicit request to implement an Animoid plan with deepseek-v4-flash, get it reviewed, remediate with gpt-5.6-terra, and get a final sol review. Do not use for a single one-off dispatch to one model.
---

# Animoid full pipeline (deepseek → user-chosen review → terra → sol)

You (the lead agent) run this whole runbook yourself, in order, using the
`subagent` tool (single mode, one agent + task at a time), `ask_model`, and
plain shell checks. Do not use `dispatch_implementation` for this pipeline —
it does not work in this environment. Do not skip stages. Do not trust any
subagent's "done" claim without checking real evidence yourself (diff, tests,
ledger) — dispatched models, especially fast ones, are known to over-claim
completion.

**All coding/TDD for the ENTIRE plan happens before any review, and all
remediation happens in one pass after ONE review — never per-task.** Stage 3
is a single subagent dispatch that implements every task in the plan, start
to finish. Do not chop the plan into per-task dispatches that each get their
own implement → review → remediate cycle — that means N separate review/
remediation rounds instead of one, which is slower and gives the reviewer
less context (it can no longer see how tasks interact). Concretely:

- Stage 3 gets the WHOLE plan text and must not return/be treated as done
  until every task in it is implemented — not just the first one or two.
- Stage 5 (review) runs exactly once, against the diff for the whole plan.
- Stage 6 (remediation) runs exactly once, fixing every finding from that
  one review in a single dispatch — not one dispatch per finding, and not a
  detour back to Stage 3 for an individual task.
- The only case where you repeat Stage 3 is if Stage 3's own verification
  (the `git diff --stat` check) shows tasks were genuinely skipped or
  missed — that's completing Stage 3, not starting a new per-task round.

Stages:

1. Take the plan
2. Load relevant Animoid skills yourself (to decide what extra context to hand each subagent)
3. Implement with the `animoid-deepseek-implementer` subagent (deepseek-v4-flash)
3.5. Run emulator tests yourself (the orchestrator) — subagents never do this
4. Ask the user what model to use for review
5. Run that review as a dynamically-generated subagent; if issues found, write a remediation plan
6. Remediate with the `animoid-terra-remediator` subagent (gpt-5.6-terra)
6.5. Run emulator tests yourself again (the orchestrator)
7. Final review with the `animoid-sol-reviewer` subagent (gpt-5.6-sol)

Pre-built agent files (in `~/.pi/agent/agents/`, each with model pinned in its
frontmatter so `subagent` spawns the right model automatically):
- `animoid-deepseek-implementer` — model `deepseek/deepseek-v4-flash`
- `animoid-terra-remediator` — model `openai-codex/gpt-5.6-terra`
- `animoid-sol-reviewer` — model `openai-codex/gpt-5.6-sol`

Template for the one stage with a model chosen at runtime:
- `review-agent.template.md` (in this skill directory) — has a `{MODEL}`
  placeholder; you fill it and write it out as a real agent file before
  calling `subagent` (see Stage 5).

---

## Stage 1 — Take the plan

Get the plan as full text (read the file if the user gave a path; otherwise
use the pasted plan text verbatim). Do not summarize or paraphrase it before
handing it to a subagent — the executor needs the real plan.

Pick a short `task_name` slug from the plan (e.g. `brush-smoothing-fix`). Use
it consistently for filenames below: `docs/plans/<slug>.*.md`.

## Stage 2 — Load relevant Animoid skills (yourself)

Read fully, into your own context, so you know what extra context to mention
in the Stage 3 task:
- `~/.pi/agent/skills/animoid/SKILL.md`
- `~/.pi/agent/skills/animoid-zig-nightly/SKILL.md`

Then check for other Animoid skills that may have been added since this
pipeline was written:

```bash
ls ~/.pi/agent/skills/ | grep '^animoid'
```

For each `animoid-*` skill found (other than the two above and
`animoid-deepseek-guardrails`, which `animoid-deepseek-implementer` already
loads itself), read its `description` frontmatter. If the plan's subject
matter matches (grep the plan text for the skill's key nouns, e.g. "JNI",
"brush", "persistence", "timeline"), note its path — you'll tell the Stage 3
subagent to also read it.

## Stage 3 — Implement with deepseek-v4-flash

One dispatch, the whole plan — not one dispatch per task. Record `BASE_SHA`
now (before any implementation):

```bash
git rev-parse HEAD
git status --short
```

Call:

```
subagent(
  agent = "animoid-deepseek-implementer",
  task = "Also read these extra skill(s) before starting, if any: <list from Stage 2, or 'none'>.\n\n"
       + "Implement this Animoid plan in full (slug: <slug>):\n\n<the actual plan text from Stage 1>"
)
```

When it returns, do NOT accept the summary at face value. Independently verify:

```bash
git status --short
git log --oneline -5
git diff --stat <BASE_SHA>..HEAD
```

If there's no ledger file, no real diff, or the diff doesn't match the plan's
tasks, treat this stage as failed — do not proceed to Stage 4. Either
re-invoke the subagent with the missing tasks called out explicitly, or hand
it back to the user.

Record the current commit/working-tree state as `IMPL_SHA` (or just use the
working tree if uncommitted — `git diff <BASE_SHA>` still works either way).

## Stage 3.5 — Run emulator tests yourself (orchestrator only)

Subagents never run the emulator (see `animoid/SKILL.md` — "Who runs
emulator tests"). This is your job now, in this session, not a subagent's:

```bash
# whatever the project's real emulator/connected-test command is, e.g.
./gradlew connectedAndroidTest
```

Keep the raw output (or save it to `docs/plans/<slug>.emulator-1.log`) —
you'll hand a summary of it to the Stage 5/6 reviewer so it can distinguish
in-scope failures from known unrelated flakiness, instead of re-running the
emulator itself.

If emulator tests fail because of the new implementation (not pre-existing
flakiness), treat Stage 3 as incomplete: send the failure back to
`animoid-deepseek-implementer` with the specific failure output, or hand it
to the user, before moving on.

## Stage 4 — Ask the user what model to use for review

Call:

```
ask_model(reason = "to review deepseek's Animoid implementation before deciding on remediation")
```

Use the returned model string as `REVIEW_MODEL` for Stage 5.

## Stage 5 — Run the review

Generate the dynamic reviewer agent:

1. Read `~/.pi/agent/skills/animoid-full-pipeline/review-agent.template.md`.
2. Replace `{MODEL}` with `REVIEW_MODEL`.
3. Write the result to `~/.pi/agent/agents/animoid-review-dynamic.md` (this
   overwrites any previous run's version — that's fine, it's per-run scratch).

Call:

```
subagent(
  agent = "animoid-review-dynamic",
  task = "Diff range: <BASE_SHA>..<IMPL_SHA-or-HEAD>. "
       + "Requirement to check against:\n\n<the original plan text from Stage 1>\n\n"
       + "Emulator test summary from this session (Stage 3.5): <pass/fail summary "
       + "and any failure output, or 'not applicable' if this plan has no Android/emulator surface>.\n\n"
       + "Write findings to docs/plans/<slug>.review.md"
)
```

Read the resulting `docs/plans/<slug>.review.md` yourself.

- **No critical/important findings** (`VERDICT: APPROVED`) → stages 6/7 are
  not needed. Tell the user the plan was implemented and reviewed clean, cite
  the review file, and stop here.
- **Critical/important findings exist** (`VERDICT: NEEDS REMEDIATION`) →
  continue to remediation. Turn the review's findings into a concrete
  task-by-task remediation plan (one task per finding, each with the file/line
  and the exact required fix — do not just paste the review verbatim, make it
  actionable). Save it to `docs/plans/<slug>.remediation.md`.

## Stage 6 — Remediate with gpt-5.6-terra

Record `PRE_TERRA_SHA` (= your `IMPL_SHA`/current HEAD from Stage 5).

Call:

```
subagent(
  agent = "animoid-terra-remediator",
  task = "Prior review file: docs/plans/<slug>.review.md (read it first). "
       + "Remediation plan to execute:\n\n<contents of docs/plans/<slug>.remediation.md>"
)
```

Same rule as Stage 3: verify with `git status --short`, `git log --oneline`,
`git diff --stat PRE_TERRA_SHA..HEAD` yourself before trusting completion.
Record the new commit as `TERRA_SHA`.

## Stage 6.5 — Run emulator tests yourself again (orchestrator only)

Same reasoning as Stage 3.5 — the `animoid-terra-remediator` subagent never
runs the emulator. Re-run it yourself now, before the final review:

```bash
./gradlew connectedAndroidTest
```

Save/keep the output the same way as Stage 3.5
(`docs/plans/<slug>.emulator-2.log`) — you'll pass a summary to Stage 7.

## Stage 7 — Final review with sol

Call:

```
subagent(
  agent = "animoid-sol-reviewer",
  task = "Diff range: <PRE_TERRA_SHA>..<TERRA_SHA-or-HEAD>. "
       + "This diff is supposed to satisfy the remediation plan below (not the "
       + "whole original feature - judge whether the remediation was actually "
       + "done):\n\n<contents of docs/plans/<slug>.remediation.md>\n\n"
       + "Emulator test summary from this session (Stage 6.5): <pass/fail summary "
       + "and any failure output, or 'not applicable'>.\n\n"
       + "Write findings to docs/plans/<slug>.sol-review.md"
)
```

Read `docs/plans/<slug>.sol-review.md` yourself and report the final verdict
to the user (approved, or list remaining issues). Do not loop the pipeline
again automatically if sol finds new issues — stop and hand control back to
the user with sol's findings; let them decide whether to run another
remediation round.

## Rules for the whole pipeline

- All coding/TDD for the whole plan happens before any review; review and
  remediation each run exactly ONCE for the whole diff, never per-task or
  per-finding (see the note at the top of this skill). If you catch yourself
  about to dispatch a second implementer call for "just this one task" before
  Stage 5's review has even run, stop — that task belongs inside the single
  Stage 3 dispatch, not a separate round.
- Subagents never run the Android emulator/connected tests — that's always
  your job (Stages 3.5 and 6.5). Every `animoid-*-implementer`/`*-remediator`
  agent file enforces this on its own side too, but don't rely on that alone:
  don't ask a subagent to "run the emulator tests" in a task string.
- Never let a subagent's self-report substitute for your own `git`/test check
  between stages — the `subagent` tool runs a real headless `pi` process per
  call, so its final text output is a summary, not proof; always verify with
  `bash` yourself.
- Never silently change model strings. The models are pinned in each agent
  file's frontmatter (`deepseek/deepseek-v4-flash`, `openai-codex/gpt-5.6-terra`,
  `openai-codex/gpt-5.6-sol`). If a subagent invocation fails because a model
  is rejected or unavailable, stop and ask the user rather than editing the
  agent file to substitute a different model.
- Keep all intermediate artifacts (`docs/plans/<slug>.review.md`,
  `.remediation.md`, `.sol-review.md`) — do not delete them; they're the audit
  trail across stages, and each subagent call is a fresh process with no
  memory of prior stages.
- `subagent` calls in this pipeline are always single-mode (`agent` + `task`),
  never `parallel`/`chain` — stages are strictly sequential and each depends
  on the previous stage's real diff.
- If the user only wants part of this (e.g. "just do deepseek + review, skip
  terra/sol"), follow their explicit instruction and stop after the requested
  stage.
