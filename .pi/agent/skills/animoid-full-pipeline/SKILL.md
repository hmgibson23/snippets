---
name: animoid-full-pipeline
description: Use when the user hands you an Animoid plan and wants the full pipeline run end to end: implement, review, remediate, and repeat review/remediation until approved or genuinely blocked. Triggers on "run the full pipeline", "pipeline this plan", "implement with luna then review with X", or an explicit request for implementation plus review/remediation. Every stage's model is user-chosen (named explicitly, or asked). Do not use for one one-off dispatch.
---

# Animoid full pipeline (implement → review ⇄ remediate until approved)

You (the lead agent) run this whole runbook yourself, in order, using the
`subagent` tool (single mode for an individual dispatch, parallel mode
`tasks: [...]` when a stage's remaining work is genuinely independent — see
`parallel-plan-execution` and the "Parallel dispatch for independent work"
section of `animoid/SKILL.md`), `ask_model`, `model-provider-routing` (to
resolve a bare tier word the user names, like "luna", to an exact
`provider/model` string), and plain shell checks. Do not skip stages. Do not
trust any subagent's "done" claim without checking real evidence yourself
(diff, tests, ledger) — dispatched models, especially fast/cheap ones, are
known to over-claim completion.

**No stage is pinned to a fixed model.** Every stage (implement, review,
remediate, final review) uses whatever model the user named for it, or asks
via `ask_model` if they didn't. This is deliberate: don't default to
deepseek for implementation just because that used to be the convention here
— always resolve the model per the "Choosing a model for a stage" section
below, every run.

**All feature coding/TDD for the ENTIRE plan happens before any review. Each
review produces one consolidated remediation round covering every finding,
followed by another whole-diff review. Repeat those review/remediation rounds
until approved or genuinely blocked — never run per-task review loops.** Stage
3 implements every task in the plan, start to finish, before Stage 4 ever runs.
That does not mean Stage 3 has to be a single serial subagent call: if the
plan's tasks are independent (different files/modules, no shared state, no
task depends on another's output), group them into dependency waves and
dispatch each wave in parallel (`subagent({ tasks: [...] })`, one call per
wave, same dynamically-generated implementer agent — same resolved model —
for every task in the batch) — do this by default whenever the plan allows
it, not only when asked. What you must never do is chop the plan into
per-task dispatches that each get their own implement → review → remediate
cycle — that means N separate review/remediation rounds instead of one,
which is slower and gives the reviewer less context (it can no longer see how
tasks interact). Concretely:

- Stage 3 must not return/be treated as done until every task in the plan is
  implemented — not just the first one or two — whether that took one
  dispatch or several parallel waves.
- Stage 5 runs one initial whole-plan review after all implementation waves.
- Stage 6 runs one consolidated remediation round for every finding from the
  current review before another review starts. Within that round, independent
  findings should still use parallel waves on the same remediation model.
- Stage 7 reviews the complete current diff. `NEEDS REMEDIATION` loops back to
  Stage 6 with every new finding; `APPROVED` exits. Never stop merely because
  one remediation round already ran.
- The only case where you repeat Stage 3 is if Stage 3's own verification
  (the `git diff --stat` check) shows tasks were genuinely skipped or
  missed — that's completing Stage 3, not starting a new per-task round.
- **Parallelism is _within_ a stage, never _across_ stages.** Stages
  themselves (3 → 3.5 → 4 → 5 → 6 → 6.5 → 7) stay strictly sequential —
  each depends on the previous stage's real, verified diff. Only the
  dispatches inside Stage 3 or Stage 6 may fan out into parallel waves.
- **A prior emulator-test failure (3.5/6.5), a completed remediation round,
  or being on a second/third loop of this pipeline does not by itself mean
  you should fall back to one-task-at-a-time serial dispatch for whatever
  independent work is still left.** Re-check independence fresh for the
  current stage's actual remaining tasks/findings every time, and
  parallelize again if they qualify — this is a common regression, watch for
  it explicitly.

Stages:

1. Take the plan
2. Load relevant Animoid skills yourself (to decide what extra context to hand each subagent)
3. Resolve the models for all stages (once), then implement with IMPLEMENT_MODEL
   3.4. Run the full host gate yourself, once (the orchestrator) — subagents in a parallel wave skip this
   3.5. Run emulator tests yourself (the orchestrator) — subagents never do this
4. Review model already resolved
5. Run that review as a dynamically-generated subagent; if issues found, write a remediation plan
6. Remediate with REMEDIATE_MODEL (already resolved)
   6.4. Run the full host gate yourself again, once (the orchestrator)
   6.5. Run emulator tests yourself again (the orchestrator)
7. Final review with FINAL_REVIEW_MODEL (already resolved), loop to Stage 6 until `APPROVED`

## Choosing a model for a stage

**Resolve models ONCE per run.** Do this at the start of Stage 3: ask the user
for the implement model AND, in the same breath, whether review/remediation/
final-review should use the same model or different ones. Then reuse those
answers for Stages 4, 6, and 7 without re-asking — do NOT call `ask_model` again
mid-run for a stage the user already answered. Re-resolve mid-run ONLY if:
the user names a new model for a later stage in conversation, or a dispatch
fails because a model is rejected/unavailable (then stop and ask per the Rules
section).

Resolution rules:

1. **The user already named a model/tier for this specific stage** in their
   instruction (e.g. "implement with luna", "have terra remediate", "final
   check with sol") → resolve it with `model-provider-routing` (handles bare
   tier words like "luna" that exist on multiple providers) to get an exact
   `provider/model` string. Don't guess a provider from an old agent file
   name — verify with `pi --list-models` per that skill.
2. **Not named** → ONE `ask_model(reason = "Animoid pipeline: pick the model per role — implement, review, remediate, final-review (or 'same for all')")` call at Stage 3 covering ALL roles. Do not call `ask_model` again for later stages.
3. **Thinking level**: if the resolved model is `openai-codex/gpt-5.6-luna`,
   use `max`; if `openai-codex/gpt-5.6-terra`, use `xhigh` (matching the
   blanket rule in `animoid/SKILL.md`). If the model doesn't take an
   explicit thinking level at all (e.g. `deepseek/deepseek-v4-flash`,
   `mimo/mimo-v2.5-pro`) or you're not sure it applies, **omit the
   `thinking:` frontmatter line entirely** rather than forcing a value onto
   a model that doesn't support it. For any other thinking-capable model the
   user picks, `high` is a reasonable default unless they say otherwise.
4. For Stage 7 specifically: if the user didn't express a preference and you
   asked up front at Stage 3, prefer suggesting one _different_ from whichever
   did Stage 3/Stage 6 — an independent final check is the point of that
   stage. Don't insist on it if the user explicitly wants the same model
   throughout.

## Dynamic agent templates (in this skill directory)

Every stage uses one of these three templates — fill `{MODEL}` (and
`{THINKING}`, or delete that whole frontmatter line per step 3 above) and
write the result to the matching path in `~/.pi/agent/agents/` before
dispatching. Each overwrites the previous run's version — that's fine, they're
per-run scratch, not history:

- `implementer-agent.template.md` → write to
  `~/.pi/agent/agents/animoid-implement-dynamic.md` (Stage 3)
- `remediator-agent.template.md` → write to
  `~/.pi/agent/agents/animoid-remediate-dynamic.md` (Stage 6)
- `review-agent.template.md` → write to
  `~/.pi/agent/agents/animoid-review-dynamic.md` (Stage 5 AND Stage 7 — same
  file, regenerated with that stage's resolved model each time since stages
  are sequential anyway)
- `planner-agent.template.md` → write to
  `~/.pi/agent/agents/animoid-plan-dynamic.md` (not a numbered stage of this
  pipeline — used for ad hoc "draft/update an Animoid plan document" asks,
  in or out of a full pipeline run)
- `investigator-agent.template.md` → write to
  `~/.pi/agent/agents/animoid-investigate-dynamic.md` (not a numbered stage —
  used for ad hoc read-only "explore/investigate this" asks)

**These five fixed filenames are the ONLY Animoid agent files that should
ever exist in `~/.pi/agent/agents/`.** Never invent a new one-off filename
for an Animoid dispatch (e.g. embedding the model name or task subject into
the filename like `openai-codex-luna-max-animoid-implementer.md` or
`animoid-luna-max-onion-plan-explorer.md`) — always overwrite one of these
five instead, picking the template by role (implement / remediate / review /
plan / investigate), not by model. This applies to one-off asks outside the
full pipeline too — see `animoid/SKILL.md`'s "One-off Animoid dispatches"
section. If you find any other `animoid-*` agent file in that directory that
isn't one of these five, it's stale debris from before this rule was
written — safe to delete.

---

## Stage 1 — Take the plan

Get the plan as full text (read the file if the user gave a path; otherwise
use the pasted plan text verbatim). Do not summarize or paraphrase it before
handing it to a subagent — the executor needs the real plan.

Pick a short `task_name` slug from the plan (e.g. `brush-smoothing-fix`). Use
it consistently for filenames below: `docs/plans/<slug>.*.md`.

## Stage 2 — Load relevant Animoid skills (yourself)

**Skip this stage if you already read `animoid/SKILL.md` and
`animoid-zig-nightly/SKILL.md` in full earlier in THIS session and they're still
in your context** (e.g. a prior pipeline run or an ad hoc Animoid dispatch) —
just re-verify no NEW `animoid-*` skill was added since:

```bash
ls ~/.pi/agent/skills/ | grep '^animoid'
```

Otherwise, read fully, into your own context, so you know what extra context to
mention in the Stage 3 task:

- `~/.pi/agent/skills/animoid/SKILL.md`
- `~/.pi/agent/skills/animoid-zig-nightly/SKILL.md`

Then check for other Animoid skills that may have been added since this pipeline
was written (same `ls | grep` above).`

For each `animoid-*` skill found (other than the two above and
`animoid-deepseek-guardrails`, which the Stage 3 dynamic implementer agent
already loads itself regardless of which model fills it), read its
`description` frontmatter. If the plan's subject matter matches (grep the
plan text for the skill's key nouns, e.g. "JNI", "brush", "persistence",
"timeline"), note its path — you'll tell the Stage 3 subagent to also read it.

## Stage 3 — Resolve models (once), then implement

Resolve ALL stage models now per "Choosing a model for a stage" above —
`IMPLEMENT_MODEL`, `REVIEW_MODEL`, `REMEDIATE_MODEL`, `FINAL_REVIEW_MODEL` in
one interaction. Then generate the dynamic implementer agent:

1. Read `~/.pi/agent/skills/animoid-full-pipeline/implementer-agent.template.md`.
2. Replace `{MODEL}` with `IMPLEMENT_MODEL`; fill or delete `{THINKING}` per
   step 3 above.
3. Write the result to `~/.pi/agent/agents/animoid-implement-dynamic.md`.

Group the plan's tasks into dependency waves first (see `parallel-plan-execution`):
tasks with no unmet dependencies and no file/module overlap with each other
go in the same wave. If the whole plan is one tightly-dependent chain, you'll
end up with one task per wave — that degrades to serial, which is fine; the
point is to actually check, not to assume serial by default.

Record `BASE_SHA` now (before any implementation):

```bash
git rev-parse HEAD
git status --short
```

**If Wave 1 has 2+ independent tasks**, dispatch them together in one
parallel call, all on `animoid-implement-dynamic`:

```
subagent({
  tasks: [
    { agent: "animoid-implement-dynamic", task: "PARALLEL WAVE - you are one of N tasks in this wave. Do NOT run the full zig build test --summary all / ReleaseFast / -Drelease=true trio - the orchestrator runs those once after the whole wave lands. Prove only your own scope with focused tests + rg wiring.\n\nAlso read these extra skill(s) first, if any: <list from Stage 2, or 'none'>.\n\nImplement this task from the plan (slug: <slug>), file scope: <exact files>:\n\n<task 1 full text>" },
    { agent: "animoid-implement-dynamic", task: "PARALLEL WAVE - you are one of N tasks in this wave. Do NOT run the full zig build test --summary all / ReleaseFast / -Drelease=true trio - the orchestrator runs those once after the whole wave lands. Prove only your own scope with focused tests + rg wiring.\n\nAlso read these extra skill(s) first, if any: <list from Stage 2, or 'none'>.\n\nImplement this task from the plan (slug: <slug>), file scope: <exact files>:\n\n<task 2 full text>" },
  ],
})
```

This explicit "PARALLEL WAVE" marker matters even though the dynamic
template/guardrails already try to infer parallel-vs-solo from file scope
alone — don't skip it just because the template has its own logic.

Each task capsule must state its exact file scope so parallel workers don't
step on each other. After a wave returns, check for conflicts before moving
on:

```bash
git status --short
git diff --stat
```

If two parallel tasks touched the same file, resolve it yourself (pick the
correct hunk, or re-dispatch one with the other's result as context) before
dispatching the next wave.

**If a wave has only one task, or the whole plan is one dependent chain**,
just dispatch that one task single-mode, same as before:

```
subagent(
  agent = "animoid-implement-dynamic",
  task = "Also read these extra skill(s) before starting, if any: <list from Stage 2, or 'none'>.\n\n"
       + "Implement this Animoid plan in full (slug: <slug>):\n\n<the actual plan text from Stage 1>"
)
```

Repeat wave-by-wave until every task in the plan has been dispatched. When
the last wave returns, do NOT accept any subagent's summary at face value.
Independently verify the whole plan's diff together:

```bash
git status --short
git log --oneline -5
git diff --stat <BASE_SHA>..HEAD
```

If there's no ledger file, no real diff, or the diff doesn't match the plan's
tasks, treat this stage as failed — do not proceed to Stage 4. Either
re-invoke `animoid-implement-dynamic` with the missing tasks called out
explicitly, or hand it back to the user.

Record the current commit/working-tree state as `IMPL_SHA` (or just use the
working tree if uncommitted — `git diff <BASE_SHA>` still works either way).

## Stage 3.4 — Run the closing host gate yourself, once (orchestrator only)

This is the expensive full-repo gate — run it exactly ONCE here, yourself,
never inside a Stage 3 subagent dispatch (parallel-wave tasks are told to
skip it; this is where it actually happens):

```bash
/opt/homebrew/bin/zig fmt <all changed files from git diff --stat BASE_SHA..HEAD>
/opt/homebrew/bin/zig build test --summary all
/opt/homebrew/bin/zig test src/run_tests.zig -O ReleaseFast
/opt/homebrew/bin/zig build -Drelease=true
```

Save the raw output (e.g. `docs/plans/<slug>.host-gate-1.log`) — you'll paste
a summary into the Stage 5 review task instead of asking the reviewer to
re-run the full suite itself. If any of these fail because of the new
implementation, treat Stage 3 as incomplete: send the failure back to
`animoid-implement-dynamic` before moving on to Stage 3.5/4.

## Stage 3.5 — Run emulator tests yourself (orchestrator only, gated)

**Check first whether this stage is even needed.** Per `animoid/SKILL.md`'s
test-pyramid contract, emulator/connected tests are only required for one
narrow category: Activity-root MotionEvents, system keys/window/insets,
SurfaceView/Vulkan PixelCopy, or lifecycle/process restoration. Look at the
Stage 3 diff (`git diff --stat <BASE_SHA>..HEAD`, plus the actual changed
files) — if none of the changed production code touches that category (e.g.
it's pure Zig logic, a pure controller/policy change, or Compose UI already
covered by Robolectric), **skip this stage entirely**, note in your own
notes that it was skipped and why, and go straight to Stage 4. Don't spend
a multi-minute `connectedAndroidTest` run on a diff that can't affect it.

If the diff DOES touch that category, run it yourself, in this session, not
a subagent's:

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
`animoid-implement-dynamic` with the specific failure output, or hand it
to the user, before moving on.

## Stage 4 — Review model (already resolved)

Use `REVIEW_MODEL` from Stage 3's resolution — do not re-ask.

## Stage 5 — Run the review

Generate the dynamic reviewer agent:

1. Read `~/.pi/agent/skills/animoid-full-pipeline/review-agent.template.md`.
2. Replace `{MODEL}` with `REVIEW_MODEL`; fill or delete `{THINKING}` per the
   "Choosing a model for a stage" rules.
3. Write the result to `~/.pi/agent/agents/animoid-review-dynamic.md` (this
   overwrites any previous run's version — that's fine, it's per-run scratch).

Call:

```
subagent(
  agent = "animoid-review-dynamic",
  task = "Diff range: <BASE_SHA>..<IMPL_SHA-or-HEAD>. "
       + "Requirement to check against:\n\n<the original plan text from Stage 1>\n\n"
       + "Full host-gate output from this session (Stage 3.4 - zig build test / ReleaseFast / release build): <paste your own summary/output here; treat as authoritative, don't ask the reviewer to re-run the full suite>.\n\n"
       + "Emulator test summary from this session (Stage 3.5): <pass/fail summary "
       + "and any failure output, or 'not applicable' if this plan has no Android/emulator surface>.\n\n"
       + "Write findings to docs/plans/<slug>.review.md"
)
```

Read the resulting `docs/plans/<slug>.review.md` yourself.

- **No critical/important findings** (`VERDICT: APPROVED`) → stages 6/7 are
  not needed. Tell the user the plan was implemented and reviewed clean, cite
  the review file, and stop here. Minor findings in the review's "Non-blocking
  notes" list do NOT block approval and must NOT trigger a remediation round —
  relay them to the user as notes only.
- **Critical/important findings exist** (`VERDICT: NEEDS REMEDIATION`) →
  continue to remediation. Turn ONLY those findings into a concrete
  task-by-task remediation plan (one task per finding, each with the file/line
  and the exact required fix — do not just paste the review verbatim, make it
  actionable; exclude minor/non-blocking rows). Save it to
  `docs/plans/<slug>.remediation.md`.

## Stage 6 — Remediate (model already resolved)

Use `REMEDIATE_MODEL` from Stage 3's resolution — do not re-ask. Generate the dynamic remediator agent:

1. Read `~/.pi/agent/skills/animoid-full-pipeline/remediator-agent.template.md`.
2. Replace `{MODEL}` with `REMEDIATE_MODEL`; fill or delete `{THINKING}`.
3. Write the result to `~/.pi/agent/agents/animoid-remediate-dynamic.md`.

Record `PRE_REMEDIATE_SHA` (= your `IMPL_SHA`/current HEAD from Stage 5).

Call:

```
subagent(
  agent = "animoid-remediate-dynamic",
  task = "Prior review file: docs/plans/<slug>.review.md (read it first). "
       + "Remediation plan to execute:\n\n<contents of docs/plans/<slug>.remediation.md>"
)
```

Same rule as Stage 3: verify with `git status --short`, `git log --oneline`,
`git diff --stat PRE_REMEDIATE_SHA..HEAD` yourself before trusting
completion, and use parallel waves for independent findings the same way as
Stage 3 (all on `animoid-remediate-dynamic`, same resolved model, and the
same "PARALLEL WAVE" marker in each task string). Record the
new commit as `REMEDIATE_SHA`.

## Stage 6.4 — Run the closing host gate yourself, once (orchestrator only)

Same as Stage 3.4 — run the full trio yourself, once, here, never inside a
Stage 6 subagent dispatch:

```bash
/opt/homebrew/bin/zig fmt <all changed files from git diff --stat PRE_REMEDIATE_SHA..HEAD>
/opt/homebrew/bin/zig build test --summary all
/opt/homebrew/bin/zig test src/run_tests.zig -O ReleaseFast
/opt/homebrew/bin/zig build -Drelease=true
```

Save output (e.g. `docs/plans/<slug>.host-gate-2.log`) to hand a summary to
Stage 7 instead of having the final reviewer re-run the full suite.

## Stage 6.5 — Run emulator tests yourself again (orchestrator only, gated)

Same gating logic as Stage 3.5 — only re-run the emulator if the
remediation diff (`git diff --stat PRE_REMEDIATE_SHA..HEAD`) actually touches
the emulator-required category AND either Stage 3.5 was skipped or the
remediation changed Android/native runtime behavior in that category. If
Stage 3.5 already ran clean and the remediation only touched non-runtime
code (e.g. a pure logic fix, a test assertion, a Zig-only change), skip this
stage too — note why, and go straight to Stage 7.

If it's actually needed, run it yourself, before the final review:

```bash
./gradlew connectedAndroidTest
```

Save/keep the output the same way as Stage 3.5
(`docs/plans/<slug>.emulator-2.log`) — you'll pass a summary to Stage 7.

## Stage 7 — Final review (model already resolved)

Use `FINAL_REVIEW_MODEL` from Stage 3's resolution — do not re-ask. Regenerate the dynamic reviewer agent (same template), overwriting `~/.pi/agent/agents/animoid-review-dynamic.md`.

Call:

```
subagent(
  agent = "animoid-review-dynamic",
  task = "Diff range: <PRE_REMEDIATE_SHA>..<REMEDIATE_SHA-or-HEAD>. "
       + "This diff is supposed to satisfy the remediation plan below (not the "
       + "whole original feature - judge whether the remediation was actually "
       + "done):\n\n<contents of docs/plans/<slug>.remediation.md>\n\n"
       + "Full host-gate output from this session (Stage 6.4 - zig build test / ReleaseFast / release build): <paste your own summary/output here; treat as authoritative>.\n\n"
       + "Emulator test summary from this session (Stage 6.5): <pass/fail summary "
       + "and any failure output, or 'not applicable'>.\n\n"
       + "Write findings to docs/plans/<slug>.final-review.md"
)
```

Read the review yourself.

- `VERDICT: APPROVED` → exit loop and report completion.
- `VERDICT: NEEDS REMEDIATION` → verify each finding, write one actionable
  remediation plan covering every critical/important row, increment the round,
  then repeat Stages 6, 6.4, gated 6.5, and 7. Keep going without asking for
  routine authorization.
- Stop only for a genuine blocker requiring user authority, unavailable
  credentials/artifacts, model failure with no authorized substitute, or an
  unresolved architecture conflict. Passing tests do not override review
  findings.

Preserve every round's audit trail. First-round legacy names may remain
`<slug>.remediation.md` and `<slug>.final-review.md`; later rounds must use
numbered files such as `<slug>.remediation.round-2.md` and
`<slug>.final-review.round-2.md` instead of overwriting prior reports.

## Rules for the whole pipeline

- All planned feature coding/TDD happens before initial review. After that,
  run one whole-diff review and one consolidated remediation pass per round,
  never per-task or per-finding review loops. If review still finds important
  issues, start another consolidated remediation round and continue until
  `APPROVED` or genuinely blocked.
- Subagents never run the Android emulator/connected tests — that's always
  your job (Stages 3.5 and 6.5). The dynamic implementer/remediator templates
  enforce this on their own side too, but don't rely on that alone: don't ask
  a subagent to "run the emulator tests" in a task string.
- Subagents in a parallel wave never run the full `zig build test --summary
all` / `-O ReleaseFast` / `-Drelease=true` trio either — that's always your
  job too, exactly once per stage (Stages 3.4 and 6.4), after the whole wave
  lands. Running it per parallel task multiplies a full host build/test/
  release cycle by however many parallel tasks you dispatched, for zero extra
  benefit — this is a known, previously-diagnosed cause of the pipeline being
  much slower than it should be on parallel waves. Always include the
  "PARALLEL WAVE" marker in each task string of a parallel dispatch so the
  worker knows to skip its own full-gate run.
- Never let a subagent's self-report substitute for your own `git`/test check
  between stages — the `subagent` tool runs a real headless `pi` process per
  call, so its final text output is a summary, not proof; always verify with
  `bash` yourself.
- Models are resolved ONCE per run at Stage 3 per "Choosing a model for a
  stage" — never silently substitute a different model mid-stage. If a
  subagent invocation fails because a model is rejected or unavailable, stop
  and ask the user rather than guessing a replacement.
- Keep all intermediate artifacts (`docs/plans/<slug>.review.md`, every
  numbered remediation plan, and every numbered final review). Never overwrite
  or delete prior rounds; they are the audit trail and each subagent call has
  fresh context.
- Stage-to-stage calls (4, 5, 6.5, 7, and the single-task case of 3/6) are
  single-mode (`agent` + `task`). This does NOT ban `subagent({ tasks: [...] })`
  parallel dispatch _inside_ Stage 3 or Stage 6 for independent waves/findings
  — that parallel fan-out is required (see the top of this file and
  "Parallel dispatch" in `animoid/SKILL.md`), not an exception to it. What's
  banned is parallelizing _across_ stages (e.g. starting Stage 5 review before
  every Stage 3 wave has returned) — stages themselves stay strictly
  sequential and each depends on the previous stage's real diff.
- If the user only wants part of this (e.g. "just implement + review, skip
  remediation/final review"), follow their explicit instruction and stop
  after the requested stage.
