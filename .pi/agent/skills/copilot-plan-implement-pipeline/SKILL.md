---
name: copilot-plan-implement-pipeline
description: Use when the user wants a generic plan-implement-review-remediate loop, not tied to any one project (contrast with animoid-full-pipeline, which is Animoid-specific). Triggers on "run the copilot pipeline", "plan then implement with copilot", or a request to plan something, pick a model to implement it, then have it reviewed and remediated. The user picks the implementer model each round; the planner role is always whatever model is running this skill (no separate planner subagent).
---

# Copilot plan → implement → review → remediate pipeline

You (the lead agent, running right now, in whatever model you currently are)
are the **planner** for this entire pipeline. There is no separate planner
subagent — planning, reviewing, and drafting remediation all happen inline in
this conversation, by you. Only the **implementer** role is ever dispatched as
a subagent, and its model is chosen by the user, fresh, at the start of every
round (they may pick the same model again or a different one each time).

This pipeline is generic — it works on any repo, any language, any kind of
plan. Do not pull in Animoid-specific skills or assumptions unless the user's
plan is actually about the Animoid project (in which case load `animoid` /
`animoid-zig-nightly` yourself as you normally would).

If the plan's tasks are mostly independent and speed matters more than a
single serial implementer working through everything, use
`parallel-plan-execution` instead — it dispatches independent tasks across
multiple parallel subagents, still with exactly one whole-diff review and
one remediation pass per round like this pipeline, just with actual
concurrency in the implementation stage.

**All coding/TDD for the ENTIRE plan happens in Stage 3 before Stage 4's
review runs even once.** Stage 3 is a single subagent dispatch covering every
task in the plan — never split the plan into per-task dispatches that each
get their own implement → review → remediate cycle. Review (Stage 4) and any
resulting remediation plan (Stage 5) each cover the whole diff in one pass,
not one review/remediation per task or per finding. The only thing that
repeats per-task is the implementer subagent's own internal TDD loop
(RED → GREEN per task) *inside* its single Stage 3 dispatch — that is not a
separate round and does not involve you reviewing anything yet.

Do not use `dispatch_implementation` — use the `subagent` tool. See
`~/.pi/agent/skills/animoid-full-pipeline/SKILL.md` for background on why (the
`subagent` tool spawns a real headless `pi` process per call and passes its model directly in the `subagent` call).

## Stages (one "round" = stages 2-5; can repeat)

1. Plan (you, inline)
2. Ask the user which model implements this round
3. Dispatch the implementer subagent with that model
4. Review the diff yourself (you, inline) against the plan
5. If issues: draft a remediation plan (you, inline), report, and STOP — do
   not auto-loop. Only start another round if the user tells you to.

---

## Stage 1 — Plan (inline, no subagent)

If the user already handed you a concrete plan (file or pasted text), use it
verbatim — don't paraphrase it away. If they gave you a vague ask instead, do
normal planning yourself here (use `writing-plans` / `brainstorming` skills as
you normally would) until you have a concrete, task-by-task plan you're
confident in.

Save it to `docs/plans/<slug>.plan.md` (pick a short `slug` from the request,
e.g. `rate-limit-fix`). This file is the plan of record for every round below
— remediation plans reference it, they don't replace it.

Record the starting state:

```bash
git rev-parse HEAD
git status --short
```

Call this `BASE_SHA`.

## Stage 2 — Ask the user which model implements

Call:

```
ask_model(reason = "to pick the model that will implement this round")
```

Use the returned string as `IMPL_MODEL`.

## Stage 3 — Dispatch the implementer

One dispatch, the whole plan (or the whole remediation plan, on a
remediation round) — never one dispatch per task. Use the stable `implementer`
profile and pass `IMPL_MODEL` directly in the call.

Call:

```
subagent(
  agent = "implementer",
  model = IMPL_MODEL,
  task = "Implement this plan in full (read docs/plans/<slug>.plan.md yourself "
       + "for the complete task list; the summary below is for context only):\n\n"
       + "<the plan text, or the remediation plan text if this is a remediation round>"
)
```

On a **remediation round** (see Stage 5), the task text is the remediation
plan, not the original plan — the implementer should only fix the listed
findings, not redo everything.

## Stage 4 — Review the diff yourself

Do NOT accept the subagent's self-report. Independently check:

```bash
git status --short
git log --oneline -5
git diff --stat BASE_SHA..HEAD
```

Then actually review the diff against the plan (or remediation plan) line by
line, yourself, as the planner:
- Is every task/finding actually addressed?
- Any new function/type/field — does something non-test actually call it?
  (`grep`/`rg` and check.)
- Do added/changed tests assert real behaviour, not `expect(true)` or
  no-op-equivalent checks?
- Run the project's real test/build commands yourself and look at the output
  — don't take the implementer's word for "tests pass".
- Anything else clearly wrong, unsafe, or half-done.

If there's no real diff at all, or it doesn't match the plan's tasks, treat
this round as failed — don't move on to "approved"; either re-dispatch with
the gap called out explicitly, or stop and tell the user it didn't work.

## Stage 5 — Verdict: stop and report

**No issues found** → tell the user it's implemented and reviewed clean, cite
the plan file and the diff range. Stop. Pipeline complete for this request.

**Issues found** → write a concrete, actionable remediation plan yourself
(one item per issue, each naming the file/line and the exact required fix —
not a copy-paste of vague complaints) to
`docs/plans/<slug>.remediation.md` (or `.remediation-round<N>.md` if this
isn't the first remediation round — keep every round's file, don't overwrite
history).

Report to the user:
- What was implemented, by which model.
- What you found wrong, with specifics.
- The remediation plan you drafted.
- Ask explicitly: do they want another round? If yes, which model for the
  implementer this time (default: same as last round, but let them choose;
  Stage 2 runs again either way).

**Do not start another round yourself.** Wait for the user's explicit go-ahead
— this is by design (per the user), not a missing feature. If they say yes,
go back to Stage 2, using the remediation plan as this round's task and
`docs/plans/<slug>.remediation.md` (or the latest round's file) as the
reference.

## Rules for the whole pipeline

- All coding/TDD for the whole plan happens in Stage 3 before Stage 4's
  review runs even once; review and any remediation plan each cover the
  whole diff in a single pass, never per-task or per-finding. If you catch
  yourself about to review before Stage 3 has covered every task in the
  plan, stop — finish Stage 3 first.
- Planning, reviewing, and drafting remediation are always done by you
  inline — never spin up a "planner subagent" for this pipeline.
- Only ever one subagent role here: the implementer, single-mode
  (`agent="implementer"``, one `task` at a time). Never
  parallel/chain — rounds are strictly sequential, each depending on the real
  diff from the previous round.
- Re-ask `ask_model` at the start of every round, even remediation rounds —
  don't assume the same model without asking again.
- Keep every plan/remediation file from every round; they're the audit trail,
  since each implementer subagent call is a fresh process with no memory of
  prior rounds.
- Never trust a subagent's own "done" claim as a substitute for your own
  `git diff` + real test run.
