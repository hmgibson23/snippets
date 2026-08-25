---
name: parallel-plan-execution
description: Use when executing an implementation plan/todo list where most tasks are independent and speed matters — dispatches independent tasks in parallel across multiple subagents (not one at a time), then does exactly ONE comprehensive review of the whole diff, ONE remediation pass, and a bounded orchestrator-review-and-remediate loop. Prefer this over `subagent-driven-development` (which dispatches one subagent per task serially with a mandatory two-stage review after every single task — much slower once a plan has 3+ independent tasks). Triggers on "parallelize the todo list", "run tasks in parallel then review once", "single review at the end", or a plan whose tasks don't depend on each other.
---

# Parallel plan execution (wave-parallel implement → single review → remediate loop)

## Why this exists

`subagent-driven-development` (bundled with `pi-superpowers-plus`, and very
likely to auto-match a generic "implement this plan" request) dispatches
**one subagent per task, sequentially**, with a **mandatory two-stage review
after every single task**, and explicitly forbids parallel dispatch. That's
the right shape when tasks are tightly coupled or you want a
reviewed-and-approved checkpoint after every task — but for a plan whose
tasks are mostly independent, it means N tasks × (implement + spec-review +
quality-review), all one after another. That's hours, not minutes, for
anything beyond a couple of tasks.

This skill trades some of that per-task rigor for wall-clock speed: implement
independent tasks **in parallel**, then review **once**, then remediate
**once**, then loop only if needed — matching how `copilot-plan-implement-pipeline`
already treats review/remediation (whole-diff, single-pass), but adding
actual concurrency to the implementation stage itself.

## When to use this vs alternatives

- Plan has 3+ tasks that don't share files/state and don't depend on each
  other's output → **use this skill**.
- Tasks are tightly sequential/dependent, or you specifically want a
  reviewed-and-approved checkpoint after every single task → use
  `subagent-driven-development` instead.
- You're inside a project-specific pipeline already (`animoid-full-pipeline`)
  or the generic single-implementer pipeline (`copilot-plan-implement-pipeline`)
  → don't switch mid-run; those already do whole-plan-single-dispatch +
  single review. Reach for *this* skill instead of them only when you
  specifically want wall-clock parallelism across multiple subagent
  processes, not just "no per-task review" (which those two already give
  you serially).

## Animoid UI/testing overlay

When `animoid` is active, every implementation capsule must state its test
layer and enforce test-first evidence:

- pure geometry/controller/policy work → focused JVM RED/GREEN;
- Compose semantics, layout, bounds, navigation, visibility, or callbacks →
  Robolectric Compose UI RED/GREEN under `android/app/src/test`, mounting
  production composables/routes;
- irreducible Android/JNI/runtime behavior → focused `androidTest` source plus
  compile/APK proof in subagents; connected execution remains orchestrator-only
  and requires current explicit user authorization;
- native state/pixels/transactions → imported Zig RED/GREEN.

Do not assign the same shared host-test file to parallel lanes. If production
files are disjoint but all tests currently live in one large class, first
allocate separate new test files per lane or put those lanes in sequential
waves. One lane may own shared test-harness extraction; dependent lanes start
only after that commit is integrated.

Before dispatch, require each lane to name: exact owned files, forbidden shared
files, behavioral RED command, focused GREEN command, production wiring proof,
and instrumentation compile command when it adds `androidTest`. Subagents must
never run emulator/ADB/connected tests.

After integration, run the combined Robolectric/JVM host set before the single
whole-diff review. Review must reject pure-model-only or source-text-only proof
for visible UI requirements and reject new instrumentation coverage for
host-testable UI behavior.

## The process

1. **Read the plan once.** Extract every task with its full text. For each
   task, note which files it touches and which other tasks (if any) it
   depends on (needs their output/files before it can start).

2. **Group into dependency waves** — not one task at a time:
   - Wave 1 = every task with no unmet dependencies and no file overlap with
     another Wave 1 task.
   - Wave 2 = tasks that only depend on Wave 1 tasks, etc.
   - Two tasks that touch the same file/module go in *different* waves (or
     get merged into one task) — never dispatch them against each other in
     parallel, that's a guaranteed conflict.
   - If every task genuinely depends on the previous one, you get one wave
     per task and this degrades to serial — that's fine, parallelize
     whatever you actually can and serialize the rest.

3. **(Optional) pick a model once for the whole run.** If you don't want
   pi's default model for these tasks, use `subagent-dispatch`'s pattern
   once — `ask_model`, then a per-run dynamic agent file — instead of asking
   per task. Otherwise just dispatch against the existing generic
   `implementer` agent as-is.

4. **Dispatch each wave in parallel**, one `subagent` call per wave (not
   per task), using the tool's parallel mode:

   ```ts
   subagent({
     tasks: [
       { agent: "implementer", task: "<task 1 full text + exact file scope>" },
       { agent: "implementer", task: "<task 2 full text + exact file scope>" },
       { agent: "implementer", task: "<task 3 full text + exact file scope>" },
     ],
   })
   ```

   Each task capsule must be fully self-contained — a subagent has no memory
   of the plan, the conversation, or the other parallel tasks running beside
   it. Tell each one explicitly which files are *theirs* and which are out
   of scope, to avoid accidental overlap. For UI work, name the expected JVM or
   Robolectric Compose RED/GREEN tests and require production-composable
   mounting; mention focused instrumentation only for a justified irreducible
   runtime boundary.

   **No review dispatch after each task or each wave.** Implementers
   self-review and run their own tests internally (same as any other
   implementer agent), but you do not spin up a reviewer subagent per task —
   that's the whole point of this skill.

5. **Wait for the wave, check for conflicts, then move to the next wave.**
   After a wave returns:
   - Read every subagent's report in that wave.
   - `git status --short` / `git diff --stat` — confirm the files touched
     match what you scoped, and no two parallel tasks stepped on the same
     file.
   - Verify each UI lane supplied real Robolectric Compose RED/GREEN evidence
     (not source text or pure-model-only proof), and compile any newly added
     instrumentation source without executing connected tests.
   - If two agents did conflict, resolve it yourself before starting the
     next wave (pick the correct hunk, or re-dispatch one task with the
     other's result as context) — don't let a conflict ride into the next
     wave.
   - Then dispatch the next wave, if any remain.

6. **After ALL waves are done — exactly ONE comprehensive review**, covering
   the entire diff from every wave together, never per-task and never
   per-wave:

   ```bash
   git diff --stat BASE_SHA..HEAD
   ```

   Review it yourself inline (same discipline as
   `copilot-plan-implement-pipeline` Stage 4 — or dispatch a single reviewer
   subagent if you want a fresh perspective) against the whole plan: every
   task actually done, nothing extra, tests real and passing, no dead code
   or leftovers from a discarded parallel attempt, no conflicts between
   waves that slipped through.

7. **If issues found — exactly ONE remediation pass**, not one per finding
   and not one per task: a single subagent (or you directly, for small
   fixes) that fixes every listed issue in one dispatch, referencing the
   review's exact findings.

8. **Orchestrator review of the remediation** — you, inline, check the
   remediation diff against the review findings (same discipline as step 6,
   scoped to just the remediation changes).
   - Clean → done, report to the user.
   - Not clean → go back to step 7 with the remaining findings.

9. **Bounded loop.** Cap step 7↔8 at 3 remediation rounds. If still not
   clean after 3 rounds, stop and report to the user with exactly what's
   still wrong — don't loop indefinitely on a plan that keeps failing
   review.

## Rules

- Never dispatch two parallel tasks that touch the same file — that's not
  "independent"; split differently or serialize those two into different
  waves.
- Every parallel task capsule must be fully self-contained: the subagent
  cannot see the plan, the other parallel tasks, or your conversation.
- No per-task or per-wave review dispatch — review happens exactly once,
  after all implementation waves are complete, covering the whole diff.
- No per-finding remediation dispatch — one remediation pass fixes
  everything found in that review round.
- Cap the remediate↔review loop at 3 rounds before escalating to the user.
- Don't trust a subagent's self-reported "done" — verify with `git diff`
  and real test runs yourself, same as any other subagent dispatch.
