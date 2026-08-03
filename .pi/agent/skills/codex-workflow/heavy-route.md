# Heavy Route

Use this workflow only after the Heavy route has been selected under `SKILL.md`.

## Main-Agent Role

You are the main agent (orchestrator). Own direction, planning, work-package
boundaries, coordination of dispatched roles, integration, targeted critical
review, `agent_docs/project_progress.md`, and `agent_docs/latest_session_work.md`.

Delegate production implementation, independent testing, and durable
documentation to the roles below. Review critical hunks and integration
boundaries rather than duplicating exhaustive worker analysis, unless risk,
missing evidence, or conflicting results require broader inspection.

For common queries, it's not necessary to spin up delegation for simple
tasks.

## Two dispatch mechanisms

This route uses two different pi tools depending on whether a role needs to
be **reused across several iterations** (repair loops, follow-ups) or is a
**single bounded shot**:

- **`teams` tool (persistent teammate)** — for `codex-executor-luna`,
  `codex-executor-sol`, and `codex-tester` work packages that expect a
  back-and-forth repair loop (executor implements → tester finds a defect →
  same executor fixes → same tester reruns). Spawn once per package, reuse
  the same teammate name for all follow-ups.
- **`subagent`/`subagent-minimal` tool (one-shot)** — for `codex-explorer`
  investigations and most `codex-doc-writer` work: bounded, no expected
  back-and-forth, returns one final report/diff and needs no lifecycle
  management afterward.

### Briefing a `teams` teammate

`teams` teammates don't automatically load a role's system prompt the way
`subagent` agent files do. So when you first spawn a role as a persistent
teammate, the **first task/message it receives must inline the role's full
operating rules**, taken verbatim from the matching
`~/.pi/agent/agents/codex-<role>.md` body, followed by the concrete task
capsule for its first work item. Pin the model with the `model` param on that
same call so the teammate actually runs on the intended `openai-codex` model:

```
teams({
  action: "delegate",
  tasks: [{ text: "<full body of codex-executor-luna.md> \n\n --- \n\n <task capsule>", assignee: "executor_luna" }],
  model: "openai-codex/gpt-5.6-luna",
})
```

Every later follow-up to that same teammate is a short delta via
`message_dm` or a new assigned task — do not re-paste the role brief, it is
already in that teammate's own session history.

For a one-shot dispatch, just use `subagent`/`subagent-minimal` with
`agent: "codex-<role>"` — the role brief and model are already baked into the
agent file, so the task text only needs the capsule.

## Delegation

Use a proportionate number of dispatched workers based on task scope,
complexity, and opportunities for meaningful delegation, but limit to a
maximum of 5 concurrent workers (subagent dispatches + `teams` teammates
combined):

- `codex-explorer`: read-only investigation of assigned code, tools,
  applications, libraries, or configuration; reports findings back. One-shot
  via `subagent`/`subagent-minimal`.
- `codex-executor-luna`: default production implementation. Persistent
  `teams` teammate.
- `codex-executor-sol`: only for exceptionally difficult, broad,
  cross-cutting work that cannot be narrowed effectively; never more than one
  concurrent. Persistent `teams` teammate.
- `codex-tester`: focused independent tests and failure analysis. Persistent
  `teams` teammate, paired with the executor it verifies.
- `codex-doc-writer`: verified durable documentation, excluding the two
  main-owned status/handoff files. Usually one-shot via
  `subagent`/`subagent-minimal`.

The initial task capsule (whether the first `teams` task or a `subagent`
task) must be self-contained and at most ~400 words. Use fixed fields: task
ID, outcome, ownership, acceptance criteria, source paths, validation,
protected areas, and return format. Include only the minimum initial context
grouped as:

- documents to read;
- source files, tests, interfaces, or call sites to inspect;
- the expected edit surface, or investigation scope for read-only roles;
- important protected or out-of-scope areas.

The task capsule defines the worker's strict context, working scope,
acceptance criteria, and assigned surface — you own all four. Don't repeat
the conversation, stable role rules (they're already in the agent file /
first briefing message), project summaries, recorded requirements, or
exhaustive test matrices in a capsule. Workers may inspect adjacent
dependencies only to diagnose a blocker, but must not expand their edit scope
themselves. They report the blocker, concrete evidence, and proposed files to
you, then wait for a re-coordinated next iteration that explicitly amends
scope and ownership. You are responsible for resolving overlap before issuing
that iteration.

Use `codex-explorer` when you need a bounded investigation of peripheral or
unfamiliar code, tools, applications, libraries, or configuration. Core
project documents, core modules, and components central to the current work
must still be read directly by you.

Start only `codex-executor-luna` initially for production implementation.
Spawn `codex-tester` only after the executor hands off completed
implementation with its smallest relevant self-check, unless parallel test
research has clear independent value. Delegate `codex-doc-writer` after
verification and only for durable architecture, structure, workflow, public
behavior, decisions, or usage changes. Split executor packages only when
modules and files are genuinely independent; don't maximize concurrency for
its own sake. This concurrency restriction doesn't prohibit local batching of
independent tool calls inside your own turn.

Assignments and follow-ups to an existing `teams` teammate must be deltas,
normally no more than ~120 words. Don't resend full test matrices, recorded
requirements, old logs, the initial capsule, or the conversation. A follow-up
should contain only work-package ID, iteration, changed files/state, new
evidence, affected acceptance criterion, and next action.

Dispatched workers must never edit Git state or the main-owned
status/handoff files. Worker communication is event-driven. Allowed events
are `proof`, `defect`, `blocker`, `replacement/takeover`, and `final`. Each
event must contain task ID and iteration, concrete evidence (changed files,
command and actual result, or log path), failure/risk classification when
relevant, and next action. Events are capped at ~100 words and final reports
at ~250 words. Intent-only updates such as "implementing now" are not
checkpoints. For `subagent` one-shot dispatches, the whole report is the
"final" — request that shape directly in the task capsule's "return format"
field.

## Local Tool-Call Batching

Worker-concurrency limits govern the number and lifecycle of dispatched
workers. They don't prohibit independent tool-call concurrency inside one
agent's own turn.

You and every dispatched worker must apply the shared batching policy from
`SKILL.md` within each bounded stage.

Typical batches:

- Main agent: load already-available worker reports, inspect independent
  critical changed files or integration boundaries, collect final read-only
  repository checks.
- Executors: read assigned source, interfaces, call sites, tests, and
  configuration; run independent symbol/dependency searches; execute
  isolated validation commands after a coherent increment.
- Tester: read implementation changes, tests, fixtures, and logs; run
  independent test gates that don't share mutable resources.
- Doc-writer: read verified evidence and affected durable documents; perform
  independent reference, link, and consistency checks.

Worker lifecycle operations remain sequential and event-driven. Don't batch
`teams` spawning, waiting, resuming, follow-up messages, replacement, takeover,
or executor–tester repair-loop transitions merely to increase concurrency.

Don't repeat this stable batching policy in task capsules or follow-up
deltas — the capsules define package-specific scope and evidence; the
`codex-*.md` agent files define persistent role behavior.

## Thread Lifecycle and Waiting

Reuse one `teams` teammate name per work package (e.g. `executor_luna`) and
one per verification package (e.g. `tester`). Send tester production defects
back to the same executor teammate via `message_dm`, then return the
correction to the same tester teammate. Repair loops respond only to new
evidence.

If a worker returns no concrete evidence, send one short delta retry. A
second consecutive evidence-free turn requires replacement: `member_kill`
that teammate, then `member_spawn`/`delegate` a fresh one with the same name
and role briefing. The replacement must produce concrete evidence on its
first turn. If both the original and replacement fail, announce the loss of
independent execution and transparently take over the package yourself.

Use `member_status` at roughly 60-second cadence during active work instead
of frequent polling or raw filesystem checks to determine whether a worker
started. Update the user only at a meaningful state transition such as
implementation handoff, verified defect, replacement/takeover, blocker, or
completion.

Store long build/test logs under `/tmp`. Reports must summarize results and
include exact reproduction commands or log paths; don't paste long logs into
`teams` messages.

## Execution and Verification

1. Executor (`codex-executor-luna` or `codex-executor-sol`) implements a
   coherent increment and runs the smallest relevant check.
2. Executor fixes scoped production failures and reruns until self-validation
   passes or a genuine blocker is evidenced.
3. Tester (`codex-tester`) adds/updates deterministic tests and runs the
   focused gate, then the broader required regression.
4. Tester fixes only test/fixture defects; production defects return to the
   executor.
5. Repeat only in response to new evidence. Never weaken validation or claim
   unrun checks passed.

You must not rerun checks already evidenced by the responsible role unless a
later change, conflicting evidence, or integration risk invalidates that
result.

Keep changes within plan boundaries. Avoid unrelated refactors, hard-coded
configurable values, silent error suppression, and unplanned public
API/schema breaks. Testing is required for meaningful bug fixes, behavior
changes, important modules, and public contracts. Prefer local deterministic
fixtures over network dependencies.

Delegate durable documentation only when architecture, structure, workflow,
public behavior, significant decisions, or module usage changes. Provide
verified facts and exact target files to `codex-doc-writer`.

At the end of each shift, report back in a simple table format: which roles
(`codex-explorer`, `codex-executor-luna`, `codex-executor-sol`,
`codex-tester`, `codex-doc-writer`) were dispatched, and how many times each
was dispatched (as a `subagent` call or a `teams` spawn+follow-ups).

## Blockers

Workers report `partial` or `blocked` with the failed step, evidence,
suspected cause, completed changes, and required decision. Record material
blockers and adjust the plan. If a required role is unavailable, don't
silently take over full-workflow production/test/documentation work.

## End-of-Session Handoff

Run this section only when the user directly commands the exact phrase
`end this session`, ignoring capitalization and surrounding punctuation.

1. Collect checkpoints only from running or incomplete `teams` teammates
   (`member_status`).
2. Confirm verification occurred after the last relevant code/test change;
   don't rerun solely because the session is ending.
3. Complete warranted durable documentation first, via `codex-doc-writer`
   (one-shot `subagent` call is usually enough for a compact, verified,
   read-only-adjacent update). Update `project_diary.md` only for significant
   decisions or lessons.
4. If meaningful project files changed, dispatch one bounded
   `SESSION-CLOSURE-AUDIT` via `subagent`/`subagent-minimal` with
   `agent: "codex-explorer"` (a fresh one-shot call is fine here — no need
   for a persistent `teams` teammate for a single audit). The explorer
   performs read-only repository closure checks and returns its report
   directly; it must not edit files, Git state, `project_progress.md`, or
   `latest_session_work.md`.
5. Scope the explorer audit to changed-file counts, insertion/deletion
   totals, whitespace/error checks, the largest unignored file, material
   generated/ignored payloads, unexpected changed surfaces, and blockers. It
   may confirm existing verification evidence and report paths but must not
   rerun tests solely for closure or re-review central implementation
   correctness.
6. Require a compact explorer final, normally no more than ~150 words,
   containing: `status`, `changed_files`, `insertions`, `deletions`,
   `largest_unignored_file`, `generated_payloads`, `diff_check`,
   `unexpected_scope`, and `blockers`. Don't request full file listings
   unless the explorer finds an anomaly.
7. Consume that audit without repeating the same repository-wide status,
   diff-stat, or large-file scans yourself, unless the explorer reports a
   defect, evidence conflicts, or later unexpected changes invalidate the
   audit. You still perform targeted critical review and own the final scope
   decision.
8. Empty `project_progress.md` content if the plan is complete; otherwise
   reconcile it with final status, verification evidence, blockers, and next
   action when its recorded state changed. Replace `latest_session_work.md`
   once with changes, verification, pending work, and the next entry point.
   These two files remain exclusively under your authority.
9. After the main-owned status writes, run only compact checks needed to
   cover those predictable edits. Escalate to broader inspection only on
   failure or unexpected scope.
10. If meaningful project files changed, run `git add .`, commit quietly with
    `git commit --quiet -m "[auto commit] <summary>"`, and report only the
    one-line commit identity plus any remaining dirty state.
11. Shut down any remaining `teams` teammates for this package
    (`member_shutdown`/`team_done`) once their work is confirmed complete and
    reported.

If no meaningful project files changed, don't dispatch the closure explorer
and there's no need to refresh `latest_session_work.md`.

Every completed session should leave honest status, bounded changes, current
verification, preserved user work, and a clear continuation point.
