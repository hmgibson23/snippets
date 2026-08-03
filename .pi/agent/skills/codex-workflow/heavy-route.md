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

## Dispatch mechanism: `subagent` only

Every dispatched worker in this route is a bounded, one-shot call through the
`subagent`/`subagent-minimal` tool with `agent: "codex-<role>"`. There is no
persistent teammate/thread in this setup — each call spins up an isolated
headless `pi` subprocess that runs the task to completion and returns one
final report, with no memory of anything beyond that single call.

The role brief and model are already baked into the matching
`~/.pi/agent/agents/codex-<role>.md` file, so a task capsule only needs the
concrete work-package content — you don't need to re-paste any role rules.

### Repair loops without a persistent thread

The original design (and an earlier `teams`-based version of this route)
assumed a persistent worker you could message repeatedly. Without that, an
executor → tester → executor repair loop is just **a sequence of independent
`subagent` calls**, where each new call's task text carries forward exactly
the state the previous call would have remembered:

1. Dispatch `codex-executor-luna` (or `-sol`) with the task capsule. It
   returns a final report: files changed, validation run, actual result.
2. Dispatch `codex-tester` with a task capsule that includes: the acceptance
   criteria, the file paths that changed, and (if useful) a compact `git
   diff --stat`/summary of what the executor reported. It returns
   `passed`/`failed`/`partial`/`blocked` plus evidence.
3. If the tester reports a production defect: dispatch `codex-executor-luna`
   again (same role, fresh call) with a task capsule containing only the
   delta — reproduction command, expected vs. actual, affected criterion, log
   path from the tester's report. It does not need the original capsule
   re-sent; give it enough of the prior evidence to act, not the whole
   conversation.
4. Repeat only in response to new evidence, same as before.

This is more verbose per iteration (you carry the delta yourself instead of
a teammate remembering it), but functionally equivalent for this workflow's
purposes: each subagent call is already expected to be self-contained and
capsule-driven, never conversational.

## Delegation

Use a proportionate number of dispatched workers based on task scope,
complexity, and opportunities for meaningful delegation, but limit to a
maximum of 5 outstanding/in-flight `subagent` dispatches at once:

- `codex-explorer`: read-only investigation of assigned code, tools,
  applications, libraries, or configuration; reports findings back.
- `codex-executor-luna`: default production implementation.
- `codex-executor-sol`: only for exceptionally difficult, broad,
  cross-cutting work that cannot be narrowed effectively; never more than one
  concurrent.
- `codex-tester`: focused independent tests and failure analysis, paired
  with the executor it verifies (see repair-loop pattern above).
- `codex-doc-writer`: verified durable documentation, excluding the two
  main-owned status/handoff files.

The task capsule for each dispatch must be self-contained and at most ~400
words. Use fixed fields: task ID, outcome, ownership, acceptance criteria,
source paths, validation, protected areas, and return format. Include only
the minimum initial context grouped as:

- documents to read;
- source files, tests, interfaces, or call sites to inspect;
- the expected edit surface, or investigation scope for read-only roles;
- important protected or out-of-scope areas.

The task capsule defines the worker's strict context, working scope,
acceptance criteria, and assigned surface — you own all four. Don't repeat
the conversation, stable role rules (already in the agent file), project
summaries, recorded requirements, or exhaustive test matrices in a capsule.
Workers may inspect adjacent dependencies only to diagnose a blocker, but
must not expand their edit scope themselves. They report the blocker,
concrete evidence, and proposed files in their final report; you then issue
a fresh, re-coordinated dispatch (new `subagent` call) that explicitly
amends scope and ownership. You are responsible for resolving overlap before
issuing that next call.

Use `codex-explorer` when you need a bounded investigation of peripheral or
unfamiliar code, tools, applications, libraries, or configuration. Core
project documents, core modules, and components central to the current work
must still be read directly by you.

Start only `codex-executor-luna` initially for production implementation.
Dispatch `codex-tester` only after the executor's final report shows
completed implementation with its smallest relevant self-check, unless
parallel test research has clear independent value. Delegate
`codex-doc-writer` after verification and only for durable architecture,
structure, workflow, public behavior, decisions, or usage changes. Split
executor packages only when modules and files are genuinely independent;
don't maximize concurrency for its own sake. This concurrency restriction
doesn't prohibit local batching of independent tool calls inside your own
turn, or running several independent `subagent` dispatches in parallel via
the `parallel`/`chain` modes when the tasks are genuinely independent.

Dispatched workers must never edit Git state or the main-owned
status/handoff files. Do not trust a worker's self-reported "done" — its
final report is a summary from a real headless `pi` subprocess with no
memory of this conversation, not proof. Verify with `git diff`/tests
yourself before treating a work package as closed.

## Local Tool-Call Batching

Worker-concurrency limits govern the number of dispatched workers. They
don't prohibit independent tool-call concurrency inside one agent's own
turn.

You and every dispatched worker must apply the shared batching policy from
`SKILL.md` within each bounded stage.

Typical batches:

- Main agent: inspect independent critical changed files or integration
  boundaries after a dispatch returns, collect final read-only repository
  checks.
- Executors: read assigned source, interfaces, call sites, tests, and
  configuration; run independent symbol/dependency searches; execute
  isolated validation commands after a coherent increment.
- Tester: read implementation changes, tests, fixtures, and logs; run
  independent test gates that don't share mutable resources.
- Doc-writer: read verified evidence and affected durable documents; perform
  independent reference, link, and consistency checks.

Don't repeat this stable batching policy in task capsules — the capsules
define package-specific scope and evidence; the `codex-*.md` agent files
define persistent role behavior.

## Execution and Verification

1. Executor (`codex-executor-luna` or `codex-executor-sol`) implements a
   coherent increment and runs the smallest relevant check.
2. Executor fixes scoped production failures and reruns until self-validation
   passes or a genuine blocker is evidenced, then returns its final report.
3. Tester (`codex-tester`) adds/updates deterministic tests and runs the
   focused gate, then the broader required regression, in a fresh dispatch
   that references the executor's reported changes.
4. Tester fixes only test/fixture defects; production defects go back to a
   fresh executor dispatch (see repair-loop pattern above).
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
was dispatched.

## Blockers

Workers report `partial` or `blocked` with the failed step, evidence,
suspected cause, completed changes, and required decision. Record material
blockers and adjust the plan. If a required role can't complete its package,
don't silently take over full-workflow production/test/documentation work —
either retry with a corrected capsule, or explicitly tell the user you're
taking over and why.

## End-of-Session Handoff

Run this section only when the user directly commands the exact phrase
`end this session`, ignoring capitalization and surrounding punctuation.

1. Confirm verification occurred after the last relevant code/test change;
   don't rerun solely because the session is ending.
2. Complete warranted durable documentation first, via a `codex-doc-writer`
   dispatch. Update `project_diary.md` only for significant decisions or
   lessons.
3. If meaningful project files changed, dispatch one bounded
   `SESSION-CLOSURE-AUDIT` via `subagent`/`subagent-minimal` with
   `agent: "codex-explorer"`. The explorer performs read-only repository
   closure checks and returns its report directly; it must not edit files,
   Git state, `project_progress.md`, or `latest_session_work.md`.
4. Scope the explorer audit to changed-file counts, insertion/deletion
   totals, whitespace/error checks, the largest unignored file, material
   generated/ignored payloads, unexpected changed surfaces, and blockers. It
   may confirm existing verification evidence and report paths but must not
   rerun tests solely for closure or re-review central implementation
   correctness.
5. Require a compact explorer final, normally no more than ~150 words,
   containing: `status`, `changed_files`, `insertions`, `deletions`,
   `largest_unignored_file`, `generated_payloads`, `diff_check`,
   `unexpected_scope`, and `blockers`. Don't request full file listings
   unless the explorer finds an anomaly.
6. Consume that audit without repeating the same repository-wide status,
   diff-stat, or large-file scans yourself, unless the explorer reports a
   defect, evidence conflicts, or later unexpected changes invalidate the
   audit. You still perform targeted critical review and own the final scope
   decision.
7. Empty `project_progress.md` content if the plan is complete; otherwise
   reconcile it with final status, verification evidence, blockers, and next
   action when its recorded state changed. Replace `latest_session_work.md`
   once with changes, verification, pending work, and the next entry point.
   These two files remain exclusively under your authority.
8. After the main-owned status writes, run only compact checks needed to
   cover those predictable edits. Escalate to broader inspection only on
   failure or unexpected scope.
9. If meaningful project files changed, run `git add .`, commit quietly with
   `git commit --quiet -m "[auto commit] <summary>"`, and report only the
   one-line commit identity plus any remaining dirty state.

If no meaningful project files changed, don't dispatch the closure explorer
and there's no need to refresh `latest_session_work.md`.

Every completed session should leave honest status, bounded changes, current
verification, preserved user work, and a clear continuation point.
