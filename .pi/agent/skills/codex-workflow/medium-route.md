# Medium Route

Use this workflow only after the Medium route has been selected under `SKILL.md`.

## Main-Agent Role

You are the main agent and perform the work directly without spawning
subagents or a `teams` team.

Use the full project workflow at a proportionate level: understand the
relevant context, plan when needed, implement the requested changes, verify
the result, and keep the work within scope. Inspect only what is useful for
the current task and avoid unnecessary process overhead.

For common queries, it's not necessary to run the full workflow for simple
tasks.

## Stage Execution and Tool Batching

Divide work into bounded stages such as context loading, targeted inspection,
implementation, verification, and final review.

Before each stage, collect all independent, already-known, non-conflicting
tool calls and issue them together in one turn (see "Tool-Call Batching" in
`SKILL.md`). Evaluate the returned results together before deciding the next
stage.

Typical Medium-route batches:

- Reading several already-identified source, header, test, or configuration
  files.
- Searching several known symbols or call sites.
- Collecting independent repository metadata.
- Running isolated validation commands after a coherent implementation
  increment.

Keep implementation edits sequential when one edit depends on another, files
overlap, or intermediate results determine subsequent changes.

Run validation concurrently only when commands don't share mutable build
output, generated files, fixtures, databases, ports, devices, or other state.
Required checks remain required regardless of whether they were batched.

Don't manufacture stages or extra commands merely to create a batch. Small
tasks may remain a single inspection/edit/validation sequence.

You may use `plan_tracker` for an in-session view of the current task
breakdown; it is a convenience layer only, not a substitute for the durable
writes below.

## Plans and Status Writes

When the user says **"plan the implementation for..."** or explicitly
requests a detailed implementation plan, persist and begin it unless they
request planning only.

Record:

- Goal and scope.
- Constraints and protected areas.
- Acceptance criteria.
- Major implementation steps when useful.
- Dependencies, verification approach, known blockers, and next action.

For durable or multi-session work packages, update
`agent_docs/project_progress.md` at most twice:

1. Mark the package active and record its bounded plan.
2. Reconcile final status, verification evidence, blockers, and next action.

Don't update it after every checkpoint. Keep significant changes to the plan
understandable and traceable.

## Documentation

Update durable documentation only when architecture, structure, workflow,
public behavior, significant decisions, or module usage changes.

Use verified implementation and test results as the source of truth. Update
`agent_docs/project_diary.md` only for decisions, discarded approaches, or
lessons with lasting architectural value.

## Working Rules

Keep changes focused and preserve unrelated user work. Perform verification
appropriate to the risk and scope of the task. Don't claim unrun checks
passed, hide blockers, or broaden the task without a clear need.

Prefer one targeted inspection batch over a sequence of independent
single-file or single-search outer calls. After implementation begins, repeat
inspection only when a changed state, failure, or newly discovered dependency
provides a concrete reason.

## Blockers

When blocked, record:

- The failed step and exact evidence.
- The suspected cause.
- Completed changes and current repository state.
- The affected acceptance criterion.
- The decision, dependency, or external input required.

Don't disguise partial work as completion. Adjust the plan and preserve a
clear continuation point.

## End-of-Session Handoff

Run this section only when the user directly commands the exact phrase
`end this session`, ignoring capitalization and surrounding punctuation.

1. Confirm verification occurred after the last relevant code or test
   change; don't rerun solely because the session is ending.
2. Collect independent final read-only checks — concise status, diff
   statistics, whitespace/error checks, targeted changed-file review — in one
   bounded batch when practical. Keep status-document writes, Git staging,
   and the commit sequential.
3. Empty `project_progress.md` content if the plan is complete; otherwise
   reconcile it with the final status, verification evidence, blockers, and
   next action if its recorded state has changed.
4. Replace `latest_session_work.md` once with changes, verification, pending
   work, blockers, and the next entry point.
5. Update durable docs only when warranted, and `project_diary.md` only for
   significant decisions or lessons.
6. If meaningful project files changed, run `git add .` and commit with
   `git commit -m "[auto commit] <summary>"`.

If no meaningful project files changed, there is no need to refresh
`latest_session_work.md`.

Every completed session must leave honest status, bounded changes, current
verification, and a clear continuation point.
