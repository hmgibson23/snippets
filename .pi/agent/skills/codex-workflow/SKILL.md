---
name: codex-workflow
description: Structured light/medium/heavy route workflow for project work, using the openai-codex provider (gpt-5.6-luna/gpt-5.6-sol) for delegated roles. Use when the user says "use light/medium/heavy route", asks for the codex workflow, mentions "deployment state"/"leaf state", or wants an orchestrator coordinating explorer/executor_luna/executor_sol/tester/doc-writer style roles on a durable multi-session project. Adapted from github.com/viettran-edgeAI/codex_workflow for pi's `subagent`/`teams` tools.
---

# Codex Workflow (pi + openai-codex)

Adapted from [viettran-edgeAI/codex_workflow](https://github.com/viettran-edgeAI/codex_workflow),
originally written for the OpenAI Codex CLI's native subagent/TOML system. This
version runs the same route-selection philosophy and role design on **pi**,
using pi's own `subagent`/`subagent-minimal` and `teams` tools, and pins
delegated roles to **openai-codex** provider models (`gpt-5.6-luna`,
`gpt-5.6-sol` — confirmed available under `openai-codex/*` in this install).
Make sure you are logged into the `openai-codex` provider (ChatGPT Plus/Pro
OAuth) before using Medium/Heavy route delegation.

## Core Design Principles

The project must strictly follow modular design. Each module should have:

- A clear responsibility.
- A clear interface.
- Minimal unnecessary coupling.
- A structure that makes it easy to test, debug, replace, extend, and reuse.

Nested modules are allowed when they make responsibilities clearer. Avoid
placing unrelated responsibilities into the same file, class, service, or
large function.

- Define proportionate acceptance and verification requirements before
  implementation.
- Keep related tests cohesive enough to avoid fragmented micro-tests, but
  never reduce meaningful coverage, weaken assertions, or hide failures merely
  to save tokens or execution time.

## Tool-Call Batching (pi-native)

Pi already supports issuing several independent tool calls within one
assistant turn (see this session's own system instructions: "If you intend to
call multiple tools and there are no dependencies between the calls, make all
independent calls in the same turn"). Use that instead of the original
repo's `functions.exec`/Code Mode wrapper — the semantics are the same:

- Batch together: read-only file inspection, independent symbol/text/call-site
  searches, repository metadata/status collection, independent log/artifact
  inspection, and validation commands that don't share mutable state.
- Inspect and attribute every returned result. If a partial failure still
  leaves the other results useful, keep going with what succeeded (equivalent
  to `Promise.allSettled`); if any one failure invalidates the whole batch,
  treat the batch as failed (equivalent to `Promise.all`).
- Keep sequential: a result that determines the next operation, adaptive
  investigation, approvals/permission boundaries, `teams`/`subagent` spawn,
  wait, resume, message, or replacement operations, overlapping/order-sensitive
  writes, Git staging/commits/resets, and builds/tests sharing mutable state
  (build dir, generated output, DB, port, fixture, device).
- Don't split an otherwise-batchable inspection across repeated single calls,
  and don't manufacture extra calls just to fill a batch.

This rule applies to the main agent and to every dispatched
`codex-explorer`/`codex-executor-luna`/`codex-executor-sol`/`codex-tester`/
`codex-doc-writer` role.

## Working State

At any given time, we are in one of two working states:

- **`deployment state`**: beginning to plan a broad task or in the process of
  deploying a plan. A deployment plan can span multiple sessions.
- **`leaf state`**: for tasks outside the plan being deployed by the
  deployment state, such as general queries, document editing, or small
  file/module/tool add/modify/delete operations.

## Project Documentation Framework

The main project documents live under `agent_docs/` in the target repo:

- `agent_docs/project_overview.md` — goals, architecture, workflow, major decisions.
- `agent_docs/project_core_tech.md` — brief summary of special technologies/architectures.
- `agent_docs/project_structure.md` — directory layout, modules, components, ownership boundaries.
- `agent_docs/project_progress.md` — active implementation plan and cross-session execution status.
- `agent_docs/project_diary.md` — durable architecture decisions, discarded approaches, lessons.
- `agent_docs/latest_session_work.md` — summary of previous sessions plus unfinished tasks.
- Module-specific documents, when present.

`agent_docs/project_progress.md` and `agent_docs/latest_session_work.md` exist
to make cross-session deployment smooth. They can only be edited in
`deployment state` (Medium/Heavy route) or when the user explicitly requests
it. The main agent (you) is responsible for updating these two files;
dispatched roles (`codex-executor-*`, `codex-tester`, `codex-doc-writer`) are
never allowed to edit them.

Update documentation only with verified facts. Keep temporary reasoning, raw
logs, and short-lived checkpoints out of durable project documents.

Never delete any main project document without warning the user and
receiving a second explicit confirmation.

In a pi session you may *additionally* use the `plan_tracker` tool as a
lightweight, in-session view of the current task list — it is a convenience
layer, not a replacement for `agent_docs/project_progress.md`, which remains
the durable, cross-session, repo-committed record.

## Route Selection

There are three routes.

### Light route

Use for light tasks in the `leaf state`. Perform the task yourself. Do not
spawn subagents or a `teams` team in this route.

### Medium route

Use for deploying large tasks/plans in the `deployment state`. Perform the
work yourself — do not spawn subagents or a `teams` team in this route either.
Read and follow `medium-route.md` (in this skill's directory).

### Heavy route

You are the orchestrator, coordinating dispatched roles to deploy large
tasks/plans in the `deployment state`, using pi's `teams` tool (persistent
role-teammates) and/or the `subagent`/`subagent-minimal` tool (bounded
one-shot dispatches). Read and follow `heavy-route.md` (in this skill's
directory).

In every route, for common/simple queries it is not necessary to run the full
workflow ceremony — do the simple thing.

### Route selection rules and state interpolation

The user specifies the route, e.g. "use light/medium/heavy route...". Apply
that route for the entire session until it ends or the user switches routes.
If the user does not specify a route, default to the light route. Do not
guess a route yourself.

- Light route selected/chosen ⇒ we are in `leaf state`.
- Medium or Heavy route selected ⇒ we are in `deployment state`.

## Context Loading

- In the Light route (`leaf state`), read only the files relevant to the
  current task.
- On first entering the `deployment state`, load the foundational project
  context in one bounded read-only batch:
  1. `agent_docs/project_overview.md`
  2. `agent_docs/project_structure.md`
  3. `agent_docs/project_progress.md`
  4. `agent_docs/latest_session_work.md`
- After the batch returns, interpret overview and structure before
  reconciling progress and the latest-session handoff. This interpretation
  order does not require separate outer tool calls.
- Use the resulting status/ownership map to inspect the smallest relevant
  interfaces, call sites, tests, and configuration surface.
- Read only relevant module documentation. Expand source inspection only when
  repository evidence requires it.
- Reconstruct active tasks, dependencies, verification state, and blockers.
  Resolve contradictions with targeted evidence.
- Under the Heavy route, review only critical hunks and integration
  boundaries after delegation, unless risk, missing evidence, or conflicting
  results require broader inspection.

## Delegated Roles (openai-codex provider)

Five role agents live under `~/.pi/agent/agents/` and are usable directly via
the `subagent`/`subagent-minimal` tools (`agent: "codex-<role>"`), and are
also the briefing text sent to persistent `teams` teammates in the Heavy
route. All are pinned to `openai-codex` models to match the source repo:

| Role | File | Model | Purpose |
|---|---|---|---|
| Explorer | `codex-explorer` | `openai-codex/gpt-5.6-luna` | read-only investigation, reports only |
| Executor (default) | `codex-executor-luna` | `openai-codex/gpt-5.6-luna` | default production implementation |
| Executor (hard cases) | `codex-executor-sol` | `openai-codex/gpt-5.6-sol` | one exceptionally hard cross-cutting package, never >1 concurrent |
| Tester | `codex-tester` | `openai-codex/gpt-5.6-luna` | independent verification, tests/fixtures only |
| Doc-writer | `codex-doc-writer` | `openai-codex/gpt-5.6-luna` | durable docs from verified facts, excludes progress/handoff files |

To use a different `openai-codex` model tier (e.g. `gpt-5.6-terra`,
`gpt-5.5`, `gpt-5.4`), edit the `model:` frontmatter line in the matching
`~/.pi/agent/agents/codex-*.md` file — the routing/ownership rules below don't
change.

## Platform-specific paths

Paths in this workflow are written using `/` as a platform-neutral separator.
When running filesystem commands, use paths appropriate for the current
operating system and shell:

- On Linux and macOS, use `/`.
- On Windows, use the equivalent Windows path format and `\` where required.

Do not treat the example path separator as a literal requirement. Resolve
every path using the conventions of the current environment.
