---
name: team-aware-planning
description: Write implementation plans that include execution handoff choices, especially when a multi-step task may be done solo, in a separate session, or by teammates. Use when you have a spec or requirements for a multi-step task and want team-aware execution options.
---

# Team-Aware Planning

Write compact, execution-ready implementation plans with explicit execution handoff options. Prefer small, concrete tasks with exact files, tests, and commands.

## Rules
- Keep plans practical, not bloated
- Favor TDD and verification
- Save plans to `docs/plans/YYYY-MM-DD-<feature-name>.md`
- Assume the implementer has little context
- When tasks are parallelizable, explicitly call that out

## Plan format
Start with:

```markdown
# [Feature Name] Implementation Plan

> **REQUIRED SUB-SKILL:** Use the executing-plans skill to implement this plan task-by-task.

**Goal:** [One sentence]

**Architecture:** [2-3 sentences]

**Tech Stack:** [Key technologies/libraries]

---
```

Then list short tasks with:
- exact files to create/modify
- exact tests to add/run
- concrete commands
- expected outcomes

## Execution handoff
After saving the plan, mark the planning phase complete.

Then offer **three** execution options:

**1. Subagent-Driven (this session)**  
Fresh subagent per task with review checkpoints.

**2. Parallel Session (separate)**  
A separate execution session with human review between batches.

**3. Dispatch a Team**  
Use the `teams` tool with the `agent-teams` / `team-dispatch` workflow for long-lived teammates working through the plan in parallel.

## Recommendation policy
- If the plan is mostly sequential, recommend **Subagent-Driven** or **Parallel Session** as appropriate.
- If the plan has multiple independent tasks or clear review/implementation splits, recommend **Dispatch a Team**.
- When recommending **Dispatch a Team**, say that team observability is available for follow-up questions like “why is this taking so long?” and “why did we stop using the team?”.
- If the user chooses **Dispatch a Team**, keep using the existing team across later plan tasks by default unless the remaining work becomes tiny or strictly sequential.
- If switching away from the team, explain why clearly instead of silently continuing solo.

## Handoff wording
Use this exact shape:

**Plan complete and saved to `docs/plans/<filename>.md`. Three execution options:**

**1. Subagent-Driven (this session)** — Fresh subagent per task with review checkpoints.

**2. Parallel Session (separate)** — Batch execution with human review between batches.

**3. Dispatch a Team** — Reuse long-lived teammates to work through independent tasks in parallel, with status/observability support for follow-up questions.

Then add a recommendation sentence based on task structure.
