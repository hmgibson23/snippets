---
name: team-routing
description: Decide whether to use the `teams` tool before any team is active. Use when work might be parallelizable but delegation has not yet been chosen. Do not use for active-team management, teammate status questions, or explicit team-dispatch requests.
---

# Team Routing

Use this skill only when deciding whether Pi should stay solo or start using the **`teams`** tool.

## Boundaries
- Use **this** skill for the routing decision itself.
- Use **`team-dispatch`** once delegation is chosen or the user explicitly wants a team.
- Use **`team-observability`** for status, slowdown, stuck-worker, or solo-fallback questions about an active/recent team.

## Default policy

Be **conservative** about delegation.

Prefer doing the work directly unless one of these is true:
- there are **2 or more independent tasks** that can proceed in parallel
- the task naturally splits into distinct tracks like **research + implementation**, **implementation + review**, or **multiple isolated investigations**
- long-running coordination would clearly benefit from separate workers
- there is real uncertainty about whether the coordination upside outweighs the overhead

If a team is **already active**, prefer **continued reuse of that same team** for later plan tasks unless the remaining work is tiny, strictly sequential, or no longer benefits from delegation.

## Do use `teams` when

- the work can be divided into separate, non-blocking subtasks
- independent files or components can be handled separately
- you want one worker to investigate while another implements
- you want a delegated review or verification pass that does not block other work
- the task has enough clearly independent scope that starting a team is plausibly worth the coordination overhead

## Do NOT use `teams` when

- the task is a small edit, single bugfix, or narrow question
- the work is mostly sequential and one step depends tightly on the previous one
- the task is exploratory but not parallelizable
- coordination overhead would outweigh any speedup
- there is no clear split into independent subtasks

## Team system policy

Use **`@tmustier/pi-agent-teams`** via the **`teams`** tool as the default and only team system.

Do **not** route work to `pi-teams`.

## Operating style

When you choose to delegate:
1. Break the work into small independent tasks
2. Assign clear outcomes for each worker
3. Prefer checking worker status before interrupting or reassigning
4. Integrate and verify results yourself before claiming success
5. If the user is working from a plan with multiple independent tasks, consider **Dispatch a Team** as a first-class execution option
6. When a team is already active, keep using it by default across the plan instead of silently switching to solo work

When you choose **not** to delegate:
- say so briefly if useful
- if a team was already active and you are switching to solo work, explain **why** inline
- continue directly without unnecessary team setup

## Observability handoff

If the user asks why work is taking a long time, what a teammate is doing, whether someone is stuck, or why the team is no longer being used, hand off to **`team-observability`** rather than treating that as a routing decision.

## Rule of thumb

If you cannot name **at least two independent subtasks**, do **not** use `teams`.
