---
name: team-dispatch
description: Coordinate team reuse and delegation after the decision to use teammates has already been made. Use when the user explicitly wants teammates, or when Pi already has a team and needs guidance on keeping, reusing, or ending it. Do not use for the initial delegation decision.
---

# Team Dispatch

Use this skill for long-lived worker coordination after the routing decision is already made.

## Boundaries
- Use **`team-routing`** first when it is still unclear whether a team should be used.
- Use **this** skill once the answer is yes, or when the user explicitly wants teammates.
- Use **`agent-teams`** for the full packaged reference on `teams` tool actions.
- Use **`team-observability`** for follow-up status and bottleneck questions.

## Core policy
- Prefer teams when there are multiple independent tasks, implementation/review splits, or sustained coordination work.
- If a team is already active, keep reusing that team across later plan tasks by default.
- Do **not** silently stop using the team after the first delegation wave.
- If you intentionally switch to solo work, explain why inline and record it with `team_execution_note` when available.

## Dispatch a Team
When the user has a plan with parallelizable tasks, present **Dispatch a Team** as a first-class execution option.

When chosen:
1. delegate the first wave of independent tasks with `teams`
2. keep the team alive for later plan tasks unless remaining work is tiny or strictly sequential
3. use observability before interrupting workers

## Observability
When the user asks:
- why work is taking so long
- what a teammate is doing
- whether someone is stuck
- why Pi stopped using the team

Do this first:
1. call `team_execution_status`
2. call `teams` with `action: "member_status"`
3. if needed, call `teams` with `action: "member_status", name: "<worker>"`

Then explain:
- what is happening now
- the likely bottleneck
- what you recommend next

## Intervention policy
Safe automatic actions:
- inspect status
- refresh worker state
- send a gentle steer

Ask first before:
- wrap-up
- reassignment
- shutdown / `team_done`
- ending team use

## Delegation style
- break work into clear, independent tasks
- assign explicit owners when useful
- prefer `member_status` before reassigning or interrupting
- integrate and verify results yourself
