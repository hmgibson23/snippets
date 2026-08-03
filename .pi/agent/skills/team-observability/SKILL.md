---
name: team-observability
description: Diagnose status and bottlenecks for an active or recently used team. Use when the user asks why team work is slow, what a teammate is doing, whether someone is stuck, or why Pi stopped reusing the team. Do not use to decide whether to start delegation.
---

# Team Observability

Use this skill when a team is active or was recently active and the user asks for status, delay diagnosis, or coordination reasoning.

## Boundaries
- Use **this** skill for diagnosis and explanation only.
- Use **`team-routing`** for the initial solo-vs-team decision.
- Use **`team-dispatch`** to spawn, manage, reuse, or steer teammates after delegation is chosen.

## Default workflow
1. Inspect the current team execution context with `team_execution_status`.
2. If a team is active or recently active, call the `teams` tool with `action: "member_status"`.
3. If the user asks about a specific teammate, call `teams` with `action: "member_status", name: "<name>"`.
4. Summarize the bottleneck in plain English.
5. If helpful, suggest next actions.

## Interpretation rules
- If a worker is `stalled`, say so clearly.
- If the current activity is a tool like bash/test/search, mention that as the likely source of delay.
- If the team is no longer being reused, explain the recorded fallback reason from `team_execution_status`.
- Distinguish between:
  - actively working
  - blocked/waiting
  - stalled
  - paused team execution
  - deliberate solo fallback

## Allowed automatic actions
You may automatically:
- refresh status
- inspect workers
- send a gentle steer if clearly useful

Ask first before:
- wrap-up
- reassignment
- shutdown / team_done
- ending team use

## Response pattern
Answer with:
1. **What is happening now**
2. **Why it is slow / blocked / solo**
3. **What I recommend next**

Keep it crisp and operational.
