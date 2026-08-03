---
name: plan-then-local-exec
description: Plan with current (strong) model, then dispatch ONE worker with a local model to implement via the `subagent` tool. Use when you want strong-model planning + local-model execution. Handles plan creation, worker dispatch, review, and cleanup.
---

# Plan then Local Exec

**One cycle:** Plan with your current model → Dispatch one local-model worker via `subagent` → Review → Cleanup.

The key insight: local models need **much** more detailed plans. Every file path, every code block, every command with expected output goes in the plan. Leave nothing to inference.

This uses the `subagent`/`subagent-minimal` tool, not a persistent worker —
each dispatch is a single bounded call that runs to completion and returns
one final report. There's no live "monitor while it runs" step: the tool
call itself blocks until the local-model worker finishes (or times out).

## Prerequisites

- Leader on a capable planning model (Claude Sonnet, GPT-4o, etc.)
- At least one local model in `~/.pi/agent/models.json` (e.g. `local/north`, `local/mistral`)
- Project has a `docs/plans/` directory (create if missing)

---

## Workflow

### Phase 1: Scope

**Ask the user two things (one message):**
1. "What needs to be built or changed?"
2. "Which local model should implement it?" — list available local models from models.json

Once you have both answers, move to Phase 2.

### Phase 2: Write the Plan

Create a **hyper-detailed** plan at `docs/plans/YYYY-MM-DD-<feature>.md`.

Follow the writing-plans structure but with extra detail since a local model executes it:

**Header:**
```markdown
# [Feature] Implementation Plan

**Target model:** local/<model-name>
**Plan author:** <current leader model>

## Context
- What this feature does
- Which files exist and what they do (read the codebase)
- Architecture decisions the implementer doesn't need to rethink
```

**Each task MUST include:**
- **Exact file paths** for create/modify/delete
- **Complete code** for new files (imports, everything)
- **Exact search/replace text** for modifications (show the old block, show the new block)
- **Exact shell commands** to run, with expected output
- **Test-first: write the test, show expected failure, write the code, show expected pass**

Example task:
```markdown
### Task 1: Add JSON output flag to status command

**Files:**
- Modify: `src/commands/status.ts` — add `--json` flag parsing
- Create: `tests/commands/status.test.ts` — new test file
- Modify: `src/cli.ts` — register `--json` flag

**Step 1: Write failing test**

Create `tests/commands/status.test.ts`:
```typescript
import { describe, it, expect } from 'vitest';
import { runStatus } from '../src/commands/status';

describe('status --json', () => {
  it('returns JSON output when --json flag is set', async () => {
    const result = await runStatus({ json: true });
    expect(() => JSON.parse(result)).not.toThrow();
    const parsed = JSON.parse(result);
    expect(parsed).toHaveProperty('uptime');
    expect(parsed).toHaveProperty('version');
  });
});
```

**Step 2: Run test, expect failure**

```bash
npx vitest run tests/commands/status.test.ts 2>&1
```
Expected: FAIL — `runStatus` doesn't accept `{ json: true }` yet.

**Step 3: Implement**

In `src/commands/status.ts`, add JSON branch after existing status logic:
```typescript
// After line 45, add:
if (opts.json) {
  return JSON.stringify({
    uptime: process.uptime(),
    version: pkg.version,
    mode: opts.mode || 'normal',
  }, null, 2);
}
```

**Step 4: Run test, expect pass**

```bash
npx vitest run tests/commands/status.test.ts 2>&1
```
Expected: PASS

**Step 5: Commit**

```bash
git add src/commands/status.ts tests/commands/status.test.ts
git commit -m "feat: add --json output flag to status command"
```
```

### Phase 3: Dispatch One Worker

Write a per-run agent file pinned to the chosen local model (same pattern as
`subagent-dispatch`'s dynamic templates — this overwrites any previous run,
which is fine, it's per-run scratch):

Write to `~/.pi/agent/agents/local-plan-worker.md`:

```markdown
---
name: local-plan-worker
description: Implements a hyper-detailed plan on a user-chosen local model. Generic, one-off.
tools: read,write,edit,bash,grep,find,ls
model: local/<model-name>
timeout: 120
---

You are implementing a hyper-detailed plan written for you by a stronger
planning model. Read the plan file named in the task fully before touching
anything.

Rules:
- Execute each task in the plan in order. Do not skip steps, do not move to
  the next task until the current one's test passes.
- Follow the plan's exact file paths, code blocks, and commands — it was
  written with no inference required.
- Run the specified test after each implementation step; expect the stated
  result before moving on.
- Commit after each completed task, using the commit message given in the
  plan if one is provided.
- This is a non-interactive run — if something is ambiguous, make the most
  reasonable call given the plan's stated context and say so explicitly in
  your final report. Do not stall waiting for a reply.
- When ALL tasks are complete and verified, report: tasks completed, test
  results, commit SHAs, and any deviations from the plan.
```

Replace `local/<model-name>` with the model chosen in Phase 1 (must match
`~/.pi/agent/models.json` exactly, e.g. `local/north`, `local/mistral`,
`local/qwen14`).

Then dispatch:

```
subagent({
  agent: "local-plan-worker",
  task: "Implement the plan at docs/plans/<plan-file.md>. Read the plan file first, then execute it exactly as written."
})
```

This call blocks until the worker finishes (or its timeout is hit) and
returns one final report — there's no separate "wait/poll" step.

### Phase 4: Review

When the `subagent` call returns:
1. Verify key tests pass yourself (`git diff`, rerun the test command) —
   don't just trust the worker's self-reported "done".
2. Check the implementation matches the plan.
3. Report results to the user:
   - What was implemented
   - Test results
   - Any deviations from the plan
   - Commit SHAs if applicable

### Phase 5: Cleanup

Nothing to shut down — a `subagent` call has no lingering process once it
returns. If the user wants changes:
- Small: you can make them directly (leader is on strong model)
- Large: create a revised plan and re-dispatch (Phase 3 again)

---

## When the Worker Gets Stuck

Since each dispatch is a fresh, memoryless call, "getting stuck" shows up as
an incomplete/blocked final report, not a live conversation you can steer
mid-run.

1. **Plan ambiguity:** The plan wasn't detailed enough. Note what confused
   the worker from its final report, update the plan, re-dispatch (a fresh
   `subagent` call with the same agent).
2. **Test failures:** Check the worker's report for what failed. If it's a
   real plan/codebase mismatch, fix the plan and re-dispatch; don't just
   re-run hoping for a different result.
3. **Different codebase state:** The plan assumed different code. Read the
   actual files, update the plan, re-dispatch.

**To re-dispatch after fixing the plan or continuing partial progress:**

```
subagent({
  agent: "local-plan-worker",
  task: "Continue implementing the updated plan at docs/plans/<plan-file.md>. Tasks 1-<N> are already done and committed (see git log) — start from task <N+1>."
})
```

Since there's no persistent memory between calls, always state explicitly in
the task text what's already done, referencing git history/committed state
rather than assuming the worker remembers a prior run.

## When the Plan Is Complex (>8 tasks)

Split into phases. Create one plan per phase. Dispatch for each phase
sequentially (one `subagent` call per phase). This keeps each plan small
enough for the local model to hold in context.

---

## Example Full Flow

```
User: Add JSON output to status command
Pi:   Which local model should implement this? Available: local/north, local/mistral, local/qwen14
User: local/north

[Pi reads codebase: src/commands/status.ts, src/cli.ts]
[Pi writes docs/plans/2026-07-03-json-flag.md with hyper-detailed steps]
[Pi writes ~/.pi/agent/agents/local-plan-worker.md pinned to model: local/north]
[Pi dispatches: subagent({ agent: "local-plan-worker", task: "Implement the plan at docs/plans/2026-07-03-json-flag.md" })]

[call returns with a final report:]
Implemented status --json flag. Tests pass (2/2). Committed as abc1234.
Summary:
  - Added --json flag parsing to cli.ts
  - Added JSON output branch to status.ts
  - Added test file with 2 tests
  - All tests passing

[Pi verifies: git diff, reruns tests, matches plan, reports to user]

User: Looks good
```
