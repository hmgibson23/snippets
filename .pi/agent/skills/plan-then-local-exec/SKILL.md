---
name: plan-then-local-exec
description: Plan with current (strong) model, then dispatch ONE worker with a local model to implement. Use when you want strong-model planning + local-model execution. Handles plan creation, worker dispatch, progress monitoring, and cleanup.
---

# Plan then Local Exec

**One cycle:** Plan with your current model → Dispatch one local-model worker → Review → Cleanup.

The key insight: local models need **much** more detailed plans. Every file path, every code block, every command with expected output goes in the plan. Leave nothing to inference.

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

After saving the plan, dispatch **one** worker with the chosen local model:

```
teams({
  action: "delegate",
  model: "local/<model-name>",
  tasks: [
    {
      text: `Implement the plan at docs/plans/<plan-file.md>.

Read the plan file first. Execute each task step-by-step in order.
Do not skip steps. Do not move to the next task until the current one passes.
Run tests after each implementation step. Commit after each completed task.

When ALL tasks are complete and verified, report back with a summary of what was implemented.`
    }
  ]
})
```

**Important:** Use `model: "local/<name>"` matching exactly what's in `models.json`. Examples: `"local/north"`, `"local/mistral"`, `"local/qwen14"`.

### Phase 4: Monitor

While the worker runs:
- Check `teams({ action: "member_status" })` periodically to see progress
- If the worker reports back with questions, answer them
- If the worker gets stuck, decide: update plan and re-dispatch, or escalate to user

### Phase 5: Review

When the worker reports done:
1. Verify key tests pass (ask the worker or run them yourself)
2. Check the implementation matches the plan
3. Report results to the user:
   - What was implemented
   - Test results
   - Any deviations from the plan
   - Commit SHAs if applicable

### Phase 6: Cleanup

When the user confirms satisfaction:

```
teams({ action: "team_done" })
```

This shuts down the worker and hides the team widget.

If the user wants changes:
- Small: you can make them directly (leader is on strong model)
- Large: create a revised plan and re-dispatch

---

## When the Worker Gets Stuck

1. **Plan ambiguity:** The plan wasn't detailed enough. Note what confused the worker, update the plan, re-dispatch.
2. **Test failures:** The worker should fix within reason. If tests keep failing, inspect and provide guidance.
3. **Different codebase state:** The plan assumed different code. Read the actual files, update the plan, re-dispatch.

**To re-dispatch after fixing the plan:**
```
teams({
  action: "delegate",
  model: "local/<model-name>",
  tasks: [{ text: "Continue implementing the updated plan at docs/plans/.... Start from where you left off." }]
})
```

## When the Plan Is Complex (>8 tasks)

Split into phases. Create one plan per phase. Dispatch for each phase sequentially. This keeps each plan small enough for the local model to hold in context.

---

## Example Full Flow

```
User: Add JSON output to status command
Pi:   Which local model should implement this? Available: local/north, local/mistral, local/qwen14
User: local/north

[Pi reads codebase: src/commands/status.ts, src/cli.ts]
[Pi writes docs/plans/2026-07-03-json-flag.md with hyper-detailed steps]
[Pi dispatches one worker: teams({ action: "delegate", model: "local/north", tasks: [...] })]

Worker: Implemented status --json flag. Tests pass (2/2). Committed as abc1234.
Worker: Summary:
  - Added --json flag parsing to cli.ts
  - Added JSON output branch to status.ts
  - Added test file with 2 tests
  - All tests passing

[Pi checks: tests pass, matches plan, reports to user]

User: Looks good
Pi:   teams({ action: "team_done" })
```
