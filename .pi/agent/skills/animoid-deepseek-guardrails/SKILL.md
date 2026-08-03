---
name: animoid-deepseek-guardrails
description: Keeps a fast, weaker execution model (especially deepseek-v4-flash) honest while implementing an Animoid plan. Forces one-task-at-a-time execution, a todo list, RED-before-code, wiring proof, and real command output before completion claims.
---

# Animoid deepseek guardrails

You are a fast model. Your known failure is: **you say a plan or task is DONE when it is not.** This skill removes that failure. Follow it literally. Do not improvise.

Also load and obey: `execute-plan-fully` (the workflow) and `animoid-zig-nightly` (Zig/JNI correctness) and `animoid` (project rules). This skill sits ON TOP of them and only adds hard stops. If they conflict with each other, the plan and the user win; this skill never loosens their gates.

> Dispatching this to a worker? Prepend `dispatch-preamble.md` (next to this file) to the plan you pass to `dispatch_implementation`, then append the real plan under its `--- PLAN ---` marker.

## THE ONE RULE

> You may NOT write the words **done, complete, finished, working, fixed, passing, or ✅** about any task unless you pasted, THIS TURN, the real command output that proves it.

No pasted output = not done. Your memory is not evidence. A summary is not evidence. "Should work" is not evidence. Compiling is not passing. Green aggregate tests do not prove wiring, UI, JNI, or persistence.

If you are about to claim progress, STOP and run the Done Test (bottom of this file) first.

## Setup (do once, before any edit)

Run these and paste the output:

```bash
/opt/homebrew/bin/zig version          # must be 0.17.0-dev.1387+01b60634c
bash ~/.pi/agent/skills/animoid/scripts/check-zig-toolchain.sh
git status --short
git log -1 --oneline
```

Never run the Android emulator, `adb`, connected/instrumented tests
(`connectedAndroidTest`, running `androidTest` on a device/emulator), or
`android-emulator-gate.sh`. That is the orchestrator's job only — you are a
dispatched subagent with no visibility into whether another process already
owns the device. If a task needs runtime Android verification,
`assembleDebug`/`compileDebugAndroidTestSources` (compile-only) is as far as
you go; note in the ledger that emulator verification is still needed and
move on.

Then read the WHOLE plan and create a Pi todo list with one item per plan task. Do not create an execution-ledger file or a pre-edit runtime-baseline task. If context is compacted, reread the plan and current todo list before the next edit.

## The loop — repeat for EVERY task, in plan order

Never skip a task. Never reorder. Never do "only the important ones". One task at a time.

For syntax, read `references/zig-modern-syntax.md` in the `animoid-zig-nightly` skill — do NOT reconstruct Zig from memory.

```
1. READ    → open the exact production file + its tests. rg the symbol + callers. Paste what you found.
2. RED     → write the smallest test. RUN IT. Paste output. It MUST fail for a BEHAVIOUR reason
             (e.g. "expected 3 dabs, got 0"). A compile error / FileNotFound is NOT red — fix and rerun.
3. CODE    → write the minimum production change. No TODOs, no stubs, no no-op branches, no catch unreachable.
4. WIRING  → prove a NON-test file calls your new code. Paste the rg line:
             rg -n '<NewSymbol>\(' --glob '!**/test/**' --glob '!**/androidTest/**' src/
             If every hit is a test → NOT wired → task INCOMPLETE. Keep working.
5. GREEN   → run the focused test wrapper (below). Paste output.
6. CLOSE   → mark the todo complete only after focused tests and wiring proof. Then move to the next task.
```

After each task, do NOT stop to ask "should I continue?". Continue to the next task automatically. A progress update is not a stopping point.

## Animoid evidence commands (these ARE the proof — raw `zig test` is not)

Focused test (verifies the named test actually ran):

```bash
bash ~/.pi/agent/skills/animoid-zig-nightly/scripts/run-focused-test.sh \
  "$PWD" '<filter>' '<fully-qualified expected test name>'
```

Test-quality lint on your added tests:

```bash
bash ~/.pi/agent/skills/animoid-zig-nightly/scripts/lint-added-tests.sh "$PWD" main
```

Final gates (run FRESH this turn, paste each):

```bash
/opt/homebrew/bin/zig fmt <changed-files>
/opt/homebrew/bin/zig build test --summary all
/opt/homebrew/bin/zig test src/run_tests.zig -O ReleaseFast
/opt/homebrew/bin/zig build -Drelease=true
git diff --check
git status --short
```

## Banned excuses (if you catch yourself writing one, you are NOT done — keep working)

| You are tempted to write… | Reality |
|---|---|
| "The change is complete and should pass the tests." | You didn't run them. Run them, paste output. |
| "This is functionally equivalent / basically done." | Not done. Prove exact behaviour with a test. |
| "The build compiles, so the task is finished." | Compiling ≠ passing ≠ wired. Run tests + rg wiring. |
| "All tests pass" (without pasted output) | No paste = no claim. Run the wrapper, paste it. |
| "I added the function / setter / field." | A setter with no production caller is INCOMPLETE. Paste the rg wiring line. |
| "The test passes now" (after weakening it) | Weakening/deleting/skipping a valid test is forbidden. Revert it. |
| "I'll mark the remaining tasks done for brevity." | Never. Every row needs its own pasted evidence. |
| "It doesn't crash, so it works." | "No crash" is not a valid test. Assert exact behaviour. |
| "The plan is mostly complete." | "Mostly" = INCOMPLETE. List which rows lack evidence and finish them. |

## Do not

- Do not run the Android emulator, `adb`, or connected/instrumented tests — only the orchestrator does that (see Setup above).
- Do not invent test results, counts, or output. Paste only real terminal output.
- Do not `expect(true)`, non-zero/threshold-only, or hash-sentinel assertions (the lint will fail you).
- Do not use remembered Zig 0.11–0.16 syntax. Use the pinned compiler and `references/zig-modern-syntax.md`.
- Do not cast a negative JNI int before validating it. Validate, then `@intCast`.
- Do not commit unless the plan/user authorises it. Do not bundle unrelated changes.
- Do not stop because a task is hard or context was compacted. Re-read the plan and todo list, then continue.

## STOP — the Done Test (run before ANY completion claim)

Answer every question. If any answer is NO or "I'm not sure" → you are NOT done. Keep working. Do not write COMPLETE.

1. Did I run EVERY task in the plan, in order? (check the todo list)
2. Is every todo complete with real test and wiring evidence?
3. For every new symbol, did I paste an `rg` line showing a NON-test production file calls it?
4. Did I run the focused wrapper and the four final gates FRESH this turn and paste their output?
5. Did the test-added lint pass?
6. Is `git diff --check` clean and `git status --short` showing only intended files?
7. Did I avoid weakening, skipping, or deleting any valid test?

Only if ALL seven are YES, and only after re-reading the whole plan against the todo list, may you write:

**COMPLETE** — followed by concise final-gate results.

If any is NO, write instead: **INCOMPLETE — remaining todos: <list>**, then keep working on the first remaining todo.
