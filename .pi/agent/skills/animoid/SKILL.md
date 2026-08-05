---
name: animoid
description: Use when planning, implementing, debugging, reviewing, or verifying Animoid work involving its Zig 0.17 nightly engine, Android editor, JNI bridge, Vulkan presenter, timeline, brushes, selection, animation, effects, persistence, or tests.
---

# Animoid Workflow

## Pinned coding subagents

Before every Animoid coding/remediation dispatch, read agent frontmatter and verify exact model AND thinking level. Use only:

| Role | Agent profile | Required model | Required thinking |
|---|---|---|---|
| Fast implementation | `animoid-deepseek-implementer` | `deepseek/deepseek-v4-flash` | (model has no thinking levels) |
| MiMo implementation/remediation | `animoid-mimo-pro-remediator` | `mimo/mimo-v2.5-pro` | (unspecified — leave as-is) |
| Terra remediation | `animoid-terra-remediator` | `openai-codex/gpt-5.6-terra` | `high` |

**Blanket rule, applies to every Animoid agent file pinned to one of these two
models, not just the ones named above:** any agent whose `model:` is
`openai-codex/gpt-5.6-luna` (`animoid-luna-fast`, `animoid-luna-remediator`,
`animoid-luna-codex-remediator`, `animoid-luna-plan-implementer`, or a future
one-off) must have `thinking: xhigh`. Any agent whose `model:` is
`openai-codex/gpt-5.6-terra` (`animoid-terra-implementer`,
`animoid-terra-remediator`, or a dynamically-generated review agent that
resolved to terra) must have `thinking: high`. If you find or create one
that doesn't match, fix its frontmatter before dispatching — don't dispatch
with the wrong thinking level and don't silently accept a mismatch.

Never dispatch `opencode/mimo-v2.5-free` for Animoid work. Hugo has a paid MiMo subscription; MiMo means `mimo/mimo-v2.5-pro` unless he explicitly requests another variant. If frontmatter differs, stop and repair routing before dispatch. Keep each one-shot task narrow enough to finish, and give build/test commands enough timeout for Zig/Gradle.

## Parallel dispatch for independent work

Whenever there are 2+ independent tasks or remediation findings (different
files/modules, no shared state, neither needs the other's output), dispatch
them **in parallel** — one `subagent({ tasks: [...] })` call covering the
whole independent batch, not one call per task waited on serially. See
`parallel-plan-execution` for the full wave/dependency-grouping method (group
by independence, one call per wave not per task, still exactly one review/
remediation pass covering everything in that round).

Parallelizing never means changing which model does the work — every task
in a parallel batch still uses the correct pinned agent from the table above
(e.g. several `animoid-deepseek-implementer` calls in one parallel batch, or
several `animoid-terra-remediator` calls in one parallel batch).

Not independent — never run these against each other in parallel: two tasks
that touch the same file/module, a JNI declaration and its Kotlin caller, or
any pair where one's output is the other's input. Put those in the same
task, or in different sequential waves.

**A prior emulator-test failure, a completed remediation round, or the
pipeline being on its second or third loop does NOT by itself mean the next
batch of independent work must go back to one-task-at-a-time serial
dispatch.** Re-check independence fresh for whatever work actually remains
and parallelize it again if it qualifies — don't downgrade to serial out of
caution alone just because something upstream failed or this is a later
round. If you notice yourself about to dispatch one subagent at a time for
genuinely independent work, stop and re-group into parallel waves first.

## Who runs emulator tests

Emulator/connected Android tests (`connectedAndroidTest`, instrumented
`androidTest` execution, anything needing `adb` or a running
emulator/device, `android-emulator-gate.sh`) are run **only by the
top-level orchestrating session** — the human-facing conversation, never by
a dispatched subagent (`subagent`/`subagent-minimal`, any
`animoid-*-implementer`/`*-remediator`/`*-reviewer` agent). There is one
shared emulator/device; running it from multiple headless subagent processes
risks device contention, wasted time budget, and unreviewable flaky results.

If you are reading this skill as a dispatched subagent: stop at host-level
verification (steps 1-2 below plus, if relevant, an `androidTest`/gradle
**compile-only** check such as `assembleDebug` or
`compileDebugAndroidTestSources`). Never actually run the emulator or launch
connected tests yourself, even if a plan step or an earlier instruction in
this skill seems to ask for it. Note in your final report which
changes need emulator verification and stop — the orchestrator runs that
pass itself once your run completes.

Steps 3-5 below ("run the relevant emulator tests") describe the
orchestrator's job only.

## Steps

1. Use Pi's todo list to track work. Do not create execution-ledger files or add a baseline-runtime-evidence task.
2. Use test-driven development: write the smallest meaningful failing test, confirm the intended failure, implement through the production route, then run focused host tests.
3. (Orchestrator only) Finish all planned code changes before running Android emulator tests. Do not reinstall or run connected tests after every code edit.
4. (Orchestrator only) Once code changes are complete, run the relevant emulator tests in one pass.
5. (Orchestrator only) Review the full diff and production wiring after that run. Fix issues found in review, rerun affected host tests, and rerun emulator coverage only when review changes Android/native runtime behaviour.

Preserve unrelated working-tree changes. For Zig/JNI/native work, also use `animoid-zig-nightly`.

This workflow overrides any generic skill instruction to create a ledger file or collect pre-edit emulator/runtime baseline evidence for Animoid.
