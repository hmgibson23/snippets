---
name: animoid
description: Use when planning, implementing, debugging, reviewing, or verifying Animoid work involving its Zig 0.17 nightly engine, Android editor, JNI bridge, Vulkan presenter, timeline, brushes, selection, animation, effects, persistence, or tests.
---

# Animoid Workflow

## Pinned coding subagents

Before every Animoid coding/remediation dispatch, read agent frontmatter and verify exact model. Use only:

| Role | Agent profile | Required model |
|---|---|---|
| Fast implementation | `animoid-deepseek-implementer` | `deepseek/deepseek-v4-flash` |
| MiMo implementation/remediation | `animoid-mimo-pro-remediator` | `mimo/mimo-v2.5-pro` |
| Terra remediation | `animoid-terra-remediator` | `openai-codex/gpt-5.6-terra` |

Never dispatch `opencode/mimo-v2.5-free` for Animoid work. Hugo has a paid MiMo subscription; MiMo means `mimo/mimo-v2.5-pro` unless he explicitly requests another variant. If frontmatter differs, stop and repair routing before dispatch. Keep each one-shot task narrow enough to finish, and give build/test commands enough timeout for Zig/Gradle.

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
