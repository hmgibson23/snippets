---
name: animoid
description: Use when planning, implementing, debugging, reviewing, or verifying Animoid work involving its Zig 0.17 nightly engine, Android editor, JNI bridge, Vulkan presenter, timeline, brushes, selection, animation, effects, persistence, or tests.
---

# Animoid Workflow

## Model choice for coding subagents — user-chosen, not pinned

No Animoid coding/remediation/review dispatch is pinned to a fixed model by
default. Use whatever model the user names for that dispatch (resolved via
`model-provider-routing` if it's a bare tier word like "luna" that exists on
several providers), or `ask_model` if they didn't name one. Don't default to
a specific model (e.g. deepseek) out of habit or because an old agent file
happened to pin it — always resolve per-dispatch. `animoid-full-pipeline`'s
"Choosing a model for a stage" section is the canonical version of this rule
for pipeline runs; apply the same logic for one-off ad hoc dispatches too.

**Whatever model you resolve, verify its thinking level before dispatching:**

| Resolved model | Required thinking |
|---|---|
| `openai-codex/gpt-5.6-luna` | `max` |
| `openai-codex/gpt-5.6-terra` | `high` |
| Any model with no thinking-level concept (e.g. `deepseek/deepseek-v4-flash`, `mimo/mimo-v2.5-pro`) | omit the `thinking:` frontmatter field entirely — don't force one on |
| Any other thinking-capable model | `high` by default unless the user says otherwise |

If you find or generate an agent file whose `thinking:` doesn't match this
table for its resolved model, fix its frontmatter before dispatching — don't
dispatch with the wrong thinking level and don't silently accept a mismatch.

Never dispatch `opencode/mimo-v2.5-free` for Animoid work. Hugo has a paid MiMo subscription; MiMo means `mimo/mimo-v2.5-pro` unless he explicitly requests another variant. If frontmatter differs, stop and repair routing before dispatch. Keep each one-shot task narrow enough to finish, and give build/test commands enough timeout for Zig/Gradle.

## One-off Animoid dispatches (outside the full pipeline)

Even a quick ad hoc ask ("investigate why X crashes", "implement just this
one task", "review this diff", "draft a plan for Y") that isn't going
through the whole `animoid-full-pipeline` runbook must still use that
skill's five fixed dynamic template files, never a new hand-written agent
file:

| One-off task shape | Template (in `animoid-full-pipeline/`) | Write to (fixed path, always overwrite) |
|---|---|---|
| Implement/fix code | `implementer-agent.template.md` | `~/.pi/agent/agents/animoid-implement-dynamic.md` |
| Remediate specific findings | `remediator-agent.template.md` | `~/.pi/agent/agents/animoid-remediate-dynamic.md` |
| Review a diff | `review-agent.template.md` | `~/.pi/agent/agents/animoid-review-dynamic.md` |
| Draft/update a plan document | `planner-agent.template.md` | `~/.pi/agent/agents/animoid-plan-dynamic.md` |
| Read-only investigate/explore, no edits | `investigator-agent.template.md` | `~/.pi/agent/agents/animoid-investigate-dynamic.md` |

Resolve the model/thinking level per this skill's table above, fill
`{MODEL}`/`{THINKING}` into the matching template, write it to the fixed
path (overwriting whatever was there from a previous run), then dispatch
with `subagent`. Never mint a brand-new filename for the dispatch (e.g.
nothing like `openai-codex-luna-max-animoid-implementer.md` or
`animoid-luna-max-onion-plan-explorer.md`) — that's the anti-pattern that
caused agent-file proliferation before this rule existed. Picking the wrong
role for the task (e.g. using the implementer template for a read-only
investigation) is also wrong — match role to task shape, not convenience.

This overrides the generic `subagent-dispatch` skill's "write a new one-off
agent file yourself if none fit" fallback for ALL Animoid work — Animoid
always has a fitting template among these five; that fallback should never
trigger here.

## Parallel dispatch for independent work

Whenever there are 2+ independent tasks or remediation findings (different
files/modules, no shared state, neither needs the other's output), dispatch
them **in parallel** — one `subagent({ tasks: [...] })` call covering the
whole independent batch, not one call per task waited on serially. See
`parallel-plan-execution` for the full wave/dependency-grouping method (group
by independence, one call per wave not per task, still exactly one review/
remediation pass covering everything in that round).

Parallelizing never means changing which model does the work — every task
in a parallel batch still uses the same resolved model for that stage (e.g.
several `animoid-implement-dynamic` calls in one parallel batch, all filled
with the same `IMPLEMENT_MODEL`, or several `animoid-remediate-dynamic`
calls all filled with the same `REMEDIATE_MODEL`).

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

## Animoid test-pyramid contract

Choose test destination from behavior, not convenience:

| Behavior under test | Required default |
|---|---|
| Pure controller, policy, geometry, codec, routing, or immutable projection | JVM unit test under `android/app/src/test` |
| Compose rendering, semantics, bounds, enabled/selected state, navigation, callbacks, scrolling, dialogs, or focus | Robolectric Compose UI test under `android/app/src/test`, mounting production composables/routes |
| Native IDs, pixels, revisions, transactions, undo, persistence, OOM/non-mutation | Zig test imported by `src/run_tests.zig` |
| Irreducible JNI loading/marshalling or Android framework/runtime boundary | Focused `androidTest` instrumentation test |
| Activity-root MotionEvents, system keys/window/insets, SurfaceView/Vulkan PixelCopy, lifecycle/process restoration | Focused connected/emulator test, orchestrator-only |

For Compose/editor UI work, every behavior-level UI requirement must have a
Robolectric Compose host test unless a concrete Android-only boundary makes
that impossible. A pure model test does not prove rendering, visibility,
semantics, geometry, or callback wiring. A source-text grep does not prove UI
behavior. Mount the production composable or an extracted production host,
assert exact semantics/bounds/state, and invoke the real typed callback.

Add instrumentation coverage only for an irreducible boundary. Do not move
host-testable semantics/layout/model behavior into `androidTest`, and do not
retain or add broad connected inventories as a substitute for focused
Robolectric/JVM/Zig authority. Dispatched agents may compile `androidTest`
sources/APKs but never execute them.

During TDD, run focused JVM/Robolectric/Zig tests. Compile Kotlin/APKs when
cross-surface declarations change. Defer aggregate host gates until coherent
integration; defer connected execution until final integration and only when
the current user instruction explicitly authorizes it.

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

1. Use Pi's todo list to track work. For written-plan execution, create/update the plan-required ledger before production edits; do not add an emulator/runtime-baseline task when connected execution is deferred.
2. Use test-driven development: write the smallest meaningful failing test at the correct pyramid layer, confirm the intended behavioral failure, implement through the production route, then run focused host tests.
3. (Orchestrator only) Finish all planned code changes before any authorized Android emulator tests. Do not reinstall or run connected tests after every code edit.
4. (Orchestrator only) When explicitly authorized, run relevant emulator tests once in final integration. Otherwise compile instrumentation/APKs and report connected verification deferred, never passing.
5. Run one consolidated whole-diff review after implementation waves, one consolidated remediation pass, then lead final review/verification. Rerun affected host tests after remediation; rerun connected coverage only when authorized and remediation changes Android/native runtime behavior.

Preserve unrelated working-tree changes. For Zig/JNI/native work, also use `animoid-zig-nightly`.

This workflow overrides generic advice to run pre-edit emulator/runtime baselines or to place host-testable UI behavior in instrumentation. Project-required plan ledgers still apply.
