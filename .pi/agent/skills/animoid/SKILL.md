---
name: animoid
description: Use when planning, implementing, debugging, reviewing, or verifying Animoid work involving its Zig 0.17 nightly engine, Android editor, JNI bridge, Vulkan presenter, timeline, brushes, selection, animation, effects, persistence, or tests.
---

# Animoid Workflow

## Execution model

Perform Animoid work directly in the main Pi session. This installation has no
subagent runtime. Keep the same evidence, Zig/JNI, test-pyramid, and
orchestrator-only emulator requirements below; do not attempt to delegate work
to a missing `subagent` tool.

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
Robolectric/JVM/Zig authority. Compile `androidTest` sources/APKs before
connected execution when the boundary requires it.

During TDD, run focused JVM/Robolectric/Zig tests. Compile Kotlin/APKs when
cross-surface declarations change. Defer aggregate host gates until coherent
integration; defer connected execution until final integration and only when
the current user instruction explicitly authorizes it.

## Who runs emulator tests

Emulator/connected Android tests (`connectedAndroidTest`, instrumented
`androidTest` execution, anything needing `adb` or a running emulator/device,
or `android-emulator-gate.sh`) run only once after coherent integration and
only when the current user instruction authorizes them. There is one shared
emulator/device; do not run competing connected-test processes.

## Steps

1. Use Pi's todo list to track work. For written-plan execution, create/update the plan-required ledger before production edits; do not add an emulator/runtime-baseline task when connected execution is deferred.
2. Use test-driven development: write the smallest meaningful failing test at the correct pyramid layer, confirm the intended behavioral failure, implement through the production route, then run focused host tests.
3. (Orchestrator only) Finish all planned code changes before any authorized Android emulator tests. Do not reinstall or run connected tests after every code edit.
4. (Orchestrator only) When explicitly authorized, run relevant emulator tests once in final integration. Otherwise compile instrumentation/APKs and report connected verification deferred, never passing.
5. Run one consolidated whole-diff review after implementation waves, one consolidated remediation pass, then lead final review/verification. Rerun affected host tests after remediation; rerun connected coverage only when authorized and remediation changes Android/native runtime behavior.

Preserve unrelated working-tree changes. For Zig/JNI/native work, also use `animoid-zig-nightly`.

This workflow overrides generic advice to run pre-edit emulator/runtime baselines or to place host-testable UI behavior in instrumentation. Project-required plan ledgers still apply.
