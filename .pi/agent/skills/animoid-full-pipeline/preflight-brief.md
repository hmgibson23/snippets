# Animoid subagent pre-flight brief

Single-file distillation of the full skill set (animoid-deepseek-guardrails,
execute-plan-fully, animoid-zig-nightly, animoid). This is your working rulebook.
Deep-dive into the underlying skill files ONLY if you hit a case this brief
doesn't cover — don't re-read them up front.

## 1. Evidence gates (never fake completion)

- You may NOT write done / complete / finished / passing / working / ✅ about any
  task unless you pasted the real command output THIS TURN that proves it.
  Memory is not evidence. A summary is not evidence. Compiling is not passing.
- Keep a ledger file `docs/plans/<slug>.ledger.md` — one row per requirement,
  edited after every task. It is the source of truth, not your memory. If context
  is compacted, re-read it before your next edit.
- Do EVERY task you were given, in order. No skipping, reordering, or "only the
  important ones". Auto-continue between tasks; never stop to ask.
- When unsure, pick the SAFER assumption: assume not wired, assume row incomplete
  — then prove otherwise.
- Do not invent test output, counts, or results. Paste only real terminal output.

## 2. Per-task loop

```
READ   → open exact production file + tests; rg symbol + callers; paste what you found
RED    → smallest test; RUN IT; paste output; MUST fail for a BEHAVIOUR reason
         ("expected 3 dabs, got 0"). Compile error / FileNotFound ≠ red — fix scaffolding, rerun.
CODE   → minimum production change. No TODOs, stubs, no-op branches, catch unreachable.
WIRING → paste an rg line proving a NON-test production file calls your new code:
         rg -n '<NewSymbol>(' --glob '!**/test/**' --glob '!**/androidTest/**' src/ android/app/src/main/
         Setter/field/function with no production caller = INCOMPLETE.
GREEN  → run focused test wrapper (below); paste output.
CLOSE  → update ledger rows with evidence; move to next task immediately.
```

Never weaken, delete, skip, or quarantine a valid test to get green.
Debug systematically on failure: read full error, smallest repro, ONE hypothesis,
fix root cause. Never substitute retries/delays/relaxed assertions for diagnosis.

## 3. Test placement + quality (Animoid pyramid)

| Behavior | Required test home |
|---|---|
| Pure logic/controller/policy/geometry/codec | JVM unit test under `android/app/src/test` |
| Compose rendering, semantics, bounds, state, navigation, callbacks | Robolectric Compose UI test under `android/app/src/test`, mounting PRODUCTION composables |
| Native IDs, pixels, revisions, undo, persistence | Zig test imported by `src/run_tests.zig` |
| Irreducible JNI loading/marshalling | Focused androidTest (compile-only for subagents) |
| Activity-root MotionEvents, system keys, SurfaceView/Vulkan PixelCopy, lifecycle restore | Emulator — orchestrator ONLY |

Reject: `expect(true)`, "does not crash", non-zero-only counts, hash-sentinel,
metadata-only preview checks. Assert exact values/states/bytes.
The lint will fail you otherwise: `bash ~/.pi/agent/skills/animoid-zig-nightly/scripts/lint-added-tests.sh "$PWD" main`

## 4. Zig/JNI correctness essentials

- Pinned compiler `/opt/homebrew/bin/zig` (`0.17.0-dev.1387+01b60634c`). Never use
  remembered 0.11–0.16 syntax. For nontrivial NEW Zig code, consult
  `~/.pi/agent/skills/animoid-zig-nightly/references/zig-modern-syntax.md` — but
  only then, not up front.
- Validate signed JNI ints BEFORE `@intCast` (negative cast can trap):
  `if (preset_id < 0) return 0; const id: u8 = @intCast(preset_id);`
- Unknown enum values return failure WITHOUT mutation — never silently map to a
  valid state.
- Transactional native state: validate all scalars → decode to temp → allocate →
  publish together → release old → return success. On failure old state stays usable.
- Brush emission: one unique monotonic EMITTED index per raster application, never
  reused across centers/sub-stamps; dynamics derived from THAT index; previous
  logical position updated once after all sub-stamps. If your task touches brush
  hot paths, read the worked example in `animoid-zig-nightly/SKILL.md` first.
- JNI change ⇒ BOTH sides match (Kotlin declaration ↔ Zig export), validation,
  real production caller, failure semantics propagate.
- Acceptance evidence uses the wrapper (raw `zig test --test-filter` doesn't count):

```bash
bash ~/.pi/agent/skills/animoid-zig-nightly/scripts/run-focused-test.sh "$PWD" '<filter>' '<expected fully-qualified test name>'
```

## 5. Hard bans (all models)

- NEVER run the Android emulator, `adb`, `connectedAndroidTest`, or any instrumented
  test on a device/emulator. Orchestrator-only. Compile-only (`assembleDebug`,
  `compileDebugAndroidTestSources`) is fine.
- Do not commit unless plan/user authorizes it. Don't bundle unrelated changes.
- Preserve unrelated working-tree changes.
- No SIMD/@Vector in brush merge paths — scalar-only unless the plan says otherwise.

## 6. Parallel wave vs solo — check FIRST

- **Solo dispatch** (task hands you the whole plan): run the full trio fresh before
  claiming COMPLETE, and pass the 7-question Done Test below:
  `zig build test --summary all` · `zig test src/run_tests.zig -O ReleaseFast` · `zig build -Drelease=true`
- **One task of a PARALLEL WAVE** (explicit file scope given): your done claim covers
  ONLY your scope. Gate = `zig fmt <your changed files>` + `git diff --check` +
  `git status --short` + focused test + rg wiring proof. Do NOT run the trio — the
  orchestrator runs it once after the whole wave lands.

Done Test (solo only, before ANY COMPLETE claim): every task attempted in order /
every todo has real evidence / every new symbol has an rg wiring line / focused
wrapper run fresh this turn (+ solo trio) / lint passed / git diff --check clean /
no valid test weakened. Any NO → write "INCOMPLETE — remaining rows: …" and keep
working. All YES → COMPLETE + ledger table + pasted gate output.

## 7. Setup commands (once, before first edit)

```bash
/opt/homebrew/bin/zig version
bash ~/.pi/agent/skills/animoid/scripts/check-zig-toolchain.sh
git status --short
git log -1 --oneline
```
