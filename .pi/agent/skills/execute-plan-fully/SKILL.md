---
name: execute-plan-fully
description: Implement an existing written software plan completely, task by task, with test-first development, production-wiring checks, available runtime/emulator execution, test-harness validation, fresh final gates, and an evidence-based handoff. Use when the user says "use the execute-plan-fully skill to implement this plan", points to a plan file, pastes an implementation plan, asks an agent to carry a plan through without repeatedly stopping, or needs strict prevention of compile-only or test-only completion claims. Works with any model and uses Pi-native skills and tools; it does not bootstrap or invoke Codex skills.
---

# Execute Plan Fully

## Project workflow overrides

When the `animoid` skill is active:

- use Pi's todo list **and** create/update any ledger required by the written plan or repository instructions before production edits;
- do not create a pre-edit emulator/runtime-baseline task when connected execution is explicitly deferred;
- place tests by Animoid's test-pyramid contract: JVM for pure logic, Robolectric Compose host tests for rendering/semantics/layout/navigation/callbacks, Zig for native authority, and focused instrumentation only for irreducible Android/JNI/runtime boundaries;
- finish implementation waves and focused host TDD first, then run one consolidated review/remediation cycle;
- never run emulator/ADB/connected tests without current explicit user authorization. Compile instrumentation/APKs and report runtime verification deferred when authorization is absent.

These rules replace conflicting generic runtime guidance below; ledger, production-wiring, RED/GREEN, fresh host gates, and evidence requirements still apply.

## READ FIRST — the 6 rules that stop fake completion

1. **Keep a ledger FILE.** Write `docs/plans/<plan-name>.ledger.md`. One row per plan requirement. Edit it after every task. This file — not your memory — is the truth. If context is compacted, RE-READ this file before your next edit.
2. **Do every task, in order.** Never pick a subset. Never reprioritise. No "high priority only".
3. **RED before code.** Write a small test, run it, watch it FAIL for a behaviour reason, THEN write production code.
4. **Prove wiring.** A new function/class that only tests call is NOT implemented. Show a real non-test caller with `rg`.
5. **Gates need pasted output.** You may not say COMPLETE unless you ran the final gate THIS TURN and pasted its real output.
6. **If any ledger row lacks evidence → INCOMPLETE. Keep working.** Green aggregate tests do NOT cover missing UI, JNI, persistence, wiring, deletions, or commits.

If unsure at any decision point, pick the SAFER option: assume it is not wired, assume the test does not reach production, assume the row is incomplete. Then prove otherwise.

## The task loop (do this for every task)

```
Inspect  →  RED  →  Implement  →  Prove wiring  →  GREEN  →  Close
```

Each step has ONE gate you must pass before the next. Details in "Execute Each Task" below.

---

## Use Pi natively

- Use Pi's current tools and skills.
- Load skills from `~/.pi/agent/skills` through Pi's skill mechanism when the user, project instructions, or plan names them.
- Never run `~/.codex/superpowers/...`, `superpowers-codex bootstrap`, or Codex `use-skill` commands.
- Do not dispatch another model or team unless the user or plan explicitly asks for delegation.
- If another active Pi skill conflicts with this workflow, follow instruction priority and STATE the conflict. Do not silently mix workflows.

## Resolve the plan

Accept any of: an explicit path; a pasted plan; a plan clearly meant by "this plan"; the newest clearly relevant plan when the conversation makes it unambiguous.

Read the WHOLE plan before editing. Follow links and referenced local files needed to understand it.

- If you cannot safely identify which plan → ask ONE short question. Otherwise do not ask; start.
- Do not replace implementation with more planning.
- Do not rewrite the plan to make incomplete work look done.

## Establish the execution contract (before editing)

Do these in order:

1. Read project instructions (`AGENTS.md`, build/test docs).
2. Inspect branch, `git status`, recent commits, existing user changes.
3. Preserve unrelated changes. Never reset, discard, or overwrite them.
4. Discover what you can actually run locally:
   - attached devices / simulators / emulators;
   - installed SDKs, runtimes, browsers, containers, test services;
   - repo runner scripts and CI-equivalent commands;
   - credentials/artifacts that are genuinely unavailable.
5. If the plan touches a runtime/integration surface, run the cheapest authorized smoke test BEFORE editing. When Animoid connected execution is deferred, use focused host/Robolectric tests and compile-only Android checks instead; do not probe emulator/ADB availability or execute connected tests.
6. Record baseline failures with exact test, stage, process, artifact. Do NOT call them product defects until the harness proves it reached the real production state.
7. **Build the ledger file.** Create `docs/plans/<plan-name>.ledger.md` from this template and fill one row per requirement:

   ```markdown
   # Ledger: <plan-name>
   base: <plan-base-sha>   branch: <branch>   worktree: <path>

   | # | requirement (task/file/test/wiring/DoD/deferral) | status | evidence (command + output OR file:line OR commit) |
   |---|---------------------------------------------------|--------|-----------------------------------------------------|
   | 1 | <exact requirement copied from plan>              | todo   |                                                     |
   ```

   Include one row for EVERY: task, step, file operation, required test/command, production-wiring requirement, definition-of-done item, and explicitly deferred check. Also add a row for every available in-scope runtime gate (compiling a runtime test is NOT running it).
8. Also use Pi's todo/checklist tool if available — but the ledger FILE stays the source of truth.
9. Mark only the FIRST task in progress. Never mark work complete based on intention.

The plan is authoritative inside the user's requested scope. Newer user corrections override it.

---

## Execute each task

### 1. Inspect  ·  gate: you can name the real production file and its callers
- Read the exact production files and tests the plan names.
- `rg` for definitions, callers, duplicate state, legacy paths, related tests.
- Confirm the plan still matches current code. Fix stale line numbers / renamed files without changing required behaviour.

### 2. RED  ·  gate: the test fails for a BEHAVIOUR reason
- Add or strengthen the smallest test showing the missing behaviour.
- Run it. It must FAIL.
- Read the failure text. It must be a product/behaviour failure, e.g. `expected 3 dabs, got 0`.
  - NOT counted as RED: `error: unused variable`, `FileNotFound`, missing import, broken fixture, unrelated build break. Fix those and rerun until it fails for the real reason.
- Confirm the test reached the intended production state: correct screen/process, real callback, valid fixture, required thread, correct coordinate space.
- If a valid existing test already fails for the exact reason, use it.
- If a strong existing test already passes and truly covers the requirement, record that as baseline — do NOT break code to manufacture RED.
- NEVER weaken, delete, skip, quarantine, or exclude a valid test to get green.

### 3. Implement the minimum complete production change  ·  gate: one authoritative path, no stubs
- Implement the plan's architecture in PRODUCTION code.
- One authoritative path per state transition / side effect.
- Remove superseded booleans, callbacks, helpers, parallel state machines when the plan requires consolidation.
- Preserve document/user state on failures.
- Do NOT add: TODOs, stubs, placeholder UI, no-op branches, hard-coded test results, broad exception swallowing, arbitrary sleeps, retry loops that hide the defect.
- Avoid unrelated refactors and speculative features.

### 4. Prove wiring  ·  gate: a NON-test file calls the new code
Positive recipe — run and PASTE a line:
```bash
rg -n '<NewSymbolOrFunc>\(' --glob '!**/test/**' --glob '!**/androidTest/**' src/ android/app/src/main/
```
- If every match is under a test dir → NOT wired → this task is INCOMPLETE.
- For each new reducer/controller/route/adapter/state object: find its real caller in production sources (`src/main`, `src`, platform equivalent) and confirm the UI/JNI/CLI callback uses that same function/object.
- Test the callable production seam, not a copy of its behaviour. A debug bridge is not the release path.
- For UI/device work: existence ≠ visibility ≠ usability. Prove host-testable rendering, semantics, exact bounds/state, navigation, and typed callbacks with Robolectric Compose mounting production composables/routes. Use focused instrumentation only when Android runtime is irreducible; use authorized connected tests for real root/window/surface behavior.

### 5. GREEN  ·  gate: focused + adjacent tests pass, diff reviewed
- Run the focused test.
- Run adjacent subsystem tests.
- Run the plan's task-level build/compile command.
- Read failures fully; fix root causes before continuing.
- Review the diff for accidental changes; validate failure paths as well as success paths.

### 6. Close  ·  gate: ledger rows for this task have evidence
- Update the ledger FILE: set each of this task's rows to `done` with command+output / file:line / commit evidence.
- Commit only when the user or plan authorises commits. Use the plan's atomic boundary + message.
- Never bundle unrelated user changes into the commit. A requested commit missing from `git log` = task incomplete.
- Mark the task complete only after tests + wiring + task acceptance pass.
- Continue immediately to the next task. A progress update is NOT a stopping point.

---

## Debug systematically

When a command fails:
1. Read the complete error; find the earliest wrong boundary.
2. Reproduce with the smallest command.
3. Trace inputs/state through the real production path.
4. Form ONE concrete hypothesis.
5. Test it with logs / inspection / a focused test.
6. Fix the root cause.
7. Rerun focused → adjacent → broader gates.

Never substitute retries, delays, suppressed errors, fallback defaults, or relaxed assertions for diagnosis.

### Harness failure vs product failure — 2-question triage
Before you change production code because an integration/device test failed, answer:
- **Q1: Did the test enter the real screen/process?** Prove it with a log line. If NO → harness bug.
- **Q2: Did the input actually reach production (e.g. DOWN consumed by native)?** Prove it. If NO → harness bug.

If either is NO, fix the HARNESS, not production. Full harness defect list to rule out:
- test never entered the intended screen/process;
- two Activity/app/test lifecycle owners launched;
- fixture kept stale state or was invalid before the action;
- a main-thread-only API ran from the instrumentation thread;
- logical/view/root/window/device coordinates mixed;
- synthetic input skipped the real arbitration/dispatch root;
- assertions ran before the terminal callback/checkpoint;
- cleanup destroyed a resource while a live UI/surface still owned it;
- semantics queries used stale labels or asserted existence instead of display/reachability.

Fix the shared harness ONCE when a cluster shares a cause. Do not patch dozens of symptom assertions. Do not call a harness exception proof of a product defect. After the harness is sound, treat every surviving behaviour failure as a product failure until disproved.

Treat test infrastructure as production-quality: deterministic fixtures, one lifecycle owner, typed results, bounded waits on observable conditions, failure artifacts.

If the plan's proposed implementation is impossible in the current codebase, exhaust safe in-scope alternatives first. Stop ONLY for a genuine blocker needing user authority, unavailable credentials/artifacts, or a material architecture choice the plan does not resolve. Report the exact evidence and the smallest decision needed.

## Keep going

- Work through all sequential tasks without asking for routine confirmation.
- Give concise progress updates during long work, then continue.
- Do not stop because a task is hard, context compacted, a test is slow, or an optional device/service is absent.
- Respect explicit CURRENT deferrals. Do not inherit stale deferrals blindly — a newer user instruction overrides an older plan note.
- When device/runtime testing is relevant, respect current project/user authorization before checking or using simulator/emulator/device state. Animoid's explicit connected-test deferral overrides generic auto-discovery.
- If authorized and a compatible emulator/simulator is installed, use it for compatible gates. Reserve physical hardware only for what the emulator cannot represent.
- Compile-only runtime tests are acceptable ONLY when no compatible runtime exists or the user explicitly deferred execution this turn. State which condition applies.
- Do not broaden scope into deployment, release, messaging, or destructive operations unless the plan authorises them.

## Runtime and emulator gates

When the repo has integration / connected / e2e / UI / device tests:
1. Find the canonical runner; enumerate available targets before editing.
2. Prefer the installed tablet/phone/browser simulator over deferral when it covers the behaviour.
3. Record target name, OS/API/runtime version, architecture, resolution (where relevant), git SHA, artifact hashes.
4. Run a small known test FIRST to prove the runner executes tests (not just installs/compiles them).
5. Run the focused failing workflow during development.
6. Run the full unfiltered suite at the final gate unless the plan defines a narrower gate.
7. Capture machine-readable results + logs, crash/exit info, and a screenshot or equivalent failure artifact.
8. Treat zero executed tests, missing result file, instrumentation/process death, or runner timeout as FAILURE.
9. For native / lifecycle / race / flake fixes, require two consecutive clean full runs unless the plan defines a stronger repetition gate.

Android specifics:
- Check `adb devices -l` and the SDK emulator AVD list.
- Use an installed compatible AVD when no physical device is attached.
- Enter the intended app screen explicitly; Activity launch alone may leave you in Gallery/Home.
- One canonical Activity/editor rule. Do not mix `ActivityTestRule`, `ActivityScenarioRule`, and Compose Activity owners.
- Run Canvas/View/native mutations on Android main; keep encoding/artifact I/O off main.
- Dispatch synthetic stylus/touch through the actual production root with an explicit coordinate type; assert DOWN consumed before pixel/state assertions.
- Never manually destroy a native engine while a live Activity/SurfaceView/renderer/callback may still own it. Let the production lifecycle tear it down.
- Do not `pm clear` a physical/user device. Any disposable-emulator cleanup must first prove the target is the intended AVD.
- Keep physical hardware only for hardware-specific signals: pressure, tilt, hover, vendor GPU/gralloc, real latency.

## Final gate — run fresh, paste output

After all tasks look complete, run these THIS TURN and PASTE the real output. Beside each, PASS looks like:

```bash
git status --short          # PASS: only intended files listed
git diff --check            # PASS: prints nothing
<repo test command>         # PASS: ends "All N tests passed" / "0 failures"
<repo build command>        # PASS: build succeeded, exit 0
<runtime/emulator gate>     # PASS: N tests executed, 0 failures, no crash
git log --oneline <base>..HEAD   # PASS: every plan-required commit present
```

Then:
1. Reread the ENTIRE plan; compare every ledger row against the diff.
2. Run every final command from a fresh invocation (not cached).
3. Run repo-wide tests/builds in proportion to risk.
4. Execute every compatible **authorized** runtime/emulator gate discovered at the start. When current instructions defer connected execution, compile runtime tests/APKs, record exact deferral, and do not imply execution passed.
5. Run formatting / lint / type / ABI / source-search gates the plan requires.
6. `git diff --check` clean (Git).
7. `git status` shows only intended files changed.
8. Record exact test counts, failures, skips, command results, runtime target, artifacts, hashes, commits.
9. Inspect runtime logs / exit info for crashes the runner summarised poorly.
10. Label only genuinely unavailable / explicitly deferred external/manual checks as deferred — never imply they ran.

Old logs, cached summaries, prior-agent claims, and green tests for a test-only model are NOT completion evidence.

## Completion gate

Write **COMPLETE** only when ALL are true (check each against the ledger file):
- [ ] every host-completable ledger row satisfied with pasted evidence;
- [ ] every compatible, locally available, in-scope runtime/emulator gate passed;
- [ ] focused and final gates passed FRESH this turn;
- [ ] new abstractions are called by production (rg proof pasted);
- [ ] required legacy bypasses removed;
- [ ] no valid test weakened;
- [ ] integration tests reached the real production state through a valid harness;
- [ ] no runtime/instrumentation/native crash during the final gate;
- [ ] failure behaviour preserves user/project state;
- [ ] worktree contains only intended changes;
- [ ] all unperformed checks clearly marked deferred.

Any box unchecked → write **INCOMPLETE**, list the failing rows, and keep working unless genuinely blocked.

## Final handoff

Return a concise evidence report:
- outcome: complete / incomplete / blocked;
- each plan task and its status;
- production files changed;
- tests added/strengthened, with RED/GREEN evidence;
- exact final commands and results;
- runtime/emulator target, executed test count, failures/skips, artifact locations;
- production-wiring searches (the pasted rg lines);
- commits and artifact hashes when applicable;
- deviations from the plan with reasons;
- explicitly deferred device/external/manual verification;
- any remaining gap, stated plainly.

Do not claim done because compilation succeeded. Do not summarise a red connected/runtime suite as "pre-existing failures" without proving ownership and either fixing the in-scope root cause or recording a genuine scoped blocker.

## Invocation examples

```text
Use the execute-plan-fully skill to implement the plan at docs/plans/2026-07-22-editor-remediation.md.
```

```text
Use execute-plan-fully to implement this pasted plan. Do not require the Android device; I will verify it later.
```

```text
Use the execute-plan-fully skill to finish the current implementation plan, including all tests and production-wiring checks.
```
