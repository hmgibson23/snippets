---
name: animoid-zig-nightly
description: Enforces Animoid's pinned Zig 0.17 nightly, JNI ABI validation, deterministic brush/raster hot-path design, persistence compatibility, and production-path test quality. Use for any Animoid Zig, JNI, renderer, brush, raster, serialization, or native Android change, especially when DeepSeek implements a plan.
---

# Animoid Zig Nightly

Load `animoid` and an execution workflow skill (e.g. `execute-plan-fully`) too. This skill narrows attention to Zig/JNI correctness. The plan stays authoritative.

## READ FIRST

1. Use the pinned compiler at `/opt/homebrew/bin/zig` — never remembered Zig 0.11–0.16 syntax. For correct nightly syntax (I/O, `main(init)`, build system, containers, format, removed features), load `references/zig-modern-syntax.md` — do NOT reconstruct from memory or from public "Zig 0.15" references; this nightly landed the async `Io` interface and a new entrypoint that those get wrong.
2. Every JNI change needs BOTH sides + validation + a real caller (JNI ABI checklist below).
3. Validate signed JNI ints BEFORE `@intCast`. A negative cast can trap.
4. Brush emission: one emitted index per raster application, never reused, never shared between centers (worked example below).
5. A green test only counts if it would FAIL when you break production. Prove RED first.
6. The acceptance-evidence commands are the wrapper scripts — raw `zig test --test-filter` does NOT count.

## Pinned toolchain

Run before edits:

```bash
/Users/hugo/.pi/agent/skills/animoid/scripts/check-zig-toolchain.sh
/opt/homebrew/bin/zig env
```

Required Zig: `0.17.0-dev.1387+01b60634c` at `/opt/homebrew/bin/zig`.

Authority order:

1. installed compiler diagnostics;
2. installed stdlib at `/opt/homebrew/lib/zig-dev/lib/std`;
3. compiling Animoid patterns;
4. Zig master docs.

Never substitute remembered Zig 0.11–0.16 syntax. Zig analyzes declarations lazily: import new tests through `src/run_tests.zig` and run the direct ReleaseFast test root.

## Read-before-edit boundary

Before writing code, inspect every changed symbol's:

- its definition;
- all production callers;
- the state owner and its reset/deinit path;
- the JNI declaration/export pair (when applicable);
- the serializer/deserializer and old-version fixtures (when persisted);
- the nearest behaviour tests;
- the hot-path loop that contains the call.

Do not patch from grep snippets alone. If a row is empty, you have not read enough yet.

## Prove RED before code (worked example)

Write the smallest test, run it, and confirm it fails for a BEHAVIOUR reason:

```
GOOD RED:  test "rejects negative preset id" → FAIL: expected return 0, got trap/1
BAD  RED:  FAIL: error: unused local 'id'      ← compile error, does NOT count
BAD  RED:  FAIL: FileNotFound run_tests.zig    ← setup error, does NOT count
```

If the failure is a compile/setup error, fix the test scaffolding and rerun until it fails because the PRODUCTION behaviour is missing.

## Signed JNI values before unsigned casts

Validate signed JNI integers before `@intCast`:

```zig
if (preset_id < 0 or preset_id > brush.max_builtin_preset_id) return 0;
const id: u8 = @intCast(preset_id);
return @intFromBool(renderer.selectPreset(id));
```

Forbidden:

```zig
renderer.selectPreset(@intCast(preset_id)); // negative input may trap
```

Unknown enum values must return failure WITHOUT mutation unless the format contract explicitly defines a fallback. Never silently map an unknown external value to `.multiply`, ID `0`, or any other valid state.

For rejected mutations, snapshot and compare the exact prior state: active ID, preset struct/serialized bytes, resource dimensions and bytes, descriptors, colour, and any other plan-named state.

## JNI ABI checklist

For each native API change, verify:

| Boundary | Evidence |
| --- | --- |
| Kotlin declaration | exact name, order, types, return |
| Zig export | matching mangled name and ABI types |
| Validation | negative, upper bound, invalid enum, bad array length |
| Production caller | actual editor/controller route (paste an `rg` line, non-test file) |
| Failure semantics | no mutation, Boolean/sentinel propagated |
| Tests | boundary AND production-route behaviour |

A renderer setter with no bridge export and no editor caller is INCOMPLETE. A Kotlin caller that ignores the required failure is INCOMPLETE.

## Transactional native state

Mutation order:

1. validate every scalar, enum, dimension, and byte length;
2. decode/copy into temporary storage;
3. allocate replacement state if needed;
4. publish all fields together;
5. clear/release superseded state;
6. return success and invalidate preview/presentation.

On failure, the old state and last presentable frame stay usable. No `catch unreachable`, OOM panic, partial resource copy, or fallback "success".

## Deterministic brush emission model

Keep these concepts DISTINCT — do not conflate them:

- **logical center index** — one per resampler output;
- **emitted index** — one per actual paint/erase raster application;
- **nominal dab** — pressure, velocity, tilt, taper, base rotation; NO random size/rotation/scatter;
- **emitted dab** — nominal dab PLUS deterministic per-emission dynamics;
- **previous logical position** — updates once, AFTER all sub-stamps for the center.

Use one seed mixer with named, distinct salts for size, rotation, scatter, stamp-count, and colour. Never apply the same dynamic twice.

Per logical center:

1. evaluate the nominal dab;
2. derive the stamp count once;
3. for each sub-stamp, consume a unique monotonic emitted index;
4. derive emitted size/rotation/scatter/colour from THAT index;
5. update dirty bounds and tracing from the ACTUAL emitted dab/center;
6. invoke the exact raster path once;
7. increment the emitted index;
8. after the loop, update previous logical position once.

**Worked example — one center, `stamp_count = 3`, emitted index starts at 5:**

```
center C0: stamp_count=3
  sub-stamp 0 → emitted index 5  → raster once
  sub-stamp 1 → emitted index 6  → raster once
  sub-stamp 2 → emitted index 7  → raster once
  update previous logical position ONCE
next center C1 starts at emitted index 8
```

Wrong outcomes this must NOT produce:

```
all three sub-stamps use index 5          (index incremented only once)
C1 reuses index 7                          (overlap between adjacent centers)
```

### Forbidden defect → correct behaviour

| Forbidden defect | Correct behaviour |
| --- | --- |
| increment emitted index once for multiple sub-stamps | increment once per sub-stamp; each raster call gets its own index |
| overlapping emitted indices between adjacent centers | next center starts strictly after the previous center's last index |
| scatter the nominal center, then scatter again from the already-scattered center | scatter exactly once, from the nominal center |
| apply rotation jitter to both nominal dab and emitted dab | apply rotation jitter only to the emitted dab |
| clamp hairline presets to `min_size` | let `minimum_output_diameter` govern hairline output |
| dirty/tracing bounds from the nominal dab only | compute bounds from the actual emitted dab/center |
| preview uses seed `0` for every step | preview uses the same per-emission seed sequence as live |
| cursor evaluates a randomized size | cursor evaluates the nominal (non-random) size |

Blur/smudge stay explicitly single-application unless a newer plan says otherwise.

## Resource resolution

When the plan requires one resolver, implement ONE pure function whose inputs identify the preset plus its custom/built-in resource sources. Its output must carry all semantics: tip, grain, inversion, flips, strengths, scale, coordinates, blend mode, and `tip_replaces_shape`.

Prove ALL consumers call the same resolver:

- live renderer;
- cursor coverage;
- built-in preview;
- serialized preview;
- imported-resource preview;
- duplicate/restore path.

A global `activeDabResources()` used only by the active renderer is NOT a pure shared resolver. Preview must not borrow unrelated active-brush state.

## Persistence and editor transaction

The current writer cannot be the source of an old-version migration fixture. Use explicit version encoders or committed byte fixtures independent of the v8 writer.

Test:

- every enum value;
- both Boolean values where meaningful;
- numeric boundaries;
- truncated input;
- unknown enum/tag;
- exact non-mutation after failure.

The editor Cancel/Reset snapshot must include: preset bytes, descriptor metadata, native tip bytes, native grain bytes, dimensions, strengths, flips, coordinate mode, and blend mode. Restoring bytes alone is insufficient when preset installation clears resources.

## Behaviour-test standard

The required behaviour determines the valid proof:

| Requirement | Valid proof |
| --- | --- |
| N raster applications | counter incremented immediately at the real raster call, exact `N` |
| exact pixels | full expected buffer/fixture/hash constant with an explained origin |
| invalid selection non-destructive | exact before/after state and serialized bytes |
| flip/blend preview | asymmetric resource through the production preview, unequal/exact output |
| deterministic seed | same seed → exact equal; different seed → exact unequal |
| edge clipping | full buffer before/after, or the exact changed-index set |
| migration | independent old-version fixture decoded to exact fields |

Reject `expect(true)`, "does not crash", non-zero-only hash/count, metadata-only preview tests, and scalar-vs-scalar self-comparison.

## Scalar raster rule

The current brush merge candidate is scalar-only. No SIMD, `@Vector`, kernel selector, an oracle mislabeled as an optimized path, or brush benchmark.

Deleting SIMD must not silently rewrite scalar arithmetic or leave comments describing removed optimization. Preserve the known scalar implementation unless exact pixel fixtures justify a change. Review hot-loop cost after deletion.

## Focused and final commands

Raw `zig test --test-filter` is DIAGNOSTIC ONLY. For acceptance EVIDENCE, use the named-test identity wrapper — it verifies the expected fully-qualified test name actually ran:

```bash
bash ~/.pi/agent/skills/animoid-zig-nightly/scripts/run-focused-test.sh \
  "$PWD" '<filter>' '<fully-qualified expected test name>'
bash ~/.pi/agent/skills/animoid-zig-nightly/scripts/lint-added-tests.sh "$PWD" main
/opt/homebrew/bin/zig fmt <changed-zig-files>
/opt/homebrew/bin/zig build test --summary all
/opt/homebrew/bin/zig test src/run_tests.zig -O ReleaseFast
/opt/homebrew/bin/zig build -Drelease=true
git diff --check
bash ~/.pi/agent/skills/animoid-zig-nightly/scripts/check-worktree-complete.sh \
  "$PWD" main third_party/skia/prebuilt/prebuilt third_party/skia/src -- \
  'test: strengthen final brush contracts' \
  'fix: correct nominal brush emission' \
  'fix: route all brush resources through resolver' \
  'fix: finish advanced brush editor transaction'
```

Before completion, search changed code and named scope for plan-forbidden remnants. READ the matches. A zero-match command alone cannot prove required POSITIVE wiring — to prove wiring, paste an `rg` line showing a non-test production file calls the new code.
