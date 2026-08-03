# Zig Nightly Guide for Animoid

Read this file before non-trivial Zig work in Animoid.

For concrete correct-syntax examples (Writergate I/O, `main(init)` entrypoint,
`std.Io.File` with the `io` param, build-system `root_module`, `.empty`/`.init`
containers, `{f}` format, removed features), load
`../../animoid-zig-nightly/references/zig-modern-syntax.md`. Every entry there is
verified against this pinned stdlib. Public "Zig 0.15" references are WRONG for
this nightly — do not copy them.

## Exact toolchain

| Item | Value |
| --- | --- |
| Zig executable | `/opt/homebrew/bin/zig` |
| Zig version | `0.17.0-dev.1387+01b60634c` |
| Zig implementation | `/opt/homebrew/lib/zig-dev/zig` |
| Standard library | `/opt/homebrew/lib/zig-dev/lib/std` |
| Compatible ZLS | `/Users/hugo/.local/bin/zls` |
| ZLS version | `0.17.0-dev.44+8da87d4f` |

Run:

```bash
/Users/hugo/.pi/agent/skills/animoid/scripts/check-zig-toolchain.sh
/opt/homebrew/bin/zig env
/opt/homebrew/bin/zig build --help
```

The repository currently accepts `-Drelease=true`; do not replace it with a remembered `-Doptimize=ReleaseFast` build option unless `zig build --help` for this checkout lists that option. Direct `zig test` does accept `-O ReleaseFast`.

Official secondary references:

- language reference for the development branch: <https://ziglang.org/documentation/master/>
- bundled standard-library browser: run `/opt/homebrew/bin/zig std`
- ZLS installation/version compatibility: <https://zigtools.org/zls/install/>
- ZLS configuration: <https://zigtools.org/zls/configure/zls-json/>

The installed compiler, installed standard library, and compiling repository code override examples from those pages when nightly APIs differ.

## Diagnostics workflow

1. Locate the active declaration with `rg`, then read its caller, owner, deinit path, and tests.
2. If Pi exposes `lsp`, use diagnostics, hover, go-to-definition, and find-references. Point its Zig server to `/Users/hugo/.local/bin/zls`.
3. Compile the smallest real root. Zig's lazy analysis means an unreferenced declaration can remain broken.
4. Run a focused failing test before production edits.
5. Format only changed `.zig` files.
6. Run the full Zig host gate before crossing into Kotlin/emulator work.

Useful commands:

```bash
rg -n 'symbolName|test "' src
/opt/homebrew/bin/zig ast-check src/path/to/file.zig
/opt/homebrew/bin/zig test src/run_tests.zig -O ReleaseFast --test-filter 'focused name'
/opt/homebrew/bin/zig fmt src/path/to/changed-file.zig
/opt/homebrew/bin/zig build test --summary all
/opt/homebrew/bin/zig build
```

Do not accept clean LSP diagnostics as a substitute for the two final build commands.

The checked-in skill configuration is
`/Users/hugo/.pi/agent/skills/animoid/references/zls.json`. If the Pi LSP
adapter accepts a ZLS config path, pass that file rather than creating a
different global configuration.

## Ownership and OOM

Before changing memory code, write down:

- allocator used;
- owner of the returned allocation;
- lifetime boundary;
- deinit/release function;
- whether a slice/list header can outlive its backing arena;
- state visible if allocation fails halfway through.

Correct mutation order:

1. validate IDs, bounds, modes, and owner revision;
2. allocate and fully initialize replacement state;
3. construct undo data;
4. atomically publish the replacement;
5. release superseded state;
6. mark dirty/present only after success.

Use `errdefer` for partially constructed ownership. On failure, return a typed error/Boolean/sentinel matching the existing API and leave the old document and preview usable.

Particular Animoid hazards:

- renderer arenas are destroyed and recreated; every arena-backed global header must be reset;
- preview/selection/transform buffers may have different owners and must not be double-released;
- an `ArrayList` with stale capacity is still a dangling pointer even if its length is zero;
- undo allocation failure must not leave a mutation without an undo record;
- JNI buffers, strings, and array elements cannot be retained after the JNI call;
- page/document/renderer allocators are not interchangeable merely because their element types match.

## ABI and persistence

For each Zig/JNI API change:

1. identify the Kotlin declaration and native export;
2. preserve parameter order, signedness meaning, and return convention;
3. validate negative Kotlin/JNI integers before casting to unsigned Zig values;
4. add exact boundary tests;
5. prove invalid input leaves state and serialized bytes unchanged.

For persisted structs/enums:

- never reorder or renumber tags casually;
- decode unknown values non-destructively;
- keep older-version fixtures or explicit encoders independent from the current writer;
- compare meaningful decoded state and exact unchanged-state behavior, not merely “non-zero.”

## Brush hot-path rules

The merge candidate is deliberately scalar-only. Remove dormant/mislabelled SIMD, vector selectors, and benchmark build steps. Do not spend time tuning SIMD until brush identity, resources, dynamics, previews, editor Cancel/Reset, persistence, pressure, smoothing, paint, and erase are truthful and stable.

For stroke code:

- allocate/reserve before the gesture or reuse owned scratch;
- keep resampling based on nominal diameter, not randomized diameter;
- derive deterministic per-emitted-dab seeds with separate salts;
- keep cursor sizing nominal and stable;
- make live stroke, preview, and persisted-resource resolution share the same pure rules;
- update previous-position state once per logical resampled center;
- preserve selection clipping, dirty bounds, undo, and presenter publication.

## Common false completions

- A field is serialized but production never reads it.
- A setter exists but the editor cannot reach it.
- A preview uses current active resources instead of the preset being previewed.
- A “SIMD” test calls scalar production dispatch on both sides.
- A migration fixture is produced by the current writer.
- A failure test checks only that the process did not crash.
- A helper test passes while the JNI/renderer stroke route is still broken.

Treat each as incomplete.
