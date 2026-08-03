# Zig 0.17 Nightly Syntax — Animoid pinned toolchain

Correct modern syntax for `0.17.0-dev.1387+01b60634c` at `/opt/homebrew/bin/zig`.
Do NOT reconstruct from memory. Training data is mostly 0.11–0.14, and even
public "Zig 0.15" references are WRONG here — this nightly landed the async
`Io` interface and a new program entrypoint. Every entry below was verified
against the installed stdlib at `/opt/homebrew/lib/zig-dev/lib/std`.

Authority order still applies: installed compiler diagnostics > installed
stdlib source > compiling Animoid code > docs. If a pattern here ever conflicts
with a compiler error on this checkout, the compiler wins — read the stdlib
source and update this file.

## Verify-first commands

```bash
/opt/homebrew/bin/zig version          # expect 0.17.0-dev.1387+01b60634c
/opt/homebrew/bin/zig ast-check <file.zig>
rg -n '<symbol>' /opt/homebrew/lib/zig-dev/lib/std
```

---

## Program entrypoint (0.17 — NEW, not in 0.15 refs)

`main` receives `std.process.Init`, which owns the `Io` implementation and
allocators. There is no free `std.heap.page_allocator` entry ritual and no
`std.io.getStdOut()`.

```zig
const std = @import("std");

pub fn main(init: std.process.Init) !void {
    const io = init.io;                 // async I/O implementation
    const gpa = init.gpa;               // threadsafe general purpose allocator
    const arena = init.arena.allocator(); // process-lifetime arena

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &stdout_writer.interface;
    try stdout.print("hello {s}\n", .{"world"});
    try stdout.flush();                 // REQUIRED
}
```

`std.process.Init` fields (verified `std/process.zig`): `minimal` (`.args`,
`.environ`), `arena: *ArenaAllocator`, `gpa: Allocator`, `io: Io`,
`environ_map`, `preopens`. Use `main(minimal: std.process.Init.Minimal)` if you
only need args/env.

> Animoid is a native Android library, not a CLI — you rarely write `main`. But
> tests, benchmarks, and any host tool follow this shape. See
> `std/crypto/benchmark.zig` for a real example.

---

## File I/O threads the `Io` interface (0.17 — differs from 0.15)

`File` now lives at `std.Io.File`. Its `reader`/`writer` take `io` as the first
argument. The 0.15 pattern `std.fs.File.stdout().writer(&buf)` does NOT compile
here.

```zig
// stdout / stderr / stdin
var buf: [4096]u8 = undefined;
var fw = std.Io.File.stdout().writer(io, &buf);
const w = &fw.interface;
try w.print("...", .{});
try w.flush();

// arbitrary file
var file_writer = file.writer(io, &buf);      // sig: writer(file, io, buffer)
var file_reader = file.reader(io, &buf);      // sig: reader(file, io, buffer)
```

Signatures (verified `std/Io/File.zig`): `pub fn writer(file, io: Io, buffer)`,
`pub fn reader(file, io: Io, buffer)`, plus `writerStreaming`/`readerStreaming`.

---

## Writer / Reader interfaces (non-generic — "Writergate")

`std.Io.Writer` / `std.Io.Reader` are non-generic; buffer lives in the
interface. Old `std.io.getStdOut().writer()`, `BufferedWriter`, `GenericWriter`,
`AnyWriter`, `FixedBufferStream` are gone.

```zig
// fixed buffer, no file
var b: [256]u8 = undefined;
var w: std.Io.Writer = .fixed(&b);
try w.print("Hello {s}", .{"world"});
const out = w.buffered();               // "Hello world"

// allocating writer
var aw: std.Io.Writer.Allocating = .init(gpa);
defer aw.deinit();
try aw.writer.print("...", .{});

// counting → Discarding.fullCount()
var d: std.Io.Writer.Discarding = ...;  // has .fullCount()
```

```zig
// reader from slice
var r: std.Io.Reader = .fixed("hello\nworld");
while (try r.takeDelimiter('\n')) |line| { ... } // null at EOF, excludes '\n'

const v  = try r.takeInt(u32, .big);    // takeInt(T, endian)
const a  = try r.takeArray(4);          // *[4]u8
const p  = try r.takeStructPointer(T);  // *align(1) T  (no bare takeStruct)
const by = try r.takeByte();
```

Verified: `takeDelimiter`, `takeDelimiterInclusive/Exclusive`, `takeInt`,
`takeVarInt`, `takeArray`, `takeStructPointer`, `takeByte` in `std/Io/Reader.zig`.
There is NO `takeStruct` — use `takeStructPointer`.

---

## Custom format method (0.17)

```zig
// WRONG (pre-0.15)
pub fn format(self: @This(), comptime fmt: []const u8, opts, writer: anytype) !void

// CORRECT (verified std/SemanticVersion.zig, std/Io/Writer.zig)
pub fn format(self: @This(), w: *std.Io.Writer) std.Io.Writer.Error!void {
    try w.print("...", .{});
}
```

Call format methods with `{f}`:

```zig
std.debug.print("{f}\n", .{value});     // {} on a formattable type = error
```

`std.fmt.Formatter` → use `std.fmt.Alt(T, formatFn)` / `std.fmt.alt(...)`
(verified `std/fmt.zig`).

---

## Container initialization

Never `.{}` for collections. Use `.empty` / `.init` (verified
`std/array_list.zig`, `std/hash_map.zig`, `std/heap/debug_allocator.zig`).

```zig
var list: std.ArrayList(u32) = .empty;                 // ArrayList is UNMANAGED default
var map:  std.AutoHashMapUnmanaged(u32, u32) = .empty;
var gpa:  std.heap.DebugAllocator(.{}) = .init;

// ArrayList methods now take the allocator per call:
try list.append(gpa, item);
list.deinit(gpa);

// BoundedArray removed → fixed backing buffer
var backing: [8]i32 = undefined;
var stack = std.ArrayList(i32).initBuffer(&backing);
```

Naming (verified):
- `std.ArrayListUnmanaged` → `std.ArrayList` (managed `Managed`/`AlignedManaged` deprecated).
- `std.heap.GeneralPurposeAllocator` alias **REMOVED** on this nightly — use
  `std.heap.DebugAllocator` only (grep confirms no GPA alias in `std/heap.zig`).

---

## Build system (`build.zig`)

`root_source_file` removed from `addExecutable`/`addLibrary`/`addTest`
(verified `ExecutableOptions.root_module: *Module` in `std/Build.zig`).

```zig
const exe = b.addExecutable(.{
    .name = "app",
    .root_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    }),
});

exe.root_module.addImport("helper", helper_mod);       // not exe.addModule(...)

const dep = b.dependency("lib", .{ .target = target, .optimize = optimize });
exe.root_module.addImport("lib", dep.module("lib"));
```

Compile-level `exe.linkSystemLibrary()`, `addCSourceFiles()`, `addIncludePath()`,
`linkLibC()` are deprecated — call the `exe.root_module.*` equivalents.

> Animoid build reality (from `animoid`/`zig-nightly.md`): this repo accepts
> `-Drelease=true`. Do NOT substitute a remembered `-Doptimize=ReleaseFast`
> build option unless `zig build --help` on this checkout lists it. Direct
> `zig test` still accepts `-O ReleaseFast`.

---

## Removed / changed language features

| Removed / changed | Correct 0.17 |
| --- | --- |
| `usingnamespace @import(...)` | explicit re-export: `const m = @import(...); pub const x = m.x;` |
| `async` / `await` keywords | gone — async is via the `Io` interface now |
| `@setCold(true)` | `@branchHint(.cold)` — first statement in block |
| `@export(foo, ...)` | `@export(&foo, ...)` — takes a pointer |
| asm clobbers `: "rcx","r11"` | `: .{ .rcx = true, .r11 = true }` |
| `@fence(...)` | removed — use stronger atomic ordering / RMW |

---

## Error → fix quick table (0.17-verified)

| Error / symbol | Fix |
| --- | --- |
| `no field 'root_source_file'` | `root_module = b.createModule(.{...})` |
| `expected type 'Io'` on file `.writer`/`.reader` | pass `io` first: `file.writer(io, &buf)` |
| `main` signature mismatch | `pub fn main(init: std.process.Init) !void` |
| `no member 'getStdOut'` / `std.io.*` writer | `std.Io.File.stdout().writer(io, &buf)` then `.interface` |
| `GeneralPurposeAllocator` undeclared | `std.heap.DebugAllocator` |
| ambiguous `{}` on formattable | use `{f}` |
| `use of undefined value` | arithmetic on `undefined` is illegal |
| `std.fmt.Formatter` undeclared | `std.fmt.Alt` / `std.fmt.alt` |
| `fmtSliceEscapeLower/Upper` | `std.ascii.hexEscape(bytes, .lower/.upper)` |
| `std.BoundedArray` undeclared | `std.ArrayList(T).initBuffer(&backing)` |
| `std.fifo.LinearFifo` | removed — `std.Io.Reader`/`Writer` patterns |

---

## Decl literals, labeled switch, non-exhaustive switch (0.14+, still valid)

```zig
const a: S = .default;        // S.default
const b: S = .init(42);       // S.init(42)

state: switch (initial) {     // labeled switch state machine
    .idle => continue :state .running,
    .running => if (done) break :state result else continue :state .running,
}

switch (value) {              // non-exhaustive enum: named + else + _
    .a, .b => {},
    else => {},
    _ => {},
}
```

Before trusting any of the above in production code, still run
`zig ast-check` and the focused test wrapper — this is a moving nightly.
