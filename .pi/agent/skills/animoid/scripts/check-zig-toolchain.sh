#!/bin/sh
set -eu

expected_zig='/opt/homebrew/bin/zig'
expected_zig_version='0.17.0-dev.1387+01b60634c'
expected_zls='/Users/hugo/.local/bin/zls'
expected_zls_version='0.17.0-dev.44+8da87d4f'

if [ ! -x "$expected_zig" ]; then
    echo "ERROR: required Zig executable is missing: $expected_zig" >&2
    exit 1
fi

actual_zig_version=$("$expected_zig" version)
if [ "$actual_zig_version" != "$expected_zig_version" ]; then
    echo "ERROR: Animoid requires Zig $expected_zig_version; found $actual_zig_version" >&2
    exit 1
fi

zig_env=$("$expected_zig" env)
printf '%s\n' "$zig_env" | grep -F '/opt/homebrew/lib/zig-dev/lib/std' >/dev/null || {
    echo 'ERROR: Zig standard-library path does not match the Animoid toolchain.' >&2
    exit 1
}

if [ ! -x "$expected_zls" ]; then
    echo "ERROR: compatible ZLS is missing: $expected_zls" >&2
    echo "Do not install stable ZLS for this nightly Zig compiler." >&2
    exit 1
fi

actual_zls_version=$("$expected_zls" --version)
if [ "$actual_zls_version" != "$expected_zls_version" ]; then
    echo "ERROR: expected ZLS $expected_zls_version; found $actual_zls_version" >&2
    exit 1
fi

echo "Zig: $expected_zig ($actual_zig_version)"
echo "stdlib: /opt/homebrew/lib/zig-dev/lib/std"
echo "ZLS: $expected_zls ($actual_zls_version)"
echo 'Animoid Zig toolchain is ready.'
