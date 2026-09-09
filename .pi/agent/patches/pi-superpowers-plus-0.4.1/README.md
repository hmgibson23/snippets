# pi-superpowers-plus 0.4.1 local subagent patch

The package manager installs `pi-superpowers-plus` as production-only content,
so its local runtime changes under `~/.pi/agent/npm/node_modules/` are not
tracked automatically. This bundle is the source of truth for the local
subagent patch (per-call model/thinking overrides, timeout parsing, fast-start,
and focused tests).

After installing or updating exactly version 0.4.1, reapply it from the Pi
configuration directory:

```bash
cp -R ~/.pi/agent/patches/pi-superpowers-plus-0.4.1/extensions/subagent/. \
  ~/.pi/agent/npm/node_modules/pi-superpowers-plus/extensions/subagent/
```

Then run `/reload` and verify:

```bash
cd ~/.pi/agent/npm/node_modules/pi-superpowers-plus
npx --yes vitest@4.0.18 run extensions/subagent/agents.test.ts
```

Do not apply this bundle to another package version without reviewing the
upstream changes first.
