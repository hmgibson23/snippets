---
name: model-provider-routing
description: Use before dispatching any subagent (or picking a model) whenever the instruction names a bare model/tier word without an explicit provider (e.g. "use subagent luna", "dispatch on sol", "spin up an opus reviewer"). Prevents silently picking the wrong provider when the same model id is offered by several providers at once. Not needed when the user names a provider explicitly, or inside a pipeline that intentionally pins a fixed provider (animoid-full-pipeline, codex-workflow).
---

# Model/provider routing for ambiguous bare model names

## The problem

Many literal model IDs in this install are offered by **several different
providers at once**, with the exact same id string. Confirmed overlaps
(verified via `pi --list-models`, re-check any time — this list can drift):

- `gpt-5.6-luna` / `gpt-5.6-sol` / `gpt-5.6-terra` / `gpt-5.5` / `gpt-5.4` —
  available on `openai-codex`, `github-copilot`, `opencode`, `opencode-go`,
  and (per the static provider catalog, may need auth) `openai`,
  `azure-openai-responses`, `amazon-bedrock` (as `openai.<id>`),
  `cloudflare-ai-gateway`, `openrouter` (as `openai/<id>`),
  `vercel-ai-gateway` (as `openai/<id>`).
- Claude tiers (`opus`/`sonnet`/`haiku`) — available on `github-copilot`,
  `opencode`, `opencode-zen-anthropic`, and usually `anthropic`,
  `amazon-bedrock`, `openrouter`, `vercel-ai-gateway` — **and the literal id
  spelling can differ by provider** (e.g. `github-copilot` uses
  `claude-opus-4.6` with dots, `opencode` uses `claude-opus-4-6` with
  dashes for the same model tier).

So "luna" or "opus" is not a unique identifier — it names a *model*, not a
*provider*. Never assume it belongs to one specific provider just because
some existing agent file's frontmatter happens to say so, or because a past
session picked one.

## Resolution order

When you're about to dispatch a subagent or otherwise pin
a model, and the instruction is a bare tier/model word with no explicit
provider:

1. **Explicit provider named by the user** ("use codex luna", "copilot
   opus", "run it on bedrock") → use that provider. Done.

2. **Inside a pipeline/skill that intentionally pins a fixed provider per
   role** (e.g. `codex-workflow`'s `codex-executor-luna`/`codex-executor-sol`/
   etc., any agent file whose *name itself* embeds the provider like
   `codex-executor-luna`) → honor that pin. These are deliberate,
   documented design decisions, not the bug this skill guards against. Don't
   "fix" them to match the current session provider. (`animoid-full-pipeline`
   is NOT one of these anymore — every stage there resolves its model fresh
   per run via this skill or `ask_model`, nothing is pinned.)

3. **Otherwise (ad hoc / generic dispatch, no pin, no named provider)** →
   default the provider to the **current session's active provider**, read
   from the `PI_PROVIDER` environment variable (`PI_MODEL` gives the exact
   current model if the user says "this model"/"the model I'm using now").
   Do not guess from whichever pre-existing agent file happens to fuzzy-match
   the tier word in its filename — check its `model:` frontmatter explicitly
   and treat a mismatch as disqualifying, not as "close enough".

4. **Verify before dispatching.** Run:

   ```
   pi --list-models | grep -i '<model-id>'
   ```

   to see every provider that currently exposes that id (this reflects real
   auth/availability, not just the static catalog). Confirm the resolved
   `<provider>/<model-id>` combo is an exact line in that output.
   - If the id doesn't exist verbatim on the resolved provider but a
     differently-spelled equivalent does (e.g. dots vs dashes, `claude-opus-4.6`
     vs `claude-opus-4-6`), use the provider's actual spelling — don't error
     out over cosmetic id differences.
   - If the resolved provider doesn't have the model at all, tell the user
     which providers do (from the grep output) and ask them to pick, instead
     of silently substituting a different provider than the one they're
     currently on.

5. **Still ambiguous?** Ask — use the `ask_model` tool (see
   `subagent-dispatch`) rather than guessing. Don't dispatch on a guess just
   to avoid a question.

## Don't create new stale ambiguity

If you write a fresh one-off agent file (per `subagent-dispatch`'s
`{MODEL}`-template pattern) for a bare tier word, name it so the provider is
obvious from the filename (e.g. `codex-executor-luna`, not a bare
`luna-executor` that could later be confused for a copilot or opencode
variant). Never leave two same-purpose agent files pinned to different
providers under near-identical names without the provider explicit in both
names — that's exactly how this class of bug gets baked in.
